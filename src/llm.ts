export type Model = {
  apiKey: string;
  model: string;
  baseUrl?: string;
  maxTokens?: number;
};

export type ContentBlock =
  | { type: "text"; text: string }
  | { type: "tool_use"; id: string; name: string; input: string }
  | { type: "tool_result"; tool_use_id: string; content: string };

export type Message = {
  role: "user" | "assistant";
  content: string | ContentBlock[];
};

export type Context = {
  systemPrompt?: string;
  messages: Message[];
};

export type StreamEvent =
  | { type: "text_delta"; delta: string }
  | { type: "tool_call"; id: string; name: string; args: string }
  | {
      type: "done";
      stopReason: "end_turn" | "tool_use" | "max_tokens" | "aborted";
    }
  | { type: "error"; error: Error };

export type ToolDef = {
  name: string;
  description: string;
  parameters: object;
};

type OpenAIChunk = {
  choices: Array<{
    delta?: {
      content?: string;
      tool_calls?: Array<{
        index?: number;
        id?: string;
        function?: { name?: string; arguments?: string };
      }>;
    };
    finish_reason?: string;
  }>;
};

function handleSSELine(
  data: string,
  toolCallBuffers: Map<number, { id: string; name: string; argsBuf: string }>,
): {
  textDelta: string | null;
  stopReason: "end_turn" | "tool_use" | "max_tokens" | null;
} {
  let chunk: OpenAIChunk;
  try {
    chunk = JSON.parse(data) as OpenAIChunk;
  } catch {
    return { textDelta: null, stopReason: null };
  }

  const choice = chunk.choices[0];
  if (!choice) return { textDelta: null, stopReason: null };

  let textDelta: string | null = null;
  let stopReason: "end_turn" | "tool_use" | "max_tokens" | null = null;

  if (choice.delta?.content) textDelta = choice.delta.content;

  if (choice.delta?.tool_calls) {
    for (const tc of choice.delta.tool_calls) {
      const idx = tc.index ?? 0;
      if (!toolCallBuffers.has(idx)) {
        toolCallBuffers.set(idx, {
          id: tc.id ?? `call_${idx}`,
          name: "",
          argsBuf: "",
        });
      }
      const entry = toolCallBuffers.get(idx)!;
      if (tc.id) entry.id = tc.id;
      if (tc.function?.name) entry.name = tc.function.name;
      if (tc.function?.arguments) entry.argsBuf += tc.function.arguments;
    }
  }

  if (choice.finish_reason === "tool_calls") stopReason = "tool_use";
  else if (choice.finish_reason === "length") stopReason = "max_tokens";

  return { textDelta, stopReason };
}

function flushToolCalls(
  toolCallBuffers: Map<number, { id: string; name: string; argsBuf: string }>,
): { id: string; name: string; args: unknown }[] {
  const calls: { id: string; name: string; args: unknown }[] = [];
  for (const [, tc] of [...toolCallBuffers].sort((a, b) => a[0] - b[0])) {
    let args: unknown = {};
    if (tc.argsBuf) {
      try {
        args = JSON.parse(tc.argsBuf);
      } catch {
        args = {};
      }
    }
    calls.push({ id: tc.id, name: tc.name, args });
  }
  return calls;
}

export function contextToOpenAIMessages(context: Context): object[] {
  const messages: object[] = [];
  if (context.systemPrompt)
    messages.push({ role: "system", content: context.systemPrompt });

  for (const msg of context.messages) {
    if (typeof msg.content === "string") {
      messages.push({ role: msg.role, content: msg.content });
      continue;
    }

    const blocks = msg.content;
    if (msg.role === "assistant") {
      const toolCalls: object[] = [];
      let text = "";
      for (const b of blocks) {
        if (b.type === "text") text += b.text;
        else if (b.type === "tool_use") {
          toolCalls.push({
            id: b.id,
            type: "function",
            function: { name: b.name, arguments: JSON.stringify(b.input) },
          });
        }
      }
      const content = text || (toolCalls.length ? null : "");
      messages.push({
        role: "assistant",
        content,
        tool_calls: toolCalls.length ? toolCalls : undefined,
      });
    } else {
      for (const b of blocks) {
        if (b.type === "tool_result") {
          messages.push({
            role: "tool",
            tool_call_id: b.tool_use_id,
            content: b.content,
          });
        } else if (b.type === "text") {
          messages.push({ role: "user", content: b.text });
        }
      }
    }
  }
  return messages;
}

export async function* stream(
  model: Model,
  context: Context,
  opts: { tools?: ToolDef[]; signal?: AbortSignal },
): AsyncGenerator<StreamEvent> {
  const url = `${model.baseUrl ?? "https://api.openai.com/v1"}/chat/completions`;
  const messages = contextToOpenAIMessages(context);

  const body: Record<string, unknown> = {
    model: model.model,
    stream: true,
    messages,
  };
  if (model.maxTokens) body.max_tokens = model.maxTokens;
  if (opts.tools?.length) {
    body.tools = opts.tools.map((t) => ({
      type: "function",
      function: {
        name: t.name,
        description: t.description,
        parameters: t.parameters,
      },
    }));
  }

  let response: Response;
  try {
    response = await fetch(url, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        authorization: `Bearer ${model.apiKey}`,
      },
      body: JSON.stringify(body),
      signal: opts.signal as AbortSignal | null,
    });
  } catch (e) {
    if (opts.signal?.aborted) {
      yield { type: "done", stopReason: "aborted" };
      return;
    }
    yield { type: "error", error: e as Error };
    return;
  }

  if (!response.ok || !response.body) {
    const text = await response.text().catch(() => "unknmown error");
    yield {
      type: "error",
      error: new Error(`API ${response.status}: ${text}`),
    };
  }

  const reader = response.body?.getReader();
  const decoder = new TextDecoder();
  let buf = "";
  let stopReason: "end_turn" | "tool_use" | "max_tokens" = "end_turn";
  const toolCallBuffers = new Map<
    number,
    { id: string; name: string; argsBuf: string }
  >();

  try {
    while (true) {
      const { done, value } = await reader!.read();
      if (done) break;
      buf += decoder.decode(value, { stream: true });

      let nl: number;
      while ((nl = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, nl).trim();
        buf = buf.slice(nl + 1);

        if (!line.startsWith("data")) continue;
        const data = line.slice(6); 
        if (data == "[DONE]") continue;

        const result = handleSSELine(data, toolCallBuffers); //data is a slice here ??
        if (result.textDelta)
          yield { type: "text_delta", delta: result.textDelta };
        if (result.stopReason) stopReason = result.stopReason;
      }
    }
  } catch (e) {
    if (opts.signal?.aborted) {
      yield { type: "done", stopReason: "aborted" };
      return;
    }
    yield { type: "error", error: e as Error };
    return;
  }

  for (const tc of flushToolCalls(toolCallBuffers)) {
    yield {
      type: "tool_call",
      id: tc.id,
      name: tc.name,
      args: tc.args as string,
    };
  }

  yield {
    type: "done",
    stopReason: opts.signal?.aborted ? "aborted" : stopReason,
  };
}

export function buildAssistantMessage(
  text: string,
  toolCalls: { id: string; name: string; args: unknown }[],
): Message {
  const content: ContentBlock[] = [];
  if (text) content.push({ type: "text", text });
  for (const tc of toolCalls) {
    content.push({
      type: "tool_use",
      id: tc.id,
      name: tc.name,
      input: tc.args as string,
    });
  }
  return { role: "assistant", content };
}

export function buildToolResultMessage(
  results: { tool_use_id: string; content: string }[],
): Message {
  return {
    role: "user",
    content: results.map((r) => ({
      type: "tool_result" as const,
      tool_use_id: r.tool_use_id,
      content: r.content,
    })),
  };
}



