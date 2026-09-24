import {
  stream,
  buildAssistantMessage,
  buildToolResultMessage,
  type Model,
  type Context,
} from "./llm.js";

export type AgentTool = {
  name: string;
  description: string;
  parameters: object;
  needApproval?: boolean;
  execute: (args: unknown, signal?: AbortSignal) => Promise<string>;
};

type AgentEvent =
  | { type: "assistant_text"; delta: string }
  | { type: "tool_call"; id: string; name: string; args: unknown }
  | { type: "tool_result"; id: string; name: string; result: string }
  | {
      type: "turn_end";
      stopReason: "end_turn" | "max_tokens" | "aborted" | "error";
    };

const COMPACT_THRESHOLD = 50;
const KEEP_RECENT = 20;

async function compactContext(
  model: Model,
  context: Context,
  signal: AbortSignal,
): Promise<void> {
  if (signal?.aborted) return;
  if (context.messages.length < COMPACT_THRESHOLD) return;

  const oldMessages = context.messages.slice(0, -KEEP_RECENT);
  const recentMessages = context.messages.slice(-KEEP_RECENT);

  const conversationText = oldMessages
    .map(
      (m) =>
        `${m.role}: ${typeof m.content === "string" ? m.content : JSON.stringify(m.content)}`,
    )
    .join("\n");

  const summaryContext: Context = {
    systemPrompt: "summarise this context",
    messages: [{ role: "user", content: conversationText }],
  };

  let summary = "";
  let failed = false;
  for await (const ev of stream(model, summaryContext, { signal })) {
    if (ev.type === "text_delta") summary += ev.delta;
    else if (
      ev.type === "error" ||
      (ev.type === "done" && ev.stopReason === "aborted")
    ) {
      failed = true;
      break;
    }
  }

  if (failed || !summary) return;

  context.messages = [
    { role: "user", content: `[context summary]\n${summary}` },
    ...recentMessages,
  ];
}

export async function* runAgent(
  model: Model,
  context: Context,
  tools: AgentTool[],
  signal: AbortSignal,
  approve?: (name: string, args: unknown) => Promise<boolean>,
): AsyncGenerator<AgentEvent> {
  const toolMap = new Map(tools.map((t) => [t.name, t]));
  const toolDefs = tools.map((t) => ({
    name: t.name,
    description: t.description,
    parameters: t.parameters,
  }));

  while (true) {
    await compactContext(model, context, signal);

    let text = "";
    let stopReason: "end_turn" | "tool_use" | "max_tokens" | "aborted" =
      "end_turn";
    const toolCalls: { id: string; name: string; args: unknown }[] = [];

    for await (const ev of stream(model, context, {
      tools: toolDefs,
      signal,
    })) {
      if (ev.type === "text_delta") {
        text += ev.delta;
        yield { type: "assistant_text", delta: ev.delta };
      } else if (ev.type === "tool_call") {
        toolCalls.push({ id: ev.id, name: ev.name, args: ev.args });
      } else if (ev.type === "done") {
        stopReason = ev.stopReason;
        if (ev.stopReason === "aborted") {
          context.messages.push(buildAssistantMessage(text, []));
          yield { type: "turn_end", stopReason: "aborted" };
          return;
        }
      } else if (ev.type === "error") {
        context.messages.push(buildAssistantMessage(text, []));
        yield {
          type: "assistant_text",
          delta: `\n[error] ${ev.error.message}`,
        };
        yield { type: "turn_end", stopReason: "error" };
        return;
      }
    }

    context.messages.push(buildAssistantMessage(text, toolCalls));

    if (stopReason === "max_tokens" && toolCalls.length > 0) {
      const results = toolCalls.map((tc) => ({
        tool_use_id: tc.id,
        content: `error: output truncated by max_tokens, tool "${tc.name}" args may be incomplete.`,
      }));
      context.messages.push(buildToolResultMessage(results));
      for (let i = 0; i < toolCalls.length; i++) {
        yield { type: "tool_call", ...toolCalls[i]! };
        yield {
          type: "tool_result",
          id: toolCalls[i]!.id,
          name: toolCalls[i]!.name,
          result: results[i]!.content,
        };
      }
      continue;
    }

    const reason = stopReason === "tool_use" ? "end_turn" : stopReason;
    if (toolCalls.length === 0) {
      yield { type: "turn_end", stopReason: reason };
      return;
    }

    if (toolCalls.length === 0) {
      return;
    }

    const results: { tool_use_id: string; content: string }[] = [];
    for (const tc of toolCalls) {
      yield { type: "tool_call", ...tc };
      const tool = toolMap.get(tc.name);
      let result: string;
      if (!tool) {
        result = `error: tool "${tc.name}" not found`;
      } else if (
        tool.needApproval &&
        approve &&
        !(await approve(tc.name, tc.args))
      ) {
        result = "error: user denied this tool call";
      } else {
        try {
          result = await tool.execute(tc.args, signal);
        } catch (e) {
          result = `error: ${(e as Error).message}`;
        }
      }
      results.push({ tool_use_id: tc.id, content: result });
      yield { type: "tool_result", id: tc.id, name: tc.name, result };

      if (signal?.aborted) break;
    }

    for (const tc of toolCalls.slice(results.length)) {
      results.push({ tool_use_id: tc.id, content: "error: aborted" });
      yield { type: "tool_call", ...tc };
      yield {
        type: "tool_result",
        id: tc.id,
        name: tc.name,
        result: "error: aborted",
      };
    }

    context.messages.push(buildToolResultMessage(results));
  }
}
