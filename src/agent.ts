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

export async function* runAgent(
  model: Model,
  context: Context,
  tools: AgentTool[],
  signal: AbortSignal,
): AsyncGenerator<AgentEvent> {
  const toolMap = new Map(tools.map((t) => [t.name, t]));
  const toolDefs = tools.map((t) => ({
    name: t.name,
    description: t.description,
    parameters: t.parameters,
  }));

  while (true) {
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
        yield { type: "tool_call", id: ev.id, name: ev.name, args: ev.args };
      } else if (ev.type === "done") {
        stopReason = ev.stopReason;
      }
    }

    context.messages.push(buildAssistantMessage(text, toolCalls));

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
      const tool = toolMap.get(tc.name);
      let result: string;
      if (!tool) {
        result = `error: tool "${tc.name}" not found`;
      } else {
        try {
          result = await tool.execute(tc.args, signal);
        } catch (e) {
          result = `error: ${(e as Error).message}`;
        }
      }
      results.push({ tool_use_id: tc.id, content: result });
      yield { type: "tool_result", id: tc.id, name: tc.name, result };
    }

    context.messages.push(buildToolResultMessage(results));
  }
}
