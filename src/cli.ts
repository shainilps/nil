import { runAgent } from "./agent.js";
import type { Context, Message, Model } from "./llm.js";
import { builtinTools } from "./tool.js";
import { Tui } from "./tui.js";
import { promises as fs } from "node:fs";

import * as path from "node:path";
import * as os from "node:os";

const SESSION_DIR = path.join(os.homedir(), ".nanopi");
const SESSION_FILE = path.join(SESSION_DIR, "session.jsonl");
const SYSTEM_PROMPT =
  "You are a coding assistant. Use the provided tools to read/write files\
   and execute commands to complete tasks.\
   Read before modifying, and after making changes you can run commands to verify";

let persistedCount = 0;

async function main() {
  const apiKey = process.env.NANOPI_API_KEY;
  if (!apiKey) {
    console.error("NANOPI_API_KEY is not set");
    process.exit(1);
  }

  const model: Model = {
    apiKey,
    model: process.env.NANOPI_MODEL ?? "glm-5.2",
    baseUrl: process.env.NANOPI_BASE_URL ?? "https://api.openai.com/v1",
    maxTokens: 4096,
  };

  const context: Context = {
    systemPrompt: SYSTEM_PROMPT,
    messages: await loadSession(),
  };

  const tools = builtinTools();
  const tui = new Tui();

  tui.onPrompt(async (text) => {
    try {
      context.messages.push({ role: "user", content: text });
      tui.setBusy(true);
      const ctrl = new AbortController();
      tui.onAbort(() => ctrl.abort());

      for await (const ev of runAgent(model, context, tools, ctrl.signal)) {
        switch (ev.type) {
          case "assistant_text":
            tui.printText(ev.delta);
            break;
          case "tool_call":
            tui.printToolCall(ev.name, ev.args);
            break;
          case "tool_result":
            tui.printToolResult(ev.name, ev.result);
            break;
          case "turn_end":
            if (ev.stopReason === "max_tokens")
              tui.printText("\n[output truncated by max_tokens]");
            if (ev.stopReason === "error") tui.printText("\n[error occurred]");
            tui.printTurnEnd();
            break;
        }
      }

      await persistSession(context.messages);
    } catch (e) {
      console.error(`\n[error] ${(e as Error).message}`);
    } finally {
      tui.setBusy(false);
    }
  });
  tui.start();
}

export async function loadSession(
  file: string = SESSION_FILE,
): Promise<Message[]> {
  try {
    const data = await fs.readFile(file, "utf-8");
    const lines = data.trim().split("\n").filter(Boolean);
    const messages = lines.flatMap((line) => {
      try {
        return [JSON.parse(line) as Message];
      } catch {
        return [];
      }
    });
    persistedCount = messages.length;
    return messages;
  } catch {
    return [];
  }
}

export async function persistSession(
  messages: Message[],
  file: string = SESSION_FILE,
): Promise<void> {
  await fs.mkdir(path.dirname(file) || ".", { recursive: true });
  const newMessages = messages.slice(persistedCount);
  for (const msg of newMessages) {
    await fs.appendFile(file, JSON.stringify(msg) + "\n", "utf-8");
  }
  persistedCount = messages.length;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
