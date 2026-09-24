import { runAgent } from "./agent.js";
import type { Context, Message, Model } from "./llm.js";
import { builtinTools } from "./tool.js";
import { Tui } from "./tui.js";
import { promises as fs } from "node:fs";
import { randomBytes } from "node:crypto";

import * as path from "node:path";

const SESSION_DIR = "./.nil";
const SYSTEM_PROMPT = [
  `You are nil, a coding agent. Working directory: ${process.cwd()}`,
  "Use tools to inspect files and run commands; never guess file contents.",
  "- Read a file before editing it. Prefer edit over write_file for existing files.",
  "- After changes, verify (build, test, or re-read) when practical.",
  "- Be brief. Say what you changed; don't paste whole files back.",
].join("\n");

let persistedCount = 0;

async function main() {
  const apiKey = process.env.NIL_API_KEY;
  if (!apiKey) {
    console.error("NIL_API_KEY is not set");
    process.exit(1);
  }

  const model: Model = {
    apiKey,
    model: process.env.NIL_MODEL ?? "gpt-5.4-nano",
    baseUrl: process.env.NIL_BASE_URL ?? "https://api.openai.com/v1",
    maxTokens: 4096,
  };

  const sessionFile = await resolveSession(process.argv.slice(2));
  const context: Context = {
    systemPrompt: SYSTEM_PROMPT,
    messages: await loadSession(sessionFile),
  };
  const sessionId = path.basename(sessionFile, ".jsonl");
  console.log(
    context.messages.length
      ? `resumed session ${sessionId} (${context.messages.length} messages)`
      : `session ${sessionId}`,
  );

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

      await persistSession(context.messages, sessionFile);
    } catch (e) {
      console.error(`\n[error] ${(e as Error).message}`);
    } finally {
      tui.setBusy(false);
    }
  });
  tui.start();
}

async function resolveSession(argv: string[]): Promise<string> {
  const i = argv.indexOf("--resume");
  const known = i === -1 ? [] : argv.slice(i, i + 2).filter((a, j) => j === 0 || !a.startsWith("-"));
  const unknown = argv.find((a) => !known.includes(a));
  if (unknown) {
    console.error(`unknown argument: ${unknown}\nusage: nil [--resume [id]]`);
    process.exit(1);
  }
  if (i === -1) {
    const id = randomBytes(4).toString("hex");
    return path.join(SESSION_DIR, `${id}.jsonl`);
  }

  const id = argv[i + 1];
  if (id && !id.startsWith("-")) {
    let names: string[] = [];
    try {
      names = await fs.readdir(SESSION_DIR);
    } catch {}
    const matches = names.filter((n) => n.startsWith(id) && n.endsWith(".jsonl"));
    if (matches.length !== 1) {
      console.error(
        matches.length
          ? `session id "${id}" is ambiguous: ${matches.map((m) => path.basename(m, ".jsonl")).join(", ")}`
          : `session ${id} not found in ${SESSION_DIR} (sessions are saved after the first message)`,
      );
      process.exit(1);
    }
    return path.join(SESSION_DIR, matches[0]!);
  }

  const latest = await latestSession();
  if (!latest) {
    console.error(`no sessions to resume in ${SESSION_DIR}`);
    process.exit(1);
  }
  return latest;
}

async function latestSession(): Promise<string | null> {
  let names: string[];
  try {
    names = (await fs.readdir(SESSION_DIR)).filter((n) => n.endsWith(".jsonl"));
  } catch {
    return null;
  }
  let best: { file: string; mtime: number } | null = null;
  for (const n of names) {
    const file = path.join(SESSION_DIR, n);
    const { mtimeMs } = await fs.stat(file);
    if (!best || mtimeMs > best.mtime) best = { file, mtime: mtimeMs };
  }
  return best?.file ?? null;
}

export async function loadSession(file: string): Promise<Message[]> {
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
  file: string,
): Promise<void> {
  await fs.mkdir(path.dirname(file) || ".", { recursive: true });
  // compaction shrank the history: rewrite the file instead of appending
  if (messages.length < persistedCount) {
    await fs.writeFile(file, "", "utf-8");
    persistedCount = 0;
  }
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
