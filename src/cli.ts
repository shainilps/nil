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

  const args = parseArgs(process.argv.slice(2));
  const sessionFile = await resolveSession(args.resume);
  const context: Context = {
    systemPrompt: SYSTEM_PROMPT + (await loadAgentsMd()),
    messages: await loadSession(sessionFile),
  };
  const sessionId = path.basename(sessionFile, ".jsonl");
  console.log(
    context.messages.length
      ? `resumed session ${sessionId} (${context.messages.length} messages)`
      : `session ${sessionId}`,
    `(mode: ${args.auto ? "auto" : "manual"})`,
  );

  const tools = builtinTools();
  const tui = new Tui();

  const alwaysAllow = new Set<string>();
  const approve = async (name: string, toolArgs: unknown) => {
    if (args.auto || alwaysAllow.has(name)) return true;
    tui.printText(preview(name, toolArgs));
    const answer = await tui.confirm("allow? [y/n/a] ");
    if (answer === "a") alwaysAllow.add(name);
    return answer === "y" || answer === "a";
  };

  tui.onPrompt(async (text) => {
    try {
      context.messages.push({ role: "user", content: text });
      tui.setBusy(true);
      const ctrl = new AbortController();
      tui.onAbort(() => ctrl.abort());

      for await (const ev of runAgent(
        model,
        context,
        tools,
        ctrl.signal,
        approve,
      )) {
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

async function loadAgentsMd(): Promise<string> {
  try {
    const text = await fs.readFile("AGENTS.md", "utf-8");
    return `\n\n# Project instructions (AGENTS.md)\n${text}`;
  } catch {
    return "";
  }
}

function preview(name: string, toolArgs: unknown): string {
  const a = toolArgs as Record<string, string>;
  if (name === "run_bash") return `  $ ${a.command}\n`;
  if (name === "write_file")
    return `  write ${a.path} (${String(a.content ?? "").split("\n").length} lines)\n`;
  if (name === "edit") {
    const lines = (text: string | undefined, sign: string) =>
      String(text ?? "")
        .split("\n")
        .map((l) => `  ${sign} ${l}`)
        .join("\n");
    return `  edit ${a.path}\n${lines(a.old_string, "-")}\n${lines(a.new_string, "+")}\n`;
  }
  return "";
}

// nil [--auto] [--resume [id]]
function parseArgs(argv: string[]): {
  auto: boolean;
  resume: string | true | undefined;
} {
  let auto = false;
  let resume: string | true | undefined;
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]!;
    if (a === "--auto") auto = true;
    else if (a === "--resume") {
      const next = argv[i + 1];
      if (next && !next.startsWith("-")) {
        resume = next;
        i++;
      } else resume = true;
    } else {
      console.error(`unknown argument: ${a}\nusage: nil [--auto] [--resume [id]]`);
      process.exit(1);
    }
  }
  return { auto, resume };
}

// undefined: new session; true: latest in SESSION_DIR; string: id prefix
async function resolveSession(resume: string | true | undefined): Promise<string> {
  if (resume === undefined) {
    const id = randomBytes(4).toString("hex");
    return path.join(SESSION_DIR, `${id}.jsonl`);
  }

  if (typeof resume === "string") {
    const id = resume;
    let names: string[] = [];
    try {
      names = await fs.readdir(SESSION_DIR);
    } catch {}
    const matches = names.filter(
      (n) => n.startsWith(id) && n.endsWith(".jsonl"),
    );
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
