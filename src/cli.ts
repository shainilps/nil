import { runAgent } from "./agent.js";
import { listModels } from "./llm.js";
import type { Context, Message, Model, ModelInfo } from "./llm.js";
import { builtinTools } from "./tool.js";
import { Tui, color } from "./tui.js";
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
  const tui = new Tui(path.join(SESSION_DIR, "history"));
  tui.printNotice(
    [
      context.messages.length
        ? `resumed session ${sessionId} (${context.messages.length} messages)`
        : `session ${sessionId}`,
      model.model,
      args.auto ? "auto" : "manual",
      "/help for commands",
    ].join(" · ") + "\n",
  );

  const tools = builtinTools();

  const alwaysAllow = new Set<string>();
  const approve = async (name: string, toolArgs: unknown) => {
    if (args.auto || alwaysAllow.has(name)) return true;
    tui.printText(preview(name, toolArgs));
    const answer = await tui.confirm("  allow? [y/n/a] ");
    if (answer === "a") alwaysAllow.add(name);
    return answer === "y" || answer === "a";
  };

  let models: ModelInfo[] | null = null;
  const getModels = async () => (models ??= await listModels(model));

  const commands: Record<string, (arg: string) => Promise<void>> = {
    async help() {
      tui.printText(
        [
          "/models [filter]  list chat models (tool support shown when known)",
          "/model [id]       show or switch the current model",
          "/mode [auto|manual]  show or switch tool approval mode",
          "/help             this list",
          "",
        ].join("\n"),
      );
    },
    async mode(arg) {
      if (arg === "auto") args.auto = true;
      else if (arg === "manual") {
        args.auto = false;
        alwaysAllow.clear(); 
      } else if (arg)
        return tui.printText("usage: /mode [auto|manual]\n");
      tui.printText(`mode: ${args.auto ? "auto" : "manual"}\n`);
    },
    async models(filter) {
      const list = (await getModels()).filter((m) => m.id.includes(filter));
      const usable = list.filter((m) => m.tools !== false);
      for (const m of usable)
        tui.printText(`${m.id === model.model ? "*" : " "} ${m.id}\n`);
      const hidden = list.length - usable.length;
      tui.printText(
        `${usable.length} model${usable.length === 1 ? "" : "s"}${hidden ? ` (${hidden} without tool support hidden)` : ""}\n`,
      );
    },
    async model(id) {
      if (!id) return tui.printText(`current model: ${model.model}\n`);
      let info: ModelInfo | undefined;
      try {
        info = (await getModels()).find((m) => m.id === id);
        if (!info)
          return tui.printText(`unknown model "${id}", see /models ${id}\n`);
      } catch (e) {
        tui.printText(`couldn't verify model (${(e as Error).message})\n`);
      }
      if (info?.tools === false)
        tui.printText("warning: this model doesn't support tools\n");
      model.model = id;
      tui.printText(`switched to ${id}\n`);
    },
  };

  tui.onPrompt(async (text) => {
    const startedAt = Date.now();
    tui.setBusy(true);
    try {
      if (text.startsWith("/")) {
        const [name = "", ...rest] = text.slice(1).split(/\s+/);
        const command = Object.hasOwn(commands, name) ? commands[name] : undefined;
        if (command) await command(rest.join(" "));
        else tui.printText(`unknown command /${name}, try /help\n`);
        return;
      }

      context.messages.push({ role: "user", content: text });
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
          case "tool_start":
            tui.spin("running");
            break;
          case "tool_result":
            tui.printToolResult(ev.name, ev.result);
            break;
          case "turn_end":
            if (ev.stopReason === "max_tokens")
              tui.printNotice("[output truncated by max_tokens]", "yellow");
            if (ev.stopReason === "error")
              tui.printNotice("[error occurred]", "red");
            tui.printTurnEnd(turnStats(Date.now() - startedAt, ev.usage));
            break;
        }
      }

      await persistSession(context.messages, sessionFile);
    } catch (e) {
      tui.printNotice(`[error] ${(e as Error).message}`, "red");
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

function turnStats(ms: number, usage: { input: number; output: number }): string {
  const k = (n: number) => (n >= 1000 ? `${(n / 1000).toFixed(1)}k` : `${n}`);
  const time = `${(ms / 1000).toFixed(1)}s`;
  if (!usage.input && !usage.output) return time;
  return `${time} · ↑${k(usage.input)} ↓${k(usage.output)} tokens`;
}

function preview(name: string, toolArgs: unknown): string {
  if (name !== "edit") return "";
  const a = toolArgs as Record<string, string>;
  const lines = (text: string | undefined, sign: string, tint: (s: string) => string) =>
    String(text ?? "")
      .split("\n")
      .map((l) => tint(`  ${sign} ${l}`))
      .join("\n");
  return `${lines(a.old_string, "-", color.red)}\n${lines(a.new_string, "+", color.green)}\n`;
}

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
