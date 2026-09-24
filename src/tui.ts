import * as readline from "readline";
import * as fs from "node:fs";
import * as path from "node:path";

const useColor = process.stdout.isTTY && !process.env.NO_COLOR;
const ansi = (code: string) => (s: string) =>
  useColor ? `\x1b[${code}m${s}\x1b[0m` : s;

export const color = {
  dim: ansi("2"),
  bold: ansi("1"),
  red: ansi("31"),
  green: ansi("32"),
  yellow: ansi("33"),
  cyan: ansi("36"),
};

const SPINNER = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
const RESULT_PREVIEW_LINES = 3;
const HISTORY_SIZE = 200;
// show elapsed seconds once waiting gets noticeable
const SHOW_ELAPSED_AFTER_MS = 3000;

export class Tui {
  private rl: readline.Interface | null = null;
  private onPromptCb: ((text: string) => void) | null = null;
  private onAbortCb: (() => void) | null = null;
  private aborted = false;
  private busy = false;
  private confirming = false;
  private confirmCancelled = false;
  private spinner: NodeJS.Timeout | null = null;
  private atLineStart = true;

  // historyFile: where up/down-arrow prompt history is kept between runs
  constructor(private historyFile?: string) {}

  onPrompt(cb: (text: string) => void): void {
    this.onPromptCb = cb;
  }

  onAbort(cb: () => void): void {
    this.onAbortCb = cb;
  }

  start(): void {
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      history: this.loadHistory(),
      historySize: HISTORY_SIZE,
      removeHistoryDuplicates: true,
    });
    // without a SIGINT listener readline closes itself on Ctrl+C
    this.rl.on("SIGINT", () => {
      if (!this.busy) {
        this.stop();
        process.exit(0);
      }
      if (this.aborted) return;
      this.aborted = true;
      this.onAbortCb?.();
      // finish the open confirm() question so readline is free again
      if (this.confirming) {
        this.confirmCancelled = true;
        this.rl?.write("\n");
      }
    });

    this.prompt();
  }

  private prompt(): void {
    if (!this.rl) return;
    if (this.busy) return;
    this.aborted = false;
    this.rl.question(color.bold(color.cyan("❯ ")), (answer) => {
      const text = answer.trim();
      if (text) {
        this.saveHistory();
        this.onPromptCb?.(text);
      } else {
        this.prompt();
      }
    });
  }

  setBusy(busy: boolean): void {
    this.busy = busy;
    if (busy) {
      this.startSpinner();
    } else {
      this.stopSpinner();
      this.prompt();
    }
  }

  confirm(question: string): Promise<string> {
    this.stopSpinner();
    return new Promise((resolve) => {
      if (!this.rl) return resolve("n");
      this.confirming = true;
      this.confirmCancelled = false;
      this.rl.question(color.yellow(question), (answer) => {
        this.confirming = false;
        // keep y/n answers out of the up-arrow history
        const history = this.history();
        if (answer && history[0] === answer) history.shift();
        this.atLineStart = true;
        resolve(this.confirmCancelled ? "n" : answer.trim().toLowerCase());
      });
    });
  }

  printText(text: string): void {
    this.write(text);
  }

  printNotice(text: string, tint: keyof typeof color = "dim"): void {
    this.newline();
    this.write(color[tint](text) + "\n");
  }

  printToolCall(name: string, args: unknown): void {
    this.newline();
    const width = process.stdout.columns || 80;
    const summary = truncate(summarize(name, args), width - name.length - 4);
    this.write(`${color.cyan("●")} ${color.bold(name)} ${summary}\n`);
  }

  printToolResult(_name: string, result: string): void {
    this.newline();
    const failed = result.startsWith("error:") || result.startsWith("[exit");
    const tint = failed ? color.red : color.dim;
    const lines = result.trimEnd().split("\n");
    const shown = result.trim() ? lines.slice(0, RESULT_PREVIEW_LINES) : ["(no output)"];
    const width = (process.stdout.columns || 80) - 4;
    shown.forEach((line, i) =>
      this.write(`${i === 0 ? "  ⎿ " : "    "}${tint(truncate(line, width))}\n`),
    );
    const hidden = lines.length - shown.length;
    if (result.trim() && hidden > 0)
      this.write(`    ${color.dim(`… ${hidden} more lines`)}\n`);
    // the next model call is pending now
    if (this.busy) this.startSpinner();
  }

  // show a spinner with a label, e.g. while a tool runs
  spin(label: string): void {
    this.stopSpinner();
    this.startSpinner(label);
  }

  printTurnEnd(stats?: string): void {
    this.newline();
    if (stats) this.write(color.dim(`✓ ${stats}`) + "\n");
    this.write("\n");
  }

  stop(): void {
    this.stopSpinner();
    this.rl?.close();
    this.rl = null;
  }

  // every visible write goes through here, so the spinner never gets mixed in
  private write(s: string): void {
    if (!s) return;
    this.stopSpinner();
    process.stdout.write(s);
    this.atLineStart = s.endsWith("\n");
  }

  private newline(): void {
    if (!this.atLineStart) this.write("\n");
  }

  private startSpinner(label = "thinking"): void {
    if (!process.stdout.isTTY || this.spinner) return;
    this.newline();
    let i = 0;
    const started = Date.now();
    const draw = () => {
      const ms = Date.now() - started;
      const elapsed = ms >= SHOW_ELAPSED_AFTER_MS ? ` ${Math.floor(ms / 1000)}s` : "";
      process.stdout.write(
        `\r${color.cyan(SPINNER[i++ % SPINNER.length]!)} ${color.dim(`${label}…${elapsed}`)}`,
      );
    };
    draw();
    this.spinner = setInterval(draw, 80);
  }

  // readline keeps history newest-first; it isn't in the public typings
  private history(): string[] {
    return (this.rl as unknown as { history?: string[] } | null)?.history ?? [];
  }

  private loadHistory(): string[] {
    if (!this.historyFile) return [];
    try {
      const lines = fs.readFileSync(this.historyFile, "utf-8").split("\n");
      return lines.filter(Boolean).slice(-HISTORY_SIZE).reverse();
    } catch {
      return [];
    }
  }

  private saveHistory(): void {
    if (!this.historyFile) return;
    try {
      fs.mkdirSync(path.dirname(this.historyFile), { recursive: true });
      const lines = [...this.history()].reverse();
      fs.writeFileSync(this.historyFile, lines.join("\n") + "\n");
    } catch {
      // history is a convenience; never fail a prompt over it
    }
  }

  private stopSpinner(): void {
    if (!this.spinner) return;
    clearInterval(this.spinner);
    this.spinner = null;
    process.stdout.write("\r\x1b[K");
  }
}

function summarize(name: string, args: unknown): string {
  const a = (args ?? {}) as Record<string, unknown>;
  const str = (v: unknown) => (typeof v === "string" ? v : "");
  switch (name) {
    case "run_bash":
      return str(a.command).split("\n")[0]!;
    case "write_file":
      return `${str(a.path)} (${str(a.content).split("\n").length} lines)`;
    case "read_file":
    case "edit":
      return str(a.path);
    default:
      return JSON.stringify(args);
  }
}

// cut by visible characters; good enough for plain tool text
function truncate(s: string, max: number): string {
  if (max < 10) max = 10;
  return s.length > max ? s.slice(0, max - 1) + "…" : s;
}
