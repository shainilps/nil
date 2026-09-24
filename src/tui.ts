import * as readline from "readline";

export class Tui {
  private rl: readline.Interface | null = null;
  private onPromptCb: ((text: string) => void) | null = null;
  private onAbortCb: (() => void) | null = null;
  private aborted = false;
  private busy = false;

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
    });
    process.stdin.on(
      "keypress",
      (_ch: string, key: { ctrl?: boolean; name?: string } | undefined) => {
        if (this.busy && key?.ctrl && key?.name === "c" && !this.aborted) {
          this.aborted = true;
          this.onAbortCb?.();
        }
      },
    );

    this.prompt();
  }
  private prompt(): void {
    if (!this.rl) return;
    if (this.busy) return;
    this.aborted = false;
    this.rl.question("> ", (answer) => {
      const text = answer.trim();
      if (text) {
        this.onPromptCb?.(text);
      } else {
        this.prompt();
      }
    });
  }

  setBusy(busy: boolean): void {
    this.busy = busy;
    if (!busy) this.prompt();
  }

  printText(delta: string): void {
    process.stdout.write(delta);
  }

  printToolCall(name: string, args: unknown): void {
    process.stdout.write(`\n[tool: ${name}] ${JSON.stringify(args)}\n`);
  }

  printToolResult(name: string, result: string): void {
    process.stdout.write(`[result: ${name}] ${result}\n`);
  }

  printTurnEnd(): void {
    process.stdout.write("\n");
  }

  stop(): void {
    this.rl?.close();
    this.rl = null;
    process.stdin.removeAllListeners("keypress");
  }
}
