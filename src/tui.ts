import * as readline from "readline";

export class Tui {
  private rl: readline.Interface | null = null;
  private onPromptCb: ((text: string) => void) | null = null;
  private onAbortCb: (() => void) | null = null;
  private aborted = false;
  private busy = false;
  private confirming = false;
  private confirmCancelled = false;

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

  confirm(question: string): Promise<string> {
    return new Promise((resolve) => {
      if (!this.rl) return resolve("n");
      this.confirming = true;
      this.confirmCancelled = false;
      this.rl.question(question, (answer) => {
        this.confirming = false;
        resolve(this.confirmCancelled ? "n" : answer.trim().toLowerCase());
      });
    });
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
  }
}
