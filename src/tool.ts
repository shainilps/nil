import type { AgentTool } from "./agent.js";
import { promises as fs } from "node:fs";
import * as path from "node:path";
import { exec } from "node:child_process";
import { promisify } from "node:util";
import * as os from "node:os";

const execAsync = promisify(exec);

const MAX_OUTPUT_LINES = 200;

let truncateCounter = 0;

async function truncateOutput(
  content: string,
  maxLines = MAX_OUTPUT_LINES,
): Promise<string> {
  const lines = content.split("\n");
  if (lines.length <= maxLines) return content;
  const kept = lines.slice(-maxLines).join("\n");
  const tmpPath = path.join(
    os.tmpdir(),
    `nil-output-${process.pid}-${truncateCounter++}.txt`,
  );
  await fs.writeFile(tmpPath, content, "utf-8");
  return `[output truncated: showing last ${maxLines} of ${lines.length} lines. full output: ${tmpPath}]\n${kept}`;
}

const readFile: AgentTool = {
  name: "read_file",
  description:
    "Read a text file. Files over 200 lines return only the last 200; use run_bash (e.g. sed -n '1,100p') for other ranges.",
  parameters: {
    type: "object",
    properties: {
      path: { type: "string", description: "File path, absolute or relative to cwd" },
    },
    required: ["path"],
  },
  execute: async (args) => {
    const { path: filePath } = args as { path: string };
    const content = await fs.readFile(filePath, "utf-8");
    return await truncateOutput(content);
  },
};

const writeFile: AgentTool = {
  needApproval: true,
  name: "write_file",
  description:
    "Create or fully overwrite a file (parent dirs are created). For small changes to existing files use edit.",
  parameters: {
    type: "object",
    properties: {
      path: { type: "string", description: "File path" },
      content: { type: "string", description: "Full file content" },
    },
    required: ["path", "content"],
  },
  execute: async (args) => {
    const { path: filePath, content } = args as {
      path: string;
      content: string;
    };
    await fs.mkdir(path.dirname(filePath) || ".", { recursive: true });
    await fs.writeFile(filePath, content, "utf-8");
    return `wrote ${filePath} (${content.length} chars)`;
  },
};

const edit: AgentTool = {
  needApproval: true,
  name: "edit",
  description:
    "Replace one exact occurrence of old_string with new_string in a file. old_string must match exactly (including whitespace) and be unique; add surrounding lines if needed.",
  parameters: {
    type: "object",
    properties: {
      path: { type: "string", description: "File path" },
      old_string: { type: "string", description: "Exact text to replace" },
      new_string: { type: "string", description: "Replacement text" },
    },
    required: ["path", "old_string", "new_string"],
  },
  execute: async (args) => {
    const {
      path: filePath,
      old_string,
      new_string,
    } = args as { path: string; old_string: string; new_string: string };
    const content = await fs.readFile(filePath, "utf-8");
    const count = content.split(old_string).length - 1;
    if (count === 0) throw new Error(`old_string not found in ${filePath}`);
    if (count > 1)
      throw new Error(
        `old_string matches ${count} places in ${filePath}, must be unique`,
      );
    const newContent = content.replace(old_string, () => new_string);
    await fs.writeFile(filePath, newContent, "utf-8");
    const lines = (s: string) => s.split("\n").length;
    return `edited ${filePath} (+${lines(new_string)} −${lines(old_string)})`;
  },
};

const runBash: AgentTool = {
  needApproval: true,
  name: "run_bash",
  description:
    "Run a shell command in cwd (30s timeout, non-interactive). Returns stdout/stderr; output over 200 lines is cut to the last 200 with the full log saved to a temp file.",
  parameters: {
    type: "object",
    properties: {
      command: { type: "string", description: "Shell command" },
    },
    required: ["command"],
  },
  execute: async (args: unknown, signal?: AbortSignal) => {
    const { command } = args as { command: string };
    try {
      const { stdout, stderr } = await execAsync(command, {
        maxBuffer: 1024 * 1024,
        timeout: 30000,
        signal,
      });
      const output = stderr ? `[stderr] ${stderr}\n[stdout] ${stdout}` : stdout;
      return await truncateOutput(output);
    } catch (e: unknown) {
      if (signal?.aborted) return "aborted";
      const err = e as NodeJS.ErrnoException & {
        code?: number;
        stdout?: string;
        stderr?: string;
      };
      return `[exit ${err.code}] ${err.stderr ?? ""}${err.stdout ?? ""}`;
    }
  },
};

export function builtinTools(): AgentTool[] {
  return [readFile, writeFile, edit, runBash];
}
