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
    `nanopi-output-${process.pid}-${truncateCounter++}.txt`,
  );
  await fs.writeFile(tmpPath, content, "utf-8");
  return `[output truncated: showing last ${maxLines} of ${lines.length} lines. full output: ${tmpPath}]\n${kept}`;
}

const readFile: AgentTool = {
  name: "read_file",
  description: "read file: reads the path and provide output within 200 lines",
  parameters: {
    type: "object",
    properties: { path: { type: "string", description: "path of the file" } },
    required: ["path"],
  },
  execute: async (args) => {
    const { path: filePath } = args as { path: string };
    const content = await fs.readFile(filePath, "utf-8");
    return await truncateOutput(content);
  },
};

const writeFile: AgentTool = {
  name: "write_file",
  description: "write the content to the file",
  parameters: {
    type: "object",
    properties: {
      path: { type: "string", description: "path of the file" },
      content: {
        type: "string",
        description: "content that needs to be written",
      },
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
  name: "edit",
  description: "edits the file with new content replacing with old content",
  parameters: {
    type: "object",
    properties: {
      path: { type: "string", description: "file path" },
      old_string: {
        type: "string",
        description: "content to be replaced",
      },
      new_string: { type: "string", description: "content to be added" },
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
    return `edited ${filePath}: replaced ${old_string.length} chars`;
  },
};

const runBash: AgentTool = {
  name: "run_bash",
  description:
    "run the bash command and provide the output in 200 lines or else return the file path",
  parameters: {
    type: "object",
    properties: {
      command: {
        type: "string",
        description: "bash command that needs to be run",
      },
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
