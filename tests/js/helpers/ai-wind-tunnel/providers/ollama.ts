/**
 * Ollama: `ollama serve` on a free port with its own model directory, or a running server
 * (AI_WT_OLLAMA_URL). Models are pulled through the executor (`ai.pullModel`), which is
 * part of what the cell tests.
 */

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { cacheDir } from "../paths.js";
import { assertNotReserved, freePort, spawnLogged, waitForHttp } from "../procs.js";
import { envUrl, type Driver, type Session } from "./types.js";

function ollamaBinary(): string | null {
  const candidates = [process.env.AI_WT_OLLAMA_BIN, path.join(os.homedir(), ".local", "bin", "ollama"), "/usr/local/bin/ollama", "/usr/bin/ollama", "/opt/homebrew/bin/ollama"];
  for (const c of candidates) if (c && existsSync(c)) return c;
  try {
    return execFileSync(process.platform === "win32" ? "where" : "which", ["ollama"], { encoding: "utf8" }).split("\n")[0].trim() || null;
  } catch {
    return null;
  }
}

/** The compute library Ollama's runner reports, from its log: `library=ROCm` / `CUDA` / `Vulkan` / `Metal` / `cpu`. */
export function ollamaBackendFromLog(log: string): string | undefined {
  const libs = [...log.matchAll(/library=(\w+)/g)].map((m) => m[1].toLowerCase());
  const gpu = libs.find((l) => l !== "cpu");
  return gpu ?? libs[0];
}

export const ollama: Driver = {
  id: "ollama",
  async plan(ctx) {
    const url = envUrl("AI_WT_OLLAMA_URL");
    if (url) return { variants: [{ label: "attach" }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_OLLAMA_URL" };
    if (!ollamaBinary()) return { skip: "no ollama binary (AI_WT_OLLAMA_BIN, PATH, ~/.local/bin)" };
    return { variants: [{ label: "auto" }] };
  },
  async open(ctx): Promise<Session> {
    const attached = envUrl("AI_WT_OLLAMA_URL");
    let baseUrl: string;
    let stop = async () => {};
    let pid: number | undefined;
    const logFile = path.join(ctx.runDir, "ollama.log");
    if (attached) {
      assertNotReserved(attached);
      baseUrl = attached;
    } else {
      const port = await freePort();
      // Cached models make reruns fast; AI_WT_FRESH=1 pulls into an empty directory.
      const models = process.env.AI_WT_FRESH ? mkdtempSync(path.join(os.tmpdir(), "ai-wt-ollama-")) : path.join(cacheDir(), "ollama");
      mkdirSync(models, { recursive: true });
      const s = spawnLogged(ollamaBinary()!, ["serve"], logFile, { OLLAMA_HOST: `127.0.0.1:${port}`, OLLAMA_MODELS: models, OLLAMA_KEEP_ALIVE: "30m" });
      baseUrl = `http://127.0.0.1:${port}`;
      pid = s.proc.pid;
      stop = () => s.stop();
      await waitForHttp(`${baseUrl}/api/version`, { timeoutMs: 60_000, alive: s.running, label: "ollama serve" });
    }
    const version = await fetch(`${baseUrl}/api/version`).then((r) => r.json()).then((j: any) => j.version as string).catch(() => undefined);
    ctx.log(`ollama ${version ?? "?"} at ${baseUrl}`);
    return {
      kind: "OLLAMA",
      mode: attached ? "attach" : "spawned",
      version,
      pullsThroughExecutor: true,
      async serve(model) {
        return {
          baseUrl,
          remoteModel: model.remoteModel,
          pids: () => (pid ? [pid] : []),
          backend: () => {
            try {
              return ollamaBackendFromLog(readFileSync(logFile, "utf8"));
            } catch {
              return undefined;
            }
          },
          async release() {
            // Unload, so the next model's memory peak is its own.
            await fetch(`${baseUrl}/api/generate`, { method: "POST", body: JSON.stringify({ model: model.remoteModel, keep_alive: 0 }) }).catch(() => {});
          },
        };
      },
      close: stop,
    };
  },
};
