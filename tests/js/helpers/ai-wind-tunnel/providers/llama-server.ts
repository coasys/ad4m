/**
 * llama-server (llama.cpp): one process per model, from the manifest's builds for this
 * platform (downloaded and checked like the executor's local setup will), from a binary
 * (AI_WT_LLAMA_SERVER, backend in AI_WT_LLAMA_BACKEND), or a running server
 * (AI_WT_LLAMA_SERVER_URL, serving whatever model it was started with).
 */

import { existsSync, readdirSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { platformKey } from "../fingerprint.js";
import { installModel, installRuntime } from "../install.js";
import { assertNotReserved, freePort, spawnLogged, waitForHttp } from "../procs.js";
import { envUrl, type Driver, type RunContext, type Session, type Variant } from "./types.js";

/**
 * Library directories a ROCm build needs on Linux: the build's HIP backend links the system
 * ROCm runtime (libamdhip64, rocBLAS, hipBLAS), which a ROCm install often keeps off the
 * default search path. Without them llama-server finds no device and runs on the CPU.
 * AI_WT_ROCM_LIB overrides.
 */
export function rocmLibDirs(): string[] {
  if (process.env.AI_WT_ROCM_LIB) return process.env.AI_WT_ROCM_LIB.split(":");
  const dirs = ["/opt/rocm/lib"];
  try {
    for (const d of readdirSync("/opt/rocm")) if (/^core-[\d.]+$/.test(d)) dirs.push(path.join("/opt/rocm", d, "lib"));
  } catch {}
  return dirs.filter((d) => existsSync(d));
}

/** The manifest builds for this machine whose backend it can use, narrowed by `--llama-builds`. */
export function llamaVariants(ctx: Pick<RunContext, "fp" | "manifest" | "llamaBuilds">): Variant[] {
  const rt = ctx.manifest.runtimes["llama-server"];
  const prefix = `${platformKey(ctx.fp)}-`;
  const all = Object.keys(rt?.builds ?? {})
    .filter((k) => k.startsWith(prefix))
    .map((k) => ({ runtimeBuild: k, backend: k.slice(prefix.length), label: k }))
    .filter((v) => (ctx.fp.backends as string[]).includes(v.backend));
  if (ctx.llamaBuilds?.length) return all.filter((v) => ctx.llamaBuilds!.includes(v.runtimeBuild) || ctx.llamaBuilds!.includes(v.backend));
  // Default: the GPU builds when there is a GPU backend, the CPU build otherwise.
  const gpu = all.filter((v) => v.backend !== "cpu");
  return gpu.length ? gpu : all;
}

export const llamaServer: Driver = {
  id: "llama-server",
  async plan(ctx) {
    if (envUrl("AI_WT_LLAMA_SERVER_URL")) return { variants: [{ label: "attach", backend: process.env.AI_WT_LLAMA_BACKEND }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_LLAMA_SERVER_URL" };
    if (process.env.AI_WT_LLAMA_SERVER) return { variants: [{ label: "binary", backend: process.env.AI_WT_LLAMA_BACKEND ?? "unknown" }] };
    const variants = llamaVariants(ctx);
    if (variants.length === 0) return { skip: `the manifest has no llama-server build for ${platformKey(ctx.fp)} with a backend this machine has (${ctx.fp.backends.join(", ")})` };
    return { variants };
  },
  async open(ctx, variant): Promise<Session> {
    const attached = envUrl("AI_WT_LLAMA_SERVER_URL");
    if (attached) {
      assertNotReserved(attached);
      const listed: any = await fetch(`${attached}/v1/models`).then((r) => r.json());
      const remote = listed?.data?.[0]?.id ?? "default";
      return {
        kind: "LLAMA_SERVER",
        mode: "attach",
        async serve() {
          return { baseUrl: attached, remoteModel: remote, pids: () => [], async release() {} };
        },
        async close() {},
      };
    }
    let binary = process.env.AI_WT_LLAMA_SERVER;
    let version: string | undefined;
    if (!binary) ({ binary, version } = await installRuntime(ctx.manifest, "llama-server", variant.runtimeBuild!, ctx.log));
    const gpu = variant.backend !== "cpu";
    return {
      kind: "LLAMA_SERVER",
      mode: "spawned",
      version,
      async serve(model, service) {
        if (!model.modelId) throw new Error(`llama-server serves manifest models only; '${model.remoteModel}' has no modelId`);
        const file = await installModel(ctx.manifest, model.modelId, ctx.log);
        const port = await freePort();
        // The flags that shape serving (recorded with the cell, and recommended with it).
        // Thinking is off: an app's `ai.prompt` cannot turn it off or cap it, and a thinking
        // model then spends the whole context thinking and answers nothing.
        const flags = ["-c", "8192", ...(gpu ? ["-ngl", "999"] : []), ...(service === "embeddings" ? ["--embeddings"] : ["--reasoning", "off"])];
        const args = ["-m", file, "--host", "127.0.0.1", "--port", String(port), "--alias", model.remoteModel, "-t", String(Math.min(16, os.cpus().length)), ...flags];
        const started = Date.now();
        const s = spawnLogged(binary!, args, path.join(ctx.runDir, `llama-server-${variant.label}-${model.modelId}.log`), {
          LD_LIBRARY_PATH: [path.dirname(binary!), ...(variant.backend === "rocm" ? rocmLibDirs() : []), process.env.LD_LIBRARY_PATH].filter(Boolean).join(":"),
        });
        try {
          // 503 while the model loads, 200 once it serves.
          await waitForHttp(`http://127.0.0.1:${port}/health`, { timeoutMs: 600_000, alive: () => s.proc.exitCode === null, label: `llama-server ${variant.label} ${model.modelId}` });
        } catch (e) {
          await s.stop();
          throw e;
        }
        return {
          baseUrl: `http://127.0.0.1:${port}`,
          remoteModel: model.remoteModel,
          readyMs: Date.now() - started,
          pids: () => (s.proc.pid ? [s.proc.pid] : []),
          backend: () => variant.backend,
          flags,
          release: () => s.stop(),
        };
      },
      async close() {},
    };
  },
};
