/**
 * whisper-server (whisper.cpp): one process per model, from the manifest's builds, a
 * binary (AI_WT_WHISPER_SERVER) or a running server (AI_WT_WHISPER_SERVER_URL, measured only
 * for the model AI_WT_WHISPER_SERVER_MODEL names: it does not say what it loaded). It is
 * started with `--request-path /v1/audio --inference-path /transcriptions`, which serves
 * the OpenAI path the executor posts to (`/v1/audio/transcriptions`); `/inference` is
 * then gone. Servers stay up until the run ends: the TTS round trip transcribes with them.
 */

import os from "node:os";
import path from "node:path";
import { platformKey } from "../fingerprint.js";
import { installModel, installRuntime } from "../install.js";
import { assertNotReserved, freePort, spawnLogged, waitForHttp, type Spawned } from "../procs.js";
import { encodeWav } from "../verify.js";
import { assertAttachedModel, envUrl, type Driver, type Session } from "./types.js";

/** Whether `baseUrl` answers a transcription upload on the OpenAI path. */
async function servesOpenAiPath(baseUrl: string): Promise<string | null> {
  const form = new FormData();
  form.append("file", new Blob([new Uint8Array(encodeWav(new Float32Array(16000), 16000))], { type: "audio/wav" }), "probe.wav");
  form.append("model", "probe");
  const res = await fetch(`${baseUrl}/v1/audio/transcriptions`, { method: "POST", body: form });
  return res.ok ? null : `POST /v1/audio/transcriptions answered HTTP ${res.status}`;
}

export const whisperServer: Driver = {
  id: "whisper-server",
  async plan(ctx) {
    if (envUrl("AI_WT_WHISPER_SERVER_URL")) return { variants: [{ label: "attach" }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_WHISPER_SERVER_URL" };
    if (process.env.AI_WT_WHISPER_SERVER) return { variants: [{ label: "binary", backend: process.env.AI_WT_WHISPER_BACKEND ?? "cpu" }] };
    const rt = ctx.manifest.runtimes["whisper-server"];
    const prefix = `${platformKey(ctx.fp)}-`;
    const builds = Object.keys(rt?.builds ?? {}).filter((k) => k.startsWith(prefix) && (ctx.fp.backends as string[]).includes(k.slice(prefix.length)));
    if (builds.length === 0) return { skip: `the manifest has no whisper-server build for ${platformKey(ctx.fp)} (whisper.cpp publishes Linux and Windows builds; on macOS build it from source and set AI_WT_WHISPER_SERVER)` };
    // One build: the GPU one if there is, else CPU.
    const pick = builds.find((b) => !b.endsWith("-cpu")) ?? builds[0];
    return { variants: [{ runtimeBuild: pick, backend: pick.slice(prefix.length), label: pick }] };
  },
  async open(ctx, variant): Promise<Session> {
    const attached = envUrl("AI_WT_WHISPER_SERVER_URL");
    if (attached) {
      assertNotReserved(attached);
      const problem = await servesOpenAiPath(attached);
      if (problem) throw new Error(`${attached}: ${problem}; start whisper-server with --request-path /v1/audio --inference-path /transcriptions`);
      return {
        kind: "OPEN_AI_AUDIO",
        mode: "attach",
        async serve(model) {
          // It transcribes with the model it was started with, whatever the upload names.
          assertAttachedModel(model, "AI_WT_WHISPER_SERVER_MODEL");
          return { baseUrl: attached, remoteModel: model.remoteModel, pids: () => [], staysUp: true, async release() {} };
        },
        async close() {},
      };
    }
    let binary = process.env.AI_WT_WHISPER_SERVER;
    let version: string | undefined;
    if (!binary) ({ binary, version } = await installRuntime(ctx.manifest, "whisper-server", variant.runtimeBuild!, ctx.log));
    const running: Spawned[] = [];
    return {
      kind: "OPEN_AI_AUDIO",
      mode: "spawned",
      version,
      async serve(model) {
        if (!model.modelId) throw new Error(`whisper-server serves manifest models only; '${model.remoteModel}' has no modelId`);
        const file = await installModel(ctx.manifest, model.modelId, ctx.log);
        const port = await freePort();
        const threads = Math.min(8, os.cpus().length);
        const started = Date.now();
        const flags = ["--request-path", "/v1/audio", "--inference-path", "/transcriptions", "-t", String(threads)];
        const s = spawnLogged(
          binary!,
          ["-m", file, "--host", "127.0.0.1", "--port", String(port), ...flags],
          path.join(ctx.runDir, `whisper-server-${model.modelId}.log`),
          { LD_LIBRARY_PATH: [path.dirname(binary!), process.env.LD_LIBRARY_PATH].filter(Boolean).join(":") },
        );
        running.push(s);
        const baseUrl = `http://127.0.0.1:${port}`;
        // It answers 404 on `/` once listening; the model is loaded before it listens.
        await waitForHttp(`${baseUrl}/`, { timeoutMs: 120_000, ok: () => true, alive: s.running, label: `whisper-server ${model.modelId}` });
        const readyMs = Date.now() - started;
        const problem = await servesOpenAiPath(baseUrl);
        if (problem) throw new Error(`whisper-server ${version ?? ""}: ${problem}`);
        return { baseUrl, remoteModel: model.remoteModel, readyMs, pids: () => (s.proc.pid ? [s.proc.pid] : []), backend: () => variant.backend, flags, staysUp: true, async release() {} };
      },
      async close() {
        await Promise.all(running.map((s) => s.stop()));
      },
    };
  },
};
