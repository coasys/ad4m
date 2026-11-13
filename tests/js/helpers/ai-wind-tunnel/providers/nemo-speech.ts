/**
 * nemo-speech (NVIDIA NeMo-Speech.cpp, ggml): `nemo-speech serve` answers
 * `POST /v1/audio/transcriptions` and lists its model on `GET /v1/models`, so the executor
 * reaches it as an `OPEN_AI_AUDIO` endpoint with nothing in between. One process per model,
 * from the manifest's builds (every build whose backend this machine has, CPU included:
 * the CPU real-time factor is what a machine without a GPU gets), a binary
 * (AI_WT_NEMO_SPEECH, backend in AI_WT_NEMO_SPEECH_BACKEND) or a running server
 * (AI_WT_NEMO_SPEECH_URL, measured only for the model AI_WT_NEMO_SPEECH_MODEL names or it lists).
 */

import path from "node:path";
import { platformKey } from "../fingerprint.js";
import { installModel, installRuntime } from "../install.js";
import { assertNotReserved, freePort, spawnLogged, waitForHttp, type Spawned } from "../procs.js";
import { assertAttachedModel, envUrl, type Driver, type RunContext, type Session, type Variant } from "./types.js";

/** Every manifest build of runtime `id` for this platform whose backend the machine can use, GPU ones first. */
export function runtimeVariants(ctx: Pick<RunContext, "fp" | "manifest">, id: string): Variant[] {
  const prefix = `${platformKey(ctx.fp)}-`;
  return Object.keys(ctx.manifest.runtimes[id]?.builds ?? {})
    .filter((k) => k.startsWith(prefix) && (ctx.fp.backends as string[]).includes(k.slice(prefix.length)))
    .map((k) => ({ runtimeBuild: k, backend: k.slice(prefix.length), label: k }))
    .sort((a, b) => Number(a.backend === "cpu") - Number(b.backend === "cpu"));
}

/** The id `nemo-speech serve` lists for its ASR model: requests must name it or nothing. */
async function listedModel(baseUrl: string): Promise<string> {
  const j: any = await fetch(`${baseUrl}/v1/models`).then((r) => r.json());
  const id = j?.data?.find((m: any) => m.capability === "transcription")?.id ?? j?.data?.[0]?.id;
  if (!id) throw new Error(`${baseUrl}/v1/models lists no transcription model`);
  return id;
}

export const nemoSpeech: Driver = {
  id: "nemo-speech",
  async plan(ctx) {
    if (envUrl("AI_WT_NEMO_SPEECH_URL")) return { variants: [{ label: "attach", backend: process.env.AI_WT_NEMO_SPEECH_BACKEND }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_NEMO_SPEECH_URL" };
    if (process.env.AI_WT_NEMO_SPEECH) return { variants: [{ label: "binary", backend: process.env.AI_WT_NEMO_SPEECH_BACKEND ?? "cpu" }] };
    const variants = runtimeVariants(ctx, "nemo-speech");
    if (variants.length === 0) return { skip: `the manifest has no nemo-speech build for ${platformKey(ctx.fp)} with a backend this machine has (${ctx.fp.backends.join(", ")})` };
    return { variants };
  },
  async open(ctx, variant): Promise<Session> {
    const attached = envUrl("AI_WT_NEMO_SPEECH_URL");
    if (attached) {
      assertNotReserved(attached);
      const remote = await listedModel(attached);
      return {
        kind: "OPEN_AI_AUDIO",
        mode: "attach",
        async serve(model) {
          // It hosts the one ASR model it was started with.
          assertAttachedModel(model, "AI_WT_NEMO_SPEECH_MODEL", [remote]);
          return { baseUrl: attached, remoteModel: remote, pids: () => [], staysUp: true, async release() {} };
        },
        async close() {},
      };
    }
    let binary = process.env.AI_WT_NEMO_SPEECH;
    let version: string | undefined;
    if (!binary) ({ binary, version } = await installRuntime(ctx.manifest, "nemo-speech", variant.runtimeBuild!, ctx.log));
    const running: Spawned[] = [];
    return {
      kind: "OPEN_AI_AUDIO",
      mode: "spawned",
      version,
      async serve(model) {
        if (!model.modelId) throw new Error(`nemo-speech serves manifest models only; '${model.remoteModel}' has no modelId`);
        const file = await installModel(ctx.manifest, model.modelId, ctx.log);
        const port = await freePort();
        // `--device`: auto would pick the GPU on a CPU variant. No playground.
        const flags = ["--device", variant.backend === "cpu" ? "cpu" : variant.backend ?? "auto", "--no-ui"];
        const started = Date.now();
        const s = spawnLogged(binary!, ["serve", "--asr-model", file, "--host", "127.0.0.1", "--port", String(port), ...flags], path.join(ctx.runDir, `nemo-speech-${variant.label}-${model.modelId}.log`));
        running.push(s);
        const baseUrl = `http://127.0.0.1:${port}`;
        try {
          // 503 until the engine is loaded and warmed up.
          await waitForHttp(`${baseUrl}/ready`, { timeoutMs: 600_000, alive: s.running, label: `nemo-speech ${variant.label} ${model.modelId}` });
        } catch (e) {
          await s.stop();
          throw e;
        }
        const readyMs = Date.now() - started;
        const ready: any = await fetch(`${baseUrl}/ready`).then((r) => r.json()).catch(() => ({}));
        return {
          baseUrl,
          remoteModel: await listedModel(baseUrl),
          readyMs,
          pids: () => (s.proc.pid ? [s.proc.pid] : []),
          // What the server says it runs on (`/ready`'s device), else the build's backend.
          backend: () => (typeof ready.device === "string" ? ready.device.replace(/:\d+$/, "") : variant.backend),
          flags,
          release: () => s.stop(),
        };
      },
      async close() {
        await Promise.all(running.map((s) => s.stop()));
      },
    };
  },
};
