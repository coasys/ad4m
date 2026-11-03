/**
 * sherpa-onnx (k2-fsa, onnxruntime): `sherpa-onnx-offline-websocket-server` from the
 * manifest's builds, one process per model. Its only network API is a WebSocket protocol
 * of its own (no HTTP route, no `/v1`), so the executor reaches it through an OpenAI-shaped
 * front the run starts beside it (`audio-front.ts`): the cells measure server and front
 * together, as an executor endpoint kind or a sidecar would run them.
 */

import os from "node:os";
import path from "node:path";
import { platformKey } from "../fingerprint.js";
import { installModelFiles, installRuntime } from "../install.js";
import { freePort, spawnLogged, waitForHttp, type Spawned } from "../procs.js";
import { sherpaTranscribe, startAudioFront, wavTo16k, type AudioFront } from "./audio-front.js";
import { runtimeVariants } from "./nemo-speech.js";
import type { Driver, Session } from "./types.js";

export const sherpaOnnx: Driver = {
  id: "sherpa-onnx",
  async plan(ctx) {
    if (ctx.attach) return { skip: "--attach: sherpa-onnx has no OpenAI-shaped server to attach to" };
    if (process.env.AI_WT_SHERPA_ONNX) return { variants: [{ label: "binary", backend: process.env.AI_WT_SHERPA_ONNX_BACKEND ?? "cpu" }] };
    const variants = runtimeVariants(ctx, "sherpa-onnx");
    if (variants.length === 0) return { skip: `the manifest has no sherpa-onnx build for ${platformKey(ctx.fp)} with a backend this machine has (${ctx.fp.backends.join(", ")})` };
    return { variants };
  },
  async open(ctx, variant): Promise<Session> {
    let binary = process.env.AI_WT_SHERPA_ONNX;
    let version: string | undefined;
    if (!binary) ({ binary, version } = await installRuntime(ctx.manifest, "sherpa-onnx", variant.runtimeBuild!, ctx.log));
    const running: { server: Spawned; front: AudioFront }[] = [];
    return {
      kind: "OPEN_AI_AUDIO",
      mode: "spawned",
      version,
      async serve(model) {
        if (!model.modelId) throw new Error(`sherpa-onnx serves manifest models only; '${model.remoteModel}' has no modelId`);
        const { file, files } = await installModelFiles(ctx.manifest, model.modelId, ctx.log);
        for (const role of ["decoder", "joiner", "tokens"]) if (!files[role]) throw new Error(`manifest model ${model.modelId} has no '${role}' file`);
        const wsPort = await freePort();
        const threads = Math.min(8, os.cpus().length);
        const provider = variant.backend === "cuda" ? "cuda" : "cpu";
        const flags = [`--provider=${provider}`, `--num-threads=${threads}`, "--num-work-threads=1", "--max-batch-size=1"];
        const started = Date.now();
        const server = spawnLogged(
          binary!,
          [`--port=${wsPort}`, `--encoder=${file}`, `--decoder=${files.decoder}`, `--joiner=${files.joiner}`, `--tokens=${files.tokens}`, ...flags],
          path.join(ctx.runDir, `sherpa-onnx-${variant.label}-${model.modelId}.log`),
          { LD_LIBRARY_PATH: [path.join(path.dirname(binary!), "..", "lib"), process.env.LD_LIBRARY_PATH].filter(Boolean).join(":") },
          // It writes its own `log.txt` into the working directory: keep it in the run's logs.
          ctx.runDir,
        );
        // It answers plain HTTP (a 404 without a doc root) once the model is loaded and it listens.
        try {
          await waitForHttp(`http://127.0.0.1:${wsPort}/`, { timeoutMs: 300_000, ok: () => true, alive: server.running, label: `sherpa-onnx ${model.modelId}` });
        } catch (e) {
          await server.stop();
          throw e;
        }
        const readyMs = Date.now() - started;
        const front = await startAudioFront(await freePort(), model.remoteModel, (audio) => sherpaTranscribe(`ws://127.0.0.1:${wsPort}`, wavTo16k(audio).samples));
        const entry = { server, front };
        running.push(entry);
        return {
          baseUrl: front.baseUrl,
          remoteModel: model.remoteModel,
          readyMs,
          pids: () => (server.proc.pid ? [server.proc.pid] : []),
          backend: () => provider,
          flags,
          adapter: "OpenAI-shaped front: /v1/audio/transcriptions -> sherpa-onnx offline WebSocket",
          async release() {
            await front.close().catch(() => {});
            await server.stop();
            running.splice(running.indexOf(entry), 1);
          },
        };
      },
      async close() {
        await Promise.all(running.map(async (r) => (await r.front.close().catch(() => {}), await r.server.stop())));
      },
    };
  },
};
