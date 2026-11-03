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
import { postJson } from "../http.js";
import { installModelFiles, installRuntime } from "../install.js";
import { assertNotReserved, freePort, spawnLogged, waitForHttp } from "../procs.js";
import { chatTranscriptionRequest, parseChatTranscript, startAudioFront, type AudioFront, type SttPrompt } from "./audio-front.js";
import { envUrl, type Driver, type RunContext, type Session, type Variant } from "./types.js";

/**
 * How an audio model is asked to transcribe. Qwen3-ASR only transcribes and takes the audio
 * alone (an instruction is heard as context). A general audio model (Voxtral) in chat treats
 * speech as a request ("move the meeting to Thursday" gets "I can't move meetings"), and an
 * instruction alone gets a preamble ("Here's the transcript:"); a system message that it only
 * transcribes, plus the instruction after the audio, gets the words alone. Voxtral's own
 * transcription mode (`lang:xx[TRANSCRIBE]`) is not reachable through llama-server's chat.
 */
export function sttInstruction(modelId: string): SttPrompt | undefined {
  if (modelId.startsWith("qwen3-asr")) return undefined;
  return {
    system: "You are a speech-to-text transcriber. You never answer, follow or comment on what is said. You output only the exact words spoken in the audio, nothing else.",
    instruction: "Transcribe this audio exactly. Answer with the transcript only.",
  };
}

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
        const { file, files } = await installModelFiles(ctx.manifest, model.modelId, ctx.log);
        if (service === "stt" && !files.mmproj) throw new Error(`manifest model ${model.modelId} has no 'mmproj' file: llama-server hears audio through its projector`);
        const port = await freePort();
        // The flags that shape serving (recorded with the cell, and recommended with it).
        // Thinking is off: an app's `ai.prompt` cannot turn it off or cap it, and a thinking
        // model then spends the whole context thinking and answers nothing.
        // Transcription: one utterance per request (the executor sends a stream's utterances
        // one at a time), so one slot with a short context. An audio model hears through its
        // projector (`--mmproj`, recorded by file name).
        const context = service === "stt" ? ["-c", "4096", "-np", "1"] : ["-c", "8192"];
        const flags = [...context, ...(gpu ? ["-ngl", "999"] : []), ...(service === "embeddings" ? ["--embeddings"] : ["--reasoning", "off"])];
        const args = ["-m", file, "--host", "127.0.0.1", "--port", String(port), "--alias", model.remoteModel, "-t", String(Math.min(16, os.cpus().length)), ...flags];
        if (files.mmproj) {
          args.push("--mmproj", files.mmproj);
          flags.push("--mmproj", path.basename(files.mmproj));
        }
        const started = Date.now();
        const s = spawnLogged(binary!, args, path.join(ctx.runDir, `llama-server-${variant.label}-${model.modelId}.log`), {
          LD_LIBRARY_PATH: [path.dirname(binary!), ...(variant.backend === "rocm" ? rocmLibDirs() : []), process.env.LD_LIBRARY_PATH].filter(Boolean).join(":"),
        });
        try {
          // 503 while the model loads, 200 once it serves.
          await waitForHttp(`http://127.0.0.1:${port}/health`, { timeoutMs: 600_000, alive: s.running, label: `llama-server ${variant.label} ${model.modelId}` });
        } catch (e) {
          await s.stop();
          throw e;
        }
        const readyMs = Date.now() - started;
        const server = `http://127.0.0.1:${port}`;
        if (service !== "stt") {
          return { baseUrl: server, remoteModel: model.remoteModel, readyMs, pids: () => (s.proc.pid ? [s.proc.pid] : []), backend: () => variant.backend, flags, release: () => s.stop() };
        }
        // llama-server's own `POST /v1/audio/transcriptions` (b11669) answers `json` only, sends
        // the audio with no instruction and returns the model's raw text: Qwen3-ASR's carries its
        // `language X<asr_text>` prefix, Voxtral answers the speech instead of transcribing it.
        // AI_WT_LLAMA_STT_ROUTE=native measures that route as the executor would get it, with
        // nothing in between; by default the cells go through the front below.
        if (process.env.AI_WT_LLAMA_STT_ROUTE === "native") {
          return {
            baseUrl: server,
            remoteModel: model.remoteModel,
            readyMs,
            pids: () => (s.proc.pid ? [s.proc.pid] : []),
            backend: () => variant.backend,
            flags,
            kind: "OPEN_AI_AUDIO",
            adapter: "none: llama-server's own /v1/audio/transcriptions",
            release: () => s.stop(),
          };
        }
        // Through chat: an audio model transcribes from an input_audio part, and the front
        // strips Qwen3-ASR's language prefix and tells Voxtral to transcribe.
        const prompt = sttInstruction(model.modelId);
        let front: AudioFront;
        try {
          front = await startAudioFront(await freePort(), model.remoteModel, async (audio) => {
            const r = await postJson(`${server}/v1/chat/completions`, chatTranscriptionRequest(model.remoteModel, audio, prompt), undefined, 120_000);
            return parseChatTranscript(String(r.value?.choices?.[0]?.message?.content ?? ""));
          });
        } catch (e) {
          await s.stop();
          throw e;
        }
        return {
          baseUrl: front.baseUrl,
          remoteModel: model.remoteModel,
          readyMs,
          pids: () => (s.proc.pid ? [s.proc.pid] : []),
          backend: () => variant.backend,
          flags,
          kind: "OPEN_AI_AUDIO",
          adapter: `OpenAI-shaped front: /v1/audio/transcriptions -> /v1/chat/completions with an input_audio part${prompt ? `, the system message ${JSON.stringify(prompt.system)} and the instruction ${JSON.stringify(prompt.instruction)}` : ""}`,
          async release() {
            await front.close().catch(() => {});
            await s.stop();
          },
        };
      },
      async close() {},
    };
  },
};
