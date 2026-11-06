// Unit tests of the AI wind tunnel's pure parts: matrix building, verification scoring,
// statistics, hardware classification, memory fit, evidence validation and recommendation
// derivation. No executor and no model server: `pnpm run test-ai-wind-tunnel-unit`.

import { expect } from "chai";
import { mkdtempSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { loadHardwareClasses, loadManifest, loadMatrixPlan, type Manifest, type MatrixPlan } from "../helpers/ai-wind-tunnel/data.js";
import { evidenceRelPath, loadAllEvidence, validateEvidence } from "../helpers/ai-wind-tunnel/evidence.js";
import { fingerprintId, gpuBudget, isIntegratedAmdName } from "../helpers/ai-wind-tunnel/fingerprint.js";
import { classify, fallbackClasses, memoryFit, type ClassInput } from "../helpers/ai-wind-tunnel/hardware-class.js";
import { buildMatrix, parseList, resolveModel } from "../helpers/ai-wind-tunnel/matrix.js";
import { DATA_DIR } from "../helpers/ai-wind-tunnel/paths.js";
import { streamedTranscript } from "../helpers/ai-wind-tunnel/cells.js";
import { drmClientBytes, spawnLogged, waitForHttp } from "../helpers/ai-wind-tunnel/procs.js";
import { chatTranscriptionRequest, parseChatTranscript, parseSherpaResult, sherpaFrame, startAudioFront } from "../helpers/ai-wind-tunnel/providers/audio-front.js";
import { containerMemoryArgs } from "../helpers/ai-wind-tunnel/providers/docker.js";
import { llamaVariants, sttInstruction } from "../helpers/ai-wind-tunnel/providers/llama-server.js";
import { runtimeVariants } from "../helpers/ai-wind-tunnel/providers/nemo-speech.js";
import { ollamaBackendFromLog } from "../helpers/ai-wind-tunnel/providers/ollama.js";
import { deriveRecommendations, parseParams } from "../helpers/ai-wind-tunnel/recommend.js";
import { medianOf, quantile, summarize } from "../helpers/ai-wind-tunnel/stats.js";
import { PROVIDERS, SERVICES, type CellResult, type Evidence, type Fingerprint } from "../helpers/ai-wind-tunnel/types.js";
import {
  addNoise,
  checkOffload,
  checkToolCall,
  consistentDimension,
  cosine,
  decodeWav,
  encodeWav,
  normalizeForWer,
  numberWords,
  rank,
  resample,
  rms,
  scoreRetrieval,
  speechDurationRange,
  stripThinking,
  wer,
  wordEditDistance,
} from "../helpers/ai-wind-tunnel/verify.js";

const GIB = 1024 ** 3;

const fp = (over: Partial<Fingerprint> = {}): Fingerprint => {
  const base = {
    os: "linux" as const,
    osVersion: "Test",
    kernel: "1",
    arch: "x64" as const,
    cpu: { model: "Test CPU", cores: 8, threads: 16 },
    memoryBytes: 32 * GIB,
    gpus: [],
    backends: ["cpu" as const],
    gpuBudgetBytes: null,
    drivers: {},
    ...over,
  };
  return { ...base, id: fingerprintId(base) };
};

function cell(over: Partial<CellResult>): CellResult {
  return {
    provider: "llama-server",
    service: "llm",
    status: "pass",
    model: { modelId: "m-small", remoteModel: "m-small", params: "1B", memoryBytes: 1e9 },
    checks: [{ name: "works", pass: true, detail: "ok" }],
    metrics: {},
    durationMs: 1,
    ...over,
  };
}

function evidence(cls: string, cells: CellResult[], finishedAt = "2026-11-03T00:00:00.000Z"): Evidence {
  return {
    schemaVersion: 1,
    runId: "r1",
    startedAt: "2026-11-03T00:00:00.000Z",
    finishedAt,
    hardwareClass: cls,
    fingerprint: fp(),
    executor: { commit: "abc", binary: "ad4m-executor" },
    options: { repeat: 3, concurrency: [1, 4], providers: [...PROVIDERS], services: [...SERVICES], direct: true },
    providers: {},
    cells,
  };
}

const stat = (median: number) => ({ n: 3, median, p95: median, min: median, max: median, mean: median });

describe("AI wind tunnel (unit)", () => {
  describe("matrix", () => {
    const manifest: Manifest = {
      schemaVersion: 1,
      runtimes: {},
      models: { "tiny-llm": { service: "llm", provider: "llama-server", url: "https://x/tiny.gguf", sha256: "0".repeat(64), sizeBytes: 100, license: "MIT", params: "1B", memoryBytes: 200 } },
    };
    const plan = {
      schemaVersion: 1,
      services: ["llm", "completion", "embeddings", "stt", "tts"],
      providers: {
        "llama-server": { endpointKind: "LLAMA_SERVER", models: { llm: [{ modelId: "tiny-llm" }], embeddings: [] }, unsupported: { stt: "no audio" } },
        ollama: { endpointKind: "OLLAMA", models: { llm: [{ remoteModel: "tiny:1b", sizeBytes: 50 }] }, unsupported: { tts: "no speech" } },
      },
    } as unknown as MatrixPlan;

    it("takes a manifest model's size, licence and memory", () => {
      expect(resolveModel({ modelId: "tiny-llm" }, manifest)).to.deep.include({ modelId: "tiny-llm", remoteModel: "tiny-llm", memoryBytes: 200, license: "MIT", params: "1B" });
    });
    it("records the download's sha256, so evidence names the file it measured", () => {
      expect(resolveModel({ modelId: "tiny-llm" }, manifest)).to.deep.include({ source: "https://x/tiny.gguf", sha256: "0".repeat(64) });
    });
    it("uses a provider's own name, the download as the memory floor", () => {
      expect(resolveModel({ remoteModel: "tiny:1b", sizeBytes: 50 }, manifest)).to.deep.equal({ remoteModel: "tiny:1b", sizeBytes: 50, memoryBytes: 50 });
    });
    it("counts the files beside a model into its download size", () => {
      const withFiles: Manifest = {
        ...manifest,
        models: { asr: { ...manifest.models["tiny-llm"], service: "stt", memoryBytes: 300, files: { mmproj: { url: "https://x/mmproj.gguf", sha256: "1".repeat(64), sizeBytes: 30 } } } },
      };
      expect(resolveModel({ modelId: "asr" }, withFiles).sizeBytes).to.equal(130);
    });
    it("refuses a modelId the manifest lacks", () => {
      expect(() => resolveModel({ modelId: "nope" }, manifest)).to.throw(/nope/);
    });
    it("builds cells in provider order, unsupported ones with their reason, none for an empty list", () => {
      const cells = buildMatrix(plan, manifest, ["ollama", "llama-server"], ["llm", "embeddings", "stt", "tts"]);
      expect(cells.map((c) => `${c.provider}:${c.service}:${c.unsupported ?? c.model?.remoteModel}`)).to.deep.equal([
        "ollama:llm:tiny:1b",
        "ollama:tts:no speech",
        "llama-server:llm:tiny-llm",
        "llama-server:stt:no audio",
      ]);
    });
    it("parses provider and service lists, refusing unknown ids", () => {
      expect(parseList(undefined, SERVICES, "services")).to.deep.equal([...SERVICES]);
      expect(parseList("llm, tts", SERVICES, "services")).to.deep.equal(["llm", "tts"]);
      expect(() => parseList("llm,video", SERVICES, "services")).to.throw(/video/);
    });
    it("the checked-in plan resolves against the checked-in manifest", () => {
      const cells = buildMatrix(loadMatrixPlan(), loadManifest(), [...PROVIDERS], [...SERVICES]);
      expect(cells.length).to.be.greaterThan(10);
      for (const c of cells) expect(Boolean(c.unsupported) !== Boolean(c.model), `${c.provider} ${c.service}`).to.be.true;
    });
  });

  describe("manifest", () => {
    const m = loadManifest();
    it("is schema v1 with hashes, sizes and licences on every model", () => {
      expect(m.schemaVersion).to.equal(1);
      for (const [id, model] of Object.entries(m.models)) {
        expect(id, id).to.match(/^[a-z0-9][a-z0-9._-]*$/);
        expect(model.sha256, id).to.match(/^[0-9a-f]{64}$/);
        expect(model.sizeBytes, id).to.be.greaterThan(0);
        expect(model.memoryBytes, id).to.be.at.least(model.sizeBytes);
        expect(model.license, id).to.be.a("string").and.not.empty;
        expect(["llm", "embeddings", "stt", "tts"]).to.include(model.service);
      }
    });
    it("pins every model download to a Hugging Face commit, never a branch", () => {
      // An upstream re-upload on `main` would change the file under a fixed sha256
      // and break every install; a commit URL keeps serving the hashed file.
      for (const [id, model] of Object.entries(m.models)) {
        expect(model.url, id).to.match(/^https:\/\/huggingface\.co\/[^/]+\/[^/]+\/resolve\/[0-9a-f]{40}\/[^/].*$/);
        expect(model.url, id).not.to.include("/resolve/main/");
      }
    });
    it("names every runtime build <os>-<arch>-<backend> with a hash and a binary path", () => {
      for (const [id, rt] of Object.entries(m.runtimes)) {
        expect(["llama-server", "whisper-server", "nemo-speech", "sherpa-onnx"]).to.include(rt.provider);
        expect(rt.provider, id).to.equal(id);
        for (const [key, b] of Object.entries(rt.builds)) {
          expect(key, `${id} ${key}`).to.match(/^(linux|macos|windows)-(x64|arm64)-(cpu|vulkan|rocm|cuda|metal)$/);
          expect(b.sha256, key).to.match(/^[0-9a-f]{64}$/);
          expect(b.url, key).to.match(/^https:\/\//);
          expect(b.binary, key).to.be.a("string").and.not.empty;
        }
      }
    });
    it("gives every further model file a hash, a size and a pinned URL", () => {
      for (const [id, model] of Object.entries(m.models)) {
        for (const [role, f] of Object.entries(model.files ?? {})) {
          expect(f.sha256, `${id} ${role}`).to.match(/^[0-9a-f]{64}$/);
          expect(f.sizeBytes, `${id} ${role}`).to.be.greaterThan(0);
          expect(f.url, `${id} ${role}`).to.match(/^https:\/\/huggingface\.co\/[^/]+\/[^/]+\/resolve\/[0-9a-f]{40}\/[^/].*$/);
        }
        // What the runtime needs beside the model file.
        if (model.provider === "sherpa-onnx") expect(Object.keys(model.files ?? {}).sort(), id).to.deep.equal(["decoder", "joiner", "tokens"]);
        if (model.provider === "llama-server" && model.service === "stt") expect(model.files?.mmproj, id).to.be.an("object");
      }
    });
    it("picks every build of a speech runtime this machine can run, GPU ones first, CPU kept", () => {
      const ctx = (backends: any[], os: any = "linux", arch: any = "x64") => ({ fp: fp({ backends, os, arch }), manifest: m });
      expect(runtimeVariants(ctx(["cpu", "rocm", "vulkan"]), "nemo-speech").map((v) => v.runtimeBuild)).to.deep.equal(["linux-x64-vulkan", "linux-x64-cpu"]);
      expect(runtimeVariants(ctx(["cpu", "metal"], "macos", "arm64"), "nemo-speech").map((v) => v.runtimeBuild)).to.deep.equal(["macos-arm64-metal", "macos-arm64-cpu"]);
      expect(runtimeVariants(ctx(["cpu", "vulkan"]), "sherpa-onnx").map((v) => v.runtimeBuild)).to.deep.equal(["linux-x64-cpu"]);
      expect(runtimeVariants(ctx(["cpu"]), "no-such-runtime")).to.deep.equal([]);
    });
    it("picks the llama-server builds this machine's backends can run, GPU ones first", () => {
      const ctx = (backends: any[], llamaBuilds?: string[]) => ({ fp: fp({ backends }), manifest: m, llamaBuilds });
      expect(llamaVariants(ctx(["cpu"])).map((v) => v.runtimeBuild)).to.deep.equal(["linux-x64-cpu"]);
      expect(llamaVariants(ctx(["cpu", "rocm", "vulkan"])).map((v) => v.runtimeBuild).sort()).to.deep.equal(["linux-x64-rocm", "linux-x64-vulkan"]);
      expect(llamaVariants(ctx(["cpu", "rocm", "vulkan"], ["cpu"])).map((v) => v.runtimeBuild)).to.deep.equal(["linux-x64-cpu"]);
    });
  });

  describe("verification", () => {
    it("normalises text for WER: case, punctuation, hyphens, numbers", () => {
      expect(normalizeForWer("Hello, World! It's a well-known 9-to-5 job.")).to.deep.equal(["hello", "world", "it's", "a", "well", "known", "nine", "to", "five", "job"]);
      expect(normalizeForWer("1,250 people")).to.deep.equal(["one", "thousand", "two", "hundred", "fifty", "people"]);
      expect(numberWords(342)).to.equal("three hundred forty two");
      expect(numberWords(20)).to.equal("twenty");
    });
    it("counts substitutions, deletions and insertions", () => {
      expect(wordEditDistance(["a", "b", "c"], ["a", "x", "c", "d"])).to.equal(2);
      expect(wordEditDistance([], ["a"])).to.equal(1);
    });
    it("scores word error rate", () => {
      expect(wer("If you can read this, transcription is working.", " If you can read this, transcription is working.\n")).to.equal(0);
      expect(wer("the cat sat on the mat", "the cat sat on a mat")).to.be.closeTo(1 / 6, 1e-9);
      expect(wer("one two", "")).to.equal(1);
      expect(wer("", "")).to.equal(0);
      expect(wer("opens at nine", "opens at 9")).to.equal(0);
    });
    it("strips thinking, closed or cut off", () => {
      expect(stripThinking("<think>hmm</think>\nParis")).to.equal("Paris");
      expect(stripThinking("Paris <think>still going")).to.equal("Paris");
    });
    it("ranks passages by cosine similarity and scores retrieval", () => {
      expect(cosine([1, 0], [1, 0])).to.equal(1);
      expect(cosine([1, 0], [0, 1])).to.equal(0);
      expect(() => cosine([1], [1, 2])).to.throw(/dimension/);
      const passages = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
      expect(rank([0.1, 0.9, 0.2], passages)).to.deep.equal([1, 2, 0]);
      const ok = scoreRetrieval([[1, 0.1, 0], [0, 0, 1]], passages, [0, 2]);
      expect(ok).to.deep.include({ correct: 2, total: 2, mrr: 1 });
      const miss = scoreRetrieval([[0, 1, 0]], passages, [0]);
      expect(miss.correct).to.equal(0);
      expect(miss.misses).to.deep.equal([{ query: 0, expected: 0, got: 1 }]);
      expect(miss.mrr).to.be.closeTo(1 / 2, 1e-9);
    });
    it("checks a dimension is shared and finite", () => {
      expect(consistentDimension([[1, 2], [3, 4]])).to.equal(2);
      expect(consistentDimension([[1, 2], [3]])).to.equal(null);
      expect(consistentDimension([[]])).to.equal(null);
      expect(consistentDimension([[1, NaN]])).to.equal(null);
    });
    it("checks a tool call's name and JSON arguments", () => {
      expect(checkToolCall([{ function: { name: "get_weather", arguments: '{"city":"Paris"}' } }], "get_weather", "city", /paris/i).pass).to.be.true;
      expect(checkToolCall([{ function: { name: "get_weather", arguments: { city: "Paris, FR" } } }], "get_weather", "city", /paris/i).pass).to.be.true;
      expect(checkToolCall([{ function: { name: "other", arguments: "{}" } }], "get_weather", "city", /paris/i).detail).to.match(/other/);
      expect(checkToolCall([{ function: { name: "get_weather", arguments: "{city: Paris" } }], "get_weather", "city", /paris/i).detail).to.match(/not JSON/);
      expect(checkToolCall(undefined, "get_weather", "city", /paris/i).pass).to.be.false;
    });
    it("round-trips WAV and decodes float and stereo files", () => {
      const tone = new Float32Array(16000).map((_, i) => 0.5 * Math.sin((2 * Math.PI * 440 * i) / 16000));
      const back = decodeWav(encodeWav(tone, 16000));
      expect(back).to.deep.include({ sampleRate: 16000, channels: 1, bitsPerSample: 16 });
      expect(back.durationSec).to.equal(1);
      expect(rms(back.samples)).to.be.closeTo(0.5 / Math.SQRT2, 0.01);

      // 32-bit float stereo with an unknown data size, as streaming servers write it.
      const frames = 100;
      const h = Buffer.alloc(44);
      h.write("RIFF", 0, "ascii");
      h.writeUInt32LE(0xffffffff, 4);
      h.write("WAVEfmt ", 8, "ascii");
      h.writeUInt32LE(16, 16);
      h.writeUInt16LE(3, 20);
      h.writeUInt16LE(2, 22);
      h.writeUInt32LE(24000, 24);
      h.writeUInt32LE(24000 * 8, 28);
      h.writeUInt16LE(8, 32);
      h.writeUInt16LE(32, 34);
      h.write("data", 36, "ascii");
      h.writeUInt32LE(0xffffffff, 40);
      const d = Buffer.alloc(frames * 8);
      for (let i = 0; i < frames; i++) {
        d.writeFloatLE(0.25, i * 8);
        d.writeFloatLE(0.75, i * 8 + 4);
      }
      const f = decodeWav(Buffer.concat([new Uint8Array(h), new Uint8Array(d)]));
      expect(f.samples.length).to.equal(frames);
      expect(f.samples[0]).to.be.closeTo(0.5, 1e-6);
      expect(() => decodeWav(Buffer.from("not audio at all"))).to.throw(/RIFF/);
    });
    it("resamples to the target rate", () => {
      expect(resample(new Float32Array(24000), 24000, 16000).length).to.equal(16000);
      const same = new Float32Array([1, 2]);
      expect(resample(same, 16000, 16000)).to.equal(same);
    });
    it("fails a GPU build that kept the model off the GPU", () => {
      expect(checkOffload("rocm", 0, 8e8)!.pass).to.be.false;
      expect(checkOffload("rocm", 0, 8e8)!.detail).to.match(/fell back to the CPU/);
      expect(checkOffload("vulkan", 2.7e9, 8e8)!.pass).to.be.true;
      expect(checkOffload("cpu", 0, 8e8)).to.equal(null);
      expect(checkOffload("rocm", null, 8e8)).to.equal(null);
      expect(checkOffload(undefined, 0, 8e8)).to.equal(null);
    });
    it("adds seeded noise at the asked level, the same on every run", () => {
      const tone = new Float32Array(16000).map((_, i) => 0.3 * Math.sin((2 * Math.PI * 300 * i) / 16000));
      const a = addNoise(tone, 0, 7);
      expect(addNoise(tone, 0, 7)).to.deep.equal(a);
      expect(addNoise(tone, 0, 8)).to.not.deep.equal(a);
      const noise = a.map((x, i) => x - tone[i]);
      expect(rms(noise) / rms(tone)).to.be.closeTo(1, 0.05);
      const quiet = addNoise(tone, 20, 7).map((x, i) => x - tone[i]);
      expect(rms(quiet) / rms(tone)).to.be.closeTo(0.1, 0.01);
    });
    it("bounds a plausible speech duration", () => {
      const r = speechDurationRange(10);
      expect(r.min).to.equal(1.5);
      expect(r.max).to.equal(13.5);
    });
  });

  describe("transcription fronts", () => {
    it("scores every piece a stream publishes for an utterance, not only the first", async () => {
      // An upstream that streams its transcript publishes a piece per segment, with its own
      // spacing; the pieces cross the event socket after the feed resolved.
      const texts: string[] = [];
      setTimeout(() => texts.push("The quick"), 10);
      setTimeout(() => texts.push(" brown fox."), 150);
      const { text } = await streamedTranscript(texts, Promise.resolve(), { timeoutMs: 5_000 });
      expect(text).to.equal("The quick brown fox.");
    });
    it("frames an utterance as sherpa-onnx's offline server reads it", () => {
      const f = sherpaFrame(new Float32Array([0.5, -0.25]), 16000);
      expect(f.length).to.equal(16);
      expect(f.readInt32LE(0)).to.equal(16000);
      expect(f.readInt32LE(4)).to.equal(8);
      expect(f.readFloatLE(8)).to.equal(0.5);
      expect(f.readFloatLE(12)).to.equal(-0.25);
    });
    it("reads sherpa-onnx's result, with a language only when it names one", () => {
      expect(parseSherpaResult('{"lang": "", "text": " Hello there.", "tokens": []}')).to.deep.equal({ text: "Hello there." });
      expect(parseSherpaResult('{"lang": "<|en|>", "text": "Hi"}')).to.deep.equal({ text: "Hi", language: "<|en|>" });
      expect(() => parseSherpaResult('{"error": "x"}')).to.throw(/no text/);
    });
    it("reads Qwen3-ASR's language prefix and passes other models' text through", () => {
      expect(parseChatTranscript("language English<asr_text>The forecast says rain.")).to.deep.equal({ text: "The forecast says rain.", language: "English" });
      expect(parseChatTranscript("language None<asr_text>")).to.deep.equal({ text: "" });
      expect(parseChatTranscript("  The forecast says rain.\n")).to.deep.equal({ text: "The forecast says rain." });
    });
    it("asks llama-server for a greedy transcription with the audio as input_audio", () => {
      const wav = Buffer.from("RIFF");
      const r: any = chatTranscriptionRequest("m", wav, { system: "Only transcribe.", instruction: "Transcribe." });
      expect(r.temperature).to.equal(0);
      expect(r.messages[0]).to.deep.equal({ role: "system", content: "Only transcribe." });
      expect(r.messages[1].content[0]).to.deep.equal({ type: "input_audio", input_audio: { data: wav.toString("base64"), format: "wav" } });
      expect(r.messages[1].content[1]).to.deep.equal({ type: "text", text: "Transcribe." });
      const bare: any = chatTranscriptionRequest("m", wav);
      expect(bare.messages).to.have.length(1);
      expect(bare.messages[0].content).to.have.length(1);
      expect(sttInstruction("qwen3-asr-0.6b-q8_0")).to.equal(undefined);
      expect(sttInstruction("voxtral-mini-3b-2507-q4_k_m")).to.have.keys("system", "instruction");
    });
    it("serves an OpenAI-shaped transcription route and model list", async () => {
      const seen: Record<string, string>[] = [];
      const front = await startAudioFront(0, "asr-model", async (audio, fields) => {
        seen.push(fields);
        return { text: `${decodeWav(audio).samples.length} samples`, language: "en" };
      });
      try {
        const models: any = await fetch(`${front.baseUrl}/v1/models`).then((r) => r.json());
        expect(models.data[0].id).to.equal("asr-model");
        const upload = (format?: string) => {
          const form = new FormData();
          form.append("file", new Blob([new Uint8Array(encodeWav(new Float32Array(1600), 16000))], { type: "audio/wav" }), "a.wav");
          form.append("model", "asr-model");
          if (format) form.append("response_format", format);
          return fetch(`${front.baseUrl}/v1/audio/transcriptions`, { method: "POST", body: form });
        };
        expect(await upload().then((r) => r.json())).to.deep.equal({ text: "1600 samples" });
        expect(await upload("verbose_json").then((r) => r.json())).to.deep.equal({ task: "transcribe", language: "en", duration: 0.1, text: "1600 samples" });
        expect(await upload("text").then((r) => r.text())).to.equal("1600 samples");
        expect(seen[1]).to.deep.include({ model: "asr-model", response_format: "verbose_json" });
        const missing = await fetch(`${front.baseUrl}/v1/audio/transcriptions`, { method: "POST", body: new FormData() });
        expect(missing.status).to.equal(400);
        expect((await fetch(`${front.baseUrl}/v1/chat/completions`, { method: "POST" })).status).to.equal(404);
      } finally {
        await front.close();
      }
    });
  });

  describe("processes", () => {
    // amdgpu fdinfo on 7.0 (from 6.13): drm_print_memory_stats, then the legacy keys again
    // as an alias of drm-resident-*.
    const fdinfo613 = [
      "drm-driver:\tamdgpu",
      "drm-client-id:\t43",
      "drm-total-cpu:\t256580 KiB",
      "drm-resident-cpu:\t0",
      "drm-total-gtt:\t1455376 KiB",
      "drm-shared-gtt:\t0",
      "drm-resident-gtt:\t1455364 KiB",
      "drm-purgeable-gtt:\t0",
      "drm-total-vram:\t952 KiB",
      "drm-resident-vram:\t952 KiB",
      "drm-memory-vram:\t952 KiB",
      "drm-memory-gtt: \t1455364 KiB",
      "drm-memory-cpu: \t0 KiB",
      "amd-requested-gtt:\t1455376 KiB",
    ].join("\n");
    // Up to 6.12: the legacy keys only.
    const fdinfo612 = ["drm-driver:\tamdgpu", "drm-client-id:\t43", "drm-memory-vram:\t952 KiB", "drm-memory-gtt: \t1455364 KiB", "drm-memory-cpu: \t0 KiB"].join("\n");
    it("counts a DRM client's GPU memory once on amdgpu from kernel 6.13", () => {
      expect(drmClientBytes(fdinfo613)).to.equal((1455364 + 952) * 1024);
    });
    it("counts the legacy drm-memory keys where no drm-resident key is printed", () => {
      expect(drmClientBytes(fdinfo612)).to.equal((1455364 + 952) * 1024);
      expect(drmClientBytes("drm-client-id:\t1\ndrm-resident-local:\t2 MiB\ndrm-resident-system:\t0")).to.equal(2 * 1024 * 1024);
    });
    it("caps a container's memory and swap alike, or not at all", () => {
      expect(containerMemoryArgs("8g")).to.deep.equal(["--memory", "8g", "--memory-swap", "8g"]);
      expect(containerMemoryArgs("")).to.deep.equal([]);
      expect(containerMemoryArgs(undefined)).to.deep.equal([]);
      expect(() => containerMemoryArgs("lots")).to.throw(/not a docker memory size/);
    });
    it("stops waiting for a server that a signal killed", async () => {
      // An aborting server (a GGML_ASSERT) ends by SIGABRT: no exit code, only a signal code.
      const s = spawnLogged("sh", ["-c", "kill -ABRT $$"], path.join(mkdtempSync(path.join(os.tmpdir(), "ai-wt-unit-")), "abort.log"));
      await s.exited;
      expect(s.proc.exitCode).to.equal(null);
      expect(s.running()).to.equal(false);
      const t0 = Date.now();
      let error = "";
      await waitForHttp("http://127.0.0.1:9/", { timeoutMs: 30_000, alive: s.running, label: "aborted" }).catch((e) => (error = e.message));
      expect(error).to.match(/aborted exited before it answered/);
      expect(Date.now() - t0).to.be.below(5_000);
    });
  });

  describe("statistics", () => {
    it("interpolates quantiles", () => {
      expect(quantile([1, 2, 3, 4], 0.5)).to.equal(2.5);
      expect(quantile([10], 0.95)).to.equal(10);
      expect(quantile([0, 100], 0.95)).to.equal(95);
    });
    it("summarises finite values and ignores the rest", () => {
      expect(summarize([3, 1, 2, null, NaN, undefined])).to.deep.equal({ n: 3, median: 2, p95: 2.9, min: 1, max: 3, mean: 2 });
      expect(summarize([])).to.equal(null);
    });
    it("reads a median from a stat or a number", () => {
      expect(medianOf(stat(7))).to.equal(7);
      expect(medianOf(4)).to.equal(4);
      expect(medianOf("x")).to.equal(null);
    });
  });

  describe("hardware classes", () => {
    const rules = loadHardwareClasses().rules;
    const fixtures: { name: string; input: ClassInput; expected: string }[] = JSON.parse(readFileSync(path.join(DATA_DIR, "hardware-class-fixtures.json"), "utf8")).fixtures;

    for (const f of fixtures) {
      it(`classifies ${f.name} as ${f.expected}`, () => {
        expect(classify(f.input, rules)).to.equal(f.expected);
      });
    }
    it("ends in a catch-all, so every machine has a class", () => {
      expect(classify({ os: "other", arch: "other", memoryBytes: 1, backends: [], gpus: [] }, rules)).to.equal("cpu-8gb");
    });
    it("gives every class a family and tier, and unique ids", () => {
      const ids = rules.map((r) => r.class);
      expect(new Set(ids).size).to.equal(ids.length);
      for (const r of rules) expect(r.tierGb).to.be.greaterThan(0);
    });
    it("falls back to smaller tiers of the family, then to cpu classes", () => {
      expect(fallbackClasses("amd-igpu-uma-64", rules)).to.deep.equal(["amd-igpu-uma-64", "amd-igpu-uma-32", "amd-igpu-uma-16", "cpu-64gb", "cpu-32gb", "cpu-16gb", "cpu-8gb"]);
      expect(fallbackClasses("cpu-16gb", rules)).to.deep.equal(["cpu-16gb", "cpu-8gb"]);
      expect(fallbackClasses("nvidia-12gb", rules)).to.deep.equal(["nvidia-12gb", "nvidia-8gb", "cpu-8gb"]);
      expect(fallbackClasses("unknown", rules)).to.deep.equal(["unknown"]);
    });
    it("fits a model in min(GPU budget - 1 GiB, RAM - 4 GiB)", () => {
      expect(memoryFit(5 * GIB, 32 * GIB, 8 * GIB)).to.deep.include({ fits: true, budgetBytes: 7 * GIB });
      expect(memoryFit(7.5 * GIB, 32 * GIB, 8 * GIB).fits).to.be.false;
      expect(memoryFit(10 * GIB, 12 * GIB, 64 * GIB)).to.deep.include({ fits: false, budgetBytes: 8 * GIB });
      expect(memoryFit(20 * GIB, 32 * GIB, null)).to.deep.include({ fits: true, budgetBytes: 28 * GIB });
    });
    it("budgets a GPU by its kind", () => {
      expect(gpuBudget([{ vendor: "nvidia", name: "x", vramBytes: 12 * GIB, unified: false }], ["cpu", "cuda"], 64 * GIB)).to.equal(12 * GIB);
      expect(gpuBudget([{ vendor: "amd", name: "x", vramBytes: 0.5 * GIB, gttBytes: 61 * GIB, unified: true }], ["cpu", "vulkan"], 122 * GIB)).to.equal(61.5 * GIB);
      expect(gpuBudget([{ vendor: "apple", name: "M2", vramBytes: 0, unified: true }], ["cpu", "metal"], 16 * GIB)).to.equal(12 * GIB);
      expect(gpuBudget([{ vendor: "nvidia", name: "x", vramBytes: 12 * GIB, unified: false }], ["cpu"], 64 * GIB)).to.equal(null);
    });
    it("tells AMD APUs from discrete Radeons by name", () => {
      expect(isIntegratedAmdName("Advanced Micro Devices, Inc. [AMD/ATI] Strix Halo [Radeon Graphics / Radeon 8050S Graphics / Radeon 8060S Graphics]")).to.be.true;
      expect(isIntegratedAmdName("AMD Radeon 780M")).to.be.true;
      expect(isIntegratedAmdName("Navi 31 [Radeon RX 7900 XT/7900 XTX]")).to.be.false;
      expect(isIntegratedAmdName("Radeon Pro W7800")).to.be.false;
    });
    it("hashes the hardware, not the host, into an 8-hex fingerprint id", () => {
      const a = fp();
      expect(a.id).to.match(/^[0-9a-f]{8}$/);
      expect(fp({ osVersion: "Other" }).id).to.equal(a.id);
      expect(fp({ memoryBytes: 64 * GIB }).id).to.not.equal(a.id);
    });
    it("reads the backend from Ollama's log", () => {
      expect(ollamaBackendFromLog("... library=cpu ...\n... inference compute id=0 library=ROCm compute=gfx1151")).to.equal("rocm");
      expect(ollamaBackendFromLog("library=cpu")).to.equal("cpu");
      expect(ollamaBackendFromLog("nothing")).to.equal(undefined);
    });
  });

  describe("evidence", () => {
    it("accepts a well-formed file", () => {
      expect(validateEvidence(evidence("cpu-32gb", [cell({})]))).to.deep.equal([]);
    });
    it("names what is wrong with a malformed one", () => {
      const e: any = evidence("cpu-32gb", [
        cell({ status: "pass", checks: [{ name: "x", pass: false, detail: "no" }] }),
        cell({ status: "skipped" }),
        cell({ provider: "nope" as any, metrics: { bad: { median: 1 } as any } }),
      ]);
      e.schemaVersion = 2;
      const errors = validateEvidence(e);
      expect(errors).to.include("schemaVersion must be 1");
      expect(errors.some((x) => /cells\[0\] passes with a failed/.test(x))).to.be.true;
      expect(errors.some((x) => /cells\[1\] is skipped without a reason/.test(x))).to.be.true;
      expect(errors.some((x) => /cells\[2\]\.provider/.test(x))).to.be.true;
      expect(errors.some((x) => /metrics\.bad/.test(x))).to.be.true;
      expect(validateEvidence(null)).to.deep.equal(["evidence is not an object"]);
    });
    it("places evidence by class, date and fingerprint", () => {
      expect(evidenceRelPath("amd-igpu-uma-128", "2026-11-03T10:00:00Z", "d9d043fd")).to.equal("ai-wind-tunnel/evidence/amd-igpu-uma-128/2026-11-03-d9d043fd.json");
      expect(() => evidenceRelPath("../x", "2026-11-03", "d9d043fd")).to.throw(/safe/);
    });
    it("every checked-in evidence file validates and sits in its class's directory", () => {
      for (const e of loadAllEvidence()) expect(validateEvidence(e.evidence), e.path).to.deep.equal([]);
    });
  });

  describe("recommendations", () => {
    const llm = (id: string, params: string, decodeTps: number, ttftMs: number, extra: Partial<CellResult> = {}) =>
      cell({ model: { modelId: id, remoteModel: id, params, memoryBytes: 1e9 }, metrics: { decodeTps: stat(decodeTps), ttftMs: stat(ttftMs) }, ...extra });

    it("parses parameter counts", () => {
      expect(parseParams("0.8B")).to.equal(8e8);
      expect(parseParams("137M")).to.equal(1.37e8);
      expect(parseParams(undefined)).to.equal(null);
    });
    it("recommends the biggest model over the floor, citing its evidence", () => {
      const r = deriveRecommendations(
        [{ path: "ai-wind-tunnel/evidence/cpu-32gb/a.json", evidence: evidence("cpu-32gb", [llm("small", "1B", 60, 200), llm("big", "4B", 20, 900), llm("huge", "8B", 5, 3000)]) }],
        "2026-11-03T00:00:00.000Z",
      );
      expect(r.schemaVersion).to.equal(1);
      expect(r.classes["cpu-32gb"].llm).to.deep.include({ provider: "llama-server", modelId: "big", evidence: ["ai-wind-tunnel/evidence/cpu-32gb/a.json"], candidates: 3, overFloor: 2 });
      expect(r.classes["cpu-32gb"].llm!.metrics).to.deep.equal({ decodeTps: 20, ttftMs: 900 });
    });
    it("ignores failed cells and cells that do not fit the machine", () => {
      const fit = { fits: false, modelBytes: 1, budgetBytes: 0, gpuBudgetBytes: null, ramBytes: 1 };
      const r = deriveRecommendations(
        [{ path: "e.json", evidence: evidence("cpu-8gb", [llm("big", "4B", 30, 500, { status: "fail", reason: "wrong answer" }), llm("bigger", "8B", 30, 500, { fit }), llm("small", "1B", 30, 500)]) }],
        "t",
      );
      expect(r.classes["cpu-8gb"].llm!.modelId).to.equal("small");
    });
    it("gives a class with no cell over the floor, or no evidence, no recommendation", () => {
      const r = deriveRecommendations([{ path: "e.json", evidence: evidence("cpu-8gb", [llm("slow", "4B", 3, 9000)]) }], "t");
      expect(r.classes).to.deep.equal({});
      expect(deriveRecommendations([], "t").classes).to.deep.equal({});
    });
    it("combines runs of the same class: the median of medians, every file cited", () => {
      const r = deriveRecommendations(
        [
          { path: "a.json", evidence: evidence("cpu-16gb", [llm("m", "1B", 20, 400)]) },
          { path: "b.json", evidence: evidence("cpu-16gb", [llm("m", "1B", 40, 600)]) },
        ],
        "t",
      );
      expect(r.classes["cpu-16gb"].llm).to.deep.include({ metrics: { decodeTps: 30, ttftMs: 500 }, evidence: ["a.json", "b.json"] });
    });
    it("orders STT by word error rate, then real-time factor", () => {
      const stt = (id: string, w: number, rtf: number) => cell({ service: "stt", provider: "whisper-server", model: { modelId: id, remoteModel: id }, metrics: { werMean: w, rtf: stat(rtf) } });
      const r = deriveRecommendations([{ path: "e.json", evidence: evidence("cpu-16gb", [stt("fast-sloppy", 0.2, 0.05), stt("accurate", 0.0, 0.3), stt("accurate-faster", 0.01, 0.1), stt("too-slow", 0, 0.9)]) }], "t");
      expect(r.classes["cpu-16gb"].stt!.modelId).to.equal("accurate-faster");
    });
    it("needs every floor metric, and ranks a candidate missing a sort key last", () => {
      const noTps = cell({ model: { modelId: "no-tps", remoteModel: "no-tps", params: "8B" }, metrics: { ttftMs: stat(100) } });
      const r = deriveRecommendations([{ path: "e.json", evidence: evidence("cpu-16gb", [noTps, llm("small", "1B", 30, 500)]) }], "t");
      expect(r.classes["cpu-16gb"].llm).to.deep.include({ modelId: "small", candidates: 2, overFloor: 1 });
      const noParams = cell({ model: { modelId: "no-params", remoteModel: "no-params" }, metrics: { decodeTps: stat(90), ttftMs: stat(100) } });
      const r2 = deriveRecommendations([{ path: "e.json", evidence: evidence("cpu-16gb", [noParams, llm("small", "1B", 30, 500)]) }], "t");
      expect(r2.classes["cpu-16gb"].llm!.modelId).to.equal("small");
    });
    it("leaves out cells measured through a harness-only adapter", () => {
      const stt = (id: string, rtf: number, adapter?: string) =>
        cell({ service: "stt", provider: "llama-server", model: { modelId: id, remoteModel: id }, metrics: { werMean: 0, rtf: stat(rtf), ...(adapter ? { adapter } : {}) } });
      const r = deriveRecommendations(
        [{ path: "e.json", evidence: evidence("cpu-16gb", [stt("fronted", 0.01, "OpenAI-shaped front: x -> y"), stt("native", 0.1, "none: llama-server's own /v1/audio/transcriptions"), stt("plain", 0.2)]) }],
        "t",
      );
      expect(r.classes["cpu-16gb"].stt).to.deep.include({ modelId: "native", candidates: 2 });
      const only = deriveRecommendations([{ path: "e.json", evidence: evidence("cpu-16gb", [stt("fronted", 0.01, "OpenAI-shaped front: x -> y")]) }], "t");
      expect(only.classes).to.deep.equal({});
    });
    it("lets the newest evidence for a key decide: a newer failure withdraws it", () => {
      const r = deriveRecommendations(
        [
          { path: "old.json", evidence: evidence("cpu-16gb", [llm("big", "4B", 30, 500), llm("small", "1B", 30, 500)], "2026-11-03T00:00:00.000Z") },
          { path: "new.json", evidence: evidence("cpu-16gb", [llm("big", "4B", 30, 500, { status: "fail", reason: "wrong answer" })], "2026-12-01T00:00:00.000Z") },
        ],
        "t",
      );
      expect(r.classes["cpu-16gb"].llm).to.deep.include({ modelId: "small", candidates: 1 });
      // A newer pass after the failure brings it back, citing the passing files.
      const back = deriveRecommendations(
        [
          { path: "a.json", evidence: evidence("cpu-16gb", [llm("big", "4B", 30, 500)], "2026-11-03T00:00:00.000Z") },
          { path: "b.json", evidence: evidence("cpu-16gb", [llm("big", "4B", 30, 500, { status: "fail", reason: "x" })], "2026-11-10T00:00:00.000Z") },
          { path: "c.json", evidence: evidence("cpu-16gb", [llm("big", "4B", 30, 500)], "2026-12-01T00:00:00.000Z") },
        ],
        "t",
      );
      expect(back.classes["cpu-16gb"].llm).to.deep.include({ modelId: "big", evidence: ["a.json", "c.json"] });
    });
    it("keeps a provider's own model name when there is no manifest id", () => {
      const r = deriveRecommendations(
        [{ path: "e.json", evidence: evidence("cpu-16gb", [cell({ provider: "ollama", model: { remoteModel: "qwen3.5:2b", params: "2B" }, metrics: { decodeTps: stat(40), ttftMs: stat(300) } })]) }],
        "t",
      );
      expect(r.classes["cpu-16gb"].llm).to.deep.include({ provider: "ollama", remoteModel: "qwen3.5:2b" });
      expect(r.classes["cpu-16gb"].llm).to.not.have.property("modelId");
    });
    it("the checked-in recommendations.json is what the checked-in evidence derives", () => {
      const all = loadAllEvidence();
      const generatedAt = all.map((e) => e.evidence.finishedAt).sort().pop() ?? "1970-01-01T00:00:00.000Z";
      const derived = JSON.parse(JSON.stringify(deriveRecommendations(all, generatedAt)));
      const file = JSON.parse(readFileSync(path.join(DATA_DIR, "recommendations.json"), "utf8"));
      expect(file).to.deep.equal(derived);
      // Every recommendation cites evidence that exists.
      const paths = new Set(all.map((e) => e.path));
      for (const services of Object.values(file.classes) as any[]) for (const rec of Object.values(services) as any[]) for (const p of rec.evidence) expect(paths.has(p), p).to.be.true;
    });
  });
});
