/**
 * What each cell does. Every measured call goes through the executor: `/v1` for the
 * OpenAI-shaped API and the SDK's `ai.*` RPCs (task prompts, embeddings, transcription
 * streams). With `directV1` the same request also goes straight to the provider, so the
 * executor's overhead is a number.
 */

import { PROMPTS, WEATHER_TOOL, loadAudio, loadRetrieval, loadSttFixtures, type LoadedAudio } from "./fixtures.js";
import { postJson, speech, streamCompletion, transcribe, transcribeNoHint, type StreamResult } from "./http.js";
import type { WtExecutor } from "./executor.js";
import { summarize } from "./stats.js";
import type { Check, EndpointKindId, Metric, Stat } from "./types.js";
import { checkToolCall, consistentDimension, cosine, decodeWav, encodeWav, resample, rms, scoreRetrieval, speechDurationRange, stripThinking, wer } from "./verify.js";

export interface CellEnv {
  ex: WtExecutor;
  /** The executor's name for the model: what `/v1` requests name. */
  modelName: string;
  /** The executor's model id. */
  modelId: string;
  /** The model id the provider knows, for direct calls. */
  remoteModel: string;
  kind: EndpointKindId;
  /** The provider's own `/v1`, when direct calls are measured. */
  directV1: string | null;
  repeat: number;
  concurrency: number[];
  voice?: string;
  /** For the TTS round trip: an STT model on the executor that passed its cell. */
  sttReference?: { modelName: string; label: string };
  log(line: string): void;
}

export interface CellOutcome {
  checks: Check[];
  metrics: Record<string, Metric>;
}

const nonce = () => `[${Math.random().toString(36).slice(2, 10)}] `;

/** The executor's call and the direct one, in alternating order by repetition `i`. */
async function fairPair(i: number, viaExecutor: () => Promise<unknown>, direct: (() => Promise<unknown>) | null): Promise<void> {
  if (direct && i % 2 === 1) {
    await direct();
    await viaExecutor();
  } else {
    await viaExecutor();
    if (direct) await direct();
  }
}

/**
 * Request fields that turn thinking off: the chat template's switch for llama-server and
 * vLLM, `reasoning_effort: "none"` for Ollama's OpenAI route (the executor passes `/v1`
 * chat through to it; with thinking on, a reasoning model spends `max_tokens` thinking and
 * answers nothing).
 */
function noThinking(kind: EndpointKindId): Record<string, unknown> {
  if (kind === "OLLAMA") return { reasoning_effort: "none" };
  return kind === "LLAMA_SERVER" || kind === "VLLM" || kind === "OPEN_AI_COMPATIBLE" ? { chat_template_kwargs: { enable_thinking: false } } : {};
}

function overhead(exec: Stat | null, direct: Stat | null): number | null {
  return exec && direct ? Math.round((exec.median - direct.median) * 10) / 10 : null;
}

async function attempt<T>(checks: Check[], name: string, fn: () => Promise<{ pass: boolean; detail: string; value?: any; threshold?: any; result?: T }>): Promise<T | undefined> {
  try {
    const r = await fn();
    checks.push({ name, pass: r.pass, detail: r.detail, ...(r.value !== undefined ? { value: r.value } : {}), ...(r.threshold !== undefined ? { threshold: r.threshold } : {}) });
    return r.result;
  } catch (e: any) {
    checks.push({ name, pass: false, detail: `error: ${String(e?.message ?? e).slice(0, 500)}` });
    return undefined;
  }
}

function decodeRate(s: StreamResult): { tokens: number; tps: number | null } {
  const tokens = s.usage?.completion_tokens ?? s.tokenChunks;
  if (s.ttftMs === null || tokens < 2) return { tokens, tps: null };
  const decodeMs = (s.chunkTimes[s.chunkTimes.length - 1] ?? s.totalMs) - s.ttftMs;
  return { tokens, tps: decodeMs > 0 ? ((tokens - 1) * 1000) / decodeMs : null };
}

// ---------------------------------------------------------------------------------------------
// llm: chat (factual), task prompt over the SDK, tool call, streaming; TTFT, decode and prefill
// rates, latency, throughput at each concurrency, executor overhead.
// ---------------------------------------------------------------------------------------------

export async function llmCell(env: CellEnv): Promise<CellOutcome> {
  const { ex } = env;
  const checks: Check[] = [];
  const metrics: Record<string, Metric> = {};
  const url = `${ex.v1}/chat/completions`;
  const chat = (content: string, extra: Record<string, unknown> = {}) => ({
    model: env.modelName,
    messages: [
      { role: "system", content: PROMPTS.system },
      { role: "user", content },
    ],
    temperature: 0,
    max_tokens: 1024,
    ...noThinking(env.kind),
    ...extra,
  });

  // First request: a cold model loads here.
  await attempt(checks, "factual answer (/v1/chat/completions)", async () => {
    const r = await postJson(url, chat(PROMPTS.factual), ex.token);
    metrics.firstRequestMs = Math.round(r.ms);
    const text = stripThinking(r.value.choices?.[0]?.message?.content ?? "");
    return { pass: PROMPTS.factualExpect.test(text), detail: JSON.stringify(text.slice(0, 160)), value: text.slice(0, 160) };
  });

  await attempt(checks, "task prompt (SDK ai.prompt)", async () => {
    const task = await ex.client.ai.addTask(`wt-${env.modelId}`, env.modelId, PROMPTS.system, []);
    try {
      const text = stripThinking(await ex.client.ai.prompt(task.taskId, PROMPTS.factual, { timeoutMs: 600_000 }));
      return { pass: PROMPTS.factualExpect.test(text), detail: JSON.stringify(text.slice(0, 160)) };
    } finally {
      await ex.client.ai.removeTask(task.taskId).catch(() => {});
    }
  });

  await attempt(checks, "tool call (/v1/chat/completions, tools)", async () => {
    const r = await postJson(url, chat(PROMPTS.tool, { tools: [WEATHER_TOOL], tool_choice: "auto" }), ex.token);
    const c = checkToolCall(r.value.choices?.[0]?.message?.tool_calls, "get_weather", "city", /paris/i);
    return { pass: c.pass, detail: c.detail };
  });

  await attempt(checks, "streaming (/v1/chat/completions, stream)", async () => {
    const s = await streamCompletion(url, chat(PROMPTS.stream), ex.token);
    const text = stripThinking(s.content);
    const pass = s.tokenChunks > 1 && PROMPTS.streamExpect.test(text);
    return { pass, detail: `${s.tokenChunks} chunks, ${JSON.stringify(text.slice(0, 120))}`, value: s.tokenChunks, threshold: "> 1 chunk" };
  });

  // Performance.
  const gen = (extra: Record<string, unknown> = {}) => chat(nonce() + PROMPTS.generate, { max_tokens: 128, temperature: 0.7, ...extra });
  const ttft: number[] = [], tps: number[] = [], prefill: number[] = [], lat: number[] = [], direct: number[] = [];
  let usageSeen = false;
  for (let i = 0; i < env.repeat; i++) {
    const s = await streamCompletion(url, gen(), ex.token);
    if (s.ttftMs !== null) ttft.push(s.ttftMs);
    const d = decodeRate(s);
    if (d.tps !== null) tps.push(d.tps);
    usageSeen ||= !!s.usage?.completion_tokens;

    const long = await streamCompletion(url, chat(nonce() + PROMPTS.longContext, { max_tokens: 1 }), ex.token);
    const promptTokens = long.usage?.prompt_tokens ?? Math.round((nonce() + PROMPTS.longContext + PROMPTS.system).length / 4);
    if (long.ttftMs) prefill.push((promptTokens * 1000) / long.ttftMs);

    // Each call its own nonce (no prompt-cache hit from the call before) and the order
    // alternating, so the executor-vs-direct difference is the executor's, not the cache's.
    await fairPair(
      i,
      async () => lat.push((await postJson(url, chat(nonce() + PROMPTS.factual, { max_tokens: 16 }), ex.token)).ms),
      env.directV1
        ? async () => direct.push((await postJson(`${env.directV1}/chat/completions`, { ...chat(nonce() + PROMPTS.factual, { max_tokens: 16 }), model: env.remoteModel })).ms)
        : null,
    );
  }
  metrics.ttftMs = summarize(ttft);
  metrics.decodeTps = summarize(tps);
  metrics.prefillTps = summarize(prefill);
  metrics.latencyMs = summarize(lat);
  metrics.tokenCountSource = usageSeen ? "usage" : "chunks";
  if (env.directV1) {
    metrics.directLatencyMs = summarize(direct);
    metrics.executorOverheadMs = overhead(metrics.latencyMs, metrics.directLatencyMs);
  }
  for (const c of env.concurrency) {
    const runs: number[] = [];
    for (let i = 0; i < Math.max(1, Math.min(env.repeat, 2)); i++) {
      const t0 = performance.now();
      const results = await Promise.all(Array.from({ length: c }, () => streamCompletion(url, gen(), ex.token)));
      const tokens = results.reduce((a, s) => a + decodeRate(s).tokens, 0);
      runs.push((tokens * 1000) / (performance.now() - t0));
    }
    metrics[`throughputTpsC${c}`] = summarize(runs);
  }
  return { checks, metrics };
}

// ---------------------------------------------------------------------------------------------
// completion: /v1/completions answers the prompt's continuation.
// ---------------------------------------------------------------------------------------------

export async function completionCell(env: CellEnv): Promise<CellOutcome> {
  const { ex } = env;
  const checks: Check[] = [];
  const metrics: Record<string, Metric> = {};
  const body = (model: string, prefix = "") => ({ model, prompt: prefix + PROMPTS.completion, max_tokens: 16, temperature: 0 });
  await attempt(checks, "continuation (/v1/completions)", async () => {
    const r = await postJson(`${ex.v1}/completions`, body(env.modelName), ex.token);
    metrics.firstRequestMs = Math.round(r.ms);
    const text = stripThinking(r.value.choices?.[0]?.text ?? "");
    return { pass: PROMPTS.completionExpect.test(text), detail: JSON.stringify(text.slice(0, 160)) };
  });
  const lat: number[] = [], direct: number[] = [];
  for (let i = 0; i < env.repeat; i++) {
    await fairPair(
      i,
      async () => lat.push((await postJson(`${ex.v1}/completions`, body(env.modelName, nonce()), ex.token)).ms),
      env.directV1 ? async () => direct.push((await postJson(`${env.directV1}/completions`, body(env.remoteModel, nonce()))).ms) : null,
    );
  }
  metrics.latencyMs = summarize(lat);
  if (env.directV1) {
    metrics.directLatencyMs = summarize(direct);
    metrics.executorOverheadMs = overhead(metrics.latencyMs, metrics.directLatencyMs);
  }
  return { checks, metrics };
}

// ---------------------------------------------------------------------------------------------
// embeddings: one dimension everywhere, the SDK agrees with /v1, retrieval ranks right.
// ---------------------------------------------------------------------------------------------

export async function embeddingsCell(env: CellEnv): Promise<CellOutcome> {
  const { ex } = env;
  const checks: Check[] = [];
  const metrics: Record<string, Metric> = {};
  const fx = loadRetrieval();
  const url = `${ex.v1}/embeddings`;
  const embed = async (input: string[], base = url, model = env.modelName, token: string | undefined = ex.token) => {
    const r = await postJson(base, { model, input }, token);
    const data = [...(r.value.data ?? [])].sort((a: any, b: any) => a.index - b.index);
    return { ms: r.ms, vectors: data.map((d: any) => d.embedding as number[]) };
  };

  const docs = fx.passages.map((p) => fx.documentPrefix + p);
  const queries = fx.queries.map((q) => fx.queryPrefix + q.text);
  const first = await attempt(checks, "embeds a batch (/v1/embeddings)", async () => {
    const r = await embed(docs);
    metrics.firstRequestMs = Math.round(r.ms);
    return { pass: r.vectors.length === docs.length, detail: `${r.vectors.length} vectors for ${docs.length} inputs`, result: r.vectors };
  });
  const qv = await attempt(checks, "embeds queries one at a time", async () => {
    const vs: number[][] = [];
    for (const q of queries) vs.push((await embed([q])).vectors[0]);
    return { pass: vs.length === queries.length, detail: `${vs.length} query vectors`, result: vs };
  });
  if (first && qv) {
    const dim = consistentDimension([...first, ...qv]);
    metrics.dimension = dim;
    checks.push({ name: "dimension is consistent", pass: dim !== null, detail: dim ? `${dim} dimensions` : "dimensions differ or are empty", value: dim });
    if (dim) {
      const out = scoreRetrieval(qv, first, fx.queries.map((q) => q.expected));
      metrics.mrr = Math.round(out.mrr * 1000) / 1000;
      checks.push({
        name: "retrieval ranks the expected passage first",
        pass: out.correct === out.total,
        detail: `${out.correct}/${out.total} top-1, MRR ${out.mrr.toFixed(3)}${out.misses.length ? `; misses ${JSON.stringify(out.misses)}` : ""}`,
        value: out.correct,
        threshold: out.total,
      });
    }
    await attempt(checks, "SDK ai.embed matches /v1", async () => {
      const v = await ex.client.ai.embed(env.modelId, docs[0], { timeoutMs: 120_000 });
      const sim = v.length === first[0].length ? cosine(v, first[0]) : -1;
      return { pass: sim > 0.99, detail: `${v.length} dims, cosine ${sim.toFixed(4)} to the /v1 vector`, value: Math.round(sim * 1e4) / 1e4, threshold: "> 0.99" };
    });
  }

  const batch = Array.from({ length: 32 }, (_, i) => `${fx.documentPrefix}${i}: ${fx.passages[i % fx.passages.length]}`);
  const batchMs: number[] = [], single: number[] = [], direct: number[] = [];
  for (let i = 0; i < env.repeat; i++) {
    batchMs.push((await embed(batch)).ms);
    single.push((await embed([queries[i % queries.length]])).ms);
    if (env.directV1) direct.push((await embed(batch, `${env.directV1}/embeddings`, env.remoteModel, undefined)).ms);
  }
  metrics.batchLatencyMs = summarize(batchMs);
  metrics.textsPerSec = summarize(batchMs.map((ms) => (batch.length * 1000) / ms));
  metrics.singleLatencyMs = summarize(single);
  if (env.directV1) {
    metrics.directBatchLatencyMs = summarize(direct);
    metrics.executorOverheadMs = overhead(metrics.batchLatencyMs, metrics.directBatchLatencyMs);
  }
  return { checks, metrics };
}

// ---------------------------------------------------------------------------------------------
// stt: word error rate on recordings with known transcripts, over /v1 and the SDK's stream.
// ---------------------------------------------------------------------------------------------

let audioCache: LoadedAudio[] | null = null;
export function sttFixtures(): LoadedAudio[] {
  return (audioCache ??= loadSttFixtures().utterances.map(loadAudio));
}

export async function sttCell(env: CellEnv): Promise<CellOutcome> {
  const { ex } = env;
  const checks: Check[] = [];
  const metrics: Record<string, Metric> = {};
  const clips = sttFixtures();
  const wers: number[] = [], streamWers: number[] = [], rtf: number[] = [], lat: number[] = [], streamLat: number[] = [], direct: number[] = [];
  let first = true;
  for (const clip of clips) {
    await attempt(checks, `transcribes ${clip.name} (/v1/audio/transcriptions)`, async () => {
      const r = await transcribe(ex.v1, env.modelName, clip.wav16k, ex.token);
      if (first) metrics.firstRequestMs = Math.round(r.ms);
      first = false;
      const w = wer(clip.transcript, r.value);
      wers.push(w);
      return { pass: w <= clip.maxWer, detail: `WER ${w.toFixed(3)}: ${JSON.stringify(r.value.trim().slice(0, 160))}`, value: Math.round(w * 1000) / 1000, threshold: clip.maxWer };
    });
    await attempt(checks, `transcribes ${clip.name} (SDK transcription stream)`, async () => {
      const texts: string[] = [];
      const streamId = await ex.client.ai.openTranscriptionStream(env.modelId, (t) => texts.push(t));
      try {
        const t0 = performance.now();
        await ex.client.ai.feedTranscriptionStream(streamId, clip.samples);
        const deadline = Date.now() + 120_000;
        while (texts.length === 0 && Date.now() < deadline) await new Promise((r) => setTimeout(r, 20));
        streamLat.push(performance.now() - t0);
        const text = texts.join(" ");
        const w = wer(clip.transcript, text);
        streamWers.push(w);
        return { pass: w <= clip.maxWer, detail: `WER ${w.toFixed(3)}: ${JSON.stringify(text.trim().slice(0, 160))}`, value: Math.round(w * 1000) / 1000, threshold: clip.maxWer };
      } finally {
        await ex.client.ai.closeTranscriptionStream(streamId).catch(() => {});
      }
    });
    // Language detection: the stream sends no language hint, so the model decides what it
    // hears. The noisy clip is where a model mistakes the language; record what it said.
    if (clip.noise) {
      try {
        const r = await transcribeNoHint(ex.v1, env.modelName, clip.wav16k, ex.token);
        metrics.noisyLanguage = r.value.language ?? "not reported";
        metrics.noisyNoHintWer = Math.round(wer(clip.transcript, r.value.text) * 1000) / 1000;
        metrics.noisyNoHintText = r.value.text.trim().slice(0, 200);
      } catch (e: any) {
        metrics.noisyLanguage = `failed: ${String(e?.message ?? e).slice(0, 200)}`;
      }
    }
    for (let i = 0; i < env.repeat; i++) {
      const r = await transcribe(ex.v1, env.modelName, clip.wav16k, ex.token);
      lat.push(r.ms);
      rtf.push(r.ms / 1000 / clip.durationSec);
      if (env.directV1) direct.push((await transcribe(env.directV1, env.remoteModel, clip.wav16k)).ms);
    }
  }
  metrics.wer = summarize(wers);
  // The mean over clips: the median of mostly clean clips is 0 for every model, the mean is not.
  metrics.werMean = wers.length ? Math.round((wers.reduce((a, b) => a + b, 0) / wers.length) * 1000) / 1000 : null;
  // The same over the SDK stream, which sends no language hint.
  metrics.streamWerMean = streamWers.length ? Math.round((streamWers.reduce((a, b) => a + b, 0) / streamWers.length) * 1000) / 1000 : null;
  metrics.rtf = summarize(rtf);
  metrics.latencyMs = summarize(lat);
  metrics.streamLatencyMs = summarize(streamLat);
  metrics.audioSec = Math.round(clips.reduce((a, c) => a + c.durationSec, 0) * 100) / 100;
  if (env.directV1) {
    metrics.directLatencyMs = summarize(direct);
    metrics.executorOverheadMs = overhead(metrics.latencyMs, metrics.directLatencyMs);
  }
  return { checks, metrics };
}

// ---------------------------------------------------------------------------------------------
// tts: audio that decodes, lasts as long as the words need, and comes back through STT.
// ---------------------------------------------------------------------------------------------

export async function ttsCell(env: CellEnv): Promise<CellOutcome> {
  const { ex } = env;
  const checks: Check[] = [];
  const metrics: Record<string, Metric> = {};
  const texts = loadSttFixtures().ttsTexts;
  const body = (model: string, input: string) => ({ model, input, voice: env.voice ?? "alloy", response_format: "wav" });
  const rtf: number[] = [], ttfb: number[] = [], lat: number[] = [], rtWer: number[] = [], direct: number[] = [];
  let first = true;
  for (const [ti, t] of texts.entries()) {
    const audio = await attempt(checks, `speaks text ${ti + 1} (/v1/audio/speech)`, async () => {
      const r = await speech(ex.v1, body(env.modelName, t.text), ex.token);
      if (first) metrics.firstRequestMs = Math.round(r.totalMs);
      first = false;
      const wav = decodeWav(r.audio);
      const words = t.text.split(/\s+/).length;
      const range = speechDurationRange(words);
      const level = rms(wav.samples);
      const pass = wav.durationSec >= range.min && wav.durationSec <= range.max && level > 0.005;
      return {
        pass,
        detail: `${wav.durationSec.toFixed(2)} s at ${wav.sampleRate} Hz for ${words} words (expected ${range.min.toFixed(1)}-${range.max.toFixed(1)} s), RMS ${level.toFixed(3)}`,
        value: Math.round(wav.durationSec * 100) / 100,
        threshold: `${range.min.toFixed(1)}-${range.max.toFixed(1)} s`,
        result: wav,
      };
    });
    if (!audio) continue;
    if (!env.sttReference) {
      checks.push({ name: `round trip of text ${ti + 1} through STT`, pass: false, detail: "no STT model passed its cell in this run to transcribe with" });
    } else {
      await attempt(checks, `round trip of text ${ti + 1} through STT (${env.sttReference.label})`, async () => {
        const wav16 = encodeWav(resample(audio.samples, audio.sampleRate, 16000), 16000);
        const r = await transcribe(ex.v1, env.sttReference!.modelName, wav16, ex.token);
        const w = wer(t.text, r.value);
        rtWer.push(w);
        return { pass: w <= t.maxWer, detail: `WER ${w.toFixed(3)}: ${JSON.stringify(r.value.trim().slice(0, 160))}`, value: Math.round(w * 1000) / 1000, threshold: t.maxWer };
      });
    }
    for (let i = 0; i < env.repeat; i++) {
      const r = await speech(ex.v1, body(env.modelName, t.text), ex.token);
      const dur = decodeWav(r.audio).durationSec;
      lat.push(r.totalMs);
      ttfb.push(r.ttfbMs);
      if (dur > 0) rtf.push(r.totalMs / 1000 / dur);
      if (env.directV1) direct.push((await speech(env.directV1, body(env.remoteModel, t.text))).totalMs);
    }
  }
  metrics.rtf = summarize(rtf);
  metrics.ttfbMs = summarize(ttfb);
  metrics.latencyMs = summarize(lat);
  metrics.roundTripWer = summarize(rtWer);
  if (env.sttReference) metrics.roundTripStt = env.sttReference.label;
  if (env.directV1) {
    metrics.directLatencyMs = summarize(direct);
    metrics.executorOverheadMs = overhead(metrics.latencyMs, metrics.directLatencyMs);
  }
  return { checks, metrics };
}
