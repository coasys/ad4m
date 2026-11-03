/** Verification scoring: word error rate, retrieval ranking, answer checks, WAV decoding. Pure. */

/**
 * Text for word error rate: lower case, punctuation dropped (an apostrophe inside a word
 * stays: "it's"), hyphens split words, whole numbers below a million spelt out ("9" and
 * "nine" are the same word to a listener), whitespace collapsed.
 */
export function normalizeForWer(text: string): string[] {
  return text
    .toLowerCase()
    .normalize("NFKD")
    .replace(/\p{M}/gu, "")
    .replace(/[\p{Pi}\p{Pf}]/gu, "'")
    .replace(/(\d),(\d{3})\b/g, "$1$2")
    .replace(/[\p{Pd}/]/gu, " ")
    .replace(/[^a-z0-9' ]+/g, " ")
    .replace(/(^|\s)'+|'+(?=\s|$)/g, " ")
    .replace(/\b\d{1,6}\b/g, (n) => ` ${numberWords(Number(n))} `)
    .split(/\s+/)
    .filter(Boolean);
}

const ONES = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
const TENS = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];

/** 0 <= n < 1e6 in words, without "and": 342 -> "three hundred forty two". */
export function numberWords(n: number): string {
  if (n < 20) return ONES[n];
  if (n < 100) return TENS[Math.floor(n / 10)] + (n % 10 ? ` ${ONES[n % 10]}` : "");
  if (n < 1000) return `${ONES[Math.floor(n / 100)]} hundred` + (n % 100 ? ` ${numberWords(n % 100)}` : "");
  return `${numberWords(Math.floor(n / 1000))} thousand` + (n % 1000 ? ` ${numberWords(n % 1000)}` : "");
}

/** Word-level edit distance (substitutions + deletions + insertions). */
export function wordEditDistance(ref: string[], hyp: string[]): number {
  let prev = Array.from({ length: hyp.length + 1 }, (_, j) => j);
  for (let i = 1; i <= ref.length; i++) {
    const cur = [i];
    for (let j = 1; j <= hyp.length; j++) {
      cur[j] = Math.min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + (ref[i - 1] === hyp[j - 1] ? 0 : 1));
    }
    prev = cur;
  }
  return prev[hyp.length];
}

/** Word error rate of `hypothesis` against `reference`: edits / reference words. 0 is perfect; it can exceed 1. */
export function wer(reference: string, hypothesis: string): number {
  const ref = normalizeForWer(reference);
  const hyp = normalizeForWer(hypothesis);
  if (ref.length === 0) return hyp.length === 0 ? 0 : 1;
  return wordEditDistance(ref, hyp) / ref.length;
}

/** The answer with any `<think>...</think>` block (and an unclosed trailing one) removed. */
export function stripThinking(text: string): string {
  return text.replace(/<think>[\s\S]*?<\/think>/g, "").replace(/<think>[\s\S]*$/, "").trim();
}

export function cosine(a: number[], b: number[]): number {
  if (a.length !== b.length) throw new Error(`vectors differ in dimension: ${a.length} vs ${b.length}`);
  let dot = 0, na = 0, nb = 0;
  for (let i = 0; i < a.length; i++) {
    dot += a[i] * b[i];
    na += a[i] * a[i];
    nb += b[i] * b[i];
  }
  if (na === 0 || nb === 0) return 0;
  return dot / Math.sqrt(na * nb);
}

/** Passage indices ordered by cosine similarity to the query, best first. */
export function rank(query: number[], passages: number[][]): number[] {
  return passages
    .map((p, i) => ({ i, s: cosine(query, p) }))
    .sort((x, y) => y.s - x.s)
    .map((x) => x.i);
}

export interface RetrievalOutcome {
  /** Queries whose expected passage ranked first. */
  correct: number;
  total: number;
  /** Mean reciprocal rank of the expected passages. */
  mrr: number;
  misses: { query: number; expected: number; got: number }[];
}

export function scoreRetrieval(queries: number[][], passages: number[][], expected: number[]): RetrievalOutcome {
  let correct = 0, rr = 0;
  const misses: RetrievalOutcome["misses"] = [];
  queries.forEach((q, qi) => {
    const order = rank(q, passages);
    const at = order.indexOf(expected[qi]);
    rr += 1 / (at + 1);
    if (at === 0) correct++;
    else misses.push({ query: qi, expected: expected[qi], got: order[0] });
  });
  return { correct, total: queries.length, mrr: queries.length ? rr / queries.length : 0, misses };
}

/** Every vector has the same, non-zero dimension: that dimension, else null. */
export function consistentDimension(vectors: number[][]): number | null {
  if (vectors.length === 0) return null;
  const d = vectors[0].length;
  return d > 0 && vectors.every((v) => v.length === d && v.every(Number.isFinite)) ? d : null;
}

export interface ToolCallCheck {
  pass: boolean;
  detail: string;
}

/** The first tool call names `name` and carries JSON arguments whose `argKey` matches `argPattern`. */
export function checkToolCall(
  toolCalls: Array<{ function?: { name?: string; arguments?: string | object } }> | undefined,
  name: string,
  argKey: string,
  argPattern: RegExp,
): ToolCallCheck {
  const call = toolCalls?.[0];
  if (!call?.function) return { pass: false, detail: "no tool call in the reply" };
  if (call.function.name !== name) return { pass: false, detail: `called '${call.function.name}', not '${name}'` };
  let args: any;
  try {
    args = typeof call.function.arguments === "string" ? JSON.parse(call.function.arguments) : call.function.arguments;
  } catch {
    return { pass: false, detail: `arguments are not JSON: ${String(call.function.arguments).slice(0, 120)}` };
  }
  if (!args || typeof args !== "object") return { pass: false, detail: "arguments are not an object" };
  const v = args[argKey];
  if (typeof v !== "string" || !argPattern.test(v)) return { pass: false, detail: `${argKey}=${JSON.stringify(v)} does not match ${argPattern}` };
  return { pass: true, detail: `${name}(${JSON.stringify(args)})` };
}

export interface DecodedWav {
  sampleRate: number;
  channels: number;
  bitsPerSample: number;
  /** Mono samples in -1..1 (channels averaged). */
  samples: Float32Array;
  durationSec: number;
}

/**
 * Decode a RIFF/WAVE buffer: PCM 8/16/24/32-bit or IEEE float 32-bit. A data chunk whose
 * size is unknown (0 or 0xFFFFFFFF, as streaming servers write it) runs to the end.
 */
export function decodeWav(buf: Buffer): DecodedWav {
  if (buf.length < 12 || buf.toString("ascii", 0, 4) !== "RIFF" || buf.toString("ascii", 8, 12) !== "WAVE") {
    throw new Error("not a RIFF/WAVE file");
  }
  let off = 12;
  let fmt: { format: number; channels: number; sampleRate: number; bits: number } | null = null;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4);
    let size = buf.readUInt32LE(off + 4);
    const body = off + 8;
    if (id === "fmt ") {
      let format = buf.readUInt16LE(body);
      if (format === 0xfffe && size >= 26) format = buf.readUInt16LE(body + 24); // WAVE_FORMAT_EXTENSIBLE
      fmt = { format, channels: buf.readUInt16LE(body + 2), sampleRate: buf.readUInt32LE(body + 4), bits: buf.readUInt16LE(body + 14) };
    } else if (id === "data") {
      if (!fmt) throw new Error("data chunk before fmt chunk");
      if (size === 0 || size === 0xffffffff || body + size > buf.length) size = buf.length - body;
      const bytes = fmt.bits / 8;
      const frames = Math.floor(size / (bytes * fmt.channels));
      const samples = new Float32Array(frames);
      for (let f = 0; f < frames; f++) {
        let acc = 0;
        for (let c = 0; c < fmt.channels; c++) {
          const p = body + (f * fmt.channels + c) * bytes;
          if (fmt.format === 3 && fmt.bits === 32) acc += buf.readFloatLE(p);
          else if (fmt.bits === 16) acc += buf.readInt16LE(p) / 32768;
          else if (fmt.bits === 24) acc += buf.readIntLE(p, 3) / 8388608;
          else if (fmt.bits === 32) acc += buf.readInt32LE(p) / 2147483648;
          else if (fmt.bits === 8) acc += (buf.readUInt8(p) - 128) / 128;
          else throw new Error(`unsupported WAV sample format ${fmt.format}/${fmt.bits}`);
        }
        samples[f] = acc / fmt.channels;
      }
      return { sampleRate: fmt.sampleRate, channels: fmt.channels, bitsPerSample: fmt.bits, samples, durationSec: frames / fmt.sampleRate };
    }
    off = body + size + (size % 2);
  }
  throw new Error("no data chunk");
}

/** 16-bit PCM mono WAV of `samples` (-1..1). */
export function encodeWav(samples: Float32Array, sampleRate: number): Buffer {
  const data = Buffer.alloc(samples.length * 2);
  for (let i = 0; i < samples.length; i++) data.writeInt16LE(Math.round(Math.max(-1, Math.min(1, samples[i])) * 32767), i * 2);
  const h = Buffer.alloc(44);
  h.write("RIFF", 0, "ascii");
  h.writeUInt32LE(36 + data.length, 4);
  h.write("WAVE", 8, "ascii");
  h.write("fmt ", 12, "ascii");
  h.writeUInt32LE(16, 16);
  h.writeUInt16LE(1, 20);
  h.writeUInt16LE(1, 22);
  h.writeUInt32LE(sampleRate, 24);
  h.writeUInt32LE(sampleRate * 2, 28);
  h.writeUInt16LE(2, 32);
  h.writeUInt16LE(16, 34);
  h.write("data", 36, "ascii");
  h.writeUInt32LE(data.length, 40);
  return Buffer.concat([new Uint8Array(h.buffer, h.byteOffset, h.length), new Uint8Array(data.buffer, data.byteOffset, data.length)]);
}

/** Linear-interpolation resampling, enough for speech into a 16 kHz recogniser. */
export function resample(samples: Float32Array, from: number, to: number): Float32Array {
  if (from === to) return samples;
  const n = Math.floor((samples.length * to) / from);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const x = (i * from) / to;
    const i0 = Math.floor(x);
    const i1 = Math.min(i0 + 1, samples.length - 1);
    out[i] = samples[i0] + (samples[i1] - samples[i0]) * (x - i0);
  }
  return out;
}

/** Root-mean-square level: near 0 is silence. */
export function rms(samples: Float32Array): number {
  let s = 0;
  for (let i = 0; i < samples.length; i++) s += samples[i] * samples[i];
  return samples.length ? Math.sqrt(s / samples.length) : 0;
}

/**
 * Seconds of speech plausible for `words` words: 0.15 s to 1.2 s a word (fast speech to a
 * slow, paused read), plus up to 1.5 s of leading and trailing silence.
 */
export function speechDurationRange(words: number): { min: number; max: number } {
  return { min: words * 0.15, max: words * 1.2 + 1.5 };
}

/**
 * A GPU build must hold the model in GPU memory: a build that finds no device runs on the
 * CPU without saying so, and its numbers would pass for the GPU's. Null (no check) for a CPU
 * backend, an unknown model size, or where GPU memory is not readable (`peakGpuBytes` null).
 */
export function checkOffload(
  backend: string | undefined,
  peakGpuBytes: number | null,
  modelBytes: number | undefined,
): { name: string; pass: boolean; detail: string; value: number; threshold: string } | null {
  if (!backend || backend === "cpu" || !modelBytes || peakGpuBytes === null) return null;
  const pass = peakGpuBytes >= modelBytes * 0.5;
  return {
    name: `model runs on the GPU (${backend})`,
    pass,
    detail: `${(peakGpuBytes / 1e9).toFixed(2)} GB of GPU memory held for a ${(modelBytes / 1e9).toFixed(2)} GB model${pass ? "" : ": the build fell back to the CPU"}`,
    value: peakGpuBytes,
    threshold: ">= half the model size",
  };
}
