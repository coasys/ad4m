/** Fixtures the cells verify against: prompts, the retrieval set, audio with known transcripts, TTS texts. */

import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import path from "node:path";
import { FIXTURES_DIR, REPO_ROOT } from "./paths.js";
import { addNoise, decodeWav, encodeWav, resample } from "./verify.js";

export interface RetrievalFixture {
  queryPrefix: string;
  documentPrefix: string;
  passages: string[];
  queries: { text: string; expected: number }[];
}

export const loadRetrieval = (): RetrievalFixture => JSON.parse(readFileSync(path.join(FIXTURES_DIR, "retrieval.json"), "utf8"));

export interface AudioFixture {
  name: string;
  /** Repo-relative. */
  file: string;
  transcript: string;
  maxWer: number;
  source: string;
  /** Add seeded white noise this many dB below the signal (a harder variant of a clip). */
  noise?: { snrDb: number; seed: number };
}

export interface LoadedAudio extends AudioFixture {
  /** 16 kHz mono. */
  samples: Float32Array;
  wav16k: Buffer;
  durationSec: number;
}

export interface SttFixtures {
  utterances: AudioFixture[];
  /** Texts the TTS cells speak; their round trip through STT must come back. */
  ttsTexts: { text: string; maxWer: number }[];
}

export const loadSttFixtures = (): SttFixtures => JSON.parse(readFileSync(path.join(FIXTURES_DIR, "audio.json"), "utf8"));

/** Any audio file as 16 kHz mono samples: WAV in TS, anything else through ffmpeg. */
export function decodeTo16k(file: string): Float32Array {
  if (file.endsWith(".wav")) {
    const w = decodeWav(readFileSync(file));
    return resample(w.samples, w.sampleRate, 16000);
  }
  let raw: Buffer;
  try {
    raw = execFileSync("ffmpeg", ["-loglevel", "error", "-i", file, "-f", "f32le", "-ac", "1", "-ar", "16000", "-"], { maxBuffer: 256 * 1024 * 1024 });
  } catch (e: any) {
    throw new Error(`decoding ${file} needs ffmpeg on PATH: ${e.message}`);
  }
  return new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4).slice();
}

export function loadAudio(f: AudioFixture): LoadedAudio {
  const clean = decodeTo16k(path.join(REPO_ROOT, f.file));
  const samples = f.noise ? addNoise(clean, f.noise.snrDb, f.noise.seed) : clean;
  return { ...f, samples, wav16k: encodeWav(samples, 16000), durationSec: samples.length / 16000 };
}

export const PROMPTS = {
  system: "You are a concise assistant. Answer briefly.",
  factual: "What is the capital of France? Answer with the city name only.",
  factualExpect: /\bparis\b/i,
  tool: "What is the weather in Paris right now? Use the get_weather tool.",
  stream: "Count from one to ten in words, separated by commas.",
  generate: "Write a short paragraph about the history of the bicycle.",
  completion: "The capital city of France is",
  completionExpect: /\bparis\b/i,
  /** About 600 tokens; a nonce in front defeats prompt caches, so each repetition prefills it all. */
  longContext:
    "The following is a set of notes on the history of cartography. ".repeat(4) +
    "Early maps were drawn on clay tablets in Mesopotamia and showed fields, rivers and cities. Greek scholars such as Eratosthenes estimated the circumference of the Earth and introduced lines of latitude. Ptolemy's Geography collected coordinates for thousands of places and shaped European maps for more than a millennium. Medieval mappae mundi placed Jerusalem at the centre and mixed geography with scripture. Portolan charts drawn by Mediterranean sailors showed coastlines with remarkable accuracy and were criss-crossed by rhumb lines. The age of exploration brought new coastlines, and Mercator's projection of 1569 let navigators plot a constant compass bearing as a straight line. Triangulation surveys in the eighteenth century, such as the Cassini survey of France, measured whole countries with chains and theodolites. Aerial photography in the twentieth century and satellite imagery later made maps faster to update, and digital geographic information systems now layer roads, buildings and elevation into one searchable model. ".repeat(4) +
    "Summarise these notes in one sentence.",
};

export const WEATHER_TOOL = {
  type: "function",
  function: {
    name: "get_weather",
    description: "Get the current weather for a city.",
    parameters: {
      type: "object",
      properties: { city: { type: "string", description: "The city name" } },
      required: ["city"],
    },
  },
};
