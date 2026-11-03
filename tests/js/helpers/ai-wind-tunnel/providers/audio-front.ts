/**
 * An OpenAI-shaped transcription front for a server that does not speak
 * `POST /v1/audio/transcriptions`: sherpa-onnx (a WebSocket protocol of its own) and
 * llama-server (audio models answer `/v1/chat/completions` with an `input_audio` part).
 * The executor's `OPEN_AI_AUDIO` endpoint points at the front, which turns each multipart
 * upload into the server's request and answers `json`, `verbose_json` or `text`. It is what
 * the executor would need to do itself (an endpoint kind) or run beside the server (a
 * sidecar) to use such a server; its cost is part of what the cells measure.
 */

import { createServer, type IncomingMessage, type Server, type ServerResponse } from "node:http";
import type { AddressInfo } from "node:net";
import { decodeWav, resample } from "../verify.js";

export interface FrontReply {
  text: string;
  /** The language the model says it heard, when it says. */
  language?: string;
}

/** Transcribe one uploaded file (as uploaded: a WAV from the executor) with the request's form fields. */
export type FrontTranscriber = (audio: Buffer, fields: Record<string, string>) => Promise<FrontReply>;

export interface AudioFront {
  /** Server root without `/v1`, what the executor's endpoint gets. */
  baseUrl: string;
  close(): Promise<void>;
}

/** 16 kHz mono samples of an uploaded WAV. */
export function wavTo16k(audio: Buffer): { samples: Float32Array; durationSec: number } {
  const w = decodeWav(audio);
  const samples = w.sampleRate === 16000 ? w.samples : resample(w.samples, w.sampleRate, 16000);
  return { samples, durationSec: samples.length / 16000 };
}

async function readBody(req: IncomingMessage): Promise<Buffer> {
  const parts: Buffer[] = [];
  for await (const c of req) parts.push(c as Buffer);
  return Buffer.concat(parts);
}

function send(res: ServerResponse, status: number, body: unknown, type = "application/json"): void {
  const text = typeof body === "string" ? body : JSON.stringify(body);
  res.writeHead(status, { "Content-Type": type, "Content-Length": Buffer.byteLength(text) });
  res.end(text);
}

/**
 * Serve `transcribe` on 127.0.0.1:`port` as `POST /v1/audio/transcriptions`, and list
 * `modelId` on `GET /v1/models` so the executor's health check finds the endpoint reachable.
 */
export async function startAudioFront(port: number, modelId: string, transcribe: FrontTranscriber): Promise<AudioFront> {
  const server: Server = createServer(async (req, res) => {
    try {
      const url = new URL(req.url ?? "/", "http://front");
      if (req.method === "GET" && url.pathname === "/v1/models") {
        return send(res, 200, { object: "list", data: [{ id: modelId, object: "model", owned_by: "local" }] });
      }
      if (req.method !== "POST" || url.pathname !== "/v1/audio/transcriptions") {
        return send(res, 404, { error: { message: `no route ${req.method} ${url.pathname}`, type: "not_found" } });
      }
      const body = await readBody(req);
      const form = await new Response(body, { headers: { "Content-Type": req.headers["content-type"] ?? "" } }).formData();
      const file = form.get("file");
      if (!file || typeof file === "string") return send(res, 400, { error: { message: "the upload has no file part", type: "invalid_request_error" } });
      const fields: Record<string, string> = {};
      for (const [k, v] of form.entries()) if (typeof v === "string") fields[k] = v;
      const audio = Buffer.from(await file.arrayBuffer());
      const reply = await transcribe(audio, fields);
      const format = fields.response_format ?? "json";
      if (format === "text") return send(res, 200, reply.text, "text/plain; charset=utf-8");
      if (format === "verbose_json") {
        let duration: number | null = null;
        try {
          duration = wavTo16k(audio).durationSec;
        } catch {}
        return send(res, 200, { task: "transcribe", language: reply.language ?? "", duration, text: reply.text });
      }
      return send(res, 200, { text: reply.text });
    } catch (e: any) {
      send(res, 500, { error: { message: String(e?.message ?? e), type: "server_error" } });
    }
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, "127.0.0.1", () => resolve());
  });
  const bound = (server.address() as AddressInfo).port;
  return {
    baseUrl: `http://127.0.0.1:${bound}`,
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  };
}

// ---------------------------------------------------------------------------------------------
// sherpa-onnx: `sherpa-onnx-offline-websocket-server`
// ---------------------------------------------------------------------------------------------

/**
 * One utterance as sherpa-onnx's offline WebSocket server takes it: a binary message of
 * the sample rate (int32 LE), the byte length of the samples (int32 LE), then the samples
 * as float32 LE (sherpa-onnx/csrc/offline-websocket-server-impl.cc, `OnMessage`).
 */
export function sherpaFrame(samples: Float32Array, sampleRate = 16000): Buffer {
  const head = Buffer.alloc(8);
  head.writeInt32LE(sampleRate, 0);
  head.writeInt32LE(samples.byteLength, 4);
  return Buffer.concat([head, Buffer.from(samples.buffer, samples.byteOffset, samples.byteLength)]);
}

/** The server's answer: `OfflineRecognitionResult::AsJsonString` (`text`, `lang`, tokens...). */
export function parseSherpaResult(message: string): FrontReply {
  const j = JSON.parse(message);
  if (typeof j.text !== "string") throw new Error(`sherpa-onnx answered no text: ${message.slice(0, 200)}`);
  return { text: j.text.trim(), ...(j.lang ? { language: String(j.lang) } : {}) };
}

/** Transcribe `samples` (16 kHz mono) on the sherpa-onnx server at `wsUrl`: one connection per utterance. */
export function sherpaTranscribe(wsUrl: string, samples: Float32Array, timeoutMs = 120_000): Promise<FrontReply> {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(wsUrl);
    ws.binaryType = "arraybuffer";
    let settled = false;
    const finish = (fn: () => void) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      fn();
    };
    const timer = setTimeout(() => finish(() => (ws.close(), reject(new Error(`sherpa-onnx gave no answer in ${timeoutMs / 1000} s`)))), timeoutMs);
    ws.onopen = () => ws.send(sherpaFrame(samples));
    ws.onmessage = (ev) => {
      const text = typeof ev.data === "string" ? ev.data : Buffer.from(ev.data as ArrayBuffer).toString("utf8");
      finish(() => {
        try {
          resolve(parseSherpaResult(text));
        } catch (e) {
          reject(e);
        }
        // The server closes the connection once told the client is done.
        ws.send("Done");
      });
    };
    ws.onerror = () => finish(() => reject(new Error(`sherpa-onnx WebSocket ${wsUrl} failed`)));
    ws.onclose = (ev) => finish(() => reject(new Error(`sherpa-onnx closed the connection: ${ev.code} ${ev.reason}`)));
  });
}

// ---------------------------------------------------------------------------------------------
// llama-server: audio models through `/v1/chat/completions`
// ---------------------------------------------------------------------------------------------

/** What a general audio model is told so that it transcribes: a system message and an instruction after the audio. */
export interface SttPrompt {
  system?: string;
  instruction?: string;
}

/**
 * The chat request that transcribes `wav` on llama-server: one user message with the audio
 * as an `input_audio` part (tools/server/README.md, "For multimodal input"), and for models
 * that need telling, a system message and the instruction as a text part after the audio.
 * Greedy, so runs repeat.
 */
export function chatTranscriptionRequest(model: string, wav: Buffer, prompt?: SttPrompt): Record<string, unknown> {
  const content: unknown[] = [{ type: "input_audio", input_audio: { data: wav.toString("base64"), format: "wav" } }];
  if (prompt?.instruction) content.push({ type: "text", text: prompt.instruction });
  const messages: unknown[] = [{ role: "user", content }];
  if (prompt?.system) messages.unshift({ role: "system", content: prompt.system });
  return { model, messages, temperature: 0, max_tokens: 1024 };
}

/**
 * The transcript in an audio model's chat answer. Qwen3-ASR answers
 * `language English<asr_text>The text.` (the language it detected, then the text); other
 * models answer the text.
 */
export function parseChatTranscript(content: string): FrontReply {
  const m = content.match(/^\s*language\s+([^<\n]*?)\s*<asr_text>([\s\S]*)$/);
  if (m) return { text: m[2].trim(), ...(m[1] && m[1].toLowerCase() !== "none" ? { language: m[1] } : {}) };
  return { text: content.trim() };
}
