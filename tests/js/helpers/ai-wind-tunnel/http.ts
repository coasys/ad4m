/** Timed OpenAI-shaped HTTP calls: JSON, SSE streams, multipart uploads, audio downloads. */

export interface Timed<T> {
  ms: number;
  value: T;
}

function headers(token?: string, json = true): Record<string, string> {
  return { ...(json ? { "Content-Type": "application/json" } : {}), ...(token ? { Authorization: `Bearer ${token}` } : {}) };
}

async function failure(res: Response, url: string): Promise<Error> {
  const text = await res.text().catch(() => "");
  return new Error(`${url} answered HTTP ${res.status}: ${text.slice(0, 400)}`);
}

export async function postJson(url: string, body: unknown, token?: string, timeoutMs = 600_000): Promise<Timed<any>> {
  const t0 = performance.now();
  const res = await fetch(url, { method: "POST", headers: headers(token), body: JSON.stringify(body), signal: AbortSignal.timeout(timeoutMs) });
  if (!res.ok) throw await failure(res, url);
  const value = await res.json();
  return { ms: performance.now() - t0, value };
}

export interface StreamResult {
  /** To the first generated token: content, reasoning or a tool call. */
  ttftMs: number | null;
  totalMs: number;
  /** Chunks that carried generated text. */
  tokenChunks: number;
  content: string;
  reasoning: string;
  usage: { prompt_tokens?: number; completion_tokens?: number } | null;
  /** Time of each token chunk after the request, ms. */
  chunkTimes: number[];
}

/** POST a chat or text completion with `stream: true` and read the SSE answer. */
export async function streamCompletion(url: string, body: Record<string, unknown>, token?: string, timeoutMs = 600_000): Promise<StreamResult> {
  const t0 = performance.now();
  const res = await fetch(url, {
    method: "POST",
    headers: headers(token),
    body: JSON.stringify({ ...body, stream: true, stream_options: { include_usage: true } }),
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!res.ok || !res.body) throw await failure(res, url);
  const out: StreamResult = { ttftMs: null, totalMs: 0, tokenChunks: 0, content: "", reasoning: "", usage: null, chunkTimes: [] };
  const decoder = new TextDecoder();
  let buf = "";
  const handle = (data: string) => {
    if (data === "[DONE]") return;
    let j: any;
    try {
      j = JSON.parse(data);
    } catch {
      return;
    }
    if (j.error) throw new Error(`stream error: ${JSON.stringify(j.error).slice(0, 300)}`);
    if (j.usage) out.usage = j.usage;
    for (const ch of j.choices ?? []) {
      const d = ch.delta ?? {};
      const text = (d.content ?? ch.text ?? "") as string;
      const reasoning = (d.reasoning_content ?? d.reasoning ?? "") as string;
      if (text || reasoning || d.tool_calls) {
        const now = performance.now() - t0;
        if (out.ttftMs === null) out.ttftMs = now;
        out.tokenChunks++;
        out.chunkTimes.push(now);
        out.content += text;
        out.reasoning += reasoning;
      }
    }
  };
  for await (const chunk of res.body as any as AsyncIterable<Uint8Array>) {
    buf += decoder.decode(chunk, { stream: true });
    let i: number;
    while ((i = buf.indexOf("\n")) >= 0) {
      const line = buf.slice(0, i).trim();
      buf = buf.slice(i + 1);
      if (line.startsWith("data:")) handle(line.slice(5).trim());
    }
  }
  if (buf.trim().startsWith("data:")) handle(buf.trim().slice(5).trim());
  out.totalMs = performance.now() - t0;
  return out;
}

/** Upload `wav` to an OpenAI transcription route; answers the text. */
export async function transcribe(v1: string, model: string, wav: Buffer, token?: string): Promise<Timed<string>> {
  const form = new FormData();
  form.append("file", new Blob([new Uint8Array(wav)], { type: "audio/wav" }), "audio.wav");
  form.append("model", model);
  form.append("response_format", "json");
  form.append("language", "en");
  const url = `${v1}/audio/transcriptions`;
  const t0 = performance.now();
  const res = await fetch(url, { method: "POST", headers: headers(token, false), body: form, signal: AbortSignal.timeout(600_000) });
  if (!res.ok) throw await failure(res, url);
  const j: any = await res.json();
  return { ms: performance.now() - t0, value: String(j.text ?? "") };
}

export interface SpeechResult {
  ttfbMs: number;
  totalMs: number;
  audio: Buffer;
  contentType: string;
}

/** POST to an OpenAI speech route and read the audio, timing its first byte. */
export async function speech(v1: string, body: Record<string, unknown>, token?: string): Promise<SpeechResult> {
  const url = `${v1}/audio/speech`;
  const t0 = performance.now();
  const res = await fetch(url, { method: "POST", headers: headers(token), body: JSON.stringify(body), signal: AbortSignal.timeout(600_000) });
  if (!res.ok || !res.body) throw await failure(res, url);
  const parts: Uint8Array[] = [];
  let ttfb: number | null = null;
  for await (const chunk of res.body as any as AsyncIterable<Uint8Array>) {
    if (ttfb === null && chunk.length) ttfb = performance.now() - t0;
    parts.push(new Uint8Array(chunk));
  }
  return { ttfbMs: ttfb ?? performance.now() - t0, totalMs: performance.now() - t0, audio: Buffer.concat(parts), contentType: res.headers.get("content-type") ?? "" };
}
