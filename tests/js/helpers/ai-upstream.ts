// A fake OpenAI-shaped model server for the per-push AI contract tests: the executor talks
// to it over its real wire, and no model weights are involved. Deterministic answers:
//
//   GET  /v1/models                  the three models below
//   POST /v1/chat/completions        "echo: <last user message>", as JSON or SSE (with
//                                    `usage`); a request with `tools` gets a call of the
//                                    first tool instead
//   POST /v1/embeddings              one 8-dimension vector per input, from its characters
//   POST /v1/audio/transcriptions    {"text": "fake transcript of <n> bytes"}
//
// Every request is recorded (`requests`) so a test can check what the executor sent.

import http from "http";
import { AddressInfo } from "net";

export const FAKE_CHAT_MODEL = "fake-chat";
export const FAKE_EMBED_MODEL = "fake-embed";
export const FAKE_STT_MODEL = "fake-stt";

/** The vector the fake answers for `text`. */
export function fakeVector(text: string): number[] {
  const v = new Array(8).fill(0);
  for (let i = 0; i < text.length; i++) v[i % 8] += text.charCodeAt(i) / 1000;
  return v.map((x) => Math.round(x * 1000) / 1000);
}

export interface RecordedRequest {
  method: string;
  path: string;
  body: Buffer;
  json?: any;
}

export interface FakeUpstream {
  /** `http://127.0.0.1:<port>`, without `/v1`. */
  url: string;
  requests: RecordedRequest[];
  close(): Promise<void>;
}

function lastUserText(messages: any[]): string {
  const last = [...(messages ?? [])].reverse().find((m) => m.role === "user");
  if (!last) return "";
  return typeof last.content === "string"
    ? last.content
    : (last.content ?? []).map((p: any) => p.text ?? "").join("");
}

function chat(req: any, res: http.ServerResponse) {
  const text = `echo: ${lastUserText(req.messages)}`;
  const tool = req.tools?.[0]?.function?.name;
  const usage = { prompt_tokens: 7, completion_tokens: 3, total_tokens: 10 };
  const message = tool
    ? {
        role: "assistant",
        content: null,
        tool_calls: [
          { id: "call_1", type: "function", function: { name: tool, arguments: "{}" } },
        ],
      }
    : { role: "assistant", content: text };

  if (!req.stream) {
    res.writeHead(200, { "content-type": "application/json" });
    res.end(
      JSON.stringify({
        id: "chatcmpl-fake",
        object: "chat.completion",
        created: 0,
        model: req.model,
        choices: [{ index: 0, message, finish_reason: tool ? "tool_calls" : "stop" }],
        usage,
      })
    );
    return;
  }
  res.writeHead(200, { "content-type": "text/event-stream" });
  const chunk = (delta: any, finish: string | null, extra: any = {}) =>
    res.write(
      `data: ${JSON.stringify({
        id: "chatcmpl-fake",
        object: "chat.completion.chunk",
        created: 0,
        model: req.model,
        choices: [{ index: 0, delta, finish_reason: finish }],
        ...extra,
      })}\n\n`
    );
  chunk({ role: "assistant" }, null);
  if (tool) {
    chunk({ tool_calls: message.tool_calls!.map((c, index) => ({ index, ...c })) }, null);
  } else {
    for (const word of text.split(/(?= )/)) chunk({ content: word }, null);
  }
  chunk({}, tool ? "tool_calls" : "stop");
  if (req.stream_options?.include_usage) {
    res.write(
      `data: ${JSON.stringify({ id: "chatcmpl-fake", object: "chat.completion.chunk", created: 0, model: req.model, choices: [], usage })}\n\n`
    );
  }
  res.end("data: [DONE]\n\n");
}

/** Starts the fake on a free loopback port. */
export async function startFakeUpstream(): Promise<FakeUpstream> {
  const requests: RecordedRequest[] = [];
  const server = http.createServer((request, res) => {
    const chunks: Buffer[] = [];
    request.on("data", (c) => chunks.push(c));
    request.on("end", () => {
      const body = Buffer.concat(chunks);
      const path = (request.url ?? "").split("?")[0];
      let json: any;
      if ((request.headers["content-type"] ?? "").includes("application/json")) {
        try {
          json = JSON.parse(body.toString("utf8"));
        } catch {
          json = undefined;
        }
      }
      requests.push({ method: request.method ?? "", path, body, json });

      if (request.method === "GET" && path === "/v1/models") {
        res.writeHead(200, { "content-type": "application/json" });
        res.end(
          JSON.stringify({
            object: "list",
            data: [FAKE_CHAT_MODEL, FAKE_EMBED_MODEL, FAKE_STT_MODEL].map((id) => ({
              id,
              object: "model",
              owned_by: "fake",
            })),
          })
        );
      } else if (request.method === "POST" && path === "/v1/chat/completions") {
        chat(json ?? {}, res);
      } else if (request.method === "POST" && path === "/v1/embeddings") {
        const inputs: string[] = Array.isArray(json?.input) ? json.input : [json?.input ?? ""];
        res.writeHead(200, { "content-type": "application/json" });
        res.end(
          JSON.stringify({
            object: "list",
            model: json?.model,
            data: inputs.map((text, index) => ({
              object: "embedding",
              index,
              embedding: fakeVector(text),
            })),
            usage: { prompt_tokens: inputs.length, total_tokens: inputs.length },
          })
        );
      } else if (request.method === "POST" && path === "/v1/audio/transcriptions") {
        res.writeHead(200, { "content-type": "application/json" });
        res.end(JSON.stringify({ text: `fake transcript of ${body.length} bytes` }));
      } else {
        res.writeHead(404, { "content-type": "application/json" });
        res.end(JSON.stringify({ error: { message: `no route ${path}` } }));
      }
    });
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address() as AddressInfo;
  return {
    url: `http://127.0.0.1:${port}`,
    requests,
    close: () => new Promise((resolve) => server.close(() => resolve())),
  };
}
