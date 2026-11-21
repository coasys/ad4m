import type { Ad4mClient, EndpointKind } from "@coasys/ad4m";

/**
 * Staging's llama-server, one slot at `-c 65536`. The nightly `llm-e2e` job
 * points here and does not start a server of its own.
 */
export const E2E_SERVER_SLOT_TOKENS = 65_536;

/**
 * Context budget handed to the executor. The slot is 65536; the chat template
 * and the server's reasoning budget (2048) sit on top of what we send, so the
 * default stays 8192 under the slot. A request that fills the slot is refused.
 */
export const E2E_MAX_NUM_CTX_DEFAULT = E2E_SERVER_SLOT_TOKENS - 8_192;

export const E2E_BASE_URL =
  process.env.INTERPRETATION_E2E_BASE_URL || "http://127.0.0.1:12460/v1";

export const E2E_MODEL =
  process.env.INTERPRETATION_E2E_MODEL || "qwen3.8-27b-ud-q4_k_m";

const ENDPOINT_KINDS: readonly EndpointKind[] = [
  "OLLAMA",
  "LLAMA_SERVER",
  "VLLM",
  "OPEN_AI",
  "ANTHROPIC",
  "OPEN_AI_AUDIO",
  "OPEN_AI_COMPATIBLE",
];

/**
 * `LLAMA_SERVER` is the kind whose client sends tools as definitions and can
 * turn thinking off (`chat_template_kwargs`). llama-server is started with
 * `--jinja`, which is what makes those definitions come back as `tool_calls`.
 * An `OPEN_AI` endpoint sends the same `tools` array but cannot disable
 * thinking, so a reasoning model spends the window and answers empty.
 */
export function e2eEndpointKind(): EndpointKind {
  const raw = process.env.INTERPRETATION_E2E_ENDPOINT_KIND || "LLAMA_SERVER";
  if ((ENDPOINT_KINDS as readonly string[]).includes(raw)) return raw as EndpointKind;
  throw new Error(
    `INTERPRETATION_E2E_ENDPOINT_KIND=${raw} is not an endpoint kind. llama-server --jinja wants LLAMA_SERVER`,
  );
}

/** Stay strictly under the server slot. An over-slot value uses the default. */
export function e2eMaxNumCtx(): number {
  const requested = Number(process.env.INTERPRETATION_E2E_MAX_NUM_CTX);
  const value =
    Number.isFinite(requested) && requested > 0 ? requested : E2E_MAX_NUM_CTX_DEFAULT;
  if (value >= E2E_SERVER_SLOT_TOKENS) {
    console.warn(
      `[e2e] INTERPRETATION_E2E_MAX_NUM_CTX=${value} is not under the llama-server slot (${E2E_SERVER_SLOT_TOKENS}); using ${E2E_MAX_NUM_CTX_DEFAULT}`,
    );
    return E2E_MAX_NUM_CTX_DEFAULT;
  }
  return value;
}

/**
 * How long one completion may take, queueing on the single slot included.
 * A few minutes of waiting behind staging, then generation. Capped so a
 * stray env value cannot park a test forever.
 */
export const E2E_REQUEST_TIMEOUT_SECS = (() => {
  const requested = Number(process.env.INTERPRETATION_E2E_REQUEST_TIMEOUT_SECS);
  const value = Number.isFinite(requested) && requested > 0 ? requested : 900;
  return Math.min(Math.max(value, 60), 1_800);
})();

/** How long a test waits for one model pass, queueing included. */
export const E2E_PASS_TIMEOUT_MS = 600_000;

/**
 * Register the LLM the nightly suites prompt, and answer its id.
 *
 * Kind, base URL and model come from the environment (defaults: staging's
 * llama-server). `nativeTools` is set: an OpenAI-shaped server, llama-server
 * included, does not send tools unless the model says so, and the
 * prompt-injected path is the one that 500s against a Qwen tool parser.
 * `maxNumCtx` is the budget the executor sizes prompts to. It is not a
 * per-request context the server applies — the slot is already 65536 — and
 * it stays under that slot.
 *
 * The endpoint is saved once per executor and reused after.
 */
export async function addE2eLlm(client: Ad4mClient, name: string): Promise<string> {
  const baseUrl = E2E_BASE_URL;
  const kind = e2eEndpointKind();
  const maxNumCtx = e2eMaxNumCtx();
  const requestTimeoutSecs = E2E_REQUEST_TIMEOUT_SECS;
  const apiKey = process.env.INTERPRETATION_E2E_API_KEY;
  const trim = (url: string) => url.replace(/\/+$/, "");
  const saved = (await client.ai.endpoints()).find(
    (e) =>
      e.kind === kind &&
      trim(e.baseUrl) === trim(baseUrl) &&
      e.maxNumCtx === maxNumCtx &&
      e.requestTimeoutSecs === requestTimeoutSecs,
  );
  const endpoint =
    saved ??
    (await client.ai.addEndpoint({
      name: `llm-${kind.toLowerCase()}-${new URL(baseUrl).host}`,
      kind,
      baseUrl,
      ...(apiKey ? { apiKey } : {}),
      maxNumCtx,
      requestTimeoutSecs,
    }));
  return client.ai.addModel({
    name,
    modelType: "LLM",
    endpointId: endpoint.id,
    remoteModel: E2E_MODEL,
    nativeTools: true,
  });
}
