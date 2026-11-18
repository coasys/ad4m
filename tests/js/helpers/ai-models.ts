import type { Ad4mClient } from "@coasys/ad4m";

/**
 * Register `model` on the Ollama at `baseUrl` as an LLM named `name`, and answer its id.
 * The endpoint is native Ollama, so tools reach the model as definitions on `/api/chat`
 * (Ollama 0.40's Qwen parser answers tools written into the prompt with a 500), and
 * `maxNumCtx` caps the context it asks for (the provider sends the Modelfile's num_ctx,
 * else the model's trained window, up to that cap), so Ollama keeps the model at staging's
 * residency. The endpoint is saved
 * once per executor and reused after.
 * For the suites that run against a real model (`LLM_E2E=1`, nightly).
 */
export async function addOllamaLlm(
  client: Ad4mClient,
  name: string,
  baseUrl: string,
  model: string,
  maxNumCtx: number,
  apiKey = "ollama"
): Promise<string> {
  const trim = (url: string) => url.replace(/\/+$/, "");
  const saved = (await client.ai.endpoints()).find(
    (e) => e.kind === "OLLAMA" && trim(e.baseUrl) === trim(baseUrl) && e.maxNumCtx === maxNumCtx
  );
  const endpoint =
    saved ??
    (await client.ai.addEndpoint({
      name: `llm-${new URL(baseUrl).host}`,
      kind: "OLLAMA",
      baseUrl,
      apiKey,
      maxNumCtx,
    }));
  return client.ai.addModel({ name, modelType: "LLM", endpointId: endpoint.id, remoteModel: model });
}
