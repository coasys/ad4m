import type { Ad4mClient } from "@coasys/ad4m";

/**
 * Register `model` on the OpenAI-shaped server at `baseUrl` as an LLM named `name`, and
 * answer its id. The server's endpoint is saved once per executor and reused after.
 * For the suites that run against a real model (`LLM_E2E=1`, nightly).
 */
export async function addOpenAiShapedLlm(
  client: Ad4mClient,
  name: string,
  baseUrl: string,
  model: string,
  apiKey = "ollama"
): Promise<string> {
  const trim = (url: string) => url.replace(/\/+$/, "");
  const saved = (await client.ai.endpoints()).find((e) => trim(e.baseUrl) === trim(baseUrl));
  const endpoint =
    saved ??
    (await client.ai.addEndpoint({
      name: `llm-${new URL(baseUrl).host}`,
      kind: "OPEN_AI_COMPATIBLE",
      baseUrl,
      apiKey,
    }));
  return client.ai.addModel({ name, modelType: "LLM", endpointId: endpoint.id, remoteModel: model });
}
