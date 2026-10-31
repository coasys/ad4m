import type {
  Ad4mClient,
  AiEndpoint,
  EndpointKind,
  ModelType,
} from "@coasys/ad4m";

/** How the launcher names each kind of model server. */
export const ENDPOINT_KINDS: { kind: EndpointKind; label: string; baseUrl: string }[] = [
  { kind: "OLLAMA", label: "Ollama", baseUrl: "http://127.0.0.1:11434" },
  { kind: "LLAMA_SERVER", label: "llama-server", baseUrl: "http://127.0.0.1:9931" },
  { kind: "VLLM", label: "vLLM", baseUrl: "http://127.0.0.1:8000" },
  { kind: "OPEN_AI_AUDIO", label: "Transcription server (speaches, Parakeet)", baseUrl: "http://127.0.0.1:8000" },
  { kind: "OPEN_AI", label: "OpenAI", baseUrl: "https://api.openai.com/v1" },
  { kind: "ANTHROPIC", label: "Anthropic", baseUrl: "https://api.anthropic.com" },
  { kind: "OPEN_AI_COMPATIBLE", label: "Other OpenAI-compatible API", baseUrl: "https://" },
];

export function kindLabel(kind: EndpointKind): string {
  return ENDPOINT_KINDS.find((k) => k.kind === kind)?.label ?? kind;
}

/** The capability a model type needs from its endpoint. */
export function capabilityOf(modelType: ModelType) {
  switch (modelType) {
    case "LLM":
      return "chat" as const;
    case "EMBEDDING":
      return "embeddings" as const;
    case "TRANSCRIPTION":
      return "transcription" as const;
    case "SPEECH":
      return "speech" as const;
  }
}

/** Whether `served` lists `model` (Ollama lists `m` as `m:latest`). */
export function serves(served: string[], model: string): boolean {
  return served.some((s) => s === model || s === `${model}:latest`);
}

function sameUrl(a: string, b: string): boolean {
  return a.replace(/\/+$/, "") === b.replace(/\/+$/, "");
}

/**
 * The saved endpoint of `kind` at `baseUrl`, or a new one (named by its kind and port,
 * suffixed when the name is taken). The executor checks a new endpoint once before it
 * saves it.
 */
export async function endpointFor(
  client: Ad4mClient,
  kind: EndpointKind,
  baseUrl: string,
  apiKey?: string
): Promise<AiEndpoint> {
  const endpoints = await client.ai.endpoints();
  const saved = endpoints.find((e) => e.kind === kind && sameUrl(e.baseUrl, baseUrl));
  if (saved && !apiKey) return saved;
  const port = new URL(baseUrl).port;
  const base = `${kindLabel(kind).split(" ")[0].toLowerCase()}${port ? `-${port}` : ""}`;
  const taken = new Set(endpoints.map((e) => e.name));
  let name = base;
  for (let n = 2; taken.has(name); n++) name = `${base}-${n}`;
  return client.ai.addEndpoint({ name, kind, baseUrl, apiKey: apiKey || undefined });
}

/**
 * Register `remoteModel` on `endpoint` as a `modelType` model, unless it is registered
 * already, and make it the default of its type when the type has none. Answers its id.
 */
export async function registerModel(
  client: Ad4mClient,
  endpoint: AiEndpoint,
  modelType: ModelType,
  remoteModel: string
): Promise<string> {
  const existing = (await client.ai.getModels()).find(
    (m) =>
      m.endpointId === endpoint.id &&
      m.remoteModel === remoteModel &&
      m.modelType === modelType
  );
  const id =
    existing?.id ??
    (await client.ai.addModel({
      name: remoteModel,
      modelType,
      endpointId: endpoint.id,
      remoteModel,
    }));
  if (!(await client.ai.getDefaultModel(modelType))) {
    await client.ai.setDefaultModel(modelType, id);
  }
  return id;
}
