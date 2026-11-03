import type {
  Ad4mClient,
  AiEndpoint,
  DiscoveredModel,
  EndpointKind,
  LocalAiPlannedModel,
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

/**
 * How the launcher names an endpoint's server. A managed endpoint (local AI) by the runtime
 * the executor runs for it, not by the kind's examples: its transcription server is
 * nemo-speech, which no user-added transcription server need be.
 */
export function endpointLabel(endpoint: Pick<AiEndpoint, "kind" | "managed">): string {
  if (endpoint.managed) {
    if (endpoint.kind === "LLAMA_SERVER") return "llama-server";
    if (endpoint.kind === "OPEN_AI_AUDIO") return "nemo-speech (Parakeet)";
  }
  return kindLabel(endpoint.kind);
}

/**
 * The note beside a planned local model the wind tunnel did not measure on this hardware
 * (`basis: "default"`: the manifest's smallest model for the service), else none.
 */
export function basisNote(model: Pick<LocalAiPlannedModel, "basis">): string | null {
  return model.basis === "default" ? "not measured on this hardware" : null;
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

/** The ids of the models discovery says chat. */
export function chatModels(discovered: DiscoveredModel[]): string[] {
  return discovered.filter((m) => m.capabilities.includes("chat")).map((m) => m.id);
}

/**
 * The chat model onboarding preselects: the first one the first detected server already
 * serves (`servedChat`, by base URL), so a person with a model server can go on without a
 * download. `null` when none serves one.
 */
export function preselectedChat<S extends { baseUrl: string }>(
  servers: S[],
  servedChat: Record<string, string[]>
): { server: S; remoteModel: string } | null {
  for (const server of servers) {
    const remoteModel = servedChat[server.baseUrl]?.[0];
    if (remoteModel) return { server, remoteModel };
  }
  return null;
}

/**
 * The context-window ceiling typed into the form: `undefined` when empty (the endpoint
 * keeps none, and Ollama's own window per model applies), the number when it is a whole
 * number the executor's `u32` holds, `null` when it is neither.
 */
export function parseMaxNumCtx(typed: string): number | undefined | null {
  if (typed.trim() === "") return undefined;
  const cap = Number(typed);
  return Number.isSafeInteger(cap) && cap > 0 && cap <= 0xffffffff ? cap : null;
}

/**
 * `endpoint` as an update input with `maxNumCtx` as its ceiling and every other setting
 * as it is (the key is left out, so the stored one stays).
 */
export function withMaxNumCtx(endpoint: AiEndpoint, maxNumCtx: number | undefined) {
  return {
    name: endpoint.name,
    kind: endpoint.kind,
    baseUrl: endpoint.baseUrl,
    capabilities: endpoint.capabilities,
    maxNumCtx,
    requestTimeoutSecs: endpoint.requestTimeoutSecs ?? undefined,
    maxConcurrent: endpoint.maxConcurrent ?? undefined,
  };
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
