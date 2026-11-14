/**
 * The AI wind tunnel's shapes: the cells it runs, the evidence a run writes and the
 * fingerprint of the machine it ran on. Evidence files are data other code reads (the
 * recommendation script, and later the executor's local AI setup), so every field here
 * is part of schema v1: add fields, never change the meaning of one.
 */

export const EVIDENCE_SCHEMA_VERSION = 1;
export const RECOMMENDATIONS_SCHEMA_VERSION = 1;

export const SERVICES = ["llm", "completion", "embeddings", "stt", "tts"] as const;
export type Service = (typeof SERVICES)[number];

export const PROVIDERS = ["ollama", "llama-server", "vllm", "whisper-server", "speaches", "nemo-speech", "sherpa-onnx"] as const;
export type ProviderId = (typeof PROVIDERS)[number];

/**
 * pass: every check held. fail: a check did not hold, or the cell broke while running.
 * unsupported: the provider cannot serve the service at all. skipped: the provider is
 * not available on this machine (no binary, no GPU, no docker).
 */
export type CellStatus = "pass" | "fail" | "unsupported" | "skipped";

export type EndpointKindId =
  | "OLLAMA"
  | "LLAMA_SERVER"
  | "VLLM"
  | "OPEN_AI"
  | "ANTHROPIC"
  | "OPEN_AI_AUDIO"
  | "OPEN_AI_COMPATIBLE";

/** A model a cell runs: a manifest entry by `modelId`, or a name the provider pulls itself. */
export interface CellModel {
  modelId?: string;
  remoteModel: string;
  sizeBytes?: number;
  memoryBytes?: number;
  license?: string;
  params?: string;
  source?: string;
  /** The manifest's sha256 of the model file. */
  sha256?: string;
  /** TTS voice. */
  voice?: string;
  /** llama-server's context in tokens (`-c`); left out: 8192. A model meant for long prompts is measured at the context it is recommended with. */
  context?: number;
}

/** One cell of the matrix before it runs. */
export interface PlannedCell {
  provider: ProviderId;
  service: Service;
  model?: CellModel;
  /** Set for `unsupported` cells: why the provider cannot serve the service. */
  unsupported?: string;
}

export interface Check {
  name: string;
  pass: boolean;
  detail: string;
  value?: number | string | boolean | null;
  threshold?: number | string | null;
}

/** Median, p95 and spread of one measured quantity over the repetitions. */
export interface Stat {
  n: number;
  median: number;
  p95: number;
  min: number;
  max: number;
  mean: number;
}

export type Metric = Stat | number | string | boolean | null;

export interface MemoryFit {
  fits: boolean;
  modelBytes: number;
  /** min(GPU budget - 1 GiB, RAM - 4 GiB); null GPU budget means RAM only. */
  budgetBytes: number;
  gpuBudgetBytes: number | null;
  ramBytes: number;
}

export interface CellResult {
  provider: ProviderId;
  service: Service;
  status: CellStatus;
  /** Why the cell is unsupported, skipped or failed. */
  reason?: string;
  model?: CellModel;
  /** A manifest runtime build (`linux-x64-rocm`), for runtimes the executor can install. */
  runtimeBuild?: string;
  /** The compute backend the provider ran on: cpu, rocm, vulkan, cuda, metal. */
  backend?: string;
  endpointKind?: EndpointKindId;
  /** Server flags the run started the provider with that shape serving, without paths or ports. */
  serverFlags?: string[];
  checks: Check[];
  metrics: Record<string, Metric>;
  fit?: MemoryFit;
  durationMs: number;
}

export interface GpuInfo {
  vendor: "nvidia" | "amd" | "intel" | "apple" | "other";
  name: string;
  /** Dedicated memory; 0 when the GPU has none of its own. */
  vramBytes: number;
  /** Shares system RAM (integrated GPU, Apple Silicon). */
  unified: boolean;
  /** System memory the GPU may map (amdgpu GTT), where readable. */
  gttBytes?: number;
  driver?: string;
  pciId?: string;
}

export type Backend = "cpu" | "cuda" | "rocm" | "vulkan" | "metal";

export interface Fingerprint {
  os: "linux" | "macos" | "windows" | "other";
  osVersion: string;
  kernel: string;
  arch: "x64" | "arm64" | "other";
  cpu: { model: string; cores: number; threads: number };
  memoryBytes: number;
  gpus: GpuInfo[];
  /** Compute backends usable on this machine. */
  backends: Backend[];
  /** Memory a GPU backend can use; null without a GPU backend. */
  gpuBudgetBytes: number | null;
  /** Driver and stack versions: rocm, cuda, amdgpu, nvidia, vulkan... */
  drivers: Record<string, string>;
  /** A short hash of the hardware, not of the host: the same machine model collides on purpose. */
  id: string;
}

export interface ProviderRun {
  status: "available" | "skipped";
  reason?: string;
  /** attach: a server the operator runs; spawned: a process the run started; docker: a container. */
  mode?: "attach" | "spawned" | "docker";
  version?: string;
  image?: string;
  builds?: string[];
}

export interface Evidence {
  schemaVersion: number;
  runId: string;
  startedAt: string;
  finishedAt: string;
  hardwareClass: string;
  fingerprint: Fingerprint;
  /** `commit`: what the binary was built from ("unknown" when nobody said); `harnessCommit`: the wind tunnel's. */
  executor: { commit: string; binary: string; version?: string; harnessCommit?: string };
  options: { repeat: number; concurrency: number[]; providers: ProviderId[]; services: Service[]; direct: boolean };
  providers: Partial<Record<ProviderId, ProviderRun>>;
  cells: CellResult[];
}
