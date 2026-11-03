import type { Manifest, MatrixPlan } from "../data.js";
import type { CellModel, EndpointKindId, Fingerprint, ProviderId, Service } from "../types.js";

export interface RunContext {
  runId: string;
  /** Logs and scratch files of this run. */
  runDir: string;
  fp: Fingerprint;
  manifest: Manifest;
  plan: MatrixPlan;
  /** Only attach to servers named by AI_WT_*_URL; start nothing. */
  attach: boolean;
  /** `--llama-builds`: manifest build keys or backends to run llama-server on. */
  llamaBuilds?: string[];
  log(line: string): void;
}

/** One way a provider runs here: a runtime build and the backend it computes on. */
export interface Variant {
  runtimeBuild?: string;
  backend?: string;
  label: string;
}

/** A model being served. */
export interface Served {
  /** Server root, without `/v1`: what the executor's endpoint gets. */
  baseUrl: string;
  /** The id the server knows the model by. */
  remoteModel: string;
  /** Root processes whose trees hold the model, for memory sampling. */
  pids(): number[];
  /** Start to ready, when the run started the server for this model (includes the model load). */
  readyMs?: number;
  /** The backend the server reports using, when it says. */
  backend?(): string | undefined;
  /** Server flags that shape serving (context, offload, thinking), without paths or ports. */
  flags?: string[];
  release(): Promise<void>;
}

export interface Session {
  kind: EndpointKindId;
  mode: "attach" | "spawned" | "docker";
  version?: string;
  image?: string;
  /** Ollama: models arrive through the executor's `ai.pullModel`. */
  pullsThroughExecutor?: boolean;
  serve(model: CellModel, service: Service): Promise<Served>;
  close(): Promise<void>;
}

export type PlanResult = { variants: Variant[] } | { skip: string };

export interface Driver {
  id: ProviderId;
  plan(ctx: RunContext): Promise<PlanResult>;
  open(ctx: RunContext, variant: Variant): Promise<Session>;
}

/** The provider is not usable here after all (a GPU stack that does not run): its cells are skipped, not failed. */
export class SkipError extends Error {}

export function envUrl(name: string): string | undefined {
  const v = process.env[name];
  return v ? v.replace(/\/+$/, "").replace(/\/v1$/, "") : undefined;
}
