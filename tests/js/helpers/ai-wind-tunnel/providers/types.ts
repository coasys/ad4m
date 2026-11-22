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
  /** The endpoint kind the executor registers, when it differs from the session's (an audio front on llama-server). */
  kind?: EndpointKindId;
  /** What stands between the executor and the server, when something does (an OpenAI-shaped front). */
  adapter?: string;
  /**
   * The server still answers for this model after `release`, until the session closes (an
   * attached server, whisper-server, speaches). Only such a model can be the STT reference
   * that a later TTS cell's round trip transcribes with.
   */
  staysUp?: boolean;
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

/** Whether a passed STT cell's model can transcribe for later TTS cells: every check passed and its server stays up. */
export function canBeSttReference(served: Pick<Served, "staysUp">, checks: { pass: boolean }[]): boolean {
  return !!served.staysUp && checks.length > 0 && checks.every((k) => k.pass);
}

/**
 * An attached server serves the model it was started with, whatever a request names. A cell
 * for model `model` runs on it only when the server is known to serve that model: one of
 * `listed` (its `/v1/models` ids) or the id in `envName` (a manifest modelId or the
 * provider's model name). Any other planned model is skipped, so evidence never names a
 * model that was not measured.
 */
export function assertAttachedModel(model: CellModel, envName: string, listed: string[] = []): void {
  const named = process.env[envName];
  const ids = [...new Set([model.modelId, model.remoteModel].filter((x): x is string => !!x))];
  if (ids.some((id) => id === named || listed.includes(id))) return;
  const serves = [named, ...listed].filter(Boolean).join(", ") || "an unknown model";
  throw new SkipError(`the attached server serves ${serves}, not ${ids.join(" / ")}; set ${envName} to the model it was started with`);
}

export function envUrl(name: string): string | undefined {
  const v = process.env[name];
  return v ? v.replace(/\/+$/, "").replace(/\/v1$/, "") : undefined;
}
