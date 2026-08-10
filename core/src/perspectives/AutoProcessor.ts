// Neighbourhood auto-processor: client-side types for `perspective.addAutoProcessor`
// and the `auto-processor-event` step signals. These mirror the Rust
// `AutoProcessorConfig` / `auto_processor::events::AutoProcessorEvent`
// (serde `camelCase`).

/** One step in an auto-processor pass (serde camelCase of the Rust enum). */
export type AutoProcessorStep =
  | "batchReady"
  | "claimed"
  | "backedOff"
  | "awaitingAuthor"
  | "notCandidate"
  | "gatheringTranscript"
  | "runningInterpretation"
  | "processed"
  | "shapesMissing"
  | "emptyTranscript";

/** A single step-signal from one auto-processor pass on one perspective. */
export interface AutoProcessorEvent {
  type: "auto-processor-event";
  perspectiveUuid: string;
  processorId: string;
  /** DID of the agent that ran the pass (which peer claimed/processed/backed off). */
  agentDid?: string;
  step: AutoProcessorStep;
  /** The batch's source item ids (present from `batchReady` onward). */
  itemIds: string[];
  /** Instance base URIs written by the pass (present on `processed`). */
  bases: string[];
  /** Free-form context for the step (a holder/elected DID, an error, …). */
  detail?: string;
}

/** Configuration for `perspective.addAutoProcessor` (everything but `uuid`). */
export interface AddAutoProcessorConfig {
  /** Human-meaningful processor id (unique per perspective). */
  processorId: string;
  /** SPARQL `SELECT ?speaker ?text` over the source items to interpret. */
  sourceScopeQuery: string;
  /** Class URIs (SHACL `target_class`) to materialize each pass. */
  interpretationClasses: string[];
  /** Quiet-window (ms) after the last new item before a pass runs. */
  debounceMs: number;
  /** Minimum items before a pass runs (Flux "wait for N inputs"). Default 1. */
  batchMin?: number;
  /** Cap on items per pass. */
  batchMax: number;
  /** Safety flush (ms) for a sub-`batchMin` batch; omit to wait indefinitely. */
  maxWaitMs?: number;
  /** How long a won claim is authoritative before peers may re-claim (ms). */
  claimTtlMs: number;
  /** Optional LLM model tag override (else the executor's default). */
  llmModel?: string;
  /** Optional serialized `DedupStrategy` JSON (else NormalizedString). */
  dedupStrategyJson?: string;
}
