/**
 * TypeScript @Model classes mirroring the Rust-side SDNA for the generic
 * interpretation subsystem.  Apps can import these to query the perspective
 * graph for interpretation history and pending overlay proposals without
 * knowing the raw link predicates.
 *
 * Registration:
 *   await InterpretationOverlay.register(perspective);
 *   await InterpretationRun.register(perspective);
 *   await AutoProcessorConfig.register(perspective);
 *
 * Querying:
 *   const pending = await InterpretationOverlay.findAll(perspective);
 *   const runs    = await InterpretationRun.findAll(perspective);
 *   const configs = await AutoProcessorConfig.findAll(perspective);
 */

import { Ad4mModel } from "../model/Ad4mModel";
import { Flag, HasMany, Optional, Property } from "../model/decorators";
import { Model } from "../model/decorators";

// ── AutoProcessorConfig ─────────────────────────────────────────────────────
// Mirrors the Rust `auto_processor::config::AutoProcessorConfig` SDNA.
// One node per registered processor; the node URI is
// `ad4m://autoprocessor/<processorId>`.

@Model({ name: "AutoProcessorConfig" })
export class AutoProcessorConfig extends Ad4mModel {
  @Flag({ through: "rdf://type", value: "ad4m://AutoProcessor" })
  type: string = "ad4m://AutoProcessor";

  /** Unique processor id — the dedup key for auto-processor instances. */
  @Property({ through: "ad4m://processor_id", required: true, identity: true })
  processorId: string = "";

  /** SPARQL SELECT that gathers source items each pass. */
  @Property({ through: "ad4m://source_scope_query", required: true })
  sourceScopeQuery: string = "";

  /** URI prefix for minted instances; omit for the per-processor default. */
  @Optional({ through: "ad4m://base_prefix", resolveLanguage: "literal" })
  basePrefix?: string;

  /** Class URIs (SHACL targetClass) to materialise each pass. */
  @HasMany({ through: "ad4m://interpretation_class" })
  interpretationClasses: string[] = [];

  /** Quiet-window (ms) after the last new item before a pass runs. */
  @Property({ through: "ad4m://debounce_ms", required: true })
  debounceMs: string = "200";

  /** Minimum batch size before a pass runs (default 1). */
  @Optional({ through: "ad4m://batch_min", resolveLanguage: "literal" })
  batchMin?: string;

  /** Maximum items per pass. */
  @Property({ through: "ad4m://batch_max", required: true })
  batchMax: string = "32";

  /** Safety flush (ms) for a sub-`batchMin` batch; absent = wait indefinitely. */
  @Optional({ through: "ad4m://max_wait_ms", resolveLanguage: "literal" })
  maxWaitMs?: string;

  /** How long a won claim is authoritative before peers may re-claim (ms). */
  @Property({ through: "ad4m://claim_ttl_ms", required: true })
  claimTtlMs: string = "60000";

  /** How far back (ms) each pass looks; absent = unbounded. */
  @Optional({ through: "ad4m://source_window_ms", resolveLanguage: "literal" })
  sourceWindowMs?: string;

  /** Serialised `DedupStrategy` JSON (absent = NormalizedString). */
  @Optional({ through: "ad4m://dedup_strategy", resolveLanguage: "literal" })
  dedupStrategy?: string;
}

// ── InterpretationRun ───────────────────────────────────────────────────────
// Mirrors the Rust `interpretation::overlay::InterpretationRun` SDNA.
// One node per completed pass; the node URI is `ad4m://interp/run/<runId>`.

@Model({ name: "InterpretationRun" })
export class InterpretationRun extends Ad4mModel {
  @Flag({ through: "ad4m://type", value: "ad4m://interpretation-run" })
  type: string = "ad4m://interpretation-run";

  /** UUID for this run — the dedup key across run nodes. */
  @Property({ through: "ad4m://interp/run_id", required: true, identity: true })
  runId: string = "";

  /** LLM model id used for this run. */
  @Optional({ through: "ad4m://interp/model", resolveLanguage: "literal" })
  model?: string;

  /** SHA-256 hex of the system prompt + few-shots at the time of the run. */
  @Optional({ through: "ad4m://interp/prompt_version", resolveLanguage: "literal" })
  promptVersion?: string;

  /** RFC3339 timestamp when the run completed. */
  @Optional({ through: "ad4m://interp/ran_at", resolveLanguage: "literal" })
  ranAt?: string;

  /** URI of the AutoProcessorConfig node that triggered this run. */
  @Optional({ through: "ad4m://interp/processor", resolveLanguage: "literal" })
  processor?: string;

  /**
   * Source item IDs consumed by this pass.
   * Stored as multiple links with the same predicate; the processed-turn
   * cursor uses these to avoid re-interpreting a turn.
   */
  @HasMany({ through: "ad4m://interp/sources" })
  sources: string[] = [];
}

// ── InterpretationOverlay ───────────────────────────────────────────────────
// Mirrors the Rust `interpretation::overlay::InterpretationOverlay` SDNA.
// An overlay sits on the *same* base URI as the proposed instance — it is a
// pending LLM suggestion awaiting human accept/reject.
//
// The dynamic "inferred" values are stored as links with predicates of the
// form `ad4m://interp/inferred/<realPredicate>` and are NOT captured here as
// fixed @Property fields because the predicates vary per class.  Read them
// directly via perspective.get({ source: overlay.baseExpression }).
//
// Accept  → perspective.acceptInterpretationOverlay(base)   [deletes the overlay]
// Reject  → perspective.rejectInterpretationOverlay(base)   [drops the suggestion]
//
// Note: `findAll` finds ALL nodes that currently carry a `kind` link, i.e.
// all pending proposals.  Accepted/rejected overlays are removed from the
// graph and will not appear.

@Model({ name: "InterpretationOverlay" })
export class InterpretationOverlay extends Ad4mModel {
  /**
   * Whether the LLM authored the whole instance (`"create"`) or proposed
   * changes to an existing one (`"update"`).  This is the discriminator link
   * that makes a node identifiable as an overlay.
   */
  @Property({ through: "ad4m://interp/kind", required: true, resolveLanguage: "literal" })
  kind: string = "";

  /** URI of the InterpretationRun that last wrote this overlay, if present. */
  @Optional({ through: "ad4m://interp/run", resolveLanguage: "literal" })
  run?: string;
}
