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
import { HasMany, Optional, Property } from "../model/decorators";
import { Model } from "../model/decorators";

// ── AutoProcessorConfig ─────────────────────────────────────────────────────
// Mirrors the Rust `auto_processor::config::AutoProcessorConfig` SDNA.
// One node per registered processor; the node URI is
// `ad4m://autoprocessor/<processorId>`.

// Model name = "AutoProcessor" (NOT "AutoProcessorConfig") to match the Rust
// hardwired subject class in `perspectives/auto_processor/config.rs` — that
// alignment is what the SDNA parity test in
// `tests/js/tests/model/interpretation-models.test.ts` guards. The TS class
// symbol keeps the `Config` suffix for readability (`AutoProcessorConfig` reads
// better than a bare `AutoProcessor` for an "instances of the config record"
// query), but the on-graph subject class it registers is `AutoProcessor`.
@Model({ name: "AutoProcessor" })
export class AutoProcessorConfig extends Ad4mModel {
  /** Unique processor id — the dedup key for auto-processor instances. */
  // First-declared property: `buildSHACL` derives the shape namespace from
  // its `through` prefix, so `ad4m://processor_id` → `ad4m://` → target_class
  // `ad4m://AutoProcessor` (matches Rust `AUTO_PROCESSOR_TARGET_CLASS`).
  @Property({ through: "ad4m://processor_id", required: true, identity: true })
  processorId: string = "";

  /** SPARQL SELECT that gathers source items each pass. */
  @Property({ through: "ad4m://source_scope_query", required: true })
  sourceScopeQuery: string = "";

  /** URI prefix for minted instances; omit for the per-processor default. */
  // No resolveLanguage — Rust SDNA declares none for this property, so under
  // PR #874 both sides get the fast deterministic typed-literal path.
  // (resolveLanguage: "literal" would ask the executor for a signed envelope
  // that the Rust writer never produces for config metadata.)
  @Optional({ through: "ad4m://base_prefix" })
  basePrefix?: string;

  /** Class URIs (SHACL targetClass) to materialise each pass. */
  @HasMany({ through: "ad4m://interpretation_class", datatype: "xsd:string" })
  interpretationClasses: string[] = [];

  /** Quiet-window (ms) after the last new item before a pass runs. */
  @Property({ through: "ad4m://debounce_ms", required: true })
  debounceMs: string = "200";

  /** Minimum batch size before a pass runs (default 1). */
  @Optional({ through: "ad4m://batch_min" })
  batchMin?: string;

  /** Maximum items per pass. */
  @Property({ through: "ad4m://batch_max", required: true })
  batchMax: string = "32";

  /** Safety flush (ms) for a sub-`batchMin` batch; absent = wait indefinitely. */
  @Optional({ through: "ad4m://max_wait_ms" })
  maxWaitMs?: string;

  /** How long a won claim is authoritative before peers may re-claim (ms). */
  @Property({ through: "ad4m://claim_ttl_ms", required: true })
  claimTtlMs: string = "60000";

  /** How far back (ms) each pass looks; absent = unbounded. */
  @Optional({ through: "ad4m://source_window_ms" })
  sourceWindowMs?: string;

  /** Serialised `DedupStrategy` JSON (absent = NormalizedString). */
  @Optional({ through: "ad4m://dedup_strategy" })
  dedupStrategy?: string;

  /**
   * Serialised `Scope` JSON (absent = whole-perspective dedup set).
   * Constrains the existing-instance lookup to the subtree rooted at
   * `Scope.id` linked via `Scope.predicate` — the SoA-tree "existing items
   * live under THIS project node" pattern. Stored as an opaque JSON blob
   * because SHACL properties carry no rich-object type-check; the Rust
   * watcher parses it back into `Scope`.
   */
  @Optional({ through: "ad4m://existing_scope" })
  existingScope?: string;

  /**
   * Serialised `Scope` JSON (absent = mint sites are unlinked). When set,
   * every freshly created base URI gets an additional `Scope.id
   * --Scope.predicate--> new-uri` link written into the perspective, so
   * mints become first-class children of the target scope. Must be the
   * `Raw` form of `Scope`; `Model` scopes carry no linking predicate.
   */
  @Optional({ through: "ad4m://mint_scope" })
  mintScope?: string;

  /**
   * Live debug knob (`"true"` / `"false"` string, per SDNA literal-string
   * encoding). When on, the pass enriches its `processed` event with the
   * raw LLM prompt + response AND persists the same strings on the pass's
   * `InterpretationRun`. Absent = default `"false"` (no debug telemetry).
   */
  @Optional({ through: "ad4m://debug_mode", resolveLanguage: "literal" })
  debugMode?: string;

  // No `@Flag` type discriminator (Nico 2026-08-19: "type flags are an
  // anti-pattern for subject classes; match over all the properties
  // instead"). Conformance is by the presence of `processorId` +
  // `sourceScopeQuery` + `interpretationClasses` + `debounceMs` +
  // `batchMax` + `claimTtlMs` — the Rust SDNA takes the same shape.
}

// ── InterpretationRun ───────────────────────────────────────────────────────
// Mirrors the Rust `interpretation::overlay::InterpretationRun` SDNA.
// One node per completed pass; the node URI is `ad4m://interp/run/<runId>`.

@Model({ name: "InterpretationRun" })
export class InterpretationRun extends Ad4mModel {
  // No `@Flag` type discriminator — Rust SDNA drops `ad4m://type` too.
  // Conformance is by `runId` (identity) — same pattern
  // `InterpretationOverlay` already uses (`kind` is its discriminator).

  /** UUID for this run — the dedup key across run nodes. */
  @Property({ through: "ad4m://interp/run_id", required: true, identity: true })
  runId: string = "";

  /** LLM model id used for this run. */
  @Optional({ through: "ad4m://interp/model" })
  model?: string;

  /** SHA-256 hex of the system prompt + few-shots at the time of the run. */
  @Optional({ through: "ad4m://interp/prompt_version" })
  promptVersion?: string;

  /** RFC3339 timestamp when the run completed. */
  @Optional({ through: "ad4m://interp/ran_at" })
  ranAt?: string;

  /** URI of the AutoProcessorConfig node that triggered this run. */
  @Optional({ through: "ad4m://interp/processor" })
  processor?: string;

  /**
   * Source item IDs consumed by this pass.
   * Stored as multiple links with the same predicate; the processed-turn
   * cursor uses these to avoid re-interpreting a turn. Values are plain
   * strings (turn hex), so the property opts into `sh:datatype`-gated
   * literal decoding on hydration.
   */
  @HasMany({ through: "ad4m://interp/sources", datatype: "xsd:string" })
  sources: string[] = [];

  /** Raw LLM prompt this pass fed the model. Present only when the pass's
   *  AutoProcessor had `debugMode: true`. Absent by default. */
  @Optional({ through: "ad4m://interp/debug_prompt", resolveLanguage: "literal" })
  debugPrompt?: string;

  /** Raw LLM response this pass received. Same rules as `debugPrompt`. */
  @Optional({ through: "ad4m://interp/debug_response", resolveLanguage: "literal" })
  debugResponse?: string;
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
  @Property({ through: "ad4m://interp/kind", required: true })
  kind: string = "";

  /** URI of the InterpretationRun that last wrote this overlay, if present. */
  @Optional({ through: "ad4m://interp/run" })
  run?: string;
}
