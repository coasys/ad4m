// Neighbourhood auto-processor: client-side types for `perspective.addAutoProcessor`
// and the `auto-processor-event` step signals. These mirror the Rust
// `AutoProcessorConfig` / `auto_processor::events::AutoProcessorEvent`
// (serde `camelCase`).

/**
 * Raw form of `Scope` (parent node + linking predicate) — the only variant
 * the WebSocket auto-processor config accepts. `Scope`'s model form
 * (`{ model: typeof Ad4mModel, id, field? }`) references a JS class
 * constructor, which JSON.stringify silently drops, so the executor would
 * receive `{ id }` and the scope would be unresolvable. Client callers who
 * hold a model class should resolve its predicate via
 * `ModelQueryBuilder.buildParentScope()` first and pass the result here.
 */
export type RawScope = { id: string; predicate: string };

/**
 * One step in an auto-processor pass (serde camelCase of the Rust enum).
 *
 * ## Lifecycle (winning peer, debug_mode: true)
 *
 * A pass on the winning executor emits, in order:
 *
 *  1. `batchReady`             — debounced batch reached threshold, pass starting
 *  2. `claimed`                — this executor won the `try_claim`
 *  3. `gatheringTranscript`    — pulling the batch's transcript turns
 *  4. `runningInterpretation`  — about to invoke the LLM
 *  5. `llmRequestSent`         — prompt built + dispatched; carries `llmInput`
 *                                (only when `debug_mode: true`)
 *  6. `llmResponseReceived`    — LLM response arrived; carries `llmOutput`
 *                                (only when `debug_mode: true`) — the UI can
 *                                render "waiting on LLM" between steps 5 and 6
 *  7. `processed`              — pass complete; carries `bases[]` (new/updated
 *                                instance URIs)
 *
 * ## Lifecycle (losing peer, or short-circuit)
 *
 * - `batchReady` → `backedOff`       — another peer claimed the batch first
 * - `batchReady` → `awaitingAuthor`  — no author of the batch is online
 * - `batchReady` → `notCandidate`    — this peer stood down for an earlier author
 * - `claimed` → `shapesMissing`      — a configured class shape hasn't synced
 * - `claimed` → `emptyTranscript`    — the batch drained empty
 *
 * ## Payload fields per step
 *
 * All steps carry `perspectiveUuid`, `processorId`, `agentDid`, `itemIds[]`.
 * Additional payload:
 *
 * - `backedOff`, `notCandidate`         → `detail` = holder / elected-author DID
 * - `shapesMissing`                     → `detail` = comma-joined missing class URIs
 * - `llmRequestSent`                    → `llmInput` = raw prompt
 * - `llmResponseReceived`               → `llmOutput` = raw LLM response
 * - `processed`                         → `bases[]` = new/updated instance URIs
 */
export type AutoProcessorStep =
  | "batchReady"
  | "claimed"
  | "backedOff"
  | "awaitingAuthor"
  | "notCandidate"
  | "gatheringTranscript"
  | "runningInterpretation"
  | "llmRequestSent"
  | "llmResponseReceived"
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
  /** Raw LLM prompt this pass fed the model. Present ONLY on
   *  `llmRequestSent` events, and only when the processor was configured
   *  with `debugMode: true`. Never carried on `processed`. */
  llmInput?: string;
  /** Raw LLM response this pass received. Present ONLY on
   *  `llmResponseReceived` events, and only when the processor was
   *  configured with `debugMode: true`. Never carried on `processed`. */
  llmOutput?: string;
}

/**
 * Coarse-grained phase of a neighbourhood-state event.
 *
 * `claimed` — this executor just started a pass.
 * `finished` — the pass completed successfully.
 * `abandoned` — the pass short-circuited (missing shape / empty batch /
 *   error); the claim will TTL-expire.
 */
export type NeighbourhoodPhase = "claimed" | "finished" | "abandoned";

/**
 * `auto-processor-neighbourhood-state` — perspective-scoped observability
 * event. Fires when THIS executor claims, finishes, or abandons a batch.
 * Anyone with perspective read access sees it, so a UI can render "someone
 * is auto-processing this" without receiving the batch payload or LLM I/O.
 *
 * Cross-executor visibility (peer's claim reaching us via Holochain sync) is
 * NOT covered here; consumers who need that subscribe to `link-added` and
 * filter for the `has_claim` predicate on the shared perspective.
 */
export interface AutoProcessorNeighbourhoodStateEvent {
  type: "auto-processor-neighbourhood-state";
  perspectiveUuid: string;
  processorId: string;
  /** DID that claimed the batch — the pass owner's DID. */
  claimantDid: string;
  /** SHA-256 hex of the batch's item-id set — merge `claimed` + `finished`
   *  for the same key to render a single row in a UI. */
  batchKey: string;
  phase: NeighbourhoodPhase;
}

/** Configuration for `perspective.addAutoProcessor` (everything but `uuid`). */
export interface AddAutoProcessorConfig {
  /** Human-meaningful processor id (unique per perspective). */
  processorId: string;
  /**
   * SPARQL `SELECT ?speaker ?text ?timestamp` over the source items to
   * interpret. All three bindings are required: `?timestamp` is what makes a
   * turn identifiable, so the processed-turn cursor can tell a re-gathered
   * turn from the same wording said again later. Read the body link's reifier
   * for author and timestamp rather than an app-level `ns://author`:
   *
   * ```sparql
   * SELECT ?speaker ?text ?timestamp WHERE {
   *   ?m <ns://body> ?text .
   *   ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
   *   ?r <ad4m://ontology/author> ?speaker .
   *   ?r <ad4m://ontology/timestamp> ?timestamp .
   * }
   * ORDER BY ?timestamp
   * ```
   *
   * Swap `ns://body` for your own body predicate. A query binding only
   * speaker+text fails the gather.
   */
  sourceScopeQuery: string;
  /**
   * URI namespace new interpreted instances are minted under (the "spawn
   * scope"), e.g. `soa://project/42/`. Omit for a per-processor default
   * (`ad4m://autoprocessor/<processorId>/instance/`). Point this and
   * `sourceScopeQuery` at the same subtree to confine a processor to it.
   */
  basePrefix?: string;
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
  /** Optional serialized `DedupStrategy` JSON (else NormalizedString). */
  dedupStrategyJson?: string;
  /**
   * How far back (ms) each pass looks: turns older than `now - window` are
   * dropped, and the processed-turn cursor only counts runs that finished
   * inside the same window. Omit for no window — every gathered turn is a
   * candidate and the cursor is the unbounded union of this processor's past
   * runs.
   */
  sourceWindowMs?: number;
  /**
   * Optional parent-scope filter for the dedup lookup: when set, only
   * existing instances of the interpretation classes that live under this
   * scope are candidates for upsert. Omit for the whole-perspective dedup
   * set (pre-scope behaviour). Raw form only — see `RawScope`.
   */
  existingScope?: RawScope;
  /**
   * Optional parent-scope target for newly minted instances: when set,
   * every base URI the pass CREATES is additionally linked as a child of
   * `mintScope.id` via the scope's predicate — turning the SoA-tree
   * "children live under this node" pattern from a URI-prefix convention
   * into an actual graph edge. Upserts of pre-existing instances are NOT
   * linked (would multi-parent unrelated graph state). Raw form only —
   * see `RawScope`; the Rust watcher rejects `Model` scopes at runtime.
   */
  mintScope?: RawScope;
  /**
   * Live debug knob for UI observability. When `true`, each pass enriches
   * the `processed` `auto-processor-event` with the raw LLM prompt +
   * response, and persists the same strings on the pass's
   * `InterpretationRun` (`debugPrompt` / `debugResponse`) so a UI can look
   * them up post-hoc even if it missed the live event. Default `false` —
   * LLM I/O is large (tens of KB) and would otherwise inflate every event
   * and every shared-graph sync.
   */
  debugMode?: boolean;
}

/**
 * Raw REST-API response shape for a pending interpretation overlay.
 * This is the plain-data DTO returned by `PerspectiveProxy.interpretationOverlays()`.
 * For perspective-graph querying, use the `@Model` class `InterpretationOverlay`
 * from `@coasys/ad4m` (which supports `findAll`, `findOne`, etc.).
 */
export interface InterpretationOverlayInfo {
  /** The base instance the overlay sits on. */
  base: string;
  /** Whether the LLM authored the whole instance (`"create"`) or proposed
   *  changes to an existing one (`"update"`). */
  kind: "create" | "update";
  /** The `InterpretationRun` that last wrote it, if present. */
  run: string | null;
  /** `[realPredicate, stagedValue]` pairs — the model's proposed values. */
  inferred: [string, any][];
}
