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
 * ## Lifecycle (winning peer, emitDebugEvents: true)
 *
 * A pass on the winning executor emits, in order:
 *
 *  1. `batchReady`             — debounced batch reached threshold, pass starting
 *  2. `claimed`                — this executor won the `try_claim`
 *  3. `gatheringTranscript`    — pulling the batch's transcript turns
 *  4. `runningInterpretation`  — about to invoke the LLM
 *  5. `llmRequestSent`         — prompt built + dispatched; carries `llmInput`
 *                                (only when `emitDebugEvents: true`)
 *  6. `llmResponseReceived`    — LLM response arrived; carries `llmOutput`
 *                                (only when `emitDebugEvents: true`) — the UI
 *                                can render "waiting on LLM" between steps 5-6
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
 * - any → `failed`                   — the model errored or timed out;
 *   `detail` carries why. Distinct from the two above, which are the pass
 *   correctly finding nothing to do — reporting "nothing to extract" when
 *   the LLM endpoint was unreachable points someone at their transcript
 *   instead of at their model settings.
 *
 * ## Lifecycle (one-shot `runInterpretation`)
 *
 * A one-shot pass emits nothing unless the caller passes `observe` (see
 * {@link PerspectiveProxy.runInterpretation}). With it:
 *
 * `runningInterpretation` → [`llmRequestSent` → `llmResponseReceived`] →
 * `processed` | `failed`
 *
 * — plus `claimed` → `finished`/`abandoned` on the neighbourhood stream, so
 * one-shot and watch passes render through the same consumer code. There is
 * no `batchReady`, `gatheringTranscript` or claim: the caller supplied the
 * transcript, and `processorId`/`batchKey` both carry the caller's own
 * `observationId`.
 *
 * ## Payload fields per step
 *
 * All steps carry `perspectiveUuid`, `processorId`, `agentDid`, `itemIds[]`
 * and `batchKey` (the join key to the neighbourhood-state stream).
 * Additional payload:
 *
 * - `backedOff`, `notCandidate`         → `detail` = holder / elected-author DID
 * - `shapesMissing`                     → `detail` = comma-joined missing class URIs
 * - `failed`                            → `detail` = the error
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
  | "emptyTranscript"
  | "failed";

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
  /**
   * Content hash of the batch — the same value
   * {@link AutoProcessorNeighbourhoodStateEvent.batchKey} carries, and the
   * key that joins the two streams.
   *
   * A UI that renders one row per pass subscribes to both: the
   * perspective-scoped neighbourhood stream opens the row (and names the
   * claimant), while this DID-scoped stream fills in the fine-grained
   * steps and LLM I/O for the pass this agent is running. Matching them on
   * `processorId` alone breaks as soon as a processor runs a second pass;
   * matching on `itemIds` means re-implementing the Rust SHA-256 and its
   * exact serialization. So both streams carry the same key.
   *
   * Present from `batchReady` onward. Optional on the type because a
   * pre-#903 executor does not send it — treat its absence as "cannot
   * correlate", not as an error.
   */
  batchKey?: string;
  /** Instance base URIs written by the pass (present on `processed`). */
  bases: string[];
  /** Free-form context for the step (a holder/elected DID, an error, …). */
  detail?: string;
  /** Raw LLM prompt this pass fed the model. Present ONLY on
   *  `llmRequestSent` events, and only when the processor was configured
   *  with `emitDebugEvents: true`. Never carried on `processed`. */
  llmInput?: string;
  /** Raw LLM response this pass received. Present ONLY on
   *  `llmResponseReceived` events, and only when the processor was
   *  configured with `emitDebugEvents: true`. Never carried on `processed`. */
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

/**
 * Opt a one-shot `runInterpretation` call into the same event streams a
 * standing watch produces.
 *
 * Without it the call is silent until it resolves — which, on a local model,
 * is minutes of a UI having nothing to say. With it the pass reports
 * `runningInterpretation` → `processed`/`failed` on the DID-scoped stream and
 * `claimed` → `finished`/`abandoned` on the perspective-scoped one.
 */
export interface RunInterpretationObserveOptions {
  /**
   * The caller's own id for this pass, echoed back as both `processorId` and
   * `batchKey` on every event it emits.
   *
   * Supplied by the caller rather than minted by the executor because
   * `runInterpretation` is a single blocking call: there is no earlier
   * response that could hand back a server-side id, so the events would
   * arrive with nothing to match them against. Any value unique among this
   * client's in-flight passes will do.
   */
  observationId: string;
  /**
   * Also emit `llmRequestSent` / `llmResponseReceived` with the raw prompt
   * and response. Default `false`. These never persist on a one-shot pass —
   * there is no `AutoProcessorConfig` to carry a `persistDebug` opt-in, and
   * writing tens of KB of prompt into the shared graph on every button press
   * is not a default worth having.
   */
  emitDebugEvents?: boolean;
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
   * Persist the raw LLM prompt + response on the pass's `InterpretationRun`
   * (`debugPrompt` / `debugResponse`) so a UI can look them up post-hoc.
   * Independent of `emitDebugEvents` — Nico's PR #903 split so a caller
   * can persist without emitting mid-pass events (retrospective inspection
   * only) or emit without persisting (live observability, no graph-sync
   * payload). Default `false` — LLM I/O is 10s of KB per pass and every
   * enabled peer syncs it.
   */
  persistDebug?: boolean;
  /**
   * Emit `LlmRequestSent` and `LlmResponseReceived` `auto-processor-event`s
   * mid-pass, so a subscribed UI can render "waiting on LLM" between
   * prompt-send and response-receive. See `persistDebug` for the split
   * rationale. Default `false`.
   */
  emitDebugEvents?: boolean;
  /**
   * Legacy backwards-compat alias — pre-split callers set `debugMode` as a
   * single coupled flag. When present and both `persistDebug` +
   * `emitDebugEvents` are absent, its value is expanded to both. The two
   * specific fields take precedence when present. Prefer the split fields;
   * this alias is retained so pre-split clients still work.
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
