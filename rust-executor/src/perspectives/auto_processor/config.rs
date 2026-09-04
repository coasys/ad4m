//! `AutoProcessor` configuration — the "which processor runs on this
//! perspective, over what content, with what LLM" record.
//!
//! Phase **P-B1** of the AutoProcessor arc. Config primitive only — the
//! executor watcher (P-B2) reads these back from a perspective, then debounces
//! new content in `source_scope_query`, wins a claim via [`super::claim`], and
//! calls `run_interpretation_with_strategy` for the configured
//! `interpretation_classes`.
//!
//! A processor is a **hard-wired SHACL subject class** ([`AUTO_PROCESSOR_SDNA`],
//! registered on first write by [`ensure_auto_processor_class`]), written with
//! `create_subject` and read back with `model_query`. So app UIs can list,
//! inspect and edit processors through the ordinary model API instead of
//! reverse-engineering a private link shape. Instances live in the shared
//! perspective (the setters below carry no `local` flag, so their links are
//! `Shared`), which means every peer in the neighbourhood sees the same
//! processor set once state syncs, and the election in
//! [`super::claim::try_claim`] is over the same batches on every peer.
//!
//! `dedup_strategy_json` is intentionally stored as an opaque JSON blob rather
//! than a typed enum: the watcher (P-B2) will deserialize it into
//! [`crate::perspectives::interpretation::DedupStrategy`]. This keeps P-B1 free
//! of the dedup module's shape (which does not implement `Serialize`).
//!
//! Numeric scalars are stored as strings (`literal:string:5000`) and parsed on
//! read: SHACL properties carry no int type-check, so a hand-edited or
//! partially-synced value has to be validated here anyway — see
//! [`config_from_instance`].

use crate::agent::AgentContext;
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};

use super::scalar_string;
use crate::perspectives::hardwired_class::{ensure_subject_class, subject_class_registered};

/// Instances already reported as unloadable, so each is complained about once.
///
/// The watch loop polls `load_processors` several times a second on every perspective, and a
/// rejected instance is rejected identically every time — so an unthrottled warning emits the same
/// line thousands of times a minute, per instance. That is not merely untidy: it buries the log a
/// person is reading to find out *why*, which is the exact situation the warning exists to serve.
///
/// Keyed by instance URI and never cleared. A processor whose config is repaired starts loading and
/// stops reaching this path at all; one that is deleted never returns. The set is therefore bounded
/// by the number of distinct broken instances, which is small and finite.
static REPORTED_BAD_INSTANCES: std::sync::OnceLock<
    std::sync::Mutex<std::collections::HashSet<String>>,
> = std::sync::OnceLock::new();

/// Whether this is the first time this instance has failed to load.
fn first_report_for(instance_id: &str) -> bool {
    REPORTED_BAD_INSTANCES
        .get_or_init(Default::default)
        .lock()
        .map(|mut seen| seen.insert(instance_id.to_string()))
        // A poisoned mutex means another thread panicked mid-report. Log rather than swallow: a
        // repeated line is a smaller problem than a silent one.
        .unwrap_or(true)
}

/// Local subject-class name of a processor config.
pub(crate) const AUTO_PROCESSOR_CLASS: &str = "AutoProcessor";
/// Target-class URI of [`AUTO_PROCESSOR_CLASS`] — used to detect prior
/// registration.
const AUTO_PROCESSOR_TARGET_CLASS: &str = "ad4m://AutoProcessor";

/// Hard-wired SDNA for the [`AUTO_PROCESSOR_CLASS`] subject class.
///
/// No dedicated `rdf://type` flag by design (Nico 2026-08-19: "type flags
/// are an anti-pattern for subject classes; match over all the properties
/// instead"). Conformance is entirely by the presence of the required
/// scalars: a node with `processor_id` + `source_scope_query` +
/// `interpretation_class` + `debounce_ms` + `batch_max` + `claim_ttl_ms`
/// IS an AutoProcessor. A half-synced node missing any of them is simply
/// not an instance yet. Same pattern the `InterpretationOverlay` class
/// already uses (`kind` is its discriminator).
///
/// `interpretation_class` is declared `collection` — the marker the shape
/// reader keys on for a multi-valued *literal* property — so its `addLink`
/// setter accumulates class URIs instead of replacing them, and hydration
/// returns them as an array. No `maxCount` is declared on it: the number of
/// classes one processor materializes is open-ended.
// Loaded from a JSON file so the TS parity test can read the SAME file and
// build its expectations from IT — no hand-maintained parallel reference in
// either language (2026-08-20 debug: hardcoded references in both tests
// missed the property-name divergence entirely).
const AUTO_PROCESSOR_SDNA: &str = include_str!("../hardwired_sdna/auto_processor.json");

/// Everything the executor watcher (P-B2) needs to schedule and run a single
/// auto-processor pass over a source perspective.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AutoProcessorConfig {
    /// Human-meaningful processor name. Also part of the batch-node URI in
    /// [`super::claim::batch_node`], so claims by different processors never
    /// collide even on the same id-set.
    pub processor_id: String,
    /// SPARQL `SELECT` returning `?speaker` + `?text` + `?timestamp` bindings —
    /// the same shape
    /// [`crate::perspectives::interpretation::graph::gather_transcript_sparql`]
    /// accepts. Defines "the content this processor watches". Copy
    /// [`crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY`]
    /// (reifier `ad4m://ontology/author` + `ad4m://ontology/timestamp` on the
    /// body link) rather than selecting speaker+text alone.
    pub source_scope_query: String,
    /// URI namespace new interpreted instances are minted under (the "spawn
    /// scope"), e.g. `soa://project/42/`. `None` falls back to a per-processor
    /// default (`ad4m://autoprocessor/<id>/instance/`), preserving the original
    /// behaviour when a config omits `ad4m://base_prefix`. Existing instances to
    /// upsert into are gathered from the perspective's registered classes; a
    /// caller that wants create+update confined to one subtree points both this
    /// and `source_scope_query` at that subtree.
    pub base_prefix: Option<String>,
    /// Class URIs (SHACL `target_class`) the interpretation engine is asked
    /// to materialize on each pass. Must contain at least one entry;
    /// [`load_processors`] skips otherwise.
    pub interpretation_classes: Vec<String>,
    /// Canonical flow URIs (`SHACLFlow.flow_uri()`, `{namespace}{name}Flow`)
    /// this processor is flow-aware of. Flow features — prompt rendering,
    /// the proposal pass, and auto-spawn — run ONLY on selected flows
    /// (Nico 2026-09-04: flows are targeted per processor, like the class
    /// selection above). Empty = flow features off for this processor.
    /// Absent on pre-flow configs, so hydration defaults to empty rather
    /// than bailing.
    pub flows: Vec<String>,
    /// After a new source item lands, wait this long with no further arrivals
    /// before running a pass (batches bursts of typing / imports).
    pub debounce_ms: i64,
    /// Minimum number of pending items required before a pass runs — the Flux
    /// "wait for N inputs" threshold. A batch below this size waits for more
    /// arrivals (subject to [`Self::max_wait_ms`]) rather than draining. `1`
    /// (the default when the SDNA omits `ad4m://batch_min`) reproduces the
    /// original "run as soon as the debounce settles" behaviour.
    pub batch_min: usize,
    /// Cap on how many items a single pass may include, so a large paste
    /// doesn't blow the LLM context window.
    pub batch_max: usize,
    /// Safety valve for [`Self::batch_min`]: once the oldest pending item has
    /// waited this long, a sub-threshold batch drains anyway so it is never
    /// orphaned. `None` (the default) = pure Flux parity: wait indefinitely
    /// until `batch_min` items accumulate.
    pub max_wait_ms: Option<i64>,
    /// TTL passed to [`super::claim::try_claim`] — how long a Won claim is
    /// treated as authoritative before other peers may re-claim.
    pub claim_ttl_ms: i64,
    /// Serialized `DedupStrategy` (JSON blob, opaque to this module).
    /// Deserialized by P-B2 into
    /// [`crate::perspectives::interpretation::DedupStrategy`]. `None` = the
    /// default (`NormalizedString`) — preserving existing runner behaviour.
    pub dedup_strategy_json: Option<String>,
    /// How far back the watch tick looks: gathered turns older than
    /// `now - window` are dropped, and the processed-turn cursor only loads
    /// `InterpretationRun.sources` from runs whose `ran_at` is inside the same
    /// window. `None` (omit `ad4m://source_window_ms`) means **no window** —
    /// every gathered turn is a candidate and the cursor is the unbounded
    /// union of this processor's run sources. `<= 0` is invalid and the
    /// processor is not loaded.
    pub source_window_ms: Option<i64>,
    /// Optional parent-scope filter passed to
    /// [`crate::perspectives::interpretation::graph::existing_instance_context`]
    /// when the watcher gathers existing instances for dedup. `None` keeps the
    /// whole-perspective behaviour (all instances of every class are
    /// candidates for upsert). Setting it constrains the dedup lookup to the
    /// subtree rooted at [`Scope::id`] linked via [`Scope::predicate`] — the
    /// SoA-tree pattern of "existing items live under THIS project node."
    pub existing_scope: Option<Scope>,
    /// Optional parent-scope target for newly minted instances. When set, every
    /// new base URI returned by an interpretation pass gets an additional
    /// `Scope::id --predicate--> new-uri` link written into the perspective,
    /// so the minted instance becomes a first-class child of the target scope
    /// (not just a URI-prefix convention). `None` skips the child-link write
    /// and preserves the pre-scope behaviour where mint sites are unlinked.
    /// May differ from [`Self::existing_scope`] — a watcher can read from a
    /// broader subtree than it writes into, or vice-versa.
    pub mint_scope: Option<Scope>,
    /// Interpretation-pass tool-call budget. `None` or `Some(0)` = the
    /// original single-shot LLM path (no tools). `Some(N)` with `N > 0` =
    /// engage the tool-calling harness (see [`crate::ai_service::harness`])
    /// and let the LLM make up to `N` tool calls per pass before being
    /// forced to answer. The cap prevents a stuck or adversarial model
    /// from DoS'ing the extraction pass.
    pub max_tool_calls: Option<u32>,
    /// When `true`, enables full debug observability for this processor:
    /// 1. Persists the raw LLM prompt + response on the pass's
    ///    `InterpretationRun` node (`debugPrompt`/`debugResponse`) for
    ///    retrospective inspection.
    /// 2. Emits `LlmRequestSent` and `LlmResponseReceived`
    ///    [`super::events::AutoProcessorEvent`]s mid-pass, so a subscribed
    ///    UI can render "waiting on LLM" between prompt-send and
    ///    response-receive instead of a single lump payload at `Processed`.
    ///
    /// Default: `false`. Prompts + responses are large (10s of KB) and
    /// every enabled peer syncs them for every pass.
    pub emit_debug_events: bool,
}

/// Deterministic node URI for an AutoProcessor. Deterministic in
/// `processor_id` so every peer addresses the same node once state syncs.
pub fn processor_node(processor_id: &str) -> String {
    format!("ad4m://autoprocessor/{processor_id}")
}

/// Idempotently register the hard-wired [`AUTO_PROCESSOR_CLASS`] subject class.
/// Refreshes the SHACL if an older registration predates the `flows` selection.
pub async fn ensure_auto_processor_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        AUTO_PROCESSOR_CLASS,
        AUTO_PROCESSOR_TARGET_CLASS,
        AUTO_PROCESSOR_SDNA,
        Some("ad4m://flow"),
        context,
    )
    .await
}

/// Register the class if needed, then mint the processor as a subject instance.
/// Writing the same `processor_id` twice overwrites its scalars (the setters
/// are `setSingleTarget`) and appends its interpretation classes (deduplicated
/// on read by [`load_processors`]).
///
/// `emit_debug_events_write` is a tri-state intent, resolved at the API
/// boundary from the request's `Option<bool>` field:
/// * `Some(true)` writes the link as `"true"`.
/// * `Some(false)` writes the link as `"false"` — actively overwriting any
///   pre-existing `"true"` link on an updated processor via `setSingleTarget`.
/// * `None` omits the value entirely — the setter is not called and any
///   pre-existing link is preserved verbatim.
///
pub async fn write_processor(
    perspective: &mut PerspectiveInstance,
    cfg: &AutoProcessorConfig,
    emit_debug_events_write: Option<bool>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    // Emptiness is still a hard error — a processor with no classes materialises nothing — but the
    // members themselves are written as links below rather than through the values map.
    if cfg.interpretation_classes.is_empty() {
        anyhow::bail!(
            "write_processor: `{}` has no interpretation_classes",
            cfg.processor_id
        );
    }
    ensure_auto_processor_class(perspective, context).await?;

    let node = processor_node(&cfg.processor_id);
    // Keys MUST match the camelCase property names in AUTO_PROCESSOR_SDNA above
    // (which in turn match the TS @Model field names). The setter lookup in
    // `create_subject` keys on the SDNA property name, not on the path — a
    // snake_case key here means "no setter found, keep the placeholder"
    // (2026-08-20 debug: exact bug we hit).
    let mut values = serde_json::json!({
        "processorId": cfg.processor_id,
        "sourceScopeQuery": cfg.source_scope_query,
        "debounceMs": cfg.debounce_ms.to_string(),
        "batchMin": cfg.batch_min.to_string(),
        "batchMax": cfg.batch_max.to_string(),
        "claimTtlMs": cfg.claim_ttl_ms.to_string(),
    });
    if let Some(base_prefix) = &cfg.base_prefix {
        values["basePrefix"] = base_prefix.clone().into();
    }
    if let Some(max_wait_ms) = cfg.max_wait_ms {
        values["maxWaitMs"] = max_wait_ms.to_string().into();
    }
    if let Some(dedup) = &cfg.dedup_strategy_json {
        values["dedupStrategy"] = dedup.clone().into();
    }
    if let Some(source_window_ms) = cfg.source_window_ms {
        values["sourceWindowMs"] = source_window_ms.to_string().into();
    }
    if let Some(existing_scope) = &cfg.existing_scope {
        let json = serde_json::to_string(existing_scope)
            .map_err(|e| anyhow::anyhow!("write_processor: serialize existing_scope: {e:#}"))?;
        values["existingScope"] = json.into();
    }
    if let Some(mint_scope) = &cfg.mint_scope {
        let json = serde_json::to_string(mint_scope)
            .map_err(|e| anyhow::anyhow!("write_processor: serialize mint_scope: {e:#}"))?;
        values["mintScope"] = json.into();
    }
    if let Some(max_tool_calls) = cfg.max_tool_calls {
        values["maxToolCalls"] = max_tool_calls.to_string().into();
    }
    if let Some(v) = emit_debug_events_write {
        values["emitDebugEvents"] = v.to_string().into();
    }

    // Batch the create_subject + follow-on update_subject calls so the whole
    // AutoProcessorConfig instance lands as ONE `Shared` commit — instead of
    // ~11 individual commits (one per property setter) from `execute_commands`.
    // A single-class processor already blew past the per-perspective
    // `IMMEDIATE_COMMITS_COUNT=20` immediate-commit throttle in
    // `perspective_instance::commit` when combined with the other setup
    // writes on a fresh neighbourhood — subsequent commits (including the
    // wave-1 `InterpretationRun` cursor) landed in the pending-diff queue
    // and only drained on the 3-second timer, well past any p-diff-sync
    // gossip window that would have carried the cursor to peers before the
    // next batch arrived (2026-08-20 Marvin flake root-cause).
    let batch_id = perspective.create_batch().await;
    let write_result: anyhow::Result<()> = async {
        perspective
            .create_subject(
                class_option(),
                node.clone(),
                Some(values),
                Some(batch_id.clone()),
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_processor: create_subject failed: {e:#}"))?;
        /*
           The classes are written as links, not through the values map.

           `create_subject` and `update_subject` resolve a property by looking up its
           `ad4m://setter` actions, and a *collection* has none — it carries `ad4m://adder`
           instead. So every value handed to them for `interpretationClasses` was silently
           discarded, and the instance came back without the one property `load_processors`
           requires first. Every scalar landed, so the write reported success and the failure
           surfaced only as the reader calling the instance malformed, once per watch tick,
           forever.

           Adding the links directly is what the collection's own `addLink` action would do,
           and it needs no shape lookup to get there. Same batch as the instance, so the classes
           and the config it belongs to still commit atomically — a half-written processor is
           precisely the state that produced this bug.

           `Shared`, matching every other link this class writes: the processor set is neighbourhood
           state, and a local-only class list would make one peer's view of what to extract differ
           from everyone else's.
        */
        for class in cfg.interpretation_classes.iter() {
            perspective
                .add_link(
                    Link {
                        source: node.clone(),
                        predicate: Some("ad4m://interpretation_class".to_string()),
                        target: format!("literal:string:{}", class),
                    },
                    LinkStatus::Shared,
                    Some(batch_id.clone()),
                    context,
                )
                .await
                .map_err(|e| {
                    anyhow::anyhow!("write_processor: add_link(interpretation_class) failed: {e:#}")
                })?;
        }
        // Flow selection rides the same way as the class list: links in the
        // same batch, `Shared` status — a peer whose processor view lacked
        // the flow set would run flow-blind passes on the same batches.
        for flow in cfg.flows.iter() {
            perspective
                .add_link(
                    Link {
                        source: node.clone(),
                        predicate: Some("ad4m://flow".to_string()),
                        target: format!("literal:string:{}", flow),
                    },
                    LinkStatus::Shared,
                    Some(batch_id.clone()),
                    context,
                )
                .await
                .map_err(|e| anyhow::anyhow!("write_processor: add_link(flow) failed: {e:#}"))?;
        }
        Ok(())
    }
    .await;

    if let Err(e) = write_result {
        // Discard the pending batch so it doesn't linger in `batch_store` and
        // never get committed. `discard_batch` is idempotent-ish (it returns
        // an error if the id is unknown, which we intentionally ignore) so
        // this is safe on both partial-write and never-wrote failures.
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(e);
    }

    if let Err(e) = perspective.commit_batch(batch_id.clone(), context).await {
        // Defense-in-depth: `commit_batch` already tries to remove the batch
        // on failure per its contract, but drop it explicitly here too so a
        // change to `commit_batch`'s control flow can't leave a stale batch
        // in the store.
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(anyhow::anyhow!(
            "write_processor: commit_batch failed: {e:#}"
        ));
    }

    Ok(())
}

/// Stop a processor by deleting its config instance. Returns whether there was one to delete.
///
/// The registration *is* data: [`load_processors`] reads the processor set out of the perspective's
/// own graph on every watch tick, so deleting the instance is what stops the watch, and there is
/// nothing else to unregister. Without this the only way to stop a processor was for a client to
/// work out the node URI and delete the links itself — reaching around a subject class whose whole
/// purpose is that a processor can be administered through the ordinary model API.
///
/// Idempotent, and it reports which case it was rather than treating one as a failure: a caller
/// tidying up after a processor another peer has already removed is doing the right thing.
///
/// The processor's `InterpretationRun` nodes are deliberately left behind. They are both the record
/// of what it did and the processed-turn cursor keyed on this node (see [`super::cursor`]), so
/// deleting them would make a processor later registered under the same id re-interpret every turn
/// the first one had already read.
///
/// One batch, so the instance goes away in a single commit. A partial removal would leave a node
/// that no longer conforms to the class — invisible to [`load_processors`] and therefore stopped,
/// but stopped by accident rather than by the write, and still carrying whichever links happened to
/// survive.
pub async fn remove_processor(
    perspective: &mut PerspectiveInstance,
    processor_id: &str,
    context: &AgentContext,
) -> anyhow::Result<bool> {
    let node = processor_node(processor_id);
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(node.clone()),
            ..Default::default()
        })
        .await
        .map_err(|e| {
            anyhow::anyhow!("remove_processor(`{processor_id}`): get_links failed: {e:#}")
        })?;

    // Nothing here. Not an error: a processor that was never written and one another peer removed
    // first are the same state, and neither is a failure of this call.
    if links.is_empty() {
        return Ok(false);
    }

    let batch_id = perspective.create_batch().await;
    let removal = perspective
        .remove_links(
            links.into_iter().map(LinkExpression::from).collect(),
            Some(batch_id.clone()),
        )
        .await;

    if let Err(e) = removal {
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(anyhow::anyhow!(
            "remove_processor(`{processor_id}`): remove_links failed: {e:#}"
        ));
    }

    if let Err(e) = perspective.commit_batch(batch_id.clone(), context).await {
        let _ = perspective.discard_batch(&batch_id).await;
        return Err(anyhow::anyhow!(
            "remove_processor(`{processor_id}`): commit_batch failed: {e:#}"
        ));
    }

    Ok(true)
}

fn class_option() -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(AUTO_PROCESSOR_CLASS.to_string()),
        query: None,
    }
}

/// Load every AutoProcessor config from the perspective. Instances missing a
/// required scalar are not `AutoProcessor` instances yet as far as the class's
/// conformance goes (so a partially-synced write from another peer is invisible
/// rather than fatal); one that conforms but carries an unparseable numeric is
/// logged and skipped. Returns configs sorted by `processor_id` for
/// deterministic iteration order.
pub async fn load_processors(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<Vec<AutoProcessorConfig>> {
    // The watcher polls this on every perspective, most of which never declare
    // a processor: no registered class ⇒ no shape to query ⇒ nothing to load.
    if !subject_class_registered(perspective, AUTO_PROCESSOR_TARGET_CLASS).await? {
        return Ok(Vec::new());
    }
    // Property names MUST match the camelCase names in AUTO_PROCESSOR_SDNA
    // above — model_query keys results by property name.
    let query = serde_json::json!({
        "properties": [
            "processorId", "sourceScopeQuery", "basePrefix",
            "interpretationClasses", "flows", "debounceMs", "batchMin", "batchMax",
            "maxWaitMs", "claimTtlMs", "dedupStrategy", "sourceWindowMs",
            "existingScope", "mintScope", "maxToolCalls", "emitDebugEvents",
        ]
    })
    .to_string();
    let result_json = perspective
        .model_query(AUTO_PROCESSOR_CLASS, &query)
        .await
        .map_err(|e| anyhow::anyhow!("load_processors: model_query failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&result_json)
        .map_err(|e| anyhow::anyhow!("load_processors: bad model_query result: {e:#}"))?;

    let mut out = Vec::new();
    for instance in result["instances"].as_array().into_iter().flatten() {
        match config_from_instance(instance) {
            Some(cfg) => out.push(cfg),
            /*
               The instance itself, not just the fact that it failed.

               "has missing / unparseable fields" names neither the field nor its value, and this
               fires once per watch tick forever — so the symptom is an endless stream of a message
               that cannot be acted on. Diagnosing one meant reading three files and guessing, and
               the guesses were wrong twice.

               Every branch that returns `None` from `config_from_instance` is a statement about one
               property's value, so printing the whole instance answers the question outright: which
               field is absent, and how the ones that are present are actually encoded. That second
               half matters more than it looks — a scalar that arrives in an unexpected encoding
               reads as "missing" here, and nothing else in the message would distinguish the two.

               At `warn` alongside the existing line rather than `debug`, because a processor that
               silently stops running is exactly the situation where nobody has debug logging on
               yet, and the instance is a few hundred bytes once per skip.
            */
            None => {
                let id = instance["id"].as_str().unwrap_or("<no id>");
                if first_report_for(id) {
                    log::warn!(
                        "load_processors: AutoProcessor instance `{}` has missing / unparseable \
                         fields; skipping (further occurrences suppressed). Instance as read: {}",
                        id,
                        instance
                    );
                } else {
                    log::debug!("load_processors: still skipping `{}`", id);
                }
            }
        }
    }
    out.sort_by(|a, b| a.processor_id.cmp(&b.processor_id));
    Ok(out)
}

/// Map one hydrated `model_query` instance onto an [`AutoProcessorConfig`], or
/// `None` when a field is missing or does not parse.
fn config_from_instance(instance: &serde_json::Value) -> Option<AutoProcessorConfig> {
    let scalar = |name: &str| scalar_string(instance.get(name));
    let number = |name: &str| scalar(name)?.parse::<i64>().ok();
    let count = |name: &str| scalar(name)?.parse::<usize>().ok();

    // Hydration returns a collection as an array; sort + dedup for a stable
    // iteration order in the watcher (and in downstream tests that assert on
    // the config shape), since neither store order nor sync order is
    // guaranteed.
    let mut interpretation_classes: Vec<String> = instance
        .get("interpretationClasses")?
        .as_array()?
        .iter()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect();
    interpretation_classes.sort();
    interpretation_classes.dedup();
    if interpretation_classes.is_empty() {
        return None;
    }

    // Flow selection: optional collection, same hydrated-array shape as
    // `interpretationClasses`. Absent (pre-flow config) → empty = no flow
    // features. Present-but-malformed bails (`None`) like every other typed
    // field — `interpretation_classes` above gets the loud `first_report_for`
    // warn on malformed data, and a silently flow-blind processor must not be
    // indistinguishable from a deliberately flow-blind one. Sorted + deduped
    // for the same stable-iteration reason.
    let mut flows: Vec<String> = match instance.get("flows") {
        None => Vec::new(),
        Some(v) => v
            .as_array()?
            .iter()
            .filter_map(|v| v.as_str().map(str::to_string))
            .collect(),
    };
    flows.sort();
    flows.dedup();

    // Optional thresholds. Absent `batchMin` → 1 (original behaviour). An
    // absent `maxWaitMs` → `None` (wait indefinitely). A *present but
    // unparseable* value is a config error, so we bail (`None`) exactly like
    // the required fields rather than silently defaulting.
    let batch_min = match scalar("batchMin") {
        Some(_) => count("batchMin")?.max(1),
        None => 1,
    };
    let max_wait_ms = match scalar("maxWaitMs") {
        Some(_) => Some(number("maxWaitMs")?),
        None => None,
    };

    // Timings must be in range or the batch/claim primitives misbehave: a
    // `batchMax` of 0 drains nothing (empty passes forever), a non-positive
    // `claimTtlMs` makes each peer's own claim expire the moment it is
    // written (so every peer reads an empty holder set and processes the same
    // batch), and a *negative* `maxWaitMs` makes the oldest-item deadline
    // "expire" immediately, silently bypassing `batchMin` on every pass. Treat
    // an out-of-range value like an unparseable one — the processor is simply
    // not loaded (and a warn is logged by the caller).
    let debounce_ms = number("debounceMs")?;
    let batch_max = count("batchMax")?;
    let claim_ttl_ms = number("claimTtlMs")?;
    if debounce_ms < 0 || batch_max < 1 || claim_ttl_ms <= 0 || max_wait_ms.is_some_and(|w| w < 0) {
        return None;
    }
    let source_window_ms = match scalar("sourceWindowMs") {
        Some(_) => Some(number("sourceWindowMs")?),
        None => None,
    };
    if source_window_ms.is_some_and(|w| w <= 0) {
        return None;
    }

    // Scope JSON blobs — a present-but-unparseable value is a config error
    // (bail, same policy as other required-shape fields). Absent → None.
    let existing_scope = match scalar("existingScope") {
        Some(json) => match serde_json::from_str::<Scope>(&json) {
            Ok(scope) => Some(scope),
            Err(e) => {
                log::warn!("config_from_instance: existingScope parse failed: {e:#}");
                return None;
            }
        },
        None => None,
    };
    let mint_scope = match scalar("mintScope") {
        Some(json) => match serde_json::from_str::<Scope>(&json) {
            Ok(scope) => Some(scope),
            Err(e) => {
                log::warn!("config_from_instance: mintScope parse failed: {e:#}");
                return None;
            }
        },
        None => None,
    };

    // `maxToolCalls`: absent → `None` (single-shot, no harness). Present but
    // unparseable → bail like the other required-shape fields; silently
    // defaulting to "no tool calls" on a typo would mask the config bug.
    let max_tool_calls = match scalar("maxToolCalls") {
        Some(s) => Some(s.parse::<u32>().ok()?),
        None => None,
    };
    let parse_bool = |name: &str| -> Option<Option<bool>> {
        match scalar(name) {
            Some(s) => match s.parse::<bool>() {
                Ok(v) => Some(Some(v)),
                Err(_) => {
                    log::warn!("config_from_instance: {name} is not `true`/`false`");
                    None
                }
            },
            None => Some(None),
        }
    };
    let emit_debug_events = parse_bool("emitDebugEvents")?.unwrap_or(false);

    Some(AutoProcessorConfig {
        processor_id: scalar("processorId")?,
        source_scope_query: scalar("sourceScopeQuery")?,
        base_prefix: scalar("basePrefix"),
        interpretation_classes,
        flows,
        debounce_ms,
        batch_min,
        batch_max,
        max_wait_ms,
        claim_ttl_ms,
        dedup_strategy_json: scalar("dedupStrategy"),
        source_window_ms,
        existing_scope,
        mint_scope,
        max_tool_calls,
        emit_debug_events,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::{Link, LinkQuery, LinkStatus};

    // SDNA parity is checked TS-side in
    // `tests/js/tests/model/interpretation-models.test.ts`, which reads the
    // same `hardwired_sdna/auto_processor.json` this module `include_str!`s
    // and compares (path, name) pairs against
    // `AutoProcessorConfig.generateSHACL().shape`. A hardcoded Rust-side
    // reference set would fork the source of truth (2026-08-20 bug: paths
    // matched, names diverged, both sides' parity tests passed while
    // `create_subject` writes silently no-op'd).

    fn sample_config(id: &str) -> AutoProcessorConfig {
        AutoProcessorConfig {
            processor_id: id.into(),
            source_scope_query: format!(
                "SELECT ?speaker ?text WHERE {{ ?s <ns://{id}/turn> ?t . }}"
            ),
            base_prefix: Some(format!("soa://{id}/instance/")),
            // Kept in sorted order to match [`load_processors`]'s canonical
            // ordering — see the assertion in
            // [`interpretation_classes_multiple_roundtrip_sorted`] below for
            // the "input order is arbitrary; output order is sorted" contract.
            interpretation_classes: vec!["ns://Question".into(), "ns://Task".into()],
            flows: vec![],
            debounce_ms: 5_000,
            batch_min: 3,
            batch_max: 32,
            max_wait_ms: Some(60_000),
            claim_ttl_ms: 120_000,
            dedup_strategy_json: Some(r#"{"kind":"normalized"}"#.into()),
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
            max_tool_calls: None,
            emit_debug_events: false,
        }
    }

    /// Round-trip: write one config, load it back, get an equal struct.
    #[tokio::test]
    async fn write_then_load_roundtrip() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = sample_config("summariser");
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write_processor");
        let loaded = load_processors(&p).await.expect("load_processors");
        assert_eq!(loaded, vec![cfg]);
    }

    /// The config lives in the *shared* graph: every link the subject class
    /// writes must be `Shared`, or peers would never see the processor set and
    /// the claim election would run over different batches per peer.
    #[tokio::test]
    async fn write_creates_shared_links() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("shared"), Some(false), &ctx)
            .await
            .expect("write_processor");
        let links = p
            .get_links(&LinkQuery {
                source: Some(processor_node("shared")),
                ..Default::default()
            })
            .await
            .expect("get_links");
        assert!(!links.is_empty(), "processor node must carry links");
        for l in &links {
            assert_eq!(
                l.status,
                Some(LinkStatus::Shared),
                "link {:?} must be Shared",
                l.data
            );
        }
    }

    /// A perspective that never declared a processor has no registered class —
    /// the watcher polls `load_processors` on every perspective, so that must
    /// be an empty read, not an error.
    #[tokio::test]
    async fn load_without_registered_class_is_empty() {
        let (p, _shapes, _ctx) = setup_perspective_no_llm(&[]).await;
        let loaded = load_processors(&p).await.expect("load_processors");
        assert!(loaded.is_empty());
    }

    /// Multiple processors on one perspective load together, sorted by
    /// `processor_id` for deterministic order.
    #[tokio::test]
    async fn load_returns_all_processors_sorted() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        // write in reverse-alphabetical order to prove sort is by the field,
        // not by write order.
        for id in ["z-tagger", "a-summariser", "m-classifier"] {
            write_processor(&mut p, &sample_config(id), Some(false), &ctx)
                .await
                .expect("write");
        }
        let ids: Vec<String> = load_processors(&p)
            .await
            .expect("load")
            .into_iter()
            .map(|c| c.processor_id)
            .collect();
        assert_eq!(ids, vec!["a-summariser", "m-classifier", "z-tagger"]);
    }

    /// Optionals absent on the wire come back as `None`, don't disqualify
    /// the config, and the batch numerics round-trip losslessly.
    #[tokio::test]
    async fn load_handles_missing_optionals() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("minimal");
        cfg.dedup_strategy_json = None;
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0], cfg);
        assert!(loaded[0].dedup_strategy_json.is_none());
        assert!(loaded[0].source_window_ms.is_none());
    }

    /// An explicit `source_window_ms` round-trips; omitted stays `None`
    /// (unbounded gather + cursor).
    #[tokio::test]
    async fn source_window_ms_optional_roundtrip() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("windowed");
        cfg.source_window_ms = Some(42);
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded[0].source_window_ms, Some(42));
    }

    /// Both `Scope` variants round-trip verbatim through SDNA-serialized JSON.
    /// Absent scopes stay `None`. Two independent processors may configure
    /// different scopes without cross-contamination — the config
    /// `PartialEq`/`Eq` assertion confirms every field survives verbatim.
    #[tokio::test]
    async fn scope_fields_optional_roundtrip() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        // Case 1: both scopes absent (baseline).
        let bare = sample_config("bare");
        write_processor(&mut p, &bare, Some(false), &ctx)
            .await
            .expect("write");

        // Case 2: `Raw` scope on both sides — different parent nodes so we can
        // prove they don't get conflated on read.
        let mut raw = sample_config("raw-scoped");
        raw.existing_scope = Some(Scope::Raw {
            id: "soa://project/42".into(),
            predicate: "soa://contains".into(),
        });
        raw.mint_scope = Some(Scope::Raw {
            id: "soa://project/42/backlog".into(),
            predicate: "ad4m://has_child".into(),
        });
        write_processor(&mut p, &raw, Some(false), &ctx)
            .await
            .expect("write");

        // Case 3: `Model` scope — verifies the second untagged variant survives
        // the JSON round-trip too (a bare `Raw` would silently match `Model`
        // if the deserializer preferred the first variant).
        let mut model = sample_config("model-scoped");
        model.existing_scope = Some(Scope::Model {
            model: "Project".into(),
            id: "soa://project/42".into(),
            field: Some("tasks".into()),
        });
        model.mint_scope = None;
        write_processor(&mut p, &model, Some(false), &ctx)
            .await
            .expect("write");

        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 3, "three processors round-trip");
        // load_processors sorts by processor_id → bare, model-scoped, raw-scoped.
        assert_eq!(loaded[0], bare);
        assert_eq!(loaded[1], model);
        assert_eq!(loaded[2], raw);
    }

    /// A `mint_scope` value that fails to parse as JSON is a config error, so
    /// the processor is skipped rather than silently loaded without scope
    /// wiring — otherwise a hand-edited scope with a typo would mint
    /// unlinked children as if it were correctly configured.
    #[tokio::test]
    async fn load_skips_node_with_unparseable_scope() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("good-scope"), Some(false), &ctx)
            .await
            .expect("write");
        write_processor(&mut p, &sample_config("garbled-scope"), Some(false), &ctx)
            .await
            .expect("write");
        p.update_subject(
            class_option(),
            processor_node("garbled-scope"),
            serde_json::json!({ "mintScope": "not-json {" }),
            None,
            &ctx,
        )
        .await
        .expect("seed garbled scope");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "good-scope");
    }

    /// A node typed AutoProcessor but missing every required scalar (here: a
    /// partially-synced write with only the type link) does not conform to the
    /// class, so it is invisible rather than crashed on — the watcher must be
    /// robust to in-flight sync state.
    #[tokio::test]
    async fn load_skips_incomplete_node() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        // A complete one first, so the class is registered and we prove the
        // partial is skipped while the valid one still comes through.
        write_processor(&mut p, &sample_config("complete"), Some(false), &ctx)
            .await
            .expect("write complete");
        p.add_links(
            vec![Link {
                source: processor_node("partial"),
                predicate: Some("rdf://type".into()),
                target: AUTO_PROCESSOR_TARGET_CLASS.into(),
            }],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("seed partial");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "complete");
    }

    /// A conforming node whose `debounce_ms` is not parseable as `i64` is
    /// treated the same as a missing field: skipped with a warn, not returned.
    /// The garbled value is written through the class's own setter (rather than
    /// hand-rolled links) so it lands exactly where the loader reads it.
    #[tokio::test]
    async fn load_skips_node_with_unparseable_numeric() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("garbled"), Some(false), &ctx)
            .await
            .expect("write");
        write_processor(&mut p, &sample_config("garbled-2"), Some(false), &ctx)
            .await
            .expect("write");
        p.update_subject(
            class_option(),
            processor_node("garbled-2"),
            serde_json::json!({ "debounceMs": "not-a-number" }),
            None,
            &ctx,
        )
        .await
        .expect("garble debounce_ms");

        let loaded = load_processors(&p).await.expect("load");
        // The well-formed "garbled" config comes through; the node with the
        // unparseable numeric is dropped.
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "garbled");
    }

    /// `claim_ttl_ms` parses fine as `i64`, but a value of `0` is out of
    /// range: a claim that expires the instant it is written lets every peer
    /// re-process the same batch. The in-range-but-invalid case is dropped
    /// exactly like an unparseable one.
    #[tokio::test]
    async fn load_skips_node_with_zero_claim_ttl() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("good"), Some(false), &ctx)
            .await
            .expect("write good");
        write_processor(&mut p, &sample_config("zero-ttl"), Some(false), &ctx)
            .await
            .expect("write zero-ttl");
        p.update_subject(
            class_option(),
            processor_node("zero-ttl"),
            serde_json::json!({ "claimTtlMs": "0" }),
            None,
            &ctx,
        )
        .await
        .expect("zero claim_ttl_ms");

        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "good");
    }

    /// A *negative* `max_wait_ms` parses but is invalid: it would make the
    /// oldest-item deadline expire immediately and silently bypass `batch_min`
    /// on every pass. Treat it as out of range and drop the node.
    #[tokio::test]
    async fn load_skips_node_with_negative_max_wait() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("good"), Some(false), &ctx)
            .await
            .expect("write good");
        write_processor(&mut p, &sample_config("neg-wait"), Some(false), &ctx)
            .await
            .expect("write neg-wait");
        p.update_subject(
            class_option(),
            processor_node("neg-wait"),
            serde_json::json!({ "maxWaitMs": "-1" }),
            None,
            &ctx,
        )
        .await
        .expect("negative max_wait_ms");

        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "good");
    }

    /// Multiple `interpretation_class` targets on the same node round-trip
    /// as the multi-value field, and the loader canonicalises the order —
    /// callers get a stable sequence regardless of write / sync order.
    #[tokio::test]
    async fn interpretation_classes_multiple_roundtrip_sorted() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("many-classes");
        // Deliberately unsorted input.
        cfg.interpretation_classes = vec![
            "ns://Task".into(),
            "ns://ConversationSubgroup".into(),
            "ns://Observation".into(),
            "ns://Question".into(),
        ];
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(
            loaded[0].interpretation_classes,
            vec![
                "ns://ConversationSubgroup".to_string(),
                "ns://Observation".to_string(),
                "ns://Question".to_string(),
                "ns://Task".to_string(),
            ],
            "loader must return interpretation_classes sorted alphabetically"
        );
    }

    /// Flow selection rides the same write/load path as the class list —
    /// sorted + deduped on load, and an absent selection (every pre-flow
    /// config in the wild) loads as empty rather than failing.
    #[tokio::test]
    async fn flows_roundtrip_sorted_and_default_empty() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        // No flows set → loads as empty (backwards compatible).
        let cfg = sample_config("flow-less");
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert!(loaded[0].flows.is_empty(), "absent selection loads empty");

        // Deliberately unsorted + duplicated selection round-trips clean.
        let mut cfg = sample_config("flow-full");
        cfg.flows = vec![
            "delivery://DeliveryFlow".into(),
            "coasys://DeliberationFlow".into(),
            "delivery://DeliveryFlow".into(),
        ];
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        let full = loaded
            .iter()
            .find(|c| c.processor_id == "flow-full")
            .expect("flow-full loads");
        assert_eq!(
            full.flows,
            vec![
                "coasys://DeliberationFlow".to_string(),
                "delivery://DeliveryFlow".to_string(),
            ],
            "flows must load sorted and deduped"
        );
    }

    /// Duplicate `interpretation_class` targets are collapsed by the loader
    /// (harmless double-write, or two peers seeding overlapping configs).
    #[tokio::test]
    async fn interpretation_classes_dedup_on_load() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("dup-classes");
        cfg.interpretation_classes = vec![
            "ns://Task".into(),
            "ns://Task".into(),
            "ns://Question".into(),
        ];
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(
            loaded[0].interpretation_classes,
            vec!["ns://Question".to_string(), "ns://Task".to_string()]
        );
    }

    /// Removing a processor takes it out of the loaded set — which is what stops its watch,
    /// since the loop reads the set back out of the graph on every tick. Its neighbours are left
    /// alone: one processor going away must not disturb another on the same perspective.
    #[tokio::test]
    async fn remove_processor_deletes_only_that_processor() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        for id in ["keep-me", "remove-me"] {
            write_processor(&mut p, &sample_config(id), Some(false), &ctx)
                .await
                .expect("write");
        }

        let removed = remove_processor(&mut p, "remove-me", &ctx)
            .await
            .expect("remove");
        assert!(removed, "there was a processor to remove");

        let ids: Vec<String> = load_processors(&p)
            .await
            .expect("load")
            .into_iter()
            .map(|c| c.processor_id)
            .collect();
        assert_eq!(ids, vec!["keep-me"]);

        let leftovers = p
            .get_links(&LinkQuery {
                source: Some(processor_node("remove-me")),
                ..Default::default()
            })
            .await
            .expect("get_links");
        assert!(
            leftovers.is_empty(),
            "the config node must carry no links, found {leftovers:?}"
        );
    }

    /// Removing a processor that is not there answers `false` rather than erroring. A caller
    /// tidying up after a processor a peer already removed has done the right thing, and a
    /// teardown path is the worst place to raise an avoidable failure.
    #[tokio::test]
    async fn remove_processor_is_idempotent() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config("once"), Some(false), &ctx)
            .await
            .expect("write");

        assert!(remove_processor(&mut p, "once", &ctx).await.expect("first"));
        assert!(!remove_processor(&mut p, "once", &ctx)
            .await
            .expect("second"));
        assert!(
            !remove_processor(&mut p, "never-existed", &ctx)
                .await
                .expect("unknown id"),
            "an unknown processor id is not an error"
        );
    }

    /// Removal really clears the node, so a processor registered again under the same id starts
    /// from the new config rather than inheriting the old one's classes.
    ///
    /// Worth pinning precisely because `write_processor` *appends* to the class collection: the
    /// only reason a re-registration does not accumulate the previous list is that removal left
    /// nothing behind. A removal that missed a link would show up here and nowhere else.
    #[tokio::test]
    async fn a_removed_processor_can_be_registered_again() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("recycled");
        cfg.interpretation_classes = vec!["ns://Task".into()];
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write first");
        remove_processor(&mut p, "recycled", &ctx)
            .await
            .expect("remove");

        cfg.interpretation_classes = vec!["ns://Belief".into()];
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write again");

        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(
            loaded[0].interpretation_classes,
            vec!["ns://Belief".to_string()],
            "a re-registered processor must not inherit the removed one's classes"
        );
    }

    /// A processor written with an empty id is unloadable — `scalar_string` reads an empty scalar
    /// as absent, so `config_from_instance` rejects it — and must still be removable.
    ///
    /// `perspective.addAutoProcessor` refuses an empty `processorId` precisely because of the
    /// first half, but that is a validation on the way *in*: a config written before it existed,
    /// or by a writer that is not that handler, is exactly the junk removal has to be able to
    /// clear. Refusing the id on the way out would strand it with nothing able to take it away.
    #[tokio::test]
    async fn an_unloadable_empty_id_processor_is_still_removable() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        write_processor(&mut p, &sample_config(""), Some(false), &ctx)
            .await
            .expect("write");

        assert!(
            load_processors(&p).await.expect("load").is_empty(),
            "an empty processor_id is read as absent, so the config must not load"
        );

        assert!(
            remove_processor(&mut p, "", &ctx).await.expect("remove"),
            "the config's links are there to remove even though it never loaded"
        );
        let leftovers = p
            .get_links(&LinkQuery {
                source: Some(processor_node("")),
                ..Default::default()
            })
            .await
            .expect("get_links");
        assert!(
            leftovers.is_empty(),
            "nothing must be left behind, found {leftovers:?}"
        );
    }

    /// A config with an empty `interpretation_classes` cannot be written —
    /// nothing to interpret would make the watcher a no-op.
    #[tokio::test]
    async fn write_rejects_empty_interpretation_classes() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("empty");
        cfg.interpretation_classes.clear();
        let err = write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect_err("must reject empty interpretation_classes");
        assert!(
            err.to_string().contains("interpretation_classes"),
            "unexpected error: {err}"
        );
    }

    /// CodeRabbit #903 CR #3 regression, applied per-switch. Each of
    /// `emit_debug_events_write` carries the tri-state intent:
    ///
    /// - `Some(true)` writes `"true"` — the field loads ON.
    /// - `Some(false)` after `Some(true)` writes `"false"` — the field
    ///   loads OFF.
    /// - `None` after `Some(true)` skips the setter — the existing
    ///   `"true"` link is preserved and the field loads ON.
    #[tokio::test]
    async fn debug_switch_tri_state_write_and_preserve() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = sample_config("debug-toggle");

        write_processor(&mut p, &cfg, Some(true), &ctx)
            .await
            .expect("write on");
        let loaded = load_processors(&p).await.expect("load-on");
        assert!(
            loaded[0].emit_debug_events,
            "emit_debug_events must load enabled"
        );

        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write off");
        let loaded = load_processors(&p).await.expect("load-off");
        assert!(
            !loaded[0].emit_debug_events,
            "emit_debug_events MUST load disabled after Some(false)"
        );

        write_processor(&mut p, &cfg, Some(true), &ctx)
            .await
            .expect("write on again");
        write_processor(&mut p, &cfg, None, &ctx)
            .await
            .expect("write no-touch");
        let loaded = load_processors(&p).await.expect("load-preserve");
        assert!(
            loaded[0].emit_debug_events,
            "emit_debug_events MUST stay enabled when the caller omits it (tri-state preserve)"
        );
    }

    /// The tri-state write preserves the existing value when `None` is passed.
    #[tokio::test]
    async fn emit_debug_events_tristate_preserves() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = sample_config("tristate");

        write_processor(&mut p, &cfg, Some(true), &ctx)
            .await
            .expect("write on");
        let loaded = load_processors(&p).await.expect("load on");
        assert!(loaded[0].emit_debug_events);

        // None preserves existing value
        write_processor(&mut p, &cfg, None, &ctx)
            .await
            .expect("write None");
        let loaded = load_processors(&p).await.expect("load None");
        assert!(
            loaded[0].emit_debug_events,
            "emit_debug_events must stay enabled when the caller omits it (tri-state preserve)"
        );

        // Explicit false turns it off
        write_processor(&mut p, &cfg, Some(false), &ctx)
            .await
            .expect("write off");
        let loaded = load_processors(&p).await.expect("load off");
        assert!(!loaded[0].emit_debug_events);
    }
}
