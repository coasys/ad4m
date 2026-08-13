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
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};

use super::scalar_string;
use crate::perspectives::hardwired_class::{ensure_subject_class, subject_class_registered};

/// Local subject-class name of a processor config.
pub(crate) const AUTO_PROCESSOR_CLASS: &str = "AutoProcessor";
/// Target-class URI of [`AUTO_PROCESSOR_CLASS`] — used to detect prior
/// registration.
const AUTO_PROCESSOR_TARGET_CLASS: &str = "ad4m://AutoProcessor";

/// Hard-wired SDNA for the [`AUTO_PROCESSOR_CLASS`] subject class.
///
/// The `type` flag is the class discriminator (and, with the other
/// `min_count: 1` properties, the `model_query` conformance pattern: a
/// half-synced node missing a required scalar simply is not an instance yet).
/// `interpretation_class` is declared `collection` — the marker the shape
/// reader keys on for a multi-valued *literal* property — so its `addLink`
/// setter accumulates class URIs instead of replacing them, and hydration
/// returns them as an array. No `maxCount` is declared on it: the number of
/// classes one processor materializes is open-ended.
const AUTO_PROCESSOR_SDNA: &str = r#"{
  "target_class":"ad4m://AutoProcessor",
  "interpretation_hint":"An automatic interpretation processor: the content it watches, the classes it materializes, and its batching/claim timings.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"rdf://type","target":"ad4m://AutoProcessor"}],
  "properties":[
    {"path":"rdf://type","name":"type","has_value":"ad4m://AutoProcessor","min_count":1,"max_count":1},
    {"path":"ad4m://processor_id","name":"processor_id","identity":true,"min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://processor_id","target":"value"}]},
    {"path":"ad4m://source_scope_query","name":"source_scope_query","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://source_scope_query","target":"value"}]},
    {"path":"ad4m://base_prefix","name":"base_prefix","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://base_prefix","target":"value"}]},
    {"path":"ad4m://interpretation_class","name":"interpretation_class","collection":true,"min_count":1,"setter":[{"action":"addLink","source":"this","predicate":"ad4m://interpretation_class","target":"value"}]},
    {"path":"ad4m://debounce_ms","name":"debounce_ms","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://debounce_ms","target":"value"}]},
    {"path":"ad4m://batch_min","name":"batch_min","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://batch_min","target":"value"}]},
    {"path":"ad4m://batch_max","name":"batch_max","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://batch_max","target":"value"}]},
    {"path":"ad4m://max_wait_ms","name":"max_wait_ms","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://max_wait_ms","target":"value"}]},
    {"path":"ad4m://claim_ttl_ms","name":"claim_ttl_ms","min_count":1,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://claim_ttl_ms","target":"value"}]},
    {"path":"ad4m://source_window_ms","name":"source_window_ms","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://source_window_ms","target":"value"}]},
    {"path":"ad4m://dedup_strategy","name":"dedup_strategy","min_count":0,"max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ad4m://dedup_strategy","target":"value"}]}
  ]
}"#;

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
}

/// Deterministic node URI for an AutoProcessor. Deterministic in
/// `processor_id` so every peer addresses the same node once state syncs.
pub fn processor_node(processor_id: &str) -> String {
    format!("ad4m://autoprocessor/{processor_id}")
}

/// Idempotently register the hard-wired [`AUTO_PROCESSOR_CLASS`] subject class.
/// Refreshes the SHACL if an older registration predates `source_window_ms`.
pub async fn ensure_auto_processor_class(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        AUTO_PROCESSOR_CLASS,
        AUTO_PROCESSOR_TARGET_CLASS,
        AUTO_PROCESSOR_SDNA,
        Some("ad4m://source_window_ms"),
        context,
    )
    .await
}

/// Register the class if needed, then mint the processor as a subject instance.
/// Writing the same `processor_id` twice overwrites its scalars (the setters
/// are `setSingleTarget`) and appends its interpretation classes (deduplicated
/// on read by [`load_processors`]).
pub async fn write_processor(
    perspective: &mut PerspectiveInstance,
    cfg: &AutoProcessorConfig,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let Some((first_class, more_classes)) = cfg.interpretation_classes.split_first() else {
        anyhow::bail!(
            "write_processor: `{}` has no interpretation_classes",
            cfg.processor_id
        );
    };
    ensure_auto_processor_class(perspective, context).await?;

    let node = processor_node(&cfg.processor_id);
    let mut values = serde_json::json!({
        "processor_id": cfg.processor_id,
        "source_scope_query": cfg.source_scope_query,
        "interpretation_class": first_class,
        "debounce_ms": cfg.debounce_ms.to_string(),
        "batch_min": cfg.batch_min.to_string(),
        "batch_max": cfg.batch_max.to_string(),
        "claim_ttl_ms": cfg.claim_ttl_ms.to_string(),
    });
    if let Some(base_prefix) = &cfg.base_prefix {
        values["base_prefix"] = base_prefix.clone().into();
    }
    if let Some(max_wait_ms) = cfg.max_wait_ms {
        values["max_wait_ms"] = max_wait_ms.to_string().into();
    }
    if let Some(dedup) = &cfg.dedup_strategy_json {
        values["dedup_strategy"] = dedup.clone().into();
    }
    if let Some(source_window_ms) = cfg.source_window_ms {
        values["source_window_ms"] = source_window_ms.to_string().into();
    }
    perspective
        .create_subject(class_option(), node.clone(), Some(values), None, context)
        .await
        .map_err(|e| anyhow::anyhow!("write_processor: create_subject failed: {e:#}"))?;

    // `create_subject` applies one value per property, so the remaining
    // members of the `interpretation_class` collection go through the same
    // `addLink` setter one at a time.
    for class in more_classes {
        perspective
            .update_subject(
                class_option(),
                node.clone(),
                serde_json::json!({ "interpretation_class": class }),
                None,
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("write_processor: update_subject(class) failed: {e:#}"))?;
    }
    Ok(())
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
    let query = serde_json::json!({
        "properties": [
            "processor_id", "source_scope_query", "base_prefix",
            "interpretation_class", "debounce_ms", "batch_min", "batch_max",
            "max_wait_ms", "claim_ttl_ms", "dedup_strategy", "source_window_ms",
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
            None => log::warn!(
                "load_processors: AutoProcessor instance `{}` has missing / unparseable \
                 fields; skipping",
                instance["id"].as_str().unwrap_or("<no id>")
            ),
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
        .get("interpretation_class")?
        .as_array()?
        .iter()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect();
    interpretation_classes.sort();
    interpretation_classes.dedup();
    if interpretation_classes.is_empty() {
        return None;
    }

    // Optional thresholds. Absent `batch_min` → 1 (original behaviour). An
    // absent `max_wait_ms` → `None` (wait indefinitely). A *present but
    // unparseable* value is a config error, so we bail (`None`) exactly like
    // the required fields rather than silently defaulting.
    let batch_min = match scalar("batch_min") {
        Some(_) => count("batch_min")?.max(1),
        None => 1,
    };
    let max_wait_ms = match scalar("max_wait_ms") {
        Some(_) => Some(number("max_wait_ms")?),
        None => None,
    };

    // Timings must be in range or the batch/claim primitives misbehave: a
    // `batch_max` of 0 drains nothing (empty passes forever), a non-positive
    // `claim_ttl_ms` makes each peer's own claim expire the moment it is
    // written (so every peer reads an empty holder set and processes the same
    // batch), and a *negative* `max_wait_ms` makes the oldest-item deadline
    // "expire" immediately, silently bypassing `batch_min` on every pass. Treat
    // an out-of-range value like an unparseable one — the processor is simply
    // not loaded (and a warn is logged by the caller).
    let debounce_ms = number("debounce_ms")?;
    let batch_max = count("batch_max")?;
    let claim_ttl_ms = number("claim_ttl_ms")?;
    if debounce_ms < 0 || batch_max < 1 || claim_ttl_ms <= 0 || max_wait_ms.is_some_and(|w| w < 0) {
        return None;
    }
    let source_window_ms = match scalar("source_window_ms") {
        Some(_) => Some(number("source_window_ms")?),
        None => None,
    };
    if source_window_ms.is_some_and(|w| w <= 0) {
        return None;
    }

    Some(AutoProcessorConfig {
        processor_id: scalar("processor_id")?,
        source_scope_query: scalar("source_scope_query")?,
        base_prefix: scalar("base_prefix"),
        interpretation_classes,
        debounce_ms,
        batch_min,
        batch_max,
        max_wait_ms,
        claim_ttl_ms,
        dedup_strategy_json: scalar("dedup_strategy"),
        source_window_ms,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::{Link, LinkQuery, LinkStatus};

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
            debounce_ms: 5_000,
            batch_min: 3,
            batch_max: 32,
            max_wait_ms: Some(60_000),
            claim_ttl_ms: 120_000,
            dedup_strategy_json: Some(r#"{"kind":"normalized"}"#.into()),
            source_window_ms: None,
        }
    }

    /// Round-trip: write one config, load it back, get an equal struct.
    #[tokio::test]
    async fn write_then_load_roundtrip() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = sample_config("summariser");
        write_processor(&mut p, &cfg, &ctx)
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
        write_processor(&mut p, &sample_config("shared"), &ctx)
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
            write_processor(&mut p, &sample_config(id), &ctx)
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
        write_processor(&mut p, &cfg, &ctx).await.expect("write");
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
        write_processor(&mut p, &cfg, &ctx).await.expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded[0].source_window_ms, Some(42));
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
        write_processor(&mut p, &sample_config("complete"), &ctx)
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
        write_processor(&mut p, &sample_config("garbled"), &ctx)
            .await
            .expect("write");
        write_processor(&mut p, &sample_config("garbled-2"), &ctx)
            .await
            .expect("write");
        p.update_subject(
            class_option(),
            processor_node("garbled-2"),
            serde_json::json!({ "debounce_ms": "not-a-number" }),
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
        write_processor(&mut p, &sample_config("good"), &ctx)
            .await
            .expect("write good");
        write_processor(&mut p, &sample_config("zero-ttl"), &ctx)
            .await
            .expect("write zero-ttl");
        p.update_subject(
            class_option(),
            processor_node("zero-ttl"),
            serde_json::json!({ "claim_ttl_ms": "0" }),
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
        write_processor(&mut p, &sample_config("good"), &ctx)
            .await
            .expect("write good");
        write_processor(&mut p, &sample_config("neg-wait"), &ctx)
            .await
            .expect("write neg-wait");
        p.update_subject(
            class_option(),
            processor_node("neg-wait"),
            serde_json::json!({ "max_wait_ms": "-1" }),
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
        write_processor(&mut p, &cfg, &ctx).await.expect("write");
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
        write_processor(&mut p, &cfg, &ctx).await.expect("write");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(
            loaded[0].interpretation_classes,
            vec!["ns://Question".to_string(), "ns://Task".to_string()]
        );
    }

    /// A config with an empty `interpretation_classes` cannot be written —
    /// nothing to interpret would make the watcher a no-op.
    #[tokio::test]
    async fn write_rejects_empty_interpretation_classes() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let mut cfg = sample_config("empty");
        cfg.interpretation_classes.clear();
        let err = write_processor(&mut p, &cfg, &ctx)
            .await
            .expect_err("must reject empty interpretation_classes");
        assert!(
            err.to_string().contains("interpretation_classes"),
            "unexpected error: {err}"
        );
    }
}
