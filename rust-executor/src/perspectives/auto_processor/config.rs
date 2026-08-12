//! `AutoProcessor` configuration — the "which processor runs on this
//! perspective, over what content, with what LLM" record.
//!
//! Phase **P-B1** of the AutoProcessor arc. Config primitive only — the
//! executor watcher (P-B2) reads these back from a perspective, then debounces
//! new content in `source_scope_query`, wins a claim via [`super::claim`], and
//! calls `run_interpretation_with_strategy` for the configured
//! `interpretation_classes`.
//!
//! Kept as a hand-crafted link shape (parallel to [`super::claim`]) rather
//! than a full SHACL subject class. This is executor-owned coordination
//! config, not user-domain data; turning it into a proper SubjectClass so app
//! UIs can configure processors from outside is a follow-up. Living in the
//! shared perspective (as `Shared` links) means every peer in the
//! neighbourhood sees the same processor set once state syncs, so the
//! election in [`super::claim::try_claim`] is over the same batches on every
//! peer.
//!
//! `dedup_strategy_json` is intentionally stored as an opaque JSON blob rather
//! than a typed enum: the watcher (P-B2) will deserialize it into
//! [`crate::perspectives::interpretation::DedupStrategy`]. This keeps P-B1 free
//! of the dedup module's shape (which does not implement `Serialize`).
//!
//! ## Link shape (all `Shared`)
//! ```text
//! processor node = ad4m://autoprocessor/<processor_id>
//!   -- rdf://type                  --> ad4m://AutoProcessor
//!   -- ad4m://processor_id         --> <string>          (identity)
//!   -- ad4m://source_scope_query   --> <SPARQL SELECT returning ?speaker ?text>
//!   -- ad4m://interpretation_class --> <class URI>        (repeatable, >= 1)
//!   -- ad4m://debounce_ms          --> <i64 as string>
//!   -- ad4m://batch_min            --> <usize as string>  (optional, default 1)
//!   -- ad4m://batch_max            --> <usize as string>
//!   -- ad4m://max_wait_ms          --> <i64 as string>    (optional)
//!   -- ad4m://claim_ttl_ms         --> <i64 as string>
//!   -- ad4m://dedup_strategy       --> <JSON string>      (optional)
//! ```

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkQuery, LinkStatus};

const P_TYPE: &str = "rdf://type";
const T_AUTO_PROCESSOR: &str = "ad4m://AutoProcessor";
const P_PROCESSOR_ID: &str = "ad4m://processor_id";
const P_SOURCE_SCOPE_QUERY: &str = "ad4m://source_scope_query";
const P_INTERPRETATION_CLASS: &str = "ad4m://interpretation_class";
const P_DEBOUNCE_MS: &str = "ad4m://debounce_ms";
const P_BATCH_MIN: &str = "ad4m://batch_min";
const P_BATCH_MAX: &str = "ad4m://batch_max";
const P_MAX_WAIT_MS: &str = "ad4m://max_wait_ms";
const P_CLAIM_TTL_MS: &str = "ad4m://claim_ttl_ms";
const P_DEDUP_STRATEGY: &str = "ad4m://dedup_strategy";

/// Everything the executor watcher (P-B2) needs to schedule and run a single
/// auto-processor pass over a source perspective.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AutoProcessorConfig {
    /// Human-meaningful processor name. Also part of the batch-node URI in
    /// [`super::claim::batch_node`], so claims by different processors never
    /// collide even on the same id-set.
    pub processor_id: String,
    /// SPARQL `SELECT` returning `?speaker` + `?text` bindings — the same
    /// shape the interpretation engine's
    /// [`crate::perspectives::interpretation::graph::gather_transcript_sparql`]
    /// accepts. Defines "the content this processor watches".
    pub source_scope_query: String,
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
}

/// Deterministic node URI for an AutoProcessor. Deterministic in
/// `processor_id` so every peer addresses the same node once state syncs.
pub fn processor_node(processor_id: &str) -> String {
    format!("ad4m://autoprocessor/{processor_id}")
}

/// Write an AutoProcessor config as `Shared` links so it syncs across the
/// neighbourhood. Idempotent-per-call in the sense that it always writes the
/// full set; but writing the same config twice will create duplicate links
/// (dedup up on read via [`load_processors`]).
pub async fn write_processor(
    perspective: &mut PerspectiveInstance,
    cfg: &AutoProcessorConfig,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if cfg.interpretation_classes.is_empty() {
        anyhow::bail!(
            "write_processor: `{}` has no interpretation_classes",
            cfg.processor_id
        );
    }
    let node = processor_node(&cfg.processor_id);
    let mut links = vec![
        Link {
            source: node.clone(),
            predicate: Some(P_TYPE.into()),
            target: T_AUTO_PROCESSOR.into(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_PROCESSOR_ID.into()),
            target: cfg.processor_id.clone(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_SOURCE_SCOPE_QUERY.into()),
            target: cfg.source_scope_query.clone(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_DEBOUNCE_MS.into()),
            target: cfg.debounce_ms.to_string(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_BATCH_MIN.into()),
            target: cfg.batch_min.to_string(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_BATCH_MAX.into()),
            target: cfg.batch_max.to_string(),
        },
        Link {
            source: node.clone(),
            predicate: Some(P_CLAIM_TTL_MS.into()),
            target: cfg.claim_ttl_ms.to_string(),
        },
    ];
    if let Some(max_wait_ms) = cfg.max_wait_ms {
        links.push(Link {
            source: node.clone(),
            predicate: Some(P_MAX_WAIT_MS.into()),
            target: max_wait_ms.to_string(),
        });
    }
    for class in &cfg.interpretation_classes {
        links.push(Link {
            source: node.clone(),
            predicate: Some(P_INTERPRETATION_CLASS.into()),
            target: class.clone(),
        });
    }
    if let Some(dedup) = &cfg.dedup_strategy_json {
        links.push(Link {
            source: node,
            predicate: Some(P_DEDUP_STRATEGY.into()),
            target: dedup.clone(),
        });
    }
    perspective
        .add_links(links, LinkStatus::Shared, None, context)
        .await
        .map_err(|e| anyhow::anyhow!("write_processor: add_links failed: {e:#}"))?;
    Ok(())
}

async fn get_targets(
    perspective: &PerspectiveInstance,
    source: &str,
    predicate: &str,
) -> anyhow::Result<Vec<String>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(source.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("get_links({source} {predicate}): {e:#}"))?;
    Ok(links.into_iter().map(|l| l.data.target).collect())
}

async fn first_target(
    perspective: &PerspectiveInstance,
    source: &str,
    predicate: &str,
) -> anyhow::Result<Option<String>> {
    Ok(get_targets(perspective, source, predicate)
        .await?
        .into_iter()
        .next())
}

/// Load every AutoProcessor config from the perspective. Nodes typed
/// `ad4m://AutoProcessor` but missing any required scalar (or a parseable
/// numeric) are logged and skipped — so an in-flight write from another peer
/// (partial sync) does not crash the watcher, and neither does human hand-
/// editing of the graph. Returns configs sorted by `processor_id` for
/// deterministic iteration order.
pub async fn load_processors(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<Vec<AutoProcessorConfig>> {
    let typed_links = perspective
        .get_links(&LinkQuery {
            predicate: Some(P_TYPE.into()),
            target: Some(T_AUTO_PROCESSOR.into()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("load_processors: get_links(type) failed: {e:#}"))?;

    let mut nodes: Vec<String> = typed_links.into_iter().map(|l| l.data.source).collect();
    nodes.sort();
    nodes.dedup();

    let mut out = Vec::new();
    for node in nodes {
        match load_one(perspective, &node).await? {
            Some(cfg) => out.push(cfg),
            None => log::warn!(
                "load_processors: node `{node}` typed AutoProcessor but missing / \
                    unparseable required fields; skipping"
            ),
        }
    }
    out.sort_by(|a, b| a.processor_id.cmp(&b.processor_id));
    Ok(out)
}

async fn load_one(
    perspective: &PerspectiveInstance,
    node: &str,
) -> anyhow::Result<Option<AutoProcessorConfig>> {
    let Some(processor_id) = first_target(perspective, node, P_PROCESSOR_ID).await? else {
        return Ok(None);
    };
    let Some(source_scope_query) = first_target(perspective, node, P_SOURCE_SCOPE_QUERY).await?
    else {
        return Ok(None);
    };
    let mut interpretation_classes = get_targets(perspective, node, P_INTERPRETATION_CLASS).await?;
    if interpretation_classes.is_empty() {
        return Ok(None);
    }
    // `get_links` order is not guaranteed across storage backends / sync
    // states, so sort for deterministic iteration in the watcher (and in
    // downstream tests that assert on the config shape).
    interpretation_classes.sort();
    interpretation_classes.dedup();
    let Some(debounce_ms_s) = first_target(perspective, node, P_DEBOUNCE_MS).await? else {
        return Ok(None);
    };
    let Some(batch_max_s) = first_target(perspective, node, P_BATCH_MAX).await? else {
        return Ok(None);
    };
    let Some(claim_ttl_ms_s) = first_target(perspective, node, P_CLAIM_TTL_MS).await? else {
        return Ok(None);
    };
    let (Ok(debounce_ms), Ok(batch_max), Ok(claim_ttl_ms)) = (
        debounce_ms_s.parse::<i64>(),
        batch_max_s.parse::<usize>(),
        claim_ttl_ms_s.parse::<i64>(),
    ) else {
        return Ok(None);
    };

    // Optional thresholds. Absent `batch_min` → 1 (original behaviour). An
    // absent `max_wait_ms` → `None` (wait indefinitely). A *present but
    // unparseable* value is a config error, so we bail (`Ok(None)`) exactly
    // like the required fields rather than silently defaulting.
    let batch_min = match first_target(perspective, node, P_BATCH_MIN).await? {
        Some(s) => match s.parse::<usize>() {
            Ok(n) => n.max(1),
            Err(_) => return Ok(None),
        },
        None => 1,
    };
    let max_wait_ms = match first_target(perspective, node, P_MAX_WAIT_MS).await? {
        Some(s) => match s.parse::<i64>() {
            Ok(n) => Some(n),
            Err(_) => return Ok(None),
        },
        None => None,
    };

    let dedup_strategy_json = first_target(perspective, node, P_DEDUP_STRATEGY).await?;

    Ok(Some(AutoProcessorConfig {
        processor_id,
        source_scope_query,
        interpretation_classes,
        debounce_ms,
        batch_min,
        batch_max,
        max_wait_ms,
        claim_ttl_ms,
        dedup_strategy_json,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::LinkStatus;

    fn sample_config(id: &str) -> AutoProcessorConfig {
        AutoProcessorConfig {
            processor_id: id.into(),
            source_scope_query: format!(
                "SELECT ?speaker ?text WHERE {{ ?s <ns://{id}/turn> ?t . }}"
            ),
            // Kept in sorted order to match [`load_processors`]'s canonical
            // ordering — see the assertion in
            // [`load_returns_classes_in_deterministic_order`] below for the
            // "input order is arbitrary; output order is sorted" contract.
            interpretation_classes: vec!["ns://Question".into(), "ns://Task".into()],
            debounce_ms: 5_000,
            batch_min: 3,
            batch_max: 32,
            max_wait_ms: Some(60_000),
            claim_ttl_ms: 120_000,
            dedup_strategy_json: Some(r#"{"kind":"normalized"}"#.into()),
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
    }

    /// A node typed AutoProcessor but missing a required scalar (here: a
    /// partially-synced write with only the type link) is skipped, not
    /// crashed on — the watcher must be robust to in-flight sync state.
    #[tokio::test]
    async fn load_skips_incomplete_node() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        p.add_links(
            vec![Link {
                source: processor_node("partial"),
                predicate: Some(P_TYPE.into()),
                target: T_AUTO_PROCESSOR.into(),
            }],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("seed partial");
        // A complete one alongside, to prove the partial is skipped but the
        // valid one still comes through.
        write_processor(&mut p, &sample_config("complete"), &ctx)
            .await
            .expect("write complete");
        let loaded = load_processors(&p).await.expect("load");
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "complete");
    }

    /// A node whose `debounce_ms` link is not parseable as `i64` is treated
    /// the same as a missing field: skipped with a warn, not returned.
    #[tokio::test]
    async fn load_skips_node_with_unparseable_numeric() {
        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
        let cfg = sample_config("garbled");
        write_processor(&mut p, &cfg, &ctx).await.expect("write");
        // Overwrite debounce_ms with a non-numeric target by adding another
        // link with the same source+predicate: the first-target reader picks
        // the first, which we can't guarantee is our garbled one — instead
        // seed a whole SEPARATE node manually with a garbled numeric.
        p.add_links(
            vec![
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_TYPE.into()),
                    target: T_AUTO_PROCESSOR.into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_PROCESSOR_ID.into()),
                    target: "garbled-2".into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_SOURCE_SCOPE_QUERY.into()),
                    target: "SELECT ?speaker ?text WHERE {}".into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_INTERPRETATION_CLASS.into()),
                    target: "ns://Task".into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_DEBOUNCE_MS.into()),
                    target: "not-a-number".into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_BATCH_MAX.into()),
                    target: "32".into(),
                },
                Link {
                    source: processor_node("garbled-2"),
                    predicate: Some(P_CLAIM_TTL_MS.into()),
                    target: "60000".into(),
                },
            ],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("seed garbled-2");

        let loaded = load_processors(&p).await.expect("load");
        // The original well-formed "garbled" config comes through; the
        // second, unparseable node is dropped.
        assert_eq!(loaded.len(), 1);
        assert_eq!(loaded[0].processor_id, "garbled");
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
