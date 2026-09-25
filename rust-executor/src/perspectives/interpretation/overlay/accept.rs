//! Human accept / reject of the interpretation overlay (design §8 accept-loop).
//!
//! An `InterpretationOverlay` marks an instance (or a single property) as
//! LLM-inferred and, when the §4 gate declined to touch a human/seed value,
//! carries the model's *staged* value in an `inferred/<p>` link. A human then
//! resolves each suggestion:
//!
//! * **accept** — the LLM's value becomes the real, human-owned value and the
//!   overlay is **deleted** (Nico's call: no `accepted` flag, since one base can
//!   be touched by many runs and kept overlays would accumulate). Once the
//!   overlay is gone the §4 gate treats the base as human-owned, so a later run
//!   will not silently overwrite it — delete *is* the lock.
//! * **reject** — the suggestion is dropped. Rejecting a whole *create* removes
//!   the suggested instance entirely; rejecting an *update* just drops the
//!   overlay and leaves the real value as it was.
//!
//! Property-scoped accept/reject act on a single `inferred/<p>`; when the last
//! `inferred/<p>` is gone the overlay's `kind`/`run` links are cleaned up too.

use super::{INFERRED_PREFIX, OVERLAY_KIND_PRED};
use crate::agent::AgentContext;
use crate::perspectives::interpretation::replace_link;
use crate::perspectives::model_query::utils::{emittable_iri, parse_literal_value};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::sparql_store::decorated_links_query;
use crate::types::{DecoratedLinkExpression, LinkExpression, LinkQuery};
use std::collections::BTreeMap;

const OVERLAY_RUN_PRED: &str = "ad4m://interp/run";

/// One pending overlay, flattened for the query surface / UIs.
#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct OverlayView {
    /// The base instance the overlay sits on.
    pub base: String,
    /// `"create"` | `"update"` — whether the LLM authored the whole instance or
    /// only proposed changes to an existing one.
    pub kind: String,
    /// The `InterpretationRun` that last wrote it, if present.
    pub run: Option<String>,
    /// `(real predicate, staged value)` pairs — the model's proposed values a
    /// human has not yet accepted.
    pub inferred: Vec<(String, serde_json::Value)>,
}

/// Read the overlay on `base`, or `None` when the base carries none. Only the
/// tests call it now (as the per-base reference for [`list_overlays`]).
#[cfg(test)]
pub(crate) async fn overlay_of(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<Option<OverlayView>> {
    let all = links_from(perspective, base).await?;
    Ok(view_from_links(base, &all))
}

/// Build the view for `base` from its links, in `get_links` order (timestamp
/// ascending). `None` when no `kind` link is among them. Links with other
/// predicates are ignored, so this accepts either every link of the base or
/// only its overlay links.
fn view_from_links(base: &str, links: &[DecoratedLinkExpression]) -> Option<OverlayView> {
    let kind = first_target(links, OVERLAY_KIND_PRED)?;
    let run = first_target(links, OVERLAY_RUN_PRED);
    let inferred = links
        .iter()
        .filter_map(|l| {
            let pred = l.data.predicate.as_deref()?;
            let real = pred.strip_prefix(INFERRED_PREFIX)?;
            Some((real.to_string(), parse_literal_value(&l.data.target)))
        })
        .collect();
    Some(OverlayView {
        base: base.to_string(),
        kind,
        run,
        inferred,
    })
}

/// Every base in the perspective that currently carries an overlay — the
/// pending-suggestions list a UI renders — sorted by base.
///
/// Two reads, whatever the number of overlays:
///
/// 1. `get_links` on the `kind` predicate gives the bases.
/// 2. One SPARQL query over all those bases returns only their overlay links
///    (`kind`, `run`, `inferred/<p>`). The predicate filter runs in the store,
///    so a base's normal data is never decoded or returned.
///
/// The previous version ran step 1 and then one `get_links` per base, each
/// returning *every* link of the base: 1+N reads that grew with the number of
/// pending overlays and with the size of each base.
///
/// `model_query` does not fit here: it reads instances of one SHACL class,
/// while an overlay can sit on a base of any class (or none), and its
/// `inferred/<p>` predicates are not known in advance. `LinkQuery` matches
/// exact predicates only, hence the SPARQL filter.
///
/// The result is the same as calling [`overlay_of`] on each base: step 2
/// decodes rows the way `get_links` does, each base's links are sorted by
/// timestamp like `get_links`, and [`view_from_links`] builds both views.
/// Like `get_links`, it applies no per-user filter; access is checked per
/// perspective by the caller.
pub(crate) async fn list_overlays(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<Vec<OverlayView>> {
    let kind_links = perspective
        .get_links(&LinkQuery {
            predicate: Some(OVERLAY_KIND_PRED.to_string()),
            ..Default::default()
        })
        .await?;
    let mut bases: Vec<String> = kind_links.into_iter().map(|l| l.data.source).collect();
    bases.sort();
    bases.dedup();
    if bases.is_empty() {
        return Ok(Vec::new());
    }

    // Bases that parse as IRIs go into one query with a seekable `VALUES`
    // block. The store also holds `new_unchecked` subjects that do not (Flux's
    // `literal://string:…` ids). SPARQL can only match those through a
    // `STR()` filter, which scans every quad, so they are read one index seek
    // each, as before. There are few of them.
    let (iri_bases, other_bases): (Vec<String>, Vec<String>) =
        bases.into_iter().partition(|b| emittable_iri(b));
    let mut by_base: BTreeMap<String, Vec<DecoratedLinkExpression>> = BTreeMap::new();
    if !iri_bases.is_empty() {
        let rows = perspective
            .sparql_store
            .query_decorated_links(&overlay_links_query(&iri_bases))?;
        for l in rows {
            by_base.entry(l.data.source.clone()).or_default().push(l);
        }
    }
    for base in other_bases {
        let links = links_from(perspective, &base).await?;
        by_base.insert(base, links);
    }

    Ok(by_base
        .into_iter()
        .filter_map(|(base, mut links)| {
            // Same stable sort as `PerspectiveInstance::get_links`.
            links.sort_by_key(|l| {
                chrono::DateTime::parse_from_rfc3339(&l.timestamp).unwrap_or_default()
            });
            view_from_links(&base, &links)
        })
        .collect())
}

/// The batched read behind [`list_overlays`]: every overlay link whose
/// source is one of `bases`, in the row shape `get_links` decodes. Every base
/// must pass [`emittable_iri`].
fn overlay_links_query(bases: &[String]) -> String {
    let values = bases
        .iter()
        .map(|b| format!("<{b}>"))
        .collect::<Vec<_>>()
        .join(" ");
    decorated_links_query(
        &format!("VALUES ?source {{ {values} }}"),
        &format!(
            r#"?predicate = <{OVERLAY_KIND_PRED}> || ?predicate = <{OVERLAY_RUN_PRED}> || STRSTARTS(STR(?predicate), "{INFERRED_PREFIX}")"#
        ),
    )
}

/// Accept the overlay's suggestion(s) on `base`: materialize the staged value(s)
/// as the real, human-owned value(s) and delete the (targeted) overlay. With
/// `property = None` the whole overlay is accepted; with `Some(p)` only that
/// property's suggestion is.
pub(crate) async fn accept_interpretation(
    perspective: &mut PerspectiveInstance,
    base: &str,
    property: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let all = links_from(perspective, base).await?;
    if first_target(&all, OVERLAY_KIND_PRED).is_none() {
        anyhow::bail!("accept_interpretation: no overlay on `{base}`");
    }

    // Materialize each targeted `inferred/<p>` into the real value, then drop it.
    for l in inferred_links(&all, property) {
        let real_pred = l
            .data
            .predicate
            .as_deref()
            .and_then(|p| p.strip_prefix(INFERRED_PREFIX))
            .expect("filtered to inferred links");
        // Copy the staged literal target verbatim onto the real predicate — same
        // encoding the interpreter writes for a literal-valued property. Accept
        // is a user-triggered single-property operation, so no caller-owned
        // batch to thread — `None` matches every other one-shot call site.
        replace_link(perspective, base, real_pred, &l.data.target, None, context).await?;
        perspective
            .remove_links(vec![LinkExpression::from(l.clone())], None)
            .await?;
    }

    // If no staged suggestion remains, the overlay has done its job — remove its
    // `kind`/`run` so the base reads as plain human-owned data (delete = lock).
    prune_overlay_shell_if_empty(perspective, base).await
}

/// Reject the overlay's suggestion(s) on `base`. `property = Some(p)` drops just
/// that suggestion. `property = None` rejects the whole base: a `create` overlay
/// deletes the suggested instance outright — including any inbound links (e.g.
/// an AutoProcessor `mint_scope` parent link) that would otherwise dangle after
/// the base is gone; an `update` overlay is removed while the real
/// (pre-inference) value is left untouched.
pub(crate) async fn reject_interpretation(
    perspective: &mut PerspectiveInstance,
    base: &str,
    property: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let _ = context;
    let all = links_from(perspective, base).await?;
    let raw_kind = match first_target(&all, OVERLAY_KIND_PRED) {
        Some(k) => k,
        None => anyhow::bail!("reject_interpretation: no overlay on `{base}`"),
    };
    // `write_overlay` uses `create_subject`'s setter path, which encodes a
    // non-URI string target as `literal:string:<value>`. Compare against the
    // decoded value or a whole-base reject of a `create` silently takes the
    // update branch and leaves the LLM-authored instance orphaned in the
    // graph (CodeRabbit #881 review). `parse_literal_value` also handles the
    // constructor's plain-string target (`"create"`) — that branch returns
    // `Value::String("create")` unchanged.
    let kind = match parse_literal_value(&raw_kind) {
        serde_json::Value::String(s) => s,
        other => other.to_string(),
    };

    if let Some(_prop) = property {
        // Drop just this suggestion; the real value stays as it is.
        remove(perspective, inferred_links(&all, property)).await?;
        return prune_overlay_shell_if_empty(perspective, base).await;
    }

    // Whole-base reject.
    if kind == "create" {
        // The LLM authored the whole instance — discard it, its overlay, AND
        // every inbound link that points at it (e.g. the `mint_scope` parent
        // link an AutoProcessor writes). Without the inbound sweep, a
        // parent-scope UI querying "children of X" would keep seeing a link
        // to a base whose scalars have all been deleted (CodeRabbit #881
        // review).
        remove(perspective, all.iter().collect()).await?;
        let inbound = perspective
            .get_links(&LinkQuery {
                target: Some(base.to_string()),
                ..Default::default()
            })
            .await?;
        if !inbound.is_empty() {
            remove(perspective, inbound.iter().collect()).await?;
        }
        Ok(())
    } else {
        // Update: drop only the overlay (kind + run + all inferred), keep the
        // real value the human/prior state already holds.
        let shell = all
            .iter()
            .filter(|l| is_overlay_link(l))
            .collect::<Vec<_>>();
        remove(perspective, shell).await
    }
}

// ── helpers ────────────────────────────────────────────────────────────────

async fn links_from(
    perspective: &PerspectiveInstance,
    base: &str,
) -> anyhow::Result<Vec<DecoratedLinkExpression>> {
    Ok(perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await?)
}

fn first_target(links: &[DecoratedLinkExpression], predicate: &str) -> Option<String> {
    links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some(predicate))
        .map(|l| l.data.target.clone())
}

/// The `inferred/<p>` links on the base, optionally narrowed to one real
/// predicate `property`.
fn inferred_links<'a>(
    links: &'a [DecoratedLinkExpression],
    property: Option<&str>,
) -> Vec<&'a DecoratedLinkExpression> {
    links
        .iter()
        .filter(|l| {
            let Some(pred) = l.data.predicate.as_deref() else {
                return false;
            };
            let Some(real) = pred.strip_prefix(INFERRED_PREFIX) else {
                return false;
            };
            property.is_none_or(|p| p == real)
        })
        .collect()
}

/// True for a link that is part of the overlay itself (its `kind`/`run`
/// discriminators or an `inferred/<p>` snapshot) rather than real instance data.
fn is_overlay_link(l: &DecoratedLinkExpression) -> bool {
    match l.data.predicate.as_deref() {
        Some(p) => {
            p == OVERLAY_KIND_PRED || p == OVERLAY_RUN_PRED || p.starts_with(INFERRED_PREFIX)
        }
        None => false,
    }
}

async fn remove(
    perspective: &mut PerspectiveInstance,
    links: Vec<&DecoratedLinkExpression>,
) -> anyhow::Result<()> {
    if links.is_empty() {
        return Ok(());
    }
    let exprs: Vec<LinkExpression> = links.into_iter().cloned().map(Into::into).collect();
    perspective.remove_links(exprs, None).await?;
    Ok(())
}

/// Once no `inferred/<p>` suggestion is left on the base, remove the overlay's
/// `kind`/`run` shell too — no overlay means the §4 gate treats the base as
/// human-owned.
async fn prune_overlay_shell_if_empty(
    perspective: &mut PerspectiveInstance,
    base: &str,
) -> anyhow::Result<()> {
    let all = links_from(perspective, base).await?;
    let any_inferred = all.iter().any(|l| {
        l.data
            .predicate
            .as_deref()
            .is_some_and(|p| p.starts_with(INFERRED_PREFIX))
    });
    if any_inferred {
        return Ok(());
    }
    let shell = all
        .iter()
        .filter(|l| {
            matches!(
                l.data.predicate.as_deref(),
                Some(OVERLAY_KIND_PRED) | Some(OVERLAY_RUN_PRED)
            )
        })
        .collect::<Vec<_>>();
    remove(perspective, shell).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::types::{Link, LinkStatus};

    async fn add(
        p: &mut PerspectiveInstance,
        base: &str,
        pred: &str,
        target: &str,
        ctx: &AgentContext,
    ) {
        p.add_links(
            vec![Link {
                source: base.into(),
                predicate: Some(pred.into()),
                target: target.into(),
            }],
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("add_links");
    }

    async fn preds_on(p: &PerspectiveInstance, base: &str) -> Vec<String> {
        p.get_links(&LinkQuery {
            source: Some(base.into()),
            ..Default::default()
        })
        .await
        .unwrap()
        .into_iter()
        .filter_map(|l| l.data.predicate)
        .collect()
    }

    /// A create-overlay: one real value plus a staged `inferred/<p>` snapshot.
    async fn seed_create(
        p: &mut PerspectiveInstance,
        base: &str,
        real_pred: &str,
        val: &str,
        ctx: &AgentContext,
    ) {
        add(p, base, real_pred, val, ctx).await;
        add(p, base, OVERLAY_KIND_PRED, "create", ctx).await;
        add(p, base, OVERLAY_RUN_PRED, "ad4m://interp/run/r1", ctx).await;
        add(p, base, &format!("{INFERRED_PREFIX}{real_pred}"), val, ctx).await;
    }

    #[tokio::test]
    async fn accept_deletes_overlay_keeps_real_value() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/1";
        seed_create(&mut p, base, "soa://title", "literal:string:Fix", &ctx).await;
        assert!(overlay_of(&p, base).await.unwrap().is_some());

        accept_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            overlay_of(&p, base).await.unwrap().is_none(),
            "overlay deleted"
        );
        let preds = preds_on(&p, base).await;
        assert!(
            preds.contains(&"soa://title".to_string()),
            "real value kept"
        );
        assert!(
            !preds.iter().any(|x| x.starts_with(INFERRED_PREFIX)
                || x == OVERLAY_KIND_PRED
                || x == OVERLAY_RUN_PRED),
            "no overlay links remain"
        );
    }

    #[tokio::test]
    async fn reject_create_deletes_whole_instance() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/2";
        seed_create(&mut p, base, "soa://title", "literal:string:Drop", &ctx).await;

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            preds_on(&p, base).await.is_empty(),
            "suggested instance fully removed"
        );
    }

    /// Production kind values are literal-encoded by the SDNA setter path
    /// (`write_overlay` → `create_subject` → `setSingleTarget` on a
    /// non-URI string). A whole-base reject must normalise the target
    /// before comparing to `"create"`, or it silently takes the update
    /// branch and leaves the LLM-authored instance orphaned — CodeRabbit
    /// #881 review. Also verifies inbound-link sweep so an AutoProcessor
    /// `mint_scope` parent link doesn't dangle after the base is gone.
    #[tokio::test]
    async fn reject_create_normalises_literal_kind_and_sweeps_inbound_links() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Task/lit";
        add(&mut p, base, "soa://title", "literal:string:Ship", &ctx).await;
        // Production-shaped `kind`: literal-encoded, exactly what
        // `write_overlay` writes via the SDNA setter.
        add(
            &mut p,
            base,
            OVERLAY_KIND_PRED,
            "literal:string:create",
            &ctx,
        )
        .await;
        add(
            &mut p,
            base,
            OVERLAY_RUN_PRED,
            "ad4m://interp/run/rlit",
            &ctx,
        )
        .await;
        add(
            &mut p,
            base,
            &format!("{INFERRED_PREFIX}soa://title"),
            "literal:string:Ship",
            &ctx,
        )
        .await;
        // Simulate an inbound `mint_scope` parent link the auto-processor
        // would have written.
        let parent = "soa://project/lit";
        let contains_pred = "soa://contains";
        p.add_link(
            crate::types::Link {
                source: parent.into(),
                predicate: Some(contains_pred.into()),
                target: base.into(),
            },
            crate::types::LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        assert!(
            preds_on(&p, base).await.is_empty(),
            "literal-encoded kind still routes to the create branch: \
             suggested instance fully removed"
        );
        let inbound_after = p
            .get_links(&LinkQuery {
                target: Some(base.into()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert!(
            inbound_after.is_empty(),
            "inbound links to the rejected base are also swept, no dangling \
             parent references"
        );
    }

    #[tokio::test]
    async fn reject_update_keeps_real_drops_overlay() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let base = "soa://ext/Sub/3";
        add(&mut p, base, "soa://summary", "literal:string:Human", &ctx).await;
        add(&mut p, base, OVERLAY_KIND_PRED, "update", &ctx).await;
        add(&mut p, base, OVERLAY_RUN_PRED, "ad4m://interp/run/r2", &ctx).await;
        add(
            &mut p,
            base,
            &format!("{INFERRED_PREFIX}soa://summary"),
            "literal:string:LLM",
            &ctx,
        )
        .await;

        reject_interpretation(&mut p, base, None, &ctx)
            .await
            .unwrap();

        let preds = preds_on(&p, base).await;
        assert!(
            preds.contains(&"soa://summary".to_string()),
            "human value kept"
        );
        assert!(
            overlay_of(&p, base).await.unwrap().is_none(),
            "overlay gone"
        );
    }

    // ── list_overlays: single query vs the old per-base algorithm ──────────

    /// The previous `list_overlays`, kept as the parity and timing reference:
    /// one `get_links` for the `kind` links, then `overlay_of` (a `get_links`
    /// of every link of the base) once per base.
    async fn list_overlays_per_base(p: &PerspectiveInstance) -> Vec<OverlayView> {
        let kind_links = p
            .get_links(&LinkQuery {
                predicate: Some(OVERLAY_KIND_PRED.to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        let mut bases: Vec<String> = kind_links.into_iter().map(|l| l.data.source).collect();
        bases.sort();
        bases.dedup();
        let mut out = Vec::new();
        for base in bases {
            if let Some(v) = overlay_of(p, &base).await.unwrap() {
                out.push(v);
            }
        }
        out
    }

    /// Write a link straight into the store with a chosen author and
    /// timestamp, to force timestamp ties and out-of-order inserts.
    fn put(p: &PerspectiveInstance, base: &str, pred: &str, target: &str, author: &str, ts: &str) {
        p.sparql_store
            .add_link(&LinkExpression {
                author: author.into(),
                timestamp: ts.into(),
                data: Link {
                    source: base.into(),
                    predicate: Some(pred.into()),
                    target: target.into(),
                },
                proof: crate::types::ExpressionProof {
                    key: "k".into(),
                    signature: "s".into(),
                },
                status: Some(LinkStatus::Shared),
            })
            .unwrap();
    }

    fn json(v: &[OverlayView]) -> String {
        serde_json::to_string(v).unwrap()
    }

    fn inferred_of<'a>(v: &'a [OverlayView], base: &str) -> Vec<&'a str> {
        v.iter()
            .find(|o| o.base == base)
            .unwrap()
            .inferred
            .iter()
            .map(|(p, _)| p.as_str())
            .collect()
    }

    #[tokio::test]
    async fn list_overlays_matches_per_base_reference() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let inf = |real: &str| format!("{INFERRED_PREFIX}{real}");

        // A: create overlay, three inferred props with different literal
        // types, plus real values and unrelated links on the same base.
        let a = "soa://ext/Task/a";
        add(&mut p, a, "soa://title", "literal:string:Fix", &ctx).await;
        add(&mut p, a, "soa://tag", "soa://tag/urgent", &ctx).await;
        add(&mut p, a, OVERLAY_KIND_PRED, "literal:string:create", &ctx).await;
        add(&mut p, a, OVERLAY_RUN_PRED, "ad4m://interp/run/r1", &ctx).await;
        add(&mut p, a, &inf("soa://title"), "literal:string:Fix", &ctx).await;
        add(&mut p, a, &inf("soa://done"), "literal:boolean:true", &ctx).await;
        add(&mut p, a, &inf("soa://points"), "literal:number:3", &ctx).await;
        // Look-alike predicates in the interp namespace that are NOT overlay
        // links: they must not become `run` or an `inferred` entry.
        add(&mut p, a, "ad4m://interp/running", "soa://x", &ctx).await;
        add(&mut p, a, "ad4m://interp/inferred", "soa://x", &ctx).await;

        // B: update overlay without a run; its inferred links share one
        // timestamp and were inserted newest-first.
        let b = "soa://ext/Task/b";
        add(&mut p, b, "soa://summary", "literal:string:Human", &ctx).await;
        let ts = "2030-01-01T00:00:00.000Z";
        put(
            &p,
            b,
            &inf("soa://summary"),
            "literal:string:LLM",
            "did:key:zA",
            "2030-01-01T00:00:01.000Z",
        );
        put(&p, b, OVERLAY_KIND_PRED, "update", "did:key:zA", ts);
        put(
            &p,
            b,
            &inf("soa://zeta"),
            "literal:string:z",
            "did:key:zA",
            ts,
        );
        put(
            &p,
            b,
            &inf("soa://alpha"),
            "literal:string:a",
            "did:key:zA",
            ts,
        );
        put(
            &p,
            b,
            &inf("soa://mid"),
            "literal:string:m",
            "did:key:zA",
            ts,
        );

        // C: two agents wrote different `kind` and `run` values at the same
        // time (peer sync), and one of them wrote the same inferred value.
        let c = "soa://ext/Task/c";
        put(&p, c, OVERLAY_KIND_PRED, "update", "did:key:zB", ts);
        put(&p, c, OVERLAY_KIND_PRED, "create", "did:key:zA", ts);
        put(
            &p,
            c,
            OVERLAY_RUN_PRED,
            "ad4m://interp/run/r9",
            "did:key:zB",
            ts,
        );
        put(
            &p,
            c,
            OVERLAY_RUN_PRED,
            "ad4m://interp/run/r3",
            "did:key:zA",
            ts,
        );
        put(
            &p,
            c,
            &inf("soa://title"),
            "literal:string:Same",
            "did:key:zA",
            ts,
        );
        put(
            &p,
            c,
            &inf("soa://title"),
            "literal:string:Same",
            "did:key:zB",
            ts,
        );

        // D: inferred links but no `kind` — not an overlay.
        let d = "soa://ext/Task/d";
        add(
            &mut p,
            d,
            &inf("soa://title"),
            "literal:string:Orphan",
            &ctx,
        )
        .await;
        add(&mut p, d, OVERLAY_RUN_PRED, "ad4m://interp/run/r1", &ctx).await;

        // Other nodes: the run node's own metadata, links that point AT an
        // overlay base, and a `kind` IRI used as a target.
        add(
            &mut p,
            "ad4m://interp/run/r1",
            "ad4m://interp/model",
            "literal:string:m",
            &ctx,
        )
        .await;
        add(&mut p, "soa://ext/Project/1", "soa://has_task", a, &ctx).await;
        add(
            &mut p,
            "soa://ext/Other/1",
            "soa://refers",
            OVERLAY_KIND_PRED,
            &ctx,
        )
        .await;
        add(
            &mut p,
            "soa://ext/Other/1",
            &inf("soa://title"),
            "literal:string:X",
            &ctx,
        )
        .await;

        // E: a `new_unchecked` base that is not a parseable IRI (Flux's
        // `literal://string:` ids), read through the `STR()` fallback.
        let e = "literal://string:taskE";
        add(&mut p, e, OVERLAY_KIND_PRED, "literal:string:create", &ctx).await;
        add(&mut p, e, &inf("soa://title"), "literal:string:E", &ctx).await;
        add(&mut p, e, "soa://title", "literal:string:E", &ctx).await;
        assert!(!emittable_iri(e), "E must take the STR() path");

        // F: eight inferred links written in an order unrelated to their
        // timestamps, and a second, later `kind` written last. Rows come out
        // of the store in neither timestamp nor predicate order, so both the
        // inferred order and which `kind` wins depend on the per-base sort.
        let f = "soa://ext/Task/f";
        put(&p, f, OVERLAY_KIND_PRED, "update", "did:key:zA", ts);
        let second = [3, 7, 0, 5, 1, 6, 2, 4];
        for (i, sec) in second.iter().enumerate() {
            put(
                &p,
                f,
                &inf(&format!("soa://f{i}")),
                "literal:string:x",
                "did:key:zA",
                &format!("2030-01-01T00:00:0{}.000Z", sec + 1),
            );
        }
        put(
            &p,
            f,
            OVERLAY_KIND_PRED,
            "create",
            "did:key:zA",
            "2030-01-01T00:00:10.000Z",
        );

        let reference = list_overlays_per_base(&p).await;
        let new = list_overlays(&p).await.unwrap();
        assert_eq!(
            json(&new),
            json(&reference),
            "same output as the per-base algorithm"
        );

        // Hand-checked expectations, so the test does not rest on the
        // reference alone.
        let bases: Vec<&str> = new.iter().map(|o| o.base.as_str()).collect();
        assert_eq!(
            bases,
            vec![e, a, b, c, f],
            "sorted, deduplicated, no kind-less base"
        );
        // `add` stamps milliseconds, so A's links may tie: check A as a set.
        let mut ia = inferred_of(&new, a);
        ia.sort();
        assert_eq!(ia, vec!["soa://done", "soa://points", "soa://title"]);
        let va = new.iter().find(|o| o.base == a).unwrap();
        assert_eq!(va.run.as_deref(), Some("ad4m://interp/run/r1"));
        let value = |pred: &str| {
            va.inferred
                .iter()
                .find(|(p, _)| p == pred)
                .unwrap()
                .1
                .clone()
        };
        assert_eq!(value("soa://done"), serde_json::json!(true));
        assert_eq!(value("soa://points"), serde_json::json!(3));
        let vb = new.iter().find(|o| o.base == b).unwrap();
        assert_eq!(vb.kind, "update");
        assert_eq!(vb.run, None);
        assert_eq!(inferred_of(&new, b).len(), 4);
        assert_eq!(
            *inferred_of(&new, b).last().unwrap(),
            "soa://summary",
            "newest last"
        );
        assert_eq!(
            inferred_of(&new, c),
            vec!["soa://title", "soa://title"],
            "one row per author"
        );
        assert_eq!(inferred_of(&new, e), vec!["soa://title"]);
        assert_eq!(
            inferred_of(&new, f),
            vec![
                "soa://f2", "soa://f4", "soa://f6", "soa://f0", "soa://f7", "soa://f3", "soa://f5",
                "soa://f1",
            ],
            "oldest first, as get_links sorts"
        );
        let vf = new.iter().find(|o| o.base == f).unwrap();
        assert_eq!(vf.kind, "update", "the oldest `kind` wins");
    }

    /// `n` overlay bases, each with `extra` normal links, a `kind`, a `run`
    /// and three `inferred/<p>` links.
    async fn seed_overlays(
        p: &mut PerspectiveInstance,
        ctx: &AgentContext,
        n: usize,
        extra: usize,
    ) {
        let mut links = Vec::new();
        for i in 0..n {
            let base = format!("soa://ext/Scale/{i}");
            let mut l = |pred: String, target: String| {
                links.push(Link {
                    source: base.clone(),
                    predicate: Some(pred),
                    target,
                })
            };
            for f in 0..extra {
                l(
                    format!("soa://field{f}"),
                    format!("literal:string:v{i}_{f}"),
                );
            }
            l(OVERLAY_KIND_PRED.into(), "literal:string:create".into());
            l(OVERLAY_RUN_PRED.into(), "ad4m://interp/run/r1".into());
            for f in 0..3 {
                l(
                    format!("{INFERRED_PREFIX}soa://field{f}"),
                    format!("literal:string:v{i}_{f}"),
                );
            }
        }
        p.add_links(links, LinkStatus::Local, None, ctx)
            .await
            .unwrap();
    }

    /// Check that both algorithms give the same `n` views, then return the
    /// mean wall time of each in ms, as `(per-base, batched)`.
    async fn time_both(p: &PerspectiveInstance, n: usize, reps: u32) -> (f64, f64) {
        // The first calls also warm the store's caches for both paths.
        let old = list_overlays_per_base(p).await;
        let new = list_overlays(p).await.unwrap();
        assert_eq!(
            json(&new),
            json(&old),
            "same output as the per-base algorithm"
        );
        assert_eq!(new.len(), n);
        let t = std::time::Instant::now();
        for _ in 0..reps {
            list_overlays_per_base(p).await;
        }
        let old_ms = t.elapsed().as_secs_f64() * 1000.0 / f64::from(reps);
        let t = std::time::Instant::now();
        for _ in 0..reps {
            list_overlays(p).await.unwrap();
        }
        let new_ms = t.elapsed().as_secs_f64() * 1000.0 / f64::from(reps);
        (old_ms, new_ms)
    }

    /// #1017: with 300 pending overlays, each on a base with 50 normal links,
    /// the list must equal the per-base result and take at most half the
    /// time. Runs on RocksDB, the store a running executor uses. Measured
    /// locally at 6.1x (see `list_overlays_scaling`). The per-base cost grows
    /// with base size and the batched one does not, so 50 links leave room
    /// for a loaded CI machine; the 2x bound still fails when `list_overlays`
    /// goes back to one read per base (both sides then take the same time).
    #[tokio::test]
    async fn list_overlays_300_overlays_same_result_and_faster() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let dir = tempfile::tempdir().unwrap();
        p.sparql_store = std::sync::Arc::new(
            crate::perspectives::sparql_store::SparqlStore::new(Some(dir.path().to_str().unwrap()))
                .unwrap(),
        );
        seed_overlays(&mut p, &ctx, 300, 50).await;
        let (old_ms, new_ms) = time_both(&p, 300, 3).await;
        println!("300 overlays, RocksDB: per-base {old_ms:.1} ms, batched {new_ms:.1} ms");
        assert!(
            new_ms * 2.0 < old_ms,
            "batched list_overlays ({new_ms:.1} ms) is not at least 2x faster than per-base ({old_ms:.1} ms)"
        );
    }

    /// Blocking review item on #1125: an overlay on a base that is not an
    /// emittable IRI (Flux's `literal://string:` ids) must not make
    /// `list_overlays` scan the store. SPARQL can match such a base only
    /// through `FILTER(STR(?source) IN …)`, which leaves the triple pattern
    /// unbound and walks every quad; `list_overlays` reads it with one
    /// `get_links` seek instead. Here: one such overlay among 20k unrelated
    /// links on RocksDB. The reference is the scan form the first version of
    /// #1125 used. Measured locally: scan 111 ms, `list_overlays` 0.18 ms;
    /// with the scan back in `list_overlays` both take ~50 ms and the 10x
    /// bound fails.
    #[tokio::test]
    async fn list_overlays_non_iri_base_does_not_scan() {
        let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
        let dir = tempfile::tempdir().unwrap();
        p.sparql_store = std::sync::Arc::new(
            crate::perspectives::sparql_store::SparqlStore::new(Some(dir.path().to_str().unwrap()))
                .unwrap(),
        );
        let mut links = Vec::new();
        for i in 0..2000 {
            for f in 0..10 {
                links.push(Link {
                    source: format!("soa://ext/Other/{i}"),
                    predicate: Some(format!("soa://field{f}")),
                    target: format!("literal:string:v{i}_{f}"),
                });
            }
        }
        let flux = "literal://string:fluxMessage1";
        assert!(!emittable_iri(flux));
        for (pred, target) in [
            (OVERLAY_KIND_PRED.to_string(), "literal:string:update"),
            (OVERLAY_RUN_PRED.to_string(), "ad4m://interp/run/r1"),
            (format!("{INFERRED_PREFIX}soa://title"), "literal:string:Hi"),
            ("soa://body".to_string(), "literal:string:hello"),
        ] {
            links.push(Link {
                source: flux.into(),
                predicate: Some(pred),
                target: target.into(),
            });
        }
        p.add_links(links, LinkStatus::Local, None, &ctx)
            .await
            .unwrap();

        let new = list_overlays(&p).await.unwrap();
        assert_eq!(json(&new), json(&list_overlays_per_base(&p).await));
        assert_eq!(new.len(), 1);
        assert_eq!(inferred_of(&new, flux), vec!["soa://title"]);

        // The scan the non-IRI group used to run.
        let scan = decorated_links_query(
            "",
            &format!(
                r#"STR(?source) IN ("{flux}") && (?predicate = <{OVERLAY_KIND_PRED}> || ?predicate = <{OVERLAY_RUN_PRED}> || STRSTARTS(STR(?predicate), "{INFERRED_PREFIX}"))"#
            ),
        );
        assert_eq!(
            p.sparql_store.query_decorated_links(&scan).unwrap().len(),
            3
        );

        let reps = 5;
        let t = std::time::Instant::now();
        for _ in 0..reps {
            p.sparql_store.query_decorated_links(&scan).unwrap();
        }
        let scan_ms = t.elapsed().as_secs_f64() * 1000.0 / f64::from(reps);
        let t = std::time::Instant::now();
        for _ in 0..reps {
            list_overlays(&p).await.unwrap();
        }
        let new_ms = t.elapsed().as_secs_f64() * 1000.0 / f64::from(reps);
        println!(
            "1 non-IRI overlay + 20k links, RocksDB: STR() scan {scan_ms:.2} ms, list_overlays {new_ms:.2} ms"
        );
        assert!(
            new_ms * 10.0 < scan_ms,
            "list_overlays ({new_ms:.2} ms) is not 10x faster than the STR() scan ({scan_ms:.2} ms)"
        );
    }

    /// The timing table for the PR: both algorithms on the in-memory store
    /// and on RocksDB (what a running executor uses). Not run in CI, as wall
    /// time is machine-dependent; run with
    /// `cargo test --release -p ad4m-executor --lib list_overlays_scaling -- --ignored --nocapture`.
    #[tokio::test]
    #[ignore]
    async fn list_overlays_scaling() {
        use crate::perspectives::sparql_store::SparqlStore;
        // (overlays, normal links per base)
        let cases = [
            (100usize, 8usize),
            (300, 8),
            (1000, 8),
            (300, 20),
            (300, 50),
        ];
        for rocks in [false, true] {
            for (n, extra) in cases {
                let (mut p, _s, ctx) = setup_perspective_no_llm(&[]).await;
                let dir = tempfile::tempdir().unwrap();
                if rocks {
                    p.sparql_store = std::sync::Arc::new(
                        SparqlStore::new(Some(dir.path().to_str().unwrap())).unwrap(),
                    );
                }
                seed_overlays(&mut p, &ctx, n, extra).await;
                let (old_ms, new_ms) = time_both(&p, n, 5).await;
                println!(
                    "SCALING store={} overlays={n} normal_links_per_base={extra}: per-base {old_ms:.1} ms, batched {new_ms:.1} ms",
                    if rocks { "rocksdb" } else { "memory" },
                );
            }
        }
    }
}
