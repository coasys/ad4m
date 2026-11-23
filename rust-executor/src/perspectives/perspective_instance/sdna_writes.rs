//! The `add_sdna` / `add_sdna_batch` write path: name-keyed SDNA
//! registration with the `ad4m://has_subject_class` / `ad4m://sdna`
//! bookkeeping links.
//!
//! `perspective.registerShapes` is the SDK's registration path and names a
//! class by the basename of its `sh:targetClass` IRI. Mounts need the
//! name the language reports from `mountShapes()` instead (the mount
//! contract separates the query name from the class IRI on purpose), and
//! `mount_classes`' ownership checks read the `has_subject_class` name
//! links, so this file keeps the earlier writer for them.

use super::PerspectiveInstance;
use crate::agent::AgentContext;
use crate::perspectives::shacl_parser::{
    check_consensus_rule_links, parse_flow_to_links, parse_shacl_to_links,
};
use crate::types::{Link, LinkExpression, LinkQuery, LinkStatus};
use ad4m_client::literal::Literal;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum SdnaType {
    SubjectClass,
    Flow,
    Custom,
}

impl SdnaType {
    pub fn from_string(s: &str) -> Result<Self, AnyError> {
        match s {
            "subject_class" => Ok(SdnaType::SubjectClass),
            "flow" => Ok(SdnaType::Flow),
            "custom" => Ok(SdnaType::Custom),
            _ => Err(anyhow!(
                "Invalid SDNA type: {}. Must one of 'subject_class', 'flow' or 'custom'.",
                s
            )),
        }
    }
}

/// Parse the `shacl_json` handed to `add_sdna` / `add_sdna_batch` into links.
/// A flow definition (`SHACLFlow`: states, transitions, no `target_class`)
/// goes through the flow parser; everything else is a class shape (#1650).
/// A flow whose `consensusRule` could not be read back is refused here, before
/// `add_sdna` / `add_sdna_batch` write anything (#1144).
fn parse_sdna_json_to_links(
    sdna_type: &SdnaType,
    json: &str,
    name: &str,
) -> Result<Vec<Link>, AnyError> {
    match sdna_type {
        SdnaType::Flow => {
            let links = parse_flow_to_links(json, name)?;
            check_consensus_rule_links(&links)?;
            Ok(links)
        }
        SdnaType::SubjectClass | SdnaType::Custom => parse_shacl_to_links(json, name),
    }
}

/// An SDNA entry with its SHACL parsed: name, code, type, links.
type ParsedSdna = (String, String, SdnaType, Option<Vec<Link>>);

/// Parse every entry before writing any of them, so one invalid shape
/// refuses the whole batch instead of leaving it half-applied.
fn parse_sdna_entries(
    entries: Vec<(String, String, SdnaType, Option<String>)>,
) -> Result<Vec<ParsedSdna>, AnyError> {
    entries
        .into_iter()
        .map(|(name, sdna_code, sdna_type, shacl_json)| {
            let shacl_links = shacl_json
                .as_deref()
                .map(|shacl| parse_sdna_json_to_links(&sdna_type, shacl, &name))
                .transpose()?;
            Ok((name, sdna_code, sdna_type, shacl_links))
        })
        .collect()
}

impl PerspectiveInstance {
    /// Adds the given Social DNA code to the perspective's SDNA code
    /// If shacl_json is provided, also stores SHACL as queryable RDF links
    pub async fn add_sdna(
        &mut self,
        name: String,
        sdna_code: String,
        sdna_type: SdnaType,
        shacl_json: Option<String>,
        context: &AgentContext,
    ) -> Result<bool, AnyError> {
        let is_flow = matches!(sdna_type, SdnaType::Flow);
        // Validate before touching the store: see `add_sdna_inner`.
        let shacl_links = shacl_json
            .as_deref()
            .map(|shacl| parse_sdna_json_to_links(&sdna_type, shacl, &name))
            .transpose()?;
        let result = {
            let mutex = self.sdna_change_mutex.clone();
            let _guard = mutex.lock().await;
            self.add_sdna_inner(name, sdna_code, sdna_type, shacl_links, context)
                .await?
        };

        // A flow definition entering the perspective means callers will
        // immediately try to read FlowInstance / FlowTransitionProposal rows.
        // Register those two hard-wired runtime classes now, so `findAll`
        // answers `[]` on a fresh perspective instead of "no SHACL shape
        // stored" (#1007).
        //
        // This MUST be outside the `sdna_change_mutex` guard above, and the
        // explicit block is what releases it. `ensure_flow_model_classes`
        // reaches `register_shapes`, which takes the same mutex — and it is
        // a plain non-reentrant `tokio::sync::Mutex`, so re-entering it while
        // held deadlocks.
        //
        // `Box::pin` is still required here: the call is still recursive in
        // *type* terms even though it no longer re-enters the lock.
        if is_flow {
            Box::pin(crate::perspectives::flow_classes::ensure_flow_model_classes(self, context))
                .await
                .map_err(|e| anyhow!("ensure_flow_model_classes: {e:#}"))?;
        }

        Ok(result)
    }

    /// Remove every SHACL link associated with the given target-class URIs.
    /// Called when refreshing an existing `SubjectClass` so old property
    /// triples are not left behind to compete with the fresh shape.
    ///
    /// Walks the link graph produced by [`parse_shacl_to_links`]:
    ///   target_class_uri
    ///     ── rdf://type      ──▶ ad4m://SubjectClass
    ///     ── ad4m://shape    ──▶ shape_uri
    ///   shape_uri
    ///     ── sh://property   ──▶ prop_shape_uri  (one per declared property)
    ///     ── *other shape-level predicates*
    ///   prop_shape_uri
    ///     ── *property-level predicates*
    async fn remove_subject_class_shacl_links(
        &mut self,
        target_class_uris: &[String],
    ) -> Result<(), AnyError> {
        let mut to_remove: Vec<LinkExpression> = Vec::new();

        for target_class_uri in target_class_uris {
            // 1. target_class_uri → rdf://type → ad4m://SubjectClass
            //    and target_class_uri → ad4m://shape → shape_uri
            let target_class_links = self.get_links_local(&LinkQuery {
                source: Some(target_class_uri.clone()),
                ..Default::default()
            })?;

            let shape_uris: Vec<String> = target_class_links
                .iter()
                .filter(|(link, _)| link.data.predicate.as_deref() == Some("ad4m://shape"))
                .map(|(link, _)| link.data.target.clone())
                .collect();

            to_remove.extend(
                target_class_links
                    .into_iter()
                    .filter(|(link, _)| {
                        matches!(
                            link.data.predicate.as_deref(),
                            Some("rdf://type") | Some("ad4m://shape")
                        )
                    })
                    .map(|(link, _)| link),
            );

            for shape_uri in &shape_uris {
                let shape_links = self.get_links_local(&LinkQuery {
                    source: Some(shape_uri.clone()),
                    ..Default::default()
                })?;

                let prop_shape_uris: Vec<String> = shape_links
                    .iter()
                    .filter(|(link, _)| link.data.predicate.as_deref() == Some("sh://property"))
                    .map(|(link, _)| link.data.target.clone())
                    .collect();

                to_remove.extend(shape_links.into_iter().map(|(link, _)| link));

                for prop_shape_uri in prop_shape_uris {
                    let prop_links = self.get_links_local(&LinkQuery {
                        source: Some(prop_shape_uri),
                        ..Default::default()
                    })?;
                    to_remove.extend(prop_links.into_iter().map(|(link, _)| link));
                }
            }
        }

        if !to_remove.is_empty() {
            self.remove_links(to_remove, None).await?;
        }

        Ok(())
    }

    /// Batch variant: registers multiple SDNA entries under a single mutex acquisition.
    pub async fn add_sdna_batch(
        &mut self,
        entries: Vec<(String, String, SdnaType, Option<String>)>,
        context: &AgentContext,
    ) -> Result<Vec<bool>, AnyError> {
        let any_flow = entries
            .iter()
            .any(|(_, _, sdna_type, _)| matches!(sdna_type, SdnaType::Flow));

        let entries = parse_sdna_entries(entries)?;
        let results = {
            let mutex = self.sdna_change_mutex.clone();
            let _guard = mutex.lock().await;
            self.add_parsed_sdna(entries, context).await?
        };

        // Same rule as `add_sdna`: register the flow-runtime classes only after
        // the guard is released, because doing so re-enters the shape
        // registration lock. Once per batch rather than per entry — the
        // registration is idempotent, so the extra calls would be harmless,
        // just wasted writes.
        if any_flow {
            Box::pin(crate::perspectives::flow_classes::ensure_flow_model_classes(self, context))
                .await
                .map_err(|e| anyhow!("ensure_flow_model_classes: {e:#}"))?;
        }

        Ok(results)
    }

    /// `add_sdna_batch` for a caller that already holds `sdna_change_mutex`
    /// (a mount checks class ownership, claims and installs under one
    /// hold). A flow entry is refused: registering the flow-runtime classes
    /// re-enters the mutex.
    pub(super) async fn add_sdna_batch_locked(
        &mut self,
        entries: Vec<(String, String, SdnaType, Option<String>)>,
        context: &AgentContext,
    ) -> Result<Vec<bool>, AnyError> {
        if entries
            .iter()
            .any(|(_, _, sdna_type, _)| matches!(sdna_type, SdnaType::Flow))
        {
            return Err(anyhow!(
                "a flow cannot be registered under a held SDNA lock: use add_sdna_batch"
            ));
        }
        let entries = parse_sdna_entries(entries)?;
        self.add_parsed_sdna(entries, context).await
    }

    /// Write parsed SDNA entries, one `add_sdna_inner` each. The caller
    /// holds `sdna_change_mutex`.
    async fn add_parsed_sdna(
        &mut self,
        entries: Vec<ParsedSdna>,
        context: &AgentContext,
    ) -> Result<Vec<bool>, AnyError> {
        let mut results = Vec::with_capacity(entries.len());
        for (name, sdna_code, sdna_type, shacl_links) in entries {
            let result = self
                .add_sdna_inner(name, sdna_code, sdna_type, shacl_links, context)
                .await?;
            results.push(result);
        }
        Ok(results)
    }

    /// Inner implementation of add_sdna without mutex acquisition.
    ///
    /// Takes the SHACL already parsed: the purge of an existing class's
    /// SHACL and the `has_subject_class` / `sdna` writes below are shared
    /// links, so a shape that `parse_shacl_to_links` refuses must be refused
    /// before any of them run. Otherwise a rejected re-registration deletes
    /// the class for every peer (#1348).
    async fn add_sdna_inner(
        &mut self,
        name: String,
        mut sdna_code: String,
        sdna_type: SdnaType,
        shacl_links: Option<Vec<Link>>,
        context: &AgentContext,
    ) -> Result<bool, AnyError> {
        let predicate = match sdna_type {
            SdnaType::SubjectClass => "ad4m://has_subject_class",
            SdnaType::Flow => "ad4m://has_flow",
            SdnaType::Custom => "ad4m://has_custom_sdna",
        };

        let literal_name = Literal::from_string(name.clone())
            .to_url()
            .expect("just initialized Literal couldn't be turned into URL");

        let mut sdna_links: Vec<Link> = Vec::new();

        // For SubjectClass refresh: if a class with this name already exists
        // we must purge the prior SHACL graph before writing new triples.
        // Without this, divergent old/new SHACL property triples coexist in
        // the store and the loader produces non-deterministic shapes.
        //
        // When `shacl_links` is `None` and the class already exists, we
        // preserve the historical no-op behaviour: nothing to refresh.
        if matches!(sdna_type, SdnaType::SubjectClass) {
            // Check for any existing SubjectClass with this name, regardless of namespace
            // We query by target (ad4m://SubjectClass) and then filter by class name
            let all_class_links = self.get_links_local(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })?;

            // Collect any existing class URIs that match this name
            let existing_target_class_uris: Vec<String> = all_class_links
                .iter()
                .filter_map(|(link, _)| {
                    let source = &link.data.source;
                    let matches = source
                        .split("://")
                        .last()
                        .and_then(|s| s.split('/').last())
                        .map(|class_name| class_name == name)
                        .unwrap_or(false);
                    if matches {
                        Some(source.clone())
                    } else {
                        None
                    }
                })
                .collect();

            if !existing_target_class_uris.is_empty() {
                if shacl_links.is_none() {
                    log::info!(
                        "Class '{}' SHACL definition already exists, skipping duplicate",
                        name
                    );
                    return Ok(true);
                }

                log::info!(
                    "Class '{}' already exists — refreshing SHACL definition",
                    name
                );
                self.remove_subject_class_shacl_links(&existing_target_class_uris)
                    .await?;
            }
        }

        if (Literal::from_url(sdna_code.clone())).is_err() {
            sdna_code = Literal::from_string(sdna_code)
                .to_url()
                .expect("just initialized Literal couldn't be turned into URL");
        }

        sdna_links.push(Link {
            source: "ad4m://self".to_string(),
            predicate: Some(predicate.to_string()),
            target: literal_name.clone(),
        });

        // Store the Prolog code for backward compatibility with getSdna()
        // SHACL links are the source of truth for schema operations,
        // but Prolog code is still stored for retrieval
        sdna_links.push(Link {
            source: literal_name.clone(),
            predicate: Some("ad4m://sdna".to_string()),
            target: sdna_code.clone(),
        });

        self.add_links(sdna_links, LinkStatus::Shared, None, context)
            .await?;

        // Handle SHACL links if SHACL JSON provided explicitly
        if let Some(mut shacl_links) = shacl_links {
            // The class index keys on the `sh:targetClass` node, whose
            // basename may differ from the name the class was registered
            // under (a mount name is not its target class's basename). Give
            // the name its own class node pointing at the same shape — the
            // alias `find_class` resolves the registered name by. A mount's
            // shapes get theirs here too.
            let target_class = shacl_links
                .iter()
                .find(|l| {
                    l.predicate.as_deref() == Some("rdf://type")
                        && l.target == "ad4m://SubjectClass"
                })
                .map(|l| l.source.clone());
            if let Some(target_class) = target_class {
                if crate::perspectives::shacl_parser::class_name_of(&target_class) != name {
                    if let Some(shape_uri) = shacl_links
                        .iter()
                        .find(|l| {
                            l.source == target_class
                                && l.predicate.as_deref() == Some("ad4m://shape")
                        })
                        .map(|l| l.target.clone())
                    {
                        let name_node = format!("literal:string:shacl://{name}");
                        shacl_links.push(Link {
                            source: name_node.clone(),
                            predicate: Some("rdf://type".to_string()),
                            target: "ad4m://SubjectClass".to_string(),
                        });
                        shacl_links.push(Link {
                            source: name_node,
                            predicate: Some("ad4m://shape".to_string()),
                            target: shape_uri,
                        });
                    }
                }
            }
            self.add_links(shacl_links, LinkStatus::Shared, None, context)
                .await?;
            // SHACL just changed for this class — drop any cached shape so the
            // next query re-parses against the fresh store state.
            self.clear_shape_cache();
        }

        // Flow-runtime class registration deliberately does NOT happen here:
        // this function runs while `sdna_change_mutex` is held and that
        // registration re-enters it — see `add_sdna`, which does it after
        // releasing the guard.

        Ok(true)
    }
}
