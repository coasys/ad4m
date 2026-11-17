//! Mounted languages (design §1, §2): an
//! `ad4m://self -[ad4m://mount]-> <lang>://project` link makes this
//! perspective a live view over the language's records. The mount installs
//! the language's `mountShapes()` SHACL, keeps a `Local` version link, and
//! mirrors its `mountSnapshot()` records as `Local` links, so model
//! queries, SPARQL and subscriptions see them like any other instances.
//! `sync_mount` re-reads the snapshot and diffs it against the store;
//! `apply_mount_delta` applies the same expansion to one delta (`call`
//! writes and language-emitted changes).
//!
//! Ownership: every `Local` link whose source starts with `<lang>://`
//! belongs to the mount — `unmount` deletes them all. The mount's own
//! bookkeeping (the version and class-claim links under `<lang>://project`)
//! is exempt from the snapshot sweep. The mount link itself is `Shared` so
//! peers see it (design §5.2); `perspective.publishSnapshot` is
//! unimplemented (api/perspectives_ws.rs), so no RPC ever exports the
//! `Local` mirror.

use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;
use tokio::time::sleep;

use super::{PerspectiveInstance, SdnaType};
use crate::agent::AgentContext;
use crate::languages::mount_types::{MountDelta, MountEntry, MountShapes, MountSyncReport};
use crate::languages::LanguageController;
use crate::perspectives::model_query::utils::{literal_target, parse_literal_value};
use crate::perspectives::subject_classes_of::local_name;
use crate::perspectives::{register_mount, unregister_mount};
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery, LinkStatus};

/// The mount link: `ad4m://self -[ad4m://mount]-> <lang>://project`,
/// `Shared` (design §5.2).
const MOUNT_PREDICATE: &str = "ad4m://mount";
const MOUNT_SOURCE: &str = "ad4m://self";
const MOUNT_PROJECT_PATH: &str = "project";
/// `<lang>://project -[ad4m://mount_shapes_version]-> literal:string:<v>`
/// records which `mountShapes()` version is installed (design §2.4).
const VERSION_PREDICATE: &str = "ad4m://mount_shapes_version";
/// `<lang>://project -[ad4m://mounts_class]-> literal:string:<name>` per
/// class the mount installed: the persistent record the one-mount-per-
/// class-set check consults even when the owning language is not loaded.
const CLASS_PREDICATE: &str = "ad4m://mounts_class";

/// The interval between resync passes over a perspective's mounts
/// (design §1.5: 15 s), in milliseconds. An `AtomicU64` so a test can
/// shorten it through `set_mount_resync_interval_for_test`.
static MOUNT_RESYNC_INTERVAL_MS: AtomicU64 = AtomicU64::new(15_000);

/// How long `mount_resync_loop` sleeps between passes.
fn mount_resync_interval() -> Duration {
    Duration::from_millis(MOUNT_RESYNC_INTERVAL_MS.load(Ordering::Relaxed))
}

/// Test helper: set the resync interval. Tests run serially; a test that
/// shortens it must set it back.
#[cfg(test)]
pub(crate) fn set_mount_resync_interval_for_test(interval: Duration) {
    MOUNT_RESYNC_INTERVAL_MS.store(interval.as_millis() as u64, Ordering::Relaxed);
}

/// The mount's bookkeeping lives under the language's project handle.
fn mount_target(lang: &str) -> String {
    format!("{lang}://{MOUNT_PROJECT_PATH}")
}

/// Bookkeeping predicates never belong to a record, so the snapshot sweep
/// and the per-address diff must leave them alone.
fn is_bookkeeping(predicate: Option<&str>) -> bool {
    matches!(predicate, Some(VERSION_PREDICATE) | Some(CLASS_PREDICATE))
}

impl PerspectiveInstance {
    /// Mount a language: refuse when it does not export `mountShapes()` /
    /// `mountSnapshot()` or when another mount in this perspective claims
    /// one of its class names; install the shapes; write the mount link;
    /// then run the first sync.
    pub async fn mount(&mut self, lang_address: &str) -> Result<(), AnyError> {
        if self.mounts().await?.iter().any(|l| l == lang_address) {
            return Ok(());
        }
        let controller = LanguageController::global_instance();
        let shapes = controller.mount_shapes(lang_address).await?;
        self.refuse_class_collisions(lang_address, &shapes).await?;
        self.install_mount_shapes(lang_address, &shapes).await?;
        self.add_links(
            vec![Link {
                source: MOUNT_SOURCE.to_string(),
                predicate: Some(MOUNT_PREDICATE.to_string()),
                target: mount_target(lang_address),
            }],
            LinkStatus::Shared,
            None,
            &AgentContext::main_agent(),
        )
        .await?;
        register_mount(lang_address, &self.uuid);
        self.sync_mount(lang_address).await.map(|_| ())
    }

    /// Drop the mount: every `Local` link under `<lang>://` plus the mount
    /// link itself, and the registry entry.
    pub async fn unmount(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let prefix = format!("{lang_address}://");
        let target = mount_target(lang_address);
        let doomed: Vec<LinkExpression> = self
            .get_links(&LinkQuery::default())
            .await?
            .into_iter()
            .filter(|l| {
                (l.data.source.starts_with(&prefix) && l.status == Some(LinkStatus::Local))
                    || (l.data.source == MOUNT_SOURCE
                        && l.data.predicate.as_deref() == Some(MOUNT_PREDICATE)
                        && l.data.target == target)
            })
            .map(LinkExpression::from)
            .collect();
        self.remove_links(doomed, None).await?;
        unregister_mount(lang_address, &self.uuid);
        Ok(())
    }

    /// Re-read the snapshot and diff every record against the store; a
    /// `mountShapes()` version change re-installs the shapes first
    /// (design §2.4). Addresses the snapshot no longer lists are removed.
    pub async fn sync_mount(
        &mut self,
        lang_address: &str,
    ) -> Result<MountSyncReport, AnyError> {
        let controller = LanguageController::global_instance();
        let shapes = controller.mount_shapes(lang_address).await?;
        if self.mounted_version(lang_address).await?.as_deref() != Some(shapes.version.as_str()) {
            self.install_mount_shapes(lang_address, &shapes).await?;
        }
        let snapshot = controller.mount_snapshot(lang_address).await?;

        let mut report = MountSyncReport::default();
        let mut wanted_sources = HashSet::new();
        for entry in &snapshot.entries {
            wanted_sources.insert(format!("{lang_address}://{}", entry.address));
            let r = self.apply_mount_entry(lang_address, entry).await?;
            report.added += r.added;
            report.removed += r.removed;
        }

        // Snapshot sync: a mounted address missing from the snapshot is
        // removed — all the links the mount owns under that source.
        let doomed: Vec<LinkExpression> = self
            .owned_links(lang_address)
            .await?
            .into_iter()
            .filter(|l| !wanted_sources.contains(&l.data.source))
            .map(LinkExpression::from)
            .collect();
        report.removed += self.remove_links(doomed, None).await?.len();
        Ok(report)
    }

    /// Apply one delta (`changed` records re-expanded, `removed` addresses
    /// swept) — the same per-address diff `sync_mount` runs.
    pub async fn apply_mount_delta(
        &mut self,
        lang_address: &str,
        delta: &MountDelta,
    ) -> Result<MountSyncReport, AnyError> {
        let mut report = MountSyncReport::default();
        for entry in &delta.changed {
            let r = self.apply_mount_entry(lang_address, entry).await?;
            report.added += r.added;
            report.removed += r.removed;
        }
        for address in &delta.removed {
            let doomed: Vec<LinkExpression> = self
                .get_links(&LinkQuery {
                    source: Some(format!("{lang_address}://{address}")),
                    ..Default::default()
                })
                .await?
                .into_iter()
                .filter(|l| {
                    l.status == Some(LinkStatus::Local)
                        && !is_bookkeeping(l.data.predicate.as_deref())
                })
                .map(LinkExpression::from)
                .collect();
            report.removed += self.remove_links(doomed, None).await?.len();
        }
        Ok(report)
    }

    /// The addresses of every mounted language, from the mount links.
    pub async fn mounts(&self) -> Result<Vec<String>, AnyError> {
        let links = self
            .get_links(&LinkQuery {
                source: Some(MOUNT_SOURCE.to_string()),
                predicate: Some(MOUNT_PREDICATE.to_string()),
                ..Default::default()
            })
            .await?;
        let mut langs: Vec<String> = links
            .iter()
            .filter_map(|l| {
                LanguageController::parse_expr_url(&l.data.target)
                    .ok()
                    .map(|(lang, _)| lang)
            })
            .collect();
        langs.sort();
        langs.dedup();
        Ok(langs)
    }

    /// Startup: read the mount links, wait for each language to load
    /// (languages load after the conductor —
    /// `languages/conductor_languages.rs`), register it, and sync. A mount
    /// link naming a language that is not installed is skipped with one
    /// warning (a peer without the passphrase cannot install it, design
    /// §5.2). Never fails.
    pub(crate) async fn restore_mounts(&mut self) {
        let langs = match self.mounts().await {
            Ok(langs) => langs,
            Err(e) => {
                log::warn!("perspective {}: reading mount links failed: {e}", self.uuid);
                return;
            }
        };
        let controller = LanguageController::global_instance();
        for lang in langs {
            loop {
                if self.is_teardown.load(Ordering::Acquire) {
                    return;
                }
                if controller.is_language_loaded(&lang).await {
                    break;
                }
                if !crate::utils::languages_directory()
                    .join(&lang)
                    .join("bundle.js")
                    .exists()
                {
                    // A mount whose language is not installed is skipped,
                    // not an error (design §5.2).
                    log::warn!(
                        "perspective {}: mounted language {lang} is not installed; skipping its mount",
                        self.uuid
                    );
                    break;
                }
                sleep(Duration::from_secs(5)).await;
            }
            if self.is_teardown.load(Ordering::Acquire)
                || !controller.is_language_loaded(&lang).await
            {
                continue;
            }
            register_mount(&lang, &self.uuid);
            if let Err(e) = self.sync_mount(&lang).await {
                log::warn!(
                    "perspective {}: restoring mount {lang} failed: {e}",
                    self.uuid
                );
            }
        }
    }

    /// Re-runs every mount's snapshot every `mount_resync_interval()`
    /// (design §1.5: pushed deltas are best effort — the timer catches
    /// what a signal missed). Joined into `start_background_tasks`; exits
    /// on `is_teardown` like the other perspective loops. A failed sync
    /// logs one warning and the loop continues.
    pub(super) async fn mount_resync_loop(&self) {
        let uuid = self.uuid.clone();
        log::debug!("Starting mount resync loop for perspective {}", uuid);

        while !self.is_teardown.load(Ordering::Acquire) {
            // Sleep in slices (the fallback sync loop's pattern) so
            // teardown is observed promptly even at the 15 s interval.
            let mut remaining = mount_resync_interval();
            while remaining > Duration::from_millis(0) {
                if self.is_teardown.load(Ordering::Acquire) {
                    log::debug!("Mount resync loop ended for perspective {}", uuid);
                    return;
                }
                let step = std::cmp::min(Duration::from_secs(1), remaining);
                sleep(step).await;
                remaining = remaining.saturating_sub(step);
            }
            if self.is_teardown.load(Ordering::Acquire) {
                break;
            }

            // `mounts()` is one predicate read — a perspective with no
            // mounts costs nothing per tick.
            let langs = match self.mounts().await {
                Ok(langs) => langs,
                Err(e) => {
                    log::warn!("mount resync for perspective {uuid}: reading mounts failed: {e}");
                    continue;
                }
            };
            for lang in langs {
                // `sync_mount` takes `&mut self`; a clone is the same
                // instance.
                let mut p = self.clone();
                if let Err(e) = p.sync_mount(&lang).await {
                    log::warn!("resync of mount {lang} for perspective {uuid} failed: {e}");
                }
                if self.is_teardown.load(Ordering::Acquire) {
                    break;
                }
            }
        }

        log::debug!("Mount resync loop ended for perspective {}", uuid);
    }

    /// One mount per class set: `mount` refuses when another mounted
    /// language claims a class name `shapes` brings (design §2.2).
    async fn refuse_class_collisions(
        &self,
        lang_address: &str,
        shapes: &MountShapes,
    ) -> Result<(), AnyError> {
        let new_names: HashSet<&str> = shapes.shapes.iter().map(|s| s.name.as_str()).collect();
        for other in self.mounts().await? {
            if other == lang_address {
                continue;
            }
            let claimed = self.claimed_classes(&other).await?;
            if let Some(name) = new_names.iter().find(|n| claimed.contains(**n)) {
                return Err(anyhow!(
                    "cannot mount {lang_address}: class {name} is already mounted by language {other} in this perspective"
                ));
            }
        }
        Ok(())
    }

    /// The class names `lang`'s mount installed, from the claim links it
    /// wrote — the record that survives restarts and unloaded languages.
    /// An empty answer falls back to asking the language itself.
    async fn claimed_classes(&self, lang_address: &str) -> Result<HashSet<String>, AnyError> {
        let links = self
            .get_links(&LinkQuery {
                source: Some(mount_target(lang_address)),
                predicate: Some(CLASS_PREDICATE.to_string()),
                ..Default::default()
            })
            .await?;
        let mut names: HashSet<String> = links
            .iter()
            .filter_map(|l| match parse_literal_value(&l.data.target) {
                Value::String(s) => Some(s),
                _ => None,
            })
            .collect();
        if names.is_empty()
            && LanguageController::global_instance()
                .is_language_loaded(lang_address)
                .await
        {
            if let Ok(shapes) = LanguageController::global_instance()
                .mount_shapes(lang_address)
                .await
            {
                names = shapes.shapes.into_iter().map(|s| s.name).collect();
            }
        }
        Ok(names)
    }

    /// Install `shapes` through `add_sdna_batch` (one SubjectClass per
    /// shape, Shared as `add_sdna` writes them) and rewrite the mount's
    /// bookkeeping: the new version link and a class-claim link per shape.
    async fn install_mount_shapes(
        &mut self,
        lang_address: &str,
        shapes: &MountShapes,
    ) -> Result<(), AnyError> {
        let context = AgentContext::main_agent();
        let entries = shapes
            .shapes
            .iter()
            .map(|s| {
                (
                    s.name.clone(),
                    String::new(),
                    SdnaType::SubjectClass,
                    Some(s.shape.to_string()),
                )
            })
            .collect();
        self.add_sdna_batch(entries, &context).await?;
        self.alias_mount_class_names(shapes).await?;

        self.remove_bookkeeping(lang_address).await?;
        let mut links = Vec::with_capacity(shapes.shapes.len() + 1);
        links.push(Link {
            source: mount_target(lang_address),
            predicate: Some(VERSION_PREDICATE.to_string()),
            target: literal_target(&Value::String(shapes.version.clone()))?,
        });
        for shape in &shapes.shapes {
            links.push(Link {
                source: mount_target(lang_address),
                predicate: Some(CLASS_PREDICATE.to_string()),
                target: literal_target(&Value::String(shape.name.clone()))?,
            });
        }
        self.add_links(links, LinkStatus::Local, None, &context)
            .await?;
        Ok(())
    }

    /// A mount shape's `name` is what callers query by
    /// (`modelQuery("MountThing")`), while `load_shape` finds a class by the
    /// basename of its `sh:targetClass` (`mounttest://Thing` → `Thing`).
    /// When the two differ — the mount contract separates them on purpose
    /// (design §2.3: prefixed names, stable class IRIs) — the name-mapping
    /// node `literal:string:shacl://<name>` that `parse_shacl_to_links`
    /// already wrote becomes an alias class node: typed
    /// `ad4m://SubjectClass` and pointing at the same shape URI, so
    /// `get_shape` and `model_query` resolve the name to the shape. The
    /// `target_class` node stays too, so its basename still resolves.
    async fn alias_mount_class_names(&mut self, shapes: &MountShapes) -> Result<(), AnyError> {
        let mut links = Vec::new();
        for shape in &shapes.shapes {
            let target_class = shape.shape["target_class"].as_str().unwrap_or_default();
            if target_class.is_empty() || local_name(target_class) == shape.name {
                continue;
            }
            let name_node = format!("literal:string:shacl://{}", shape.name);
            let shape_uri = self
                .get_links(&LinkQuery {
                    source: Some(name_node.clone()),
                    predicate: Some("ad4m://shacl_shape_uri".to_string()),
                    ..Default::default()
                })
                .await?
                .first()
                .map(|l| l.data.target.clone())
                .ok_or_else(|| anyhow!("mounted class '{}' has no shape URI", shape.name))?;
            links.push(Link {
                source: name_node.clone(),
                predicate: Some("rdf://type".to_string()),
                target: "ad4m://SubjectClass".to_string(),
            });
            links.push(Link {
                source: name_node,
                predicate: Some("ad4m://shape".to_string()),
                target: shape_uri,
            });
        }
        if !links.is_empty() {
            self.add_links(links, LinkStatus::Shared, None, &AgentContext::main_agent())
                .await?;
        }
        Ok(())
    }

    /// Delete the mount's bookkeeping links under `<lang>://project`.
    async fn remove_bookkeeping(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let doomed: Vec<LinkExpression> = self
            .get_links(&LinkQuery {
                source: Some(mount_target(lang_address)),
                ..Default::default()
            })
            .await?
            .into_iter()
            .filter(|l| is_bookkeeping(l.data.predicate.as_deref()))
            .map(LinkExpression::from)
            .collect();
        self.remove_links(doomed, None).await?;
        Ok(())
    }

    /// The version `mountShapes()` reported when the shapes were last
    /// installed, decoded off its literal link.
    async fn mounted_version(&self, lang_address: &str) -> Result<Option<String>, AnyError> {
        let links = self
            .get_links(&LinkQuery {
                source: Some(mount_target(lang_address)),
                predicate: Some(VERSION_PREDICATE.to_string()),
                ..Default::default()
            })
            .await?;
        Ok(links.first().and_then(|l| {
            match parse_literal_value(&l.data.target) {
                Value::String(s) => Some(s),
                _ => None,
            }
        }))
    }

    /// Every `Local` record link under `<lang>://` — the data the mount
    /// owns; bookkeeping links are not part of the sweep.
    async fn owned_links(
        &self,
        lang_address: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let prefix = format!("{lang_address}://");
        Ok(self
            .get_links(&LinkQuery::default())
            .await?
            .into_iter()
            .filter(|l| {
                l.status == Some(LinkStatus::Local)
                    && l.data.source.starts_with(&prefix)
                    && !is_bookkeeping(l.data.predicate.as_deref())
            })
            .collect())
    }

    /// Diff one record against the store: current = its `Local` links with
    /// exactly this source, wanted = the expansion. `current − wanted` is
    /// removed, `wanted − current` added; equal links stay untouched (no
    /// subscription noise).
    async fn apply_mount_entry(
        &mut self,
        lang_address: &str,
        entry: &MountEntry,
    ) -> Result<MountSyncReport, AnyError> {
        let source = format!("{lang_address}://{}", entry.address);
        let wanted = self.expand_entry(entry, &source)?;
        let wanted_set: HashSet<(Option<String>, String)> = wanted
            .iter()
            .map(|l| (l.predicate.clone(), l.target.clone()))
            .collect();

        let current = self
            .get_links(&LinkQuery {
                source: Some(source.clone()),
                ..Default::default()
            })
            .await?
            .into_iter()
            .filter(|l| {
                l.status == Some(LinkStatus::Local)
                    && !is_bookkeeping(l.data.predicate.as_deref())
            })
            .collect::<Vec<_>>();

        // A wanted link already in the store stays; `claimed` also dedupes
        // surplus identical links so a doubled link loses its copy.
        let mut claimed = HashSet::new();
        let mut doomed = Vec::new();
        for link in &current {
            let key = (link.data.predicate.clone(), link.data.target.clone());
            if wanted_set.contains(&key) && claimed.insert(key) {
                continue;
            }
            doomed.push(LinkExpression::from(link.clone()));
        }

        let mut additions = Vec::new();
        let mut added = HashSet::new();
        for link in wanted {
            let key = (link.predicate.clone(), link.target.clone());
            if !claimed.contains(&key) && added.insert(key) {
                additions.push(link);
            }
        }

        let mut report = MountSyncReport::default();
        if !doomed.is_empty() {
            report.removed += self.remove_links(doomed, None).await?.len();
        }
        if !additions.is_empty() {
            report.added += self
                .add_links(additions, LinkStatus::Local, None, &AgentContext::main_agent())
                .await?
                .len();
        }
        Ok(report)
    }

    /// Expand one snapshot/delta record into the links it wants under
    /// `source` (design §2.3): every shape flag property writes its
    /// `has_value` link (the `rdf://type` flag is what a model query calls
    /// an instance); every other property reads `data[name]` — missing or
    /// null writes nothing, an array writes one link per element, a string
    /// that is an absolute IRI writes that IRI, anything else lands at its
    /// deterministic `literal:` IRI.
    fn expand_entry(&self, entry: &MountEntry, source: &str) -> Result<Vec<Link>, AnyError> {
        let shape = self.get_shape(&entry.r#class)?;
        let mut links = Vec::new();
        let mut resolve_logged = false;
        for prop in &shape.properties {
            if prop.is_flag {
                if let Some(target) = &prop.initial_value {
                    links.push(Link {
                        source: source.to_string(),
                        predicate: Some(prop.predicate.clone()),
                        target: target.clone(),
                    });
                }
                continue;
            }
            // `resolve_language` mints provenance expressions for links an
            // app writes; a mount mirrors the language's values and writes
            // the deterministic literal — log once per expansion.
            if prop.resolve_language.is_some() && !resolve_logged {
                log::warn!(
                    "mounted shape {} uses resolve_language on some properties; \
                     mounts ignore it and store deterministic literals",
                    entry.r#class
                );
                resolve_logged = true;
            }
            let Some(value) = entry.data.get(&prop.name) else {
                continue;
            };
            let values: &[Value] = match value {
                Value::Array(items) => items,
                _ => std::slice::from_ref(value),
            };
            let mut targets = Vec::with_capacity(values.len());
            for item in values {
                if item.is_null() {
                    continue;
                }
                let target = literal_target(item)?;
                targets.push(target.clone());
                links.push(Link {
                    source: source.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target,
                });
            }
            // An array is a sequence, but hydration restores only an order it
            // can read back: a collection the shape declares ordered is
            // reordered from `ad4m://collection_order` entries on the parent.
            // The mount is the sole writer under this source, so it restates
            // the whole linked list and lets the per-address diff drop the
            // stale half (single-writer needs none of the additive-only care
            // `apply_collection_ordering` takes for concurrent peers). The
            // `now_ms` pids embed is fixed at 0 so an unchanged array
            // expands to byte-identical entries and diffs to nothing.
            if let (Value::Array(_), Some(strategy_name)) = (value, &prop.ordering) {
                match crate::perspectives::ordering::create_strategy(strategy_name) {
                    Ok(strategy) => {
                        for ordering_entry in strategy.generate_full(
                            &targets,
                            &prop.predicate,
                            &crate::agent::did(),
                            0,
                        ) {
                            match crate::perspectives::ordering::encode_ordering_entry(
                                &ordering_entry,
                            ) {
                                Ok(target) => links.push(Link {
                                    source: source.to_string(),
                                    predicate: Some(
                                        crate::perspectives::ordering::COLLECTION_ORDER_PREDICATE
                                            .to_string(),
                                    ),
                                    target,
                                }),
                                Err(e) => {
                                    log::warn!("mount: could not encode ordering entry: {e}")
                                }
                            }
                        }
                    }
                    Err(e) => log::warn!(
                        "mounted shape {} declares ordering '{strategy_name}': {e}",
                        entry.r#class
                    ),
                }
            }
        }
        Ok(links)
    }
}
