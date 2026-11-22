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
//! Ownership: the mount owns the `Local` links it wrote under `<lang>://`
//! (`mount_links`: signed at a fixed timestamp) — `unmount` deletes them
//! all, and no sync or delta touches anyone else's link there. The mount's own
//! bookkeeping (the version link under `<lang>://project`) is exempt from
//! the snapshot sweep. Which classes a mount owns is recorded outside its
//! address space (`mount_classes`). The mount link itself is `Shared` so
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

use super::mount_gates::{pause, MountPoint};
use super::mount_lifecycle::{next_mount_seq, MountPhase, MountSlot};
use super::mount_links::is_mount_written;
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

/// One snapshot or delta record, expanded into the links it wants under
/// `source` and not written yet.
struct StagedRecord {
    address: String,
    source: String,
    links: Vec<Link>,
}

/// Bookkeeping predicates never belong to a record, so the snapshot sweep
/// and the per-address diff must leave them alone.
fn is_bookkeeping(predicate: Option<&str>) -> bool {
    predicate == Some(VERSION_PREDICATE)
}

impl PerspectiveInstance {
    /// Mount a language: refuse when it does not export `mountShapes()` /
    /// `mountSnapshot()` or when its classes are not its own to install
    /// (`check_class_ownership`); install the shapes; write the first
    /// snapshot; then write the mount link and register for pushed deltas.
    /// Every record is expanded before any is written, and the mount link
    /// comes last: a mount that fails leaves no mount link and none of its
    /// records (its class claims stay, so a retry may install over its own
    /// shapes), and a retry runs it all again. While it runs the mount is
    /// `Mounting`: no sync or delta applies, and an `unmount` meanwhile
    /// wins.
    pub async fn mount(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let generation = {
            let mut slot = self.mount_lifecycles.lock(lang_address).await;
            match slot.phase {
                MountPhase::Active => return Ok(()),
                MountPhase::Mounting => {
                    return Err(anyhow!("{lang_address} is already being mounted"))
                }
                MountPhase::Inactive => {
                    self.mount_lifecycles.enter(&mut slot, MountPhase::Mounting)
                }
            }
        };
        let result = self.mount_as(lang_address, generation).await;
        if result.is_err() {
            let mut slot = self.mount_lifecycles.lock(lang_address).await;
            if slot.generation == generation && slot.phase == MountPhase::Mounting {
                self.mount_lifecycles.enter(&mut slot, MountPhase::Inactive);
            }
        }
        result
    }

    /// `mount`'s work for the mount `generation` names.
    async fn mount_as(&mut self, lang_address: &str, generation: u64) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let shapes = controller.mount_shapes(lang_address).await?;
        let seq = next_mount_seq();
        let snapshot = controller.mount_snapshot(lang_address).await?;

        let mut slot = self.mount_lifecycles.lock(lang_address).await;
        if slot.generation != generation {
            return Err(anyhow!(
                "{lang_address} was unmounted while it was being mounted"
            ));
        }
        self.install_mount_shapes(lang_address, &shapes).await?;
        let written = async {
            let records = self.stage_records(lang_address, &snapshot.entries)?;
            self.write_snapshot(lang_address, &mut slot, records, seq)
                .await?;
            if !self.has_mount_link(lang_address).await? {
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
            }
            Ok::<(), AnyError>(())
        }
        .await;
        if let Err(e) = written {
            if let Err(cleanup) = self.remove_written_under(lang_address).await {
                log::warn!(
                    "perspective {}: cleaning up the failed mount of {lang_address} failed: {cleanup}",
                    self.uuid
                );
            }
            return Err(e);
        }
        slot.phase = MountPhase::Active;
        register_mount(lang_address, &self.uuid);
        Ok(())
    }

    /// Remove every link the mount wrote under `<lang>://` (records and
    /// bookkeeping).
    async fn remove_written_under(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let prefix = format!("{lang_address}://");
        let doomed: Vec<LinkExpression> = self
            .get_links(&LinkQuery::default())
            .await?
            .into_iter()
            .filter(|l| l.data.source.starts_with(&prefix) && is_mount_written(l))
            .map(LinkExpression::from)
            .collect();
        self.remove_links(doomed, None).await?;
        Ok(())
    }

    /// Drop the mount: every link it wrote under `<lang>://` plus the mount
    /// link itself, and the registry entry. Takes the mount's apply lock
    /// and moves it to a new generation, so a sync, delta or call result
    /// still in flight finds the mount gone and writes nothing.
    pub async fn unmount(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let mut slot = self.mount_lifecycles.lock(lang_address).await;
        self.mount_lifecycles.enter(&mut slot, MountPhase::Inactive);
        unregister_mount(lang_address, &self.uuid);

        self.remove_written_under(lang_address).await?;
        let mount_links: Vec<LinkExpression> = self
            .get_links(&LinkQuery {
                source: Some(MOUNT_SOURCE.to_string()),
                predicate: Some(MOUNT_PREDICATE.to_string()),
                target: Some(mount_target(lang_address)),
                ..Default::default()
            })
            .await?
            .into_iter()
            .map(LinkExpression::from)
            .collect();
        self.remove_links(mount_links, None).await?;
        Ok(())
    }

    /// Re-read the snapshot and diff every record against the store; a
    /// `mountShapes()` version change re-installs the shapes first
    /// (design §2.4). Addresses the snapshot no longer lists are removed.
    /// Refused for a language this perspective has not mounted. The
    /// language is asked first; the result is applied under the mount's
    /// apply lock only if the mount is still the one the sync started on.
    pub async fn sync_mount(&mut self, lang_address: &str) -> Result<MountSyncReport, AnyError> {
        let Some(generation) = self.active_mount_generation(lang_address).await else {
            return Err(anyhow!(
                "language {lang_address} is not mounted in perspective {}",
                self.uuid
            ));
        };
        let controller = LanguageController::global_instance();
        let shapes = controller.mount_shapes(lang_address).await?;
        let seq = next_mount_seq();
        let snapshot = controller.mount_snapshot(lang_address).await?;
        pause(lang_address, MountPoint::SnapshotFetched).await;

        let Some(mut slot) = self
            .mount_lifecycles
            .lock_current(lang_address, generation)
            .await
        else {
            return Err(anyhow!(
                "language {lang_address} was unmounted while it synced"
            ));
        };
        if self.mounted_version(lang_address).await?.as_deref() != Some(shapes.version.as_str()) {
            self.install_mount_shapes(lang_address, &shapes).await?;
        }
        let records = self.stage_records(lang_address, &snapshot.entries)?;
        self.write_snapshot(lang_address, &mut slot, records, seq)
            .await
    }

    /// Expand `entries` into the records they want, before any is
    /// written: an entry that does not expand fails them all.
    fn stage_records(
        &self,
        lang_address: &str,
        entries: &[MountEntry],
    ) -> Result<Vec<StagedRecord>, AnyError> {
        entries
            .iter()
            .map(|entry| {
                let source = format!("{lang_address}://{}", entry.address);
                let links = self
                    .expand_entry(entry, &source)
                    .map_err(|e| anyhow!("mounted record {}: {e}", entry.address))?;
                Ok(StagedRecord {
                    address: entry.address.clone(),
                    source,
                    links,
                })
            })
            .collect()
    }

    /// Write a snapshot taken at `seq` (its `records` staged): diff each
    /// record, then sweep every record the snapshot no longer lists. Runs
    /// under the mount's apply lock (`slot`). A snapshot older than the
    /// last one applied writes nothing, and a record a delta newer than
    /// this snapshot wrote keeps the delta's state: the snapshot may have
    /// been read before the change.
    async fn write_snapshot(
        &mut self,
        lang_address: &str,
        slot: &mut MountSlot,
        records: Vec<StagedRecord>,
        seq: u64,
    ) -> Result<MountSyncReport, AnyError> {
        if slot.snapshot_is_stale(seq) {
            return Ok(MountSyncReport::default());
        }
        let mut report = MountSyncReport::default();
        let mut wanted_sources = HashSet::new();
        for record in records {
            wanted_sources.insert(record.source.clone());
            if slot.newer_than_snapshot(&record.address, seq) {
                continue;
            }
            let r = self.write_record(lang_address, record).await?;
            report.added += r.added;
            report.removed += r.removed;
        }

        // A mounted address missing from the snapshot is removed — all the
        // links the mount owns under that source.
        let prefix = format!("{lang_address}://");
        let doomed: Vec<LinkExpression> = self
            .owned_links(lang_address)
            .await?
            .into_iter()
            .filter(|l| {
                !wanted_sources.contains(&l.data.source)
                    && !l
                        .data
                        .source
                        .strip_prefix(&prefix)
                        .is_some_and(|address| slot.newer_than_snapshot(address, seq))
            })
            .map(LinkExpression::from)
            .collect();
        report.removed += self.remove_links(doomed, None).await?.len();
        slot.snapshot_applied(seq);
        Ok(report)
    }

    /// The generation of `lang`'s mount while it is active: a delta or call
    /// result takes it before it goes to the language (or as it arrives)
    /// and hands it to `apply_mount_delta`.
    pub(crate) async fn active_mount_generation(&self, lang_address: &str) -> Option<u64> {
        self.mount_lifecycles.active_generation(lang_address).await
    }

    /// Apply one delta (`changed` records re-expanded, `removed` addresses
    /// swept) — the same per-address diff `sync_mount` runs — under the
    /// mount's apply lock. A delta for a mount that is no longer the one
    /// at `generation` (unmounted since) is dropped, and so is each record
    /// of it that something newer than `seq` already wrote.
    pub async fn apply_mount_delta(
        &mut self,
        lang_address: &str,
        delta: &MountDelta,
        generation: u64,
        seq: u64,
    ) -> Result<MountSyncReport, AnyError> {
        pause(lang_address, MountPoint::DeltaReceived).await;
        let Some(mut slot) = self
            .mount_lifecycles
            .lock_current(lang_address, generation)
            .await
        else {
            log::debug!(
                "perspective {}: {lang_address} is no longer mounted; delta dropped",
                self.uuid
            );
            return Ok(MountSyncReport::default());
        };
        let mut report = MountSyncReport::default();
        for record in self.stage_records(lang_address, &delta.changed)? {
            if !slot.delta_applies(&record.address, seq) {
                continue;
            }
            let address = record.address.clone();
            let r = self.write_record(lang_address, record).await?;
            slot.delta_applied(&address, seq);
            report.added += r.added;
            report.removed += r.removed;
        }
        for address in &delta.removed {
            if !slot.delta_applies(address, seq) {
                continue;
            }
            slot.delta_applied(address, seq);
            let doomed: Vec<LinkExpression> = self
                .get_links(&LinkQuery {
                    source: Some(format!("{lang_address}://{address}")),
                    ..Default::default()
                })
                .await?
                .into_iter()
                .filter(|l| is_mount_written(l) && !is_bookkeeping(l.data.predicate.as_deref()))
                .map(LinkExpression::from)
                .collect();
            report.removed += self.remove_links(doomed, None).await?.len();
        }
        Ok(report)
    }

    /// Whether the mount link for `lang` is in the store.
    async fn has_mount_link(&self, lang_address: &str) -> Result<bool, AnyError> {
        Ok(!self
            .get_links(&LinkQuery {
                source: Some(MOUNT_SOURCE.to_string()),
                predicate: Some(MOUNT_PREDICATE.to_string()),
                target: Some(mount_target(lang_address)),
                ..Default::default()
            })
            .await?
            .is_empty())
    }

    /// Make the mount a mount link names active (startup, or a mount link
    /// a peer shared): register it for pushed deltas under a new
    /// generation. Returns whether it is active now. Checked under the
    /// apply lock, so a link an `unmount` removed meanwhile activates
    /// nothing.
    async fn activate_mount(&mut self, lang_address: &str) -> Result<bool, AnyError> {
        let mut slot = self.mount_lifecycles.lock(lang_address).await;
        match slot.phase {
            MountPhase::Active => return Ok(true),
            MountPhase::Mounting => return Ok(false),
            MountPhase::Inactive => {}
        }
        if !self.has_mount_link(lang_address).await? {
            return Ok(false);
        }
        self.mount_lifecycles.enter(&mut slot, MountPhase::Active);
        register_mount(lang_address, &self.uuid);
        Ok(true)
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
    /// `languages/conductor_languages.rs`), activate it (`activate_mount`),
    /// and sync. A mount
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
            match self.activate_mount(&lang).await {
                Ok(true) => {}
                Ok(false) => continue,
                Err(e) => {
                    log::warn!(
                        "perspective {}: restoring mount {lang} failed: {e}",
                        self.uuid
                    );
                    continue;
                }
            }
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
    /// on `is_teardown` like the other perspective loops. A mount link
    /// whose mount is not active yet (a peer shared it, or its language
    /// loaded after the restore gave up) is activated once its language is
    /// loaded. A failed sync logs one warning and the loop continues.
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
            let controller = LanguageController::global_instance();
            for lang in langs {
                if !controller.is_language_loaded(&lang).await {
                    continue;
                }
                // `sync_mount` takes `&mut self`; a clone is the same
                // instance.
                let mut p = self.clone();
                match p.activate_mount(&lang).await {
                    Ok(true) => {}
                    Ok(false) => continue,
                    Err(e) => {
                        log::warn!("resync of mount {lang} for perspective {uuid} failed: {e}");
                        continue;
                    }
                }
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

    /// Install `shapes` through `add_sdna_batch` (one SubjectClass per
    /// shape, Shared as `add_sdna` writes them) once `check_class_ownership`
    /// lets this language install them and it claimed them, then rewrite
    /// the mount's version link. The check and the install hold the
    /// perspective's install lock, so two mounts cannot both pass the check
    /// for the same class.
    async fn install_mount_shapes(
        &mut self,
        lang_address: &str,
        shapes: &MountShapes,
    ) -> Result<(), AnyError> {
        let lifecycles = self.mount_lifecycles.clone();
        let _installing = lifecycles.install_lock().await;
        self.check_class_ownership(lang_address, shapes).await?;
        // Claimed before they are installed: an install that fails halfway
        // leaves classes this language may still install over on a retry.
        self.claim_mount_classes(lang_address, shapes).await?;
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
        let version = Link {
            source: mount_target(lang_address),
            predicate: Some(VERSION_PREDICATE.to_string()),
            target: literal_target(&Value::String(shapes.version.clone()))?,
        };
        self.add_mount_links(vec![version]).await?;
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

    /// Delete the bookkeeping links the mount wrote under `<lang>://project`.
    async fn remove_bookkeeping(&mut self, lang_address: &str) -> Result<(), AnyError> {
        let doomed: Vec<LinkExpression> = self
            .get_links(&LinkQuery {
                source: Some(mount_target(lang_address)),
                ..Default::default()
            })
            .await?
            .into_iter()
            .filter(|l| is_mount_written(l) && is_bookkeeping(l.data.predicate.as_deref()))
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
        Ok(links.iter().find(|l| is_mount_written(l)).and_then(|l| {
            match parse_literal_value(&l.data.target) {
                Value::String(s) => Some(s),
                _ => None,
            }
        }))
    }

    /// Every record link the mount wrote under `<lang>://` — the data it
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
                is_mount_written(l)
                    && l.data.source.starts_with(&prefix)
                    && !is_bookkeeping(l.data.predicate.as_deref())
            })
            .collect())
    }

    /// Diff one record against the store: current = the links the mount
    /// wrote with exactly this source, wanted = the expansion. `current −
    /// wanted` is removed, `wanted − current` added; equal links stay
    /// untouched (no subscription noise). Anyone else's link under the
    /// source is neither adopted nor removed.
    async fn write_record(
        &mut self,
        lang_address: &str,
        record: StagedRecord,
    ) -> Result<MountSyncReport, AnyError> {
        let StagedRecord {
            source,
            links: wanted,
            ..
        } = record;
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
            .filter(|l| is_mount_written(l) && !is_bookkeeping(l.data.predicate.as_deref()))
            .collect::<Vec<_>>();
        pause(lang_address, MountPoint::EntryRead).await;

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
            report.added += self.add_mount_links(additions).await?;
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
