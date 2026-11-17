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

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;

use super::PerspectiveInstance;
use crate::languages::mount_types::{MountDelta, MountSyncReport};

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

impl PerspectiveInstance {
    /// Mount a language: refuse when it does not export `mountShapes()` /
    /// `mountSnapshot()` or when another mount in this perspective claims
    /// one of its class names; install the shapes; write the mount link;
    /// then run the first sync.
    pub async fn mount(&mut self, _lang_address: &str) -> Result<(), AnyError> {
        Err(anyhow!("mounts are not implemented"))
    }

    /// Drop the mount: every `Local` link under `<lang>://` plus the mount
    /// link itself, and the registry entry.
    pub async fn unmount(&mut self, _lang_address: &str) -> Result<(), AnyError> {
        Err(anyhow!("mounts are not implemented"))
    }

    /// Re-read the snapshot and diff every record against the store; a
    /// `mountShapes()` version change re-installs the shapes first
    /// (design §2.4). Addresses the snapshot no longer lists are removed.
    pub async fn sync_mount(
        &mut self,
        _lang_address: &str,
    ) -> Result<MountSyncReport, AnyError> {
        Err(anyhow!("mounts are not implemented"))
    }

    /// Apply one delta (`changed` records re-expanded, `removed` addresses
    /// swept) — the same per-address diff `sync_mount` runs.
    pub async fn apply_mount_delta(
        &mut self,
        _lang_address: &str,
        _delta: &MountDelta,
    ) -> Result<MountSyncReport, AnyError> {
        Err(anyhow!("mounts are not implemented"))
    }

    /// The addresses of every mounted language, from the mount links.
    pub async fn mounts(&self) -> Result<Vec<String>, AnyError> {
        Ok(Vec::new())
    }

    /// Startup: read the mount links, wait for each language to load
    /// (languages load after the conductor —
    /// `languages/conductor_languages.rs`), register it, and sync. A mount
    /// link naming a language that is not installed is skipped with one
    /// warning (a peer without the passphrase cannot install it, design
    /// §5.2). Never fails.
    pub(crate) async fn restore_mounts(&mut self) {}
}
