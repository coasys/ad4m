//! Bulk-import of a perspective-export file straight into the SPARQL store:
//! the path `runtime.importData {type:"perspective"}` takes. Unlike the
//! neighbourhood sync and the normal write path, an import sends no link to
//! the link language and publishes no per-link diff or Prolog update — the
//! store gains the file's links in one commit, then anything that derived
//! state from the old graph is told to re-check once.

use std::path::Path;
use std::sync::atomic::Ordering;

use deno_core::anyhow::Error;

use super::{notification_pool_name, ChangedPredicates, PerspectiveInstance};
use crate::perspectives::sparql_store::LinkImportStats;
use crate::prolog_service::{get_prolog_service, PrologMode, PROLOG_MODE};

impl PerspectiveInstance {
    /// Load `path` — the shape `runtime.exportData {type:"perspective"}`
    /// writes — into this perspective's store, returning the counts
    /// [`SparqlStore::import_links_file`] reports. An import adds links
    /// that are new to the perspective, in one commit; a file the store
    /// refuses commits nothing. The store holds its writer lock for the
    /// whole load: a concurrent `add_link` waits, and nothing sees a
    /// half-imported graph on the write side (reads use snapshots and see
    /// the load once committed).
    ///
    /// The import publishes nothing to the link language. In a
    /// neighbourhood perspective the imported Shared links sit in the
    /// store like links written while offline: the normal sync
    /// (`share_links_missing_remotely`) may publish the ones peers lack
    /// later. Local links never leave the store.
    pub(crate) async fn import_links_file(&self, path: &Path) -> Result<LinkImportStats, Error> {
        let store = self.sparql_store.clone();
        let path = path.to_path_buf();
        let stats = tokio::task::spawn_blocking(move || store.import_links_file(&path))
            .await
            .map_err(|e| deno_core::anyhow::anyhow!("import task join: {}", e))??;

        // The load wrote no diff, so nothing downstream knows the graph
        // changed. Invalidate first — every cached or pooled view of the
        // old graph must be stale before a re-check runs against it —
        // then raise the checks.
        //
        // Prolog engines must see the imported links on their next query,
        // but replaying the file's links as a diff would cost what the
        // import was built to avoid. Simple mode marks its engines dirty
        // for the lazy reload; SdnaOnly already compares SDNA links on
        // every query; Pooled drops the pools so the next query's ensure
        // step rebuilds them from the store. `clear_shape_cache` flushes
        // the SHACL shapes the same way `sync_existing_links_to_sparql`
        // does.
        self.mark_prolog_engine_dirty().await;
        // The store is committed: a failed pool removal must not skip the
        // checks below, so its error is held until they are raised.
        let mut pools = Ok(());
        if PROLOG_MODE == PrologMode::Pooled {
            let service = get_prolog_service().await;
            let uuid = self.uuid.clone();
            for pool in [uuid.clone(), notification_pool_name(&uuid)] {
                if let Err(e) = service._remove_perspective_pool(pool).await {
                    pools = pools.and(Err(e));
                }
            }
        }
        self.clear_shape_cache();

        // Mark every predicate changed so the next subscription pass
        // re-checks all live queries against the new graph rather than
        // matching against a diff that never ran, and queue the
        // subscription and notification checks.
        *self.changed_predicates.lock().await = ChangedPredicates::CheckAll;
        self.trigger_subscription_check
            .store(true, Ordering::Release);
        self.request_notification_check();

        pools.map_err(|e| {
            deno_core::anyhow::anyhow!(
                "imported {} links, but a Prolog pool of the old graph was not dropped: {}",
                stats.imported,
                e
            )
        })?;
        Ok(stats)
    }
}
