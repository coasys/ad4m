//! Bulk-import of a perspective-export file straight into the SPARQL store:
//! the path `runtime.importData {type:"perspective"}` takes. Unlike the
//! neighbourhood sync and the normal write path, an import sends no link to
//! the link language and publishes no per-link diff or Prolog update — the
//! store gains the file's links, then anything that derived state from the
//! old graph is told to re-check once.

use std::path::Path;
use std::sync::atomic::Ordering;

use deno_core::anyhow::Error;

use super::{ChangedPredicates, PerspectiveInstance};
use crate::perspectives::sparql_store::LinkImportStats;

impl PerspectiveInstance {
    /// Load `path` — the shape `runtime.exportData {type:"perspective"}`
    /// writes — into this perspective's store, returning the counts
    /// [`SparqlStore::import_links_file`] reports. The store holds its
    /// writer lock for the whole load: a concurrent `add_link` waits, and
    /// nothing sees a half-imported graph on the write side (reads use
    /// snapshots and see the load once committed).
    pub(crate) async fn import_links_file(&self, path: &Path) -> Result<LinkImportStats, Error> {
        let store = self.sparql_store.clone();
        let path = path.to_path_buf();
        let stats = tokio::task::spawn_blocking(move || store.import_links_file(&path))
            .await
            .map_err(|e| deno_core::anyhow::anyhow!("import task join: {}", e))??;

        // The load wrote no diff, so nothing downstream knows the graph
        // changed: invalidate what a query could have cached from the old
        // one (`sync_existing_links_to_sparql` flushes the shape cache the
        // same way), and mark every predicate changed so the next
        // subscription pass re-checks all live queries rather than
        // matching against a diff that never ran.
        self.clear_shape_cache();
        *self.changed_predicates.lock().await = ChangedPredicates::CheckAll;
        self.trigger_subscription_check
            .store(true, Ordering::Release);

        Ok(stats)
    }
}
