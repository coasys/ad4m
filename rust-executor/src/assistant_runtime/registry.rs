//! Run registry — one live run per thread, each on its own `tokio` task.
//!
//! Shaped like `ai_service`'s `llm_channel`: an `Arc<Mutex<HashMap<..>>>`
//! guarding the live runs. [`RunRegistry::start`] dedupes under the lock so a
//! burst of link events (a user message is several links) can only ever spawn
//! one run per thread; a run removes itself from the map when it finishes.

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::Mutex;
use tokio::task::JoinHandle;

use super::model_backend::ModelBackend;
use super::run::{self, RunInput};

struct RunHandle {
    handle: JoinHandle<()>,
}

/// Cheaply-cloneable handle to the set of live runs, keyed by thread id.
/// Holds the production [`ModelBackend`] handed to each spawned run.
#[derive(Clone)]
pub struct RunRegistry {
    inner: Arc<Mutex<HashMap<String, RunHandle>>>,
    backend: Arc<dyn ModelBackend>,
}

impl RunRegistry {
    pub fn new(backend: Arc<dyn ModelBackend>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(HashMap::new())),
            backend,
        }
    }

    /// Start a run for `input.thread_id` unless one is already live. Dedupe and
    /// insertion happen under a single lock acquisition so concurrent callers
    /// cannot both spawn.
    pub async fn start(&self, input: RunInput) {
        let thread_id = input.thread_id.clone();
        let mut guard = self.inner.lock().await;

        if let Some(existing) = guard.get(&thread_id) {
            if !existing.handle.is_finished() {
                log::debug!("assistant_runtime: run already active for thread {thread_id}");
                return;
            }
        }

        let registry = self.clone();
        let backend = self.backend.clone();
        let tid = thread_id.clone();
        let handle = tokio::spawn(async move {
            if let Err(e) = run::run_thread(input, backend).await {
                log::error!("assistant_runtime: run_thread error for {tid}: {e}");
            }
            registry.remove(&tid).await;
        });

        guard.insert(thread_id, RunHandle { handle });
    }

    async fn remove(&self, thread_id: &str) {
        self.inner.lock().await.remove(thread_id);
    }

    /// Number of currently-tracked runs (test/introspection helper).
    #[cfg(test)]
    pub async fn active_count(&self) -> usize {
        self.inner.lock().await.len()
    }
}
