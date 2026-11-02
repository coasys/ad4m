//! The default embedding model (Bert) of an executor without configured models: `load()`
//! enables it, and the first `embed` call that names it loads it. A fresh node therefore
//! downloads it only when something embeds, not when it starts.

use super::{publish_model_status, AIService, EmbeddingRequest, Result};
use crate::types::{LocalModel, Model, ModelType};
use anyhow::anyhow;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tokio::sync::{mpsc, OnceCell};

/// Name and id of the embedding model an executor without configured models serves.
pub(super) const DEFAULT_EMBEDDING_MODEL: &str = "bert";
pub(super) const DEFAULT_EMBEDDING_MODEL_ID: &str = "bert-id";

/// Loads an embedding model and returns the channel to its worker once the model is ready.
pub(super) type EmbeddingLoader = Arc<
    dyn Fn(
            Model,
        )
            -> Pin<Box<dyn Future<Output = Result<mpsc::UnboundedSender<EmbeddingRequest>>> + Send>>
        + Send
        + Sync,
>;

/// The default embedding model of an executor without configured models. `load()` only
/// enables it; the first `embed` call naming it loads it (downloading it on a fresh node),
/// and concurrent first calls wait on that load. A failed load leaves `ready` empty: its
/// error goes to the call that ran it, and the next waiting or later call loads again.
pub(super) struct DefaultEmbedding {
    enabled: AtomicBool,
    ready: OnceCell<()>,
    loader: EmbeddingLoader,
}

impl DefaultEmbedding {
    pub(super) fn new(loader: EmbeddingLoader) -> Self {
        DefaultEmbedding {
            enabled: AtomicBool::new(false),
            ready: OnceCell::new(),
            loader,
        }
    }
}

impl AIService {
    /// Makes the default model available without loading it; its status reads "Not loaded"
    /// until the first embed.
    pub(super) async fn enable_default_embedding(&self) {
        publish_model_status(
            DEFAULT_EMBEDDING_MODEL_ID.to_string(),
            0.0,
            "Not loaded",
            false,
            false,
        )
        .await;
        self.default_embedding.enabled.store(true, Ordering::SeqCst);
    }

    pub(super) async fn load_embedding_worker(
        model_config: Model,
    ) -> Result<mpsc::UnboundedSender<EmbeddingRequest>> {
        let (bert_tx, ready) = AIService::start_embedding_worker(model_config);
        ready
            .await
            .map_err(|_| anyhow!("embedding worker ended before its model loaded"))??;
        Ok(bert_tx)
    }

    /// Loads the default embedding model if `model_id` names it, it is enabled, and it has
    /// not loaded yet. See [`DefaultEmbedding`].
    pub(super) async fn ensure_default_embedding(&self, model_id: &str) -> Result<()> {
        let default = &self.default_embedding;
        if model_id != DEFAULT_EMBEDDING_MODEL || !default.enabled.load(Ordering::SeqCst) {
            return Ok(());
        }
        default
            .ready
            .get_or_try_init(|| async {
                if self.embedding_channel.lock().await.contains_key(model_id) {
                    // A model added under this name since start serves it.
                    return Ok(());
                }
                let model = Model {
                    id: DEFAULT_EMBEDDING_MODEL_ID.to_string(),
                    name: DEFAULT_EMBEDDING_MODEL.to_string(),
                    model_type: ModelType::Embedding,
                    local: Some(LocalModel {
                        file_name: DEFAULT_EMBEDDING_MODEL.to_string(),
                        tokenizer_source: None,
                        huggingface_repo: None,
                        revision: None,
                    }),
                    api: None,
                };
                let sender = (default.loader)(model).await?;
                self.embedding_channel
                    .lock()
                    .await
                    .insert(model_id.to_string(), sender);
                Ok::<(), anyhow::Error>(())
            })
            .await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use tokio::time::{sleep, Duration};

    /// A loader that counts its calls, fails the first `fail_first` of them, and otherwise
    /// returns a worker answering every request with `[1.0, 2.0]`. It takes a moment to
    /// "load" so concurrent first calls overlap.
    fn counting_embedding_loader(
        calls: Arc<std::sync::atomic::AtomicUsize>,
        fail_first: usize,
    ) -> EmbeddingLoader {
        Arc::new(move |model: Model| {
            let calls = calls.clone();
            Box::pin(async move {
                assert_eq!(model.name, DEFAULT_EMBEDDING_MODEL);
                let call = calls.fetch_add(1, Ordering::SeqCst);
                sleep(Duration::from_millis(100)).await;
                if call < fail_first {
                    return Err(anyhow!("download failed"));
                }
                let (tx, mut rx) = mpsc::unbounded_channel::<EmbeddingRequest>();
                tokio::spawn(async move {
                    while let Some(request) = rx.recv().await {
                        let _ = request.result_sender.send(Ok(vec![1.0, 2.0]));
                    }
                });
                Ok(tx)
            })
        })
    }

    #[tokio::test]
    async fn default_embedding_loads_on_first_embed_not_at_start() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let service = AIService::with_embedding_loader(counting_embedding_loader(calls.clone(), 0))
            .expect("initialization to work");
        service.load().await.expect("load to work");
        assert_eq!(
            calls.load(Ordering::SeqCst),
            0,
            "start loads no embedding model"
        );
        let status = AIService::model_status(DEFAULT_EMBEDDING_MODEL_ID.into())
            .await
            .expect("the default model has a status before its first use");
        assert_eq!(status.status, "Not loaded");
        assert!(!status.loaded && !status.downloaded);

        let (a, b) = tokio::join!(
            service.embed("bert".into(), "one".into(), None),
            service.embed("bert".into(), "two".into(), None),
        );
        assert_eq!(a.expect("first embed").embeddings, vec![1.0, 2.0]);
        assert_eq!(b.expect("second embed").embeddings, vec![1.0, 2.0]);
        service
            .embed("bert".into(), "three".into(), None)
            .await
            .expect("later embed");
        assert_eq!(
            calls.load(Ordering::SeqCst),
            1,
            "concurrent and later embeds share one load"
        );
    }

    #[tokio::test]
    async fn default_embedding_load_error_reaches_caller_and_next_call_retries() {
        Ad4mDb::init_global_instance(":memory:").expect("Ad4mDb to initialize");
        let calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let service = AIService::with_embedding_loader(counting_embedding_loader(calls.clone(), 2))
            .expect("initialization to work");
        service.load().await.expect("load to work");

        let Err(error) = service.embed("bert".into(), "one".into(), None).await else {
            panic!("a failed load fails the embed");
        };
        assert!(
            format!("{error:#}").contains("download failed"),
            "{error:#}"
        );

        // The second load fails too: the call that ran it gets the error, the waiting call
        // loads again and succeeds.
        let (a, b) = tokio::join!(
            service.embed("bert".into(), "two".into(), None),
            service.embed("bert".into(), "three".into(), None),
        );
        let errors: Vec<String> = [&a, &b]
            .iter()
            .filter_map(|r| r.as_ref().err().map(|e| format!("{e:#}")))
            .collect();
        assert_eq!(errors.len(), 1, "exactly one call sees the failed load");
        assert!(errors[0].contains("download failed"), "{}", errors[0]);
        service
            .embed("bert".into(), "four".into(), None)
            .await
            .expect("a later embed uses the loaded model");
        assert_eq!(calls.load(Ordering::SeqCst), 3);
    }
}
