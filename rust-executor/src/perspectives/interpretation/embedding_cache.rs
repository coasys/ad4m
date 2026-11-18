//! Title vectors, made on first need and kept in the `instance_embedding` table.
//!
//! A row is the vector of one instance's title for one embedding model, under the sha256 of
//! that title. A title that changed has another hash: its old vector is stale, the title is
//! embedded again and the new row replaces the old one. The model key names the registered
//! model and the remote model behind it, so a model that an operator points at another
//! remote model does not reuse the vectors of the first.

use crate::db::InstanceEmbeddingRow;
use crate::types::ModelType;
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// The sha256 (hex) of `text`: the hash a cached vector is valid for.
pub(crate) fn text_sha256(text: &str) -> String {
    hex::encode(Sha256::digest(text.as_bytes()))
}

/// The embedding model `model` names (its id, or the name of an embedding model) on this
/// executor, as `(id, cache key)`. `None` when there is no such model.
pub(crate) fn resolve_embedding_model(model: &str) -> Option<(String, String)> {
    let models = crate::db_backend::db_backend().get_models().ok()?;
    models
        .iter()
        .find(|m| m.model_type == ModelType::Embedding && m.id == model)
        .or_else(|| {
            models
                .iter()
                .find(|m| m.model_type == ModelType::Embedding && m.name == model)
        })
        .map(|m| (m.id.clone(), format!("{}:{}", m.id, m.remote_model)))
}

/// The id of this executor's default embedding model, else of its first one. `None` when
/// it has no embedding model.
pub(crate) fn default_embedding_model() -> Option<String> {
    let db = crate::db_backend::db_backend();
    let models = db.get_models().ok()?;
    let is_embedding = |id: &str| {
        models
            .iter()
            .any(|m| m.id == id && m.model_type == ModelType::Embedding)
    };
    db.get_default_model(ModelType::Embedding)
        .ok()
        .flatten()
        .filter(|id| is_embedding(id))
        .or_else(|| {
            models
                .iter()
                .find(|m| m.model_type == ModelType::Embedding)
                .map(|m| m.id.clone())
        })
}

/// How [`embed_rows`] got its vectors.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct CacheStats {
    /// Vectors read from the table.
    pub cached: usize,
    /// Titles embedded now: not in the table, or stale.
    pub embedded: usize,
}

/// The vectors of `rows` (instance id, title) on `perspective` with the embedding model
/// `model` (an id or a name), in input order. Each comes from the table when its row there
/// has the hash of the title; the others are embedded in one pass and stored. An error from
/// the model, or a model this executor does not have, is an error: nothing is stored then.
pub(crate) async fn embed_rows(
    perspective: &str,
    model: &str,
    rows: &[(String, String)],
) -> anyhow::Result<(Vec<Vec<f32>>, CacheStats)> {
    let (model_id, key) = resolve_embedding_model(model)
        .ok_or_else(|| anyhow::anyhow!("no EMBEDDING model '{model}' on this executor"))?;
    let db = crate::db_backend::db_backend();
    let ids: Vec<String> = rows.iter().map(|(id, _)| id.clone()).collect();
    let stored: HashMap<String, InstanceEmbeddingRow> = db
        .get_instance_embeddings(perspective, &key, &ids)?
        .into_iter()
        .map(|row| (row.instance_id.clone(), row))
        .collect();

    let hashes: Vec<String> = rows.iter().map(|(_, text)| text_sha256(text)).collect();
    let mut out: Vec<Option<Vec<f32>>> = vec![None; rows.len()];
    let mut missing: Vec<usize> = Vec::new();
    for (i, (id, _)) in rows.iter().enumerate() {
        match stored.get(id) {
            Some(row) if row.text_sha256 == hashes[i] => out[i] = Some(row.vector.clone()),
            _ => missing.push(i),
        }
    }
    let stats = CacheStats {
        cached: rows.len() - missing.len(),
        embedded: missing.len(),
    };
    if !missing.is_empty() {
        let texts: Vec<String> = missing.iter().map(|&i| rows[i].1.clone()).collect();
        let vectors = super::dedup::embed_via_ai_service(&model_id, &texts).await?;
        if vectors.len() != texts.len() {
            anyhow::bail!(
                "embedding model '{model}' answered {} vectors for {} texts",
                vectors.len(),
                texts.len()
            );
        }
        let new_rows: Vec<InstanceEmbeddingRow> = missing
            .iter()
            .zip(vectors.iter())
            .map(|(&i, vector)| InstanceEmbeddingRow {
                instance_id: rows[i].0.clone(),
                text_sha256: hashes[i].clone(),
                vector: vector.clone(),
            })
            .collect();
        db.put_instance_embeddings(perspective, &key, &new_rows)?;
        for (&i, vector) in missing.iter().zip(vectors) {
            out[i] = Some(vector);
        }
    }
    Ok((
        out.into_iter().map(Option::unwrap_or_default).collect(),
        stats,
    ))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// An EMBEDDING model `name` on a mock endpoint, loaded. The endpoint embeds a text to
    /// a vector of word hashes, so texts that share words point the same way. Each request
    /// adds its input count to the returned counter.
    pub(crate) async fn word_embedder(
        server: &mut mockito::ServerGuard,
        name: &str,
    ) -> (String, std::sync::Arc<std::sync::atomic::AtomicUsize>) {
        use std::sync::atomic::{AtomicUsize, Ordering};
        let embedded = std::sync::Arc::new(AtomicUsize::new(0));
        let counter = embedded.clone();
        server
            .mock("POST", "/v1/embeddings")
            .with_body_from_request(move |request| {
                let body: serde_json::Value =
                    serde_json::from_slice(request.body().unwrap()).unwrap();
                let inputs: Vec<String> = body["input"]
                    .as_array()
                    .map(|a| {
                        a.iter()
                            .map(|v| v.as_str().unwrap_or_default().to_string())
                            .collect()
                    })
                    .unwrap_or_else(|| vec![body["input"].as_str().unwrap_or_default().into()]);
                counter.fetch_add(inputs.len(), Ordering::SeqCst);
                serde_json::json!({
                    "data": inputs
                        .iter()
                        .enumerate()
                        .map(|(i, text)| serde_json::json!({ "index": i, "embedding": word_vector(text) }))
                        .collect::<Vec<_>>(),
                })
                .to_string()
                .into_bytes()
            })
            .expect_at_least(0)
            .create_async()
            .await;
        let endpoint = crate::types::Endpoint {
            id: uuid::Uuid::new_v4().to_string(),
            name: format!("{name}-server"),
            kind: crate::types::EndpointKind::LlamaServer,
            base_url: url::Url::parse(&server.url()).unwrap(),
            // mockito reuses the port of a dropped server; a key of its own keeps this
            // endpoint apart from one an earlier test saved for the same URL.
            api_key: crate::secret::Secret::new(uuid::Uuid::new_v4().to_string()),
            capabilities: vec![crate::types::AiCapability::Embeddings],
            max_num_ctx: None,
            request_timeout_secs: None,
            max_concurrent: None,
            managed: false,
            health: Default::default(),
        };
        crate::db::Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).unwrap();
        let id = crate::db::Ad4mDb::with_global_instance(|db| {
            db.add_model(&crate::types::ModelInput {
                name: name.into(),
                model_type: ModelType::Embedding,
                endpoint_id: endpoint.id.clone(),
                remote_model: "word-hash".into(),
                native_tools: None,
            })
        })
        .unwrap();
        crate::ai_service::AIService::global_instance()
            .await
            .unwrap()
            .reload_model(id.clone())
            .await
            .unwrap();
        (id, embedded)
    }

    /// 64 buckets, one count per lowercase word (its first three letters, so "calls" and
    /// "calling" meet).
    pub(crate) fn word_vector(text: &str) -> Vec<f32> {
        let mut v = vec![0.0f32; 64];
        for word in text
            .split(|c: char| !c.is_alphanumeric())
            .filter(|w| w.len() > 2)
        {
            let stem: String = word.to_lowercase().chars().take(3).collect();
            let h = stem
                .bytes()
                .fold(7u32, |h, b| h.wrapping_mul(31).wrapping_add(b as u32));
            v[(h % 64) as usize] += 1.0;
        }
        v
    }

    fn rows(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
        pairs
            .iter()
            .map(|(id, t)| (id.to_string(), t.to_string()))
            .collect()
    }

    #[tokio::test]
    async fn a_second_read_comes_from_the_cache_and_a_changed_title_is_embedded_again() {
        use std::sync::atomic::Ordering;
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::ai_service::AIService::init_global_instance()
            .await
            .unwrap();
        let mut server = mockito::Server::new_async().await;
        let (_, embedded) = word_embedder(&mut server, "cache-embed").await;

        let first = rows(&[("a", "Ship the release"), ("b", "Write the docs")]);
        let (v1, stats) = embed_rows("p", "cache-embed", &first).await.unwrap();
        assert_eq!(
            stats,
            CacheStats {
                cached: 0,
                embedded: 2
            }
        );
        assert_eq!(embedded.load(Ordering::SeqCst), 2);

        let (v2, stats) = embed_rows("p", "cache-embed", &first).await.unwrap();
        assert_eq!(
            stats,
            CacheStats {
                cached: 2,
                embedded: 0
            },
            "cache hit"
        );
        assert_eq!(embedded.load(Ordering::SeqCst), 2, "no new request");
        assert_eq!(v1, v2);

        // `b`'s title changed: its hash is not the stored one, so it is stale.
        let changed = rows(&[("a", "Ship the release"), ("b", "Plan the retreat")]);
        let (v3, stats) = embed_rows("p", "cache-embed", &changed).await.unwrap();
        assert_eq!(
            stats,
            CacheStats {
                cached: 1,
                embedded: 1
            },
            "stale on hash change"
        );
        assert_eq!(embedded.load(Ordering::SeqCst), 3);
        assert_eq!(v3[1], word_vector("Plan the retreat"));

        // The new vector replaced the old row: a read with the new title is a hit.
        let (_, stats) = embed_rows("p", "cache-embed", &changed).await.unwrap();
        assert_eq!(
            stats,
            CacheStats {
                cached: 2,
                embedded: 0
            }
        );

        // Another perspective has its own rows.
        let (_, stats) = embed_rows("q", "cache-embed", &first).await.unwrap();
        assert_eq!(
            stats,
            CacheStats {
                cached: 0,
                embedded: 2
            }
        );
    }

    #[tokio::test]
    async fn a_model_this_executor_does_not_have_is_an_error() {
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        let err = embed_rows("p", "missing", &rows(&[("a", "x")]))
            .await
            .unwrap_err();
        assert!(
            err.to_string().contains("no EMBEDDING model 'missing'"),
            "{err}"
        );
    }
}
