/// Common Prolog Pool Trait and Utilities
/// 
/// This module defines the shared interface and implementation for all Prolog pool types.
/// It eliminates code duplication between `PrologEnginePool` and `FilteredPrologPool`
/// while maintaining clean separation of concerns.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;
use futures::future::join_all;
use deno_core::anyhow::{anyhow, Error};
use scryer_prolog::Term;
use super::embedding_cache::EmbeddingCache;
use super::engine::PrologEngine;
use super::types::{QueryResolution, QueryResult};
use crate::types::DecoratedLinkExpression;

/// Interface for filtered Prolog pools
/// 
/// This trait defines the operations that different types of filtered pools should support.
/// Unlike the complete pool, filtered pools are responsible for their own filtering logic,
/// population strategies, and update handling.
/// 
/// ## Design Philosophy
/// - **Autonomy**: Each filtered pool type handles its own concerns
/// - **Specialization**: Different pool types can implement different filtering strategies
/// - **Delegation**: Complete pools delegate to filtered pools rather than managing them
/// - **Extensibility**: Easy to add new filtered pool types by implementing this trait
pub trait FilteredPool: Send + Sync {
    /// Get the filter identifier for this pool (e.g., "user123", "after:2023-01-01")
    fn filter_id(&self) -> String;
    
    /// Get a human-readable description of this pool type and filter
    fn pool_description(&self) -> String;
    
    /// Initialize the filtered pool with the given pool size
    async fn initialize(&self, pool_size: usize) -> Result<(), Error>;
    
    /// Populate this filtered pool with data from all_links
    /// 
    /// This is called by the complete pool when it has new data. The filtered pool
    /// is responsible for determining what subset of the data is relevant and
    /// populating its engines accordingly.
    async fn populate_from_complete_data(
        &self,
        module_name: String,
        all_links: &[DecoratedLinkExpression],
        neighbourhood_author: Option<String>,
    ) -> Result<(), Error>;
    
    /// Handle incremental updates from assert operations
    /// 
    /// This is called when new assert statements are added to the complete pool.
    /// The filtered pool should determine which assertions are relevant and apply them.
    async fn handle_incremental_update(
        &self,
        assert_statements: &[String],
    ) -> Result<(), Error>;
    
    /// Execute a query on this filtered pool
    async fn run_query(&self, query: String) -> Result<QueryResult, Error>;
    
    /// Execute a query on all engines in this filtered pool (for assertions)
    async fn run_query_all(&self, query: String) -> Result<(), Error>;
    
    /// Check if this filtered pool should handle the given subscription query
    /// 
    /// This allows different filtered pool types to decide whether they're
    /// the appropriate pool for a given query based on the query content.
    fn should_handle_query(&self, query: &str) -> bool;
    
    /// Clean up resources
    async fn drop_all(&self) -> Result<(), Error>;
}

/// Engine state that can be shared between different pool implementations
pub struct EnginePoolState {
    pub engines: Vec<Option<PrologEngine>>,
    pub current_facts: Option<Vec<String>>,
    pub current_all_links: Option<Vec<DecoratedLinkExpression>>,
    pub current_neighbourhood_author: Option<String>,
}

impl EnginePoolState {
    pub fn new(pool_size: usize) -> Self {
        Self {
            engines: Vec::with_capacity(pool_size),
            current_facts: None,
            current_all_links: None,
            current_neighbourhood_author: None,
        }
    }
}

/// Shared utilities for Prolog pool implementations
pub struct PoolUtils;

impl PoolUtils {
    /// Process embedding URLs in queries using the given cache
    pub async fn replace_embedding_url(query: String, embedding_cache: &Arc<RwLock<EmbeddingCache>>) -> String {
        use super::engine_pool::EMBEDDING_URL_RE;
        let mut cache = embedding_cache.write().await;
        EMBEDDING_URL_RE
            .replace_all(&query, |caps: &regex::Captures| {
                let url = &caps[1];
                let id = cache.get_or_create_id(url);
                format!("\"{}\"", id)
            })
            .to_string()
    }
    
    /// Process embedding URLs in query results using the given cache
    pub async fn replace_embedding_url_in_value_recursively(
        value: &mut Term, 
        embedding_cache: &Arc<RwLock<EmbeddingCache>>
    ) {
        let cache = embedding_cache.read().await;
        match value {
            Term::String(s) => {
                if let Some(url) = cache.get_vector_url(s) {
                    *value = Term::String(url);
                }
            }
            Term::Atom(s) => {
                if let Some(url) = cache.get_vector_url(s) {
                    *value = Term::Atom(url);
                }
            }
            Term::List(list) => {
                drop(cache); // Release cache lock before recursive calls
                for item in list {
                    Box::pin(Self::replace_embedding_url_in_value_recursively(item, embedding_cache)).await;
                }
            }
            _ => {}
        }
    }
    
    /// Preprocess program lines to handle embeddings
    pub async fn preprocess_program_lines(
        lines: Vec<String>, 
        embedding_cache: &Arc<RwLock<EmbeddingCache>>
    ) -> Vec<String> {
        let futures = lines.into_iter().map(|line| {
            let line_clone = line.clone();
            let cache_clone = embedding_cache.clone();
            async move {
                Self::replace_embedding_url(line_clone, &cache_clone).await
            }
        });
        join_all(futures).await.into_iter().collect()
    }
    
    /// Get valid engines from engine state with round-robin selection
    pub fn select_engine_round_robin<'a>(
        engines: &'a [Option<PrologEngine>], 
        next_engine: &Arc<AtomicUsize>
    ) -> Result<(usize, &'a PrologEngine), Error> {
        // Get all valid engines
        let valid_engines: Vec<_> = engines
            .iter()
            .enumerate()
            .filter_map(|(i, e)| e.as_ref().map(|engine| (i, engine)))
            .collect();
            
        if valid_engines.is_empty() {
            return Err(anyhow!("No valid Prolog engines available"));
        }

        // Round-robin selection
        let current = next_engine.fetch_add(1, Ordering::SeqCst);
        let idx = current % valid_engines.len();
        let (engine_idx, engine) = valid_engines[idx];
        
        Ok((engine_idx, engine))
    }
    
    /// Execute a query with standard error handling and embedding processing
    pub async fn execute_query_with_processing(
        query: String,
        engine: &PrologEngine,
        embedding_cache: &Arc<RwLock<EmbeddingCache>>
    ) -> Result<QueryResult, Error> {
        // Preprocess query
        let processed_query = Self::replace_embedding_url(query, embedding_cache).await;
        
        // Execute query
        let result = engine.run_query(processed_query).await?;
        
        // Postprocess result
        match result {
            Ok(mut query_resolution) => {
                if let QueryResolution::Matches(ref mut matches) = query_resolution {
                    join_all(matches.iter_mut().map(|m| {
                        join_all(m.bindings.iter_mut().map(|(_, value)| {
                            Self::replace_embedding_url_in_value_recursively(value, embedding_cache)
                        }))
                    }))
                    .await;
                }
                Ok(Ok(query_resolution))
            }
            Err(e) => Ok(Err(e))
        }
    }
    
    /// Standard error handling for engine failures
    pub async fn handle_engine_error(
        engine_state: &Arc<RwLock<EnginePoolState>>,
        engine_idx: usize,
        error: impl std::fmt::Display,
        query: &str,
        pool_name: &str,
    ) -> Result<QueryResult, Error> {
        log::error!("{} engine error: {}", pool_name, error);
        log::error!("when running query: {}", query);
        let mut engines = engine_state.write().await;
        engines.engines[engine_idx] = None;
        Err(anyhow!("{} engine failed and was invalidated: {}", pool_name, error))
    }
}

/// Trait for pools that manage engine state
pub trait EngineStateManager {
    /// Get access to the engine state
    fn engine_state(&self) -> &Arc<RwLock<EnginePoolState>>;
    
    /// Get access to the next engine counter
    fn next_engine(&self) -> &Arc<AtomicUsize>;
    
    /// Get access to the embedding cache
    fn embedding_cache(&self) -> &Arc<RwLock<EmbeddingCache>>;
    
    /// Get a descriptive name for this pool (for logging)
    fn pool_name(&self) -> String;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;

    #[tokio::test]
    async fn test_pool_utils_embedding_processing() {
        AgentService::init_global_test_instance();
        
        let cache = Arc::new(RwLock::new(EmbeddingCache::new()));
        let test_query = "test query without embeddings".to_string();
        
        // Test that normal queries pass through unchanged
        let result = PoolUtils::replace_embedding_url(test_query.clone(), &cache).await;
        assert_eq!(result, test_query);
        
        println!("✅ Pool utilities embedding processing test passed!");
    }
    
    #[tokio::test]
    async fn test_pool_utils_engine_selection() {
        let engines = vec![
            Some(PrologEngine::new()),
            None, // Failed engine
            Some(PrologEngine::new()),
        ];
        
        let next_engine = Arc::new(AtomicUsize::new(0));
        
        // Test round-robin selection skips failed engines
        let (idx1, _) = PoolUtils::select_engine_round_robin(&engines, &next_engine).unwrap();
        let (idx2, _) = PoolUtils::select_engine_round_robin(&engines, &next_engine).unwrap();
        
        // Should alternate between engines 0 and 2 (skipping 1 which is None)
        assert!(idx1 == 0 || idx1 == 2);
        assert!(idx2 == 0 || idx2 == 2);
        assert_ne!(idx1, idx2); // Should be different
        
        println!("✅ Pool utilities engine selection test passed!");
    }
} 