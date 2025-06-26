use super::embedding_cache::EmbeddingCache;
use super::engine::PrologEngine;
use super::types::{QueryResolution, QueryResult};
use deno_core::anyhow::{anyhow, Error};
use futures::future::join_all;
use lazy_static::lazy_static;
use regex::Regex;
use scryer_prolog::Term;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

pub const EMBEDDING_LANGUAGE_HASH: &str = "QmzSYwdbqjGGbYbWJvdKA4WnuFwmMx3AsTfgg7EwbeNUGyE555c";

lazy_static! {
    // Match embedding vector URLs inside string literals (both single and double quotes)
    static ref EMBEDDING_URL_RE: Regex = Regex::new(format!(r#"['"]({}://[^'"]*)['"]"#, EMBEDDING_LANGUAGE_HASH).as_str()).unwrap();
}

#[derive(Clone, Debug)]
pub enum EnginePoolType {
    Complete,
    FilteredBySource(String),
}

pub struct PrologEnginePool {
    engines: Arc<RwLock<Vec<Option<PrologEngine>>>>,
    next_engine: Arc<AtomicUsize>,
    embedding_cache: Arc<RwLock<EmbeddingCache>>,
    pool_type: EnginePoolType,
    filtered_pools: Arc<RwLock<HashMap<String, PrologEnginePool>>>,
}

impl PrologEnginePool {
    pub fn new(pool_size: usize) -> Self {
        PrologEnginePool {
            engines: Arc::new(RwLock::new(Vec::with_capacity(pool_size))),
            next_engine: Arc::new(AtomicUsize::new(0)),
            embedding_cache: Arc::new(RwLock::new(EmbeddingCache::new())),
            pool_type: EnginePoolType::Complete,
            filtered_pools: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn new_filtered(pool_size: usize, source_filter: String) -> Self {
        PrologEnginePool {
            engines: Arc::new(RwLock::new(Vec::with_capacity(pool_size))),
            next_engine: Arc::new(AtomicUsize::new(0)),
            embedding_cache: Arc::new(RwLock::new(EmbeddingCache::new())),
            pool_type: EnginePoolType::FilteredBySource(source_filter),
            filtered_pools: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn initialize(&self, pool_size: usize) -> Result<(), Error> {
        let mut engines = self.engines.write().await;
        for _ in 0..pool_size {
            let mut engine = PrologEngine::new();
            engine.spawn().await?;
            engines.push(Some(engine));
        }
        Ok(())
    }

    async fn replace_embedding_url(&self, query: String) -> String {
        let mut cache = self.embedding_cache.write().await;
        EMBEDDING_URL_RE
            .replace_all(&query, |caps: &regex::Captures| {
                let url = &caps[1];
                let id = cache.get_or_create_id(url);
                format!("\"{}\"", id)
            })
            .to_string()
    }

    async fn replace_embedding_url_in_value_recursively(&self, value: &mut Term) {
        let cache = self.embedding_cache.read().await;
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
                for item in list {
                    Box::pin(self.replace_embedding_url_in_value_recursively(item)).await;
                }
            }
            _ => {}
        }
    }

    // Helper to preprocess program lines before loading into engines
    async fn preprocess_program_lines(&self, lines: Vec<String>) -> Vec<String> {
        let futures = lines.into_iter().map(|line| {
            let line_clone = line.clone();
            async move {
                let new_line = self.replace_embedding_url(line_clone).await;
                new_line
            }
        });
        join_all(futures).await.into_iter().collect()
    }

    async fn handle_engine_error(
        &self,
        engine_idx: usize,
        error: impl std::fmt::Display,
        query: &str,
    ) -> Result<QueryResult, Error> {
        log::error!("Prolog engine error: {}", error);
        log::error!("when running query: {}", query);
        let mut engines = self.engines.write().await;
        engines[engine_idx] = None;
        Err(anyhow!("Engine failed and was invalidated: {}", error))
    }

    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (result, engine_idx) = {
            let engines = self.engines.read().await;

            // Get a vec with all non-None (invalidated) engines
            let valid_engines: Vec<_> = engines
                .iter()
                .enumerate()
                .filter_map(|(i, e)| e.as_ref().map(|engine| (i, engine)))
                .collect();
            if valid_engines.is_empty() {
                log::error!("No valid Prolog engines available");
                return Err(anyhow!("No valid Prolog engines available"));
            }

            // Round-robin selection of engine
            let current = self.next_engine.fetch_add(1, Ordering::SeqCst);
            let idx = current % valid_engines.len();
            let (engine_idx, engine) = valid_engines[idx];

            // Preprocess query to replace huge vector URLs with small cache IDs
            let processed_query = self.replace_embedding_url(query.clone()).await;

            // Run query
            let result = engine.run_query(processed_query.clone()).await;
            (result, engine_idx)
        };

        let result = match result {
            // Outer Result is an error -> engine panicked
            Err(e) => self.handle_engine_error(engine_idx, e, &query).await,
            // Inner Result is an error -> query failed
            Ok(Err(e)) => Ok(Err(e)),
            // Inner Result is a QueryResolution -> query succeeded
            Ok(Ok(mut result)) => {
                // Postprocess result to replace small cache IDs with huge vector URLs
                // In-place and async/parallel processing of all values in all matches
                if let QueryResolution::Matches(ref mut matches) = result {
                    join_all(matches.iter_mut().map(|m| {
                        join_all(m.bindings.iter_mut().map(|(_, value)| {
                            self.replace_embedding_url_in_value_recursively(value)
                        }))
                    }))
                    .await;
                }
                Ok(Ok(result))
            }
        };

        result.map_err(|e| anyhow!("{}", e))
    }

    pub async fn run_query_all(&self, query: String) -> Result<(), Error> {
        let engines = self.engines.write().await;
        let valid_engines: Vec<_> = engines.iter().filter_map(|e| e.as_ref()).collect();

        if valid_engines.is_empty() {
            return Err(anyhow!("No valid Prolog engines available"));
        }

        // Preprocess query once for all engines
        let processed_query = self.replace_embedding_url(query.clone()).await;

        let futures: Vec<_> = valid_engines
            .iter()
            .map(|engine| engine.run_query(processed_query.clone()))
            .collect();

        let results = join_all(futures).await;

        let mut errors = Vec::new();
        for result in results {
            match result? {
                Ok(QueryResolution::True) => continue,
                Ok(other) => log::info!("Unexpected query result: {:?}", other),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            log::error!(
                "Errors occurred while running queries: {}",
                errors.join(", ")
            );
            Err(anyhow!(
                "Errors occurred while running queries: {}",
                errors.join(", ")
            ))
        } else {
            Ok(())
        }
    }

    pub async fn update_all_engines(
        &self,
        module_name: String,
        program_lines: Vec<String>,
    ) -> Result<(), Error> {
        let mut engines = self.engines.write().await;

        // Reinitialize any invalid engines
        for engine_slot in engines.iter_mut() {
            if engine_slot.is_none() {
                let mut new_engine = PrologEngine::new();
                new_engine.spawn().await?;
                *engine_slot = Some(new_engine);
            }
        }

        // Determine what facts to load based on pool type
        let facts_to_load = match &self.pool_type {
            EnginePoolType::Complete => {
                // For complete pools, preprocess and load all facts
                self.preprocess_program_lines(program_lines.clone()).await
            }
            EnginePoolType::FilteredBySource(source_filter) => {
                // For filtered pools, only load facts reachable from the source
                let processed_lines = self.preprocess_program_lines(program_lines.clone()).await;
                self.get_filtered_facts_for_source(source_filter, &processed_lines).await?
            }
        };

        // Update all engines with facts
        let mut update_futures = Vec::new();
        for engine in engines.iter().filter_map(|e| e.as_ref()) {
            let update_future =
                engine.load_module_string(module_name.clone(), facts_to_load.clone());
            update_futures.push(update_future);
        }

        let results = join_all(update_futures).await;
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                log::error!("Failed to update Prolog engine {}: {}", i, e);
            }
        }

        // If this is a complete pool, also update any filtered sub-pools
        if matches!(self.pool_type, EnginePoolType::Complete) {
            let filtered_pools = self.filtered_pools.read().await;
            let update_futures: Vec<_> = filtered_pools.values().map(|pool| {
                pool.update_all_engines(module_name.clone(), program_lines.clone())
            }).collect();
            
            let results = join_all(update_futures).await;
            for (i, result) in results.into_iter().enumerate() {
                if let Err(e) = result {
                    log::error!("Failed to update filtered Prolog pool {}: {}", i, e);
                }
            }
        }

        Ok(())
    }

    pub async fn _drop_all(&self) -> Result<(), Error> {
        let engines = self.engines.read().await;
        for engine in engines.iter().filter_map(|e| e.as_ref()) {
            engine._drop()?;
        }
        Ok(())
    }

    /// Get or create a filtered pool for subscription queries with the given source filter
    pub async fn get_or_create_filtered_pool(&self, source_filter: String) -> Result<(), Error> {
        // Only complete pools can have filtered sub-pools
        if !matches!(self.pool_type, EnginePoolType::Complete) {
            return Err(anyhow!("Only complete pools can create filtered sub-pools"));
        }

        let mut filtered_pools = self.filtered_pools.write().await;
        
        if filtered_pools.contains_key(&source_filter) {
            return Ok(());
        }

        // Create new filtered pool with smaller size (2-3 engines should be enough for subscriptions)
        let filtered_pool = PrologEnginePool::new_filtered(3, source_filter.clone());
        filtered_pool.initialize(3).await?;
        
        filtered_pools.insert(source_filter.clone(), filtered_pool);
        Ok(())
    }

    /// Extract source filter from a Prolog query if it exists
    pub fn extract_source_filter(query: &str) -> Option<String> {
        // Look for patterns like triple("specific_source", _, _) or link("specific_source", _, _, _, _)
        let patterns = [
            regex::Regex::new(r#"triple\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
            regex::Regex::new(r#"link\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
            regex::Regex::new(r#"reachable\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
        ];

        for pattern in &patterns {
            if let Some(captures) = pattern.captures(query) {
                if let Some(source) = captures.get(1) {
                    let source_str = source.as_str();
                    // Only consider it a valid filter if it's not a variable (starts with uppercase or _)
                    if !source_str.starts_with(char::is_uppercase) && !source_str.starts_with('_') {
                        return Some(source_str.to_string());
                    }
                }
            }
        }
        None
    }

    /// Get filtered facts for a specific source using reachable query
    async fn get_filtered_facts_for_source(&self, source_filter: &str, all_facts: &[String]) -> Result<Vec<String>, Error> {
        // Create a temporary engine to run the reachable query
        let mut temp_engine = PrologEngine::new();
        temp_engine.spawn().await?;
        
        // Load all facts into temp engine
        temp_engine.load_module_string("temp_facts".to_string(), all_facts.to_vec()).await?;
        
        // Query for all nodes reachable from the source
        let reachable_query = format!(r#"reachable("{}", Target)"#, source_filter);
        let result = temp_engine.run_query(reachable_query).await?;
        
        let mut reachable_nodes = vec![source_filter.to_string()]; // Include the source itself
        
        if let Ok(QueryResolution::Matches(matches)) = result {
            for m in matches {
                if let Some(Term::String(target)) = m.bindings.get("Target") {
                    reachable_nodes.push(target.clone());
                } else if let Some(Term::Atom(target)) = m.bindings.get("Target") {
                    reachable_nodes.push(target.clone());
                }
            }
        }
        
        // Filter facts to only include those involving reachable nodes
        let mut filtered_facts = Vec::new();
        
        // Add the prolog setup facts (discontiguous, dynamic, etc.)
        for fact in all_facts {
            if fact.starts_with(":-") || fact.contains("discontiguous") || fact.contains("dynamic") {
                filtered_facts.push(fact.clone());
                continue;
            }
            
            // Check if this fact involves any of our reachable nodes
            let involves_reachable = reachable_nodes.iter().any(|node| {
                fact.contains(&format!(r#""{}"#, node))
            });
            
            if involves_reachable {
                filtered_facts.push(fact.clone());
            }
        }
        
        temp_engine._drop()?;
        Ok(filtered_facts)
    }

    /// Run a query with smart routing - use filtered pool if it's a subscription query with source filter
    pub async fn run_query_smart(&self, query: String, is_subscription: bool) -> Result<QueryResult, Error> {
        // If this is a subscription query and we can extract a source filter, try to use a filtered pool
        if is_subscription && matches!(self.pool_type, EnginePoolType::Complete) {
            if let Some(source_filter) = Self::extract_source_filter(&query) {
                log::debug!("Routing subscription query to filtered pool for source: {}", source_filter);
                
                // Ensure filtered pool exists
                if let Err(e) = self.get_or_create_filtered_pool(source_filter.clone()).await {
                    log::warn!("Failed to create filtered pool, falling back to complete pool: {}", e);
                    return self.run_query(query).await;
                }
                
                // Get the filtered pool and run query on it
                let filtered_pools = self.filtered_pools.read().await;
                if let Some(filtered_pool) = filtered_pools.get(&source_filter) {
                    return filtered_pool.run_query(query).await;
                }
            }
        }
        
        // Default to using this pool directly
        self.run_query(query).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use scryer_prolog::Term;
    #[tokio::test]
    async fn test_pool_initialization() {
        let pool = PrologEnginePool::new(3);
        assert!(pool.initialize(3).await.is_ok());

        let engines = pool.engines.read().await;
        assert_eq!(engines.len(), 3);
        assert!(engines.iter().all(|e| e.is_some()));
    }

    #[tokio::test]
    async fn test_run_query() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        // Test simple query
        let result = pool.run_query("true.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::True));

        // Test query that should fail
        let result = pool.run_query("false.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::False));

        // Test invalid query
        let result = pool
            .run_query("invalid_predicate(x).".to_string())
            .await
            .unwrap();
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_run_query_all() {
        let pool = PrologEnginePool::new(3);
        pool.initialize(3).await.unwrap();

        // Test simple query on all engines
        let result = pool.run_query_all("true.".to_string()).await;
        assert!(result.is_ok());

        // Test query that should fail on all engines
        let result = pool.run_query_all("broken query".to_string()).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_update_all_engines() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        // Load a simple fact into all engines
        let program = vec!["test_fact(a).".to_string()];
        let result = pool.update_all_engines("test".to_string(), program).await;
        assert!(result.is_ok());

        // Verify fact was loaded by querying
        let result = pool.run_query("test_fact(X).".to_string()).await.unwrap();
        match result {
            Ok(QueryResolution::Matches(matches)) => {
                assert_eq!(matches.len(), 1);
                assert_eq!(matches[0].bindings["X"], Term::Atom("a".to_string()));
            }
            _ => panic!("Expected matches"),
        }
    }

    #[tokio::test]
    async fn test_engine_failure_handling() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        // Force an engine failure with an invalid query
        let _ = pool.run_query("invalid_predicate(x).".to_string()).await;

        // Check that we still have valid engines
        let engines = pool.engines.read().await;
        let valid_count = engines.iter().filter(|e| e.is_some()).count();
        assert!(
            valid_count > 0,
            "Should still have valid engines after failure"
        );

        // Verify we can still run queries
        let result = pool.run_query("true.".to_string()).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_engine_recovery() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        // Force an engine failure
        let _ = pool.run_query("invalid_predicate(x).".to_string()).await;

        // Update all engines which should recover failed ones
        let program = vec!["test_fact(a).".to_string()];
        let result = pool.update_all_engines("test".to_string(), program).await;
        assert!(result.is_ok());

        // Verify all engines are valid
        let engines = pool.engines.read().await;
        assert!(engines.iter().all(|e| e.is_some()));
    }

    #[tokio::test]
    async fn test_source_filter_extraction() {
        // Test various query patterns to ensure source extraction works
        assert_eq!(
            PrologEnginePool::extract_source_filter(r#"triple("user123", "likes", Target)"#),
            Some("user123".to_string())
        );
        
        assert_eq!(
            PrologEnginePool::extract_source_filter(r#"link("user456", Predicate, Target, Timestamp, Author)"#),
            Some("user456".to_string())
        );
        
        assert_eq!(
            PrologEnginePool::extract_source_filter(r#"reachable("root_node", X)"#),
            Some("root_node".to_string())
        );
        
        // Should not extract variables (starting with uppercase or _)
        assert_eq!(
            PrologEnginePool::extract_source_filter(r#"triple(Source, "likes", Target)"#),
            None
        );
        
        assert_eq!(
            PrologEnginePool::extract_source_filter(r#"triple(_Source, "likes", Target)"#),
            None
        );
    }

    #[tokio::test]
    async fn test_filtered_pool_creation_and_routing() {
        let pool = PrologEnginePool::new(3);
        pool.initialize(3).await.unwrap();

        // Load some test facts into the main pool
        let facts = vec![
            "triple(\"user1\", \"likes\", \"item1\").".to_string(),
            "triple(\"user1\", \"owns\", \"item2\").".to_string(),
            "triple(\"user2\", \"likes\", \"item3\").".to_string(),
            "triple(\"item1\", \"type\", \"book\").".to_string(),
            "triple(\"item2\", \"type\", \"car\").".to_string(),
        ];
        pool.update_all_engines("test_facts".to_string(), facts.clone()).await.unwrap();

        // Test smart routing with subscription query
        let subscription_query = r#"triple("user1", Predicate, Target)"#.to_string();
        let result = pool.run_query_smart(subscription_query, true).await.unwrap();
        
        // Should find matches for user1's triples
        match result {
            Ok(QueryResolution::Matches(matches)) => {
                assert!(!matches.is_empty());
                // Should have matches for both "likes" and "owns" predicates
                assert!(matches.len() >= 2);
            }
            _ => panic!("Expected matches for user1 query"),
        }

        // Verify that a filtered pool was created
        let filtered_pools = pool.filtered_pools.read().await;
        assert!(filtered_pools.contains_key("user1"));
    }

    #[tokio::test]
    async fn test_filtered_pool_data_subset() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        // Load facts where only some are reachable from "user1"
        let facts = vec![
            "triple(\"user1\", \"likes\", \"item1\").".to_string(),
            "triple(\"item1\", \"type\", \"book\").".to_string(),
            "triple(\"user2\", \"likes\", \"item2\").".to_string(),
            "triple(\"item2\", \"type\", \"movie\").".to_string(),
        ];
        pool.update_all_engines("test_facts".to_string(), facts).await.unwrap();

        // Create a filtered pool for user1
        pool.get_or_create_filtered_pool("user1".to_string()).await.unwrap();
        
        // Query the filtered pool directly
        let filtered_pools = pool.filtered_pools.read().await;
        let filtered_pool = filtered_pools.get("user1").unwrap();
        
        // Query for all triples in the filtered pool
        let result = filtered_pool.run_query("triple(Source, Predicate, Target).".to_string()).await.unwrap();
        
        match result {
            Ok(QueryResolution::Matches(matches)) => {
                // Should only have triples reachable from user1
                // That would be: user1->item1, item1->book
                // Should NOT have user2->item2 or item2->movie
                for m in &matches {
                    let source = m.bindings.get("Source").unwrap();
                    // None of the matches should involve user2 or item2
                    if let Term::String(s) = source {
                        assert_ne!(s, "user2");
                        assert_ne!(s, "item2");
                    }
                }
            }
            _ => panic!("Expected matches in filtered pool"),
        }
    }

    #[tokio::test]
    async fn test_regular_queries_use_complete_pool() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        let facts = vec!["test_fact(a).".to_string()];
        pool.update_all_engines("test".to_string(), facts).await.unwrap();

        // Regular query (not a subscription) should use the complete pool
        let query = "test_fact(X).".to_string();
        let result = pool.run_query_smart(query, false).await.unwrap();
        
        match result {
            Ok(QueryResolution::Matches(matches)) => {
                assert_eq!(matches.len(), 1);
                assert_eq!(matches[0].bindings["X"], Term::Atom("a".to_string()));
            }
            _ => panic!("Expected matches"),
        }

        // No filtered pools should have been created
        let filtered_pools = pool.filtered_pools.read().await;
        assert!(filtered_pools.is_empty());
    }

    #[tokio::test]
    async fn test_drop_all() {
        let pool = PrologEnginePool::new(2);
        pool.initialize(2).await.unwrap();

        assert!(pool._drop_all().await.is_ok());

        // Verify engines are still in place but can be reinitialized
        let engines = pool.engines.read().await;
        assert_eq!(engines.len(), 2);
        assert!(engines.iter().all(|e| e.is_some()));
    }
}
