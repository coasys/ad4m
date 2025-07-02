/// Filtered Prolog Engine Pool
/// 
/// This module contains the `FilteredPrologPool` type which represents a pool of Prolog engines
/// that contain only data reachable from a specific source node. Filtered pools are created by
/// complete pools to optimize subscription queries that only need a subset of the data.
/// 
/// ## Architecture
/// - `FilteredPrologPool` contains its own engines but points back to a complete pool for data
/// - Complete pools manage creation, population, and updates of filtered pools
/// - Filtered pools focus on efficient query execution on filtered data
/// - Each filtered pool is associated with a specific source filter (e.g., "user123")

use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::RwLock;
use futures::future::join_all;
use deno_core::anyhow::{anyhow, Error};

use super::embedding_cache::EmbeddingCache;
use super::engine::PrologEngine;
use super::types::{QueryResolution, QueryResult};
use super::pool_trait::{PrologPool, EngineStateManager, EnginePoolState, PoolUtils};



/// A filtered Prolog engine pool that contains only data reachable from a specific source
/// 
/// ## Purpose
/// Filtered pools are optimized for subscription queries that only need data reachable from
/// a specific source node (e.g., all data reachable from "user123"). They provide:
/// 
/// 1. **Performance**: Smaller fact set means faster query execution
/// 2. **Isolation**: Each subscription sees only relevant data
/// 3. **Scalability**: Multiple filtered pools can run in parallel
/// 
/// ## Lifecycle
/// 1. Created by complete pool via `get_or_create_filtered_pool()`
/// 2. Populated with filtered facts via complete pool's `populate_filtered_pool_direct()`
/// 3. Updated when new assert queries affect the filtered data
/// 4. Queries are routed to filtered pools for efficiency
#[derive(Clone)]
pub struct FilteredPrologPool {
    /// The specific source filter this pool represents (e.g., "user123")
    source_filter: String,
    
    /// Combined lock for engines and current state to prevent deadlocks
    engine_state: Arc<RwLock<EnginePoolState>>,
    
    /// Round-robin index for engine selection
    next_engine: Arc<AtomicUsize>,
    
    /// Shared embedding cache for URL replacements
    embedding_cache: Arc<RwLock<EmbeddingCache>>,
    
    /// Reference back to the complete pool for data access and reachability queries
    /// This allows filtered pools to delegate complex operations to the complete pool
    complete_pool: Arc<super::engine_pool::PrologEnginePool>,
}

impl EngineStateManager for FilteredPrologPool {
    fn engine_state(&self) -> &Arc<RwLock<EnginePoolState>> {
        &self.engine_state
    }
    
    fn next_engine(&self) -> &Arc<AtomicUsize> {
        &self.next_engine
    }
    
    fn embedding_cache(&self) -> &Arc<RwLock<EmbeddingCache>> {
        &self.embedding_cache
    }
    
    fn pool_name(&self) -> String {
        format!("Filtered Pool '{}'", self.source_filter)
    }
}

impl PrologPool for FilteredPrologPool {
    async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        // Delegate to the existing implementation
        self.run_query(query).await
    }
    
    async fn run_query_all(&self, query: String) -> Result<(), Error> {
        // Delegate to the existing implementation
        self.run_query_all(query).await
    }
    
    async fn initialize(&self, pool_size: usize) -> Result<(), Error> {
        // Delegate to the existing implementation
        self.initialize(pool_size).await
    }
    
    async fn update_all_engines_with_facts(
        &self,
        module_name: String,
        facts: Vec<String>,
    ) -> Result<(), Error> {
        // Delegate to the existing implementation
        self.update_all_engines_with_facts(module_name, facts).await
    }
    
    async fn _drop_all(&self) -> Result<(), Error> {
        // Delegate to the existing implementation
        self._drop_all().await
    }
}

impl FilteredPrologPool {
    /// Create a new filtered pool for the given source filter
    /// 
    /// ## Arguments
    /// - `pool_size`: Number of Prolog engines to create in this filtered pool
    /// - `source_filter`: The source node this pool should filter by (e.g., "user123")
    /// - `complete_pool`: Reference to the complete pool for data access
    /// 
    /// ## Note
    /// This only creates the structure - you must call `initialize()` to spawn the engines
    /// and then use the complete pool's methods to populate it with filtered data.
    pub fn new(
        pool_size: usize, 
        source_filter: String, 
        complete_pool: Arc<super::engine_pool::PrologEnginePool>
    ) -> Self {
        Self {
            source_filter,
            engine_state: Arc::new(RwLock::new(EnginePoolState::new(pool_size))),
            next_engine: Arc::new(AtomicUsize::new(0)),
            embedding_cache: Arc::new(RwLock::new(EmbeddingCache::new())),
            complete_pool,
        }
    }
    
    /// Get the source filter this pool represents
    pub fn source_filter(&self) -> &str {
        &self.source_filter
    }
    
    /// Initialize the filtered pool by spawning the Prolog engines
    /// 
    /// This creates the actual Prolog engine processes but does not populate them with data.
    /// Use the complete pool's `populate_filtered_pool_direct()` method to load filtered facts.
    pub async fn initialize(&self, pool_size: usize) -> Result<(), Error> {
        let mut engines = self.engine_state.write().await;
        for _ in 0..pool_size {
            let mut engine = PrologEngine::new();
            engine.spawn().await?;
            engines.engines.push(Some(engine));
        }
        Ok(())
    }
    

    
    /// Execute a query on this filtered pool
    /// 
    /// This is the main query execution method for filtered pools. It automatically:
    /// 1. Processes embedding URLs in queries and results
    /// 2. Uses round-robin engine selection for load balancing
    /// 3. Handles engine failures gracefully
    /// 
    /// ## Performance Notes
    /// Filtered pools should execute queries faster than complete pools because they
    /// contain a smaller subset of facts, leading to more efficient Prolog execution.
    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (result, engine_idx) = {
            let engines = self.engine_state.read().await;
            let (engine_idx, engine) = PoolUtils::select_engine_round_robin(&engines.engines, &self.next_engine)?;
            let result = PoolUtils::execute_query_with_processing(query.clone(), engine, &self.embedding_cache).await;
            (result, engine_idx)
        };

        match result {
            Ok(result) => Ok(result),
            Err(e) => PoolUtils::handle_engine_error(&self.engine_state, engine_idx, e, &query, &self.pool_name()).await,
        }
    }
    
    /// Execute a query on all engines in this filtered pool
    /// 
    /// This is used for assert operations that need to update all engines consistently.
    /// It's typically called by the complete pool when propagating assert operations
    /// to filtered pools.
    pub async fn run_query_all(&self, query: String) -> Result<(), Error> {
        let engines = self.engine_state.write().await;
        let valid_engines: Vec<_> = engines.engines.iter().filter_map(|e| e.as_ref()).collect();

        if valid_engines.is_empty() {
            return Err(anyhow!("No valid Prolog engines available in filtered pool '{}'", self.source_filter));
        }

        // Preprocess query once for all engines
        let processed_query = PoolUtils::replace_embedding_url(query.clone(), &self.embedding_cache).await;

        let futures: Vec<_> = valid_engines
            .iter()
            .map(|engine| engine.run_query(processed_query.clone()))
            .collect();

        let results = join_all(futures).await;

        let mut errors = Vec::new();
        for result in results {
            match result? {
                Ok(QueryResolution::True) => continue,
                Ok(other) => log::info!("Filtered pool '{}' unexpected query result: {:?}", self.source_filter, other),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            log::error!(
                "Filtered pool '{}' errors occurred while running queries: {}",
                self.source_filter, errors.join(", ")
            );
            return Err(anyhow!(
                "Filtered pool '{}' errors occurred while running queries: {}",
                self.source_filter, errors.join(", ")
            ));
        }

        Ok(())
    }
    
    /// Update engines with pre-computed facts (called by complete pool)
    /// 
    /// This method is called by the complete pool when it has prepared filtered facts
    /// for this pool. It's the main way filtered pools receive their data.
    /// 
    /// ## Arguments
    /// - `module_name`: Name of the Prolog module to load (usually "facts")
    /// - `facts`: Pre-filtered facts ready to load into engines
    /// 
    /// ## Note
    /// The facts should already be filtered for this pool's source filter by the complete pool.
    pub async fn update_all_engines_with_facts(
        &self,
        module_name: String,
        facts: Vec<String>,
    ) -> Result<(), Error> {
        log::info!("📊 FILTERED UPDATE: Pool '{}' updating engines with {} facts", 
            self.source_filter, facts.len());
            
        let mut engines = self.engine_state.write().await;

        // Reinitialize any invalid engines
        for engine_slot in engines.engines.iter_mut() {
            if engine_slot.is_none() {
                let mut new_engine = PrologEngine::new();
                new_engine.spawn().await?;
                *engine_slot = Some(new_engine);
            }
        }

        // Preprocess the facts to handle embeddings
        let processed_facts = PoolUtils::preprocess_program_lines(facts, &self.embedding_cache).await;

        // Update all engines with the processed facts
        let mut update_futures = Vec::new();
        for engine in engines.engines.iter().filter_map(|e| e.as_ref()) {
            let update_future =
                engine.load_module_string(module_name.clone(), processed_facts.clone());
            update_futures.push(update_future);
        }

        let results = join_all(update_futures).await;
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                log::error!("Failed to update filtered pool '{}' engine {}: {}", self.source_filter, i, e);
            }
        }

        log::info!("📊 FILTERED UPDATE: Pool '{}' successfully updated all engines", self.source_filter);
        Ok(())
    }
    
    /// Drop all engines (cleanup method)
    pub async fn _drop_all(&self) -> Result<(), Error> {
        let engines = self.engine_state.read().await;
        for engine in engines.engines.iter().filter_map(|e| e.as_ref()) {
            engine._drop()?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    //! Tests for Filtered Prolog Engine Pool
    //! 
    //! These tests focus on:
    //! - Basic filtered pool functionality (creation, initialization, query execution)
    //! - Filtered pool isolation and independence
    //! - Engine management within filtered pools
    //! 
    //! For tests involving the interaction between complete pools and filtered pools,
    //! see engine_pool.rs tests.
    
    use super::*;
    use crate::agent::AgentService;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_filtered_pool_creation() {
        // This test validates the basic creation and initialization of filtered pools
        let complete_pool = Arc::new(super::super::engine_pool::PrologEnginePool::new(2));
        complete_pool.initialize(2).await.unwrap();
        
        let filtered_pool = FilteredPrologPool::new(2, "test_user".to_string(), complete_pool);
        assert_eq!(filtered_pool.source_filter(), "test_user");
        
        // Initialize the filtered pool
        filtered_pool.initialize(2).await.unwrap();
        
        // Test basic query on empty pool (should work but return false)
        let result = filtered_pool.run_query("false.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::False));
    }
    
    #[tokio::test]
    async fn test_filtered_pool_basic_functionality() {
        AgentService::init_global_test_instance();
        
        // Create complete pool and filtered pool
        let complete_pool = Arc::new(super::super::engine_pool::PrologEnginePool::new(2));
        complete_pool.initialize(2).await.unwrap();
        
        let filtered_pool = FilteredPrologPool::new(2, "user1".to_string(), complete_pool);
        filtered_pool.initialize(2).await.unwrap();
        
        // Test that we can run basic Prolog queries
        let result = filtered_pool.run_query("true.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::True));
        
        let result = filtered_pool.run_query("false.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::False));
        
        println!("✅ Filtered pool basic functionality test passed!");
        println!("✅ Filtered pool can execute basic Prolog queries");
        println!("✅ Filtered pool properly handles query results");
    }
} 