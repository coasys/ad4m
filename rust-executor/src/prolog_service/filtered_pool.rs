use deno_core::anyhow::{anyhow, Error};
use futures::future::join_all;
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

use super::embedding_cache::EmbeddingCache;
use super::engine::PrologEngine;
use super::pool_trait::{EnginePoolState, FilteredPool, PoolUtils};
use super::source_filtering;
use super::types::{QueryResolution, QueryResult};
use crate::perspectives::sdna::{get_data_facts, get_sdna_facts, get_static_infrastructure_facts};
use scryer_prolog::Term;
use std::collections::HashSet;
use std::time::Instant;

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
/// 2. Automatically populated with filtered facts during creation
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

impl FilteredPool for FilteredPrologPool {
    fn pool_description(&self) -> String {
        format!("Source-filtered pool for '{}'", self.source_filter)
    }

    /// Initialize the filtered pool by spawning the Prolog engines
    /// This creates the actual Prolog engine processes but does not populate them with data.
    /// Use the complete pool's `populate_filtered_pool_direct()` method to load filtered facts.
    async fn initialize(&self, pool_size: usize) -> Result<(), Error> {
        let mut engines = self.engine_state.write().await;
        for _ in 0..pool_size {
            let mut engine = PrologEngine::new();
            engine.spawn().await?;
            engines.engines.push(Some(engine));
        }
        Ok(())
    }

    async fn populate_from_complete_data(&self) -> Result<(), Error> {
        log::info!(
            "📊 FILTERED POPULATION: Starting population for pool '{}'",
            self.source_filter
        );

        // Create filtered facts using our own filtering logic
        let facts = self
            .create_filtered_facts()
            .await?;

        // Update all engines with the filtered facts

        log::info!(
            "📊 FILTERED UPDATE: Pool '{}' updating engines with {} facts",
            self.source_filter,
            facts.len()
        );

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
        let processed_facts =
            PoolUtils::preprocess_program_lines(facts, &self.embedding_cache).await;

        // Update all engines with the processed facts using references to avoid cloning
        let mut update_futures = Vec::new();
        for engine in engines.engines.iter().filter_map(|e| e.as_ref()) {
            let update_future =
                engine.load_module_string("facts", &processed_facts);
            update_futures.push(update_future);
        }

        let results = join_all(update_futures).await;
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                log::error!(
                    "Failed to update filtered pool '{}' engine {}: {}",
                    self.source_filter,
                    i,
                    e
                );
            }
        }

        log::info!(
            "📊 FILTERED POPULATION: Successfully populated pool '{}'",
            self.source_filter
        );
        Ok(())
    }

    async fn handle_incremental_update(&self, assert_statements: &[String]) -> Result<(), Error> {
        // Filter assert statements for relevance to this source filter
        let relevant_assertions = source_filtering::filter_assert_statements_for_source(
            assert_statements,
            &self.source_filter,
        );

        if !relevant_assertions.is_empty() {
            log::info!(
                "🔄 INCREMENTAL UPDATE: Pool '{}' - applying {} filtered assertions: {:?}",
                self.source_filter,
                relevant_assertions.len(),
                relevant_assertions
            );

            let filtered_query = format!("{}.", relevant_assertions.join(","));
            log::info!(
                "🔄 INCREMENTAL UPDATE: Pool '{}' - executing query: {}",
                self.source_filter,
                filtered_query
            );

            let result = self.run_query_all(filtered_query).await;
            match &result {
                Ok(()) => log::info!(
                    "🔄 INCREMENTAL UPDATE: Pool '{}' - assertion execution successful",
                    self.source_filter
                ),
                Err(e) => log::info!(
                    "🔄 INCREMENTAL UPDATE: Pool '{}' - assertion execution failed: {}",
                    self.source_filter,
                    e
                ),
            }
            result
        } else {
            log::info!(
                "🔄 INCREMENTAL UPDATE: No relevant assertions for filtered pool '{}'",
                self.source_filter
            );
            Ok(())
        }
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
    async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (result, engine_idx) = {
            let engines = self.engine_state.read().await;
            let (engine_idx, engine) =
                PoolUtils::select_engine_round_robin(&engines.engines, &self.next_engine)?;
            let result = PoolUtils::execute_query_with_processing(
                query.clone(),
                engine,
                &self.embedding_cache,
            )
            .await;
            (result, engine_idx)
        };

        match result {
            Ok(result) => Ok(result),
            Err(e) => {
                let pool_name = format!("Filtered Pool '{}'", self.source_filter);
                PoolUtils::handle_engine_error(
                    &self.engine_state,
                    engine_idx,
                    e,
                    &query,
                    &pool_name,
                )
                .await
            }
        }
    }

    /// Execute a query on all engines in this filtered pool
    ///
    /// This is used for assert operations that need to update all engines consistently.
    /// It's typically called by the complete pool when propagating assert operations
    /// to filtered pools.
    async fn run_query_all(&self, query: String) -> Result<(), Error> {
        let engines = self.engine_state.write().await;
        let valid_engines: Vec<_> = engines.engines.iter().filter_map(|e| e.as_ref()).collect();

        if valid_engines.is_empty() {
            return Err(anyhow!(
                "No valid Prolog engines available in filtered pool '{}'",
                self.source_filter
            ));
        }

        // Preprocess query once for all engines
        let processed_query =
            PoolUtils::replace_embedding_url(query.clone(), &self.embedding_cache).await;

        let futures: Vec<_> = valid_engines
            .iter()
            .map(|engine| engine.run_query(processed_query.clone()))
            .collect();

        let results = join_all(futures).await;

        let mut errors = Vec::new();
        for result in results {
            match result? {
                Ok(QueryResolution::True) => continue,
                Ok(other) => log::info!(
                    "Filtered pool '{}' unexpected query result: {:?}",
                    self.source_filter,
                    other
                ),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            log::error!(
                "Filtered pool '{}' errors occurred while running queries: {}",
                self.source_filter,
                errors.join(", ")
            );
            return Err(anyhow!(
                "Filtered pool '{}' errors occurred while running queries: {}",
                self.source_filter,
                errors.join(", ")
            ));
        }

        Ok(())
    }

    fn should_handle_query(&self, query: &str) -> bool {
        // Check if this query mentions our source filter
        if let Some(extracted_source) = source_filtering::extract_source_filter(query) {
            extracted_source == self.source_filter
        } else {
            false
        }
    }

    async fn drop_all(&self) -> Result<(), Error> {
        let engines = self.engine_state.read().await;
        for engine in engines.engines.iter().filter_map(|e| e.as_ref()) {
            engine._drop()?;
        }
        Ok(())
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
        complete_pool: Arc<super::engine_pool::PrologEnginePool>,
    ) -> Self {
        Self {
            source_filter,
            engine_state: Arc::new(RwLock::new(EnginePoolState::new(pool_size))),
            next_engine: Arc::new(AtomicUsize::new(0)),
            embedding_cache: Arc::new(RwLock::new(EmbeddingCache::new())),
            complete_pool,
        }
    }

    /// Create filtered facts for this specific source filter
    async fn create_filtered_facts(&self) -> Result<Vec<String>, Error> {
        log::info!(
            "📊 FACT CREATION: Creating filtered facts for source: '{}'",
            self.source_filter
        );

        // Always include infrastructure and SDNA facts - these should never be filtered
        let mut filtered_lines = get_static_infrastructure_facts();
        let sdna_facts = {
                        // Use an existing engine from the complete pool - it already has all the data loaded!
            let complete_pool_state = self.complete_pool.engine_state().read().await;
            let all_links = complete_pool_state.current_all_links.as_ref().unwrap();
            let neighbourhood_author = complete_pool_state.current_neighbourhood_author.clone();
            get_sdna_facts(all_links, neighbourhood_author)?
        };
         

        log::info!(
            "📊 FACT CREATION: Infrastructure facts: {}, SDNA facts: {}",
            filtered_lines.len(),
            sdna_facts.len()
        );

        // Log sample SDNA facts for debugging
        if !sdna_facts.is_empty() {
            log::info!("📊 FACT CREATION: Sample SDNA facts (first 5):");
            for (i, fact) in sdna_facts.iter().take(5).enumerate() {
                log::info!("  {}. {}", i + 1, fact);
            }
        } else {
            log::warn!(
                "📊 FACT CREATION: No SDNA facts found - this might indicate missing SDNA data"
            );
        }

        filtered_lines.extend(sdna_facts);

        // Get filtered data facts (this applies reachability filtering only to data, not SDNA)
        let filtered_data = self.get_filtered_data_facts().await?;
        filtered_lines.extend(filtered_data);

        // Log sample of final facts for debugging
        if !filtered_lines.is_empty() {
            log::info!("📊 FACT CREATION: Sample final facts (first 10):");
            for (i, fact) in filtered_lines.iter().take(10).enumerate() {
                log::info!("  {}. {}", i + 1, fact);
            }
        }

        Ok(filtered_lines)
    }

    /// Get filtered data facts for this source using reachable query from the complete pool
    async fn get_filtered_data_facts(
        &self,
        //all_links: &[DecoratedLinkExpression],
    ) -> Result<Vec<String>, Error> {
        let start_time = Instant::now();
        log::info!(
            "🔍 FILTERING: Starting get_filtered_data_facts for source: '{}'",
            self.source_filter
        );
        

        // Use an existing engine from the complete pool - it already has all the data loaded!
        let complete_engines = self.complete_pool.engine_state().read().await;
        let engine = complete_engines
            .engines
            .iter()
            .find_map(|e| e.as_ref())
            .ok_or_else(|| anyhow!("No engines available in complete pool"))?;
        let all_links = complete_engines.current_all_links.as_ref().unwrap();
        log::info!("🔍 FILTERING: Total links provided: {}", all_links.len());

        // Get all data facts that we want to filter
        let facts_start = Instant::now();
        let all_data_facts = get_data_facts(all_links.as_ref());
        let facts_duration = facts_start.elapsed();
        log::info!(
            "🔍 FILTERING: Generated {} data facts in {:?}",
            all_data_facts.len(),
            facts_duration
        );

        // ⏱️ MEASURE: Query the complete engine for reachable nodes
        let reachability_start = Instant::now();

        // ⚡ OPTIMIZATION: Use findall with aggressive limits to prevent performance explosion
        let reachable_query = format!(
            r#"findall(Target, reachable("{}", Target), Targets)."#,
            self.source_filter
        );
        log::info!(
            "🔍 FILTERING: Running optimized findall reachable query: {}",
            reachable_query
        );

        // Set a timeout to prevent infinite loops
        let query_timeout = tokio::time::Duration::from_secs(10); // 10 second timeout
        let result = tokio::time::timeout(query_timeout, engine.run_query(reachable_query)).await;

        let reachability_duration = reachability_start.elapsed();
        let mut reachable_nodes = vec![self.source_filter.clone()]; // Include the source itself

        match result {
            Ok(Ok(Ok(QueryResolution::Matches(matches)))) => {
                log::info!(
                    "🔍 FILTERING: Found {} findall matches in {:?}",
                    matches.len(),
                    reachability_duration
                );

                // findall returns a list of targets in the "Targets" binding
                for m in matches {
                    if let Some(Term::List(targets)) = m.bindings.get("Targets") {
                        // Use HashSet for deduplication and apply size limits
                        let mut seen = HashSet::new();
                        const MAX_REACHABLE_NODES: usize = 5000; // Conservative limit

                        for target in targets {
                            if seen.len() >= MAX_REACHABLE_NODES {
                                log::warn!("🔍 FILTERING: Reached maximum reachable nodes limit ({}), stopping", MAX_REACHABLE_NODES);
                                break;
                            }

                            let target_str = match target {
                                Term::String(s) => s.clone(),
                                Term::Atom(s) => s.clone(),
                                _ => continue,
                            };

                            if seen.insert(target_str.clone()) {
                                reachable_nodes.push(target_str);
                            }
                        }
                        log::info!(
                            "🔍 FILTERING: Deduplicated to {} unique targets",
                            seen.len()
                        );
                        break; // Only process first match
                    }
                }
            }
            Ok(Ok(Ok(_))) => {
                log::warn!("🔍 FILTERING: Findall reachable query returned unexpected result type");
            }
            Ok(Ok(Err(e))) => {
                log::warn!("🔍 FILTERING: Findall reachable query failed: {}", e);
            }
            Ok(Err(e)) => {
                log::warn!("🔍 FILTERING: Engine error during reachable query: {}", e);
            }
            Err(_) => {
                log::error!("🔍 FILTERING: Reachable query timed out after 10 seconds! Using source-only fallback.");
                // Don't try any more reachability queries - just use the source node itself
                // This prevents the system from hanging completely
            }
        }

        log::info!(
            "🔍 FILTERING: Total reachable nodes: {}",
            reachable_nodes.len()
        );
        log::info!(
            "🔍 FILTERING: Reachability query took: {:?}",
            reachability_duration
        );

        // ⚡ OPTIMIZATION: Use regex-based filtering with automatic chunking for large pattern sets
        let formatting_start = Instant::now();
        let reachable_node_patterns: Vec<String> = reachable_nodes
            .iter()
            .map(|node| format!(r#""{}"#, node))
            .collect();

        let pattern_count = reachable_node_patterns.len();

        // ⚡ SMART CHUNKING: Build regex chunks to avoid size limits
        let regex_chunks = if pattern_count == 0 {
            Vec::new() // Empty - no filtering needed
        } else {
            const MAX_PATTERNS_PER_CHUNK: usize = 50; // Conservative limit to avoid regex size explosion
            let mut chunks = Vec::new();

            for chunk in reachable_node_patterns.chunks(MAX_PATTERNS_PER_CHUNK) {
                let escaped_patterns: Vec<String> =
                    chunk.iter().map(|pattern| regex::escape(pattern)).collect();
                let alternation_pattern = format!("({})", escaped_patterns.join("|"));

                match regex::Regex::new(&alternation_pattern) {
                    Ok(regex) => chunks.push(regex),
                    Err(e) => {
                        log::error!(
                            "🔍 FILTERING: Failed to compile regex chunk with {} patterns: {}",
                            chunk.len(),
                            e
                        );
                        return Err(Error::msg(format!("Failed to compile regex chunk: {}", e)));
                    }
                }
            }

            chunks
        };

        let formatting_duration = formatting_start.elapsed();
        log::info!(
            "🔍 FILTERING: Compiled {} regex chunks for {} patterns in {:?}",
            regex_chunks.len(),
            pattern_count,
            formatting_duration
        );

        // ⚡ ULTRA-FAST FILTERING: Apply regex chunks
        let filtering_start = Instant::now();
        let original_facts_count = all_data_facts.len();

        let filtered_data_facts: Vec<String> = if regex_chunks.is_empty() {
            log::info!("🔍 FILTERING: No patterns - returning empty filtered facts");
            Vec::new()
        } else if regex_chunks.len() == 1 {
            // Single regex chunk - use optimized single regex path
            let regex = &regex_chunks[0];
            if original_facts_count > 10000 {
                source_filtering::filter_facts_parallel_regex(all_data_facts, regex)
            } else {
                source_filtering::filter_facts_sequential_regex(all_data_facts, regex)
            }
        } else {
            // Multiple regex chunks - check each fact against all chunks
            if original_facts_count > 10000 {
                source_filtering::filter_facts_parallel_chunked_regex(all_data_facts, &regex_chunks)
            } else {
                source_filtering::filter_facts_sequential_chunked_regex(
                    all_data_facts,
                    &regex_chunks,
                )
            }
        };

        let filtering_duration = filtering_start.elapsed();
        let total_duration = start_time.elapsed();

        // Performance summary
        log::info!(
            "🔍 FILTERING: ⚡ PERFORMANCE SUMMARY for source '{}':",
            self.source_filter
        );
        log::info!("🔍 FILTERING:   📊 Data generation: {:?}", facts_duration);
        log::info!(
            "🔍 FILTERING:   🔍 Reachability query: {:?}",
            reachability_duration
        );
        log::info!(
            "🔍 FILTERING:   🔧 Pattern formatting: {:?}",
            formatting_duration
        );
        log::info!(
            "🔍 FILTERING:   ⚡ Facts filtering: {:?}",
            filtering_duration
        );
        log::info!(
            "🔍 FILTERING:   🎯 Total filtering time: {:?}",
            total_duration
        );

        log::info!(
            "🔍 FILTERING: Results: {} → {} facts ({:.1}% reduction, {} facts/sec)",
            original_facts_count,
            filtered_data_facts.len(),
            (1.0 - (filtered_data_facts.len() as f64 / original_facts_count as f64)) * 100.0,
            (original_facts_count as f64 / filtering_duration.as_secs_f64()) as u64
        );

        // Log sample of filtered facts (only first few)
        if !filtered_data_facts.is_empty() {
            log::info!("🔍 FILTERING: Sample of filtered facts (first 5):");
            for (i, fact) in filtered_data_facts.iter().take(5).enumerate() {
                log::info!("  {}. {}", i + 1, fact);
            }
        }

        Ok(filtered_data_facts)
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
        assert_eq!(filtered_pool.source_filter, "test_user");

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
