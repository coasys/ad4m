//! SDNA-Only Prolog Pool
//!
//! This module provides `SdnaPrologPool`, a specialized pool type for subject class queries
//! that only require infrastructure and SDNA facts (no link data). This is optimized for
//! queries during subject instance creation that retrieve construction and setter actions.
//!
//! ## Key Features
//! - Contains only infrastructure + SDNA facts (no link data)
//! - Handles queries with predicates: subject, constructor, property_setter, collection_adder, collection_remover, collection_setter
//! - No incremental updates needed (SDNA changes trigger full pool updates)
//! - Fast initialization and query execution for subject class operations

use anyhow::{anyhow, Error};
use futures::future::join_all;
use regex::Regex;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::engine::PrologEngine;
use super::pool_trait::{EmbeddingCache, EnginePoolState, FilteredPool, PoolUtils};
use super::types::{QueryResolution, QueryResult};
use crate::perspectives::sdna::{get_sdna_facts, get_static_infrastructure_facts};

/// SDNA-Only Prolog Pool for subject class queries
///
/// This pool type is optimized for queries during subject instance creation that only
/// need access to infrastructure and SDNA facts. It contains no link data, making it
/// much faster to populate and query than complete pools.
///
/// ## Use Cases
/// - Subject constructor queries
/// - Property setter action queries
/// - Collection manipulation action queries
/// - Any query that only uses: subject, constructor, property_setter, collection_adder, collection_remover, collection_setter predicates
///
/// ## Architecture
/// - Points back to complete pool for coordination
/// - Contains only infrastructure + SDNA facts (no links)
/// - No incremental updates (SDNA changes trigger full pool updates)
/// - Small pool size (2-3 engines) for efficiency
#[derive(Clone)]
pub struct SdnaPrologPool {
    /// Combined lock for engines and current state to prevent deadlocks
    engine_pool_state: Arc<RwLock<EnginePoolState>>,

    /// Round-robin index for engine selection
    next_engine: Arc<AtomicUsize>,

    /// Shared embedding cache for URL replacements
    embedding_cache: Arc<RwLock<EmbeddingCache>>,

    /// Reference back to the complete pool for data access
    /// This allows SDNA pools to get current links and neighbourhood author data
    complete_pool: Arc<super::engine_pool::PrologEnginePool>,
}

impl FilteredPool for SdnaPrologPool {
    fn pool_description(&self) -> String {
        "SDNA-only pool for subject class queries".to_string()
    }

    /// Initialize the SDNA pool by spawning the Prolog engines
    /// This creates the actual Prolog engine processes but does not populate them with data.
    async fn initialize(&self, pool_size: usize) -> Result<(), Error> {
        let mut pool_state = self.engine_pool_state.write().await;
        for _ in 0..pool_size {
            let mut engine = PrologEngine::new();
            engine.spawn().await?;
            pool_state.engines.push(Some(engine));
        }
        Ok(())
    }

    async fn populate_from_complete_data(&self) -> Result<(), Error> {
        log::debug!("📊 SDNA POPULATION: Starting population for SDNA-only pool");

        // Create SDNA-only facts (infrastructure + SDNA, no link data)
        let facts = self.create_sdna_only_facts().await?;

        log::debug!(
            "📊 SDNA UPDATE: Pool updating engines with {} facts",
            facts.len()
        );

        let mut pool_state = self.engine_pool_state.write().await;

        // Reinitialize any invalid engines
        for engine_slot in pool_state.engines.iter_mut() {
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
        for engine in pool_state.engines.iter().filter_map(|e| e.as_ref()) {
            let update_future = engine.load_module_string("facts", &processed_facts);
            update_futures.push(update_future);
        }

        let results = join_all(update_futures).await;
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                log::error!("Failed to update SDNA pool engine {}: {}", i, e);
            }
        }

        log::debug!("📊 SDNA POPULATION: Successfully populated SDNA-only pool");
        Ok(())
    }

    async fn handle_incremental_update(&self, _assert_statements: &[String]) -> Result<(), Error> {
        // SDNA pools don't need incremental updates - SDNA changes trigger full pool updates
        log::debug!("🔄 SDNA UPDATE: Skipping incremental update for SDNA-only pool (not needed)");
        Ok(())
    }

    /// Execute a query on this SDNA pool
    ///
    /// This is optimized for subject class queries that only need SDNA and infrastructure facts.
    /// Should be much faster than complete pool queries since it contains much less data.
    async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (result, engine_idx) = {
            let pool_state = self.engine_pool_state.read().await;
            let (engine_idx, engine) =
                PoolUtils::select_engine_round_robin(&pool_state.engines, &self.next_engine)?;
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
                let pool_name = "SDNA Pool".to_string();
                PoolUtils::handle_engine_error(
                    &self.engine_pool_state,
                    engine_idx,
                    e,
                    &query,
                    &pool_name,
                )
                .await
            }
        }
    }

    /// Execute a query on all engines in this SDNA pool
    ///
    /// This is used for assert operations that need to update all engines consistently.
    async fn run_query_all(&self, query: String) -> Result<(), Error> {
        let pool_state = self.engine_pool_state.write().await;
        let valid_engines: Vec<_> = pool_state
            .engines
            .iter()
            .filter_map(|e| e.as_ref())
            .collect();

        if valid_engines.is_empty() {
            return Err(anyhow!("No valid Prolog engines available in SDNA pool"));
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
                Ok(other) => log::warn!("SDNA pool unexpected query result: {:?}", other),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            log::error!(
                "SDNA pool errors occurred while running queries: {}",
                errors.join(", ")
            );
            return Err(anyhow!(
                "SDNA pool errors occurred while running queries: {}",
                errors.join(", ")
            ));
        }

        Ok(())
    }

    fn should_handle_query(&self, query: &str) -> bool {
        // Detect subject class queries that only need SDNA facts
        self.is_subject_class_query(query)
    }

    async fn drop_all(&self) -> Result<(), Error> {
        let pool_state = self.engine_pool_state.read().await;
        for engine in pool_state.engines.iter().filter_map(|e| e.as_ref()) {
            engine._drop()?;
        }
        Ok(())
    }
}

impl SdnaPrologPool {
    /// Create a new SDNA-only pool
    ///
    /// ## Arguments
    /// - `complete_pool`: Reference to the complete pool for coordination
    ///
    /// ## Note
    /// This only creates the structure - you must call `initialize()` to spawn the engines
    /// and then populate it with SDNA data.
    pub fn new(complete_pool: Arc<super::engine_pool::PrologEnginePool>) -> Self {
        Self {
            engine_pool_state: Arc::new(RwLock::new(EnginePoolState::new())),
            next_engine: Arc::new(AtomicUsize::new(0)),
            embedding_cache: Arc::new(RwLock::new(EmbeddingCache::new())),
            complete_pool,
        }
    }

    /// Check if a query is a subject class query that should be handled by this pool
    ///
    /// Subject class queries typically involve:
    /// - subject/3 predicate (subject class definitions)
    /// - constructor/2 predicate (constructor actions)
    /// - property_setter/3 predicate (setter actions)
    /// - collection_adder/3 predicate (collection addition actions)
    /// - collection_remover/3 predicate (collection removal actions)
    /// - collection_setter/3 predicate (collection setter actions)
    ///
    /// These queries are used during subject instance creation and don't need link data.
    fn is_subject_class_query(&self, query: &str) -> bool {
        // Look for key SDNA predicates that indicate this is a subject class query
        lazy_static::lazy_static! {
            static ref SUBJECT_CLASS_PREDICATES: Regex = Regex::new(
                r"(?i)\b(subject|constructor|property_setter|collection_adder|collection_remover|collection_setter)\s*\("
            ).unwrap();

            // Exclude queries that need link data
            static ref LINK_DATA_PREDICATES: Regex = Regex::new(
                r"(?i)\b(triple|link|has_child|reachable)\s*\("
            ).unwrap();

            // Exclude instance-specific queries that need link data
            static ref INSTANCE_PREDICATES: Regex = Regex::new(
                r"(?i)\b(instance|property_getter)\s*\("
            ).unwrap();
        }

        // Must contain subject class predicates
        let has_subject_class_predicates = SUBJECT_CLASS_PREDICATES.is_match(query);

        // Must not contain link data predicates
        let has_link_data_predicates = LINK_DATA_PREDICATES.is_match(query);

        // Must not contain instance predicates
        let has_instance_predicates = INSTANCE_PREDICATES.is_match(query);

        let is_subject_class =
            has_subject_class_predicates && !has_link_data_predicates && !has_instance_predicates;

        if is_subject_class {
            log::trace!("🎯 SDNA ROUTING: Detected subject class query: {}", query);
        }

        is_subject_class
    }

    /// Create SDNA-only facts (infrastructure + SDNA, no link data)
    async fn create_sdna_only_facts(&self) -> Result<Vec<String>, Error> {
        log::debug!("📊 SDNA FACT CREATION: Creating SDNA-only facts");

        // Always include infrastructure facts
        let mut sdna_lines = get_static_infrastructure_facts();

        // Get current data from complete pool state
        let (all_links, neighbourhood_author, owner_did) = {
            let complete_pool_state = self.complete_pool.engine_state().read().await;
            let all_links = complete_pool_state.current_all_links.as_ref()
                .ok_or_else(|| anyhow!(
                    "🚨 RACE CONDITION DETECTED: SDNA pool cannot create facts because parent pool not yet populated with data. \
                     This indicates SDNA pool population happened before parent pool data population."
                ))?;
            let neighbourhood_author = complete_pool_state.current_neighbourhood_author.clone();
            let owner_did = complete_pool_state.current_owner_did.clone();
            (all_links.clone(), neighbourhood_author, owner_did)
        };

        // Add SDNA facts (subject class definitions, constructors, setters, etc.)
        let sdna_facts = get_sdna_facts(&all_links, neighbourhood_author.clone(), owner_did)?;

        log::trace!(
            "📊 SDNA FACT CREATION: Infrastructure facts: {}, SDNA facts: {}",
            sdna_lines.len(),
            sdna_facts.len()
        );

        // Log sample SDNA facts for debugging
        if !sdna_facts.is_empty() {
            log::trace!("📊 SDNA FACT CREATION: Sample SDNA facts (first 5):");
            for (i, fact) in sdna_facts.iter().take(5).enumerate() {
                log::trace!("  {}. {}", i + 1, fact);
            }
        } else {
            log::trace!("📊 SDNA FACT CREATION: No SDNA facts found");
        }

        sdna_lines.extend(sdna_facts);

        log::trace!(
            "📊 SDNA FACT CREATION: Total SDNA-only facts: {} (no link data included)",
            sdna_lines.len()
        );

        Ok(sdna_lines)
    }
}

#[cfg(test)]
mod tests {
    //! Tests for SDNA-Only Prolog Pool
    //!
    //! These tests focus on:
    //! - Basic SDNA pool functionality (creation, initialization, query execution)
    //! - Subject class query detection and routing
    //! - SDNA-only fact loading (no link data)

    use super::*;
    use crate::agent::AgentService;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_sdna_pool_creation() {
        // This test validates the basic creation and initialization of SDNA pools
        let complete_pool = Arc::new(super::super::engine_pool::PrologEnginePool::new());
        complete_pool.initialize(2).await.unwrap();

        let sdna_pool = SdnaPrologPool::new(complete_pool);
        assert_eq!(
            sdna_pool.pool_description(),
            "SDNA-only pool for subject class queries"
        );

        // Initialize the SDNA pool
        sdna_pool.initialize(2).await.unwrap();

        // Test basic query on empty pool (should work but return false)
        let result = sdna_pool.run_query("false.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::False));
    }

    #[tokio::test]
    async fn test_subject_class_query_detection() {
        let complete_pool = Arc::new(super::super::engine_pool::PrologEnginePool::new());
        let sdna_pool = SdnaPrologPool::new(complete_pool);

        // Test subject class queries (should be handled)
        assert!(sdna_pool.should_handle_query("subject(Class, Properties, Methods)."));
        assert!(sdna_pool.should_handle_query("constructor(Class, Action)."));
        assert!(sdna_pool.should_handle_query("property_setter(Class, Property, Action)."));
        assert!(sdna_pool.should_handle_query("collection_adder(Class, Property, Action)."));
        assert!(sdna_pool.should_handle_query("collection_remover(Class, Property, Action)."));
        assert!(sdna_pool.should_handle_query("collection_setter(Class, Property, Action)."));

        // Test queries with link data (should NOT be handled)
        assert!(!sdna_pool.should_handle_query("triple(S, P, O)."));
        assert!(!sdna_pool.should_handle_query("has_child(Parent, Child)."));
        assert!(!sdna_pool.should_handle_query("reachable(Source, Target)."));

        // Test instance queries (should NOT be handled)
        assert!(!sdna_pool.should_handle_query("instance(Instance, Value)."));
        assert!(!sdna_pool.should_handle_query("property_getter(Instance, Property, Value)."));

        // Test mixed queries (should NOT be handled if they contain link data)
        assert!(!sdna_pool.should_handle_query("subject(Class, Props, Methods), triple(S, P, O)."));
    }

    #[tokio::test]
    async fn test_sdna_only_population() {
        AgentService::init_global_test_instance();
        let complete_pool = Arc::new(super::super::engine_pool::PrologEnginePool::new());
        complete_pool.initialize(2).await.unwrap();

        let sdna_pool = SdnaPrologPool::new(complete_pool.clone());
        sdna_pool.initialize(2).await.unwrap();

        // Create test links (these should be ignored for SDNA pool)
        use crate::types::{DecoratedLinkExpression, Link};
        let test_links = vec![DecoratedLinkExpression {
            author: "test_author".to_string(),
            timestamp: "2023-01-01T00:00:00Z".to_string(),
            data: Link {
                source: "test_source".to_string(),
                predicate: Some("test_predicate".to_string()),
                target: "test_target".to_string(),
            },
            proof: crate::types::DecoratedExpressionProof {
                key: "test_key".to_string(),
                signature: "test_signature".to_string(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: None,
        }];

        // Set up test data in the complete pool first
        complete_pool
            .update_all_engines_with_links("facts".to_string(), test_links, None, Some("test_owner".to_string()))
            .await
            .unwrap();

        // Verify that current_owner_did was stored in pool state
        let state = complete_pool.engine_state().read().await;
        assert_eq!(state.current_owner_did, Some("test_owner".to_string()));
        drop(state);

        // Populate with test data - should only include infrastructure + SDNA
        let result = sdna_pool.populate_from_complete_data().await;
        assert!(result.is_ok());

        // Test basic infrastructure query
        let result = sdna_pool.run_query("true.".to_string()).await.unwrap();
        assert_eq!(result, Ok(QueryResolution::True));
    }
}
