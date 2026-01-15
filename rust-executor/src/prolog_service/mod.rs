use crate::types::DecoratedLinkExpression;
use deno_core::anyhow::Error;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod assert_utils;
mod embedding_cache;
pub(crate) mod engine;
pub(crate) mod engine_pool;
pub mod filtered_pool;
pub mod pool_trait;
pub mod sdna_pool;
pub mod source_filtering;
pub mod types;

use self::embedding_cache::EmbeddingCache;
use self::engine::PrologEngine;
use self::engine_pool::PrologEnginePool;
use self::types::{QueryResolution, QueryResult};

pub const DEFAULT_POOL_SIZE: usize = 5;
pub const DEFAULT_POOL_SIZE_WITH_FILTERING: usize = 2;
const SDNA_POOL_SIZE: usize = 1;
const FILTERED_POOL_SIZE: usize = 2;

/// Prolog execution mode configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrologMode {
    /// Simple mode: One engine per perspective, lazy updates on query
    /// Memory efficient but slower queries (like pre-2024 behavior)
    Simple,
    /// Pooled mode: Multiple engines with filtering and caching
    /// Faster queries but uses more memory
    #[allow(dead_code)]
    Pooled,
    /// SDNA-only mode: Lightweight engine with only SDNA facts (no link data)
    /// Perfect for testing if memory issues are from link data
    /// All SDNA introspection queries work, but data queries will return empty results
    SdnaOnly,
    /// Disabled mode: No Prolog engine is created, all operations are no-ops
    /// Queries and subscriptions will be logged with warnings
    Disabled,
}

// MEMORY OPTIMIZATION: Set to Simple for minimal memory usage (1 engine per perspective)
// Set to Pooled for maximum query performance (multiple engines with caching)
// Set to SdnaOnly for SDNA introspection without link data (minimal memory + SDNA queries work)
// Set to Disabled to turn off Prolog completely
pub static PROLOG_MODE: PrologMode = PrologMode::SdnaOnly;

#[derive(Clone)]
pub struct PrologService {
    engine_pools: Arc<RwLock<HashMap<String, PrologEnginePool>>>,
    // Simple mode: Single engine per perspective with dirty tracking
    simple_engines: Arc<RwLock<HashMap<String, SimpleEngine>>>,
}

/// Simple Prolog engine with lazy update tracking
struct SimpleEngine {
    /// Engine for regular queries
    query_engine: PrologEngine,
    /// Separate engine for subscriptions (to avoid interference)
    subscription_engine: PrologEngine,
    /// Track if links have changed since last query
    dirty: bool,
    /// Current links loaded in the engines
    current_links: Vec<DecoratedLinkExpression>,
    /// In SdnaOnly mode, track SDNA links separately for efficient updates
    /// Only update engine when SDNA actually changes, not on every data link change
    current_sdna_links: Option<Vec<DecoratedLinkExpression>>,
}

impl PrologService {
    pub fn new() -> Self {
        PrologService {
            engine_pools: Arc::new(RwLock::new(HashMap::new())),
            simple_engines: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Mark a perspective's Prolog engine as dirty (needs update before next query)
    /// Only used in Simple and SdnaOnly modes
    pub async fn mark_dirty(&self, perspective_id: &str) {
        match PROLOG_MODE {
            PrologMode::Disabled => {
                // Do nothing when disabled
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                let mut engines = self.simple_engines.write().await;
                if let Some(simple_engine) = engines.get_mut(perspective_id) {
                    simple_engine.dirty = true;
                    log::debug!("Marked Prolog engine {} as dirty", perspective_id);
                }
            }
            PrologMode::Pooled => {
                // Do nothing in pooled mode
            }
        }
    }

    /// Update Prolog engine if dirty (lazy update on query)
    /// Only used in Simple and SdnaOnly modes
    async fn ensure_engine_updated(
        &self,
        perspective_id: &str,
        links: &[DecoratedLinkExpression],
        neighbourhood_author: Option<String>,
        owner_did: Option<String>,
    ) -> Result<(), Error> {
        use crate::perspectives::sdna::{
            get_data_facts, get_sdna_facts, get_static_infrastructure_facts,
        };
        use pool_trait::PoolUtils;

        match PROLOG_MODE {
            PrologMode::Disabled => {
                // Do nothing when disabled
                return Ok(());
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Continue with normal processing
            }
            PrologMode::Pooled => {
                // Not applicable in pooled mode
                return Ok(());
            }
        }

        // LOCK SCOPE OPTIMIZATION: Acquire write lock ONLY to check state, then release
        let (needs_update, engine_exists) = {
            let engines = self.simple_engines.read().await;

            // Check if we need to update (dirty or links changed or first time)
            let needs_update = if PROLOG_MODE == PrologMode::SdnaOnly {
                // In SdnaOnly mode, only update if SDNA links actually changed
                if let Some(simple_engine) = engines.get(perspective_id) {
                    if simple_engine.dirty {
                        true
                    } else if let Some(ref current_sdna) = simple_engine.current_sdna_links {
                        // Extract current SDNA links and compare
                        let new_sdna_links = Self::extract_sdna_links(links);
                        current_sdna != &new_sdna_links
                    } else {
                        true // No SDNA tracking yet, need init
                    }
                } else {
                    true // First query = needs init
                }
            } else if let Some(simple_engine) = engines.get(perspective_id) {
                simple_engine.dirty || simple_engine.current_links != links
            } else {
                true // First query = needs init
            };

            let engine_exists = engines.contains_key(perspective_id);
            (needs_update, engine_exists)
        }; // Read lock released here

        if needs_update {
            let mode_desc = match PROLOG_MODE {
                PrologMode::SdnaOnly => "SDNA-only mode (no link data)",
                _ => "Simple mode: lazy update",
            };
            log::debug!(
                "Updating Prolog engine {} ({} with {} links)",
                perspective_id,
                mode_desc,
                links.len()
            );

            // EXPENSIVE OPERATIONS OUTSIDE THE LOCK:
            // Create and spawn engines if they don't exist (BEFORE acquiring write lock)
            let (query_engine, subscription_engine) = if !engine_exists {
                let mut qe = PrologEngine::new();
                qe.spawn().await?; // Expensive async operation - no lock held

                let mut se = PrologEngine::new();
                se.spawn().await?; // Expensive async operation - no lock held

                (qe, se)
            } else {
                // Engines exist, we'll update them - create placeholders for now
                (PrologEngine::new(), PrologEngine::new())
            };

            // Prepare facts based on mode (no lock needed - just data preparation)
            let mut facts_to_load = get_static_infrastructure_facts();

            // Only load link data if not in SDNA-only mode
            if PROLOG_MODE != PrologMode::SdnaOnly {
                facts_to_load.extend(get_data_facts(links));
            }

            // Always load SDNA facts
            facts_to_load.extend(get_sdna_facts(
                links,
                neighbourhood_author.clone(),
                owner_did.clone(),
            )?);

            // Preprocess facts (handle embeddings) - EXPENSIVE, no lock held
            let embedding_cache = Arc::new(RwLock::new(EmbeddingCache::new()));
            let processed_facts =
                PoolUtils::preprocess_program_lines(facts_to_load, &embedding_cache).await;

            // LOCK SCOPE: Acquire write lock ONLY to get mutable engine references
            let mut engines = self.simple_engines.write().await;

            // Insert new engines if needed
            if !engine_exists {
                engines.insert(
                    perspective_id.to_string(),
                    SimpleEngine {
                        query_engine,
                        subscription_engine,
                        dirty: true,
                        current_links: Vec::new(),
                        current_sdna_links: None,
                    },
                );
            }

            // Get mutable reference and move engines out temporarily
            let simple_engine = engines.get_mut(perspective_id).unwrap();

            // Move engines out of the struct temporarily
            let query_engine_to_update =
                std::mem::replace(&mut simple_engine.query_engine, PrologEngine::new());
            let subscription_engine_to_update =
                std::mem::replace(&mut simple_engine.subscription_engine, PrologEngine::new());

            // Release write lock before expensive load operations
            drop(engines);

            // EXPENSIVE OPERATIONS OUTSIDE THE LOCK:
            // Load facts into both engines
            query_engine_to_update
                .load_module_string("facts", &processed_facts)
                .await?;
            subscription_engine_to_update
                .load_module_string("facts", &processed_facts)
                .await?;

            // LOCK SCOPE: Reacquire write lock to update final state
            let mut engines = self.simple_engines.write().await;
            let simple_engine = engines.get_mut(perspective_id).unwrap();

            // Move engines back
            simple_engine.query_engine = query_engine_to_update;
            simple_engine.subscription_engine = subscription_engine_to_update;

            simple_engine.dirty = false;

            // MEMORY OPTIMIZATION: In SdnaOnly mode, don't store full links
            if PROLOG_MODE == PrologMode::SdnaOnly {
                simple_engine.current_links = Vec::new(); // Empty - not needed in SdnaOnly mode
                simple_engine.current_sdna_links = Some(Self::extract_sdna_links(links));
            } else {
                simple_engine.current_links = links.to_vec();
            }

            log::debug!(
                "Prolog engines {} updated successfully (query + subscription)",
                perspective_id
            );
        }

        Ok(())
    }

    /// Extract only SDNA links from a link list for change tracking
    fn extract_sdna_links(links: &[DecoratedLinkExpression]) -> Vec<DecoratedLinkExpression> {
        use crate::perspectives::sdna::is_sdna_link;

        links
            .iter()
            .filter(|link| is_sdna_link(&link.data))
            .cloned()
            .collect()
    }

    /// Run query in Simple or SdnaOnly mode
    pub async fn run_query_simple(
        &self,
        perspective_id: &str,
        query: String,
        links: &[DecoratedLinkExpression],
        neighbourhood_author: Option<String>,
        owner_did: Option<String>,
    ) -> Result<QueryResolution, Error> {
        use deno_core::anyhow::anyhow;

        // Check if Prolog is disabled
        if PROLOG_MODE == PrologMode::Disabled {
            log::warn!(
                "⚠️ Prolog query received but Prolog is DISABLED (perspective: {}, query: {})",
                perspective_id,
                query
            );
            return Err(anyhow!("Prolog is disabled"));
        }

        // Ensure engine is up to date
        self.ensure_engine_updated(perspective_id, links, neighbourhood_author, owner_did)
            .await?;

        // Add "." at the end if missing
        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        let engines = self.simple_engines.read().await;
        let simple_engine = engines
            .get(perspective_id)
            .ok_or_else(|| anyhow!("Prolog engine not found for perspective {}", perspective_id))?;

        // Run query through the query engine
        let result = simple_engine.query_engine.run_query(query).await?;

        // Convert QueryResult to QueryResolution
        result.map_err(|e| anyhow!("Prolog query failed: {}", e))
    }

    /// Run subscription query in Simple or SdnaOnly mode (uses separate engine)
    pub async fn run_query_subscription_simple(
        &self,
        perspective_id: &str,
        query: String,
        links: &[DecoratedLinkExpression],
        neighbourhood_author: Option<String>,
        owner_did: Option<String>,
    ) -> Result<QueryResolution, Error> {
        use deno_core::anyhow::anyhow;

        // Check if Prolog is disabled
        if PROLOG_MODE == PrologMode::Disabled {
            log::warn!(
                "⚠️ Prolog subscription query received but Prolog is DISABLED (perspective: {}, query: {})",
                perspective_id,
                query
            );
            return Err(anyhow!("Prolog is disabled"));
        }

        // Ensure engine is up to date
        self.ensure_engine_updated(perspective_id, links, neighbourhood_author, owner_did)
            .await?;

        // Add "." at the end if missing
        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        let engines = self.simple_engines.read().await;
        let simple_engine = engines
            .get(perspective_id)
            .ok_or_else(|| anyhow!("Prolog engine not found for perspective {}", perspective_id))?;

        // Run query through the subscription engine (separate from regular queries)
        let result = simple_engine.subscription_engine.run_query(query).await?;

        // Convert QueryResult to QueryResolution
        result.map_err(|e| anyhow!("Prolog subscription query failed: {}", e))
    }

    pub async fn ensure_perspective_pool(
        &self,
        perspective_id: String,
        pool_size: Option<usize>,
    ) -> Result<(), Error> {
        // Check if Prolog mode supports pooled mode
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ ensure_perspective_pool called but Prolog is DISABLED (perspective: {})",
                    perspective_id
                );
                return Ok(()); // Do nothing when disabled
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Simple/SdnaOnly modes don't use pools
                log::trace!(
                    "⚠️ ensure_perspective_pool called in Simple/SdnaOnly mode (perspective: {}) - ignoring",
                    perspective_id
                );
                return Ok(());
            }
            PrologMode::Pooled => {
                // Continue with pooled mode logic
            }
        }

        // ⚠️ DEADLOCK FIX: Use optimistic locking to avoid race conditions
        // First check with read lock (fast path)
        {
            let pools = self.engine_pools.read().await;
            if pools.contains_key(&perspective_id) {
                return Ok(()); // Pool already exists, nothing to do
            }
        }

        // Pool doesn't exist, acquire write lock to create it
        let mut pools = self.engine_pools.write().await;

        // Double-check pattern: another task might have created it while we waited for write lock
        if pools.contains_key(&perspective_id) {
            return Ok(()); // Someone else created it while we waited
        }

        // Create and initialize the pool
        let pool = PrologEnginePool::new();
        pool.initialize(pool_size.unwrap_or(DEFAULT_POOL_SIZE))
            .await?;
        pools.insert(perspective_id, pool);

        Ok(())
    }

    pub async fn _remove_perspective_pool(&self, perspective_id: String) -> Result<(), Error> {
        let mut pools = self.engine_pools.write().await;
        if let Some(pool) = pools.remove(&perspective_id) {
            pool._drop_all().await?;
        }
        Ok(())
    }

    // Currently always using smart query
    pub async fn _run_query(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<QueryResult, Error> {
        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pools = self.engine_pools.read().await;
            pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone() // Clone the Arc<> to release the lock
        }; // Read lock is released here

        pool.run_query(query).await
    }

    /// Run query directly on the SDNA pool for maximum performance
    ///
    /// This bypasses all smart routing logic and goes directly to the SDNA pool.
    /// Use this for subject class queries during create_subject flow for best performance.
    pub async fn run_query_sdna(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<QueryResult, Error> {
        // This function should only be called in Pooled mode
        // Simple/SdnaOnly modes are handled at the perspective layer
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog SDNA query received but Prolog is DISABLED (perspective: {}, query: {})",
                    perspective_id,
                    query
                );
                return Err(Error::msg("Prolog is disabled"));
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Should not be called directly in Simple/SdnaOnly - perspective layer handles routing
                log::warn!(
                    "⚠️ run_query_sdna called directly in Simple/SdnaOnly mode - this should be handled at perspective layer",
                );
                return Err(Error::msg(
                    "Direct SDNA pool queries not available in Simple/SdnaOnly mode",
                ));
            }
            PrologMode::Pooled => {
                // In pooled mode, use the dedicated SDNA pool
                let pool = {
                    let pools = self.engine_pools.read().await;
                    pools
                        .get(&perspective_id)
                        .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                        .clone()
                };
                pool.run_query_sdna(query).await
            }
        }
    }

    /// Run query with subscription optimization - uses filtered pools for subscription queries
    pub async fn run_query_smart(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<QueryResult, Error> {
        // Check if Prolog mode supports pooled queries
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog query received but Prolog is DISABLED (perspective: {}, query: {})",
                    perspective_id,
                    query
                );
                return Err(Error::msg("Prolog is disabled"));
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Smart routing is pooled-mode only
                log::warn!(
                    "⚠️ run_query_smart called in Simple/SdnaOnly mode (perspective: {}) - pooled-mode only, ignoring",
                    perspective_id
                );
                return Err(Error::msg(
                    "Smart routing not available in Simple/SdnaOnly mode",
                ));
            }
            PrologMode::Pooled => {
                // Continue with pooled mode logic
            }
        }

        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pools = self.engine_pools.read().await;
            pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone() // Clone the Arc<> to release the lock
        }; // Read lock is released here

        // The smart routing and population is now handled entirely within the engine pool
        // This eliminates circular dependencies and potential deadlocks
        let result = pool.run_query_smart(query.clone(), false).await;
        match &result {
            Ok(Ok(query_result)) => {
                log::debug!("⚡ SMART Query succeeded with result: {:?}", query_result);
            }
            Ok(Err(error)) => {
                log::warn!("⚡ SMART Query failed with error: {}", error);
            }
            Err(error) => {
                log::error!("⚡ SMART Query execution error: {}", error);
            }
        }
        result
    }

    /// Run query with subscription optimization - uses filtered pools for subscription queries
    pub async fn run_query_subscription(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<QueryResult, Error> {
        // Check if Prolog mode supports pooled subscription queries
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog subscription query received but Prolog is DISABLED (perspective: {}, query: {})",
                    perspective_id,
                    query
                );
                return Err(Error::msg("Prolog is disabled"));
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Subscription queries with pools are pooled-mode only
                log::warn!(
                    "⚠️ run_query_subscription called in Simple/SdnaOnly mode (perspective: {}) - pooled-mode only, ignoring",
                    perspective_id
                );
                return Err(Error::msg(
                    "Pooled subscriptions not available in Simple/SdnaOnly mode",
                ));
            }
            PrologMode::Pooled => {
                // Continue with pooled mode logic
            }
        }

        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pools = self.engine_pools.read().await;
            pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone() // Clone the Arc<> to release the lock
        }; // Read lock is released here

        // Increment reference count for filtered pools if this query would use one
        if let Some(source_filter) =
            crate::prolog_service::source_filtering::extract_source_filter(&query)
        {
            pool.increment_pool_reference(&source_filter).await;
        }

        // The smart routing and population is now handled entirely within the engine pool
        // This eliminates circular dependencies and potential deadlocks
        let result = pool.run_query_smart(query.clone(), true).await;
        match &result {
            Ok(Ok(_query_result)) => {
                //log::info!("🔔 SUBSCRIPTION: Query succeeded with result: {:?}", query_result);
                log::debug!("🔔 SUBSCRIPTION: Query succeeded [result omitted]");
            }
            Ok(Err(error)) => {
                log::warn!("🔔 SUBSCRIPTION: Query failed with error: {}", error);
            }
            Err(error) => {
                log::error!("🔔 SUBSCRIPTION: Query execution error: {}", error);
            }
        }
        result
    }

    /// Called when a subscription ends - decrements reference count for any associated filtered pool
    pub async fn subscription_ended(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<(), Error> {
        let pool = {
            let pools = self.engine_pools.read().await;
            pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone()
        };

        // Decrement reference count for filtered pools if this query would use one
        if let Some(source_filter) =
            crate::prolog_service::source_filtering::extract_source_filter(&query)
        {
            pool.decrement_pool_reference(&source_filter).await;
            log::debug!(
                "📊 SUBSCRIPTION END: Decremented pool reference for source: '{}'",
                source_filter
            );
        }

        Ok(())
    }

    pub async fn run_query_all(&self, perspective_id: String, query: String) -> Result<(), Error> {
        let service_start = std::time::Instant::now();
        log::debug!(
            "⚡ PROLOG SERVICE: Starting run_query_all for perspective '{}' - query: {} chars",
            perspective_id,
            query.len()
        );

        // Check if Prolog mode supports pooled queries
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::trace!(
                    "⚠️ run_query_all called but Prolog is DISABLED (perspective: {}, query: {} chars)",
                    perspective_id,
                    query.len()
                );
                return Ok(()); // Do nothing when disabled
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // This is a pooled-mode function - in Simple/SdnaOnly modes, shouldn't be called
                log::trace!(
                    "⚠️ run_query_all called in Simple/SdnaOnly mode (perspective: {}) - this is pooled-mode only, ignoring",
                    perspective_id
                );
                return Ok(());
            }
            PrologMode::Pooled => {
                // Continue with pooled mode logic
            }
        }

        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pool_lookup_start = std::time::Instant::now();
            let pools = self.engine_pools.read().await;
            let pool = pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone(); // Clone the Arc<> to release the lock
            log::trace!(
                "⚡ PROLOG SERVICE: Pool lookup took {:?}",
                pool_lookup_start.elapsed()
            );
            pool
        }; // Read lock is released here

        let query_execution_start = std::time::Instant::now();
        let result = pool.run_query_all(query).await;

        match &result {
            Ok(()) => {
                log::debug!(
                    "⚡ PROLOG SERVICE: run_query_all completed successfully in {:?} (total: {:?})",
                    query_execution_start.elapsed(),
                    service_start.elapsed()
                );
            }
            Err(e) => {
                log::error!(
                    "⚡ PROLOG SERVICE: run_query_all failed after {:?} (total: {:?}): {}",
                    query_execution_start.elapsed(),
                    service_start.elapsed(),
                    e
                );
            }
        }

        result
    }

    // Note: update_perspective_facts() removed - use update_perspective_links() for production code

    /// Update perspective with link data for optimized filtering
    pub async fn update_perspective_links(
        &self,
        perspective_id: String,
        module_name: String,
        all_links: Vec<DecoratedLinkExpression>,
        neighbourhood_author: Option<String>,
        owner_did: Option<String>,
    ) -> Result<(), Error> {
        let service_start = std::time::Instant::now();
        log::debug!("🔗 PROLOG SERVICE: Starting update_perspective_links for perspective '{}' - {} links, module: {}", 
            perspective_id, all_links.len(), module_name);

        // Check if Prolog mode supports pooled updates
        match PROLOG_MODE {
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ update_perspective_links called but Prolog is DISABLED (perspective: {}, {} links)",
                    perspective_id,
                    all_links.len()
                );
                return Ok(()); // Do nothing when disabled
            }
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Simple/SdnaOnly modes use lazy updates in ensure_engine_updated, not proactive pool updates
                log::trace!(
                    "⚠️ update_perspective_links called in Simple/SdnaOnly mode (perspective: {}, {} links) - ignoring, using lazy updates instead",
                    perspective_id,
                    all_links.len()
                );
                return Ok(());
            }
            PrologMode::Pooled => {
                // Continue with pooled mode logic
            }
        }

        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pool_lookup_start = std::time::Instant::now();
            let pools = self.engine_pools.read().await;
            let pool = pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone(); // Clone the Arc<> to release the lock
            log::trace!(
                "🔗 PROLOG SERVICE: Pool lookup took {:?}",
                pool_lookup_start.elapsed()
            );
            pool
        }; // Read lock is released here

        let update_start = std::time::Instant::now();
        let result = pool
            .update_all_engines_with_links(module_name, all_links, neighbourhood_author, owner_did)
            .await;

        match &result {
            Ok(()) => {
                log::debug!("🔗 PROLOG SERVICE: update_perspective_links completed successfully in {:?} (total: {:?})", 
                    update_start.elapsed(), service_start.elapsed());
            }
            Err(e) => {
                log::error!("🔗 PROLOG SERVICE: update_perspective_links failed after {:?} (total: {:?}): {}", 
                    update_start.elapsed(), service_start.elapsed(), e);
            }
        }

        result
    }

    pub async fn has_perspective_pool(&self, perspective_id: String) -> bool {
        let pools = self.engine_pools.read().await;
        pools.contains_key(&perspective_id)
    }
}

lazy_static! {
    static ref PROLOG_SERVICE: Arc<RwLock<Option<PrologService>>> = Arc::new(RwLock::new(None));
}

pub async fn init_prolog_service() {
    let mut lock = PROLOG_SERVICE.write().await;
    *lock = Some(PrologService::new());
}

pub async fn get_prolog_service() -> PrologService {
    let lock = PROLOG_SERVICE.read().await;
    lock.clone().expect("PrologServiceInterface not set")
}

#[cfg(test)]
mod prolog_test {
    use super::*;
    use crate::prolog_service::types::{QueryMatch, QueryResolution};
    use maplit::btreemap;
    use scryer_prolog::Term;

    #[ignore = "Doesn't work with SdnaOnly mode"]
    #[tokio::test]
    async fn test_init_prolog_service() {
        init_prolog_service().await;
        let service = get_prolog_service().await;

        let perspective_id = "test".to_string();

        // Ensure pool is created
        assert!(service
            .ensure_perspective_pool(perspective_id.clone(), None)
            .await
            .is_ok());

        // Use production method with structured link data
        use crate::types::{DecoratedLinkExpression, Link};
        let test_links = vec![
            DecoratedLinkExpression {
                author: "test_author".to_string(),
                timestamp: "2023-01-01T00:00:00Z".to_string(),
                data: Link {
                    source: "a".to_string(),
                    predicate: Some("p1".to_string()),
                    target: "b".to_string(),
                },
                proof: crate::types::DecoratedExpressionProof {
                    key: "test_key".to_string(),
                    signature: "test_signature".to_string(),
                    valid: Some(true),
                    invalid: Some(false),
                },
                status: None,
            },
            DecoratedLinkExpression {
                author: "test_author".to_string(),
                timestamp: "2023-01-01T00:00:00Z".to_string(),
                data: Link {
                    source: "a".to_string(),
                    predicate: Some("p2".to_string()),
                    target: "b".to_string(),
                },
                proof: crate::types::DecoratedExpressionProof {
                    key: "test_key".to_string(),
                    signature: "test_signature".to_string(),
                    valid: Some(true),
                    invalid: Some(false),
                },
                status: None,
            },
        ];

        // Load facts into the pool using production method
        let load_facts = service
            .update_perspective_links(
                perspective_id.clone(),
                "facts".to_string(),
                test_links,
                None,
                None,
            )
            .await;
        assert!(load_facts.is_ok());

        let query = String::from("triple(\"a\",P,\"b\").");
        let result = service
            .run_query_smart(perspective_id.clone(), query)
            .await
            .expect("no error running query");

        assert_eq!(
            result,
            Ok(QueryResolution::Matches(vec![
                QueryMatch::from(btreemap! {
                    "P" => Term::string("p1"),
                }),
                QueryMatch::from(btreemap! {
                    "P" => Term::string("p2"),
                }),
            ]))
        );

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        let result = service
            .run_query_smart(perspective_id.clone(), query)
            .await
            .expect("no error running query");

        assert_eq!(result, Ok(QueryResolution::True));

        let query = String::from("non_existant_predicate(\"a\",\"p1\",\"b\").");
        let result = service
            .run_query_smart(perspective_id.clone(), query)
            .await
            .expect("Error running query");

        assert_eq!(
            result,
            Err(String::from("{ 'error': [{ 'existence_error': ['procedure', { '/': ['non_existant_predicate', 3] }] }, { '/': ['non_existant_predicate', 3] }] }"))
        );

        // Test pool removal
        assert!(service
            ._remove_perspective_pool(perspective_id.clone())
            .await
            .is_ok());
        assert!(!service.has_perspective_pool(perspective_id.clone()).await);
    }
}
