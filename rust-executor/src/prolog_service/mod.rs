use deno_core::anyhow::Error;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use crate::types::DecoratedLinkExpression;

mod embedding_cache;
pub(crate) mod engine;
pub(crate) mod engine_pool;
pub mod assert_utils;
pub mod types;

use self::engine_pool::PrologEnginePool;
use self::types::QueryResult;

const DEFAULT_POOL_SIZE: usize = 10;

#[derive(Clone)]
pub struct PrologService {
    engine_pools: Arc<RwLock<HashMap<String, PrologEnginePool>>>,
}

impl PrologService {
    pub fn new() -> Self {
        PrologService {
            engine_pools: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn ensure_perspective_pool(&self, perspective_id: String) -> Result<(), Error> {
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
        let pool = PrologEnginePool::new(DEFAULT_POOL_SIZE);
        pool.initialize(DEFAULT_POOL_SIZE).await?;
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

    pub async fn run_query(
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

    /// Run query with subscription optimization - uses filtered pools for subscription queries
    pub async fn run_query_subscription(
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
        
        // The smart routing and population is now handled entirely within the engine pool
        // This eliminates circular dependencies and potential deadlocks
        let result = pool.run_query_smart(query.clone(), true).await;
        match &result {
            Ok(Ok(_query_result)) => {
                //log::info!("🔔 SUBSCRIPTION: Query succeeded with result: {:?}", query_result);
                log::info!("🔔 SUBSCRIPTION: Query succeeded [result omitted]");
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

    pub async fn run_query_all(&self, perspective_id: String, query: String) -> Result<(), Error> {
        let service_start = std::time::Instant::now();
        log::info!("⚡ PROLOG SERVICE: Starting run_query_all for perspective '{}' - query: {} chars", 
            perspective_id, query.len());
        
        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pool_lookup_start = std::time::Instant::now();
            let pools = self.engine_pools.read().await;
            let pool = pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone(); // Clone the Arc<> to release the lock
            log::info!("⚡ PROLOG SERVICE: Pool lookup took {:?}", pool_lookup_start.elapsed());
            pool
        }; // Read lock is released here
        
        let query_execution_start = std::time::Instant::now();
        let result = pool.run_query_all(query).await;
        
        match &result {
            Ok(()) => {
                log::info!("⚡ PROLOG SERVICE: run_query_all completed successfully in {:?} (total: {:?})", 
                    query_execution_start.elapsed(), service_start.elapsed());
            }
            Err(e) => {
                log::error!("⚡ PROLOG SERVICE: run_query_all failed after {:?} (total: {:?}): {}", 
                    query_execution_start.elapsed(), service_start.elapsed(), e);
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
    ) -> Result<(), Error> {
        let service_start = std::time::Instant::now();
        log::info!("🔗 PROLOG SERVICE: Starting update_perspective_links for perspective '{}' - {} links, module: {}", 
            perspective_id, all_links.len(), module_name);
        
        // ⚠️ DEADLOCK FIX: Minimize lock duration - get pool reference and release lock quickly
        let pool = {
            let pool_lookup_start = std::time::Instant::now();
            let pools = self.engine_pools.read().await;
            let pool = pools
                .get(&perspective_id)
                .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?
                .clone(); // Clone the Arc<> to release the lock
            log::info!("🔗 PROLOG SERVICE: Pool lookup took {:?}", pool_lookup_start.elapsed());
            pool
        }; // Read lock is released here
        
        let update_start = std::time::Instant::now();
        let result = pool.update_all_engines_with_links(module_name, all_links, neighbourhood_author).await;
        
        match &result {
            Ok(()) => {
                log::info!("🔗 PROLOG SERVICE: update_perspective_links completed successfully in {:?} (total: {:?})", 
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

    #[tokio::test]
    async fn test_init_prolog_service() {
        init_prolog_service().await;
        let service = get_prolog_service().await;

        let perspective_id = "test".to_string();

        // Ensure pool is created
        assert!(service
            .ensure_perspective_pool(perspective_id.clone())
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
            .update_perspective_links(perspective_id.clone(), "facts".to_string(), test_links, None)
            .await;
        assert!(load_facts.is_ok());

        let query = String::from("triple(\"a\",P,\"b\").");
        let result = service
            .run_query(perspective_id.clone(), query)
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
            .run_query(perspective_id.clone(), query)
            .await
            .expect("no error running query");

        assert_eq!(result, Ok(QueryResolution::True));

        let query = String::from("non_existant_predicate(\"a\",\"p1\",\"b\").");
        let result = service
            .run_query(perspective_id.clone(), query)
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
