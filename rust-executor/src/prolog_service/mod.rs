use deno_core::anyhow::Error;
use lazy_static::lazy_static;
use scryer_prolog::QueryResult;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub(crate) mod engine;
pub(crate) mod engine_pool;
pub(crate) mod prolog_service_extension;

use self::engine_pool::PrologEnginePool;

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
        let mut pools = self.engine_pools.write().await;
        if !pools.contains_key(&perspective_id) {
            let pool = PrologEnginePool::new(DEFAULT_POOL_SIZE);
            pool.initialize(DEFAULT_POOL_SIZE).await?;
            pools.insert(perspective_id, pool);
        }
        Ok(())
    }

    pub async fn remove_perspective_pool(&self, perspective_id: String) -> Result<(), Error> {
        let mut pools = self.engine_pools.write().await;
        if let Some(pool) = pools.remove(&perspective_id) {
            pool.drop_all().await?;
        }
        Ok(())
    }

    pub async fn run_query(
        &self,
        perspective_id: String,
        query: String,
    ) -> Result<QueryResult, Error> {
        let pools = self.engine_pools.read().await;
        let pool = pools
            .get(&perspective_id)
            .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?;
        pool.run_query(query).await
    }

    pub async fn run_query_all(&self, perspective_id: String, query: String) -> Result<(), Error> {
        let pools = self.engine_pools.read().await;
        let pool = pools
            .get(&perspective_id)
            .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?;
        pool.run_query_all(query).await
    }

    pub async fn update_perspective_facts(
        &self,
        perspective_id: String,
        module_name: String,
        program_lines: Vec<String>,
    ) -> Result<(), Error> {
        let pools = self.engine_pools.read().await;
        let pool = pools
            .get(&perspective_id)
            .ok_or_else(|| Error::msg("No Prolog engine pool found for perspective"))?;
        pool.update_all_engines(module_name, program_lines).await
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
    use maplit::btreemap;
    use scryer_prolog::{QueryMatch, QueryResolution, Value};

    use super::*;

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

        let facts = String::from(
            r#"
        triple("a", "p1", "b").
        triple("a", "p2", "b").
        "#,
        );

        // Load facts into the pool
        let load_facts = service
            .update_perspective_facts(perspective_id.clone(), "facts".to_string(), vec![facts])
            .await;
        assert!(load_facts.is_ok());

        let query = String::from("triple(\"a\",P,\"b\").");
        let result = service
            .run_query(perspective_id.clone(), query)
            .await
            .expect("Error running query");

        assert_eq!(
            result,
            Ok(QueryResolution::Matches(vec![
                QueryMatch::from(btreemap! {
                    "P" => Value::from("p1"),
                }),
                QueryMatch::from(btreemap! {
                    "P" => Value::from("p2"),
                }),
            ]))
        );

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        let result = service
            .run_query(perspective_id.clone(), query)
            .await
            .expect("Error running query");

        assert_eq!(result, Ok(QueryResolution::True));

        let query = String::from("non_existant_predicate(\"a\",\"p1\",\"b\").");
        let result = service
            .run_query(perspective_id.clone(), query)
            .await
            .expect("Error running query");

        assert_eq!(
            result,
            Err(String::from("error existence_error procedure / non_existant_predicate 3 / non_existant_predicate 3"))
        );

        // Test pool removal
        assert!(service
            .remove_perspective_pool(perspective_id.clone())
            .await
            .is_ok());
        assert!(!service.has_perspective_pool(perspective_id.clone()).await);
    }
}
