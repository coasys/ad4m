use super::engine::PrologEngine;
use deno_core::anyhow::{anyhow, Error};
use futures::future::join_all;
use scryer_prolog::{QueryResolution, QueryResult};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct PrologEnginePool {
    engines: Arc<RwLock<Vec<Option<PrologEngine>>>>,
    next_engine: Arc<AtomicUsize>,
}

impl PrologEnginePool {
    pub fn new(pool_size: usize) -> Self {
        PrologEnginePool {
            engines: Arc::new(RwLock::new(Vec::with_capacity(pool_size))),
            next_engine: Arc::new(AtomicUsize::new(0)),
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

    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let engines = self.engines.read().await;
        let valid_engines: Vec<_> = engines
            .iter()
            .enumerate()
            .filter_map(|(i, e)| e.as_ref().map(|engine| (i, engine)))
            .collect();
        if valid_engines.is_empty() {
            log::error!("No valid Prolog engines available");
            return Err(anyhow!("No valid Prolog engines available"));
        }

        // Use fetch_add to atomically increment and get the previous value
        let current = self.next_engine.fetch_add(1, Ordering::SeqCst);
        let idx = current % valid_engines.len();

        let (engine_idx, engine) = valid_engines[idx];
        let result = engine.run_query(query.clone()).await;

        if let Err(e) = &result {
            log::error!("Prolog engine error: {}", e);
            drop(engines);

            // Invalidate the failed engine
            let mut engines = self.engines.write().await;
            engines[engine_idx] = None;

            return Err(anyhow!("Engine failed and was invalidated: {}", e));
        }

        result
    }

    pub async fn run_query_all(&self, query: String) -> Result<(), Error> {
        let engines = self.engines.write().await;
        let valid_engines: Vec<_> = engines.iter().filter_map(|e| e.as_ref()).collect();

        if valid_engines.is_empty() {
            return Err(anyhow!("No valid Prolog engines available"));
        }

        let futures: Vec<_> = valid_engines
            .iter()
            .map(|engine| engine.run_query(query.clone()))
            .collect();

        let results = join_all(futures).await;

        for result in results {
            match result? {
                Ok(QueryResolution::True) => continue,
                Ok(other) => return Err(anyhow!("Unexpected query result: {:?}", other)),
                Err(e) => return Err(anyhow!("Query failed: {}", e)),
            }
        }

        Ok(())
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

        // Update all engines with new facts
        let mut update_futures = Vec::new();
        for engine in engines.iter().filter_map(|e| e.as_ref()) {
            let update_future =
                engine.load_module_string(module_name.clone(), program_lines.clone());
            update_futures.push(update_future);
        }

        let results = join_all(update_futures).await;
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                log::error!("Failed to update Prologengine {}: {}", i, e);
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use scryer_prolog::{QueryResolution, Value};

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
        let result = pool.run_query_all("false.".to_string()).await;
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
                assert_eq!(matches[0].bindings["X"], Value::Atom("a".to_string()));
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
