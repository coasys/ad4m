use deno_core::anyhow::Error;
use scryer_prolog::machine::parsed_results::QueryResult;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use lazy_static::lazy_static;

pub(crate) mod prolog_service_extension;
pub(crate) mod engine;

use self::engine::PrologEngine;

#[derive(Clone)]
pub struct PrologService {
    engines: Arc<RwLock<HashMap<String, PrologEngine>>>,
}

impl PrologService {
    pub fn new() -> Self {
        PrologService { engines: Arc::new(RwLock::new(HashMap::new())) }
    }

    pub async fn spawn_engine(&mut self, engine_name: String) -> Result<(), Error> {
        if self.engines.read().await.contains_key(&engine_name) {
            return Err(Error::msg("Engine already exists"));
        }

        let mut engine = PrologEngine::new();
        engine.spawn().await?;

        self.engines.write().await.insert(engine_name, engine);
        Ok(())
    }

    pub async fn run_query(&self, engine_name: String, query: String) -> Result<QueryResult, Error> {
        let engines = self.engines.read().await;
        let engine = engines.get(&engine_name).ok_or_else(|| Error::msg("Engine not found"))?;
        let result = engine.run_query(query).await?;
        Ok(result)
    }

    pub async fn load_module_string(
        &self,
        engine_name: String,
        module_name: String,
        program: String,
    ) -> Result<(), Error> {
        let engines = self.engines.read().await;
        let engine = engines.get(&engine_name).ok_or_else(|| Error::msg("Engine not found"))?;
        engine.load_module_string(module_name, program).await
    }
}

lazy_static! {
    static ref PROLOG_SERVICE: Arc<RwLock<Option<PrologService>>> =
        Arc::new(RwLock::new(None));
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

    #[tokio::test]
    async fn test_init_prolog_engine() {
        init_prolog_service().await;
        let mut service = get_prolog_service().await;

        let engine_name = "test".to_string();

        assert!(service.spawn_engine(engine_name.clone()).await.is_ok());

        let facts = String::from(
            r#"
        triple("a", "p1", "b").
        triple("a", "p2", "b").
        "#,
        );


        let load_facts = service
            .load_module_string(
                engine_name.clone(),
                "facts".to_string(), 
                facts
            )
            .await;
        assert!(load_facts.is_ok());
        println!("Facts loaded");

        let query = String::from("triple(\"a\",P,\"b\").");
        let output = service.run_query(engine_name.clone(), query).await;
        assert!(output.is_ok());

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = service.run_query(engine_name.clone(), query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());
    }
}
