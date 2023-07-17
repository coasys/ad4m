use deno_core::anyhow::Error;
use lazy_static::lazy_static;
use scryer_prolog::machine::parsed_results::QueryResult;
use std::sync::Arc;
use tokio::sync::mpsc::Sender;
use tokio::sync::{oneshot, RwLock};

#[derive(Clone)]
pub struct PrologServiceInterface {
    pub sender: Sender<PrologServiceRequest>,
}

impl PrologServiceInterface {
    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.sender
            .send(PrologServiceRequest::RunQuery(query, response_sender))
            .await
            .expect("Failed to send PrologServiceRequest::RunQuery");
        let response = response_receiver
            .await
            .expect("Failed to receive PrologServiceResponse");
        match response {
            PrologServiceResponse::QueryResult(query_result) => Ok(query_result),
            _ => unreachable!(),
        }
    }

    pub async fn load_module_string(
        &self,
        module_name: String,
        program: String,
    ) -> Result<(), Error> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.sender
            .send(PrologServiceRequest::LoadModuleString(
                module_name,
                program,
                response_sender,
            ))
            .await
            .expect("Failed to send PrologServiceRequest::LoadModuleString");
        let response = response_receiver
            .await
            .expect("Failed to receive PrologServiceResponse");
        match response {
            PrologServiceResponse::LoadModuleResult(result) => result,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum PrologServiceRequest {
    RunQuery(String, oneshot::Sender<PrologServiceResponse>),
    LoadModuleString(String, String, oneshot::Sender<PrologServiceResponse>),
}

#[derive(Debug)]
pub enum PrologServiceResponse {
    InitComplete(Result<(), Error>),
    QueryResult(QueryResult),
    LoadModuleResult(Result<(), Error>),
}

lazy_static! {
    static ref PROLOG_SERVICE: Arc<RwLock<Option<PrologServiceInterface>>> =
        Arc::new(RwLock::new(None));
}

pub async fn set_prolog_service(service: PrologServiceInterface) {
    let mut lock = PROLOG_SERVICE.write().await;
    *lock = Some(service);
}

pub async fn get_prolog_service() -> PrologServiceInterface {
    let lock = PROLOG_SERVICE.read().await;
    lock.clone().expect("PrologServiceInterface not set")
}
