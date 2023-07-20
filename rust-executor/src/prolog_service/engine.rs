use deno_core::anyhow::Error;
use scryer_prolog::machine::{Machine, parsed_results::QueryResult};
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot};

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

pub struct PrologEngine {
    request_sender: mpsc::Sender<PrologServiceRequest>,
    request_receiver: Option<mpsc::Receiver<PrologServiceRequest>>,
}

impl PrologEngine {
    pub fn new() -> PrologEngine {
        let (request_sender, request_receiver) = mpsc::channel::<PrologServiceRequest>(32);
        
        PrologEngine {
            request_sender, 
            request_receiver: Some(request_receiver)
        }
    }

    pub async fn spawn(&mut self) -> Result<(), Error> {
        let mut receiver = self.request_receiver.take().ok_or_else(|| Error::msg("PrologEngine::spawn called twice"))?;
        let (response_sender, response_receiver) = oneshot::channel();

        std::thread::spawn(move || {
            let mut machine = Machine::new_lib();

            response_sender
                .send(PrologServiceResponse::InitComplete(Ok(())))
                .unwrap();

            loop {
                match receiver.try_recv() {
                    Ok(message) => match message {
                        PrologServiceRequest::RunQuery(query, response) => {
                            let result = machine.run_query(query);
                            let _ = response.send(PrologServiceResponse::QueryResult(result));
                        }
                        PrologServiceRequest::LoadModuleString(module_name, program, response) => {
                            let _result = machine.load_module_string(module_name.as_str(), program);
                            let _ = response.send(PrologServiceResponse::LoadModuleResult(Ok(())));
                        }
                    },
                    Err(_err) => std::thread::sleep(std::time::Duration::from_millis(5)),
                }
            }
        });

        match response_receiver.await? {
            PrologServiceResponse::InitComplete(result) => result?,
            _ => unreachable!(),
        };

        Ok(())
    }

    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.request_sender
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
        self.request_sender
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

#[cfg(test)]
mod prolog_test {
    use super::*;

    #[tokio::test]
    async fn test_init_prolog_engine() {
        let mut engine = PrologEngine::new();
        assert!(engine.spawn().await.is_ok());

        let facts = String::from(
            r#"
        triple("a", "p1", "b").
        triple("a", "p2", "b").
        "#,
        );

        let load_facts = engine
            .load_module_string("facts".to_string(), facts)
            .await;
        assert!(load_facts.is_ok());
        println!("Facts loaded");

        let query = String::from("triple(\"a\",P,\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = engine.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = engine.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());
    }
}
