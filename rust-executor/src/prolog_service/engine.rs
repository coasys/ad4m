use deno_core::anyhow::Error;
use scryer_prolog::machine::{parsed_results::QueryResult, Machine};
use tokio::sync::{mpsc, oneshot};

#[derive(Debug)]
pub enum PrologServiceRequest {
    RunQuery(String, oneshot::Sender<PrologServiceResponse>),
    LoadModuleString(String, Vec<String>, oneshot::Sender<PrologServiceResponse>),
}

#[derive(Debug)]
pub enum PrologServiceResponse {
    InitComplete(Result<(), Error>),
    QueryResult(QueryResult),
    LoadModuleResult(Result<(), Error>),
}

pub struct PrologEngine {
    request_sender: mpsc::UnboundedSender<PrologServiceRequest>,
    request_receiver: Option<mpsc::UnboundedReceiver<PrologServiceRequest>>,
}

impl PrologEngine {
    pub fn new() -> PrologEngine {
        let (request_sender, request_receiver) = mpsc::unbounded_channel::<PrologServiceRequest>();

        PrologEngine {
            request_sender,
            request_receiver: Some(request_receiver),
        }
    }

    pub async fn spawn(&mut self) -> Result<(), Error> {
        let mut receiver = self
            .request_receiver
            .take()
            .ok_or_else(|| Error::msg("PrologEngine::spawn called twice"))?;
        let (response_sender, response_receiver) = oneshot::channel();

        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .thread_name(String::from("prolog_service"))
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            tokio::task::block_in_place(||
                rt.block_on(async move {
                    let mut machine = Machine::new_lib();

                    response_sender
                        .send(PrologServiceResponse::InitComplete(Ok(())))
                        .unwrap();

                    while let Some(message) = receiver.recv().await {
                        match message {
                            PrologServiceRequest::RunQuery(query, response) => {
                                let result = machine.run_query(query);
                                let _ = response.send(PrologServiceResponse::QueryResult(result));
                            }
                            PrologServiceRequest::LoadModuleString(
                                module_name,
                                program_lines,
                                response,
                            ) => {
                                let program = program_lines
                                    .iter()
                                    .map(|l| l.replace("\n", "").replace("\r", ""))
                                    .collect::<Vec<String>>()
                                    .join("\n");
                                let _result =
                                    machine.consult_module_string(module_name.as_str(), program);
                                let _ = response.send(PrologServiceResponse::LoadModuleResult(Ok(())));
                            }
                        }
                    }
                })
            );
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
            .send(PrologServiceRequest::RunQuery(query, response_sender))?;
        let response = response_receiver
            .await?;
        match response {
            PrologServiceResponse::QueryResult(query_result) => Ok(query_result),
            _ => unreachable!(),
        }
    }

    pub async fn load_module_string(
        &self,
        module_name: String,
        program_lines: Vec<String>,
    ) -> Result<(), Error> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.request_sender
            .send(PrologServiceRequest::LoadModuleString(
                module_name,
                program_lines,
                response_sender,
            ))?;
        let response = response_receiver
            .await?;
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

        let load_facts = engine.load_module_string("facts".to_string(), vec![facts]).await;
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
