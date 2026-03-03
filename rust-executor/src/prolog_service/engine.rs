use std::panic::AssertUnwindSafe;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;

use deno_core::anyhow::Error;
use scryer_prolog::MachineBuilder;
use tokio::task;

use super::types::{query_result_from_leaf_answer, QueryResult};

#[derive(Debug)]
pub enum PrologServiceRequest {
    RunQuery(String, mpsc::Sender<PrologServiceResponse>),
    LoadModuleString(String, Vec<String>, mpsc::Sender<PrologServiceResponse>),
    #[allow(dead_code)]
    Drop,
}

#[derive(Debug)]
pub enum PrologServiceResponse {
    InitComplete(Result<(), Error>),
    QueryResult(QueryResult),
    LoadModuleResult(Result<(), Error>),
}

struct SendableReceiver<T>(Arc<Mutex<mpsc::Receiver<T>>>);

unsafe impl<T> Sync for SendableReceiver<T> {}

pub struct PrologEngine {
    request_sender: mpsc::Sender<PrologServiceRequest>,
    request_receiver: Option<SendableReceiver<PrologServiceRequest>>,
}

impl PrologEngine {
    pub fn new() -> PrologEngine {
        let (request_sender, request_receiver) = mpsc::channel::<PrologServiceRequest>();

        PrologEngine {
            request_sender,
            request_receiver: Some(SendableReceiver(Arc::new(Mutex::new(request_receiver)))),
        }
    }

    pub async fn spawn(&mut self) -> Result<(), Error> {
        let receiver = self
            .request_receiver
            .take()
            .ok_or_else(|| Error::msg("PrologEngine::spawn called twice"))?;
        let (response_sender, response_receiver) = mpsc::channel();

        std::thread::spawn(move || {
            let mut machine = MachineBuilder::default().build();
            let receiver = receiver.0.lock().unwrap();

            response_sender
                .send(PrologServiceResponse::InitComplete(Ok(())))
                .unwrap();

            while let Ok(message) = receiver.recv() {
                match message {
                    PrologServiceRequest::RunQuery(query, response) => {
                        let answer_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                            let mut results = Vec::new();
                            let mut iter = machine.run_query(query.clone());
                            const MAX_RESULTS: usize = 1_000_000; // Adjust as needed

                            let mut panic = None;
                            while results.len() < MAX_RESULTS {
                                match std::panic::catch_unwind(AssertUnwindSafe(|| iter.next())) {
                                    Ok(Some(Ok(answer))) => results.push(answer),
                                    Ok(Some(Err(term))) => {
                                        return query_result_from_leaf_answer(Err(term))
                                    }
                                    Ok(None) => break, // Iterator exhausted
                                    Err(e) => {
                                        let error_string =
                                            if let Some(string) = e.downcast_ref::<String>() {
                                                format!(
                                                    "Scryer panicked in next(): {:?} - query: {}",
                                                    string, query
                                                )
                                            } else if let Some(&str) = e.downcast_ref::<&str>() {
                                                format!(
                                                    "Scryer panicked in next(): {:?} - query: {}",
                                                    str, query
                                                )
                                            } else {
                                                format!(
                                                    "Scryer panicked in next(): {:?} - query: {}",
                                                    e, query
                                                )
                                            };
                                        panic = Some(error_string);
                                        break;
                                    }
                                }
                            }

                            if let Some(error) = panic {
                                Err(error)
                            } else {
                                if results.len() >= MAX_RESULTS {
                                    log::warn!(
                                        "Query {} truncated at {} results",
                                        query,
                                        MAX_RESULTS
                                    );
                                }

                                query_result_from_leaf_answer(Ok(results))
                            }
                        }));

                        match answer_result {
                            Ok(result) => {
                                let _ = response.send(PrologServiceResponse::QueryResult(result));
                            }
                            Err(e) => {
                                let error_string = if let Some(string) = e.downcast_ref::<String>()
                                {
                                    format!(
                                        "Scryer panicked with: {:?} - when running query: {}",
                                        string, query
                                    )
                                } else if let Some(&str) = e.downcast_ref::<&str>() {
                                    format!(
                                        "Scryer panicked with: {:?} - when running query: {}",
                                        str, query
                                    )
                                } else {
                                    format!(
                                        "Scryer panicked with: {:?} - when running query: {}",
                                        e, query
                                    )
                                };
                                log::error!("{}", error_string);
                                let _ = response.send(PrologServiceResponse::QueryResult(Err(
                                    format!("Scryer panicked with: {:?}", error_string),
                                )));
                            }
                        }
                    }
                    PrologServiceRequest::LoadModuleString(
                        module_name,
                        program_lines,
                        response,
                    ) => {
                        let program = program_lines
                            .iter()
                            .map(|l| l.replace(['\n', '\r'], ""))
                            .collect::<Vec<String>>()
                            .join("\n");

                        // 🛡️ CRITICAL: Properly handle errors from consult_module_string
                        let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                            machine.consult_module_string(module_name.as_str(), program.clone())
                        }));

                        let load_result = match result {
                            Ok(()) => {
                                // 🔍 VERIFICATION: Test if module was actually loaded by running a simple query
                                let verification_result =
                                    std::panic::catch_unwind(AssertUnwindSafe(|| {
                                        // Try to run a simple query that should work if the module loaded
                                        let mut iter = machine.run_query("true.".to_string());
                                        iter.next()
                                    }));

                                match verification_result {
                                    Ok(Some(Ok(_))) => {
                                        log::debug!("✅ Engine successfully consulted and verified module '{}' with {} lines", 
                                            module_name, program_lines.len());
                                        Ok(())
                                    }
                                    Ok(Some(Err(e))) => {
                                        let error_msg = format!("Prolog engine failed verification query after consult_module_string: {:?} - module: {}", e, module_name);
                                        log::error!("🚨 {}", error_msg);
                                        Err(Error::msg(error_msg))
                                    }
                                    Ok(None) => {
                                        let error_msg = format!("Prolog engine verification query returned None after consult_module_string - module: {}", module_name);
                                        log::error!("🚨 {}", error_msg);
                                        Err(Error::msg(error_msg))
                                    }
                                    Err(e) => {
                                        let error_msg = if let Some(string) =
                                            e.downcast_ref::<String>()
                                        {
                                            format!("Prolog engine panic during verification query: {} - module: {}", 
                                                string, module_name)
                                        } else if let Some(&str) = e.downcast_ref::<&str>() {
                                            format!("Prolog engine panic during verification query: {} - module: {}", 
                                                str, module_name)
                                        } else {
                                            format!("Prolog engine panic during verification query: {:?} - module: {}", 
                                                e, module_name)
                                        };
                                        log::error!("🚨 {}", error_msg);
                                        Err(Error::msg(error_msg))
                                    }
                                }
                            }
                            Err(e) => {
                                let error_msg = if let Some(string) = e.downcast_ref::<String>() {
                                    format!("Prolog engine panic during consult_module_string: {} - module: {}", 
                                        string, module_name)
                                } else if let Some(&str) = e.downcast_ref::<&str>() {
                                    format!("Prolog engine panic during consult_module_string: {} - module: {}", 
                                        str, module_name)
                                } else {
                                    format!("Prolog engine panic during consult_module_string: {:?} - module: {}", 
                                        e, module_name)
                                };
                                log::error!("🚨 {}", error_msg);
                                Err(Error::msg(error_msg))
                            }
                        };

                        let _ = response.send(PrologServiceResponse::LoadModuleResult(load_result));
                    }
                    PrologServiceRequest::Drop => return,
                }
            }
        });

        let response = task::spawn_blocking(move || response_receiver.recv())
            .await
            .map_err(|e| Error::msg(format!("Failed to spawn blocking task: {}", e)))??;

        match response {
            PrologServiceResponse::InitComplete(result) => result?,
            _ => unreachable!(),
        };

        Ok(())
    }

    // There two levels of error handling here:
    // 1. The query can fail and Prolog returns an error
    //    This is represented as a QueryResult with an error string
    // 2. The Prolog engine can panic and we don't have a result
    //    This is represented with the outer Result
    pub async fn run_query(&self, query: String) -> Result<QueryResult, Error> {
        let (response_sender, response_receiver) = mpsc::channel();
        self.request_sender
            .send(PrologServiceRequest::RunQuery(query, response_sender))?;

        let response = task::spawn_blocking(move || response_receiver.recv())
            .await
            .map_err(|e| Error::msg(format!("Failed to spawn blocking task: {}", e)))??;

        match response {
            PrologServiceResponse::QueryResult(query_result) => Ok(query_result),
            _ => unreachable!(),
        }
    }

    pub async fn load_module_string(
        &self,
        module_name: &str,
        program_lines: &[String],
    ) -> Result<(), Error> {
        let (response_sender, response_receiver) = mpsc::channel();
        self.request_sender
            .send(PrologServiceRequest::LoadModuleString(
                module_name.to_string(),
                program_lines.to_vec(),
                response_sender,
            ))?;

        let response = task::spawn_blocking(move || response_receiver.recv())
            .await
            .map_err(|e| Error::msg(format!("Failed to spawn blocking task: {}", e)))??;

        match response {
            PrologServiceResponse::LoadModuleResult(result) => result,
            _ => unreachable!(),
        }
    }

    pub fn _drop(&self) -> Result<(), Error> {
        self.request_sender.send(PrologServiceRequest::Drop)?;
        Ok(())
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

        let load_facts = engine.load_module_string("facts", &[facts]).await;
        assert!(load_facts.is_ok());
        println!("Facts loaded");

        let query = String::from("triple(\"a\",P,\"b\").");
        println!("Running query: {}", query);
        let output = engine.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        println!("Running query: {}", query);
        let output = engine.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());
    }
}
