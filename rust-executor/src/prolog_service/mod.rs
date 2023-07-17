use deno_core::anyhow::Error;
use scryer_prolog::machine::Machine;
use std::sync::Arc;
use tokio::sync::oneshot;
use tokio::sync::{mpsc, Mutex};

pub(crate) mod interface;
pub(crate) mod prolog_service_extension;

pub(crate) use interface::{
    get_prolog_service, PrologServiceInterface, PrologServiceRequest, PrologServiceResponse,
};

use self::interface::set_prolog_service;

#[derive(Clone)]
pub struct PrologService {
    pub machine: Arc<Mutex<Machine>>,
}

impl PrologService {
    pub async fn init() -> Result<(), Error> {
        let (sender, mut receiver) = mpsc::channel::<PrologServiceRequest>(32);
        let inteface = PrologServiceInterface { sender };
        set_prolog_service(inteface).await;

        let (response_sender, response_receiver) = oneshot::channel();

        std::thread::spawn(move || {
            let service = PrologService::new().unwrap();

            response_sender
                .send(PrologServiceResponse::InitComplete(Ok(())))
                .unwrap();

            loop {
                match receiver.try_recv() {
                    Ok(message) => match message {
                        PrologServiceRequest::RunQuery(query, response) => {
                            let mut machine = loop {
                                match service.machine.try_lock() {
                                    Ok(machine) => break machine,
                                    Err(_err) => {
                                        std::thread::sleep(std::time::Duration::from_millis(5))
                                    }
                                }
                            };
                            let result = machine.run_query(query);
                            let _ = response.send(PrologServiceResponse::QueryResult(result));
                        }
                        PrologServiceRequest::LoadModuleString(module_name, program, response) => {
                            let mut machine = loop {
                                match service.machine.try_lock() {
                                    Ok(machine) => break machine,
                                    Err(_err) => {
                                        std::thread::sleep(std::time::Duration::from_millis(5))
                                    }
                                }
                            };
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

    pub fn new() -> Result<PrologService, Error> {
        let service = PrologService {
            machine: Arc::new(Mutex::new(Machine::with_test_streams())),
        };

        Ok(service)
    }
}

#[cfg(test)]
mod prolog_test {
    use super::PrologService;
    use crate::prolog_service::interface::get_prolog_service;

    #[tokio::test]
    async fn test_init_prolog_engine() {
        let service = PrologService::init().await;
        assert!(service.is_ok());

        let interface = get_prolog_service().await;

        let facts = String::from(
            r#"
        triple("a", "p1", "b").
        triple("a", "p2", "b").
        "#,
        );

        let load_facts = interface
            .load_module_string("facts".to_string(), facts)
            .await;
        assert!(load_facts.is_ok());
        println!("Facts loaded");

        let query = String::from("triple(\"a\",P,\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = interface.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = interface.run_query(query).await;
        println!("Output: {:?}", output);
        assert!(output.is_ok());
    }
}
