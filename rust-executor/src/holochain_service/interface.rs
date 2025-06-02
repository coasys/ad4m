use deno_core::error::AnyError;
use holochain::conductor::api::AppInfo;
use holochain::prelude::hash_type::Agent;
use holochain::prelude::{
    ExternIO, HoloHash, InstallAppPayload, Signal, Signature, ZomeCallResponse,
};
use lazy_static::lazy_static;
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::{oneshot, Mutex, RwLock};

#[derive(Clone)]
pub struct HolochainServiceInterface {
    pub sender: UnboundedSender<HolochainServiceRequest>,
    pub stream_receiver: Arc<Mutex<UnboundedReceiver<Signal>>>,
}

#[derive(Debug)]
pub enum HolochainServiceRequest {
    InstallApp(InstallAppPayload, oneshot::Sender<HolochainServiceResponse>),
    CallZomeFunction {
        app_id: String,
        cell_name: String,
        zome_name: String,
        fn_name: String,
        payload: Option<ExternIO>,
        response: oneshot::Sender<HolochainServiceResponse>,
    },
    RemoveApp(String, oneshot::Sender<HolochainServiceResponse>),
    AgentInfos(oneshot::Sender<HolochainServiceResponse>),
    AddAgentInfos(
        Vec<String>,
        oneshot::Sender<HolochainServiceResponse>,
    ),
    Sign(String, oneshot::Sender<HolochainServiceResponse>),
    Shutdown(oneshot::Sender<HolochainServiceResponse>),
    GetAgentKey(oneshot::Sender<HolochainServiceResponse>),
    GetAppInfo(String, oneshot::Sender<HolochainServiceResponse>),
    LogNetworkMetrics(oneshot::Sender<HolochainServiceResponse>),
    PackDna(String, oneshot::Sender<HolochainServiceResponse>),
    UnPackDna(String, oneshot::Sender<HolochainServiceResponse>),
    PackHapp(String, oneshot::Sender<HolochainServiceResponse>),
    UnPackHapp(String, oneshot::Sender<HolochainServiceResponse>),
}

#[derive(Debug)]
pub enum HolochainServiceResponse {
    InstallApp(Result<AppInfo, AnyError>),
    CallZomeFunction(Result<ZomeCallResponse, AnyError>),
    RemoveApp(Result<(), AnyError>),
    AgentInfos(Result<Vec<String>, AnyError>),
    AddAgentInfos(Result<(), AnyError>),
    Sign(Result<Signature, AnyError>),
    Shutdown(Result<(), AnyError>),
    GetAgentKey(Result<HoloHash<Agent>, AnyError>),
    GetAppInfo(Result<Option<AppInfo>, AnyError>),
    InitComplete(Result<(), AnyError>),
    LogNetworkMetrics(Result<(), AnyError>),
    PackDna(Result<String, AnyError>),
    UnPackDna(Result<String, AnyError>),
    PackHapp(Result<String, AnyError>),
    UnPackHapp(Result<String, AnyError>),
}

impl HolochainServiceInterface {
    pub async fn install_app(&self, payload: InstallAppPayload) -> Result<AppInfo, AnyError> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.sender.send(HolochainServiceRequest::InstallApp(
            payload,
            response_sender,
        ))?;
        
        match response_receiver.await.unwrap() {
            HolochainServiceResponse::InstallApp(result) => {
                if let Err(e) = &result {
                    log::error!("Error installing Holochain app: {:?}", e);
                }
                result
            }
            _ => unreachable!(),
        }
    }

    pub async fn call_zome_function(
        &self,
        app_id: String,
        cell_name: String,
        zome_name: String,
        fn_name: String,
        payload: Option<ExternIO>,
    ) -> Result<ZomeCallResponse, AnyError> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::CallZomeFunction {
                app_id,
                cell_name,
                zome_name,
                fn_name,
                payload,
                response: response_sender,
            })?;
        match response_receiver.await.unwrap() {
            HolochainServiceResponse::CallZomeFunction(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn remove_app(&self, app_id: String) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::RemoveApp(app_id, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::RemoveApp(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn agent_infos(&self) -> Result<Vec<String>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::AgentInfos(response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::AgentInfos(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn add_agent_infos(&self, agent_infos: Vec<String>) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender.send(HolochainServiceRequest::AddAgentInfos(
            agent_infos,
            response_tx,
        ))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::AddAgentInfos(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn sign(&self, data: String) -> Result<Signature, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Sign(data, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::Sign(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn shutdown(&self) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Shutdown(response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::Shutdown(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_agent_key(&self) -> Result<HoloHash<Agent>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAgentKey(response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::GetAgentKey(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_app_info(&self, app_id: String) -> Result<Option<AppInfo>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAppInfo(app_id, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::GetAppInfo(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn log_network_metrics(&self) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::LogNetworkMetrics(response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::LogNetworkMetrics(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn pack_dna(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::PackDna(path, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::PackDna(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn unpack_dna(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::UnPackDna(path, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::UnPackDna(result) => result,
            _ => unreachable!(),    
        }
    }

    pub async fn pack_happ(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::PackHapp(path, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::PackHapp(result) => result,
            _ => unreachable!(),
        }
    }
    
    pub async fn unpack_happ(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::UnPackHapp(path, response_tx))?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::UnPackHapp(result) => result,
            _ => unreachable!(),
        }
    }
}

lazy_static! {
    static ref HOLOCHAIN_SERVICE: Arc<RwLock<Option<HolochainServiceInterface>>> =
        Arc::new(RwLock::new(None));
}

pub async fn get_holochain_service() -> HolochainServiceInterface {
    let lock = HOLOCHAIN_SERVICE.read().await;
    lock.clone().expect("Holochain Conductor not started")
}

pub async fn maybe_get_holochain_service() -> Option<HolochainServiceInterface> {
    let lock = HOLOCHAIN_SERVICE.read().await;
    lock.clone()
}

pub async fn set_holochain_service(service: HolochainServiceInterface) {
    let mut lock = HOLOCHAIN_SERVICE.write().await;
    *lock = Some(service);
}
