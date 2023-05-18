use deno_core::error::AnyError;
use holochain::conductor::api::AppInfo;
use holochain::prelude::agent_store::AgentInfoSigned;
use holochain::prelude::hash_type::Agent;
use holochain::prelude::{
    ExternIO, HoloHash, InstallAppPayload, Signal, Signature, ZomeCallResponse,
};
use lazy_static::lazy_static;
use std::sync::Arc;
use tokio::sync::mpsc::{Sender, UnboundedReceiver};
use tokio::sync::{oneshot, Mutex, RwLock};

#[derive(Clone)]
pub struct HolochainServiceInterface {
    pub sender: Sender<HolochainServiceRequest>,
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
        Vec<AgentInfoSigned>,
        oneshot::Sender<HolochainServiceResponse>,
    ),
    Sign(String, oneshot::Sender<HolochainServiceResponse>),
    Shutdown(oneshot::Sender<HolochainServiceResponse>),
    GetAgentKey(oneshot::Sender<HolochainServiceResponse>),
    GetAppInfo(String, oneshot::Sender<HolochainServiceResponse>),
    GetNetworkMetrics(oneshot::Sender<HolochainServiceResponse>),
}

#[derive(Debug)]
pub enum HolochainServiceResponse {
    InstallApp(Result<AppInfo, AnyError>),
    CallZomeFunction(Result<ZomeCallResponse, AnyError>),
    RemoveApp(Result<(), AnyError>),
    AgentInfos(Result<Vec<AgentInfoSigned>, AnyError>),
    AddAgentInfos(Result<(), AnyError>),
    Sign(Result<Signature, AnyError>),
    Shutdown(Result<(), AnyError>),
    GetAgentKey(Result<HoloHash<Agent>, AnyError>),
    GetAppInfo(Result<Option<AppInfo>, AnyError>),
    InitComplete(Result<(), AnyError>),
    GetNetworkMetrics(Result<String, AnyError>),
}

impl HolochainServiceInterface {
    pub async fn install_app(&self, payload: InstallAppPayload) -> Result<AppInfo, AnyError> {
        let (response_sender, response_receiver) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::InstallApp(
                payload,
                response_sender,
            ))
            .await?;
        match response_receiver.await.unwrap() {
            HolochainServiceResponse::InstallApp(result) => result,
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
            })
            .await?;
        match response_receiver.await.unwrap() {
            HolochainServiceResponse::CallZomeFunction(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn remove_app(&self, app_id: String) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::RemoveApp(app_id, response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::RemoveApp(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn agent_infos(&self) -> Result<Vec<AgentInfoSigned>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::AgentInfos(response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::AgentInfos(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn add_agent_infos(&self, agent_infos: Vec<AgentInfoSigned>) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::AddAgentInfos(
                agent_infos,
                response_tx,
            ))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::AddAgentInfos(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn sign(&self, data: String) -> Result<Signature, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Sign(data, response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::Sign(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn shutdown(&self) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Shutdown(response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::Shutdown(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_agent_key(&self) -> Result<HoloHash<Agent>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAgentKey(response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::GetAgentKey(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_app_info(&self, app_id: String) -> Result<Option<AppInfo>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAppInfo(app_id, response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::GetAppInfo(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_network_metrics(&self) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetNetworkMetrics(response_tx))
            .await?;
        match response_rx.await.unwrap() {
            HolochainServiceResponse::GetNetworkMetrics(result) => result,
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

pub fn maybe_get_holochain_service() -> Option<HolochainServiceInterface> {
    let lock = HOLOCHAIN_SERVICE.try_read();
    match lock {
        Ok(guard) => guard.clone(),
        Err(_) => None,
    }
}

pub async fn set_holochain_service(service: HolochainServiceInterface) {
    let mut lock = HOLOCHAIN_SERVICE.write().await;
    *lock = Some(service);
}
