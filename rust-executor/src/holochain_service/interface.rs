use deno_core::error::AnyError;
use holochain::conductor::api::AppInfo;
use holochain::prelude::hash_type::Agent;
use holochain::prelude::{
    ExternIO, HoloHash, InstallAppPayload, Signal, Signature, ZomeCallResponse,
};
use lazy_static::lazy_static;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
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
    AddAgentInfos(Vec<String>, oneshot::Sender<HolochainServiceResponse>),
    Sign(String, oneshot::Sender<HolochainServiceResponse>),
    Shutdown(oneshot::Sender<HolochainServiceResponse>),
    GetAgentKey(oneshot::Sender<HolochainServiceResponse>),
    GetAppInfo(String, oneshot::Sender<HolochainServiceResponse>),
    LogNetworkMetrics(oneshot::Sender<HolochainServiceResponse>),
    GetNetworkMetrics(oneshot::Sender<HolochainServiceResponse>),
    PackDna(String, oneshot::Sender<HolochainServiceResponse>),
    UnPackDna(String, oneshot::Sender<HolochainServiceResponse>),
    PackHapp(String, oneshot::Sender<HolochainServiceResponse>),
    UnPackHapp(String, oneshot::Sender<HolochainServiceResponse>),
    NewSignKeypair(oneshot::Sender<HolochainServiceResponse>),
    SignWithKey(
        HoloHash<Agent>,
        Vec<u8>,
        oneshot::Sender<HolochainServiceResponse>,
    ),
    EnableApp(String, oneshot::Sender<HolochainServiceResponse>),
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
    GetNetworkMetrics(Result<String, AnyError>),
    PackDna(Result<String, AnyError>),
    UnPackDna(Result<String, AnyError>),
    PackHapp(Result<String, AnyError>),
    UnPackHapp(Result<String, AnyError>),
    NewSignKeypair(Result<HoloHash<Agent>, AnyError>),
    SignWithKey(Result<Signature, AnyError>),
    EnableApp(Result<(), AnyError>),
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
        match response_receiver.await? {
            HolochainServiceResponse::CallZomeFunction(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn remove_app(&self, app_id: String) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::RemoveApp(app_id, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::RemoveApp(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn enable_app(&self, app_id: String) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::EnableApp(app_id, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::EnableApp(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn agent_infos(&self) -> Result<Vec<String>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::AgentInfos(response_tx))?;
        match response_rx.await? {
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
        match response_rx.await? {
            HolochainServiceResponse::AddAgentInfos(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn sign(&self, data: String) -> Result<Signature, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Sign(data, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::Sign(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn shutdown(&self) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::Shutdown(response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::Shutdown(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_agent_key(&self) -> Result<HoloHash<Agent>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAgentKey(response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::GetAgentKey(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn sign_with_key(
        &self,
        agent_key: HoloHash<Agent>,
        data: Vec<u8>,
    ) -> Result<Signature, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender.send(HolochainServiceRequest::SignWithKey(
            agent_key,
            data,
            response_tx,
        ))?;
        match response_rx.await? {
            HolochainServiceResponse::SignWithKey(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn new_sign_keypair_random(&self) -> Result<HoloHash<Agent>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::NewSignKeypair(response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::NewSignKeypair(result) => result,
            _ => unreachable!(),
        }
    }

    /// Resolve the Holochain agent key for one language (issue #1099).
    ///
    /// Each language gets its own agent key so that two languages bundling
    /// the same DNA + network seed produce distinct cells (cell id =
    /// DNA hash + agent pubkey) instead of colliding on one shared cell
    /// with last-writer-wins signal routing. Resolution order:
    ///
    /// 1. The mapping stored in Ad4mDb from a previous resolution.
    /// 2. Adoption: an app already installed under the language's own
    ///    app id predates per-language keys — keep its key so existing
    ///    installs keep their cell, source chain, and DHT identity.
    /// 3. A fresh keypair otherwise.
    ///
    /// The resolved key is persisted, so every path is stable across
    /// restarts.
    pub async fn agent_key_for_language(
        &self,
        language_address: &str,
        app_id: &str,
    ) -> Result<HoloHash<Agent>, AnyError> {
        // Serialize per language across lookup → adoption → keygen →
        // persist: a concurrent caller waits here and then finds the
        // winner's key in the stored mapping instead of generating its own.
        let lock = LANGUAGE_KEY_LOCKS
            .lock()
            .expect("LANGUAGE_KEY_LOCKS poisoned")
            .entry(language_address.to_string())
            .or_default()
            .clone();
        let _guard = lock.lock().await;

        let setting_key = format!("language_agent_key:{}", language_address);
        // A failed read must propagate: treating it as "no stored key"
        // would generate and persist a new key over a valid mapping,
        // silently forking the language's cell identity.
        if let Some(stored) =
            crate::db::Ad4mDb::with_global_instance(|db| db.get_setting(&setting_key))?
        {
            match holochain::prelude::AgentPubKey::try_from(stored.as_str()) {
                Ok(key) => return Ok(key),
                Err(_) => log::warn!(
                    "Stored agent key for language {} is in invalid format, re-resolving",
                    language_address
                ),
            }
        }
        let key = match self.get_app_info(app_id.to_string()).await? {
            Some(app_info) => app_info.agent_pub_key,
            None => self.new_sign_keypair_random().await?,
        };
        crate::db::Ad4mDb::with_global_instance(|db| {
            db.set_setting(&setting_key, &key.to_string())
        })?;
        Ok(key)
    }

    pub async fn get_app_info(&self, app_id: String) -> Result<Option<AppInfo>, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetAppInfo(app_id, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::GetAppInfo(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn log_network_metrics(&self) -> Result<(), AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::LogNetworkMetrics(response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::LogNetworkMetrics(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn get_network_metrics(&self) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::GetNetworkMetrics(response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::GetNetworkMetrics(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn pack_dna(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::PackDna(path, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::PackDna(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn unpack_dna(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::UnPackDna(path, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::UnPackDna(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn pack_happ(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::PackHapp(path, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::PackHapp(result) => result,
            _ => unreachable!(),
        }
    }

    pub async fn unpack_happ(&self, path: String) -> Result<String, AnyError> {
        let (response_tx, response_rx) = oneshot::channel();
        self.sender
            .send(HolochainServiceRequest::UnPackHapp(path, response_tx))?;
        match response_rx.await? {
            HolochainServiceResponse::UnPackHapp(result) => result,
            _ => unreachable!(),
        }
    }
}

lazy_static! {
    static ref HOLOCHAIN_SERVICE: Arc<RwLock<Option<HolochainServiceInterface>>> =
        Arc::new(RwLock::new(None));

    /// Serializes `agent_key_for_language` per language: without this, two
    /// concurrent first resolutions of the same language can both observe
    /// "no stored key, no installed app" and each generate + persist a
    /// different key — last write wins in the DB while apps may already be
    /// installed under the losing key (cell id = DNA hash + agent key).
    static ref LANGUAGE_KEY_LOCKS: std::sync::Mutex<std::collections::HashMap<String, Arc<Mutex<()>>>> =
        std::sync::Mutex::new(std::collections::HashMap::new());
}

/// Set while `HolochainService::init` is running. What `holochain_service_once_started`
/// waits on. Set and cleared only through `ConductorStarting`.
static CONDUCTOR_STARTING: AtomicBool = AtomicBool::new(false);

/// How long any accessor waits for the conductor before giving up. Also bounds a
/// generate/unlock waiting on another's startup (`agent::conductor_startup`).
pub(crate) const SERVICE_WAIT: Duration = Duration::from_secs(120);

/// Marks a conductor start in progress for as long as it is held. Clearing on drop means an
/// error, a panic or a cancelled task can't leave the flag set.
pub struct ConductorStarting(());

impl ConductorStarting {
    pub fn begin() -> Self {
        CONDUCTOR_STARTING.store(true, Ordering::SeqCst);
        Self(())
    }
}

impl Drop for ConductorStarting {
    fn drop(&mut self) {
        CONDUCTOR_STARTING.store(false, Ordering::SeqCst);
    }
}

/// Polls `get` until it returns something, `keep_waiting` turns false, or `timeout` passes.
/// Generic over `get` only so the tests needn't touch the global service.
async fn wait_for<T, Fut>(
    timeout: Duration,
    keep_waiting: impl Fn() -> bool,
    get: impl Fn() -> Fut,
) -> Option<T>
where
    Fut: std::future::Future<Output = Option<T>>,
{
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        if let Some(value) = get().await {
            return Some(value);
        }
        if !keep_waiting() {
            // Checked again: the start may have set the service just before it stopped waiting.
            return get().await;
        }
        if tokio::time::Instant::now() >= deadline {
            return None;
        }
        tokio::time::sleep(Duration::from_millis(200)).await;
    }
}

pub async fn get_holochain_service() -> HolochainServiceInterface {
    // Language threads may call this before HolochainService::init() has
    // finished setting the global — instead of panicking immediately we
    // give the conductor up to 120 seconds to start.
    wait_for(SERVICE_WAIT, || true, maybe_get_holochain_service)
        .await
        .unwrap_or_else(|| panic!("Holochain Conductor not started after 120s timeout"))
}

/// The Holochain service, waiting for it while a conductor start is in progress.
///
/// For callers that took `maybe_get_holochain_service()` to mean "not running" because the
/// conductor used to be up by the time unlock replied (see `agent::conductor_startup`). They
/// wait out a start instead of failing in the seconds after unlock. They still get `None` at
/// once when nothing is starting it (a locked agent, or a start that failed), and after 120s
/// if a start never finishes.
pub async fn holochain_service_once_started() -> Option<HolochainServiceInterface> {
    let service = wait_for(
        SERVICE_WAIT,
        || CONDUCTOR_STARTING.load(Ordering::SeqCst),
        maybe_get_holochain_service,
    )
    .await;
    if service.is_none() && CONDUCTOR_STARTING.load(Ordering::SeqCst) {
        log::warn!("Holochain conductor still starting after 120s; giving up waiting");
    }
    service
}

pub async fn maybe_get_holochain_service() -> Option<HolochainServiceInterface> {
    let lock = HOLOCHAIN_SERVICE.read().await;
    lock.clone()
}

pub async fn set_holochain_service(service: HolochainServiceInterface) {
    let mut lock = HOLOCHAIN_SERVICE.write().await;
    *lock = Some(service);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    use std::time::Instant;

    #[tokio::test]
    async fn wait_returns_none_at_once_when_nothing_is_starting() {
        let started = Instant::now();
        let found = wait_for(SERVICE_WAIT, || false, || async { None::<()> }).await;
        assert!(found.is_none());
        assert!(started.elapsed() < Duration::from_millis(100));
    }

    #[tokio::test]
    async fn wait_returns_the_value_once_a_start_provides_it() {
        let polls = AtomicUsize::new(0);
        let found = wait_for(
            SERVICE_WAIT,
            || true,
            || {
                let n = polls.fetch_add(1, Ordering::SeqCst);
                async move { (n >= 2).then_some(()) }
            },
        )
        .await;
        assert!(found.is_some());
    }

    #[tokio::test]
    async fn wait_gives_up_when_a_start_never_finishes() {
        let found = wait_for(Duration::from_millis(300), || true, || async { None::<()> }).await;
        assert!(found.is_none());
    }

    #[tokio::test]
    async fn wait_rechecks_after_the_start_stops() {
        let polls = AtomicUsize::new(0);
        let found = wait_for(
            SERVICE_WAIT,
            || false,
            || {
                let n = polls.fetch_add(1, Ordering::SeqCst);
                async move { (n >= 1).then_some(()) }
            },
        )
        .await;
        assert!(found.is_some());
    }

    #[tokio::test]
    async fn conductor_starting_clears_even_if_the_start_panics() {
        let task = tokio::spawn(async {
            let _starting = ConductorStarting::begin();
            assert!(CONDUCTOR_STARTING.load(Ordering::SeqCst));
            panic!("start failed");
        });
        assert!(task.await.is_err());
        assert!(!CONDUCTOR_STARTING.load(Ordering::SeqCst));
    }
}
