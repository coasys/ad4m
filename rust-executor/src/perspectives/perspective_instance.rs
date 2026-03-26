use super::sdna::{generic_link_fact, is_sdna_link};
use super::shacl_parser::parse_shacl_to_links;
use super::update_perspective;
use super::utils::{prolog_get_all_string_bindings, prolog_resolution_to_string};
use crate::agent::AgentContext;
use crate::agent::{create_signed_expression, did_for_context};
use crate::graphql::graphql_types::{
    DecoratedPerspectiveDiff, LinkMutations, LinkQuery, LinkStatus, NeighbourhoodSignalFilter,
    OnlineAgent, PerspectiveExpression, PerspectiveHandle, PerspectiveLinkUpdatedWithOwner,
    PerspectiveLinkWithOwner, PerspectiveQuerySubscriptionFilter, PerspectiveState,
    PerspectiveStateFilter,
};
use crate::languages::language::Language;
use crate::languages::LanguageController;
use crate::perspectives::utils::{prolog_get_first_binding, prolog_value_to_json_string};
use crate::prolog_service::get_prolog_service;
use crate::prolog_service::types::QueryResolution;
use crate::prolog_service::PrologService;
use crate::prolog_service::{
    engine_pool::FILTERING_THRESHOLD, DEFAULT_POOL_SIZE, DEFAULT_POOL_SIZE_WITH_FILTERING,
};
use crate::prolog_service::{PrologMode, PROLOG_MODE};
use crate::pubsub::{
    get_global_pubsub, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_LINK_ADDED_TOPIC,
    PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC,
    PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC, PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC,
    RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
};
use crate::{db::Ad4mDb, types::*};
use ad4m_client::literal::Literal;
use chrono::DateTime;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use futures::future;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::future::Future;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};
use tokio::time::{sleep, Instant};
use tokio::{join, time};
use urlencoding;
use uuid;
use uuid::Uuid;

static MAX_COMMIT_BYTES: usize = 3_000_000; //3MiB
static MAX_PENDING_DIFFS_COUNT: usize = 150;
static MAX_PENDING_SECONDS: u64 = 3;
static IMMEDIATE_COMMITS_COUNT: usize = 20;
static QUERY_SUBSCRIPTION_TIMEOUT: u64 = 60; // 1 minute in seconds (was 5 min)
static QUERY_SUBSCRIPTION_CHECK_INTERVAL: u64 = 200; // 200ms

fn notification_pool_name(uuid: &str) -> String {
    format!("notification_{}", uuid)
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum SdnaType {
    SubjectClass,
    Flow,
    Custom,
}

impl SdnaType {
    pub fn from_string(s: &str) -> Result<Self, AnyError> {
        match s {
            "subject_class" => Ok(SdnaType::SubjectClass),
            "flow" => Ok(SdnaType::Flow),
            "custom" => Ok(SdnaType::Custom),
            _ => Err(anyhow!(
                "Invalid SDNA type: {}. Must one of 'subject_class', 'flow' or 'custom'.",
                s
            )),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum Action {
    #[serde(rename = "addLink")]
    AddLink,
    #[serde(rename = "removeLink")]
    RemoveLink,
    #[serde(rename = "setSingleTarget")]
    SetSingleTarget,
    #[serde(rename = "collectionSetter")]
    CollectionSetter,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Command {
    source: Option<String>,
    predicate: Option<String>,
    target: Option<String>,
    local: Option<bool>,
    action: Action,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClass {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Class")]
    class: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassProperty {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Property")]
    property: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassCollection {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Collection")]
    collection: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassActions {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Actions")]
    actions: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct PorpertyValue {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Value")]
    value: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassOption {
    #[serde(rename = "className")]
    class_name: Option<String>,
    #[serde(rename = "query")]
    query: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Parameter {
    name: String,
    value: serde_json::Value,
}

#[derive(Clone)]
struct SubscribedQuery {
    query: String,
    last_result: String,
    last_keepalive: Instant,
    user_email: Option<String>,
}

#[derive(Clone)]
pub struct PerspectiveInstance {
    pub persisted: Arc<Mutex<PerspectiveHandle>>,

    pub created_from_join: bool,
    pub is_fast_polling: bool,
    pub retries: u32,

    is_teardown: Arc<Mutex<bool>>,
    sdna_change_mutex: Arc<Mutex<()>>,
    prolog_update_mutex: Arc<RwLock<()>>,
    link_language: Arc<RwLock<Option<Language>>>,
    trigger_notification_check: Arc<Mutex<bool>>,
    trigger_prolog_subscription_check: Arc<Mutex<bool>>,
    commit_debounce_timer: Arc<Mutex<Option<tokio::time::Instant>>>,
    immediate_commits_remaining: Arc<Mutex<usize>>,
    subscribed_queries: Arc<Mutex<HashMap<String, SubscribedQuery>>>,
    batch_store: Arc<RwLock<HashMap<String, PerspectiveDiff>>>,
    // Fallback sync tracking for ensure_public_links_are_shared
    last_successful_fallback_sync: Arc<Mutex<Option<tokio::time::Instant>>>,
    fallback_sync_interval: Arc<Mutex<Duration>>,
    sparql_service: Arc<crate::sparql_service::SparqlService>,
}

impl PerspectiveInstance {
    pub fn new(handle: PerspectiveHandle, created_from_join: Option<bool>) -> Self {
        PerspectiveInstance {
            persisted: Arc::new(Mutex::new(handle.clone())),

            created_from_join: created_from_join.unwrap_or(false),
            is_fast_polling: false,
            retries: 0,
            is_teardown: Arc::new(Mutex::new(false)),
            sdna_change_mutex: Arc::new(Mutex::new(())),
            prolog_update_mutex: Arc::new(RwLock::new(())),
            link_language: Arc::new(RwLock::new(None)),
            trigger_notification_check: Arc::new(Mutex::new(false)),
            trigger_prolog_subscription_check: Arc::new(Mutex::new(false)),
            commit_debounce_timer: Arc::new(Mutex::new(None)),
            immediate_commits_remaining: Arc::new(Mutex::new(IMMEDIATE_COMMITS_COUNT)),
            subscribed_queries: Arc::new(Mutex::new(HashMap::new())),
            batch_store: Arc::new(RwLock::new(HashMap::new())),
            last_successful_fallback_sync: Arc::new(Mutex::new(None)),
            fallback_sync_interval: Arc::new(Mutex::new(Duration::from_secs(30))),
            sparql_service: Arc::new(
                crate::sparql_service::SparqlService::new(None)
                    .expect("Failed to create per-perspective SPARQL service"),
            ),
        }
    }

    pub async fn start_background_tasks(self) {
        let _ = join!(
            self.ensure_link_language(),
            self.notification_check_loop(),
            self.nh_sync_loop(),
            self.pending_diffs_loop(),
            self.subscribed_queries_loop(),
            self.fallback_sync_loop()
        );
    }

    pub async fn teardown_background_tasks(&self) {
        *self.is_teardown.lock().await = true;
    }

    /// Sync all existing links to the SPARQL (Oxigraph) store
    pub fn sync_existing_links_to_sparql(
        &self,
        links: &[DecoratedLinkExpression],
    ) -> Result<(), deno_core::anyhow::Error> {
        self.sparql_service.reload(links.to_vec())
    }

    async fn ensure_link_language(&self) {
        let mut interval = time::interval(Duration::from_secs(5));
        while !*self.is_teardown.lock().await {
            if self.link_language.read().await.is_none()
                && self.persisted.lock().await.neighbourhood.is_some()
            {
                let nh = self
                    .persisted
                    .lock()
                    .await
                    .neighbourhood
                    .as_ref()
                    .expect("must be some")
                    .clone();

                log::debug!(
                    "ensure_link_language: checking language {} for perspective",
                    nh.data.link_language
                );

                match LanguageController::language_by_address(nh.data.link_language.clone()).await {
                    Ok(Some(mut language)) => {
                        // Set local agents before storing the language
                        let agents_to_register = {
                            let handle = self.persisted.lock().await;
                            log::debug!(
                                "🔍 ensure_link_language: perspective {} has owners: {:?}",
                                handle.uuid,
                                handle.owners
                            );
                            if let Some(owners) = &handle.owners {
                                if !owners.is_empty() {
                                    log::debug!("🔍 Using owners list: {:?}", owners);
                                    owners.clone()
                                } else {
                                    // Empty owners list - use main agent
                                    log::debug!("🔍 Owners list is empty, using main agent");
                                    vec![crate::agent::did()]
                                }
                            } else {
                                // No owners set - use main agent
                                log::debug!("🔍 No owners set, using main agent");
                                vec![crate::agent::did()]
                            }
                        };

                        log::info!(
                            "🔍 Setting local agents for link language: {:?}",
                            agents_to_register
                        );
                        if let Err(e) = language.set_local_agents(agents_to_register).await {
                            log::error!("Failed to set local agents on link language: {:?}", e);
                        }

                        {
                            let mut link_language_guard = self.link_language.write().await;
                            *link_language_guard = Some(language);
                        }
                        // Cache language→perspective mapping for fast signal routing
                        {
                            let handle = self.persisted.lock().await.clone();
                            crate::perspectives::register_link_language_perspective(
                                nh.data.link_language.clone(),
                                handle,
                            );
                        }
                        if self.persisted.lock().await.state
                            == PerspectiveState::NeighbourhoodCreationInitiated
                        {
                            self.ensure_public_links_are_shared().await;
                        }
                        self.update_perspective_state_log_error(
                            PerspectiveState::LinkLanguageInstalledButNotSynced,
                        )
                        .await;
                        break;
                    }
                    Ok(None) => {
                        log::debug!(
                            "Link language {} not installed yet, retrying in 5 seconds",
                            nh.data.link_language.clone()
                        );
                        self.update_perspective_state_log_error(
                            PerspectiveState::LinkLanguageFailedToInstall,
                        )
                        .await;
                    }
                    Err(e) => {
                        log::error!("Error when calling language_by_address: {:?}", e);
                        self.update_perspective_state_log_error(
                            PerspectiveState::LinkLanguageFailedToInstall,
                        )
                        .await;
                    }
                }
            }
            interval.tick().await;
        }
    }

    async fn nh_sync_loop(&self) {
        let mut interval = time::interval(Duration::from_secs(3));
        while !*self.is_teardown.lock().await {
            // Clone the link_language without holding the lock during sync
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                match link_language.sync().await {
                    Ok(_) => {
                        // Transition to Synced state on successful sync
                        let _ = self
                            .update_perspective_state(PerspectiveState::Synced)
                            .await;
                    }
                    Err(e) => {
                        log::error!("Error calling sync on link language: {:?}", e);
                        let _ = self
                            .update_perspective_state(
                                PerspectiveState::LinkLanguageInstalledButNotSynced,
                            )
                            .await;
                    }
                }
            }
            interval.tick().await;
        }
    }

    async fn pending_diffs_loop(&self) {
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut interval = time::interval(Duration::from_millis(100));
        let mut last_diff_time = None;

        while !*self.is_teardown.lock().await {
            interval.tick().await;

            if self.has_link_language().await {
                let (_, ids) = Ad4mDb::with_global_instance(|db| {
                    db.get_pending_diffs(&uuid, Some(MAX_PENDING_DIFFS_COUNT))
                })
                .unwrap_or((PerspectiveDiff::empty(), Vec::new()));

                if ids.is_empty() {
                    continue;
                }

                if last_diff_time.is_none() {
                    // First diff in a burst - start timer
                    last_diff_time = Some(tokio::time::Instant::now());
                }

                // Commit if either:
                // 1. It's been MAX_PENDING_SECONDS since first diff in burst (don't collect longer than MAX_PENDING_SECONDS)
                if last_diff_time.unwrap().elapsed() >= Duration::from_secs(MAX_PENDING_SECONDS) {
                    if self.commit_pending_diffs().await.is_ok() {
                        last_diff_time = None;
                        log::info!("Committed diffs after reaching 10s maximum wait time");
                    }
                // 2. It's been > 1s since last new diff (burst is over)
                } else if !self.has_new_diffs_in_last_second().await {
                    if self.commit_pending_diffs().await.is_ok() {
                        last_diff_time = None;
                        log::info!("Committed diffs after 1s of inactivity");
                    }
                // 3. We have collected more than 100 diffs
                } else if ids.len() >= MAX_PENDING_DIFFS_COUNT
                    && self.commit_pending_diffs().await.is_ok()
                {
                    last_diff_time = None;
                    log::info!("Committed diffs after collecting 100");
                }
            }
        }
    }

    async fn has_new_diffs_in_last_second(&self) -> bool {
        let timer = self.commit_debounce_timer.lock().await;
        timer
            .map(|instant| instant.elapsed() < tokio::time::Duration::from_secs(1))
            .unwrap_or(false)
    }

    async fn has_link_language(&self) -> bool {
        let link_language_guard = self.link_language.read().await;
        link_language_guard.is_some()
    }

    async fn commit_pending_diffs(&self) -> Result<(), AnyError> {
        let uuid = self.persisted.lock().await.uuid.clone();

        let (pending_diffs, pending_ids) = Ad4mDb::with_global_instance(|db| {
            db.get_pending_diffs_by_size(&uuid, MAX_COMMIT_BYTES, Some(MAX_PENDING_DIFFS_COUNT))
        })?;

        if !pending_ids.is_empty() {
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                log::info!("Committing {} pending diffs...", pending_ids.len());
                let commit_result = link_language.commit(pending_diffs).await;
                match commit_result {
                    Ok(Some(_)) => {
                        Ad4mDb::with_global_instance(|db| {
                            db.clear_pending_diffs(&uuid, pending_ids)
                        })?;
                        // Reset immediate commits counter after successful commit
                        self.set_immediate_commits(IMMEDIATE_COMMITS_COUNT).await;
                        log::info!("Successfully committed pending diffs");
                        Ok(())
                    }
                    Ok(None) => Err(anyhow!("No diff returned from commit")),
                    Err(e) => Err(e),
                }
            } else {
                Ok(()) // Keep diffs if no link language
            }
        } else {
            Ok(())
        }
    }

    async fn notification_check_loop(&self) {
        //log::debug!("Starting notification check loop for perspective {}", self.persisted.lock().await.uuid);
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut interval = time::interval(Duration::from_secs(5));
        let mut before = self.notification_trigger_snapshot().await;
        while !*self.is_teardown.lock().await {
            interval.tick().await;
            let changed = *(self.trigger_notification_check.lock().await);

            if changed {
                //log::debug!("Notification check loop triggered for perspective {}", uuid);
                //let start = std::time::Instant::now();
                *(self.trigger_notification_check.lock().await) = false;
                //let snapshot_start = std::time::Instant::now();

                let after = self.notification_trigger_snapshot().await;
                //let snapshot_duration = snapshot_start.elapsed();
                //log::debug!("Notification trigger snapshot took {:?} - for perspective {}", snapshot_duration, uuid);

                //let diff_start = std::time::Instant::now();
                let new_matches = Self::subtract_before_notification_matches(&before, &after);
                //let diff_duration = diff_start.elapsed();
                //log::debug!("Computing notification diff took {:?} - for perspective {}", diff_duration, uuid);

                tokio::spawn(Self::publish_notification_matches(
                    uuid.clone(),
                    new_matches,
                ));
                before = after;
                //let total_duration = start.elapsed();
                //log::debug!("Total notification check iteration took {:?} - for perspective {}", total_duration, uuid);
            }
        }
    }

    pub async fn ensure_public_links_are_shared(&self) -> bool {
        let uuid = self.persisted.lock().await.uuid.clone();

        // Clone link_language without holding the lock
        let link_language_clone = {
            let link_language_guard = self.link_language.read().await;
            link_language_guard.clone()
        };

        if let Some(mut link_language) = link_language_clone {
            // Query SPARQL store for all links
            let decorated_links = match self.sparql_service.get_all_links() {
                Ok(links) => links,
                Err(e) => {
                    log::error!(
                        "Failed to get links from SPARQL store in ensure_public_links_are_shared for perspective {}: {}",
                        uuid, e
                    );
                    return false;
                }
            };

            let mut local_links: Vec<(LinkExpression, LinkStatus)> = decorated_links
                .into_iter()
                .map(|decorated| {
                    let status = decorated.status.clone().unwrap_or(LinkStatus::Local);
                    (LinkExpression::from(decorated), status)
                })
                .collect();

            local_links.retain(|(_, status)| status == &LinkStatus::Shared);

            let remote_links = match link_language.current_revision().await {
                Ok(Some(_)) => {
                    link_language
                        .render()
                        .await
                        .unwrap_or(None)
                        .unwrap_or_default()
                        .links
                }
                _ => vec![],
            };

            let mut links_to_commit = Vec::new();
            for (local_link, _) in &local_links {
                if !remote_links.iter().any(|e| {
                    e.author == local_link.author
                        && e.timestamp == local_link.timestamp
                        && e.data.source == local_link.data.source
                        && e.data.target == local_link.data.target
                        && e.data.predicate == local_link.data.predicate
                }) {
                    links_to_commit.push(local_link.clone());
                }
            }

            if !links_to_commit.is_empty() {
                let links_count = links_to_commit.len();
                let result = link_language
                    .commit(PerspectiveDiff {
                        additions: links_to_commit,
                        removals: vec![],
                    })
                    .await;

                if let Err(e) = result {
                    log::error!("Error calling link language's commit in ensure_public_links_are_shared: {:?}", e);
                    return false;
                }
                log::debug!(
                    "Successfully committed {} links to link language in fallback sync",
                    links_count
                );
            }

            //Ad4mDb::with_global_instance(|db| db.add_many_links(&self.persisted.lock().await.uuid, &remote_links)).unwrap(); // Assuming add_many_links takes a reference to a Vec<LinkExpression> and returns Result<(), AnyError>
            return true;
        }
        false
    }

    pub async fn update_perspective_state(&self, state: PerspectiveState) -> Result<(), AnyError> {
        if self.persisted.lock().await.state != state {
            let mut handle = self.persisted.lock().await.clone();
            handle.state = state.clone();

            update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC,
                    &serde_json::to_string(&PerspectiveStateFilter {
                        perspective: handle,
                        state: serde_json::to_string(&state)
                            .expect("must be able to serialze PerspectiveState"),
                    })
                    .unwrap(),
                )
                .await;
        }
        Ok(())
    }

    async fn update_perspective_state_log_error(&self, state: PerspectiveState) {
        if let Err(e) = self.update_perspective_state(state).await {
            log::error!("Error updating perspective state: {:?}", e);
        }
    }

    pub async fn update_from_handle(&self, handle: PerspectiveHandle) {
        *self.persisted.lock().await = handle;
    }

    pub async fn commit(&self, diff: &PerspectiveDiff) -> Result<(), AnyError> {
        let handle = self.persisted.lock().await.clone();
        if handle.neighbourhood.is_none() {
            return Ok(());
        }

        // Seeing if we already have pending diffs, to not overtake older commits but instead add this one to the queue
        let (_, pending_ids) =
            Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&handle.uuid, Some(1)))
                .unwrap_or((PerspectiveDiff::empty(), Vec::new()));

        let commit_result = if pending_ids.is_empty() {
            // No pending diffs, let's try
            // Clone link_language without holding the lock
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                // Got Link Language reference
                if link_language.current_revision().await?.is_some() {
                    // Revision set, we are synced
                    // we are in a healthy Neighbourhood state and should be able to commit
                    // but let's make sure we're not DoS'ing the link language in bursts
                    let mut immediate_commits_remaining =
                        self.immediate_commits_remaining.lock().await;
                    if *immediate_commits_remaining > 0 {
                        *immediate_commits_remaining -= 1;
                        link_language.commit(diff.clone()).await
                    } else {
                        Err(anyhow!("Debouncing commit burst"))
                    }
                } else {
                    Err(anyhow!("Link Language not synced"))
                }
            } else {
                Err(anyhow!("LinkLanguage not available"))
            }
        } else {
            Err(anyhow!("Other pending diffs already in queue"))
        };

        let ok = match commit_result {
            Ok(Some(rev)) => {
                if rev.trim().is_empty() {
                    log::warn!("Committed but got no revision from LinkLanguage!\nStoring in pending diffs for later");
                    false
                } else {
                    log::info!("Committed to revision: {}", rev);
                    true
                }
            }
            Ok(None) => {
                log::warn!("Committed but got no revision from LinkLanguage!\nStoring in pending diffs for later");
                false
            }
            Err(e) => {
                log::warn!(
                    "Error trying to commit diff: {:?}\nStoring in pending diffs for later",
                    e
                );
                false
            }
        };

        if !ok {
            // Store diff in DB
            Ad4mDb::with_global_instance(|db| db.add_pending_diff(&handle.uuid, diff))?;
            // Update or start timer
            let mut timer = self.commit_debounce_timer.lock().await;
            *timer = Some(tokio::time::Instant::now());
        }

        Ok(())
    }

    // Add method to configure immediate commits
    pub async fn set_immediate_commits(&self, count: usize) {
        *self.immediate_commits_remaining.lock().await = count;
    }

    fn spawn_commit_and_handle_error(&self, diff: &PerspectiveDiff) {
        let self_clone = self.clone();
        let diff_clone = diff.clone();

        tokio::spawn(async move {
            if let Err(e) = self_clone.commit(&diff_clone).await {
                log::error!("PerspectiveInstance::commit() returned error: {:?}\nStoring in pending diffs for later", e);
                let handle_clone = self_clone.persisted.lock().await.clone();
                Ad4mDb::with_global_instance(|db|
                    db.add_pending_diff(&handle_clone.uuid, &diff_clone)
                ).expect("Couldn't write pending diff. DB should be initialized and usable at this point");
            }
        });
    }

    pub async fn diff_from_link_language(&self, diff: PerspectiveDiff) -> Result<(), AnyError> {
        // Deduplicate by (author, timestamp, source, predicate, target)
        // Use structured keys to avoid delimiter collision issues
        let mut seen_add: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut unique_additions: Vec<LinkExpression> = Vec::new();
        for link in diff.additions.iter() {
            let key_tuple = (
                &link.author,
                &link.timestamp,
                &link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                &link.data.target,
            );
            let key = serde_json::to_string(&key_tuple).unwrap_or_else(|_| {
                // Fallback to a simple hash if serialization fails
                format!("{:?}", key_tuple)
            });
            if seen_add.insert(key) {
                unique_additions.push(link.clone());
            }
        }

        let mut seen_rem: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut unique_removals: Vec<LinkExpression> = Vec::new();
        for link in diff.removals.iter() {
            let key_tuple = (
                &link.author,
                &link.timestamp,
                &link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                &link.data.target,
            );
            let key = serde_json::to_string(&key_tuple).unwrap_or_else(|_| {
                // Fallback to a simple hash if serialization fails
                format!("{:?}", key_tuple)
            });
            if seen_rem.insert(key) {
                unique_removals.push(link.clone());
            }
        }

        let decorated_diff = DecoratedPerspectiveDiff {
            additions: unique_additions
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
            removals: unique_removals
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
        };

        // Write to SurrealDB (primary storage for links)
        self.persist_link_diff(&decorated_diff).await?;

        // Update both Prolog engines: subscription (immediate) + query (lazy)
        self.update_prolog_engines(decorated_diff.clone()).await;
        self.pubsub_publish_diff(decorated_diff).await;

        Ok(())
    }

    pub async fn telepresence_signal_from_link_language(
        &self,
        mut signal: PerspectiveExpression,
        recipient_did: Option<String>,
    ) {
        signal.verify_signatures();
        let handle = self.persisted.lock().await.clone();

        log::debug!("telepresence_signal_from_link_language: perspective={}, recipient_did={:?}, signal_author={}",
            handle.uuid, recipient_did, signal.author);

        super::publish_telepresence_signal(handle, signal, recipient_did).await;
    }

    pub async fn add_link(
        &mut self,
        link: Link,
        status: LinkStatus,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        link.validate()?;
        let link_expr: LinkExpression = create_signed_expression(link.normalize(), context)?.into();
        self.add_link_expression(link_expr, status, batch_id).await
    }

    pub async fn remove_link(
        &mut self,
        link_expression: LinkExpression,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let _handle = self.persisted.lock().await.clone();

            // Query SurrealDB instead of Rusqlite
            let decorated_link = self
                .sparql_service
                .get_link(
                    &link_expression.data.source,
                    link_expression.data.predicate.as_deref(),
                    &link_expression.data.target,
                    &link_expression.author,
                    &link_expression.timestamp,
                )?
                .ok_or(anyhow!("Link not found"))?;

            let link_from_db = LinkExpression::from(decorated_link.clone());
            let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);

            diff.removals.push(link_from_db.clone());
            Ok(DecoratedLinkExpression::from((link_from_db, status)))
        } else {
            let _handle = self.persisted.lock().await.clone();

            // Query SurrealDB instead of Rusqlite
            if let Some(decorated_link) = self.sparql_service.get_link(
                &link_expression.data.source,
                link_expression.data.predicate.as_deref(),
                &link_expression.data.target,
                &link_expression.author,
                &link_expression.timestamp,
            )? {
                let link_from_db = LinkExpression::from(decorated_link.clone());
                let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);

                let diff = PerspectiveDiff::from_removals(vec![link_expression.clone()]);
                let decorated_link_result =
                    DecoratedLinkExpression::from((link_from_db, status.clone()));
                let decorated_diff =
                    DecoratedPerspectiveDiff::from_removals(vec![decorated_link_result.clone()]);

                // Remove from SurrealDB (primary storage)
                self.persist_link_diff(&decorated_diff).await?;

                // Update both Prolog engines: subscription (immediate) + query (lazy)
                self.update_prolog_engines(decorated_diff.clone()).await;

                self.pubsub_publish_diff(decorated_diff.clone()).await;

                if status == LinkStatus::Shared {
                    self.spawn_commit_and_handle_error(&diff);
                }

                Ok(decorated_link_result)
            } else {
                Err(anyhow!("Link not found"))
            }
        }
    }

    async fn pubsub_publish_diff(&self, decorated_diff: DecoratedPerspectiveDiff) {
        // Get handle without holding lock during pubsub operations
        let handle = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.clone()
        };

        // Publish link added events - one per owner for proper multi-user isolation
        let pubsub = get_global_pubsub().await;
        let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

        if let Some(owners) = owners_list {
            for link in &decorated_diff.additions {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_ADDED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                link: link.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }

            // Publish link removed events - one per owner for proper multi-user isolation
            for link in &decorated_diff.removals {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_REMOVED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                link: link.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }
        } else {
            // For perspectives without explicit owners (main agent), publish with main agent DID
            let main_agent_did = crate::agent::did();

            for link in &decorated_diff.additions {
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_ADDED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            link: link.clone(),
                            owner: main_agent_did.clone(),
                        })
                        .unwrap(),
                    )
                    .await;
            }

            for link in &decorated_diff.removals {
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_REMOVED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            link: link.clone(),
                            owner: main_agent_did.clone(),
                        })
                        .unwrap(),
                    )
                    .await;
            }
        }
    }

    pub async fn add_link_expression(
        &mut self,
        link_expression: LinkExpression,
        status: LinkStatus,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        link_expression.data.validate()?;
        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let mut link_expr = link_expression.clone();
            link_expr.status = Some(status.clone());
            diff.additions.push(link_expr.clone());

            return Ok(DecoratedLinkExpression::from((
                link_expr.clone(),
                status.clone(),
            )));
        }

        // Store link in SurrealDB (no longer using Rusqlite for links)
        let diff = PerspectiveDiff::from_additions(vec![link_expression.clone()]);
        let decorated_link_expression =
            DecoratedLinkExpression::from((link_expression.clone(), status.clone()));
        let decorated_perspective_diff =
            DecoratedPerspectiveDiff::from_additions(vec![decorated_link_expression.clone()]);

        // Write to SurrealDB (primary storage for links)
        self.persist_link_diff(&decorated_perspective_diff).await?;

        // Update both Prolog engines: subscription (immediate) + query (lazy)
        self.update_prolog_engines(decorated_perspective_diff.clone())
            .await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
        }

        self.pubsub_publish_diff(decorated_perspective_diff).await;
        Ok(decorated_link_expression)
    }

    pub async fn add_links(
        &mut self,
        links: Vec<Link>,
        status: LinkStatus,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        for link in &links {
            link.validate()?;
        }
        let link_expressions: Result<Vec<_>, _> = links
            .into_iter()
            .map(|l| create_signed_expression(l.normalize(), context).map(LinkExpression::from))
            .collect();
        let link_expressions = link_expressions?;

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let mut decorated_expressions = Vec::new();
            for mut link_expr in link_expressions {
                link_expr.status = Some(status.clone());
                diff.additions.push(link_expr.clone());
                decorated_expressions
                    .push(DecoratedLinkExpression::from((link_expr, status.clone())));
            }

            Ok(decorated_expressions)
        } else {
            let decorated_link_expressions = link_expressions
                .clone()
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>();

            let perspective_diff = PerspectiveDiff::from_additions(link_expressions.clone());
            let decorated_perspective_diff =
                DecoratedPerspectiveDiff::from_additions(decorated_link_expressions.clone());

            // Write to SurrealDB (primary storage for links)
            self.persist_link_diff(&decorated_perspective_diff).await?;

            self.spawn_prolog_facts_update(decorated_perspective_diff.clone(), None);
            self.pubsub_publish_diff(decorated_perspective_diff).await;

            if status == LinkStatus::Shared {
                self.spawn_commit_and_handle_error(&perspective_diff);
            }

            Ok(decorated_link_expressions)
        }
    }

    pub async fn link_mutations(
        &mut self,
        mutations: LinkMutations,
        status: LinkStatus,
        context: &AgentContext,
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        let addition_links: Vec<Link> = mutations.additions.into_iter().map(Link::from).collect();
        for link in &addition_links {
            link.validate()?;
        }
        let additions = addition_links
            .into_iter()
            .map(|l| create_signed_expression(l.normalize(), context))
            .map(|r| r.map(LinkExpression::from))
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let removals = mutations
            .removals
            .into_iter()
            .map(LinkExpression::try_from)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;

        let diff = PerspectiveDiff::from(additions.clone(), removals.clone());
        let decorated_diff = DecoratedPerspectiveDiff {
            additions: additions
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>(),
            removals: removals
                .clone()
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>(),
        };

        // Write to SurrealDB (primary storage for links)
        self.persist_link_diff(&decorated_diff).await?;

        self.spawn_prolog_facts_update(decorated_diff.clone(), None);
        self.pubsub_publish_diff(decorated_diff.clone()).await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
            // Reset fallback sync interval when new shared links are added
            self.reset_fallback_sync_interval().await;
        }
        Ok(decorated_diff)
    }

    pub async fn update_link(
        &mut self,
        old_link: LinkExpression,
        new_link: Link,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();

        // Query SurrealDB instead of Rusqlite
        let decorated_link_option = self.sparql_service.get_link(
            &old_link.data.source,
            old_link.data.predicate.as_deref(),
            &old_link.data.target,
            &old_link.author,
            &old_link.timestamp,
        )?;

        let (_link, link_status) = match decorated_link_option {
            Some(decorated) => {
                let status = decorated.status.clone().unwrap_or(LinkStatus::Local);
                (LinkExpression::from(decorated), status)
            }
            None => {
                return Err(AnyError::msg(format!(
                    "NH [{}] ({}) Link not found in perspective \"{}\": {:?}",
                    handle
                        .shared_url
                        .clone()
                        .unwrap_or("not-shared".to_string()),
                    handle.name.clone().unwrap_or("<no name>".to_string()),
                    handle.uuid,
                    old_link
                )))
            }
        };

        let new_link_expression =
            LinkExpression::from(create_signed_expression(new_link.normalize(), context)?);

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            diff.removals.push(old_link.clone());
            let mut new_link_expr = new_link_expression.clone();
            new_link_expr.status = Some(link_status.clone());
            diff.additions.push(new_link_expr.clone());

            Ok(DecoratedLinkExpression::from((new_link_expr, link_status)))
        } else {
            let diff =
                PerspectiveDiff::from(vec![new_link_expression.clone()], vec![old_link.clone()]);
            let decorated_new_link_expression =
                DecoratedLinkExpression::from((new_link_expression.clone(), link_status.clone()));
            let decorated_old_link =
                DecoratedLinkExpression::from((old_link.clone(), link_status.clone()));
            let decorated_diff = DecoratedPerspectiveDiff::from(
                vec![decorated_new_link_expression.clone()],
                vec![decorated_old_link.clone()],
            );

            // Write to SurrealDB (primary storage for links)
            self.persist_link_diff(&decorated_diff).await?;

            // Update both Prolog engines: subscription (immediate) + query (lazy)
            self.update_prolog_engines(decorated_diff.clone()).await;

            // Publish link updated events - one per owner for proper multi-user isolation
            let pubsub = get_global_pubsub().await;
            let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

            if let Some(owners) = owners_list {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_UPDATED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkUpdatedWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                old_link: decorated_old_link.clone(),
                                new_link: decorated_new_link_expression.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            } else {
                // For perspectives without explicit owners (main agent), publish with main agent DID
                let main_agent_did = crate::agent::did();
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_UPDATED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkUpdatedWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            old_link: decorated_old_link.clone(),
                            new_link: decorated_new_link_expression.clone(),
                            owner: main_agent_did,
                        })
                        .unwrap(),
                    )
                    .await;
            }

            if link_status == LinkStatus::Shared {
                self.spawn_commit_and_handle_error(&diff);
            }
            Ok(decorated_new_link_expression)
        }
    }

    pub async fn remove_links(
        &mut self,
        link_expressions: Vec<LinkExpression>,
        batch_id: Option<String>,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let _handle = self.persisted.lock().await.clone();

        // Filter to only existing links and collect their statuses
        let mut existing_links = Vec::new();
        for link in link_expressions {
            // Query SurrealDB instead of Rusqlite
            if let Some(decorated_link) = self.sparql_service.get_link(
                &link.data.source,
                link.data.predicate.as_deref(),
                &link.data.target,
                &link.author,
                &link.timestamp,
            )? {
                let link_from_db = LinkExpression::from(decorated_link.clone());
                let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);
                existing_links.push((link_from_db, status));
            }
        }

        // Skip if no links found
        if existing_links.is_empty() {
            return Ok(Vec::new());
        }

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let decorated_links: Vec<_> = existing_links
                .iter()
                .map(|(link, status)| {
                    diff.removals.push(link.clone());
                    DecoratedLinkExpression::from((link.clone(), status.clone()))
                })
                .collect();

            Ok(decorated_links)
        } else {
            // Split into links and statuses
            let (links, statuses): (Vec<_>, Vec<_>) = existing_links.into_iter().unzip();

            // Create decorated versions
            let decorated_links: Vec<DecoratedLinkExpression> = links
                .into_iter()
                .zip(statuses.iter())
                .map(|(link, status)| DecoratedLinkExpression::from((link, status.clone())))
                .collect();

            let decorated_diff = DecoratedPerspectiveDiff::from_removals(decorated_links.clone());

            // Remove from SurrealDB (primary storage)
            self.persist_link_diff(&decorated_diff).await?;

            // Update both Prolog engines: subscription (immediate) + query (lazy)
            self.update_prolog_engines(decorated_diff.clone()).await;
            self.pubsub_publish_diff(decorated_diff).await;

            // Only commit shared links by filtering decorated_links
            let shared_links: Vec<LinkExpression> = decorated_links
                .iter()
                .filter(|link| link.status == Some(LinkStatus::Shared))
                .map(|link| link.clone().into())
                .collect();

            if !shared_links.is_empty() {
                let shared_diff = PerspectiveDiff {
                    additions: vec![],
                    removals: shared_links,
                };
                self.spawn_commit_and_handle_error(&shared_diff);
            }

            Ok(decorated_links)
        }
    }

    /// Helper function to efficiently fetch only SDNA-related links from the database
    /// This makes two targeted queries instead of fetching all links:
    /// 1. Links with source == "ad4m://self" (SDNA declarations)
    /// 2. Links with predicate == "ad4m://sdna" (SDNA code)
    async fn get_sdna_links_local(&self) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        // Query 1: Get all links from ad4m://self (SDNA declarations)
        let self_links = self.get_links_local(&LinkQuery {
            source: Some("ad4m://self".to_string()),
            ..Default::default()
        })?;

        // Query 2: Get all links with predicate ad4m://sdna (SDNA code)
        let sdna_code_links = self.get_links_local(&LinkQuery {
            predicate: Some("ad4m://sdna".to_string()),
            ..Default::default()
        })?;

        // Combine both result sets (using a HashSet to avoid duplicates)
        let mut seen = std::collections::HashSet::new();
        let mut all_sdna_links = Vec::new();

        for link in self_links.into_iter().chain(sdna_code_links) {
            let key = (
                link.0.data.source.clone(),
                link.0.data.predicate.clone(),
                link.0.data.target.clone(),
                link.0.author.clone(),
                link.0.timestamp.clone(),
                link.1.clone(), // Include LinkStatus
            );
            if seen.insert(key) {
                all_sdna_links.push(link);
            }
        }

        Ok(all_sdna_links)
    }

    /// Get all subject class names from SHACL links (Prolog-free implementation)
    ///
    /// This queries links with:
    /// - predicate = "rdf://type"
    /// - target = "ad4m://SubjectClass"
    ///
    /// The source of these links is the class URI (e.g., "recipe://Recipe")
    /// We extract the class name from the URI.
    pub async fn get_subject_classes_from_shacl(&self) -> Result<Vec<String>, AnyError> {
        let uuid = self.persisted.lock().await.uuid.clone();
        log::debug!(
            "🔶 get_subject_classes_from_shacl: uuid={}, Querying for SHACL class links",
            uuid
        );
        // Query for SHACL class definition links
        let shacl_class_links = self.get_links_local(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })?;
        log::debug!(
            "🔶 get_subject_classes_from_shacl: Found {} links",
            shacl_class_links.len()
        );
        for (link, _status) in &shacl_class_links {
            log::debug!(
                "🔶 get_subject_classes_from_shacl: Link: {} -> {:?} -> {}",
                link.data.source,
                link.data.predicate,
                link.data.target
            );
        }

        // Extract class names from source URIs
        let mut class_names: Vec<String> = shacl_class_links
            .iter()
            .filter_map(|(link, _status)| {
                let source = &link.data.source;
                // Class URI format: "namespace://ClassName" (e.g., "recipe://Recipe")
                // We want to extract "ClassName"
                if let Some(idx) = source.rfind("://") {
                    let after_scheme = &source[idx + 3..];
                    // Handle paths like "namespace://path/ClassName"
                    if let Some(last_slash) = after_scheme.rfind('/') {
                        Some(after_scheme[last_slash + 1..].to_string())
                    } else {
                        Some(after_scheme.to_string())
                    }
                } else {
                    None
                }
            })
            .collect();

        // Remove duplicates
        class_names.sort();
        class_names.dedup();

        Ok(class_names)
    }

    fn get_links_local(
        &self,
        query: &LinkQuery,
    ) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        let from_date = query.from_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });
        let until_date = query.until_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });

        let decorated_links = self.sparql_service.query_links(
            query.source.as_deref(),
            query.predicate.as_deref(),
            query.target.as_deref(),
            from_date.as_deref(),
            until_date.as_deref(),
            None, // Don't limit here — get_links() applies limit after sorting
        )?;

        let result: Vec<(LinkExpression, LinkStatus)> = decorated_links
            .into_iter()
            .map(|decorated| {
                let status = decorated.status.clone().unwrap_or(LinkStatus::Shared);
                let link_expr = LinkExpression {
                    author: decorated.author,
                    timestamp: decorated.timestamp,
                    data: decorated.data,
                    proof: ExpressionProof {
                        key: decorated.proof.key,
                        signature: decorated.proof.signature,
                    },
                    status: Some(status.clone()),
                };
                (link_expr, status)
            })
            .collect();

        Ok(result)
    }

    pub async fn get_links(&self, q: &LinkQuery) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let mut reverse = false;
        let mut query = q.clone();

        if let Some(until_date) = query.until_date.as_ref() {
            if let Some(from_date) = query.from_date.as_ref() {
                let chrono_from_date: chrono::DateTime<chrono::Utc> = from_date.clone().into();
                let chrono_until_date: chrono::DateTime<chrono::Utc> = until_date.clone().into();
                if chrono_from_date > chrono_until_date {
                    reverse = true;
                    query.from_date.clone_from(&q.until_date);
                    query.until_date.clone_from(&q.from_date);
                }
            }
        }

        let mut links = self.get_links_local(&query)?;

        links.sort_by(|(a, _), (b, _)| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap_or_default();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap_or_default();
            if reverse {
                b_time.cmp(&a_time)
            } else {
                a_time.cmp(&b_time)
            }
        });

        if let Some(limit) = query.limit {
            let limit = links.len().min(limit as usize);
            links = links[..limit].to_vec();
        }

        Ok(links
            .into_iter()
            .map(|(link, status)| DecoratedLinkExpression::from((link.clone(), status)))
            .collect())
    }

    /// Adds the given Social DNA code to the perspective's SDNA code
    /// If shacl_json is provided, also stores SHACL as queryable RDF links
    pub async fn add_sdna(
        &mut self,
        name: String,
        mut sdna_code: String,
        sdna_type: SdnaType,
        shacl_json: Option<String>,
        context: &AgentContext,
    ) -> Result<bool, AnyError> {
        //let mut added = false;
        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;

        let predicate = match sdna_type {
            SdnaType::SubjectClass => "ad4m://has_subject_class",
            SdnaType::Flow => "ad4m://has_flow",
            SdnaType::Custom => "ad4m://has_custom_sdna",
        };

        let literal_name = Literal::from_string(name.clone())
            .to_url()
            .expect("just initialized Literal couldn't be turned into URL");

        let mut sdna_links: Vec<Link> = Vec::new();

        // Check if SHACL definition already exists for this class BEFORE doing anything
        if matches!(sdna_type, SdnaType::SubjectClass) {
            // Check for any existing SubjectClass with this name, regardless of namespace
            // We query by target (ad4m://SubjectClass) and then filter by class name
            let all_class_links = self.get_links_local(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })?;

            // Check if any existing class matches this name
            let exists = all_class_links.iter().any(|(link, _)| {
                // Extract class name from source URI (e.g., "flux://Channel" -> "Channel")
                link.data
                    .source
                    .split("://")
                    .last()
                    .and_then(|s| s.split('/').last())
                    .map(|class_name| class_name == name)
                    .unwrap_or(false)
            });

            if exists {
                log::info!(
                    "Class '{}' SHACL definition already exists, skipping duplicate",
                    name
                );
                return Ok(true);
            }
        }

        if (Literal::from_url(sdna_code.clone())).is_err() {
            sdna_code = Literal::from_string(sdna_code)
                .to_url()
                .expect("just initialized Literal couldn't be turned into URL");
        }

        // let links = self
        //     .get_links(&LinkQuery {
        //         source: Some("ad4m://self".to_string()),
        //         predicate: Some(predicate.to_string()),
        //         target: Some(literal_name.clone()),
        //         from_date: None,
        //         until_date: None,
        //         limit: None,
        //     })
        //     .await?;
        // let author = agent::did();
        // let links = links
        //     .into_iter()
        //     .filter(|l| l.author == author)
        //     .collect::<Vec<DecoratedLinkExpression>>();
        //if links.is_empty() {
        sdna_links.push(Link {
            source: "ad4m://self".to_string(),
            predicate: Some(predicate.to_string()),
            target: literal_name.clone(),
        });

        // Store the Prolog code for backward compatibility with getSdna()
        // SHACL links are the source of truth for schema operations,
        // but Prolog code is still stored for retrieval
        sdna_links.push(Link {
            source: literal_name.clone(),
            predicate: Some("ad4m://sdna".to_string()),
            target: sdna_code.clone(),
        });

        self.add_links(sdna_links, LinkStatus::Shared, None, context)
            .await?;

        // Handle SHACL links if SHACL JSON provided explicitly
        if let Some(shacl) = shacl_json {
            let shacl_links = parse_shacl_to_links(&shacl, &name)?;
            self.add_links(shacl_links, LinkStatus::Shared, None, context)
                .await?;
        }

        //added = true;
        //}
        // Mutex guard is automatically dropped here
        Ok(true)
    }

    async fn ensure_prolog_engine_pool(&self) -> Result<(), AnyError> {
        // Get service reference and perspective data BEFORE acquiring write lock
        let service = get_prolog_service().await;
        let (uuid, owner_did, neighbourhood_author) = {
            let persisted = self.persisted.lock().await;
            let uuid = persisted.uuid.clone();
            let owner_did = persisted.get_primary_owner();
            let neighbourhood_author = persisted.neighbourhood.as_ref().map(|n| n.author.clone());
            (uuid, owner_did, neighbourhood_author)
        };

        // Check if initialization is needed WITHOUT holding any locks
        if !service.has_perspective_pool(uuid.clone()).await
            || !service
                .has_perspective_pool(notification_pool_name(&uuid))
                .await
        {
            // Get all links BEFORE acquiring write lock to avoid deadlock
            let all_links = self.get_links(&LinkQuery::default()).await?;

            // NOW take write lock after all async operations that might need locks are done
            let _guard = self.prolog_update_mutex.write().await;

            // Check if pool exists under the write lock
            if !service.has_perspective_pool(uuid.clone()).await {
                let pool_size = if all_links.len() > FILTERING_THRESHOLD {
                    Some(DEFAULT_POOL_SIZE_WITH_FILTERING)
                } else {
                    Some(DEFAULT_POOL_SIZE)
                };
                // Create and initialize new pool
                service
                    .ensure_perspective_pool(uuid.clone(), pool_size)
                    .await?;
                service
                    .update_perspective_links(
                        uuid.clone(),
                        "facts".to_string(),
                        all_links.clone(),
                        neighbourhood_author.clone(),
                        owner_did.clone(),
                    )
                    .await?;
            }

            let notification_pool = format!("notification_{}", uuid);

            if !service
                .has_perspective_pool(notification_pool.clone())
                .await
            {
                // Create and initialize new pool
                service
                    .ensure_perspective_pool(notification_pool.clone(), Some(1))
                    .await?;
                service
                    .update_perspective_links(
                        notification_pool,
                        "facts".to_string(),
                        all_links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await?;
            }
        }

        Ok(())
    }

    /// Get the appropriate prolog pool ID for the given context
    fn get_pool_id_for_context(&self, perspective_uuid: &str, context: &AgentContext) -> String {
        match &context.user_email {
            Some(user_email) => {
                // User-specific pool: "uuid_user_email"
                format!("{}_{}", perspective_uuid, user_email)
            }
            None => {
                // Main agent pool: just the uuid
                perspective_uuid.to_string()
            }
        }
    }

    /// Common helper for executing prolog queries with configurable pool, lock, and executor
    async fn prolog_query_helper<F, Fut>(
        &self,
        query: String,
        use_lock: bool,
        pool_provider: impl FnOnce(&String) -> String,
        executor: F,
    ) -> Result<QueryResolution, AnyError>
    where
        F: FnOnce(Arc<PrologService>, String, String) -> Fut,
        Fut: Future<Output = Result<Result<QueryResolution, String>, AnyError>> + Send,
    {
        let total_start = std::time::Instant::now();
        log::trace!(
            "🧠🧠 Prolog query starting: {} (chars: {})",
            query.chars().take(100).collect::<String>(),
            query.len()
        );

        let ensure_start = std::time::Instant::now();
        self.ensure_prolog_engine_pool().await?;
        log::trace!("🧠🔧 Engine pool ensured in {:?}", ensure_start.elapsed());

        let uuid_start = std::time::Instant::now();
        let uuid = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.uuid.clone()
        };
        log::trace!("🧠🔑 UUID retrieved in {:?}", uuid_start.elapsed());

        let service_start = std::time::Instant::now();
        let service = get_prolog_service().await;
        log::trace!("🧠📞 Service retrieved in {:?}", service_start.elapsed());

        let pool_name = pool_provider(&uuid);

        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        let lock_start = std::time::Instant::now();
        let _read_lock = if use_lock {
            log::trace!("🧠🔒 Waiting for prolog_update_mutex read lock...");
            let guard = self.prolog_update_mutex.read().await;
            log::trace!(
                "🧠✅ Acquired prolog_update_mutex read lock in {:?}",
                lock_start.elapsed()
            );
            Some(guard)
        } else {
            None
        };

        // Execute query with periodic logging and timeout handling
        log::trace!("🧠⏳ Starting query execution...");
        let execute_start = std::time::Instant::now();

        // Spawn a task that logs every 10 seconds while the query is running
        let query_for_logging = query.clone();
        let execute_start_for_logging = execute_start.clone();
        let logging_handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(10));
            interval.tick().await;
            loop {
                interval.tick().await;
                let elapsed = execute_start_for_logging.elapsed();
                log::warn!(
                    "🧠⏰ Prolog query still running after {:?}. Query:\n{}",
                    elapsed,
                    query_for_logging
                );
            }
        });

        // Execute query with a 60-second timeout
        let timeout_duration = std::time::Duration::from_secs(60);
        let service = Arc::new(service);
        let query_clone = query.clone();
        let result_future = executor(service, pool_name, query_clone);
        let result_with_timeout = tokio::time::timeout(timeout_duration, result_future).await;

        // Cancel the logging task since query completed or timed out
        logging_handle.abort();

        let result: Result<Result<QueryResolution, String>, AnyError> = match result_with_timeout {
            Ok(r) => {
                log::trace!(
                    "🧠✅ Query execution succeeded in {:?}",
                    execute_start.elapsed()
                );
                r
            }
            Err(_) => {
                log::error!(
                    "🧠⏱️💥 Prolog query timed out after {:?} (60s limit)\nQuery was: {}",
                    execute_start.elapsed(),
                    query
                );
                return Err(anyhow!(
                    "Prolog query execution timed out after 60 seconds. Query: {}",
                    query
                ));
            }
        };

        let match_result = match result {
            Err(e) => {
                log::error!(
                    "🧠💥 Prolog query failed after {:?}: {:?}\nQuery was: {}",
                    execute_start.elapsed(),
                    e,
                    query
                );
                Err(anyhow!(e))
            }
            Ok(resolution) => resolution.map_err(|e| {
                log::error!(
                    "🧠💥 Prolog query resolution error after {:?}: {}\nQuery was: {}",
                    execute_start.elapsed(),
                    e,
                    query
                );
                anyhow!(e)
            }),
        };

        // Log result count and total time
        if let Ok(ref resolution) = match_result {
            let result_count = match resolution {
                QueryResolution::Matches(matches) => matches.len(),
                QueryResolution::True => 1,
                QueryResolution::False => 0,
            };
            log::trace!(
                "🧠🧠🧠 Prolog query:\n{}\n==>> Result count: {}",
                query,
                result_count
            );
        }
        log::trace!("🧠⏱️ TOTAL query time: {:?}", total_start.elapsed());

        match_result
    }

    /// Executes a Prolog query against the perspective's main pool
    /// locks the prolog_update_mutex
    /// uses run_query_smart
    // pub async fn prolog_query(&self, query: String) -> Result<QueryResolution, AnyError> {
    //     self.prolog_query_helper(
    //         query,
    //         true,
    //         |uuid| uuid.clone(),
    //         |service, pool, q| async move { service.run_query_smart(pool, q).await },
    //     )
    //     .await
    // }

    /// Helper to mark the Prolog engine as dirty (needs update before next query)
    /// Only applies to Simple mode
    /// Note: SdnaOnly mode doesn't use dirty flag - it compares SDNA links directly to avoid rebuilding on non-SDNA changes
    async fn mark_prolog_engine_dirty(&self) {
        if PROLOG_MODE == PrologMode::Simple {
            let perspective_uuid = self.persisted.lock().await.uuid.clone();
            get_prolog_service()
                .await
                .mark_dirty(&perspective_uuid)
                .await;
        }
    }

    /// Combined helper: spawns Prolog facts update AND marks query engine as dirty
    /// This is the common pattern throughout the codebase
    async fn update_prolog_engines(&self, diff: DecoratedPerspectiveDiff) {
        // Update subscription engine (immediate via spawned task)
        self.spawn_prolog_facts_update(diff, None);

        // Mark query engine dirty for lazy update on next query
        self.mark_prolog_engine_dirty().await;
    }

    /// Helper for Simple/SdnaOnly modes: extracts perspective metadata, fetches appropriate links,
    /// and calls the appropriate service method
    async fn execute_simple_mode_query(
        &self,
        query: String,
        use_subscription_engine: bool,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        let service = get_prolog_service().await;

        // Extract perspective metadata (same for Simple and SdnaOnly)
        let (perspective_uuid, neighbourhood_author) = {
            let persisted_guard = self.persisted.lock().await;
            (
                persisted_guard.uuid.clone(),
                persisted_guard
                    .neighbourhood
                    .as_ref()
                    .map(|n| n.author.clone()),
            )
        };

        // Override owner_did with current user's DID if context is provided (for multi-user prolog isolation)
        let user_did = did_for_context(context)?;

        // Fetch links based on mode
        let mut links: Vec<DecoratedLinkExpression> = match PROLOG_MODE {
            PrologMode::Simple => {
                // Get all links for Simple mode
                self.get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect()
            }
            PrologMode::SdnaOnly => {
                // Get only SDNA links for SdnaOnly mode (efficient query)
                self.get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect()
            }
            _ => Vec::new(), // Should never reach here given the callers
        };

        // Filter to only show SDNA links created by this user
        links.retain(|link| {
            // Keep SDNA links only if authored by this user
            link.data.source == "ad4m://self"
                && (link.author == user_did || Some(&link.author) == neighbourhood_author.as_ref())
                || link.data.predicate.as_ref().map(|p| p.as_str()) == Some("ad4m://sdna")
                    && (link.author == user_did
                        || Some(&link.author) == neighbourhood_author.as_ref())
                || (link.data.source != "ad4m://self"
                    && link.data.predicate.as_ref().map(|p| p.as_str()) != Some("ad4m://sdna"))
        });

        // Execute the query using the appropriate engine
        let result = if use_subscription_engine {
            service
                .run_query_subscription_simple(
                    &perspective_uuid,
                    query,
                    &links,
                    neighbourhood_author,
                    Some(user_did),
                )
                .await
        } else {
            service
                .run_query_simple(
                    &perspective_uuid,
                    query,
                    &links,
                    neighbourhood_author,
                    Some(user_did),
                )
                .await
        };

        result.map_err(|e| anyhow!("{}", e))
    }

    /// Executes a Prolog query with user context - uses context-specific pool
    /// locks the prolog_update_mutex
    /// uses run_query_smart
    pub async fn prolog_query_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                self.execute_simple_mode_query(query, false, context).await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach
                let perspective_uuid = {
                    let persisted_guard = self.persisted.lock().await;
                    persisted_guard.uuid.clone()
                };

                // Ensure the user-specific pool exists
                self.ensure_prolog_engine_pool_for_context(context).await?;

                self.prolog_query_helper(
                    query,
                    true,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_smart(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                // Return empty matches instead of False/Error to allow SHACL-based SDNA to work
                Ok(QueryResolution::Matches(vec![]))
            }
        }
    }

    /// Executes a Prolog subscription query against the perspective's main pool
    /// locks the prolog_update_mutex
    /// uses run_query_subscription
    pub async fn prolog_query_subscription(
        &self,
        query: String,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                self.execute_simple_mode_query(query, true, &AgentContext::main_agent())
                    .await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach
                self.prolog_query_helper(
                    query,
                    true,
                    |uuid| uuid.clone(),
                    |service, pool, q| async move { service.run_query_subscription(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog subscription query received but Prolog is DISABLED (query: {}), returning empty result",
                    query
                );
                // Return empty result instead of error to allow SHACL-based SDNA to work
                Ok(QueryResolution::False)
            }
        }
    }

    /// Executes a Prolog subscription query with user context - uses context-specific pool
    /// locks the prolog_update_mutex
    /// uses run_query_subscription
    pub async fn prolog_query_subscription_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Context is now properly used for SDNA filtering per-user
                self.execute_simple_mode_query(query, true, context).await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach with context
                let perspective_uuid = {
                    let persisted_guard = self.persisted.lock().await;
                    persisted_guard.uuid.clone()
                };

                self.prolog_query_helper(
                    query,
                    true,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_subscription(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog subscription query received but Prolog is DISABLED (query: {}), returning empty result",
                    query
                );
                // Return empty result instead of error to allow SHACL-based SDNA to work
                Ok(QueryResolution::False)
            }
        }
    }

    /// Executes a Prolog query directly on the SDNA pool for maximum performance
    ///
    /// This bypasses all smart routing logic and goes directly to the SDNA pool.
    /// Use this for subject class queries during create_subject flow for best performance.
    /// Only use this for queries that you KNOW should be handled by the SDNA pool.
    ///
    /// does not lock the prolog_update_mutex
    /// uses run_query_sdna
    pub async fn prolog_query_sdna(&self, query: String) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple => {
                // In Simple mode, route to Simple engine which has SDNA facts
                let service = get_prolog_service().await;
                let (perspective_uuid, owner_did, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let owner_did = persisted_guard.get_primary_owner();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, owner_did, neighbourhood_author)
                };

                // Get links for SDNA fact generation
                let links = self
                    .get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::SdnaOnly => {
                // In SdnaOnly mode, route to Simple engine with only SDNA links
                let service = get_prolog_service().await;
                let (perspective_uuid, owner_did, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let owner_did = persisted_guard.get_primary_owner();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, owner_did, neighbourhood_author)
                };

                // Get only SDNA-related links from database (efficient query)
                let links = self
                    .get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::Pooled => {
                // In pooled mode, use dedicated SDNA pool
                self.prolog_query_helper(
                    query,
                    false,
                    |uuid| uuid.clone(),
                    |service, pool, q| async move { service.run_query_sdna(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => Ok(QueryResolution::Matches(vec![])),
        }
    }

    /// Executes a Prolog query directly on the SDNA pool with user context
    /// This ensures the SDNA pool has the correct owner_did for SDNA fact filtering
    ///
    /// does not lock the prolog_update_mutex
    /// uses run_query_sdna
    pub async fn prolog_query_sdna_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple => {
                // In Simple mode, route to Simple engine (no per-context pools)
                // IMPORTANT: Use context user's DID as owner_did so their SDNA links are included
                let service = get_prolog_service().await;
                let (perspective_uuid, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, neighbourhood_author)
                };

                // Use context DID as owner_did for SDNA filtering
                let owner_did = Some(if let Some(user_email) = &context.user_email {
                    crate::agent::AgentService::get_user_did_by_email(user_email)?
                } else {
                    crate::agent::AgentService::with_global_instance(|service| {
                        service.did.clone().unwrap_or_default()
                    })
                });

                // Get links for SDNA fact generation
                let links = self
                    .get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::SdnaOnly => {
                // In SdnaOnly mode, route to Simple engine (no per-context pools), only SDNA links
                // IMPORTANT: Use context user's DID as owner_did so their SDNA links are included
                let service = get_prolog_service().await;
                let (perspective_uuid, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, neighbourhood_author)
                };

                // Use context DID as owner_did for SDNA filtering
                let owner_did = Some(if let Some(user_email) = &context.user_email {
                    crate::agent::AgentService::get_user_did_by_email(user_email)?
                } else {
                    crate::agent::AgentService::with_global_instance(|service| {
                        service.did.clone().unwrap_or_default()
                    })
                });

                // Get only SDNA-related links from database (efficient query)
                let links = self
                    .get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::Pooled => {
                // In pooled mode, use per-context SDNA pool
                let perspective_uuid = {
                    let persisted_guard = self.persisted.lock().await;
                    persisted_guard.uuid.clone()
                };

                // Ensure the user-specific pool exists
                self.ensure_prolog_engine_pool_for_context(context).await?;

                self.prolog_query_helper(
                    query,
                    false,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_sdna(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => Ok(QueryResolution::Matches(vec![])),
        }
    }

    /// Ensure prolog engine pool exists for the given context with correct owner_did
    pub async fn ensure_prolog_engine_pool_for_context(
        &self,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        let (perspective_uuid, neighbourhood_author) = {
            let persisted_guard = self.persisted.lock().await;
            let neighbourhood_author = persisted_guard
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone());
            (persisted_guard.uuid.clone(), neighbourhood_author)
        };

        let pool_id = self.get_pool_id_for_context(&perspective_uuid, context);
        let owner_did = if let Some(user_email) = &context.user_email {
            crate::agent::AgentService::get_user_did_by_email(user_email)?
        } else {
            crate::agent::AgentService::with_global_instance(|service| {
                service.did.clone().unwrap_or_default()
            })
        };

        // Ensure pool exists
        let service = get_prolog_service().await;
        service
            .ensure_perspective_pool(pool_id.clone(), None)
            .await?;

        // Initialize user pool with correct neighbourhood author for SDNA governance
        // This ensures users can see SDNA from both themselves and the neighbourhood creator
        let links = self
            .get_links(&crate::graphql::graphql_types::LinkQuery::default())
            .await?;

        service
            .update_perspective_links(
                pool_id,
                "facts".to_string(),  // module_name
                links,                // already DecoratedLinkExpression
                neighbourhood_author, // neighbourhood_author for SDNA governance
                Some(owner_did),      // owner_did for SDNA
            )
            .await?;

        Ok(())
    }

    /// Executes a Prolog query against the notification pool
    /// does not lock the prolog_update_mutex
    /// uses run_query_smart
    pub async fn prolog_query_notification(
        &self,
        query: String,
    ) -> Result<QueryResolution, AnyError> {
        self.prolog_query_helper(
            query,
            false,
            |uuid| notification_pool_name(uuid),
            |service, pool, q| async move { service.run_query_smart(pool, q).await },
        )
        .await
    }

    /// Executes a SurrealQL query against the perspective's SurrealDB cache
    /// Returns results as JSON values for easy handling
    ///
    /// # Errors
    /// Returns an error if the query fails to execute or contains invalid syntax.
    /// Callers should handle errors appropriately rather than silently ignoring them.
    /// SurrealDB has been removed. Returns an error directing users to SPARQL.
    pub async fn surreal_query(&self, _query: String) -> Result<Vec<serde_json::Value>, AnyError> {
        Err(anyhow!(
            "SurrealDB has been removed. Use SPARQL queries instead (perspectiveQuerySparql)."
        ))
    }

    /// Execute a SPARQL query against this perspective's Oxigraph store
    pub fn sparql_query(&self, query: String) -> Result<String, deno_core::anyhow::Error> {
        self.sparql_service.query(&query)
    }

    /// Execute a notification trigger query against the SPARQL store.
    /// Accepts both legacy SurrealQL queries (auto-converted) and native SPARQL queries.
    pub async fn surreal_query_notification(
        &self,
        query: String,
        _user_email: Option<String>,
    ) -> Result<Vec<serde_json::Value>, AnyError> {
        let sparql = if query.trim().to_uppercase().starts_with("SELECT")
            && query.to_uppercase().contains("FROM LINK")
        {
            // Legacy SurrealQL: parse simple SELECT ... FROM link WHERE ... patterns
            Self::surrealql_to_sparql(&query)?
        } else {
            // Assume it's already a SPARQL query
            query
        };

        let result_json = self.sparql_service.query(&sparql)?;
        let results: Vec<serde_json::Value> = serde_json::from_str(&result_json)?;
        Ok(results)
    }

    /// Convert a simple SurrealQL `SELECT ... FROM link WHERE ...` query to SPARQL.
    /// Supports conditions: field = 'value', field IN ['a', 'b'], ORDER BY, LIMIT.
    /// Also supports fn::contains(...) patterns used in mention queries.
    fn surrealql_to_sparql(surreal_query: &str) -> Result<String, AnyError> {
        // Extract WHERE clause
        let upper = surreal_query.to_uppercase();
        let where_clause = if let Some(pos) = upper.find("WHERE ") {
            let rest = &surreal_query[pos + 6..];
            // Strip trailing ORDER BY / LIMIT
            let end = rest
                .to_uppercase()
                .find(" ORDER BY")
                .or_else(|| rest.to_uppercase().find(" LIMIT"))
                .unwrap_or(rest.len());
            rest[..end].trim().to_string()
        } else {
            String::new()
        };

        // Extract LIMIT
        let limit = if let Some(pos) = upper.find("LIMIT ") {
            let rest = &surreal_query[pos + 6..];
            rest.trim()
                .split_whitespace()
                .next()
                .and_then(|n| n.parse::<usize>().ok())
        } else {
            None
        };

        let mut filters = Vec::new();

        if !where_clause.is_empty() {
            // Parse conditions separated by AND
            // Handle fn::contains patterns for mention queries
            Self::parse_surrealql_conditions(&where_clause, &mut filters);
        }

        let filter_str = if filters.is_empty() {
            String::new()
        } else {
            format!("\n  FILTER({})", filters.join(" && "))
        };

        let limit_str = if let Some(n) = limit {
            format!("\nLIMIT {}", n)
        } else {
            String::new()
        };

        Ok(format!(
            "SELECT ?source ?predicate ?target WHERE {{\n  ?source ?predicate ?target .\n  FILTER(isIRI(?source) && isIRI(?predicate)){}{}}}",
            filter_str, limit_str
        ))
    }

    /// Parse SurrealQL WHERE conditions into SPARQL FILTER expressions.
    fn parse_surrealql_conditions(where_clause: &str, filters: &mut Vec<String>) {
        // Handle OR groups (for mention queries with fn::contains)
        // Also handle simple field = 'value' AND chains
        // Split on AND (case-insensitive), but be careful with parenthesized OR groups
        let parts = Self::split_and_conditions(where_clause);

        for part in parts {
            let trimmed = part.trim();
            if trimmed.is_empty() {
                continue;
            }

            // fn::contains(...) pattern → SPARQL CONTAINS
            if trimmed.contains("fn::contains") {
                // This is a mention-style condition, possibly with OR
                // Convert the whole thing to SPARQL CONTAINS filters
                if let Some(filter) = Self::convert_contains_group(trimmed) {
                    filters.push(filter);
                }
            } else if let Some(filter) = Self::convert_simple_condition(trimmed) {
                filters.push(filter);
            }
        }
    }

    /// Split a WHERE clause on top-level AND keywords (respecting parentheses).
    fn split_and_conditions(clause: &str) -> Vec<String> {
        let mut parts = Vec::new();
        let mut depth = 0;
        let mut current = String::new();
        let chars: Vec<char> = clause.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            match chars[i] {
                '(' => {
                    depth += 1;
                    current.push('(');
                }
                ')' => {
                    depth -= 1;
                    current.push(')');
                }
                _ => {
                    // Check for " AND " at depth 0
                    if depth == 0 && i + 5 <= chars.len() {
                        let slice: String = chars[i..i + 5].iter().collect();
                        if slice.to_uppercase() == " AND " {
                            parts.push(current.trim().to_string());
                            current.clear();
                            i += 5;
                            continue;
                        }
                    }
                    current.push(chars[i]);
                }
            }
            i += 1;
        }
        if !current.trim().is_empty() {
            parts.push(current.trim().to_string());
        }
        parts
    }

    /// Convert a simple SurrealQL condition like `predicate = 'value'` or
    /// `predicate IN ['a', 'b']` to a SPARQL FILTER expression.
    fn convert_simple_condition(condition: &str) -> Option<String> {
        // field = 'value'
        if let Some(eq_pos) = condition.find('=') {
            let field = condition[..eq_pos].trim().to_lowercase();
            let value = condition[eq_pos + 1..]
                .trim()
                .trim_matches('\'')
                .trim_matches('"')
                .to_string();
            let var = match field.as_str() {
                "source" => "?source",
                "predicate" => "?predicate",
                "target" => "?target",
                _ => return None,
            };
            return Some(format!("STR({}) = \"{}\"", var, value));
        }

        // field IN ['a', 'b']
        let upper = condition.to_uppercase();
        if let Some(in_pos) = upper.find(" IN ") {
            let field = condition[..in_pos].trim().to_lowercase();
            let var = match field.as_str() {
                "source" => "?source",
                "predicate" => "?predicate",
                "target" => "?target",
                _ => return None,
            };
            // Extract values from [...]
            let list_str = condition[in_pos + 4..].trim();
            let inner = list_str.trim_start_matches('[').trim_end_matches(']');
            let values: Vec<&str> = inner
                .split(',')
                .map(|v| v.trim().trim_matches('\''))
                .collect();
            let conditions: Vec<String> = values
                .iter()
                .map(|v| format!("STR({}) = \"{}\"", var, v))
                .collect();
            return Some(format!("({})", conditions.join(" || ")));
        }

        None
    }

    /// Convert fn::contains based mention conditions to SPARQL CONTAINS filters.
    fn convert_contains_group(condition: &str) -> Option<String> {
        // Extract individual fn::contains calls
        let mut contains_filters = Vec::new();
        let mut search_start = 0;

        while let Some(pos) = condition[search_start..].find("fn::contains(") {
            let abs_pos = search_start + pos;
            // Find the matching closing paren
            let after = &condition[abs_pos + 13..]; // after "fn::contains("
            let mut depth = 1;
            let mut end = 0;
            for (i, ch) in after.char_indices() {
                match ch {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let args = &after[..end];
            // args is like: string::lowercase(<string> fn::parse_literal(target)), 'term'
            // We just need the search term and the field (target)
            if let Some(comma) = args.rfind(',') {
                let term = args[comma + 1..]
                    .trim()
                    .trim_matches('\'')
                    .trim_matches('"');
                // Determine the field - check if source/predicate/target appears
                let field_part = &args[..comma].to_lowercase();
                let var = if field_part.contains("target") {
                    "?target"
                } else if field_part.contains("source") {
                    "?source"
                } else if field_part.contains("predicate") {
                    "?predicate"
                } else {
                    "?target" // default
                };
                contains_filters.push(format!("CONTAINS(LCASE(STR({})), \"{}\")", var, term));
            }
            search_start = abs_pos + 13 + end + 1;
        }

        if contains_filters.is_empty() {
            None
        } else if contains_filters.len() == 1 {
            Some(contains_filters.into_iter().next().unwrap())
        } else {
            Some(format!("({})", contains_filters.join(" || ")))
        }
    }

    pub(crate) async fn persist_link_diff(
        &self,
        diff: &DecoratedPerspectiveDiff,
    ) -> Result<(), AnyError> {
        // IMPORTANT: Process removals BEFORE additions!
        // The remove_link function matches by source/predicate/target (not unique ID).
        // If we add first and remove second, we'd delete the newly added links too.

        // Removals first
        for removal in &diff.removals {
            if let Err(e) = self.sparql_service.remove_link(removal) {
                log::warn!("Failed to remove link from SPARQL store: {:?}", e);
            }
        }
        // Additions after
        for addition in &diff.additions {
            if let Err(e) = self.sparql_service.add_link(addition) {
                log::warn!("Failed to add link to SPARQL store: {:?}", e);
            }
        }

        Ok(())
    }

    fn spawn_prolog_facts_update(
        &self,
        diff: DecoratedPerspectiveDiff,
        completion_sender: Option<tokio::sync::oneshot::Sender<()>>,
    ) {
        let self_clone = self.clone();

        tokio::spawn(async move {
            // In Disabled, Simple, or SdnaOnly mode, just trigger subscription checks
            // (Pooled mode prolog updates don't apply - run_query_all only works in Pooled mode)
            if PROLOG_MODE == PrologMode::Disabled
                || PROLOG_MODE == PrologMode::Simple
                || PROLOG_MODE == PrologMode::SdnaOnly
            {
                // Trigger notification, prolog subscription
                *(self_clone.trigger_notification_check.lock().await) = true;
                *(self_clone.trigger_prolog_subscription_check.lock().await) = true;

                self_clone.pubsub_publish_diff(diff).await;

                if let Some(sender) = completion_sender {
                    let _ = sender.send(());
                }
                return;
            }

            // Pooled mode: original full update logic
            //let spawn_start = std::time::Instant::now();
            //log::info!("🔧 PROLOG UPDATE: Starting prolog facts update task - {} add, {} rem",
            //    diff.additions.len(), diff.removals.len());

            //let ensure_pool_start = std::time::Instant::now();
            if let Err(e) = self_clone.ensure_prolog_engine_pool().await {
                log::error!("Error spawning Prolog engine pool: {:?}", e);
                if let Some(sender) = completion_sender {
                    let _ = sender.send(());
                }
                return;
            }
            //log::info!("🔧 PROLOG UPDATE: Engine pool ensured in {:?}", ensure_pool_start.elapsed());

            // Get UUID before acquiring write lock
            //let uuid_start = std::time::Instant::now();
            let uuid = {
                let persisted_guard = self_clone.persisted.lock().await;
                persisted_guard.uuid.clone()
            };
            //log::info!("🔧 PROLOG UPDATE: UUID retrieved in {:?}", uuid_start.elapsed());

            //let analysis_start = std::time::Instant::now();
            let fact_rebuild_needed = !diff.removals.is_empty()
                || diff.additions.iter().any(|link| is_sdna_link(&link.data));
            //log::info!("🔧 PROLOG UPDATE: Analysis completed in {:?} - rebuild_needed: {}",
            //    analysis_start.elapsed(), fact_rebuild_needed);

            //let mutex_wait_start = std::time::Instant::now();
            //log::info!("🔧 PROLOG UPDATE: Waiting for prolog_update_mutex...");

            let did_update = if !fact_rebuild_needed {
                //log::info!("🔧 PROLOG UPDATE: Using FAST ASSERTION path");
                // For additions only, use assertions - acquire lock only during prolog operations
                //let assertions_start = std::time::Instant::now();
                let mut assertions: Vec<String> = Vec::new();
                for addition in &diff.additions {
                    assertions.push(generic_link_fact("assert_link_and_triple", addition));
                }
                //log::info!("🔧 PROLOG UPDATE: Built {} assertions in {:?}",
                //    assertions.len(), assertions_start.elapsed());

                //let service_start = std::time::Instant::now();
                let service = get_prolog_service().await;
                //log::info!("🔧 PROLOG UPDATE: Got prolog service in {:?}", service_start.elapsed());

                // Acquire write lock only for the prolog operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                //log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());

                let query_start = std::time::Instant::now();
                let query = format!("{}.", assertions.join(","));
                //log::info!("🔧 PROLOG UPDATE: Running assertion query: {} chars", query.len());

                let service_clone = service.clone();
                let uuid_clone = uuid.clone();
                let query_clone = query.clone();
                tokio::spawn(async move {
                    if let Err(e) = service_clone
                        .run_query_all(notification_pool_name(&uuid_clone), query_clone)
                        .await
                    {
                        log::error!(
                            "Failed to update notification pool for perspective {}: {:?}",
                            uuid_clone,
                            e
                        );
                    }
                });

                match service.run_query_all(uuid, query).await {
                    Ok(()) => {
                        //log::info!("🔧 PROLOG UPDATE: Assertion query completed successfully in {:?}", query_start.elapsed());
                        true
                    }
                    Err(e) => {
                        log::error!(
                            "Error while running assertion query to update Prolog engine facts (took {:?}): {:?}", 
                            query_start.elapsed(), e
                        );
                        false
                    }
                }
            } else {
                //log::info!("🔧 PROLOG UPDATE: Using FULL REBUILD path");
                // For fact rebuild, acquire write lock for the entire operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                //log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());

                let rebuild_start = std::time::Instant::now();
                match self_clone.update_prolog_engine_facts().await {
                    Ok(()) => {
                        log::trace!(
                            "🔧 PROLOG UPDATE: Full rebuild completed successfully in {:?}",
                            rebuild_start.elapsed()
                        );
                        true
                    }
                    Err(e) => {
                        log::error!(
                            "Error while updating Prolog engine facts (took {:?}): {:?}",
                            rebuild_start.elapsed(),
                            e
                        );
                        false
                    }
                }
            };

            if did_update {
                self_clone.pubsub_publish_diff(diff).await;

                // Trigger notification and subscription checks after prolog facts are updated
                *(self_clone.trigger_notification_check.lock().await) = true;
                *(self_clone.trigger_prolog_subscription_check.lock().await) = true;
            }

            //log::info!("🔧 PROLOG UPDATE: Total prolog update task took {:?}", spawn_start.elapsed());

            // Signal completion through the oneshot channel if provided
            if let Some(sender) = completion_sender {
                let _ = sender.send(());
            }
        });
    }

    fn all_notifications_for_perspective_id(uuid: String) -> Result<Vec<Notification>, AnyError> {
        Ok(Ad4mDb::with_global_instance(|db| db.get_notifications())?
            .into_iter()
            .filter(|n| n.perspective_ids.contains(&uuid))
            .collect())
    }

    async fn calc_notification_trigger_matches(
        &self,
    ) -> Result<BTreeMap<Notification, Vec<serde_json::Value>>, AnyError> {
        // Get UUID without holding lock during operations
        let uuid = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.uuid.clone()
        };

        let notifications = Self::all_notifications_for_perspective_id(uuid.clone())?;
        //log::info!("🔔 NOTIFICATIONS: Found {} notifications for perspective {}", notifications.len(), uuid);

        //log::info!("🔔 NOTIFICATIONS: All triggers:\n{}", notifications.iter()
        //    .map(|n| n.trigger.clone())
        //    .collect::<Vec<String>>()
        //    .join("\n"));
        let mut result_map = BTreeMap::new();
        // Cache key must include both trigger and user_email for deduplication
        let mut trigger_cache: HashMap<(String, Option<String>), Vec<serde_json::Value>> =
            HashMap::new();

        for n in notifications {
            //log::info!("🔔 NOTIFICATIONS: Processing notification for perspective {}: {}", uuid, n.trigger);
            let cache_key = (n.trigger.clone(), n.user_email.clone());
            if let Some(cached_matches) = trigger_cache.get(&cache_key) {
                //log::info!("🔔 NOTIFICATIONS: Using cached matches for notification for perspective {}: {}", uuid, n.trigger);
                result_map.insert(n.clone(), cached_matches.clone());
            } else {
                //let query_start = std::time::Instant::now();
                //log::info!("🔔 NOTIFICATIONS: not cached - Querying notification for perspective {}", uuid);
                // Handle errors per-notification to prevent one user's DID failure from
                // silencing all notifications. This can happen with orphaned notifications
                // from deleted users or corrupted data.
                match self
                    .surreal_query_notification(n.trigger.clone(), n.user_email.clone())
                    .await
                {
                    Ok(matches) => {
                        trigger_cache.insert(cache_key, matches.clone());
                        result_map.insert(n.clone(), matches);
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to query notification for user {:?} in perspective {}: {:?}. Skipping this notification.",
                            n.user_email,
                            uuid,
                            e
                        );
                        // Skip this notification but continue processing others
                    }
                }
                //log::info!("🔔 NOTIFICATIONS: Querying notification: {} - took {:?}", n.trigger, query_start.elapsed());
            }
        }

        Ok(result_map)
    }

    async fn notification_trigger_snapshot(
        &self,
    ) -> BTreeMap<Notification, Vec<serde_json::Value>> {
        self.calc_notification_trigger_matches()
            .await
            .unwrap_or_else(|e| {
                log::error!("Error trying to render notification matches: {:?}", e);
                BTreeMap::new()
            })
    }

    fn subtract_before_notification_matches(
        before: &BTreeMap<Notification, Vec<serde_json::Value>>,
        after: &BTreeMap<Notification, Vec<serde_json::Value>>,
    ) -> BTreeMap<Notification, Vec<serde_json::Value>> {
        after
            .iter()
            .filter_map(|(notification, after_matches)| {
                let new_matches: Vec<serde_json::Value> =
                    if let Some(before_matches) = before.get(notification) {
                        // Find matches that exist in "after" but not in "before"
                        after_matches
                            .iter()
                            .filter(|after_match| {
                                !before_matches
                                    .iter()
                                    .any(|before_match| before_match == *after_match)
                            })
                            .cloned()
                            .collect()
                    } else {
                        // No previous matches, so all current matches are new
                        after_matches.clone()
                    };

                if new_matches.is_empty() {
                    None
                } else {
                    Some((notification.clone(), new_matches))
                }
            })
            .collect()
    }

    async fn publish_notification_matches(
        uuid: String,
        match_map: BTreeMap<Notification, Vec<serde_json::Value>>,
    ) {
        for (notification, matches) in match_map {
            if !matches.is_empty() {
                // Convert matches to JSON string
                let trigger_match =
                    serde_json::to_string(&matches).unwrap_or_else(|_| "[]".to_string());

                let payload = TriggeredNotification {
                    notification: notification.clone(),
                    perspective_id: uuid.clone(),
                    trigger_match,
                };

                let message = serde_json::to_string(&payload).unwrap();

                get_global_pubsub()
                    .await
                    .publish(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC, &message)
                    .await;

                if url::Url::parse(&notification.webhook_url).is_ok() {
                    log::info!(
                        "Notification webhook - posting to {:?}",
                        notification.webhook_url
                    );
                    let client = reqwest::Client::new();
                    let res = client
                        .post(&notification.webhook_url)
                        .bearer_auth(&notification.webhook_auth)
                        .header("Content-Type", "application/json")
                        .body(message.clone())
                        .send()
                        .await;
                    log::info!("Notification webhook response: {:?}", res);
                }
            }
        }
    }

    async fn update_prolog_engine_facts(&self) -> Result<(), AnyError> {
        // Get all required data before making service calls
        let (uuid, owner_did) = {
            let persisted_guard = self.persisted.lock().await;
            (
                persisted_guard.uuid.clone(),
                persisted_guard.get_primary_owner(),
            )
        };

        let all_links = self.get_links(&LinkQuery::default()).await?;

        let neighbourhood_author = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone())
        };

        let service = get_prolog_service().await;
        service
            .update_perspective_links(
                uuid.clone(),
                "facts".to_string(),
                all_links.clone(),
                neighbourhood_author.clone(),
                owner_did.clone(),
            )
            .await?;
        let service_clone = service.clone();
        tokio::spawn(async move {
            let _ = service_clone
                .update_perspective_links(
                    notification_pool_name(&uuid),
                    "facts".to_string(),
                    all_links,
                    neighbourhood_author,
                    owner_did,
                )
                .await;
        });
        Ok(())
    }

    async fn no_link_language_error(&self) -> AnyError {
        let (uuid, state) = {
            let handle = self.persisted.lock().await;
            (handle.uuid.clone(), handle.state.clone())
        };
        anyhow!(
            "Perspective {} has no link language installed. State is: {:?}",
            uuid,
            state
        )
    }

    pub async fn others(&self) -> Result<Vec<String>, AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        let mut all_others = if let Some(mut link_language) = link_language_clone {
            link_language.others().await?
        } else {
            return Err(self.no_link_language_error().await);
        };

        // Add all perspective owners (which includes local managed users)
        let handle = self.persisted.lock().await.clone();
        if let Some(owners) = &handle.owners {
            log::debug!("🔍 others() - Perspective owners: {:?}", owners);

            for owner_did in owners {
                if !all_others.contains(owner_did) {
                    log::debug!("✅ others() - Adding owner to others list: {}", owner_did);
                    all_others.push(owner_did.clone());
                }
            }
        }

        log::debug!("🔍 others() - Final others list: {:?}", all_others);
        Ok(all_others)
    }

    pub async fn has_telepresence_adapter(&self) -> bool {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            match link_language.has_telepresence_adapter().await {
                Ok(result) => result,
                Err(e) => {
                    log::error!("Error calling has_telepresence_adapter: {:?}", e);
                    false
                }
            }
        } else {
            false
        }
    }

    pub async fn online_agents(&self) -> Result<Vec<OnlineAgent>, AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            Ok(link_language
                .get_online_agents()
                .await?
                .into_iter()
                .map(|mut a| {
                    a.status.verify_signatures();
                    a
                })
                .collect())
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn set_online_status(&self, status: PerspectiveExpression) -> Result<(), AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            link_language.set_online_status(status).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn send_signal(
        &self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        // Check if the recipient is a locally managed user
        use crate::agent::AgentService;

        log::debug!(
            "🔔 SEND SIGNAL: Sending signal to remote agent {}",
            remote_agent_did
        );

        let current_perspective_handle = self.persisted.lock().await.clone();

        // Check if this perspective is part of a neighbourhood
        if current_perspective_handle.shared_url.is_some() {
            // Helper closure: publish a signal locally and return Ok(())
            let publish_local = |handle: PerspectiveHandle,
                                 mut signal: PerspectiveExpression,
                                 recipient: String| async move {
                signal.verify_signatures();
                get_global_pubsub()
                    .await
                    .publish(
                        &NEIGHBOURHOOD_SIGNAL_TOPIC,
                        &serde_json::to_string(&NeighbourhoodSignalFilter {
                            perspective: handle,
                            signal,
                            recipient: Some(recipient),
                        })
                        .unwrap(),
                    )
                    .await;
            };

            // Check if any managed email user is the recipient
            if let Ok(user_emails) = AgentService::list_user_emails() {
                for user_email in user_emails {
                    if let Ok(user_did) = AgentService::get_user_did_by_email(&user_email) {
                        if user_did == remote_agent_did {
                            if let Some(owners) = &current_perspective_handle.owners {
                                if owners.contains(&remote_agent_did) {
                                    log::debug!(
                                        "Routing signal locally to managed user {} in neighbourhood {:?}",
                                        user_email,
                                        current_perspective_handle.shared_url
                                    );
                                    let handle = self.persisted.lock().await.clone();
                                    publish_local(handle, payload, remote_agent_did).await;
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }

            // Check if the main agent is the recipient.
            // Treat owners=None or owners=[] as implicit main-agent ownership (legacy perspectives).
            let main_agent_did = AgentService::with_global_instance(|s| s.did.clone());
            if let Some(main_agent_did) = main_agent_did {
                if main_agent_did == remote_agent_did {
                    let is_owner = current_perspective_handle
                        .owners
                        .as_ref()
                        .map_or(true, |o| o.is_empty() || o.contains(&remote_agent_did));
                    if is_owner {
                        log::debug!(
                            "Routing signal locally to main agent in neighbourhood {:?}",
                            current_perspective_handle.shared_url
                        );
                        let handle = self.persisted.lock().await.clone();
                        publish_local(handle, payload, remote_agent_did).await;
                        return Ok(());
                    }
                }
            }
        }

        log::debug!(
            "🔔 SEND SIGNAL: Not a local user in this neighbourhood, sending through link language"
        );

        // If not a local user in this neighbourhood, send through link language
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            log::debug!("🔔 SEND SIGNAL: Sending signal through link language");
            link_language.send_signal(remote_agent_did, payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn send_broadcast(
        &self,
        payload: PerspectiveExpression,
        loopback: bool,
    ) -> Result<(), AnyError> {
        use crate::agent::AgentService;

        let current_perspective_handle = self.persisted.lock().await.clone();

        if loopback {
            // send back to all clients through neighbourhood signal subscription
            let payload_clone = payload.clone();
            let self_clone = self.clone();
            tokio::spawn(async move {
                self_clone
                    .telepresence_signal_from_link_language(payload_clone, None)
                    .await;
            });
        }

        // Route signals to all local agents (managed users + main agent) who are owners
        if current_perspective_handle.shared_url.is_some() {
            // Send to each local managed email user who is an explicit owner
            if let Some(owners) = &current_perspective_handle.owners {
                if let Ok(user_emails) = AgentService::list_user_emails() {
                    for user_email in user_emails {
                        if let Ok(user_did) = AgentService::get_user_did_by_email(&user_email) {
                            if owners.contains(&user_did) {
                                let handle = self.persisted.lock().await.clone();
                                let mut signal = payload.clone();
                                signal.verify_signatures();

                                get_global_pubsub()
                                    .await
                                    .publish(
                                        &NEIGHBOURHOOD_SIGNAL_TOPIC,
                                        &serde_json::to_string(&NeighbourhoodSignalFilter {
                                            perspective: handle,
                                            signal,
                                            recipient: Some(user_did),
                                        })
                                        .unwrap(),
                                    )
                                    .await;
                            }
                        }
                    }
                }
            }

            // Send to the main agent if it is an owner.
            // The main agent is not in list_user_emails(), so it must be handled separately.
            // Treat owners=None or owners=[] as implicit main-agent ownership (legacy perspectives).
            let main_agent_did = AgentService::with_global_instance(|s| s.did.clone());
            if let Some(main_agent_did) = main_agent_did {
                let is_owner = current_perspective_handle
                    .owners
                    .as_ref()
                    .map_or(true, |o| o.is_empty() || o.contains(&main_agent_did));
                // Don't echo the broadcast back to the sender (loopback is handled separately).
                let is_sender = payload.author == main_agent_did;
                if is_owner && !is_sender {
                    let handle = self.persisted.lock().await.clone();
                    let mut signal = payload.clone();
                    signal.verify_signatures();

                    log::debug!(
                        "Broadcasting signal locally to main agent in neighbourhood {:?}",
                        current_perspective_handle.shared_url
                    );

                    get_global_pubsub()
                        .await
                        .publish(
                            &NEIGHBOURHOOD_SIGNAL_TOPIC,
                            &serde_json::to_string(&NeighbourhoodSignalFilter {
                                perspective: handle,
                                signal,
                                recipient: Some(main_agent_did),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }
        }

        // Also send through link language for remote users
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            link_language.send_broadcast(payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn update_local_agents(&self, agents: Vec<String>) {
        log::debug!("Updating local agents for perspective: {:?}", agents);
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            if let Err(e) = link_language.set_local_agents(agents).await {
                log::error!("Failed to update local agents on link language: {:?}", e);
            }
        } else {
            log::warn!("Cannot update local agents: link language not initialized");
        }
    }

    pub async fn execute_commands(
        &mut self,
        commands: Vec<Command>,
        expression: String,
        parameters: Vec<Parameter>,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        //let execute_start = std::time::Instant::now();
        //log::info!("⚙️ EXECUTE COMMANDS: Starting execution of {} commands for expression '{}', batch_id: {:?}",
        //    commands.len(), expression, batch_id);

        let jsvalue_to_string = |value: &Value| -> String {
            match value {
                serde_json::Value::String(s) => s.clone(),
                _ => value.to_string(),
            }
        };

        let replace_this = |input: Option<String>| -> Option<String> {
            if Some(String::from("this")) == input {
                Some(expression.clone())
            } else {
                input
            }
        };

        let replace_parameters = |input: Option<String>| -> Option<String> {
            if let Some(mut output) = input {
                for parameter in &parameters {
                    output = output.replace(&parameter.name, &jsvalue_to_string(&parameter.value));
                }
                Some(output)
            } else {
                input
            }
        };

        for command in commands.iter() {
            //let command_start = std::time::Instant::now();
            //log::info!("⚙️ EXECUTE COMMANDS: Processing command {}/{}: {:?}", i + 1, commands.len(), command.action);

            let source = replace_this(replace_parameters(command.source.clone()))
                .ok_or_else(|| anyhow!("Source cannot be None"))?;
            let predicate = replace_this(replace_parameters(command.predicate.clone()));
            let target = (replace_parameters(command.target.clone()))
                .ok_or_else(|| anyhow!("Target cannot be None"))?;
            let local = command.local.unwrap_or(false);
            let status = if local {
                LinkStatus::Local
            } else {
                LinkStatus::Shared
            };

            match command.action {
                Action::AddLink => {
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
                Action::RemoveLink => {
                    let remove_source = source.clone();
                    let remove_predicate = predicate.clone();
                    let remove_target = if target == "*" { None } else { Some(target) };
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(remove_source.clone()),
                            predicate: remove_predicate.clone(),
                            target: remove_target.clone(),
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                let source_match = link_expr.data.source == remove_source;
                                let pred_match = remove_predicate.is_none()
                                    || link_expr.data.predicate == remove_predicate;
                                let target_match = remove_target.is_none()
                                    || link_expr.data.target
                                        == remove_target.as_deref().unwrap_or("");
                                !(source_match && pred_match && target_match)
                            });
                        }
                    }
                }
                Action::SetSingleTarget => {
                    if predicate.is_none() {
                        log::error!(
                            "SetSingleTarget actions with no predicate are not allowed. Skipping."
                        );
                        continue;
                    }
                    // Remove matching persisted links
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source.clone()),
                            predicate: predicate.clone(),
                            target: None,
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions so
                    // that a previous add in the same batch doesn't survive and
                    // create a duplicate (e.g. save+update in one transaction).
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                !(link_expr.data.source == source
                                    && link_expr.data.predicate == predicate)
                            });
                        }
                    }
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
                Action::CollectionSetter => {
                    // Remove matching persisted links
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source.clone()),
                            predicate: predicate.clone(),
                            target: None,
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                !(link_expr.data.source == source
                                    && link_expr.data.predicate == predicate)
                            });
                        }
                    }
                    self.add_links(
                        parameters
                            .iter()
                            .map(|p| Link {
                                source: source.clone(),
                                predicate: predicate.clone(),
                                target: jsvalue_to_string(&p.value),
                            })
                            .collect(),
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
            }

            //log::info!("⚙️ EXECUTE COMMANDS: Command {} completed in {:?}", i + 1, command_start.elapsed());
        }

        //log::info!("⚙️ EXECUTE COMMANDS: All {} commands executed in {:?}", commands.len(), execute_start.elapsed());
        Ok(())
    }

    async fn subject_class_option_to_class_name(
        &mut self,
        subject_class: SubjectClassOption,
        _context: &AgentContext,
    ) -> Result<String, AnyError> {
        //let method_start = std::time::Instant::now();
        //log::info!("🔍 SUBJECT CLASS: Starting class name resolution...");

        Ok(if let Some(class_name) = subject_class.class_name {
            class_name
        } else {
            return Err(anyhow!(
                "SubjectClassOption requires `className` to be set. Query-based lookup has been removed; resolve the class name client-side."
            ));
        })
    }

    /// Parse actions JSON from a literal target (format: "literal://string:{json}")
    fn parse_actions_from_literal(target: &str) -> Result<Vec<Command>, AnyError> {
        let prefix = "literal://string:";
        if !target.starts_with(prefix) {
            return Err(anyhow!("Invalid literal format: {}", target));
        }
        let json_str = &target[prefix.len()..];
        // Decode URL-encoded characters if present
        let decoded = urlencoding::decode(json_str)
            .map(|s| s.to_string())
            .unwrap_or_else(|_| json_str.to_string());
        serde_json::from_str(&decoded)
            .map_err(|e| anyhow!("Failed to parse actions JSON: {} - input: {}", e, decoded))
    }

    /// Get actions from SHACL links for a shape-level predicate (constructor/destructor)
    async fn get_shape_actions_from_shacl(
        &self,
        class_name: &str,
        predicate: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        // Query SurrealDB for links with the given predicate whose source ends with {ClassName}Shape
        let shape_suffix = format!("{}Shape", class_name);
        let _uuid = self.persisted.lock().await.uuid.clone();

        let links = self
            .sparql_service
            .get_links_by_predicate_and_source_suffix(predicate, &shape_suffix)?;

        // Return the first match
        if let Some(link) = links.first() {
            return Self::parse_actions_from_literal(&link.data.target).map(Some);
        }

        Ok(None)
    }

    /// Get actions from SHACL links for a property-level predicate (setter/adder/remover)
    async fn get_property_actions_from_shacl(
        &self,
        class_name: &str,
        property: &str,
        predicate: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        // Property shape URI format: {namespace}{ClassName}.{propertyName}
        let prop_suffix = format!("{}.{}", class_name, property);
        let _uuid = self.persisted.lock().await.uuid.clone();

        let links = self
            .sparql_service
            .get_links_by_predicate_and_source_suffix(predicate, &prop_suffix)?;

        // Return the first match
        if let Some(link) = links.first() {
            return Self::parse_actions_from_literal(&link.data.target).map(Some);
        }

        Ok(None)
    }

    /// Get resolve language from SHACL links
    pub async fn get_resolve_language_from_shacl(
        &self,
        class_name: &str,
        property: &str,
    ) -> Result<Option<String>, AnyError> {
        let prop_suffix = format!("{}.{}", class_name, property);
        let _uuid = self.persisted.lock().await.uuid.clone();

        let links = self
            .sparql_service
            .get_links_by_predicate_and_source_suffix("ad4m://resolveLanguage", &prop_suffix)?;

        if let Some(link) = links.first() {
            // Extract value from literal://string:{value}
            let prefix = "literal://string:";
            if link.data.target.starts_with(prefix) {
                let encoded_value = &link.data.target[prefix.len()..];
                let decoded = urlencoding::decode(encoded_value)
                    .map_err(|e| anyhow!("Failed to decode resolve language value: {}", e))?;
                return Ok(Some(decoded.to_string()));
            }
        }

        Ok(None)
    }

    async fn get_constructor_actions(&self, class_name: &str) -> Result<Vec<Command>, AnyError> {
        self.get_shape_actions_from_shacl(class_name, "ad4m://constructor")
            .await?
            .ok_or(anyhow!(
                "No SHACL constructor found for class: {}. Ensure the class has SHACL definitions.",
                class_name
            ))
    }

    async fn get_property_setter_actions(
        &self,
        class_name: &str,
        property: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        self.get_property_actions_from_shacl(class_name, property, "ad4m://setter")
            .await
    }

    pub async fn resolve_property_value(
        &self,
        class_name: &str,
        property: &str,
        value: &serde_json::Value,
        context: &AgentContext,
    ) -> Result<String, AnyError> {
        // Get resolve language from SHACL links
        let resolve_language = self
            .get_resolve_language_from_shacl(class_name, property)
            .await?;

        if let Some(resolve_language) = resolve_language {
            // Create an expression for the value
            let controller = crate::languages::LanguageController::global_instance();
            let agent_context = context.clone();
            match controller
                .expression_create(&resolve_language, value.clone(), &agent_context)
                .await
            {
                Ok(url) => Ok(url),
                Err(e) => {
                    log::warn!("Failed to create expression on {}: {}", resolve_language, e);
                    Ok(value.to_string())
                }
            }
        } else {
            let uri = match value {
                serde_json::Value::String(s) => {
                    // If the value is already a valid URI (has a scheme), use it directly.
                    // Otherwise wrap it in a literal:// URI so link targets are always valid URIs.
                    static URI_SCHEME_RE: std::sync::OnceLock<regex::Regex> =
                        std::sync::OnceLock::new();
                    let re = URI_SCHEME_RE
                        .get_or_init(|| regex::Regex::new(r"^[a-zA-Z][a-zA-Z0-9+\-._]*:").unwrap());
                    if re.is_match(s) {
                        s.clone()
                    } else {
                        Literal::from_string(s.clone())
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode string as literal URI: {}", e))?
                    }
                }
                serde_json::Value::Number(n) => {
                    if let Some(f) = n.as_f64() {
                        Literal::from_number(f)
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode number as literal URI: {}", e))?
                    } else {
                        Literal::from_string(value.to_string())
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode number as literal URI: {}", e))?
                    }
                }
                _ => value.to_string(),
            };
            Ok(uri)
        }
    }

    pub async fn create_subject(
        &mut self,
        subject_class: SubjectClassOption,
        expression_address: String,
        initial_values: Option<serde_json::Value>,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        //let create_start = std::time::Instant::now();
        //log::info!("🎯 CREATE SUBJECT: Starting create_subject for expression '{}' - batch_id: {:?}",
        //    expression_address, batch_id);

        //let class_name_start = std::time::Instant::now();
        let class_name = self
            .subject_class_option_to_class_name(subject_class, context)
            .await?;
        //log::info!("🎯 CREATE SUBJECT: Got class name '{}' in {:?}", class_name, class_name_start.elapsed());

        //let constructor_start = std::time::Instant::now();
        let mut commands = self.get_constructor_actions(&class_name).await?;
        //log::info!("🎯 CREATE SUBJECT: Got {} constructor actions in {:?}",
        //    commands.len(), constructor_start.elapsed());

        // Handle initial values if provided
        if let Some(obj) = initial_values {
            //log::info!("🎯 CREATE SUBJECT: Processing initial values...");

            if let serde_json::Value::Object(obj) = obj {
                for (prop, value) in obj.iter() {
                    //let prop_start = std::time::Instant::now();
                    if let Some(setter_commands) =
                        self.get_property_setter_actions(&class_name, prop).await?
                    {
                        let target_value = self
                            .resolve_property_value(&class_name, prop, value, context)
                            .await?;

                        //log::info!("🎯 CREATE SUBJECT: Property '{}' setter resolved in {:?}",
                        //    prop, prop_start.elapsed());

                        // Compare predicates between setter and constructor commands
                        for setter_cmd in setter_commands.iter() {
                            let mut overwritten = false;
                            if let Some(setter_pred) = &setter_cmd.predicate {
                                for cmd in commands.iter_mut() {
                                    if let Some(pred) = &cmd.predicate {
                                        if pred == setter_pred {
                                            cmd.target = Some(target_value.clone());
                                            overwritten = true;
                                            break;
                                        }
                                    }
                                }
                            }
                            if !overwritten {
                                commands.push(Command {
                                    target: Some(target_value.clone()),
                                    ..setter_cmd.clone()
                                });
                            }
                        }
                    }
                }
            }
        }

        //let execute_start = std::time::Instant::now();
        //log::info!("🎯 CREATE SUBJECT: Executing {} commands...", commands.len());
        // Execute the merged commands
        self.execute_commands(
            commands,
            expression_address.clone(),
            vec![],
            batch_id.clone(),
            context,
        )
        .await?;

        //log::info!("🎯 CREATE SUBJECT: Commands executed in {:?}", execute_start.elapsed());
        //log::info!("🎯 CREATE SUBJECT: Total create_subject took {:?}", create_start.elapsed());

        Ok(())
    }

    pub async fn get_subject_data(
        &mut self,
        subject_class: SubjectClassOption,
        base_expression: String,
        context: &AgentContext,
    ) -> Result<String, AnyError> {
        let mut object: HashMap<String, String> = HashMap::new();

        // Get author and timestamp from the first link mentioning base as source
        let base_query = LinkQuery {
            source: Some(base_expression.clone()),
            ..Default::default()
        };
        let base_links = self.get_links(&base_query).await?;
        let first_link = base_links
            .first()
            .ok_or_else(|| anyhow!("No links found for base expression: {}", base_expression))?;
        object.insert(
            String::from("author"),
            format!("\"{}\"", first_link.author.clone()),
        );
        object.insert(
            String::from("timestamp"),
            format!("\"{}\"", first_link.timestamp.clone()),
        );

        let class_name = self
            .subject_class_option_to_class_name(subject_class, context)
            .await?;
        let result = self
            .prolog_query_with_context(
                format!(
                    "subject_class(\"{}\", C), instance(C, \"{}\").",
                    class_name, base_expression
                ),
                context,
            )
            .await?;

        if let QueryResolution::False = result {
            log::error!(
                "No instance found for class: {} with id: {}",
                class_name,
                base_expression
            );
            return Err(anyhow!(
                "No instance found for class: {} with id: {}",
                class_name,
                base_expression
            ));
        }

        let properties_result = self
            .prolog_query_with_context(
                format!(
                    r#"subject_class("{}", C), property(C, Property)."#,
                    class_name
                ),
                context,
            )
            .await?;
        let properties: Vec<String> =
            prolog_get_all_string_bindings(&properties_result, "Property");

        for p in &properties {
            let property_values_result = self
                .prolog_query_with_context(
                    format!(
                        r#"subject_class("{}", C), property_getter(C, "{}", "{}", Value)"#,
                        class_name, base_expression, p
                    ),
                    context,
                )
                .await?;
            if let Some(property_value) = prolog_get_first_binding(&property_values_result, "Value")
            {
                let result = self
                    .prolog_query_with_context(
                        format!(
                            r#"subject_class("{}", C), property_resolve(C, "{}")"#,
                            class_name, p
                        ),
                        context,
                    )
                    .await?;
                //println!("resolve query result for {}: {:?}", p, result);
                let resolve_expression_uri = QueryResolution::False != result;
                //println!("resolve_expression_uri for {}: {:?}", p, resolve_expression_uri);
                let value = if resolve_expression_uri {
                    match &property_value {
                        scryer_prolog::Term::String(s) => {
                            let controller =
                                crate::languages::LanguageController::global_instance();
                            if let Ok((lang_address, expression_address)) =
                                crate::languages::LanguageController::parse_expr_url(s)
                            {
                                match controller
                                    .get_expression(&lang_address, &expression_address)
                                    .await
                                {
                                    Ok(Some(expr_json)) => {
                                        let rendered = crate::graphql::query_resolvers::build_expression_rendered(&expr_json, &lang_address);
                                        rendered.data
                                    }
                                    _ => prolog_value_to_json_string(property_value.clone()),
                                }
                            } else {
                                prolog_value_to_json_string(property_value.clone())
                            }
                        }
                        _x => prolog_value_to_json_string(property_value.clone()),
                    }
                } else {
                    prolog_value_to_json_string(property_value.clone())
                };
                object.insert(p.clone(), value);
            } else {
                //log::error!("Couldn't get a property value for class: `{}`, property: `{}`, base: `{}`\nProlog query result was: {:?}", class_name, p, base_expression, property_values_result);
                object.insert(p.clone(), "null".to_string());
            };
        }

        let collections_results = self
            .prolog_query_with_context(
                format!(
                    r#"subject_class("{}", C), collection(C, Collection)"#,
                    class_name
                ),
                context,
            )
            .await?;
        let collections: Vec<String> =
            prolog_get_all_string_bindings(&collections_results, "Collection");

        for c in collections {
            let collection_values_result = self
                .prolog_query_with_context(
                    format!(
                        r#"subject_class("{}", C), collection_getter(C, "{}", "{}", Value)"#,
                        class_name, base_expression, c
                    ),
                    context,
                )
                .await?;
            if let Some(collection_value) =
                prolog_get_first_binding(&collection_values_result, "Value")
            {
                object.insert(c.clone(), prolog_value_to_json_string(collection_value));
            } else {
                //log::error!("Couldn't get a collection value for class: `{}`, collection: `{}`, base: `{}`\nProlog query result was: {:?}", class_name, c, base_expression, collection_values_result);
                object.insert(c.clone(), "[]".to_string());
            }
        }

        let stringified = object
            .into_iter()
            .map(|(k, v)| format!(r#""{}": {}"#, k, v))
            .collect::<Vec<String>>()
            .join(", ");

        Ok(format!("{{ {} }}", stringified))
    }

    async fn send_subscription_update(
        &self,
        subscription_id: String,
        result: String,
        delay: Option<Duration>,
    ) {
        let uuid = self.persisted.lock().await.uuid.clone();
        tokio::spawn(async move {
            if let Some(delay) = delay {
                sleep(delay).await;
            }
            let filter = PerspectiveQuerySubscriptionFilter {
                uuid,
                subscription_id,
                result,
            };
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
                    &serde_json::to_string(&filter).unwrap(),
                )
                .await;
        });
    }

    pub async fn subscribe_and_query(
        &self,
        query: String,
        user_email: Option<String>,
    ) -> Result<(String, String), AnyError> {
        // Check if we already have a subscription with the same query and user
        let existing_subscription = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .find(|(_, q)| q.query == query && q.user_email == user_email)
                .map(|(id, _)| id.clone())
        };

        // Return existing subscription if found
        if let Some(existing_id) = existing_subscription {
            let existing_result = {
                let queries = self.subscribed_queries.lock().await;
                queries.get(&existing_id).map(|q| q.last_result.clone())
            };

            if let Some(last_result) = existing_result {
                let result_string = format!("#init#{}", last_result);
                for delay in [100, 500, 1000, 10000, 15000, 20000, 25000] {
                    self.send_subscription_update(
                        existing_id.clone(),
                        result_string.clone(),
                        Some(Duration::from_millis(delay)),
                    )
                    .await;
                }
                return Ok((existing_id, last_result));
            }
        }

        let subscription_id = uuid::Uuid::new_v4().to_string();

        // Execute prolog query with user context
        let agent_context = if let Some(email) = user_email.as_ref() {
            crate::agent::AgentContext::for_user_email(email.clone())
        } else {
            crate::agent::AgentContext::main_agent()
        };
        let initial_result = self
            .prolog_query_subscription_with_context(query.clone(), &agent_context)
            .await?;
        let result_string = prolog_resolution_to_string(initial_result);

        let subscribed_query = SubscribedQuery {
            query,
            last_result: result_string.clone(),
            last_keepalive: Instant::now(),
            user_email,
        };

        // Now insert the subscription
        self.subscribed_queries
            .lock()
            .await
            .insert(subscription_id.clone(), subscribed_query);

        // Send initial result after 3 delays
        let init_string = format!("#init#{}", result_string);
        for delay in [100, 500, 1000, 10000, 15000, 20000, 25000] {
            self.send_subscription_update(
                subscription_id.clone(),
                init_string.clone(),
                Some(Duration::from_millis(delay)),
            )
            .await;
        }

        Ok((subscription_id, result_string))
    }

    pub async fn keepalive_query(&self, subscription_id: String) -> Result<(), AnyError> {
        let mut queries = self.subscribed_queries.lock().await;
        if let Some(query) = queries.get_mut(&subscription_id) {
            query.last_keepalive = Instant::now();
            Ok(())
        } else {
            Err(anyhow!("Subscription not found"))
        }
    }

    pub async fn dispose_query_subscription(
        &self,
        subscription_id: String,
    ) -> Result<bool, AnyError> {
        let removed_query = {
            let mut queries = self.subscribed_queries.lock().await;
            queries.remove(&subscription_id)
        };

        if let Some(query) = removed_query {
            // Notify prolog service that subscription ended
            let uuid = self.persisted.lock().await.uuid.clone();
            if let Err(e) = get_prolog_service()
                .await
                .subscription_ended(uuid, query.query)
                .await
            {
                log::warn!("Failed to notify prolog service of subscription end: {}", e);
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    async fn check_subscribed_queries(&self) {
        let mut queries_to_remove = Vec::new();
        let mut query_futures = Vec::new();
        let now = Instant::now();

        // Collect only the minimal data needed: ID, query string, user_email, and keepalive time
        // DON'T clone the potentially huge last_result string
        let queries = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .map(|(id, query)| {
                    (
                        id.clone(),
                        query.query.clone(),
                        query.user_email.clone(),
                        query.last_keepalive,
                    )
                })
                .collect::<Vec<_>>()
        };

        // Create futures for each query check
        for (id, query_string, user_email, last_keepalive) in queries {
            // Check for timeout
            if now.duration_since(last_keepalive).as_secs() > QUERY_SUBSCRIPTION_TIMEOUT {
                queries_to_remove.push(id);
                continue;
            }

            // Spawn query check future
            let self_clone = self.clone();
            let query_future = async move {
                //let this_now = Instant::now();
                let agent_context = if let Some(email) = user_email {
                    crate::agent::AgentContext::for_user_email(email)
                } else {
                    crate::agent::AgentContext::main_agent()
                };
                if let Ok(result) = self_clone
                    .prolog_query_subscription_with_context(query_string, &agent_context)
                    .await
                {
                    let result_string = prolog_resolution_to_string(result);
                    // Compare with stored last_result only now, avoiding the clone earlier
                    let mut queries = self_clone.subscribed_queries.lock().await;
                    if let Some(stored_query) = queries.get_mut(&id) {
                        if result_string != stored_query.last_result {
                            //log::info!("Query {} has changed: {}", id, result_string);
                            // Release lock before sending update
                            drop(queries);
                            self_clone
                                .send_subscription_update(id.clone(), result_string.clone(), None)
                                .await;
                            // Re-acquire lock to update the result
                            let mut queries = self_clone.subscribed_queries.lock().await;
                            if let Some(stored_query) = queries.get_mut(&id) {
                                stored_query.last_result = result_string;
                            }
                        }
                    }
                }
                //log::info!("Query {} check took {:?}", id, this_now.elapsed());
            };
            query_futures.push(query_future);
        }

        // Wait for all query futures to complete
        future::join_all(query_futures).await;
        //log::info!("done checking subscribed queries in {:?}", now.elapsed());

        // Remove timed out queries and notify prolog service
        if !queries_to_remove.is_empty() {
            let removed_queries = {
                let mut queries = self.subscribed_queries.lock().await;
                queries_to_remove
                    .iter()
                    .filter_map(|id| queries.remove(id).map(|q| (id.clone(), q.query)))
                    .collect::<Vec<_>>()
            };

            // Notify prolog service for each timed out subscription
            let uuid = self.persisted.lock().await.uuid.clone();
            for (_id, query) in removed_queries {
                if let Err(e) = get_prolog_service()
                    .await
                    .subscription_ended(uuid.clone(), query)
                    .await
                {
                    log::warn!(
                        "Failed to notify prolog service of subscription timeout: {}",
                        e
                    );
                }
            }
        }
    }

    async fn subscribed_queries_loop(&self) {
        // Prolog subscriptions only make sense in Simple and Pooled modes
        // In SdnaOnly mode, link queries don't work, only SDNA queries
        // In Disabled mode, prolog is disabled entirely
        if PROLOG_MODE == PrologMode::SdnaOnly || PROLOG_MODE == PrologMode::Disabled {
            log::debug!(
                "Prolog subscription loop disabled in {:?} mode",
                PROLOG_MODE
            );
            return;
        }

        let mut log_counter = 0;
        const LOG_INTERVAL: u32 = 300; // Log every ~60 seconds (300 * 200ms)

        while !*self.is_teardown.lock().await {
            // Check trigger without holding lock during the operation
            let should_check = { *self.trigger_prolog_subscription_check.lock().await };

            if should_check {
                self.check_subscribed_queries().await;
                *self.trigger_prolog_subscription_check.lock().await = false;
            }

            // Periodic subscription logging
            log_counter += 1;
            if log_counter >= LOG_INTERVAL {
                log_counter = 0;
                // Get perspective_uuid FIRST before acquiring subscribed_queries lock to avoid deadlock
                let perspective_uuid = self.persisted.lock().await.uuid.clone();
                let queries = self.subscribed_queries.lock().await;
                if !queries.is_empty() {
                    log::info!(
                        "📊 Prolog subscriptions [{}]: {} active",
                        perspective_uuid,
                        queries.len()
                    );
                    for (id, query) in queries.iter() {
                        let query_preview = if query.query.len() > 100 {
                            format!("{}...", &query.query[..100])
                        } else {
                            query.query.clone()
                        };
                        log::info!("   - [{}]: {}", id, query_preview);
                    }
                }
            }

            sleep(Duration::from_millis(QUERY_SUBSCRIPTION_CHECK_INTERVAL)).await;
        }
    }

    async fn fallback_sync_loop(&self) {
        let uuid = self.persisted.lock().await.uuid.clone();
        log::debug!("Starting fallback sync loop for perspective {}", uuid);

        while !*self.is_teardown.lock().await {
            // Check if we should run the fallback sync (avoid holding multiple locks)
            let should_run = {
                // Check perspective state first
                let is_synced_neighbourhood = {
                    let handle = self.persisted.lock().await;
                    let result =
                        handle.state == PerspectiveState::Synced && handle.neighbourhood.is_some();
                    drop(handle); // Release lock immediately
                    result
                };

                if !is_synced_neighbourhood {
                    false
                } else {
                    // Check link language availability
                    let link_lang_available = {
                        let link_lang = self.link_language.read().await;
                        let result = link_lang.is_some();
                        drop(link_lang); // Release lock immediately
                        result
                    };

                    if !link_lang_available {
                        false
                    } else {
                        // Check timing conditions
                        let last_success = *self.last_successful_fallback_sync.lock().await;
                        let current_interval = *self.fallback_sync_interval.lock().await;

                        // Only run if we haven't had a successful sync recently or it's been a while
                        last_success.is_none() || last_success.unwrap().elapsed() > current_interval
                    }
                }
            };

            if should_run {
                log::debug!("Running fallback sync for perspective {}", uuid);
                let success = self.ensure_public_links_are_shared().await;

                if success {
                    // Update last successful sync time and increase interval
                    {
                        *self.last_successful_fallback_sync.lock().await =
                            Some(tokio::time::Instant::now());
                        *self.fallback_sync_interval.lock().await = Duration::from_secs(300);
                    }
                    log::debug!("Fallback sync successful for perspective {}, increasing interval to 5 minutes", uuid);
                } else {
                    // Reset interval to 30 seconds on failure
                    *self.fallback_sync_interval.lock().await = Duration::from_secs(30);
                    log::warn!(
                        "Fallback sync failed for perspective {}, keeping interval at 30 seconds",
                        uuid
                    );
                }
            }

            // Get fresh interval for sleep (after potential updates)
            let sleep_interval = *self.fallback_sync_interval.lock().await;
            sleep(sleep_interval).await;
        }

        log::debug!("Fallback sync loop ended for perspective {}", uuid);
    }

    /// Reset the fallback sync interval to 30 seconds when new links are added
    /// This ensures that new links get synced quickly
    async fn reset_fallback_sync_interval(&self) {
        *self.fallback_sync_interval.lock().await = Duration::from_secs(30);
        let uuid = self.persisted.lock().await.uuid.clone();
        log::debug!(
            "Reset fallback sync interval to 30 seconds for perspective {}",
            uuid
        );
    }

    pub async fn create_batch(&self) -> String {
        let batch_uuid = Uuid::new_v4().to_string();
        self.batch_store.write().await.insert(
            batch_uuid.clone(),
            PerspectiveDiff {
                additions: Vec::new(),
                removals: Vec::new(),
            },
        );
        batch_uuid
    }

    pub async fn commit_batch(
        &mut self,
        batch_uuid: String,
        context: &AgentContext,
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        //let commit_start = std::time::Instant::now();
        //log::info!("🔄 BATCH COMMIT: Starting batch commit for batch_uuid: {}", batch_uuid);
        //let batch_retrieval_start = std::time::Instant::now();

        // Get the diff without holding lock during the entire operation
        let diff = {
            let mut batch_store = self.batch_store.write().await;

            match batch_store.remove(&batch_uuid) {
                Some(diff) => diff,
                None => return Err(anyhow!("No batch found with given UUID")),
            }
        };

        //log::info!("🔄 BATCH COMMIT: Retrieved batch diff in {:?} - {} additions, {} removals",
        //    batch_retrieval_start.elapsed(), diff.additions.len(), diff.removals.len());

        //let processing_start = std::time::Instant::now();
        let mut shared_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };
        let mut local_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };

        // Process additions
        for link in diff.additions {
            let status = link.status.unwrap_or(LinkStatus::Shared);
            let signed_expr = create_signed_expression(link.data.normalize(), context)?;
            let decorated =
                DecoratedLinkExpression::from((LinkExpression::from(signed_expr), status.clone()));

            match status {
                LinkStatus::Shared => shared_diff.additions.push(decorated),
                LinkStatus::Local => local_diff.additions.push(decorated),
            }
        }

        // Process removals
        for link in diff.removals {
            let status = link.status.clone().unwrap_or(LinkStatus::Shared);
            let decorated = DecoratedLinkExpression::from((link, status.clone()));
            match status {
                LinkStatus::Shared => shared_diff.removals.push(decorated),
                LinkStatus::Local => local_diff.removals.push(decorated),
            }
        }

        //log::info!("🔄 BATCH COMMIT: Link processing took {:?} - shared: {} add/{} rem, local: {} add/{} rem",
        //    processing_start.elapsed(),
        //    shared_diff.additions.len(), shared_diff.removals.len(),
        //    local_diff.additions.len(), local_diff.removals.len());

        // Apply shared changes
        if !shared_diff.additions.is_empty() || !shared_diff.removals.is_empty() {
            //let db_start = std::time::Instant::now();
            //log::info!("🔄 BATCH COMMIT: Starting DB operations for shared changes");

            // Commit to link language (SurrealDB will be updated later via persist_link_diff)
            if self.has_link_language().await {
                //let link_lang_start = std::time::Instant::now();
                //log::info!("🔄 BATCH COMMIT: Starting link language commit");

                let perspective_diff = PerspectiveDiff {
                    additions: shared_diff
                        .additions
                        .iter()
                        .map(|l| l.clone().into())
                        .collect(),
                    removals: shared_diff
                        .removals
                        .iter()
                        .map(|l| l.clone().into())
                        .collect(),
                };
                self.spawn_commit_and_handle_error(&perspective_diff);

                //log::info!("🔄 BATCH COMMIT: Link language commit spawned in {:?}", link_lang_start.elapsed());
            }
        }

        // Create combined diff for prolog update, SurrealDB update, and return value
        let combined_diff = DecoratedPerspectiveDiff {
            additions: [shared_diff.additions.clone(), local_diff.additions.clone()].concat(),
            removals: [shared_diff.removals.clone(), local_diff.removals.clone()].concat(),
        };

        // Only spawn prolog facts update if there are changes to update
        if !combined_diff.additions.is_empty() || !combined_diff.removals.is_empty() {
            //let prolog_start = std::time::Instant::now();
            //log::info!("🔄 BATCH COMMIT: Starting prolog facts update - {} add, {} rem",
            //    combined_diff.additions.len(), combined_diff.removals.len());

            // Update prolog facts once for all changes and wait for completion
            // Update Prolog: subscription engine (immediate) + query engine (lazy)
            // Update both Prolog engines: subscription (immediate) + query (lazy)
            self.update_prolog_engines(combined_diff.clone()).await;

            self.persist_link_diff(&combined_diff).await?;

            //log::info!("🔄 BATCH COMMIT: Prolog facts update completed in {:?}", prolog_start.elapsed());
        }

        //log::info!("🔄 BATCH COMMIT: Total batch commit took {:?}", commit_start.elapsed());

        // Return combined diff
        Ok(combined_diff)
    }
}

pub fn prolog_result(result: String) -> Value {
    let v: Value = serde_json::from_str(&result).unwrap();
    match v {
        Value::String(string) => {
            if string == "true" {
                Value::Bool(true)
            } else if string == "false" {
                Value::Bool(false)
            } else {
                Value::String(string)
            }
        }
        _ => v,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;
    use crate::db::Ad4mDb;
    use crate::graphql::graphql_types::PerspectiveState;
    use crate::perspectives::perspective_instance::PerspectiveHandle;
    use crate::prolog_service::init_prolog_service;
    use crate::test_utils::setup_wallet;
    use fake::{Fake, Faker};
    use uuid::Uuid;

    async fn setup() -> PerspectiveInstance {
        setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();

        // Initialize agent and prolog services for tests
        AgentService::init_global_test_instance();
        init_prolog_service().await;

        let uuid = Uuid::new_v4().to_string();

        let instance = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid,
                name: Some("Test Perspective".to_string()),
                shared_url: None,
                neighbourhood: None,
                state: PerspectiveState::Private,
                owners: None,
            },
            None,
        );

        // Ensure prolog engine pool is initialized
        instance
            .ensure_prolog_engine_pool()
            .await
            .expect("Failed to initialize prolog engine pool");

        instance
    }

    async fn create_perspective() -> PerspectiveInstance {
        let uuid = Uuid::new_v4().to_string();

        let instance = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid,
                name: Some("Test Perspective".to_string()),
                shared_url: None,
                neighbourhood: None,
                state: PerspectiveState::Private,
                owners: None,
            },
            None,
        );

        // Ensure prolog engine pool is initialized
        instance
            .ensure_prolog_engine_pool()
            .await
            .expect("Failed to initialize prolog engine pool");

        instance
    }

    pub fn create_link() -> Link {
        Link {
            source: format!("https://{}.com", Faker.fake::<String>()),
            target: format!("https://{}.org", Faker.fake::<String>()),
            predicate: Some(format!("https://{}.net", Faker.fake::<String>())),
        }
    }

    #[tokio::test]
    async fn test_context_aware_prolog_pools() {
        let perspective = setup().await;

        // Test main agent context
        let main_context = crate::agent::AgentContext::main_agent();
        let main_pool_id = perspective.get_pool_id_for_context("test-uuid", &main_context);
        assert_eq!(main_pool_id, "test-uuid");

        // Test user context
        let user_context =
            crate::agent::AgentContext::for_user_email("test@example.com".to_string());
        let user_pool_id = perspective.get_pool_id_for_context("test-uuid", &user_context);
        assert_eq!(user_pool_id, "test-uuid_test@example.com");

        // Test different users get different pools
        let user2_context =
            crate::agent::AgentContext::for_user_email("test2@example.com".to_string());
        let user2_pool_id = perspective.get_pool_id_for_context("test-uuid", &user2_context);
        assert_eq!(user2_pool_id, "test-uuid_test2@example.com");

        // Verify they're all different
        assert_ne!(main_pool_id, user_pool_id);
        assert_ne!(user_pool_id, user2_pool_id);
        assert_ne!(main_pool_id, user2_pool_id);

        println!("✅ Context-aware prolog pool selection tests passed");
    }

    #[tokio::test]
    async fn test_get_all_links_after_adding_five() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();

        for _ in 0..5 {
            let link = create_link();
            let expression = perspective
                .add_link(
                    link.clone(),
                    LinkStatus::Local,
                    None,
                    &AgentContext::main_agent(),
                )
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery::default();
        let mut links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 5);
        let mut all_links_sorted = all_links.clone();
        let cmp = |a: &DecoratedLinkExpression, b: &DecoratedLinkExpression| {
            let at = chrono::DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let bt = chrono::DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            at.cmp(&bt)
                .then(a.data.source.cmp(&b.data.source))
                .then(a.data.predicate.cmp(&b.data.predicate))
                .then(a.data.target.cmp(&b.data.target))
                .then(a.author.cmp(&b.author))
        };
        links.sort_by(cmp);
        all_links_sorted.sort_by(cmp);
        assert_eq!(links, all_links_sorted);
    }

    #[tokio::test]
    async fn test_get_links_by_source() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();
        let source = "ad4m://self";

        for i in 0..5 {
            let mut link = create_link();
            if i % 2 == 0 {
                link.source = source.to_string();
            }

            let expression = perspective
                .add_link(
                    link.clone(),
                    LinkStatus::Shared,
                    None,
                    &AgentContext::main_agent(),
                )
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery {
            source: Some(source.to_string()),
            ..Default::default()
        };
        let mut links = perspective.get_links(&query).await.unwrap();
        let mut expected_links: Vec<_> = all_links
            .into_iter()
            .filter(|expr| expr.data.source == source)
            .collect();
        assert_eq!(links.len(), expected_links.len());
        let cmp = |a: &DecoratedLinkExpression, b: &DecoratedLinkExpression| {
            let at = chrono::DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let bt = chrono::DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            at.cmp(&bt)
                .then(a.data.predicate.cmp(&b.data.predicate))
                .then(a.data.target.cmp(&b.data.target))
                .then(a.author.cmp(&b.author))
        };
        links.sort_by(cmp);
        expected_links.sort_by(cmp);
        assert_eq!(links, expected_links);
    }

    #[tokio::test]
    async fn test_remove_link() {
        let mut perspective = setup().await;
        let link = create_link();
        let status = LinkStatus::Local;

        // Add a link to the perspective
        let expression = perspective
            .add_link(link.clone(), status, None, &AgentContext::main_agent())
            .await
            .unwrap();

        // Ensure the link is present
        let query = LinkQuery::default();
        let links_before_removal = perspective.get_links(&query).await.unwrap();
        assert!(links_before_removal.contains(&expression));

        // Remove the link from the perspective
        perspective
            .remove_link(expression.clone().into(), None)
            .await
            .unwrap();

        // Ensure the link is no longer present
        let links_after_removal = perspective.get_links(&query).await.unwrap();
        assert!(!links_after_removal.contains(&expression));
    }

    #[tokio::test]
    async fn test_link_query_date_filtering() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();
        let now = chrono::Utc::now();

        // Add links with timestamps spread out by one minute intervals
        for i in 0..5 {
            let mut link = create_link();
            link.target = format!("lang://test-target {}", i);
            let mut link = create_signed_expression(link, &AgentContext::main_agent())
                .expect("Failed to create link");
            link.timestamp = (now - chrono::Duration::minutes(5)
                + chrono::Duration::minutes(i as i64))
            .to_rfc3339();
            let expression = perspective
                .add_link_expression(LinkExpression::from(link.clone()), LinkStatus::Shared, None)
                .await
                .unwrap();
            all_links.push(expression);
            println!("Added link with timestamp: {}, {:?}", link.timestamp, link);
        }

        // Query for links with a from_date set to 3 minutes in
        let from_date = (now - chrono::Duration::minutes(5) + chrono::Duration::minutes(3)).into();
        let query_with_from_date = LinkQuery {
            from_date: Some(from_date),
            ..Default::default()
        };
        //println!("Query with from_date: {:?}", query_with_from_date);
        let links_from_date = perspective.get_links(&query_with_from_date).await.unwrap();
        //println!("Links from date: {:?}", links_from_date);
        assert_eq!(links_from_date.len(), 2);

        // Query for links with an until_date set to 3 minutes in
        let until_date = (now - chrono::Duration::minutes(5) + chrono::Duration::minutes(3)).into();
        let query_with_until_date = LinkQuery {
            until_date: Some(until_date),
            ..Default::default()
        };
        let links_until_date = perspective.get_links(&query_with_until_date).await.unwrap();
        assert_eq!(links_until_date.len(), 4);

        // Query for links with both from_date and until_date set to filter a range
        let from_date = (now - chrono::Duration::minutes(4)).into();
        let until_date =
            (now - chrono::Duration::minutes(2) - chrono::Duration::seconds(30)).into();
        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            ..Default::default()
        };
        let links_date_range = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_range.len(), 2);

        // reverse for descending order
        let from_date = (now).into();
        let until_date = (now - chrono::Duration::minutes(10)).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 5);
        assert_eq!(links_date_desc[0].data.target, all_links[4].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[3].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);
        assert_eq!(links_date_desc[3].data.target, all_links[1].data.target);
        assert_eq!(links_date_desc[4].data.target, all_links[0].data.target);

        // reverse for descending order with limit
        let from_date = (now).into();
        let until_date = (now - chrono::Duration::minutes(10)).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            limit: Some(3),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 3);
        links_date_desc
            .iter()
            .for_each(|l| println!("Link: {:?}", l.data.target));
        assert_eq!(links_date_desc[0].data.target, all_links[4].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[3].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);

        // ascending order with limit
        let from_date = (now - chrono::Duration::minutes(10)).into();
        let until_date = (now).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            limit: Some(3),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 3);
        links_date_desc
            .iter()
            .for_each(|l| println!("Link: {:?}", l.data.target));
        assert_eq!(links_date_desc[0].data.target, all_links[0].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[1].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);
    }

    // Additional tests for updateLink, removeLink, syncWithSharingAdapter, etc. would go here
    // following the same pattern as above.

    #[tokio::test]
    async fn test_batch_operations() {
        let mut perspective = setup().await;
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        perspective
            .add_link(
                link.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 0);

        // Commit the batch
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 1);

        // Verify links are now in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
    }

    #[tokio::test]
    async fn test_batch_update_and_remove() {
        let mut perspective = setup().await;
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        // Add initial link
        perspective
            .add_link(
                link.clone(),
                LinkStatus::Shared,
                None,
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.target, link.target);

        // Update link in batch
        let mut new_link = link.clone();
        new_link.target = "new_target".to_string();
        perspective
            .update_link(
                links[0].clone().into(),
                new_link.clone(),
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // Commit the batch
        perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();

        // Verify final state in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.target, new_link.target);
    }

    #[tokio::test]
    async fn test_batch_multiple_operations() {
        let mut perspective = setup().await;

        // one link outside the batch, for removal
        let link0 = create_link();
        let link0_expression = perspective
            .add_link(
                link0.clone(),
                LinkStatus::Shared,
                None,
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // two links in the batch
        let link1 = create_link();
        let mut link2 = link1.clone();
        link2.target = "test://target2".to_string();

        let batch_id = perspective.create_batch().await;

        // Add two links in batch
        perspective
            .add_link(
                link1.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();
        perspective
            .add_link(
                link2.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();
        perspective
            .remove_link(link0_expression.clone().into(), Some(batch_id.clone()))
            .await
            .unwrap();

        let query = LinkQuery::default();

        let links_before = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_before.len(), 1);

        // Commit the batch
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 2); // link1 and link2
        assert_eq!(diff.removals.len(), 1); // link1

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    #[tokio::test]
    async fn test_batch_error_handling() {
        let mut perspective = setup().await;

        // Try to commit non-existent batch
        let result = perspective
            .commit_batch("non-existent".to_string(), &AgentContext::main_agent())
            .await;
        assert!(result.is_err());

        // Create a batch
        let batch_id = perspective.create_batch().await;

        // Try to remove non-existent link in batch
        let non_existent_link = LinkExpression {
            author: "test".to_string(),
            timestamp: "0".to_string(),
            data: Link {
                source: "test://non-existent".to_string(),
                predicate: Some("test://predicate".to_string()),
                target: "test://target".to_string(),
            },
            proof: Default::default(),
            status: None,
        };
        let result = perspective
            .remove_link(non_existent_link.clone(), Some(batch_id.clone()))
            .await;
        assert!(result.is_err());

        // Try to use invalid batch ID
        let result = perspective
            .add_link(
                create_link(),
                LinkStatus::Shared,
                Some("invalid".to_string()),
                &AgentContext::main_agent(),
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_batch_with_execute_commands() {
        let mut perspective = setup().await;
        let batch_id = perspective.create_batch().await;

        // Create commands to add links
        let commands = vec![
            Command {
                source: Some("test://source1".to_string()),
                predicate: Some("test://predicate1".to_string()),
                target: Some("test://target1".to_string()),
                local: None,
                action: Action::AddLink,
            },
            Command {
                source: Some("test://source2".to_string()),
                predicate: Some("test://predicate2".to_string()),
                target: Some("test://target2".to_string()),
                local: None,
                action: Action::AddLink,
            },
        ];

        // Execute commands in batch
        perspective
            .execute_commands(
                commands,
                "test://expression".to_string(),
                vec![],
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // Verify links are not visible before commit
        let query = LinkQuery {
            source: None,
            predicate: None,
            target: None,
            from_date: None,
            until_date: None,
            limit: None,
        };
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 0);

        // Commit batch and verify links are now visible
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 2);
        assert_eq!(diff.removals.len(), 0);

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    // #[tokio::test]
    // async fn test_batch_with_create_subject() {
    //     let mut perspective = setup().await;
    //     let batch_id = perspective.create_batch().await;

    //     // Create a subject class option
    //     let subject_class = SubjectClassOption {
    //         class_name: Some("TestSubject".to_string()),
    //         query: None,
    //     };

    //     // Create subject in batch
    //     perspective.create_subject(
    //         subject_class,
    //         "test://expression1".to_string(),
    //         None,
    //         Some(batch_id.clone())
    //     ).await.unwrap();

    //     // Verify subject links are not visible before commit
    //     let query = LinkQuery {
    //         source: Some("test://expression1".to_string()),
    //         predicate: None,
    //         target: None,
    //         from_date: None,
    //         until_date: None,
    //         limit: None,
    //     };
    //     let links = perspective.get_links(&query).await.unwrap();
    //     assert_eq!(links.len(), 0);

    //     // Commit batch and verify subject links are now visible
    //     let diff = perspective.commit_batch(batch_id).await.unwrap();
    //     assert!(diff.additions.len() > 0);
    //     assert_eq!(diff.removals.len(), 0);

    //     let links_after = perspective.get_links(&query).await.unwrap();
    //     assert!(links_after.len() > 0);
    // }

    // ============================================================================
    // DOCUMENTATION EXAMPLES TESTS
    // These tests verify all query examples from the SurrealDB documentation
    // ============================================================================
}
