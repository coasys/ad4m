use super::sdna::{generic_link_fact, is_sdna_link};
use super::update_perspective;
use super::utils::{
    prolog_get_all_string_bindings, prolog_get_first_string_binding, prolog_resolution_to_string,
};
use crate::agent::{self, create_signed_expression};
use crate::graphql::graphql_types::{
    DecoratedPerspectiveDiff, ExpressionRendered, JsResultType, LinkMutations, LinkQuery,
    LinkStatus, NeighbourhoodSignalFilter, OnlineAgent, PerspectiveExpression, PerspectiveHandle,
    PerspectiveLinkFilter, PerspectiveLinkUpdatedFilter, PerspectiveQuerySubscriptionFilter,
    PerspectiveState, PerspectiveStateFilter,
};
use crate::languages::language::Language;
use crate::languages::LanguageController;
use crate::perspectives::utils::{prolog_get_first_binding, prolog_value_to_json_string};
use crate::prolog_service::get_prolog_service;
use crate::prolog_service::types::{QueryMatch, QueryResolution};
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
use json5;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};
use tokio::time::{sleep, Instant};
use tokio::{join, time};
use uuid;
use uuid::Uuid;

static MAX_COMMIT_BYTES: usize = 3_000_000; //3MiB
static MAX_PENDING_DIFFS_COUNT: usize = 150;
static MAX_PENDING_SECONDS: u64 = 3;
static IMMEDIATE_COMMITS_COUNT: usize = 20;
static QUERY_SUBSCRIPTION_TIMEOUT: u64 = 300; // 5 minutes in seconds
static QUERY_SUBSCRIPTION_CHECK_INTERVAL: u64 = 200; // 200ms

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
    link_language: Arc<Mutex<Option<Language>>>,
    trigger_notification_check: Arc<Mutex<bool>>,
    trigger_prolog_subscription_check: Arc<Mutex<bool>>,
    commit_debounce_timer: Arc<Mutex<Option<tokio::time::Instant>>>,
    immediate_commits_remaining: Arc<Mutex<usize>>,
    subscribed_queries: Arc<Mutex<HashMap<String, SubscribedQuery>>>,
    batch_store: Arc<RwLock<HashMap<String, PerspectiveDiff>>>,
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
            link_language: Arc::new(Mutex::new(None)),
            trigger_notification_check: Arc::new(Mutex::new(false)),
            trigger_prolog_subscription_check: Arc::new(Mutex::new(false)),
            commit_debounce_timer: Arc::new(Mutex::new(None)),
            immediate_commits_remaining: Arc::new(Mutex::new(IMMEDIATE_COMMITS_COUNT)),
            subscribed_queries: Arc::new(Mutex::new(HashMap::new())),
            batch_store: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn start_background_tasks(self) {
        let _ = join!(
            self.ensure_link_language(),
            self.notification_check_loop(),
            self.nh_sync_loop(),
            self.pending_diffs_loop(),
            self.subscribed_queries_loop()
        );
    }

    pub async fn teardown_background_tasks(&self) {
        *self.is_teardown.lock().await = true;
    }

    async fn ensure_link_language(&self) {
        let mut interval = time::interval(Duration::from_secs(5));
        while !*self.is_teardown.lock().await {
            if self.link_language.lock().await.is_none()
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

                match LanguageController::language_by_address(nh.data.link_language.clone()).await {
                    Ok(Some(language)) => {
                        {
                            let mut link_language_guard = self.link_language.lock().await;
                            *link_language_guard = Some(language);
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
            let mut link_language_guard = self.link_language.lock().await;
            if let Some(link_language) = link_language_guard.as_mut() {
                match link_language.sync().await {
                    Ok(_) => (),
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
        let link_language_guard = self.link_language.lock().await;
        link_language_guard.is_some()
    }

    async fn commit_pending_diffs(&self) -> Result<(), AnyError> {
        let uuid = self.persisted.lock().await.uuid.clone();

        let (pending_diffs, pending_ids) = Ad4mDb::with_global_instance(|db| {
            db.get_pending_diffs_by_size(&uuid, MAX_COMMIT_BYTES, Some(MAX_PENDING_DIFFS_COUNT))
        })?;

        if !pending_ids.is_empty() {
            let mut link_language_lock = self.link_language.lock().await;
            if let Some(link_language) = link_language_lock.as_mut() {
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
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut interval = time::interval(Duration::from_secs(5));
        let mut before = self.notification_trigger_snapshot().await;
        while !*self.is_teardown.lock().await {
            interval.tick().await;
            let changed = *(self.trigger_notification_check.lock().await);
            if changed {
                let after = self.notification_trigger_snapshot().await;
                let new_matches = Self::subtract_before_notification_matches(&before, &after);
                tokio::spawn(Self::publish_notification_matches(
                    uuid.clone(),
                    new_matches,
                ));
                before = after;
                *(self.trigger_notification_check.lock().await) = false;
            }
        }
    }

    async fn ensure_public_links_are_shared(&self) {
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
            let mut local_links =
                Ad4mDb::with_global_instance(|db| db.get_all_links(&uuid)).unwrap();

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
                let result = link_language
                    .commit(PerspectiveDiff {
                        additions: links_to_commit,
                        removals: vec![],
                    })
                    .await;

                if let Err(e) = result {
                    log::error!("Error calling link language's commit in ensure_public_links_are_shared: {:?}", e);
                }
            }

            //Ad4mDb::with_global_instance(|db| db.add_many_links(&self.persisted.lock().await.uuid, &remote_links)).unwrap(); // Assuming add_many_links takes a reference to a Vec<LinkExpression> and returns Result<(), AnyError>
        }
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
            if let Some(link_language) = self.link_language.lock().await.as_mut() {
                // Got lock on Link Language, no other commit running
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

    pub async fn diff_from_link_language(&self, diff: PerspectiveDiff) {
        let handle = self.persisted.lock().await.clone();
        if !diff.additions.is_empty() {
            Ad4mDb::with_global_instance(|db| {
                db.add_many_links(&handle.uuid, diff.additions.clone(), &LinkStatus::Shared)
            })
            .expect("Failed to add many links");
        }

        if !diff.removals.is_empty() {
            Ad4mDb::with_global_instance(|db| {
                for link in &diff.removals {
                    db.remove_link(&handle.uuid, link)
                        .expect("Failed to remove link");
                }
            });
        }

        let decorated_diff = DecoratedPerspectiveDiff {
            additions: diff
                .additions
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
            removals: diff
                .removals
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
        };

        self.spawn_prolog_facts_update(decorated_diff.clone(), None);
        self.pubsub_publish_diff(decorated_diff).await;
    }

    pub async fn telepresence_signal_from_link_language(&self, mut signal: PerspectiveExpression) {
        signal.verify_signatures();
        let handle = self.persisted.lock().await.clone();
        get_global_pubsub()
            .await
            .publish(
                &NEIGHBOURHOOD_SIGNAL_TOPIC,
                &serde_json::to_string(&NeighbourhoodSignalFilter {
                    perspective: handle,
                    signal,
                })
                .unwrap(),
            )
            .await;
    }

    pub async fn add_link(
        &mut self,
        link: Link,
        status: LinkStatus,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let link_expr: LinkExpression = create_signed_expression(link)?.into();
        self.add_link_expression(link_expr.into(), status, batch_id)
            .await
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

            let handle = self.persisted.lock().await.clone();
            let (link_from_db, status) =
                Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &link_expression))?
                    .ok_or(anyhow!("Link not found"))?;

            diff.removals.push(link_from_db.clone());
            Ok(DecoratedLinkExpression::from((link_from_db, status)))
        } else {
            let handle = self.persisted.lock().await.clone();
            if let Some((link_from_db, status)) =
                Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &link_expression))?
            {
                Ad4mDb::with_global_instance(|db| db.remove_link(&handle.uuid, &link_expression))?;
                let diff = PerspectiveDiff::from_removals(vec![link_expression.clone()]);
                let decorated_link = DecoratedLinkExpression::from((link_from_db, status.clone()));
                let decorated_diff =
                    DecoratedPerspectiveDiff::from_removals(vec![decorated_link.clone()]);

                self.spawn_prolog_facts_update(decorated_diff.clone(), None);
                self.pubsub_publish_diff(decorated_diff.clone()).await;

                if status == LinkStatus::Shared {
                    self.spawn_commit_and_handle_error(&diff);
                }

                Ok(decorated_link)
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

        for link in &decorated_diff.additions {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: link.clone(),
                    })
                    .unwrap(),
                )
                .await;
        }

        for link in &decorated_diff.removals {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: link.clone(),
                    })
                    .unwrap(),
                )
                .await;
        }
    }

    pub async fn add_link_expression(
        &mut self,
        link_expression: LinkExpression,
        status: LinkStatus,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
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
        Ad4mDb::with_global_instance(|db| db.add_link(&handle.uuid, &link_expression, &status))?;

        let diff = PerspectiveDiff::from_additions(vec![link_expression.clone()]);
        let decorated_link_expression =
            DecoratedLinkExpression::from((link_expression.clone(), status.clone()));
        let decorated_perspective_diff =
            DecoratedPerspectiveDiff::from_additions(vec![decorated_link_expression.clone()]);

        self.spawn_prolog_facts_update(decorated_perspective_diff.clone(), None);

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
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let link_expressions: Result<Vec<_>, _> = links
            .into_iter()
            .map(|l| create_signed_expression(l).map(LinkExpression::from))
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

            let handle = self.persisted.lock().await.clone();
            Ad4mDb::with_global_instance(|db| {
                db.add_many_links(&handle.uuid, link_expressions.clone(), &status)
            })?;

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
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let additions = mutations
            .additions
            .into_iter()
            .map(Link::from)
            .map(create_signed_expression)
            .map(|r| r.map(LinkExpression::from))
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let removals = mutations
            .removals
            .into_iter()
            .map(LinkExpression::try_from)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;

        Ad4mDb::with_global_instance(|db| {
            db.add_many_links(&handle.uuid, additions.clone(), &status)
        })?;

        for link in &removals {
            Ad4mDb::with_global_instance(|db| db.remove_link(&handle.uuid, link))?;
        }

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

        self.spawn_prolog_facts_update(decorated_diff.clone(), None);
        self.pubsub_publish_diff(decorated_diff.clone()).await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
        }
        Ok(decorated_diff)
    }

    pub async fn update_link(
        &mut self,
        old_link: LinkExpression,
        new_link: Link,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let link_option = Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &old_link))?;

        let (link, link_status) = match link_option {
            Some(link) => link,
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

        let new_link_expression = LinkExpression::from(create_signed_expression(new_link)?);

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
            Ad4mDb::with_global_instance(|db| {
                db.update_link(&handle.uuid, &link, &new_link_expression)
            })?;

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

            self.spawn_prolog_facts_update(decorated_diff.clone(), None);

            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_UPDATED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkUpdatedFilter {
                        perspective: handle.clone(),
                        old_link: decorated_old_link,
                        new_link: decorated_new_link_expression.clone(),
                    })
                    .unwrap(),
                )
                .await;

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
        let handle = self.persisted.lock().await.clone();

        // Filter to only existing links and collect their statuses
        let mut existing_links = Vec::new();
        for link in link_expressions {
            if let Some((link_from_db, status)) =
                Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &link))?
            {
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

            // Create diff from links that exist
            let diff = PerspectiveDiff::from_removals(links.clone());

            // Create decorated versions
            let decorated_links: Vec<DecoratedLinkExpression> = links
                .into_iter()
                .zip(statuses.iter())
                .map(|(link, status)| DecoratedLinkExpression::from((link, status.clone())))
                .collect();

            let decorated_diff = DecoratedPerspectiveDiff::from_removals(decorated_links.clone());

            // Remove from DB
            for link in diff.removals.iter() {
                Ad4mDb::with_global_instance(|db| db.remove_link(&handle.uuid, link))?;
            }

            self.spawn_prolog_facts_update(decorated_diff.clone(), None);
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

    async fn get_links_local(
        &self,
        query: &LinkQuery,
    ) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut result =
            if query.source.is_none() && query.predicate.is_none() && query.target.is_none() {
                Ad4mDb::with_global_instance(|db| db.get_all_links(&uuid))?
            } else if let Some(source) = &query.source {
                Ad4mDb::with_global_instance(|db| db.get_links_by_source(&uuid, source))?
            } else if let Some(target) = &query.target {
                Ad4mDb::with_global_instance(|db| db.get_links_by_target(&uuid, target))?
            } else if let Some(predicate) = &query.predicate {
                Ad4mDb::with_global_instance(|db| {
                    Ok::<Vec<(LinkExpression, LinkStatus)>, AnyError>(
                        db.get_all_links(&uuid)?
                            .into_iter()
                            .filter(|(link, _)| link.data.predicate.as_ref() == Some(predicate))
                            .collect::<Vec<(LinkExpression, LinkStatus)>>(),
                    )
                })?
            } else {
                vec![]
            };

        if let Some(predicate) = &query.predicate {
            result.retain(|(link, _status)| link.data.predicate.as_ref() == Some(predicate));
        }

        if let Some(target) = &query.target {
            result.retain(|(link, _status)| link.data.target == *target);
        }

        if let Some(source) = &query.source {
            result.retain(|(link, _status)| link.data.source == *source);
        }

        let until_date: Option<chrono::DateTime<chrono::Utc>> =
            query.until_date.clone().map(|d| d.into());
        let from_date: Option<chrono::DateTime<chrono::Utc>> =
            query.from_date.clone().map(|d| d.into());

        if let Some(from_date) = &from_date {
            result.retain(|(link, _)| {
                let link_date = DateTime::parse_from_rfc3339(&link.timestamp).unwrap();
                link_date >= *from_date
            });
        }

        if let Some(until_date) = &until_date {
            result.retain(|(link, _)| {
                let link_date = DateTime::parse_from_rfc3339(&link.timestamp).unwrap();
                link_date <= *until_date
            });
        }
        /*

        if let Some(limit) = query.limit {
            let limit = limit as usize;
            let result_length = result.len();
            let start_limit = if from_date >= until_date {
                result_length.saturating_sub(limit)
            } else {
                0
            } as usize;

            let end_limit = if from_date >= until_date {
                result_length
            } else {
                limit.min(result_length)
            } as usize;

            result = result[..limit as usize].to_vec();
        }
        */
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

        let mut links = self.get_links_local(&query).await?;

        links.sort_by(|(a, _), (b, _)| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
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
    pub async fn add_sdna(
        &mut self,
        name: String,
        mut sdna_code: String,
        sdna_type: SdnaType,
    ) -> Result<bool, AnyError> {
        let mut added = false;
        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;

        let predicate = match sdna_type {
            SdnaType::SubjectClass => "ad4m://has_subject_class",
            SdnaType::Flow => "ad4m://has_flow",
            SdnaType::Custom => "ad4m://has_custom_sdna",
        };

        let literal_name = Literal::from_string(name)
            .to_url()
            .expect("just initialized Literal couldn't be turned into URL");

        let links = self
            .get_links(&LinkQuery {
                source: Some("ad4m://self".to_string()),
                predicate: Some(predicate.to_string()),
                target: Some(literal_name.clone()),
                from_date: None,
                until_date: None,
                limit: None,
            })
            .await?;

        let author = agent::did();

        let mut sdna_links: Vec<Link> = Vec::new();

        let links = links
            .into_iter()
            .filter(|l| l.author == author)
            .collect::<Vec<DecoratedLinkExpression>>();

        if (Literal::from_url(sdna_code.clone())).is_err() {
            sdna_code = Literal::from_string(sdna_code)
                .to_url()
                .expect("just initialized Literal couldn't be turned into URL");
        }

        if links.is_empty() {
            sdna_links.push(Link {
                source: "ad4m://self".to_string(),
                predicate: Some(predicate.to_string()),
                target: literal_name.clone(),
            });

            sdna_links.push(Link {
                source: literal_name.clone(),
                predicate: Some("ad4m://sdna".to_string()),
                target: sdna_code,
            });

            self.add_links(sdna_links, LinkStatus::Shared, None).await?;
            added = true;
        }
        // Mutex guard is automatically dropped here
        Ok(added)
    }

    async fn ensure_prolog_engine_pool(&self) -> Result<(), AnyError> {
        // Take write lock and check if we need to initialize
        let _guard = self.prolog_update_mutex.write().await;

        // Get service reference before taking any locks
        let service = get_prolog_service().await;
        let uuid = self.persisted.lock().await.uuid.clone();

        // Check if pool exists under the write lock
        if !service.has_perspective_pool(uuid.clone()).await {
            // Create and initialize new pool
            service.ensure_perspective_pool(uuid.clone()).await?;

            // Initialize with links for optimized filtering
            let all_links = self.get_links(&LinkQuery::default()).await?;
            let neighbourhood_author = self.persisted
                .lock()
                .await
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone());
                
            service
                .update_perspective_links(uuid, "facts".to_string(), all_links, neighbourhood_author)
                .await?;
        }

        Ok(())
    }

    /// Executes a Prolog query against the engine, spawning and initializing the engine if necessary.
    pub async fn prolog_query(&self, query: String) -> Result<QueryResolution, AnyError> {
        let prolog_start = std::time::Instant::now();
        log::info!("🔍 PROLOG QUERY: Starting query: {} (chars: {})", 
            query.chars().take(100).collect::<String>(), query.len());
        
        let ensure_start = std::time::Instant::now();
        self.ensure_prolog_engine_pool().await?;
        log::info!("🔍 PROLOG QUERY: Engine pool ensured in {:?}", ensure_start.elapsed());

        let uuid_start = std::time::Instant::now();
        let uuid = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.uuid.clone()
        };
        log::info!("🔍 PROLOG QUERY: UUID retrieved in {:?}", uuid_start.elapsed());

        let service_start = std::time::Instant::now();
        let service = get_prolog_service().await;
        log::info!("🔍 PROLOG QUERY: Service retrieved in {:?}", service_start.elapsed());

        let lock_start = std::time::Instant::now();
        log::info!("🔍 PROLOG QUERY: Waiting for prolog_update_mutex read lock...");
        let _read_lock = self.prolog_update_mutex.read().await;
        log::info!("🔍 PROLOG QUERY: Acquired prolog_update_mutex read lock in {:?}", lock_start.elapsed());

        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };
        
        // ⚠️ CRITICAL: This might be blocked waiting for prolog_update_mutex!
        let query_start = std::time::Instant::now();
        log::info!("🔍 PROLOG QUERY: About to execute query...");
        
        let result = service.run_query(uuid, query.clone()).await?;
        
        log::info!("🔍 PROLOG QUERY: Query executed in {:?} (total: {:?})", 
            query_start.elapsed(), prolog_start.elapsed());
        
        match result {
            Err(e) => {
                // Engine error is handled at pool level now
                Err(anyhow!(e))
            }
            Ok(resolution) => Ok(resolution),
        }
    }

    /// Executes a Prolog subscription query with optimized routing to filtered engines
    pub async fn prolog_query_subscription(&self, query: String) -> Result<QueryResolution, AnyError> {
        // Get service reference before any locks
        let service = get_prolog_service().await;
        let uuid = self.persisted.lock().await.uuid.clone();

        // Ensure pool exists
        self.ensure_prolog_engine_pool().await?;

        let _read_lock = self.prolog_update_mutex.read().await;

        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        let result = service.run_query_subscription(uuid, query).await?;

        match result {
            Err(e) => {
                // Engine error is handled at pool level now
                Err(anyhow!(e))
            }
            Ok(resolution) => Ok(resolution),
        }
    }

    fn spawn_prolog_facts_update(
        &self,
        diff: DecoratedPerspectiveDiff,
        completion_sender: Option<tokio::sync::oneshot::Sender<()>>,
    ) {
        let self_clone = self.clone();

        tokio::spawn(async move {
            let spawn_start = std::time::Instant::now();
            log::info!("🔧 PROLOG UPDATE: Starting prolog facts update task - {} add, {} rem", 
                diff.additions.len(), diff.removals.len());
            
            let ensure_pool_start = std::time::Instant::now();
            if let Err(e) = self_clone.ensure_prolog_engine_pool().await {
                log::error!("Error spawning Prolog engine pool: {:?}", e);
                if let Some(sender) = completion_sender {
                    let _ = sender.send(());
                }
                return;
            }
            log::info!("🔧 PROLOG UPDATE: Engine pool ensured in {:?}", ensure_pool_start.elapsed());

            // Get UUID before acquiring write lock
            let uuid_start = std::time::Instant::now();
            let uuid = {
                let persisted_guard = self_clone.persisted.lock().await;
                persisted_guard.uuid.clone()
            };
            log::info!("🔧 PROLOG UPDATE: UUID retrieved in {:?}", uuid_start.elapsed());

            let analysis_start = std::time::Instant::now();
            let fact_rebuild_needed = !diff.removals.is_empty()
                || diff.additions.iter().any(|link| is_sdna_link(&link.data));
            log::info!("🔧 PROLOG UPDATE: Analysis completed in {:?} - rebuild_needed: {}", 
                analysis_start.elapsed(), fact_rebuild_needed);

            let mutex_wait_start = std::time::Instant::now();
            log::info!("🔧 PROLOG UPDATE: Waiting for prolog_update_mutex...");

            let did_update = if !fact_rebuild_needed {
                log::info!("🔧 PROLOG UPDATE: Using FAST ASSERTION path");
                // For additions only, use assertions - acquire lock only during prolog operations
                let assertions_start = std::time::Instant::now();
                let mut assertions: Vec<String> = Vec::new();
                for addition in &diff.additions {
                    assertions.push(generic_link_fact("assert_link_and_triple", addition));
                }
                log::info!("🔧 PROLOG UPDATE: Built {} assertions in {:?}", 
                    assertions.len(), assertions_start.elapsed());

                let service_start = std::time::Instant::now();
                let service = get_prolog_service().await;
                log::info!("🔧 PROLOG UPDATE: Got prolog service in {:?}", service_start.elapsed());

                // Acquire write lock only for the prolog operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());
                
                let query_start = std::time::Instant::now();
                let query = format!("{}.", assertions.join(","));
                log::info!("🔧 PROLOG UPDATE: Running assertion query: {} chars", query.len());
                
                match service.run_query_all(uuid, query).await {
                    Ok(()) => {
                        log::info!("🔧 PROLOG UPDATE: Assertion query completed successfully in {:?}", query_start.elapsed());
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
                log::info!("🔧 PROLOG UPDATE: Using FULL REBUILD path");
                // For fact rebuild, acquire write lock for the entire operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());
                
                let rebuild_start = std::time::Instant::now();
                match self_clone.update_prolog_engine_facts().await {
                    Ok(()) => {
                        log::info!("🔧 PROLOG UPDATE: Full rebuild completed successfully in {:?}", rebuild_start.elapsed());
                        true
                    }
                    Err(e) => {
                        log::error!("Error while updating Prolog engine facts (took {:?}): {:?}", 
                            rebuild_start.elapsed(), e);
                        false
                    }
                }
            };

            if did_update {
                let pubsub_start = std::time::Instant::now();
                self_clone.pubsub_publish_diff(diff).await;
                log::info!("🔧 PROLOG UPDATE: Pubsub publish completed in {:?}", pubsub_start.elapsed());

                // Trigger notification and subscription checks after prolog facts are updated
                let trigger_start = std::time::Instant::now();
                *(self_clone.trigger_notification_check.lock().await) = true;
                *(self_clone.trigger_prolog_subscription_check.lock().await) = true;
                log::info!("🔧 PROLOG UPDATE: Triggers set in {:?}", trigger_start.elapsed());
            }

            log::info!("🔧 PROLOG UPDATE: Total prolog update task took {:?}", spawn_start.elapsed());

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
    ) -> Result<BTreeMap<Notification, Vec<QueryMatch>>, AnyError> {
        // Get UUID without holding lock during operations
        let uuid = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.uuid.clone()
        };

        let notifications = Self::all_notifications_for_perspective_id(uuid)?;
        let mut result_map = BTreeMap::new();
        for n in notifications {
            if let QueryResolution::Matches(matches) = self.prolog_query(n.trigger.clone()).await? {
                result_map.insert(n.clone(), matches);
            }
        }

        Ok(result_map)
    }

    async fn notification_trigger_snapshot(&self) -> BTreeMap<Notification, Vec<QueryMatch>> {
        self.calc_notification_trigger_matches()
            .await
            .unwrap_or_else(|e| {
                log::error!("Error trying to render notification matches: {:?}", e);
                BTreeMap::new()
            })
    }

    fn subtract_before_notification_matches(
        before: &BTreeMap<Notification, Vec<QueryMatch>>,
        after: &BTreeMap<Notification, Vec<QueryMatch>>,
    ) -> BTreeMap<Notification, Vec<QueryMatch>> {
        after
            .iter()
            .map(|(notification, matches)| {
                let new_matches: Vec<QueryMatch> =
                    if let Some(old_matches) = before.get(notification) {
                        matches
                            .iter()
                            .filter(|m| !old_matches.contains(m))
                            .cloned()
                            .collect()
                    } else {
                        matches.clone()
                    };

                (notification.clone(), new_matches)
            })
            .collect()
    }

    async fn publish_notification_matches(
        uuid: String,
        match_map: BTreeMap<Notification, Vec<QueryMatch>>,
    ) {
        for (notification, matches) in match_map {
            if !matches.is_empty() {
                let payload = TriggeredNotification {
                    notification: notification.clone(),
                    perspective_id: uuid.clone(),
                    trigger_match: prolog_resolution_to_string(QueryResolution::Matches(matches)),
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
        let uuid = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.uuid.clone()
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
            .update_perspective_links(uuid, "facts".to_string(), all_links, neighbourhood_author)
            .await?;
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
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
            link_language.others().await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn has_telepresence_adapter(&self) -> bool {
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
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
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
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
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
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
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
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
        if loopback {
            // send back to all clients through neighbourhood signal subscription
            let payload_clone = payload.clone();
            let self_clone = self.clone();
            tokio::spawn(async move {
                self_clone
                    .telepresence_signal_from_link_language(payload_clone)
                    .await;
            });
        }

        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
            link_language.send_broadcast(payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn execute_commands(
        &mut self,
        commands: Vec<Command>,
        expression: String,
        parameters: Vec<Parameter>,
        batch_id: Option<String>,
    ) -> Result<(), AnyError> {
        let execute_start = std::time::Instant::now();
        log::info!("⚙️ EXECUTE COMMANDS: Starting execution of {} commands for expression '{}', batch_id: {:?}", 
            commands.len(), expression, batch_id);

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

        for (i, command) in commands.iter().enumerate() {
            let command_start = std::time::Instant::now();
            log::info!("⚙️ EXECUTE COMMANDS: Processing command {}/{}: {:?}", i + 1, commands.len(), command.action);
            
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
                    )
                    .await?;
                }
                Action::RemoveLink => {
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source),
                            predicate,
                            target: if target == "*" { None } else { Some(target) },
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                }
                Action::SetSingleTarget => {
                    if predicate.is_none() {
                        log::error!(
                            "SetSingleTarget actions with no predicate are not allowed. Skipping."
                        );
                        continue;
                    }
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
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
                        batch_id.clone(),
                    )
                    .await?;
                }
                Action::CollectionSetter => {
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
                    )
                    .await?;
                }
            }
            
            log::info!("⚙️ EXECUTE COMMANDS: Command {} completed in {:?}", i + 1, command_start.elapsed());
        }

        log::info!("⚙️ EXECUTE COMMANDS: All {} commands executed in {:?}", commands.len(), execute_start.elapsed());
        Ok(())
    }

    async fn subject_class_option_to_class_name(
        &mut self,
        subject_class: SubjectClassOption,
    ) -> Result<String, AnyError> {
        let method_start = std::time::Instant::now();
        log::info!("🔍 SUBJECT CLASS: Starting class name resolution...");
        
        let result = Ok(if subject_class.class_name.is_some() {
            let class_name = subject_class.class_name.unwrap();
            log::info!("🔍 SUBJECT CLASS: Using provided class name '{}' in {:?}", class_name, method_start.elapsed());
            class_name
        } else {
            let query = subject_class.query.ok_or(anyhow!(
                "SubjectClassOption needs to either have `name` or `query` set"
            ))?;
            
            log::info!("🔍 SUBJECT CLASS: Running prolog query to resolve class name: {}", query);
            let query_start = std::time::Instant::now();
            
            let result = self.prolog_query(query.to_string()).await.map_err(|e| {
                log::error!("Error creating subject: {:?}", e);
                e
            })?;
            
            log::info!("🔍 SUBJECT CLASS: Prolog query completed in {:?}", query_start.elapsed());
            
            let class_name = prolog_get_first_string_binding(&result, "Class")
                .ok_or(anyhow!("No matching subject class found!"))?;
            
            log::info!("🔍 SUBJECT CLASS: Resolved class name '{}' in {:?}", class_name, method_start.elapsed());
            class_name
        });
        
        result
    }

    async fn get_actions_from_prolog(
        &self,
        query: String,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        let result = self.prolog_query(query).await?;

        if let Some(actions_str) = prolog_get_first_string_binding(&result, "Actions") {
            // json5 seems to have a bug, blocking when a property is set to undefined
            let sanitized_str = actions_str.replace("undefined", "null");
            json5::from_str(&sanitized_str)
                .map(Some)
                .map_err(|e| anyhow!("Failed to parse actions: {}", e))
        } else {
            Ok(None)
        }
    }

    async fn get_constructor_actions(&self, class_name: &str) -> Result<Vec<Command>, AnyError> {
        let method_start = std::time::Instant::now();
        log::info!("🏗️ CONSTRUCTOR: Getting constructor actions for class '{}'", class_name);
        
        let query = format!(
            r#"subject_class("{}", C), constructor(C, Actions)"#,
            class_name
        );
        
        log::info!("🏗️ CONSTRUCTOR: Running prolog query: {}", query);
        let query_start = std::time::Instant::now();
        
        let result = self.get_actions_from_prolog(query)
            .await?
            .ok_or(anyhow!("No constructor found for class: {}", class_name));
        
        log::info!("🏗️ CONSTRUCTOR: Prolog query completed in {:?} (total: {:?})", 
            query_start.elapsed(), method_start.elapsed());
        
        result
    }

    async fn get_property_setter_actions(
        &self,
        class_name: &str,
        property: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        let method_start = std::time::Instant::now();
        log::info!("🔧 PROPERTY SETTER: Getting setter for class '{}', property '{}'", class_name, property);
        
        let query = format!(
            r#"subject_class("{}", C), property_setter(C, "{}", Actions)"#,
            class_name, property
        );
        
        log::info!("🔧 PROPERTY SETTER: Running prolog query: {}", query);
        let query_start = std::time::Instant::now();
        
        let result = self.get_actions_from_prolog(query).await;
        
        log::info!("🔧 PROPERTY SETTER: Prolog query completed in {:?} (total: {:?})", 
            query_start.elapsed(), method_start.elapsed());
        
        result
    }

    async fn resolve_property_value(
        &self,
        class_name: &str,
        property: &str,
        value: &serde_json::Value,
    ) -> Result<String, AnyError> {
        let resolve_result = self.prolog_query(format!(
            r#"subject_class("{}", C), property_resolve(C, "{}"), property_resolve_language(C, "{}", Language)"#,
            class_name, property, property
        )).await?;

        if let Some(resolve_language) = prolog_get_first_string_binding(&resolve_result, "Language")
        {
            // Create an expression for the value
            let mut lock = crate::js_core::JS_CORE_HANDLE.lock().await;
            let content = serde_json::to_string(value)
                .map_err(|e| anyhow!("Failed to serialize JSON value: {}", e))?;
            if let Some(ref mut js) = *lock {
                let result = js.execute(format!(
                    r#"JSON.stringify(
                        (await core.callResolver("Mutation", "expressionCreate", {{ languageAddress: "{}", content: {} }})).Ok
                    )"#,
                    resolve_language, content
                )).await?;
                Ok(result.trim_matches('"').to_string())
            } else {
                Ok(value.to_string())
            }
        } else {
            Ok(match value {
                serde_json::Value::String(s) => s.clone(),
                _ => value.to_string(),
            })
        }
    }

    pub async fn create_subject(
        &mut self,
        subject_class: SubjectClassOption,
        expression_address: String,
        initial_values: Option<serde_json::Value>,
        batch_id: Option<String>,
    ) -> Result<(), AnyError> {
        let create_start = std::time::Instant::now();
        log::info!("🎯 CREATE SUBJECT: Starting create_subject for expression '{}' - batch_id: {:?}", 
            expression_address, batch_id);
        
        let class_name_start = std::time::Instant::now();
        let class_name = self
            .subject_class_option_to_class_name(subject_class)
            .await?;
        log::info!("🎯 CREATE SUBJECT: Got class name '{}' in {:?}", class_name, class_name_start.elapsed());

        let constructor_start = std::time::Instant::now();
        let mut commands = self.get_constructor_actions(&class_name).await?;
        log::info!("🎯 CREATE SUBJECT: Got {} constructor actions in {:?}", 
            commands.len(), constructor_start.elapsed());

        // Handle initial values if provided
        if let Some(obj) = initial_values {
            let initial_values_start = std::time::Instant::now();
            log::info!("🎯 CREATE SUBJECT: Processing initial values...");
            
            if let serde_json::Value::Object(obj) = obj {
                for (prop, value) in obj.iter() {
                    let prop_start = std::time::Instant::now();
                    if let Some(setter_commands) =
                        self.get_property_setter_actions(&class_name, prop).await?
                    {
                        let target_value = self
                            .resolve_property_value(&class_name, prop, value)
                            .await?;
                        
                        log::info!("🎯 CREATE SUBJECT: Property '{}' setter resolved in {:?}", 
                            prop, prop_start.elapsed());

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

        let execute_start = std::time::Instant::now();
        log::info!("🎯 CREATE SUBJECT: Executing {} commands...", commands.len());
        // Execute the merged commands
        self.execute_commands(
            commands,
            expression_address.clone(),
            vec![],
            batch_id.clone(),
        )
        .await?;
    
        log::info!("🎯 CREATE SUBJECT: Commands executed in {:?}", execute_start.elapsed());
        log::info!("🎯 CREATE SUBJECT: Total create_subject took {:?}", create_start.elapsed());

        if batch_id.is_some() {
            return Ok(());
        }

        // Verify instance was created successfully
        let mut tries = 0;
        let mut instance_check_passed = false;
        while !instance_check_passed && tries < 50 {
            match self
                .prolog_query(format!(
                    "subject_class(\"{}\", C), instance(C, \"{}\").",
                    class_name, expression_address
                ))
                .await
            {
                Ok(QueryResolution::True) => instance_check_passed = true,
                Ok(QueryResolution::Matches(_)) => instance_check_passed = true,
                Err(e) => log::warn!("Error trying to check instance after create_subject: {}", e),
                Ok(_) => {} //log::info!("create_subject instance query returned: {:?}", r),
            }
            sleep(Duration::from_millis(10)).await;
            tries += 1;
        }

        if instance_check_passed {
            // log::info!(
            //     "Subject class \"{}\" successfully instantiated around \"{}\".",
            //     class_name,
            //     expression_address
            // );
        } else {
            log::warn!("create_subject: instance check still false after running constructor and waiting 5s. Something seems off with subject class: {}", class_name);
        }

        Ok(())
    }

    pub async fn get_subject_data(
        &mut self,
        subject_class: SubjectClassOption,
        base_expression: String,
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
            .subject_class_option_to_class_name(subject_class)
            .await?;
        let result = self
            .prolog_query(format!(
                "subject_class(\"{}\", C), instance(C, \"{}\").",
                class_name, base_expression
            ))
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
            .prolog_query(format!(
                r#"subject_class("{}", C), property(C, Property)."#,
                class_name
            ))
            .await?;
        let properties: Vec<String> =
            prolog_get_all_string_bindings(&properties_result, "Property");

        for p in &properties {
            let property_values_result = self
                .prolog_query(format!(
                    r#"subject_class("{}", C), property_getter(C, "{}", "{}", Value)"#,
                    class_name, base_expression, p
                ))
                .await?;
            if let Some(property_value) = prolog_get_first_binding(&property_values_result, "Value")
            {
                let result = self
                    .prolog_query(format!(
                        r#"subject_class("{}", C), property_resolve(C, "{}")"#,
                        class_name, p
                    ))
                    .await?;
                //println!("resolve query result for {}: {:?}", p, result);
                let resolve_expression_uri = QueryResolution::False != result;
                //println!("resolve_expression_uri for {}: {:?}", p, resolve_expression_uri);
                let value = if resolve_expression_uri {
                    match &property_value {
                        scryer_prolog::Term::String(s) => {
                            //println!("getting expr url: {}", s);
                            let mut lock = crate::js_core::JS_CORE_HANDLE.lock().await;

                            if let Some(ref mut js) = *lock {
                                let result = js.execute(format!(
                                        r#"JSON.stringify(await core.callResolver("Query", "expression", {{ url: "{}" }}))"#,
                                        s
                                    ))
                                    .await?;

                                let result: JsResultType<Option<ExpressionRendered>> =
                                    serde_json::from_str(&result)?;

                                match result {
                                    JsResultType::Ok(Some(expr)) => expr.data,
                                    JsResultType::Ok(None) | JsResultType::Error(_) => {
                                        prolog_value_to_json_string(property_value.clone())
                                    }
                                }
                            } else {
                                prolog_value_to_json_string(property_value.clone())
                            }
                        }
                        _x => {
                            //println!("Couldn't get expression subjectentity: {:?}", x);
                            prolog_value_to_json_string(property_value.clone())
                        }
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
            .prolog_query(format!(
                r#"subject_class("{}", C), collection(C, Collection)"#,
                class_name
            ))
            .await?;
        let collections: Vec<String> =
            prolog_get_all_string_bindings(&collections_results, "Collection");

        for c in collections {
            let collection_values_result = self
                .prolog_query(format!(
                    r#"subject_class("{}", C), collection_getter(C, "{}", "{}", Value)"#,
                    class_name, base_expression, c
                ))
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
                    &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC.to_string(),
                    &serde_json::to_string(&filter).unwrap(),
                )
                .await;
        });
    }

    pub async fn subscribe_and_query(&self, query: String) -> Result<(String, String), AnyError> {
        // Check if we already have a subscription with the same query
        let existing_subscription = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .find(|(_, q)| q.query == query)
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

        // Execute prolog query without holding any locks
        let initial_result = self.prolog_query_subscription(query.clone()).await?;
        let result_string = prolog_resolution_to_string(initial_result);

        let subscribed_query = SubscribedQuery {
            query,
            last_result: result_string.clone(),
            last_keepalive: Instant::now(),
        };

        // Now insert the subscription
        self.subscribed_queries
            .lock()
            .await
            .insert(subscription_id.clone(), subscribed_query);

        // Send initial result after 3 delays
        let result_string = format!("#init#{}", result_string);
        for delay in [100, 500, 1000, 10000, 15000, 20000, 25000] {
            self.send_subscription_update(
                subscription_id.clone(),
                result_string.clone(),
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
        let mut queries = self.subscribed_queries.lock().await;
        Ok(queries.remove(&subscription_id).is_some())
    }

    async fn check_subscribed_queries(&self) {
        let mut queries_to_remove = Vec::new();
        let mut query_futures = Vec::new();
        let now = Instant::now();

        // First collect all the queries and their IDs
        let queries = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .map(|(id, query)| (id.clone(), query.clone()))
                .collect::<Vec<_>>()
        };

        // Create futures for each query check
        for (id, query) in queries {
            // Check for timeout
            if now.duration_since(query.last_keepalive).as_secs() > QUERY_SUBSCRIPTION_TIMEOUT {
                queries_to_remove.push(id);
                continue;
            }

            // Spawn query check future
            let self_clone = self.clone();
            let query_future = async move {
                //let this_now = Instant::now();
                if let Ok(result) = self_clone.prolog_query_subscription(query.query.clone()).await {
                    let result_string = prolog_resolution_to_string(result);
                    if result_string != query.last_result {
                        //log::info!("Query {} has changed: {}", id, result_string);
                        // Update the query result and send notification immediately
                        {
                            self_clone
                                .send_subscription_update(id.clone(), result_string.clone(), None)
                                .await;
                            let mut queries = self_clone.subscribed_queries.lock().await;
                            if let Some(stored_query) = queries.get_mut(&id) {
                                stored_query.last_result = result_string.clone();
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

        // Remove timed out queries
        if !queries_to_remove.is_empty() {
            let mut queries = self.subscribed_queries.lock().await;
            for id in queries_to_remove {
                queries.remove(&id);
            }
        }
    }

    async fn subscribed_queries_loop(&self) {
        while !*self.is_teardown.lock().await {
            // Check trigger without holding lock during the operation
            let should_check = {
                let trigger_check = *self.trigger_prolog_subscription_check.lock().await;
                trigger_check
            };

            if should_check {
                self.check_subscribed_queries().await;
                *self.trigger_prolog_subscription_check.lock().await = false;
            }
            sleep(Duration::from_millis(QUERY_SUBSCRIPTION_CHECK_INTERVAL)).await;
        }
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
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        let commit_start = std::time::Instant::now();
        log::info!("🔄 BATCH COMMIT: Starting batch commit for batch_uuid: {}", batch_uuid);
        
        // Get the diff without holding lock during the entire operation
        let diff = {
            let batch_retrieval_start = std::time::Instant::now();
            let mut batch_store = self.batch_store.write().await;
            let result = match batch_store.remove(&batch_uuid) {
                Some(diff) => diff,
                None => return Err(anyhow!("No batch found with given UUID")),
            };
            log::info!("🔄 BATCH COMMIT: Retrieved batch diff in {:?} - {} additions, {} removals", 
                batch_retrieval_start.elapsed(), result.additions.len(), result.removals.len());
            result
        };

        let processing_start = std::time::Instant::now();
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
            let signed_expr = create_signed_expression(link.data)?;
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

        log::info!("🔄 BATCH COMMIT: Link processing took {:?} - shared: {} add/{} rem, local: {} add/{} rem", 
            processing_start.elapsed(),
            shared_diff.additions.len(), shared_diff.removals.len(),
            local_diff.additions.len(), local_diff.removals.len());

        // Get UUID without holding lock during DB operations
        let uuid = {
            let handle = self.persisted.lock().await;
            handle.uuid.clone()
        };

        // Apply shared changes
        if !shared_diff.additions.is_empty() || !shared_diff.removals.is_empty() {
            let db_start = std::time::Instant::now();
            log::info!("🔄 BATCH COMMIT: Starting DB operations for shared changes");
            
            // Add shared links to storage
            for link in &shared_diff.additions {
                Ad4mDb::with_global_instance(|db| {
                    db.add_link(&uuid, &link.clone().into(), &LinkStatus::Shared)
                })?;
            }

            // Remove shared links from storage
            for link in &shared_diff.removals {
                Ad4mDb::with_global_instance(|db| db.remove_link(&uuid, &link.clone().into()))?;
            }
            
            log::info!("🔄 BATCH COMMIT: DB operations for shared changes took {:?}", db_start.elapsed());

            // Commit to link language
            if self.has_link_language().await {
                let link_lang_start = std::time::Instant::now();
                log::info!("🔄 BATCH COMMIT: Starting link language commit");
                
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
                
                log::info!("🔄 BATCH COMMIT: Link language commit spawned in {:?}", link_lang_start.elapsed());
            }
        }

        // Apply local changes
        if !local_diff.additions.is_empty() || !local_diff.removals.is_empty() {
            let local_db_start = std::time::Instant::now();
            log::info!("🔄 BATCH COMMIT: Starting DB operations for local changes");
            
            // Add local links to storage
            for link in &local_diff.additions {
                Ad4mDb::with_global_instance(|db| {
                    db.add_link(&uuid, &link.clone().into(), &LinkStatus::Local)
                })?;
            }

            // Remove local links from storage
            for link in &local_diff.removals {
                Ad4mDb::with_global_instance(|db| db.remove_link(&uuid, &link.clone().into()))?;
            }
            
            log::info!("🔄 BATCH COMMIT: DB operations for local changes took {:?}", local_db_start.elapsed());
        }

        // Create combined diff for prolog update and return value
        let combined_diff = DecoratedPerspectiveDiff {
            additions: [shared_diff.additions.clone(), local_diff.additions.clone()].concat(),
            removals: [shared_diff.removals.clone(), local_diff.removals.clone()].concat(),
        };

        // Only spawn prolog facts update if there are changes to update
        if !combined_diff.additions.is_empty() || !combined_diff.removals.is_empty() {
            let prolog_start = std::time::Instant::now();
            log::info!("🔄 BATCH COMMIT: Starting prolog facts update - {} add, {} rem", 
                combined_diff.additions.len(), combined_diff.removals.len());
            
            // Create oneshot channel for prolog facts update completion
            let (completion_sender, completion_receiver) = tokio::sync::oneshot::channel();
            
            // Update prolog facts once for all changes and wait for completion
            self.spawn_prolog_facts_update(combined_diff.clone(), Some(completion_sender));
            let _ = completion_receiver.await;
            
            log::info!("🔄 BATCH COMMIT: Prolog facts update completed in {:?}", prolog_start.elapsed());
        }
        
        log::info!("🔄 BATCH COMMIT: Total batch commit took {:?}", commit_start.elapsed());
        
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
    use crate::db::Ad4mDb;
    use crate::graphql::graphql_types::PerspectiveState;
    use crate::perspectives::perspective_instance::PerspectiveHandle;
    use crate::test_utils::setup_wallet;
    use fake::{Fake, Faker};
    use uuid::Uuid;

    fn setup() -> PerspectiveInstance {
        setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();

        PerspectiveInstance::new(
            PerspectiveHandle {
                uuid: Uuid::new_v4().to_string(),
                name: Some("Test Perspective".to_string()),
                shared_url: None,
                neighbourhood: None,
                state: PerspectiveState::Private,
            },
            None,
        )
    }

    pub fn create_link() -> Link {
        Link {
            source: format!("https://{}.com", Faker.fake::<String>()),
            target: format!("https://{}.org", Faker.fake::<String>()),
            predicate: Some(format!("https://{}.net", Faker.fake::<String>())),
        }
    }

    #[tokio::test]
    async fn test_get_all_links_after_adding_five() {
        let mut perspective = setup();
        let mut all_links = Vec::new();

        for _ in 0..5 {
            let link = create_link();
            let expression = perspective
                .add_link(link.clone(), LinkStatus::Local, None)
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 5);
        assert_eq!(links, all_links);
    }

    #[tokio::test]
    async fn test_get_links_by_source() {
        let mut perspective = setup();
        let mut all_links = Vec::new();
        let source = "ad4m://self";

        for i in 0..5 {
            let mut link = create_link();
            if i % 2 == 0 {
                link.source = source.to_string();
            }

            let expression = perspective
                .add_link(link.clone(), LinkStatus::Shared, None)
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery {
            source: Some(source.to_string()),
            ..Default::default()
        };
        let links = perspective.get_links(&query).await.unwrap();
        let expected_links: Vec<_> = all_links
            .into_iter()
            .filter(|expr| expr.data.source == source)
            .collect();
        assert_eq!(links.len(), expected_links.len());
        assert_eq!(links, expected_links);
    }

    #[tokio::test]
    async fn test_remove_link() {
        let mut perspective = setup();
        let link = create_link();
        let status = LinkStatus::Local;

        // Add a link to the perspective
        let expression = perspective
            .add_link(link.clone(), status, None)
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
        let mut perspective = setup();
        let mut all_links = Vec::new();
        let now = chrono::Utc::now();

        // Add links with timestamps spread out by one minute intervals
        for i in 0..5 {
            let mut link = create_link();
            link.target = format!("lang://test-target {}", i);
            let mut link = create_signed_expression(link).expect("Failed to create link");
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
        let mut perspective = setup();
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        perspective
            .add_link(link.clone(), LinkStatus::Shared, Some(batch_id.clone()))
            .await
            .unwrap();

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 0);

        // Commit the batch
        let diff = perspective.commit_batch(batch_id).await.unwrap();
        assert_eq!(diff.additions.len(), 1);

        // Verify links are now in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
    }

    #[tokio::test]
    async fn test_batch_update_and_remove() {
        let mut perspective = setup();
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        // Add initial link
        perspective
            .add_link(link.clone(), LinkStatus::Shared, None)
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
            )
            .await
            .unwrap();

        // Commit the batch
        perspective.commit_batch(batch_id).await.unwrap();

        // Verify final state in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.target, new_link.target);
    }

    #[tokio::test]
    async fn test_batch_multiple_operations() {
        let mut perspective = setup();

        // one link outside the batch, for removal
        let link0 = create_link();
        let link0_expression = perspective
            .add_link(link0.clone(), LinkStatus::Shared, None)
            .await
            .unwrap();

        // two links in the batch
        let link1 = create_link();
        let mut link2 = link1.clone();
        link2.target = "target2".to_string();

        let batch_id = perspective.create_batch().await;

        // Add two links in batch
        perspective
            .add_link(link1.clone(), LinkStatus::Shared, Some(batch_id.clone()))
            .await
            .unwrap();
        perspective
            .add_link(link2.clone(), LinkStatus::Shared, Some(batch_id.clone()))
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
        let diff = perspective.commit_batch(batch_id).await.unwrap();
        assert_eq!(diff.additions.len(), 2); // link1 and link2
        assert_eq!(diff.removals.len(), 1); // link1

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    #[tokio::test]
    async fn test_batch_error_handling() {
        let mut perspective = setup();

        // Try to commit non-existent batch
        let result = perspective.commit_batch("non-existent".to_string()).await;
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
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_batch_with_execute_commands() {
        let mut perspective = setup();
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
        let diff = perspective.commit_batch(batch_id).await.unwrap();
        assert_eq!(diff.additions.len(), 2);
        assert_eq!(diff.removals.len(), 0);

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    // #[tokio::test]
    // async fn test_batch_with_create_subject() {
    //     let mut perspective = setup();
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
}
