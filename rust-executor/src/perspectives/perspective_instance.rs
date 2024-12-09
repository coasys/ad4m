use super::sdna::{generic_link_fact, init_engine_facts, is_sdna_link};
use super::update_perspective;
use super::utils::{
    prolog_get_all_string_bindings, prolog_get_first_string_binding, prolog_resolution_to_string,
};
use crate::agent::{self, create_signed_expression};
use crate::graphql::graphql_types::{
    DecoratedPerspectiveDiff, ExpressionRendered, JsResultType, LinkMutations, LinkQuery,
    LinkStatus, NeighbourhoodSignalFilter, OnlineAgent, PerspectiveExpression, PerspectiveHandle,
    PerspectiveLinkFilter, PerspectiveLinkUpdatedFilter, PerspectiveState, PerspectiveStateFilter,
};
use crate::languages::language::Language;
use crate::languages::LanguageController;
use crate::perspectives::utils::{prolog_get_first_binding, prolog_value_to_json_string};
use crate::prolog_service::engine::PrologEngine;
use crate::pubsub::{
    get_global_pubsub, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_LINK_ADDED_TOPIC,
    PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC,
    PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC, RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
};
use crate::{db::Ad4mDb, types::*};
use ad4m_client::literal::Literal;
use chrono::DateTime;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use json5;
use scryer_prolog::machine::parsed_results::{QueryMatch, QueryResolution};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};
use tokio::time::sleep;
use tokio::{join, time};

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
pub struct PerspectiveInstance {
    pub persisted: Arc<Mutex<PerspectiveHandle>>,

    pub created_from_join: bool,
    pub is_fast_polling: bool,
    pub retries: u32,

    prolog_engine: Arc<Mutex<Option<PrologEngine>>>,
    prolog_needs_rebuild: Arc<Mutex<bool>>,
    is_teardown: Arc<Mutex<bool>>,
    sdna_change_mutex: Arc<Mutex<()>>,
    prolog_update_mutex: Arc<RwLock<()>>,
    link_language: Arc<Mutex<Option<Language>>>,
    links_have_changed: Arc<Mutex<bool>>,
    commit_debounce_timer: Arc<Mutex<Option<tokio::time::Instant>>>,
    immediate_commits_remaining: Arc<Mutex<u32>>,
}

impl PerspectiveInstance {
    pub fn new(handle: PerspectiveHandle, created_from_join: Option<bool>) -> Self {
        PerspectiveInstance {
            persisted: Arc::new(Mutex::new(handle.clone())),

            created_from_join: created_from_join.unwrap_or(false),
            is_fast_polling: false,
            retries: 0,
            prolog_engine: Arc::new(Mutex::new(None)),
            prolog_needs_rebuild: Arc::new(Mutex::new(true)),
            is_teardown: Arc::new(Mutex::new(false)),
            sdna_change_mutex: Arc::new(Mutex::new(())),
            prolog_update_mutex: Arc::new(RwLock::new(())),
            link_language: Arc::new(Mutex::new(None)),
            links_have_changed: Arc::new(Mutex::new(false)),
            commit_debounce_timer: Arc::new(Mutex::new(None)),
            immediate_commits_remaining: Arc::new(Mutex::new(3)), // Default to 3 immediate commits
        }
    }

    pub async fn start_background_tasks(self) {
        let _ = join!(
            self.ensure_link_language(),
            self.notification_check_loop(),
            self.nh_sync_loop(),
            self.pending_diffs_loop(),
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
        let mut interval = time::interval(Duration::from_secs(1));
        let mut last_diff_time = None;

        while !*self.is_teardown.lock().await {
            interval.tick().await;

            if self.has_link_language().await {
                let (_, ids) = Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&uuid))
                    .unwrap_or((PerspectiveDiff::empty(), Vec::new()));

                if ids.len() == 0 {
                    continue;
                }

                if last_diff_time.is_none() {
                    // First diff in a burst - start timer
                    last_diff_time = Some(tokio::time::Instant::now());
                }

                // Commit if either:
                // 1. It's been 10s since first diff in burst (don't collect longer than 10s)
                if last_diff_time.unwrap().elapsed() >= Duration::from_secs(10) {
                    if let Ok(_) = self.commit_pending_diffs().await {
                        last_diff_time = None;
                        log::info!("Committed diffs after reaching 10s maximum wait time");
                    }
                // 2. It's been > 1s since last new diff (burst is over)
                } else if !self.has_new_diffs_in_last_second().await {
                    if let Ok(_) = self.commit_pending_diffs().await {
                        last_diff_time = None;
                        log::info!("Committed diffs after 1s of inactivity");
                    }
                // 3. We have collected more than 100 diffs
                } else if ids.len() > 100 {
                    if let Ok(_) = self.commit_pending_diffs().await {
                        last_diff_time = None;
                        log::info!("Committed diffs after collecting 100");
                    }
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

        let (pending_diffs, pending_ids) =
            Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&uuid))?;

        if !pending_ids.is_empty() {
            let mut link_language_lock = self.link_language.lock().await;
            if let Some(link_language) = link_language_lock.as_mut() {
                let commit_result = link_language.commit(pending_diffs).await;
                match commit_result {
                    Ok(Some(_)) => {
                        Ad4mDb::with_global_instance(|db| {
                            db.clear_pending_diffs(&uuid, pending_ids)
                        })?;
                        // Reset immediate commits counter after successful commit
                        let mut immediate_commits_remaining =
                            self.immediate_commits_remaining.lock().await;
                        *immediate_commits_remaining = 3; // Or whatever the default should be
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
            let mut changed = self.links_have_changed.lock().await;
            if *changed {
                let after = self.notification_trigger_snapshot().await;
                let new_matches = Self::subtract_before_notification_matches(&before, &after);
                tokio::spawn(Self::publish_notification_matches(
                    uuid.clone(),
                    new_matches,
                ));
                before = after;
                *changed = false;
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

    pub async fn commit(&self, diff: &PerspectiveDiff) -> Result<Option<String>, AnyError> {
        let handle = self.persisted.lock().await.clone();
        if handle.neighbourhood.is_none() {
            return Ok(None);
        }

        let mut can_commit = false;
        if !self.created_from_join {
            can_commit = true;
        } else if let Some(link_language) = self.link_language.lock().await.as_mut() {
            if link_language.current_revision().await?.is_some() {
                can_commit = true;
            }
        };

        if !can_commit {
            return Err(anyhow!(
                "Cannot commit diff. Not yet synced with neighbourhood..."
            ));
        }

        let mut immediate_commits = self.immediate_commits_remaining.lock().await;

        if *immediate_commits > 0 {
            // Process immediate commit
            *immediate_commits -= 1;
            if let Some(link_language) = self.link_language.lock().await.as_mut() {
                return link_language.commit(diff.clone()).await;
            }
        }

        // Store diff in DB
        Ad4mDb::with_global_instance(|db| db.add_pending_diff(&handle.uuid, diff))?;
        // Update or start timer
        let mut timer = self.commit_debounce_timer.lock().await;
        *timer = Some(tokio::time::Instant::now());

        Ok(None)
    }

    // Add method to configure immediate commits
    pub async fn set_immediate_commits(&self, count: u32) {
        *self.immediate_commits_remaining.lock().await = count;
    }

    fn spawn_commit_and_handle_error(&self, diff: &PerspectiveDiff) {
        let self_clone = self.clone();
        let diff_clone = diff.clone();

        tokio::spawn(async move {
            if (self_clone.commit(&diff_clone).await).is_err() {
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

        self.spawn_prolog_facts_update(decorated_diff.clone());
        self.pubsub_publish_diff(decorated_diff).await;
        *(self.links_have_changed.lock().await) = true;
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
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let link_expression = create_signed_expression(link)?;

        self.add_link_expression(link_expression.into(), status)
            .await
    }

    async fn pubsub_publish_diff(&self, decorated_diff: DecoratedPerspectiveDiff) {
        let handle = self.persisted.lock().await.clone();

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
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        Ad4mDb::with_global_instance(|db| db.add_link(&handle.uuid, &link_expression, &status))?;

        let diff = PerspectiveDiff::from_additions(vec![link_expression.clone()]);
        let decorated_link_expression =
            DecoratedLinkExpression::from((link_expression.clone(), status.clone()));
        let decorated_perspective_diff =
            DecoratedPerspectiveDiff::from_additions(vec![decorated_link_expression.clone()]);

        self.spawn_prolog_facts_update(decorated_perspective_diff.clone());

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
        }

        self.pubsub_publish_diff(decorated_perspective_diff).await;
        *(self.links_have_changed.lock().await) = true;
        Ok(decorated_link_expression)
    }

    pub async fn add_links(
        &mut self,
        links: Vec<Link>,
        status: LinkStatus,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let uuid = handle.uuid.clone();
        let link_expressions = links
            .into_iter()
            .map(|l| create_signed_expression(l).map(LinkExpression::from))
            .collect::<Result<Vec<LinkExpression>, AnyError>>();

        let link_expressions = link_expressions?;
        let decorated_link_expressions = link_expressions
            .clone()
            .into_iter()
            .map(|l| DecoratedLinkExpression::from((l, status.clone())))
            .collect::<Vec<DecoratedLinkExpression>>();

        let perspective_diff = PerspectiveDiff::from_additions(link_expressions.clone());
        let decorated_perspective_diff =
            DecoratedPerspectiveDiff::from_additions(decorated_link_expressions.clone());

        Ad4mDb::with_global_instance(|db| {
            db.add_many_links(&uuid, link_expressions.clone(), &status)
        })?;

        self.spawn_prolog_facts_update(decorated_perspective_diff.clone());
        self.pubsub_publish_diff(decorated_perspective_diff).await;
        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&perspective_diff);
        }
        *(self.links_have_changed.lock().await) = true;
        Ok(decorated_link_expressions)
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

        self.spawn_prolog_facts_update(decorated_diff.clone());
        self.pubsub_publish_diff(decorated_diff.clone()).await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
        }
        *(self.links_have_changed.lock().await) = true;
        Ok(decorated_diff)
    }

    pub async fn update_link(
        &mut self,
        old_link: LinkExpression,
        new_link: Link,
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

        Ad4mDb::with_global_instance(|db| {
            db.update_link(&handle.uuid, &link, &new_link_expression)
        })?;

        let diff = PerspectiveDiff::from(vec![new_link_expression.clone()], vec![old_link.clone()]);
        let decorated_new_link_expression =
            DecoratedLinkExpression::from((new_link_expression.clone(), link_status.clone()));
        let decorated_old_link =
            DecoratedLinkExpression::from((old_link.clone(), link_status.clone()));
        let decorated_diff = DecoratedPerspectiveDiff::from(
            vec![decorated_new_link_expression.clone()],
            vec![decorated_old_link.clone()],
        );

        self.spawn_prolog_facts_update(decorated_diff);

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
        *(self.links_have_changed.lock().await) = true;
        Ok(decorated_new_link_expression)
    }

    pub async fn remove_link(
        &mut self,
        link_expression: LinkExpression,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        if let Some((link_from_db, status)) =
            Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &link_expression))?
        {
            Ad4mDb::with_global_instance(|db| db.remove_link(&handle.uuid, &link_expression))?;
            let diff = PerspectiveDiff::from_removals(vec![link_expression.clone()]);
            let decorated_link = DecoratedLinkExpression::from((link_from_db, status.clone()));
            let decorated_diff =
                DecoratedPerspectiveDiff::from_removals(vec![decorated_link.clone()]);

            self.spawn_prolog_facts_update(decorated_diff.clone());
            self.pubsub_publish_diff(decorated_diff.clone()).await;

            if status == LinkStatus::Shared {
                self.spawn_commit_and_handle_error(&diff);
            }

            *(self.links_have_changed.lock().await) = true;
            Ok(decorated_link)
        } else {
            Err(anyhow!("Link not found"))
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

            self.add_links(sdna_links, LinkStatus::Shared).await?;
            added = true;
        }
        // Mutex guard is automatically dropped here
        Ok(added)
    }

    async fn ensure_prolog_engine(&self) -> Result<(), AnyError> {
        let has_prolog_engine = { self.prolog_engine.lock().await.is_some() };

        let mut rebuild_flag = self.prolog_needs_rebuild.lock().await;

        if !has_prolog_engine || *rebuild_flag {
            let _update_lock = self.prolog_update_mutex.write().await;
            let mut maybe_prolog_engine = self.prolog_engine.lock().await;
            if *rebuild_flag && maybe_prolog_engine.is_some() {
                let old_engine = maybe_prolog_engine.as_ref().unwrap();
                let _ = old_engine.drop();
                *rebuild_flag = false;
            }

            let mut engine = PrologEngine::new();
            engine
                .spawn()
                .await
                .map_err(|e| anyhow!("Failed to spawn Prolog engine: {}", e))?;
            let all_links = self.get_links(&LinkQuery::default()).await?;
            let facts = init_engine_facts(
                all_links,
                self.persisted
                    .lock()
                    .await
                    .neighbourhood
                    .as_ref()
                    .map(|n| n.author.clone()),
            )
            .await?;
            engine
                .load_module_string("facts".to_string(), facts)
                .await?;
            *maybe_prolog_engine = Some(engine);
        }

        Ok(())
    }

    /// Executes a Prolog query against the engine, spawning and initializing the engine if necessary.
    pub async fn prolog_query(&self, query: String) -> Result<QueryResolution, AnyError> {
        self.ensure_prolog_engine().await?;

        let _read_lock = self.prolog_update_mutex.read().await;
        let prolog_engine_mutex = self.prolog_engine.lock().await;
        let prolog_engine_option_ref = prolog_engine_mutex.as_ref();
        let prolog_engine = prolog_engine_option_ref
            .as_ref()
            .expect("Must be some since we initialized the engine above");

        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        let result = prolog_engine.run_query(query).await?;

        match result {
            Err(e) => {
                let mut flag = self.prolog_needs_rebuild.lock().await;
                *flag = true;
                Err(anyhow!(e))
            }
            Ok(resolution) => Ok(resolution),
        }
    }

    fn spawn_prolog_facts_update(&self, diff: DecoratedPerspectiveDiff) {
        let self_clone = self.clone();

        tokio::spawn(async move {
            if let Err(e) = self_clone.ensure_prolog_engine().await {
                log::error!("Error spawning Prolog engine: {:?}", e)
            };

            let fact_rebuild_needed = !diff.removals.is_empty()
                || diff.additions.iter().any(|link| is_sdna_link(&link.data));

            let did_update = if !fact_rebuild_needed {
                let mut assertions: Vec<String> = Vec::new();
                for addition in &diff.additions {
                    assertions.push(generic_link_fact("assert_link_and_triple", addition));
                }

                let query = format!("{}.", assertions.join(","));
                match self_clone.prolog_query(query).await {
                    Ok(QueryResolution::True) => true,
                    Err(e) => {
                        log::error!(
                            "Error while running assertion query to updating Prolog engine facts: {:?}", e
                        );
                        false
                    }
                    other => {
                        log::error!(
                            "Error getting non-true result from assertion query while updating Prolog engine facts: {:?}", other
                        );
                        false
                    }
                }
            } else {
                match self_clone.update_prolog_engine_facts().await {
                    Ok(()) => true,
                    Err(e) => {
                        log::error!("Error while updating Prolog engine facts: {:?}", e);
                        false
                    }
                }
            };

            if did_update {
                self_clone.pubsub_publish_diff(diff).await;
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
        let uuid = self.persisted.lock().await.uuid.clone();
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
        let prolog_engine_mutex = self.prolog_engine.lock().await;
        let prolog_engine_option_ref = prolog_engine_mutex.as_ref();
        let prolog_engine = prolog_engine_option_ref
            .as_ref()
            .expect("Must be some since we initialized the engine above");
        let all_links = self.get_links(&LinkQuery::default()).await?;
        let facts = init_engine_facts(
            all_links,
            self.persisted
                .lock()
                .await
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone()),
        )
        .await?;
        prolog_engine
            .load_module_string("facts".to_string(), facts)
            .await?;

        Ok(())
    }

    async fn no_link_language_error(&self) -> AnyError {
        let handle = self.persisted.lock().await.clone();
        anyhow!(
            "Perspective {} has no link language installed. State is: {:?}",
            handle.uuid,
            handle.state
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

    pub async fn send_broadcast(&self, payload: PerspectiveExpression) -> Result<(), AnyError> {
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
    ) -> Result<(), AnyError> {
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

        for command in commands {
            let source = replace_this(replace_parameters(command.source))
                .ok_or_else(|| anyhow!("Source cannot be None"))?;
            let predicate = replace_this(replace_parameters(command.predicate));
            let target = (replace_parameters(command.target))
                .ok_or_else(|| anyhow!("Source cannot be None"))?;
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
                    )
                    .await?;
                }
                Action::RemoveLink => {
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source),
                            predicate,
                            target: Some(target),
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into()).await?;
                    }
                }
                Action::SetSingleTarget => {
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
                        self.remove_link(link_expression.into()).await?;
                    }
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
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
                        self.remove_link(link_expression.into()).await?;
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
                    )
                    .await?;
                }
            }
        }

        Ok(())
    }

    async fn subject_class_option_to_class_name(
        &mut self,
        subject_class: SubjectClassOption,
    ) -> Result<String, AnyError> {
        Ok(if subject_class.class_name.is_some() {
            subject_class.class_name.unwrap()
        } else {
            let query = subject_class.query.ok_or(anyhow!(
                "SubjectClassOption needs to either have `name` or `query` set"
            ))?;
            let result = self.prolog_query(query.to_string()).await.map_err(|e| {
                log::error!("Error creating subject: {:?}", e);
                e
            })?;
            prolog_get_first_string_binding(&result, "Class")
                .ok_or(anyhow!("No matching subject class found!"))?
        })
    }

    pub async fn create_subject(
        &mut self,
        subject_class: SubjectClassOption,
        expression_address: String,
    ) -> Result<(), AnyError> {
        let class_name = self
            .subject_class_option_to_class_name(subject_class)
            .await?;
        let result = self
            .prolog_query(format!(
                "subject_class(\"{}\", C), constructor(C, Actions).",
                class_name
            ))
            .await?;
        let actions = prolog_get_first_string_binding(&result, "Actions")
            .ok_or(anyhow!("No constructor found for class: {}", class_name))?;

        let commands: Vec<Command> = json5::from_str(&actions).unwrap();
        self.execute_commands(commands, expression_address.clone(), vec![])
            .await?;

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
                Ok(r) => log::info!("create_subject instance query returned: {:?}", r),
            }
            sleep(Duration::from_millis(10)).await;
            tries += 1;
        }

        if instance_check_passed {
            log::info!(
                "Subject class \"{}\" successfully instantiated around \"{}\".",
                class_name,
                expression_address
            );
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
                        scryer_prolog::machine::parsed_results::Value::String(s) => {
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
                .add_link(link.clone(), LinkStatus::Local)
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
                .add_link(link.clone(), LinkStatus::Shared)
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
        let expression = perspective.add_link(link.clone(), status).await.unwrap();

        // Ensure the link is present
        let query = LinkQuery::default();
        let links_before_removal = perspective.get_links(&query).await.unwrap();
        assert!(links_before_removal.contains(&expression));

        // Remove the link from the perspective
        perspective
            .remove_link(expression.clone().into())
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
                .add_link_expression(LinkExpression::from(link.clone()), LinkStatus::Shared)
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
}
