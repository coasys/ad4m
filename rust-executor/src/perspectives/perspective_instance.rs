use std::sync::Arc;
use std::time::Duration;
use tokio::{join, time};
use tokio::sync::Mutex;
use ad4m_client::literal::Literal;
use chrono::DateTime;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Serialize, Deserialize};
use crate::agent::create_signed_expression;
use crate::languages::language::Language;
use crate::languages::LanguageController;
use crate::prolog_service::engine::PrologEngine;
use crate::pubsub::{get_global_pubsub, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC, PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC};
use crate::{db::Ad4mDb, types::*};
use crate::graphql::graphql_types::{DecoratedPerspectiveDiff, LinkMutations, LinkQuery, LinkStatus, NeighbourhoodSignalFilter, OnlineAgent, PerspectiveExpression, PerspectiveHandle, PerspectiveLinkFilter, PerspectiveLinkUpdatedFilter, PerspectiveState, PerspectiveStateFilter};
use super::sdna::init_engine_facts;
use super::update_perspective;
use super::utils::prolog_resolution_to_string;


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
            _ => Err(anyhow!("Invalid SDNA type: {}. Must one of 'subject_class', 'flow' or 'custom'.", s)),
        }
    }
}

#[derive(Clone)]
pub struct PerspectiveInstance {
    pub persisted: Arc<Mutex<PerspectiveHandle>>,

    pub created_from_join: bool,
    pub is_fast_polling: bool,
    pub retries: u32,

    //db: Ad4mDb,
    //agent: AgentService,
    //language_controller: LanguageController,
    //update_controllers_handle_sync_status: Box<dyn Fn(String, PerspectiveState)>,
    prolog_engine: Arc<Mutex<Option<PrologEngine>>>,
    prolog_needs_rebuild: Arc<Mutex<bool>>,
    //polling_interval: Option<tokio::time::Interval>,
    //pending_diff_polling_interval: Option<tokio::time::Interval>,
    is_teardown: bool,
    sdna_change_mutex: Arc<Mutex<()>>,
    link_language: Arc<Mutex<Option<Language>>>,
}

impl PerspectiveInstance {
    pub fn new(
            handle: PerspectiveHandle,
            created_from_join: Option<bool>,
        ) -> Self {
        PerspectiveInstance {
            persisted: Arc::new(Mutex::new(handle.clone())),

            created_from_join: created_from_join.unwrap_or(false),
            is_fast_polling: false,
            retries: 0,
            prolog_engine: Arc::new(Mutex::new(None)),
            prolog_needs_rebuild: Arc::new(Mutex::new(true)),
            is_teardown: false,
            sdna_change_mutex: Arc::new(Mutex::new(())),
            link_language: Arc::new(Mutex::new(None)),
        }
    }

    pub async fn start_background_tasks(self) {
        let _ = join!(
            self.ensure_link_language(),
            self.nh_sync_loop(),
            self.pending_diffs_loop(),
        );
    }

    async fn ensure_link_language(&self) {
        let mut interval = time::interval(Duration::from_secs(5));
        while !self.is_teardown {
            if self.link_language.lock().await.is_none() && self.persisted.lock().await.neighbourhood.is_some() {
                let nh = self.persisted.lock().await.neighbourhood.as_ref().expect("must be some").clone();

                match  LanguageController::language_by_address(nh.data.link_language.clone()).await {
                    Ok(Some(language)) => {
                        {
                            let mut link_language_guard = self.link_language.lock().await;
                            *link_language_guard = Some(language);
                        }
                        if self.persisted.lock().await.state == PerspectiveState::NeighbourhoodCreationInitiated {
                            self.ensure_public_links_are_shared().await;
                        }
                        self.update_perspective_state_log_error(PerspectiveState::LinkLanguageInstalledButNotSynced).await;
                        break;
                    },
                    Ok(None) => {
                        log::debug!("Link language {} not installed yet, retrying in 5 seconds", nh.data.link_language.clone());
                        self.update_perspective_state_log_error(PerspectiveState::LinkLanguageFailedToInstall).await;
                    },
                    Err(e) => {
                        log::error!("Error when calling language_by_address: {:?}", e);
                        self.update_perspective_state_log_error(PerspectiveState::LinkLanguageFailedToInstall).await;
                    }
                }
            }
            interval.tick().await;
        }
    }

    async fn nh_sync_loop(&self) {
        let mut interval = time::interval(Duration::from_secs(3));
        while !self.is_teardown {
            let mut link_language_guard = self.link_language.lock().await;
            if let Some(link_language) = link_language_guard.as_mut() {
                match link_language.sync().await {
                    Ok(_) => (),
                    Err(e) => {
                        log::error!("Error calling sync on link language: {:?}", e);
                        let _ = self.update_perspective_state(PerspectiveState::LinkLanguageInstalledButNotSynced).await;
                    }
                }
            }
            interval.tick().await;
        }
    }

    async fn pending_diffs_loop(&self) {
        let mut interval = time::interval(Duration::from_secs(10));
        let uuid = self.persisted.lock().await.uuid.clone();
        while !self.is_teardown {
            if let Err(e) = (|| async {
                let mut link_language_guard = self.link_language.lock().await;
                if let Some(link_language) = link_language_guard.as_mut() {
                    // We have a link language.
                    // Let's see if we have a revision yet (otherwise we're not synced yet and should keep our diffs pending)
                    if link_language.current_revision().await.map_err(|e| anyhow!("current_revision error: {}",e))?.is_some() {
                        // Ok, we are synced and have a revision. Let's commit our pending diffs.
                        let pending_diffs = Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&uuid)).map_err(|e| anyhow!("get_pending_diffs error: {}",e))?;
                        let commit_result = link_language.commit(pending_diffs).await;
                        return match commit_result {
                            Ok(Some(_)) => {
                                Ad4mDb::with_global_instance(|db| db.clear_pending_diffs(&uuid)).map_err(|e| anyhow!("clear_pending_diffs error: {}",e))?;
                                Ok(())
                            },
                            Ok(None) => {
                                Err(anyhow!("Error committing pending diffs. No diff returned."))
                            },
                            Err(e) => {
                                Err(anyhow!("Error committing pending diffs: {:?}", e))
                            }
                        }
                    }
                }
                Ok(())  
            })().await {
                log::error!("Error in pending_diffs_loop: {:?}", e);
            }
            interval.tick().await;
        }
    }

    async fn ensure_public_links_are_shared(&self) {
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
            let mut local_links = Ad4mDb::with_global_instance(|db| 
                db.get_all_links(&uuid)
            ).unwrap();

            local_links.retain(|(_, status)| status == &LinkStatus::Shared);

            let remote_links = match link_language.current_revision().await {
                Ok(Some(_)) => link_language.render()
                    .await
                    .unwrap_or(None)
                    .unwrap_or_default()
                    .links,
                _ => vec![],
            };

            let mut links_to_commit = Vec::new();
            for (local_link, _) in &local_links {
                if !remote_links.iter().any(|e| 
                    e.author == local_link.author &&
                    e.timestamp == local_link.timestamp &&
                    e.data.source == local_link.data.source &&
                    e.data.target == local_link.data.target &&
                    e.data.predicate == local_link.data.predicate
                ) {
                    links_to_commit.push(local_link.clone());
                }
            }

            if !links_to_commit.is_empty() {
                let result = link_language.commit(PerspectiveDiff {
                    additions: links_to_commit,
                    removals: vec![],
                }).await;

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
                        state: serde_json::to_string(&state).expect("must be able to serialze PerspectiveState"),
                    }).unwrap(),
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

    pub async fn commit(&self, diff: &PerspectiveDiff) -> Result<Option<Vec<u8>>, AnyError> {
        let handle = self.persisted.lock().await.clone();
        if handle.neighbourhood.is_none() {
            return Ok(None)
        }


        let mut can_commit = false;
        if !self.created_from_join {
            can_commit = true;
        } else {
            if let Some(link_language) = self.link_language.lock().await.as_mut() {
                if link_language.current_revision().await?.is_some() {
                    can_commit = true;
                }
            }
        };

        if can_commit {
            if let Some(link_language) = self.link_language.lock().await.as_mut() {
                return Ok(link_language.commit(diff.clone()).await?);
            }
        }

        Err(anyhow!("Cannot commit diff. Not yet synced with neighbourhood..."))
    }

    pub async fn diff_from_link_language(&self, diff: PerspectiveDiff) {
        let handle = self.persisted.lock().await.clone();
        if !diff.additions.is_empty() {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .add_many_links(&handle.uuid, diff.additions.clone(), &LinkStatus::Shared)
                .expect("Failed to add many links");
        }

        if !diff.removals.is_empty() {
            for link in &diff.removals {
                Ad4mDb::global_instance()
                    .lock()
                    .expect("Couldn't get write lock on Ad4mDb")
                    .as_ref()
                    .expect("Ad4mDb not initialized")
                    .remove_link(&handle.uuid, link)
                    .expect("Failed to remove link");
            }
        }
        self.set_prolog_rebuild_flag().await;


        for link in &diff.additions {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)),
                    }).unwrap(),
                )
                .await;
        }
        
        for link in &diff.removals {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)),
                    }).unwrap(),
                )
                .await;
        }
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
                }).unwrap(),
            )
            .await;
    }

    pub async fn add_link(&mut self, link: Link, status: LinkStatus) -> Result<DecoratedLinkExpression, AnyError> {
        let link_expression = create_signed_expression(link)?;
        self.add_link_expression(link_expression.into(), status).await
    }

    async fn set_prolog_rebuild_flag(&self) {
        *self.prolog_needs_rebuild.lock().await = true;
    }

    pub async fn add_link_expression(&mut self, link_expression: LinkExpression, status: LinkStatus) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_link(&handle.uuid, &link_expression, &status)?;

        if status == LinkStatus::Shared {
            let diff = PerspectiveDiff {
                additions: vec![link_expression.clone()],
                removals: vec![],
            };
            match self.commit(&diff).await {
                Ok(_) => (),
                Err(_) => {
                    Ad4mDb::global_instance()
                        .lock()
                        .expect("Couldn't get write lock on Ad4mDb")
                        .as_ref()
                        .expect("Ad4mDb not initialized")
                        .add_pending_diff(&handle.uuid, &diff)?;
                }
            }
        }

        let link_expression = DecoratedLinkExpression::from((link_expression, status));
        self.set_prolog_rebuild_flag().await;

        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_LINK_ADDED_TOPIC,
                &serde_json::to_string(&PerspectiveLinkFilter {
                    perspective: handle,
                    link: link_expression.clone(),
                }).unwrap(),
            )
            .await;

        Ok(link_expression)
    }


    pub async fn add_links(&mut self, links: Vec<Link>, status: LinkStatus) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let uuid = handle.uuid.clone();
        let link_expressions = links.into_iter()
            .map(|l| create_signed_expression(l).map(|l| LinkExpression::from(l)))
            .collect::<Result<Vec<LinkExpression>, AnyError>>();

        let link_expressions = link_expressions?;

        let diff = PerspectiveDiff {
            additions: link_expressions.clone(),
            removals: vec![],
        };
        let add_links_result = self.commit(&diff).await;

        if add_links_result.is_err() {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .add_pending_diff(&uuid, &diff)?;
        }

        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_many_links(&uuid, link_expressions.clone(), &status)?;

        let decorated_link_expressions = link_expressions.into_iter()
            .map(|l| DecoratedLinkExpression::from((l, status.clone())))
            .collect::<Vec<DecoratedLinkExpression>>();

            self.set_prolog_rebuild_flag().await;

        for link in &decorated_link_expressions {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }
        self.set_prolog_rebuild_flag().await;

        Ok(decorated_link_expressions)
    }

    pub async fn link_mutations(&mut self, mutations: LinkMutations, status: LinkStatus) -> Result<DecoratedPerspectiveDiff, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let additions = mutations.additions.into_iter()
            .map(Link::from)
            .map(create_signed_expression)
            .map(|r| r.map(LinkExpression::from))
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let removals = mutations.removals.into_iter()
            .map(LinkExpression::try_from)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;

        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_many_links(&handle.uuid, additions.clone(), &status)?;

        for link in &removals {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .remove_link(&handle.uuid, link)?;
        }

        self.set_prolog_rebuild_flag().await;

        let diff = PerspectiveDiff {
            additions: additions.clone(),
            removals: removals.clone()
        };

        let mutation_result = self.commit(&diff).await;

        if mutation_result.is_err() {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .add_pending_diff(&handle.uuid, &diff)?;
        }

        let diff = DecoratedPerspectiveDiff {
            additions: additions.into_iter().map(|l| DecoratedLinkExpression::from((l, status.clone()))).collect::<Vec<DecoratedLinkExpression>>(),
            removals: removals.clone().into_iter().map(|l| DecoratedLinkExpression::from((l, status.clone()))).collect::<Vec<DecoratedLinkExpression>>(),
        };

        for link in &diff.additions {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }

        for link in &diff.removals {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }

        self.set_prolog_rebuild_flag().await;

        Ok(diff)
    }

    pub async fn update_link(&mut self, old_link: LinkExpression, new_link: Link) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        let link_option = Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .get_link(&handle.uuid, &old_link)?;

        let (link, link_status) = match link_option {
            Some(link) => link,
            None => {
                return Err(AnyError::msg(format!(
                    "NH [{}] ({}) Link not found in perspective \"{}\": {:?}",
                    handle.shared_url.clone().unwrap_or("not-shared".to_string()),
                    handle.name.clone().unwrap_or("<no name>".to_string()),
                    handle.uuid,
                    old_link
                )))
            }
        };

        let new_link_expression = LinkExpression::from(create_signed_expression(new_link)?);

        Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .update_link(&handle.uuid, &link, &new_link_expression)?;

        let diff = PerspectiveDiff {
            additions: vec![new_link_expression.clone()],
            removals: vec![old_link.clone()],
        };
        let mutation_result = self.commit(&diff).await;

        if mutation_result.is_err() {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .add_pending_diff(&handle.uuid, &diff)?;
        }

        let new_link_expression = DecoratedLinkExpression::from((new_link_expression, link_status.clone()));
        let old_link = DecoratedLinkExpression::from((old_link, link_status));

        self.set_prolog_rebuild_flag().await;
        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_LINK_UPDATED_TOPIC,
                &serde_json::to_string(&PerspectiveLinkUpdatedFilter {
                    perspective: handle,
                    old_link,
                    new_link: new_link_expression.clone(),
                }).unwrap(),
            )
            .await;


        Ok(new_link_expression)
    }

    pub async fn remove_link(&mut self, link_expression: LinkExpression) -> Result<DecoratedLinkExpression, AnyError> {
        let handle = self.persisted.lock().await.clone();
        if let Some((link_from_db, status)) = Ad4mDb::with_global_instance(|db| db.get_link(&handle.uuid, &link_expression))? {
            Ad4mDb::with_global_instance(|db| db.remove_link(&handle.uuid, &link_expression))?;

            let diff = PerspectiveDiff {
                additions: vec![],
                removals: vec![link_expression.clone()],
            };
            let mutation_result = self.commit(&diff).await;

            if mutation_result.is_err() {
                Ad4mDb::with_global_instance(|db| db.add_pending_diff(&handle.uuid, &diff))?;
            }

            self.set_prolog_rebuild_flag().await;
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: handle,
                        link: DecoratedLinkExpression::from((link_expression.clone(), status.clone())),
                    }).unwrap(),
                )
                .await;

            Ok(DecoratedLinkExpression::from((link_from_db, status)))
        } else {
            Err(anyhow!("Link not found"))
        }
    }

    async fn get_links_local(&self, query: &LinkQuery) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        let uuid = self.persisted.lock().await.uuid.clone();
        let mut result = if query.source.is_none() && query.predicate.is_none() && query.target.is_none() {
            Ad4mDb::with_global_instance(|db| {
                db.get_all_links(&uuid)
            })?
        } else if let Some(source) = &query.source {
            Ad4mDb::with_global_instance(|db| {
                db.get_links_by_source(&uuid, source)
            })?
        } else if let Some(target) = &query.target {
            Ad4mDb::with_global_instance(|db| {
                db.get_links_by_target(&uuid, target)
            })?
        } else if let Some(predicate) = &query.predicate {
            Ad4mDb::with_global_instance(|db| {
                Ok::<Vec<(LinkExpression, LinkStatus)>, AnyError>(
                    db.get_all_links(&  uuid)?
                        .into_iter()
                        .filter(|(link, _)| link.data.predicate.as_ref() == Some(predicate))
                        .collect::<Vec<(LinkExpression, LinkStatus)>>()
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

        let until_date: Option<chrono::DateTime<chrono::Utc>> = query.until_date.clone().map(|d| d.into());
        let from_date: Option<chrono::DateTime<chrono::Utc>> = query.from_date.clone().map(|d| d.into());

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
                    query.from_date = q.until_date.clone();
                    query.until_date = q.from_date.clone();
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
            links = links[..limit as usize].to_vec();
        }

        Ok(links
            .into_iter()
            .map(|(link, status)| {
                DecoratedLinkExpression::from((link.clone(), status)).into()
            })
            .collect())
    }



    /// Adds the given Social DNA code to the perspective's SDNA code
    pub async fn add_sdna(&mut self, name: String, mut sdna_code: String, sdna_type: SdnaType) -> Result<bool, AnyError> {
        let mut added = false;
        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;

        let predicate = match sdna_type {
            SdnaType::SubjectClass => "ad4m://has_subject_class",
            SdnaType::Flow => "ad4m://has_flow",
            SdnaType::Custom => "ad4m://has_custom_sdna",
        };

        let literal_name = Literal::from_string(name).to_url().expect("just initialized Literal couldn't be turned into URL");

        let links = self.get_links(&LinkQuery {
            source: Some("ad4m://self".to_string()),
            predicate: Some(predicate.to_string()),
            target: Some(literal_name.clone()),
            from_date: None,
            until_date: None,
            limit: None,
        }).await?;

        let mut sdna_links: Vec<Link> = Vec::new();

        if let Err(_) = Literal::from_url(sdna_code.clone()) {
            sdna_code = Literal::from_string(sdna_code).to_url().expect("just initialized Literal couldn't be turned into URL");
        }



        if links.is_empty() {
            sdna_links.push(Link {
                source: "ad4m://self".to_string(),
                predicate: Some(predicate.to_string()),
                target: literal_name.clone(),
            });

            sdna_links.push(Link {
                source: literal_name,
                predicate: Some("ad4m://sdna".to_string()),
                target: sdna_code,
            });

            self.add_links(sdna_links, LinkStatus::Shared).await?;
            added = true;
        }
        // Mutex guard is automatically dropped here
        Ok(added)
    }


    /// Executes a Prolog query against the engine, spawning and initializing the engine if necessary.
    pub async fn prolog_query(&mut self, query: String) -> Result<String, AnyError> {
        let mut maybe_prolog_engine = self.prolog_engine.lock().await;
        if maybe_prolog_engine.is_none() {
            let mut engine = PrologEngine::new();
            engine.spawn().await.map_err(|e| anyhow!("Failed to spawn Prolog engine: {}", e))?;
            *maybe_prolog_engine = Some(engine);
            self.set_prolog_rebuild_flag().await;
        }

        let prolog_enging_option_ref = maybe_prolog_engine.as_ref();
        let prolog_engine = prolog_enging_option_ref.as_ref().expect("Must be some since we initialized the engine above");

        let mut needs_rebuild = self.prolog_needs_rebuild.lock().await;

        if *needs_rebuild {
            let all_links = self.get_links(&LinkQuery::default()).await?;
            let facts = init_engine_facts(all_links, self.persisted.lock().await.neighbourhood.as_ref().map(|n| n.author.clone())).await?;
            prolog_engine.load_module_string("facts".to_string(), facts).await?;
            *needs_rebuild = false;
        }

        let query = if !query.ends_with(".") {
            query + "."
        } else {
            query
        };

        let result = prolog_engine
            .run_query(query).await?.map_err(|e| anyhow!(e))?;
        Ok(prolog_resolution_to_string(result))
    }

    async fn no_link_language_error(&self) -> AnyError {
        let handle = self.persisted.lock().await.clone();
        anyhow!("Perspective {} has no link language installed. State is: {:?}", handle.uuid, handle.state)
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
            Ok(link_language.get_online_agents()
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
        payload: PerspectiveExpression
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
        payload: PerspectiveExpression
    ) -> Result<(), AnyError> {
        let mut link_language_guard = self.link_language.lock().await;
        if let Some(link_language) = link_language_guard.as_mut() {
            link_language.send_broadcast(payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }


}





#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphql::graphql_types::PerspectiveState;
    use crate::perspectives::perspective_instance::{PerspectiveHandle};
    use crate::db::Ad4mDb;
    use uuid::Uuid;
    use crate::test_utils::setup_wallet;
    use fake::{Fake, Faker};

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
             None
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
            let expression = perspective.add_link(link.clone(), LinkStatus::Local).await.unwrap();
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

            let expression = perspective.add_link(link.clone(), LinkStatus::Shared).await.unwrap();
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
        perspective.remove_link(expression.clone().into()).await.unwrap();

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
            link.target = format!("lang://test-target {}",i);
            let mut link = create_signed_expression(link).expect("Failed to create link");
            link.timestamp = (now - chrono::Duration::minutes(5) + chrono::Duration::minutes(i as i64)).to_rfc3339();
            let expression = perspective.add_link_expression(LinkExpression::from(link.clone()), LinkStatus::Shared).await.unwrap();
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
        let until_date = (now - chrono::Duration::minutes(2) - chrono::Duration::seconds(30)).into();
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
        links_date_desc.iter().for_each(|l| println!("Link: {:?}", l.data.target));
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
        links_date_desc.iter().for_each(|l| println!("Link: {:?}", l.data.target));
        assert_eq!(links_date_desc[0].data.target, all_links[0].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[1].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);



    }

    // Additional tests for updateLink, removeLink, syncWithSharingAdapter, etc. would go here
    // following the same pattern as above.
}

