// rust-executor/src/perspective.rs

use std::sync::{Arc, Mutex};
use chrono::DateTime;
use deno_core::error::AnyError;
use serde::{Serialize, Deserialize};
use crate::agent::create_signed_expression;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC};
use crate::{db::Ad4mDb, types::*};

use crate::graphql::graphql_types::{LinkQuery, PerspectiveHandle, PerspectiveLinkFilter, PerspectiveLinkUpdatedFilter};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PerspectiveInstance {
    pub persisted: PerspectiveHandle,

    pub created_from_join: bool,
    pub is_fast_polling: bool,
    pub retries: u32,
    
    //db: Ad4mDb,
    //agent: AgentService,
    //language_controller: LanguageController,
    //update_controllers_handle_sync_status: Box<dyn Fn(String, PerspectiveState)>,
    //prolog_engine: Option<PrologInstance>,
    prolog_needs_rebuild: bool,
    //polling_interval: Option<tokio::time::Interval>,
    //pending_diff_polling_interval: Option<tokio::time::Interval>,
    prolog_mutex: Arc<Mutex<()>>,
    is_teardown: bool,
    sdna_change_mutex: Arc<Mutex<()>>,
}

impl PerspectiveInstance {
    pub fn new(
            handle: PerspectiveHandle, 
            created_from_join: Option<bool>, 
        ) -> Self {
        // Constructor logic
        PerspectiveInstance {
            persisted: handle.clone(),
            
            created_from_join: created_from_join.unwrap_or(false),
            is_fast_polling: false,
            retries: 0,
            prolog_needs_rebuild: false,
            prolog_mutex: Arc::new(Mutex::new(())),
            is_teardown: false,
            sdna_change_mutex: Arc::new(Mutex::new(())),
        }
    }

    pub fn update_from_handle(&mut self, handle: PerspectiveHandle) {
        self.persisted = handle;
    }

    pub async fn commit(&self, diff: &PerspectiveDiff) -> Result<(), AnyError> {
        Err(AnyError::msg("Not implemented"))
    }

    pub async fn add_link(&mut self, link: Link, status: LinkStatus) -> Result<DecoratedLinkExpression, AnyError> {
        let link_expression = create_signed_expression(link)?;
        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_link(&self.persisted.uuid, &link_expression, &status)?;

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
                        .add_pending_diff(&self.persisted.uuid, &diff)?;
                }
            }
        }

        let link_expression = DecoratedLinkExpression::from((link_expression, status));
        self.prolog_needs_rebuild = true;

        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_LINK_ADDED_TOPIC, 
                &serde_json::to_string(&PerspectiveLinkFilter {
                    perspective: self.persisted.clone(),
                    link: link_expression.clone(),
                }).unwrap(),
            )
            .await;

        Ok(link_expression)
    }


    pub async fn add_links(&mut self, links: Vec<Link>, status: LinkStatus) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let link_expressions = links.into_iter()
            .map(|l| create_signed_expression(l))
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
                .add_pending_diff(&self.persisted.uuid, &diff)?;
        }

        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_many_links(&self.persisted.uuid, link_expressions.clone(), &status)?;

        let decorated_link_expressions = link_expressions.into_iter()
            .map(|l| DecoratedLinkExpression::from((l, status.clone())))
            .collect::<Vec<DecoratedLinkExpression>>();

        self.prolog_needs_rebuild = true;

        for link in &decorated_link_expressions {
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: self.persisted.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }
        self.prolog_needs_rebuild = true;

        Ok(decorated_link_expressions)
    }

    pub async fn link_mutations(&mut self, mutations: LinkMutations, status: Option<LinkStatus>) -> Result<PerspectiveDiff, AnyError> {
        let additions = mutations.additions.into_iter()
            .map(create_signed_expression)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let removals = mutations.removals.into_iter()
            .map(create_signed_expression)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;

        let diff = PerspectiveDiff {
            additions,
            removals,
        };

        let mutation_result = self.commit(&diff).await;

        if mutation_result.is_err() {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .add_pending_diff(&self.persisted.uuid, &diff)?;
        }

        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_many_links(&self.persisted.uuid, diff.additions.clone(), &status.clone().unwrap_or_default())?;

        for link in &diff.removals {
            Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .remove_link(&self.persisted.uuid, link)?;
        }

        self.prolog_needs_rebuild = true;

        for link in &diff.additions {
            let link = DecoratedLinkExpression::from((link.clone(), status.clone().unwrap_or_default()));
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_ADDED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: self.persisted.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }

        for link in &diff.removals {
            let link = DecoratedLinkExpression::from((link.clone(), status.clone().unwrap_or_default()));
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: self.persisted.clone(),
                        link: link.clone(),
                    }).unwrap(),
                )
                .await;
        }

        self.prolog_needs_rebuild = true;

        Ok(diff)
    }

    pub async fn update_link(&mut self, old_link: LinkExpression, new_link: Link) -> Result<DecoratedLinkExpression, AnyError> {
        let link_option = Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .get_link(&self.persisted.uuid, &old_link)?;

        let link = match link_option {
            Some(link) => link,
            None => {
                return Err(AnyError::msg(format!(
                    "NH [{}] ({}) Link not found in perspective \"{}\": {:?}",
                    self.persisted.shared_url.clone().unwrap_or("not-shared".to_string()), 
                    self.persisted.name.clone().unwrap_or("<no name>".to_string()), 
                    self.persisted.uuid, 
                    old_link
                )))
            }
        };

        let new_link_expression = create_signed_expression(new_link)?;

        Ad4mDb::global_instance()
                .lock()
                .expect("Couldn't get write lock on Ad4mDb")
                .as_ref()
                .expect("Ad4mDb not initialized")
                .update_link(&self.persisted.uuid, &link, &new_link_expression)?;

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
                .add_pending_diff(&self.persisted.uuid, &diff)?;
        }

        // TODO: actually get the status from the link
        let new_link_expression = DecoratedLinkExpression::from((new_link_expression, LinkStatus::Shared));
        let old_link = DecoratedLinkExpression::from((old_link, LinkStatus::Shared));

        self.prolog_needs_rebuild = true;
        get_global_pubsub()
            .await
            .publish(
                &PERSPECTIVE_LINK_UPDATED_TOPIC,
                &serde_json::to_string(&PerspectiveLinkUpdatedFilter {
                    perspective: self.persisted.clone(),
                    old_link,
                    new_link: new_link_expression.clone(),
                }).unwrap(),
            )
            .await;


        Ok(new_link_expression)
    }

}