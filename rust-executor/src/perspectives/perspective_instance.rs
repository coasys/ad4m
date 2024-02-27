// rust-executor/src/perspective.rs

use std::sync::{Arc, Mutex};
use deno_core::error::AnyError;
use serde::{Serialize, Deserialize};
use crate::agent::create_signed_expression;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_LINK_ADDED_TOPIC};
use crate::{db::Ad4mDb, types::*};

use crate::graphql::graphql_types::{PerspectiveHandle, PerspectiveLinkFilter};

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
}