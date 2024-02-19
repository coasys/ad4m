// rust-executor/src/perspective.rs

use std::sync::{Arc, Mutex};
use serde::{Serialize, Deserialize};
use crate::{db::Ad4mDb, types::*};

use crate::graphql::graphql_types::{PerspectiveHandle};


fn sign_link(link: Link) -> LinkExpression {
    LinkExpression {
        author: "dummy_author".to_string(),
        timestamp: "2021-01-01T00:00:00Z".to_string(),
        data: link,
        proof: ExpressionProof {
            signature: "dummy_signature".to_string(),
            key: "dummy_key".to_string(),
        },
        status: Some(LinkStatus::Shared),
    }
}

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

    pub fn add_link(&self, link: Link, status: LinkStatus) {
        let link_expression = sign_link(link);
        Ad4mDb::global_instance()
            .lock()
            .expect("Couldn't get write lock on Ad4mDb")
            .as_ref()
            .expect("Ad4mDb not initialized")
            .add_link(&self.persisted.uuid, &link_expression, &status);
    }
}