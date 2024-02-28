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

        let (link, _link_status) = match link_option {
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

    async fn get_links_local(&self, query: &LinkQuery) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        if query.source.is_none() && query.predicate.is_none() && query.target.is_none() {
            return Ad4mDb::with_global_instance(|db| {
                db.get_all_links(&self.persisted.uuid)
            })
        }

        let mut result = if let Some(source) = &query.source {
            Ad4mDb::with_global_instance(|db| {
                db.get_links_by_source(&self.persisted.uuid, source)
            })?
        } else if let Some(target) = &query.target {
            Ad4mDb::with_global_instance(|db| {
                db.get_links_by_target(&self.persisted.uuid, target)
            })?
        } else if let Some(predicate) = &query.predicate {
            Ad4mDb::with_global_instance(|db| {
                Ok::<Vec<(LinkExpression, LinkStatus)>, AnyError>(
                    db.get_all_links(&self.persisted.uuid)?
                        .into_iter()
                        .filter(|(link, status)| link.data.predicate.as_ref() == Some(predicate))
                        .collect::<Vec<(LinkExpression, LinkStatus)>>()
                )
            })?
        } else {
            vec![]
        };

        if let Some(predicate) = &query.predicate {
            result.retain(|(link, _status)| link.data.predicate.as_ref() == Some(predicate));
        }
/*
        if let Some(from_date) = &query.from_date {
            result.retain(|link| {
                let link_date = DateTime::parse_from_rfc3339(&link.timestamp).unwrap();
                if from_date >= query.until_date.unwrap_or(chrono::Utc::now()) {
                    link_date <= *from_date.into()
                } else {
                    link_date >= *from_date.into()
                }
            });
        }

        if let Some(until_date) = &query.until_date {
            result.retain(|link| {
                let link_date = DateTime::parse_from_rfc3339(&link.timestamp).unwrap();
                if query.from_date >= query.until_date {
                    link_date >= *until_date.into()
                } else {
                    link_date <= *until_date.into()
                }
            });
        }

        if let Some(limit) = query.limit {
            let start_limit = if query.from_date >= query.until_date { 
                result.len().saturating_sub(limit) 
            } else {
                0 
            };
            let end_limit = if query.from_date >= query.until_date { 
                result.len() 
            } else { 
                limit.min(result.len()) 
            };
            result = result[start_limit..end_limit].to_vec();
        }
 */
        Ok(result)
    }

    async fn get_links(&self, query: &LinkQuery) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let links = self.get_links_local(query).await?;
/*
        let reverse = query.from_date >= query.until_date;

        links.sort_by(|a, b| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            a_time.cmp(&b_time)
        });

        if let Some(limit) = query.limit {
            let start_limit = if reverse { links.len().saturating_sub(limit) } else { 0 };
            let end_limit = if reverse { links.len() } else { limit.min(links.len()) };
            links = links[start_limit..end_limit].to_vec();
        } */

        Ok(links
            .into_iter()
            .map(|(link, status)| {
                DecoratedLinkExpression::from((link.clone(), status)).into()
            })
            .collect())
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

    // Additional tests for updateLink, removeLink, syncWithSharingAdapter, etc. would go here
    // following the same pattern as above.
}

