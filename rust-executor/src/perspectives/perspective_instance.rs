// rust-executor/src/perspective.rs

use std::sync::{Arc, Mutex};
use chrono::DateTime;
use deno_core::anyhow::anyhow;
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

    pub async fn commit(&self, _diff: &PerspectiveDiff) -> Result<(), AnyError> {
        Err(AnyError::msg("Not implemented"))
    }

    pub async fn add_link(&mut self, link: Link, status: LinkStatus) -> Result<DecoratedLinkExpression, AnyError> {
        let link_expression = create_signed_expression(link)?;
        self.add_link_expression(link_expression, status).await
    }

    async fn add_link_expression(&mut self, link_expression: LinkExpression, status: LinkStatus) -> Result<DecoratedLinkExpression, AnyError> {    
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

    pub async fn remove_link(&mut self, link_expression: LinkExpression) -> Result<(), AnyError> {
        if let Some((_, status)) = Ad4mDb::with_global_instance(|db| db.get_link(&self.persisted.uuid, &link_expression))? {
            Ad4mDb::with_global_instance(|db| db.remove_link(&self.persisted.uuid, &link_expression))?;

            let diff = PerspectiveDiff {
                additions: vec![],
                removals: vec![link_expression.clone()],
            };
            let mutation_result = self.commit(&diff).await;

            if mutation_result.is_err() {
                Ad4mDb::with_global_instance(|db| db.add_pending_diff(&self.persisted.uuid, &diff))?;
            }

            self.prolog_needs_rebuild = true;
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_LINK_REMOVED_TOPIC,
                    &serde_json::to_string(&PerspectiveLinkFilter {
                        perspective: self.persisted.clone(),
                        link: DecoratedLinkExpression::from((link_expression, status)),
                    }).unwrap(),
                )
                .await;

            Ok(())
        } else {
            Err(anyhow!("Link not found"))
        }
    }

    async fn get_links_local(&self, query: &LinkQuery) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        println!("Query: {:?}", query);
        

        let mut result = if query.source.is_none() && query.predicate.is_none() && query.target.is_none() {
            Ad4mDb::with_global_instance(|db| {
                db.get_all_links(&self.persisted.uuid)
            })?
        } else if let Some(source) = &query.source {
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

        if let Some(limit) = query.limit {
            let result_length = result.len() as i32;
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

            result = result[start_limit..end_limit].to_vec();
        }
 
        Ok(result)
    }

    async fn get_links(&self, query: &LinkQuery) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let mut links = self.get_links_local(query).await?;

        let until_date: Option<chrono::DateTime<chrono::Utc>> = query.until_date.clone().map(|d| d.into());
        let from_date: Option<chrono::DateTime<chrono::Utc>> = query.from_date.clone().map(|d| d.into());

        let reverse = from_date >= until_date;

        links.sort_by(|(a, _), (b, _)| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            a_time.cmp(&b_time)
        });

        if let Some(limit) = query.limit {
            let start_limit = if reverse { links.len().saturating_sub(limit as usize) } else { 0 };
            let end_limit = if reverse { links.len() } else { limit.min(links.len() as i32) as usize };
            links = links[start_limit..end_limit].to_vec();
        } 

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
            let mut link = create_signed_expression(create_link()).expect("Failed to create link");
            link.timestamp = (now - chrono::Duration::minutes(i as i64)).to_rfc3339();
            let expression = perspective.add_link_expression(link.clone(), LinkStatus::Shared).await.unwrap();
            all_links.push(expression);
            println!("Added link with timestamp: {}, {:?}", link.timestamp, link);
        }

        // Query for links with a from_date set to 3 minutes ago
        let from_date = (now - chrono::Duration::minutes(2) - chrono::Duration::seconds(30)).into();
        let query_with_from_date = LinkQuery {
            from_date: Some(from_date),
            ..Default::default()
        };
        //println!("Query with from_date: {:?}", query_with_from_date);
        let links_from_date = perspective.get_links(&query_with_from_date).await.unwrap();
        //println!("Links from date: {:?}", links_from_date);
        assert_eq!(links_from_date.len(), 3);

        // Query for links with an until_date set to 3 minutes ago
        let until_date = (now - chrono::Duration::minutes(3)).into();
        let query_with_until_date = LinkQuery {
            until_date: Some(until_date),
            ..Default::default()
        };
        let links_until_date = perspective.get_links(&query_with_until_date).await.unwrap();
        assert_eq!(links_until_date.len(), 2);

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
    }

    // Additional tests for updateLink, removeLink, syncWithSharingAdapter, etc. would go here
    // following the same pattern as above.
}

