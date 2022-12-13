use crate::{perspectives::{PerspectivesClient, query_links::QueryLinksPerspectiveQueryLinks, add_link::AddLinkPerspectiveAddLink}, types::LinkExpression, literal::{Literal, LiteralValue}};
use anyhow::Result;
use chrono::naive::NaiveDateTime;
use serde_json::Value;
type DateTime = NaiveDateTime;


pub struct PerspectiveProxy {
    client: PerspectivesClient,
    perspective_uuid: String,
}

impl PerspectiveProxy {
    pub fn new(client: PerspectivesClient, perspective_uuid: String) -> Self {
        Self {
            client,
            perspective_uuid,
        }
    }

    pub async fn add_link(
        &self,
        source: String,
        target: String,
        predicate: Option<String>,
    ) -> Result<AddLinkPerspectiveAddLink> {
        self.client
            .add_link(self.perspective_uuid.clone(), source, target, predicate)
            .await
    }

    pub async fn get(
        &self,
        source: Option<String>,
        target: Option<String>,
        predicate: Option<String>,
        from_date: Option<DateTime>,
        until_date: Option<DateTime>,
        limit: Option<f64>,
    ) -> Result<Vec<QueryLinksPerspectiveQueryLinks>> {
        self.client
            .query_links(
                self.perspective_uuid.clone(),
                source,
                target,
                predicate,
                from_date,
                until_date,
                limit,
            )
            .await
    }

    pub async fn infer(&self, prolog_query: String) -> Result<Value> {
        self.client
            .infer(self.perspective_uuid.clone(), prolog_query)
            .await
    }

    pub async fn set_dna(&self, dna: String) -> Result<()> {
        let literal = Literal::from_string(dna);
        self.set_single_target("ad4m://self".into(), "ad4m://has_zome".into(), literal.to_url()?).await?;
        Ok(())
    }

    pub async fn get_dna(&self) -> Result<Vec<String>> {
        self.get(
            Some("ad4m://self".into()), 
            None, 
            Some("ad4m://has_zome".into()), 
            None, 
            None, 
            None
        ).await?
        .into_iter()
        .map(|link| {
            let literal = Literal::from_url(link.data.target)?;
            match literal.get() {
                Ok(LiteralValue::String(string)) => Ok(string),
                _ => Err(anyhow::anyhow!("Not a string literal"))
            }
        })
        .collect()
    }

    pub async fn get_single_target(&self, source: String, predicate: String) -> Result<String> {
        let links = self
            .client
            .query_links(
                self.perspective_uuid.clone(),
                Some(source),
                None,
                Some(predicate),
                None,
                None,
                None,
            )
            .await?;
        if links.len() == 0 {
            return Err(anyhow::anyhow!("No links found"));
        }
        if links.len() > 1 {
            return Err(anyhow::anyhow!("Multiple links found"));
        }
        Ok(links[0].data.target.clone())
    }

    pub async fn set_single_target(
        &self,
        source: String,
        predicate: String,
        target: String,
    ) -> Result<()> {
        let links: Vec<LinkExpression> = self
            .client
            .query_links(
                self.perspective_uuid.clone(),
                Some(source.clone()),
                None,
                Some(predicate.clone()),
                None,
                None,
                None,
            )
            .await?
            .into_iter()
            .map(LinkExpression::from)
            .collect();

        for link in links {
            self.client
                .remove_link(
                    self.perspective_uuid.clone(),
                    link
                )
                .await?;
        }

        self.client
            .add_link(self.perspective_uuid.clone(), source, target, Some(predicate))
            .await?;
        Ok(())
    }

    pub async fn subject_classes(&self) -> Result<Vec<String>> {
        let mut classes = Vec::new();
        if let Value::Array(classes_results) = self.infer("subject_class(X, _)".into()).await? {
            for class in classes_results {
                if let Some(Value::String(class_name)) = class.get("X") {
                    classes.push(class_name.clone());
                }
            }
        }
        Ok(classes)
    }
}