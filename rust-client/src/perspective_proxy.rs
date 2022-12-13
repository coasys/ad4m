use crate::{
    literal::{Literal, LiteralValue},
    perspectives::{
        add_link::AddLinkPerspectiveAddLink, query_links::QueryLinksPerspectiveQueryLinks,
        PerspectivesClient,
    },
    types::LinkExpression,
};
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
        self.set_single_target(
            "ad4m://self".into(),
            "ad4m://has_zome".into(),
            literal.to_url()?,
        )
        .await?;
        Ok(())
    }

    pub async fn get_dna(&self) -> Result<Vec<String>> {
        self.get(
            Some("ad4m://self".into()),
            None,
            Some("ad4m://has_zome".into()),
            None,
            None,
            None,
        )
        .await?
        .into_iter()
        .map(|link| {
            let literal = Literal::from_url(link.data.target)?;
            match literal.get() {
                Ok(LiteralValue::String(string)) => Ok(string),
                _ => Err(anyhow::anyhow!("Not a string literal")),
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
        if links.is_empty() {
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
                .remove_link(self.perspective_uuid.clone(), link)
                .await?;
        }

        self.client
            .add_link(
                self.perspective_uuid.clone(),
                source,
                target,
                Some(predicate),
            )
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

    pub async fn create_subject(&self, class: &String, base: &String) -> Result<()> {
        if let Value::Array(results) = self
            .infer(format!(
                "subject_class(\"{}\", C), constructor(C, Action)",
                class
            ))
            .await?
        {
            println!("{:?}", results);
            if let Some(Value::Object(action)) = results.first() {
                println!("{:?}", action);
                if let Value::String(action_string) = action
                    .get("Action")
                    .ok_or(anyhow::anyhow!("Unbound variable Action is not set"))?
                {
                    println!("{}", action_string);
                    self.execute_action(action_string, base).await?;
                    return Ok(());
                }
            }
        }
        Err(anyhow::anyhow!("No constructor found for class: {}", class))
    }

    async fn execute_action(&self, action: &String, base: &String) -> Result<()> {
        let comman_array: Value = serde_json::from_str(action)?;
        if let Some(commands) = comman_array.as_array() {
            for command in commands {
                if let Some(Value::String(action)) = command.get("action") {
                    let source = get_command_param(command, "source", base)
                        .ok_or(anyhow::anyhow!("No source in action"))?;
                    let target = get_command_param(command, "target", base)
                        .ok_or(anyhow::anyhow!("No target in action"))?;
                    let predicate = get_command_param(command, "predicate", base);
                    match action.as_str() {
                        "add_link" => {
                            self.add_link(source, target, predicate)
                                .await?;
                        }
                        "remove_link" => {
                            unimplemented!();
                            //let links = self.get(Some(source), Some(target), predicate, None, None, None).await?;
                            //elf.remove_link(source, target.into(), predicate.into()).await?;
                        }
                        "set_single_target" => {
                            self.set_single_target(
                                source,
                                predicate.ok_or(anyhow::anyhow!(
                                    "No predicate in set_single_target action"
                                ))?,
                                target,
                            )
                            .await?;
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }
}

fn get_command_param(command: &Value, param: &str, base: &String) -> Option<String> {
    let raw: Option<String> = command
        .get(param)
        .and_then(|s| s.as_str())
        .map(|s| s.into());
    raw.map(|s| s.replace("this", base))
}
