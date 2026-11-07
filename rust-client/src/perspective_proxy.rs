use std::collections::BTreeMap;
use std::sync::Arc;

use crate::{
    subject_proxy::SubjectProxy,
    types::{LinkExpression, LinkExpressionInput, LinkInput},
    ws_rpc::WsRpcClient,
};
use anyhow::{anyhow, Result};
use chrono::naive::NaiveDateTime;
use regex::Regex;
use serde_json::Value;
type DateTime = NaiveDateTime;

pub struct PerspectiveProxy {
    ws: Arc<WsRpcClient>,
    perspective_uuid: String,
}

impl PerspectiveProxy {
    pub fn new(ws: Arc<WsRpcClient>, perspective_uuid: String) -> Self {
        Self {
            ws,
            perspective_uuid,
        }
    }

    pub async fn add_link(
        &self,
        source: String,
        target: String,
        predicate: Option<String>,
        _status: Option<String>,
    ) -> Result<LinkExpression> {
        let link = LinkInput {
            source,
            target,
            predicate,
        };
        self.ws
            .call(
                "perspective.addLink",
                serde_json::json!({
                    "uuid": self.perspective_uuid,
                    "link": link,
                }),
            )
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
    ) -> Result<Vec<LinkExpression>> {
        let mut params = serde_json::json!({ "uuid": self.perspective_uuid });
        if let Some(v) = source {
            params["source"] = Value::String(v);
        }
        if let Some(v) = target {
            params["target"] = Value::String(v);
        }
        if let Some(v) = predicate {
            params["predicate"] = Value::String(v);
        }
        if let Some(v) = from_date {
            params["fromDate"] = Value::String(v.and_utc().to_rfc3339());
        }
        if let Some(v) = until_date {
            params["untilDate"] = Value::String(v.and_utc().to_rfc3339());
        }
        if let Some(v) = limit {
            params["limit"] = serde_json::json!(v as i64);
        }
        self.ws.call("perspective.queryLinks", params).await
    }

    pub async fn infer(&self, prolog_query: String) -> Result<Value> {
        self.ws
            .call(
                "perspective.queryProlog",
                serde_json::json!({
                    "uuid": self.perspective_uuid,
                    "query": prolog_query,
                }),
            )
            .await
    }

    pub async fn remove_link(&self, link: LinkExpression) -> Result<bool> {
        let link_input: LinkExpressionInput = link.into();
        self.ws
            .call(
                "perspective.removeLink",
                serde_json::json!({
                    "uuid": self.perspective_uuid,
                    "link": link_input,
                }),
            )
            .await
    }

    /// Register class shapes (each the SHACL shape JSON `perspective.shapes`
    /// answers) with `perspective.registerShapes`. Answers which classes were
    /// written and which were already stored as given.
    pub async fn register_shapes(&self, shapes: Vec<Value>) -> Result<Value> {
        self.ws
            .call(
                "perspective.registerShapes",
                serde_json::json!({
                    "uuid": self.perspective_uuid,
                    "shapes": shapes,
                }),
            )
            .await
    }

    /// The registered classes (`perspective.shapes`): each `{ name, shape,
    /// hash }`, sorted by name. `names` keeps only the classes named.
    pub async fn shapes(&self, names: Option<Vec<String>>) -> Result<Vec<Value>> {
        let mut params = serde_json::json!({ "uuid": self.perspective_uuid });
        if let Some(names) = names {
            params["names"] = serde_json::json!(names);
        }
        self.ws.call("perspective.shapes", params).await
    }

    pub async fn get_single_target(&self, source: String, predicate: String) -> Result<String> {
        let links = self
            .get(Some(source), None, Some(predicate), None, None, None)
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
        let links = self
            .get(
                Some(source.clone()),
                None,
                Some(predicate.clone()),
                None,
                None,
                None,
            )
            .await?;

        for link in links {
            self.remove_link(link).await?;
        }

        let link_input = LinkInput {
            source,
            target,
            predicate: Some(predicate),
        };
        self.ws
            .call::<LinkExpression>(
                "perspective.addLink",
                serde_json::json!({
                    "uuid": self.perspective_uuid,
                    "link": link_input,
                }),
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

    pub async fn subject_class_properties(&self, class: &String) -> Result<Vec<String>> {
        let mut propertys = Vec::new();
        if let Value::Array(propertys_results) = self
            .infer(format!("subject_class(\"{}\", C), property(C, X)", class))
            .await?
        {
            for property in propertys_results {
                if let Some(Value::String(property_name)) = property.get("X") {
                    propertys.push(property_name.clone());
                }
            }
        }
        Ok(propertys)
    }

    pub async fn subject_class_collections(&self, class: &String) -> Result<Vec<String>> {
        let mut collections = Vec::new();
        if let Value::Array(collection_results) = self
            .infer(format!("subject_class(\"{}\", C), collection(C, X)", class))
            .await?
        {
            for colletion in collection_results {
                if let Some(Value::String(collection_name)) = colletion.get("X") {
                    collections.push(collection_name.clone());
                }
            }
        }
        Ok(collections)
    }

    pub async fn create_subject(&self, class: &String, base: &str) -> Result<()> {
        if let Ok(Value::Array(results)) = self
            .infer(format!(
                "subject_class(\"{}\", C), constructor(C, Action)",
                class
            ))
            .await
        {
            if let Some(Value::Object(action)) = results.first() {
                if let Value::String(action_string) = action
                    .get("Action")
                    .ok_or(anyhow::anyhow!("Unbound variable Action is not set"))?
                {
                    self.execute_action(action_string, base, None).await?;
                    return Ok(());
                }
            }
        }
        Err(anyhow::anyhow!("No constructor found for class: {}", class))
    }

    pub async fn is_subject_instance(&self, class: &String, base: &String) -> Result<bool> {
        match self
            .infer(format!(
                r#"subject_class("{}", C), instance(C, "{}")"#,
                class, base
            ))
            .await?
        {
            Value::Array(results) => {
                if results.is_empty() {
                    Ok(false)
                } else {
                    Ok(true)
                }
            }
            Value::Bool(b) => Ok(b),
            _ => Ok(false),
        }
    }

    pub async fn get_subject(&self, class: &String, base: &String) -> Result<SubjectProxy<'_>> {
        if self.is_subject_instance(class, base).await? {
            Ok(SubjectProxy::new(self, class.clone(), base.clone()))
        } else {
            Err(anyhow!(
                "Expression {} is not a subject instance of class: {}",
                base,
                class
            ))
        }
    }

    pub async fn get_subject_classes(&self, base: &String) -> Result<Vec<String>> {
        let mut classes = Vec::new();
        if let Value::Array(classes_results) = self
            .infer(format!(
                r#"instance(C, "{}"), subject_class(Classname, C)"#,
                base
            ))
            .await?
        {
            for class in classes_results {
                if let Some(Value::String(class_name)) = class.get("Classname") {
                    classes.push(class_name.clone());
                }
            }
        }
        Ok(classes)
    }

    pub async fn execute_action(
        &self,
        action: &str,
        base: &str,
        params: Option<BTreeMap<&str, &String>>,
    ) -> Result<()> {
        let commands = parse_action(action)?;
        for command in commands {
            let mut command = command.replace("this", base);
            if let Some(ref params) = params {
                for (key, value) in params.iter() {
                    command = command.replace(key, value);
                }
            }
            match command.action.as_str() {
                "addLink" => {
                    self.add_link(
                        command.source,
                        command.target,
                        command.predicate,
                        command.status,
                    )
                    .await?;
                }
                "removeLink" => {
                    unimplemented!();
                }
                "setSingleTarget" => {
                    self.set_single_target(
                        command.source,
                        command
                            .predicate
                            .ok_or(anyhow::anyhow!("No predicate in set_single_target action"))?,
                        command.target,
                    )
                    .await?;
                }
                _ => {
                    return Err(anyhow::anyhow!("Unknown action: {}", command.action));
                }
            }
        }
        Ok(())
    }

    pub async fn get_neighbourhood_author(&self) -> Result<Option<String>> {
        Ok(None)
    }

    pub async fn get_local_agent_did(&self) -> Result<String> {
        Ok("local_agent".to_string())
    }
}

#[derive(Debug)]
struct Command {
    pub action: String,
    pub source: String,
    pub predicate: Option<String>,
    pub target: String,
    pub status: Option<String>,
}

impl Command {
    pub fn replace(&self, pattern: &str, replacement: &str) -> Command {
        Command {
            action: self.action.replace(pattern, replacement),
            source: self.source.replace(pattern, replacement),
            predicate: self
                .predicate
                .as_ref()
                .map(|f| f.replace(pattern, replacement)),
            target: self.target.replace(pattern, replacement),
            status: self
                .status
                .as_ref()
                .map(|f| f.replace(pattern, replacement)),
        }
    }
}

fn parse_action(action: &str) -> Result<Vec<Command>> {
    let action_regex = Regex::new(r"\[(?P<command>\{.*})*\]")?;

    let command_regex = Regex::new(
        r#"\{(action:\s*"(?P<action>[\S--,]+)",?\s*)|(source:\s*"(?P<source>[\S--,]+)",?\s*)|(predicate:\s*"(?P<predicate>[\S--,]+)",?\s*)|(target:\s*"(?P<target>[\S--,]+)",?\s*)|(status:\s*"(?P<status>[\S--,]+)",?\s*)\}"#,
    )?;

    let mut commands = Vec::new();
    for capture in action_regex.captures_iter(action) {
        let mut action = None;
        let mut source = None;
        let mut predicate = None;
        let mut target = None;
        let mut status = None;
        command_regex
            .captures_iter(capture.name("command").unwrap().as_str())
            .for_each(|capture| {
                action = action.or(capture.name("action").map(|e| e.as_str()));
                source = source.or(capture.name("source").map(|e| e.as_str()));
                predicate = predicate.or(capture.name("predicate").map(|e| e.as_str()));
                target = target.or(capture.name("target").map(|e| e.as_str()));
                status = status.or(capture.name("status").map(|e| e.as_str()));
            });

        commands.push(Command {
            action: action.ok_or(anyhow!("Comman without action"))?.into(),
            source: source.ok_or(anyhow!("Comman without source"))?.into(),
            predicate: predicate.map(|e| e.into()),
            target: target.ok_or(anyhow!("Comman without target"))?.into(),
            status: status.map(|e| e.into()),
        });
    }

    Ok(commands)
}
