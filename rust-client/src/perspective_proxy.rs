use std::collections::BTreeMap;

use crate::{
    literal::{Literal, LiteralValue},
    perspectives::{
        add_link::AddLinkPerspectiveAddLink, query_links::QueryLinksPerspectiveQueryLinks,
        PerspectivesClient,
    },
    subject_proxy::SubjectProxy,
    types::LinkExpression,
};
use anyhow::{anyhow, Result};
use chrono::naive::NaiveDateTime;
use regex::Regex;
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
        status: Option<String>,
    ) -> Result<AddLinkPerspectiveAddLink> {
        self.client
            .add_link(
                self.perspective_uuid.clone(),
                source,
                target,
                predicate,
                status,
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

    pub async fn add_dna(&self, name: String, dna: String, dna_type: String) -> Result<()> {
        let mut predicate = "ad4m://has_custom_dna";

        if dna_type == "subject_class" {
            predicate = "ad4m://has_subject_class"
        } else if dna_type == "flow" {
            predicate = "ad4m://has_flow"
        }

        let literal_name = Literal::from_string(name);

        let links = self
            .get(
                Some("ad4m://self".into()),
                Some(literal_name.to_url().unwrap()),
                Some(predicate.into()),
                None,
                None,
                None,
            )
            .await?;

        if links.is_empty() {
            self.set_single_target(
                "ad4m://self".into(),
                predicate.into(),
                literal_name.to_url().unwrap(),
            )
            .await?;
        }

        let literal = Literal::from_string(dna);

        self.add_link(
            literal_name.to_url().unwrap(),
            literal.to_url().unwrap(),
            Some("ad4m://sdna".into()),
            Some("shared".to_string()),
        )
        .await?;
        Ok(())
    }

    pub async fn get_dna(&self) -> Result<Vec<(String, String, Vec<String>, Vec<String>)>> {
        // First, find all the name literals that are linked from ad4m://self with SDNA predicates
        let sdna_predicates = vec![
            "ad4m://has_subject_class",
            "ad4m://has_flow", 
            "ad4m://has_custom_sdna"
        ];
        
        let mut all_sdna_code = Vec::new();
        
        for predicate in sdna_predicates {
            let links = self
                .get(
                    Some("ad4m://self".into()),
                    None,
                    Some(predicate.into()),
                    None,
                    None,
                    None,
                )
                .await?;
            
            // For each name literal found, get the actual SDNA code
            for link in links {
                let name_literal = link.data.target.clone();
                let name_author = link.author.clone();
                
                // Extract the class name from the literal
                let class_name = match Literal::from_url(name_literal.clone()) {
                    Ok(literal) => match literal.get() {
                        Ok(LiteralValue::String(name)) => name,
                        _ => continue,
                    },
                    Err(_) => continue,
                };
                
                // Now find the SDNA code linked from this name with predicate "ad4m://sdna"
                let sdna_links = self
                    .get(
                        Some(name_literal.clone()),
                        None,
                        Some("ad4m://sdna".into()),
                        None,
                        None,
                        None,
                    )
                    .await?;
                
                // Collect all unique authors for this SDNA code
                let mut code_authors = Vec::new();
                let mut sdna_codes = Vec::new();
                
                for sdna_link in sdna_links {
                    let literal = Literal::from_url(sdna_link.data.target.clone())?;
                    match literal.get() {
                        Ok(LiteralValue::String(string)) => {
                            sdna_codes.push(string);
                            if !code_authors.contains(&sdna_link.author) {
                                code_authors.push(sdna_link.author.clone());
                            }
                        },
                        _ => {
                            // Skip non-string literals
                            continue;
                        }
                    }
                }
                
                // Add each unique SDNA code with its authorship information
                for sdna_code in sdna_codes {
                    all_sdna_code.push((class_name.clone(), sdna_code, vec![name_author.clone()], code_authors.clone()));
                }
            }
        }
        
        Ok(all_sdna_code)
    }

    pub async fn get_dna_for_class(&self, class_name: &str) -> Result<Option<(String, String, Vec<String>, Vec<String>)>> {
        // First, find the name literal for this class that is linked from ad4m://self
        let name_literal = Literal::from_string(class_name.to_string());
        
        let links = self
            .get(
                Some("ad4m://self".into()),
                Some(name_literal.to_url().unwrap()),
                Some("ad4m://has_subject_class".into()),
                None,
                None,
                None,
            )
            .await?;
        
        if links.is_empty() {
            return Ok(None);
        }
        
        // Collect all unique name authors
        let mut name_authors = Vec::new();
        for link in &links {
            if !name_authors.contains(&link.author) {
                name_authors.push(link.author.clone());
            }
        }
        
        // Now find the SDNA code linked from this name with predicate "ad4m://sdna"
        let sdna_links = self
            .get(
                Some(name_literal.to_url().unwrap()),
                None,
                Some("ad4m://sdna".into()),
                None,
                None,
                None,
            )
            .await?;
        
        if sdna_links.is_empty() {
            return Ok(None);
        }
        
        // Collect all unique code authors and find the first SDNA code
        let mut code_authors = Vec::new();
        let mut first_sdna_code = None;
        
        for sdna_link in sdna_links {
            if !code_authors.contains(&sdna_link.author) {
                code_authors.push(sdna_link.author.clone());
            }
            
            // Get the first SDNA code we find
            if first_sdna_code.is_none() {
                let literal = Literal::from_url(sdna_link.data.target.clone())?;
                match literal.get() {
                    Ok(LiteralValue::String(string)) => {
                        first_sdna_code = Some(string);
                    },
                    _ => continue,
                }
            }
        }
        
        if let Some(sdna_code) = first_sdna_code {
            Ok(Some((class_name.to_string(), sdna_code, name_authors, code_authors)))
        } else {
            Ok(None)
        }
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
                Some("shared".to_string()),
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
            //println!("{:?}", results);
            if let Some(Value::Object(action)) = results.first() {
                //println!("{:?}", action);
                if let Value::String(action_string) = action
                    .get("Action")
                    .ok_or(anyhow::anyhow!("Unbound variable Action is not set"))?
                {
                    //println!("{}", action_string);
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

    pub async fn get_subject(&self, class: &String, base: &String) -> Result<SubjectProxy> {
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
                    //println!("addLink: {:?}", command);
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
                    //let links = self.get(Some(source), Some(target), predicate, None, None, None).await?;
                    //elf.remove_link(source, target.into(), predicate.into()).await?;
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
        // This would need to be implemented in the client to get the neighborhood author
        // For now, we'll return None and implement this later
        Ok(None)
    }

    pub async fn get_local_agent_did(&self) -> Result<String> {
        // This would need to be implemented in the client to get the local agent's DID
        // For now, we'll return a placeholder
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

    // This parses strings like:
    // {action: "<action>", source: "<source>", predicate: "<predicate>", target: "<target>", status: "<status>"}
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
        //println!("{:?}", commands);
    }

    Ok(commands)
}
