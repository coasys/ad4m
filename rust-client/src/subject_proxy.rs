use std::collections::BTreeMap;
use maplit::btreemap;
use anyhow::{anyhow, Result};
use serde_json::Value;

use crate::perspective_proxy::PerspectiveProxy;

fn prolog_list_to_array(list: &Value) -> Vec<String> {
    if let Some(Value::String(head)) = list.get("head") {
        let mut values = vec![head.clone()];
        if let Some(tail) = list.get("tail") {
            values.extend(prolog_list_to_array(tail));
        }
        values
    } else {
        vec![]
    }
}

pub struct SubjectProxy<'a> {
    perspective: &'a PerspectiveProxy,
    subject_class: String,
    base: String
}

impl <'a> SubjectProxy<'a> {
    pub fn new(perspective: &'a PerspectiveProxy, subject_class: String, base: String) -> Self {
        Self {
            perspective,
            subject_class,
            base
        }
    }

    async fn query_to_array(&self, query: String) -> Result<Vec<String>> {
        let result = self.perspective.infer(query.clone()).await?;
        let mut values = Vec::new();
        if let Some(result_array) = result.as_array() {
            for p in result_array {
                if let Some(Value::String(p)) = p.get("Value") {
                    values.push(p.clone());
                }
            }
            Ok(values)
        } else {
            Err(anyhow!("No results found running query: {}", query))
        }
    }

    pub async fn property_names(&self) -> Result<Vec<String>> {
        self
            .query_to_array(format!(r#"subject_class("{}", C), property(C, Value)"#, self.subject_class))
            .await
            .or_else(|_| Ok(Vec::new()))
    }

    pub async fn get_property_values(&self) -> Result<BTreeMap<String, String>> {
        let mut values = BTreeMap::new();
        let properties = self.property_names().await?;
        for p in properties {
            let query = format!(
                r#"subject_class("{}", C), property_getter(C, "{}", "{}", Value)"#, 
                self.subject_class,
                self.base,
                p
            );
            let result = self.perspective.infer(query).await?;

            if let Some(result_array) = result.as_array() {
                if let Some(Value::String(value)) = result_array[0].get("Value") {
                    values.insert(p, value.clone());
                }
            }
        }
        Ok(values)
    }

    pub async fn collection_names(&self) -> Result<Vec<String>> {
        self
            .query_to_array(format!(r#"subject_class("{}", C), collection(C, Value)"#, self.subject_class))
            .await
            .or_else(|_| Ok(Vec::new()))
    }

    pub async fn get_collection_values(&self) -> Result<BTreeMap<String, Vec<String>>> {
        let mut values = BTreeMap::new();
        let collections = self.collection_names().await?;
        for c in collections {
            let query = format!(
                r#"subject_class("{}", C), collection_getter(C, "{}", "{}", Value)"#, 
                self.subject_class,
                self.base,
                c
            );
            let result = self.perspective.infer(query).await?;

            if let Some(result_array) = result.as_array() {
                let mut collection_values = Vec::new();
                for p in result_array {
                    println!("{:?}", p.get("Value"));
                    let value = p.get("Value");
                    match value {
                        Some(Value::String(value)) => {
                            collection_values.push(value.clone());
                        },
                        Some(Value::Object(_)) => {
                            collection_values.extend(prolog_list_to_array(value.as_ref().unwrap()));
                        },
                        Some(Value::Array(value)) => {
                            collection_values.extend(
                                value
                                    .iter()
                                    .map(|v| v.as_str().unwrap().to_string())
                                    .collect::<Vec<String>>()
                            );
                        }
                        _ => {}
                    }
                }
                values.insert(c, collection_values);
            }
        }
        Ok(values)
    }

    pub async fn set_property(&self, property: &String, value: &String) -> Result<()> {
        let query = format!(
            r#"subject_class("{}", C), property_setter(C, "{}", Action)"#, 
            self.subject_class,
            property,
        );
        let result = self.perspective.infer(query).await?;
        if let Some(result_array) = result.as_array() {
            if let Some(Value::String(action)) = result_array[0].get("Action") {
                self.perspective.execute_action(action, &self.base, Some(btreemap!{"value" => value})).await?;
            }
        } else {
            return Err(anyhow!(r#"No property_setter found for property "{}" on class "{}""#, property, self.subject_class));
        }
        Ok(())
    }

    pub async fn add_collection(&self, collection: &String, new_element: &String) -> Result<()> {
        let query = format!(
            r#"subject_class("{}", C), collection_adder(C, "{}", Action)"#, 
            self.subject_class,
            collection,
        );
        let result = self.perspective.infer(query).await?;
        if let Some(result_array) = result.as_array() {
            if let Some(Value::String(action)) = result_array[0].get("Action") {
                self.perspective.execute_action(action, &self.base, Some(btreemap!{"value" => new_element})).await?;
            }
        } else {
            return Err(anyhow!(r#"No collection_adder found for collection "{}" on class "{}""#, collection, self.subject_class));
        }
        Ok(())
    }
}
