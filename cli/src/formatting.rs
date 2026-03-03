use anyhow::{bail, Result};
use serde_json::Value;

use crate::types::{Agent, LinkExpression, PerspectiveExpression, SentPerspectiveMessage};

pub fn print_prolog_results(results: Value) -> Result<()> {
    match results {
        Value::Bool(true) => println!("true ✅"),
        Value::Bool(false) => println!("false ❌"),
        Value::String(string) => println!("{}", string),
        Value::Array(array) => {
            println!("\x1b[90m{} results:", array.len());
            let mut i = 1;
            for item in array {
                println!("\x1b[90m{}:", i);
                print_prolog_result(item)?;
                println!("====================");
                i += 1;
            }
        }
        _ => bail!("Unexpected result value in response of run_infer()"),
    }

    Ok(())
}

fn deconstruct_value_object(value: Value) -> Result<String> {
    if let Value::Object(map) = value {
        let mut collected = vec![];
        for (key, value) in map {
            let value = match value {
                Value::String(string) => string,
                Value::Number(number) => number.to_string(),
                Value::Bool(boolean) => boolean.to_string(),
                Value::Array(array) => {
                    let array_items: Result<Vec<String>> = array
                        .into_iter()
                        .map(|item| deconstruct_value(item))
                        .collect();
                    format!("[{}]", array_items?.join(", "))
                }
                Value::Object(_) => deconstruct_value_object(value)?,
                Value::Null => "null".to_string(),
            };
            collected.push(format!("{}: {}", key, value));
        }

        Ok(collected.join(", "))
    } else {
        bail!("Can't deconstruct non-object value")
    }
}

fn deconstruct_value(value: Value) -> Result<String> {
    match value {
        Value::String(string) => Ok(string),
        Value::Number(number) => Ok(number.to_string()),
        Value::Bool(boolean) => Ok(boolean.to_string()),
        Value::Array(array) => {
            let array_items: Result<Vec<String>> = array
                .into_iter()
                .map(|item| deconstruct_value(item))
                .collect();
            Ok(format!("[{}]", array_items?.join(", ")))
        }
        Value::Object(_) => deconstruct_value_object(value),
        Value::Null => Ok("null".to_string()),
    }
}

pub fn print_prolog_result(result: Value) -> Result<()> {
    match result {
        Value::Object(map) => {
            for (key, value) in map {
                let value = match value {
                    Value::String(string) => string,
                    Value::Number(number) => number.to_string(),
                    Value::Bool(boolean) => boolean.to_string(),
                    Value::Array(array) => {
                        let array_items: Result<Vec<String>> = array
                            .into_iter()
                            .map(|item| deconstruct_value(item))
                            .collect();
                        format!("[{}]", array_items?.join(", "))
                    }
                    Value::Object(_) => deconstruct_value_object(value)?,
                    Value::Null => "null".to_string(),
                };
                println!("\x1b[36m{}:\x1b[97m {}", key, value);
            }
        }
        _ => bail!("Unexpected non-object value in prolog result: {:?}", result),
    }
    Ok(())
}

fn maybe_decode_literal(uri: String) -> String {
    if uri.starts_with("literal://") {
        let literal = uri.replace("literal://", "");
        if literal.starts_with("string:") {
            let string = literal.replace("string:", "");
            urlencoding::decode(&string)
                .map(|s| s.to_string())
                .unwrap_or(string)
        } else if literal.starts_with("number:") {
            literal.replace("number:", "")
        } else if literal.starts_with("json:") {
            let json = literal.replace("json:", "");
            if let Ok(decoded_json) = urlencoding::decode(&json) {
                let decoded_json_string = decoded_json.to_string().replace("\\'", "'");
                match serde_json::from_str::<serde_json::Value>(&decoded_json_string) {
                    Ok(expression) => return expression["data"].to_string(),
                    Err(e) => {
                        println!("Failed to decode json literal: {}", e);
                        return decoded_json.to_string();
                    }
                }
            } else {
                println!("Failed to decode url encoding");
            }
            json
        } else {
            literal
        }
    } else {
        uri
    }
}

pub fn print_link(link: LinkExpression) {
    let source = maybe_decode_literal(link.data.source);
    let target = maybe_decode_literal(link.data.target);

    match link.data.predicate {
        Some(pred) => {
            let predicate = maybe_decode_literal(pred);
            println!(
                "\x1b[90m[{}] \x1b[35m{} \x1b[97m--\x1b[95m{}\x1b[97m--> \x1b[32m{} \x1b[34m({})",
                link.timestamp, source, predicate, target, link.author
            );
        }
        None => {
            println!(
                "\x1b[90m[{}] \x1b[35m{} \x1b[97m----> \x1b[32m{} \x1b[34m({})",
                link.timestamp, source, target, link.author
            );
        }
    }
}

pub fn print_agent(agent: Agent) {
    println!("\x1b[36mDID: \x1b[97m{}", agent.did);
    println!(
        "\x1b[36mDirect message language: \x1b[97m{}",
        agent
            .direct_message_language
            .unwrap_or_else(|| "<NOT SET>".to_string())
    );
    println!("\x1b[36mPublic Perspective:");
    if let Some(perspective) = agent.perspective {
        for link in perspective.links {
            print_link(link);
        }
    }
}

pub fn print_message_perspective(perspective: PerspectiveExpression) {
    println!("\x1b[36mFrom: {}", perspective.author);
    println!("\x1b[36mTimestamp: {}", perspective.timestamp);
    for link in perspective.data.links {
        print_link(link);
    }
}

pub fn print_sent_message_perspective(sent: SentPerspectiveMessage) {
    println!("\x1b[36mTo: {}", sent.recipient);
    println!("\x1b[36mTimestamp: {}", sent.message.timestamp);
    for link in sent.message.data.links {
        print_link(link);
    }
}
