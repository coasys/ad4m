use serde_json::Value;
use anyhow::{bail, Result};

use crate::types::{LinkExpression, Agent};

pub fn print_prolog_result(result: Value) -> Result<()> {
    match result {
        Value::Object(map) => {
            for (key, value) in map {
                let value = match value {
                    Value::String(string) => string,
                    Value::Number(number) => number.to_string(),
                    Value::Bool(boolean) => boolean.to_string(),
                    Value::Array(_) => bail!("Unexpected nested object value"),
                    Value::Object(_) => bail!("Unexpected nested object value"),
                    Value::Null => "null".to_string(),
                };
                println!("\x1b[36m{}:\x1b[97m {}", key, value);
            }
        },
        _ => bail!("Unexpected non-obhect value in prolog result: {:?}", result),
    }
    Ok(())
}

fn maybe_decode_literal(uri: String) -> String {
    if uri.starts_with("literal://") {
        let literal = uri.replace("literal://", "");
        if literal.starts_with("string:") {
            let string = literal.replace("string:", "");
            urlencoding::decode(&string).map(|s| s.to_string()).unwrap_or(string)
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
    
    if let Some(pred) = link.data.predicate {
        let predicate = maybe_decode_literal(pred);
        println!("\x1b[90m[{}] \x1b[35m{} \x1b[97m--\x1b[95m{}\x1b[97m--> \x1b[32m{} \x1b[34m({})", link.timestamp, source, predicate, target, link.author);
    } else {
        println!("\x1b[90m[{}] \x1b[35m{} \x1b[97m----> \x1b[32m{} \x1b[34m({})", link.timestamp, source, target, link.author);
    }
}

pub fn print_agent(agent: Agent) {
    println!("\x1b[36mDID: \x1b[97m{}", agent.did);
    println!("\x1b[36mDirect message language: \x1b[97m{}", agent.direct_message_language.unwrap_or("<NOT SET>".to_string()));
    println!("\x1b[36mPublic Perspective:");
    if let Some(perspective) = agent.perspective {
        for link in perspective.links {
            print_link(link);
        }
    }
}