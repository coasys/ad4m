use serde_json::Value;
use anyhow::{bail, Result};

use crate::types::LinkExpression;

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

pub fn print_link(link: LinkExpression) {
    if let Some(pred) = link.data.predicate {
        print!("\x1b[90m[{}] \x1b[35m{} \x1b[97m--\x1b[95m{}\x1b[97m--> \x1b[32m{} \x1b[34m({})", link.timestamp, link.data.source, pred, link.data.target, link.author);
    } else {
        println!("\x1b[90m[{}] \x1b[35m{} \x1b[97m----> \x1b[32m{} \x1b[34m({})", link.timestamp, link.data.source, link.data.target, link.author);
    }
}