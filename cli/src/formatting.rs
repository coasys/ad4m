use serde_json::Value;
use anyhow::{bail, Result};

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