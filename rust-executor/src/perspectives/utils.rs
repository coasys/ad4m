use scryer_prolog::{QueryMatch, QueryResolution, Value};

fn sanitize_into_json(s: String) -> String {
    match s.as_str() {
        "true" => String::from("true"),
        "false" => String::from("false"),
        _ => {
            //try unescaping an escaped json string
            let wrapped_s = format!("\"{}\"", s);
            if let Ok(json_value) = serde_json::from_str::<serde_json::Value>(wrapped_s.as_str()) {
                json_value.to_string()
            } else {
                // try fixing wrong \' escape sequences:
                let fixed_s = wrapped_s.replace("\\'", "'");
                if let Ok(json_value) = serde_json::from_str::<serde_json::Value>(fixed_s.as_str())
                {
                    json_value.to_string()
                } else {
                    //treat as string literal
                    //escape double quotes
                    format!(
                        "\"{}\"",
                        s.replace('"', "\\\"")
                            .replace('\n', "\\n")
                            .replace('\t', "\\t")
                            .replace('\r', "\\r")
                    )
                }
            }
        }
    }
}
pub fn prolog_value_to_json_string(value: Value) -> String {
    match value {
        Value::Integer(i) => format!("{}", i),
        Value::Float(f) => format!("{}", f),
        Value::Rational(r) => format!("{}", r),
        Value::Atom(a) => sanitize_into_json(a),
        Value::String(s) => sanitize_into_json(s),
        Value::List(l) => {
            let mut string_result = "[".to_string();
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_result.push_str(", ");
                }
                string_result.push_str(&prolog_value_to_json_string(v.clone()));
            }
            string_result.push(']');
            string_result
        }
        Value::Structure(s, l) => {
            let mut string_result = format!("\"{}\": [", s.as_str());
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_result.push_str(", ");
                }
                string_result.push_str(&prolog_value_to_json_string(v.clone()));
            }
            string_result.push(']');
            string_result
        }
        _ => "null".to_string(),
    }
}

fn prolog_match_to_json_string(query_match: &QueryMatch) -> String {
    let mut string_result = "{".to_string();
    for (i, (k, v)) in query_match.bindings.iter().enumerate() {
        if i > 0 {
            string_result.push_str(", ");
        }
        string_result.push_str(&format!(
            "\"{}\": {}",
            k,
            prolog_value_to_json_string(v.clone())
        ));
    }
    string_result.push('}');
    string_result
}

pub fn prolog_resolution_to_string(resultion: QueryResolution) -> String {
    match resultion {
        QueryResolution::True => "true".to_string(),
        QueryResolution::False => "false".to_string(),
        QueryResolution::Matches(matches) => {
            let matches_json: Vec<String> =
                matches.iter().map(prolog_match_to_json_string).collect();
            format!("[{}]", matches_json.join(", "))
        }
    }
}

pub fn prolog_get_first_string_binding(
    result: &QueryResolution,
    variable_name: &str,
) -> Option<String> {
    prolog_get_all_string_bindings(result, variable_name)
        .into_iter()
        .next()
}

pub fn prolog_get_all_string_bindings(
    result: &QueryResolution,
    variable_name: &str,
) -> Vec<String> {
    if let QueryResolution::Matches(matches) = result {
        matches
            .iter()
            .filter_map(|m| m.bindings.get(variable_name))
            .filter_map(|value| match value {
                scryer_prolog::Value::String(s) => Some(s),
                scryer_prolog::Value::Atom(s) => Some(s),
                _ => None,
            })
            .cloned()
            .collect()
    } else {
        Vec::new()
    }
}

pub fn prolog_get_first_binding(
    result: &QueryResolution,
    variable_name: &str,
) -> Option<scryer_prolog::Value> {
    prolog_get_all_bindings(result, variable_name)
        .into_iter()
        .next()
}

pub fn prolog_get_all_bindings(
    result: &QueryResolution,
    variable_name: &str,
) -> Vec<scryer_prolog::Value> {
    if let QueryResolution::Matches(matches) = result {
        matches
            .iter()
            .filter_map(|m| m.bindings.get(variable_name))
            .cloned()
            .collect()
    } else {
        Vec::new()
    }
}
