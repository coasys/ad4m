use scryer_prolog::machine::parsed_results::{Value, QueryMatch, QueryResolution};

pub fn prolog_value_to_json_tring(value: Value) -> String {
    match value {
        Value::Integer(i) => format!("{}", i),
        Value::Float(f) => format!("{}", f),
        Value::Rational(r) => format!("{}", r),
        Value::Atom(a) => format!("{}", a.as_str()),
        Value::String(s) => 
            if let Err(_e) = serde_json::from_str::<serde_json::Value>(s.as_str()) {
                //treat as string literal
                //escape double quotes
                format!("\"{}\"", s
                    .replace("\"", "\\\"")
                    .replace("\n", "\\n")
                    .replace("\t", "\\t")
                    .replace("\r", "\\r"))
            } else {
                //return valid json string
                s
            },
        Value::List(l) => {
            let mut string_result = "[".to_string();
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_result.push_str(", ");
                }
                string_result.push_str(&prolog_value_to_json_tring(v.clone()));
            }
            string_result.push_str("]");
            string_result
        }
        Value::Structure(s, l) => {
            let mut string_result = format!("\"{}\": [", s.as_str());
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_result.push_str(", ");
                }
                string_result.push_str(&prolog_value_to_json_tring(v.clone()));
            }
            string_result.push_str("]");
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
        string_result.push_str(&format!("\"{}\": {}", k, prolog_value_to_json_tring(v.clone())));
    }
    string_result.push_str("}");
    string_result
}

pub fn prolog_resolution_to_string(resultion: QueryResolution) -> String {
    match resultion {
        QueryResolution::True => "true".to_string(),
        QueryResolution::False => "false".to_string(),
        QueryResolution::Matches(matches) => {
            let matches_json: Vec<String> = matches
                .iter()
                .map(|m| prolog_match_to_json_string(m))
                .collect();
            format!("[{}]", matches_json.join(", "))
        }
    }
}