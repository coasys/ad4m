use crate::prolog_service::types::{QueryMatch, QueryResolution};
use scryer_prolog::Term;

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

fn convert_assoc_to_json(_functor: &str, args: &[Term]) -> String {
    let mut pairs = Vec::new();
    collect_assoc_pairs(args, &mut pairs);
    
    let json_pairs: Vec<String> = pairs.iter()
        .map(|(k, v)| format!("\"{}\": {}", k, prolog_value_to_json_string(v.clone())))
        .collect();
    format!("{{{}}}", json_pairs.join(", "))
}

fn collect_assoc_pairs(args: &[Term], pairs: &mut Vec<(String, Term)>) {
    if args.len() >= 4 {
        // Pattern: -(Key, Value, Left, Right)
        if let Some(key) = get_string(&args[0]) {
            pairs.push((key, args[1].clone()));
        }
        
        // Recursively collect from subtrees
        for subtree in &args[2..4] {
            if let Term::Compound(f, inner_args) = subtree {
                if f == "-" { collect_assoc_pairs(inner_args, pairs); }
            }
        }
    }
}

fn parse_t_compound_to_json(args: &[Term]) -> String {
    let mut pairs = Vec::new();
    collect_t_pairs(args, &mut pairs);
    
    let json_pairs: Vec<String> = pairs.iter()
        .map(|(k, v)| format!("\"{}\": {}", k, v))
        .collect();
    format!("{{{}}}", json_pairs.join(", "))
}

fn collect_t_pairs(args: &[Term], pairs: &mut Vec<(String, String)>) {
    if args.len() < 2 { return; }
    
    // Extract first key-value pair
    if let (Some(key), Some(value)) = (get_string(&args[0]), get_non_separator_value(&args[1])) {
        pairs.push((key, value));
    }
    
    // Recursively process nested t(...) compounds
    args.iter().skip(2)
        .filter_map(|term| match term {
            Term::Compound(f, inner_args) if f == "t" => Some(inner_args.as_slice()),
            _ => None,
        })
        .for_each(|inner_args| collect_t_pairs(inner_args, pairs));
}

fn get_string(term: &Term) -> Option<String> {
    match term {
        Term::String(s) | Term::Atom(s) if !matches!(s.as_str(), "t" | "-" | "<" | ">") => Some(s.clone()),
        _ => None,
    }
}

fn get_non_separator_value(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) if matches!(a.as_str(), "t" | "-" | "<" | ">") => None,
        _ => Some(prolog_value_to_json_string(term.clone())),
    }
}


fn convert_internal_structure_to_json(_functor: &str, args: &[Term]) -> String {
    // Handle internal Prolog structures with alternating key-value pairs
    let pairs: Vec<String> = args.chunks(2)
        .filter_map(|chunk| {
            if chunk.len() == 2 {
                let key = get_string(&chunk[0])?;
                let value = get_non_separator_value(&chunk[1])?;
                Some(format!("\"{}\": {}", key, value))
            } else {
                None
            }
        })
        .collect();
    
    format!("{{{}}}", pairs.join(", "))
}

pub fn prolog_value_to_json_string(value: Term) -> String {
    match value {
        Term::Integer(i) => format!("{}", i),
        Term::Float(f) => format!("{}", f),
        Term::Rational(r) => format!("{}", r),
        Term::Atom(a) => sanitize_into_json(a),
        Term::String(s) => sanitize_into_json(s),
        Term::List(l) => {
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
        Term::Compound(s, l) => {
            // Handle compound terms with functor "t" as JSON objects
            // Pattern: t(key1, value1, separator, key2, value2, separator, ...)
            // where separators are "<", "-", or "t" atoms
            if s.as_str() == "t" {
                return parse_t_compound_to_json(&l);
            } else if s.as_str() == "-" {
                // Handle Prolog assoc dictionary structures
                // Pattern: -(Key, Value, Left, Right) where Left and Right are subtrees
                // For JSON conversion, we need to flatten this tree structure
                return convert_assoc_to_json(&s, &l);
            } else if s.as_str() == "<" || s.as_str() == ">" {
                // Handle other Prolog internal structures that should be treated as objects
                // These are likely from the JSON parser's internal representation
                // Convert them to proper JSON objects
                return convert_internal_structure_to_json(&s, &l);
            } else {
                // For other compound terms, use the old format
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
                Term::String(s) => Some(s),
                Term::Atom(s) => Some(s),
                _ => None,
            })
            .cloned()
            .collect()
    } else {
        Vec::new()
    }
}

pub fn prolog_get_first_binding(result: &QueryResolution, variable_name: &str) -> Option<Term> {
    prolog_get_all_bindings(result, variable_name)
        .into_iter()
        .next()
}

pub fn prolog_get_all_bindings(result: &QueryResolution, variable_name: &str) -> Vec<Term> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use scryer_prolog::Term;

    #[test]
    fn test_prolog_value_to_json_string_simple_values() {
        assert_eq!(prolog_value_to_json_string(Term::Integer(42.into())), "42");
        assert_eq!(prolog_value_to_json_string(Term::Float(3.14)), "3.14");
        assert_eq!(prolog_value_to_json_string(Term::Atom("hello".to_string())), "\"hello\"");
        assert_eq!(prolog_value_to_json_string(Term::String("world".to_string())), "\"world\"");
    }

    #[test]
    fn test_prolog_value_to_json_string_list() {
        let term = Term::List(vec![
            Term::String("a".to_string()),
            Term::String("b".to_string()),
            Term::Integer(42.into())
        ]);
        assert_eq!(prolog_value_to_json_string(term), "[\"a\", \"b\", 42]");
    }

    #[test]
    fn test_t_compound_simple_object() {
        // Simple object: t("name", "Alice")
        let term = Term::Compound("t".to_string(), vec![
            Term::String("name".to_string()),
            Term::String("Alice".to_string())
        ]);
        assert_eq!(prolog_value_to_json_string(term), r#"{"name": "Alice"}"#);
    }

    #[test]
    fn test_t_compound_with_terminators() {
        // Object with terminators: t("name", "Alice", "-", "t", "t")
        let term = Term::Compound("t".to_string(), vec![
            Term::String("name".to_string()),
            Term::String("Alice".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        assert_eq!(prolog_value_to_json_string(term), r#"{"name": "Alice"}"#);
    }

    #[test]
    fn test_t_compound_linked_list_structure() {
        // Test the linked list structure from debug logs:
        // t("created", "2025-09-22T11:00:00Z", "-", t("author", "Bob", "-", "t", "t"), t("views", 15, "-", "t", "t"))
        
        let author_node = Term::Compound("t".to_string(), vec![
            Term::String("author".to_string()),
            Term::String("Bob".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let views_node = Term::Compound("t".to_string(), vec![
            Term::String("views".to_string()),
            Term::Integer(15.into()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let main_term = Term::Compound("t".to_string(), vec![
            Term::String("created".to_string()),
            Term::String("2025-09-22T11:00:00Z".to_string()),
            Term::Atom("-".to_string()),
            author_node,
            views_node
        ]);
        
        let result = prolog_value_to_json_string(main_term);
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Valid JSON");
        
        assert_eq!(parsed["created"], "2025-09-22T11:00:00Z");
        assert_eq!(parsed["author"], "Bob");
        assert_eq!(parsed["views"], 15);
        assert_eq!(parsed.as_object().unwrap().len(), 3);
    }

    #[test]
    fn test_t_compound_with_separators() {
        // Test structure with "<" separators: t("location", "San Francisco", "<", t("bio", "Developer", "-", "t", "t"), "t")
        let bio_node = Term::Compound("t".to_string(), vec![
            Term::String("bio".to_string()),
            Term::String("Developer".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let main_term = Term::Compound("t".to_string(), vec![
            Term::String("location".to_string()),
            Term::String("San Francisco".to_string()),
            Term::Atom("<".to_string()),
            bio_node,
            Term::Atom("t".to_string())
        ]);
        
        let result = prolog_value_to_json_string(main_term);
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Valid JSON");
        
        assert_eq!(parsed["location"], "San Francisco");
        assert_eq!(parsed["bio"], "Developer");
        assert_eq!(parsed.as_object().unwrap().len(), 2);
    }

    #[test]
    fn test_t_compound_deeply_nested() {
        // Test deeply nested structure with multiple levels
        let leaf_node = Term::Compound("t".to_string(), vec![
            Term::String("deep".to_string()),
            Term::String("value".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let mid_node = Term::Compound("t".to_string(), vec![
            Term::String("nested".to_string()),
            Term::String("data".to_string()),
            Term::Atom("-".to_string()),
            leaf_node,
            Term::Atom("t".to_string())
        ]);
        
        let root_node = Term::Compound("t".to_string(), vec![
            Term::String("root".to_string()),
            Term::String("level".to_string()),
            Term::Atom("-".to_string()),
            mid_node,
            Term::Atom("t".to_string())
        ]);
        
        let result = prolog_value_to_json_string(root_node);
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Valid JSON");
        
        assert_eq!(parsed["root"], "level");
        assert_eq!(parsed["nested"], "data");
        assert_eq!(parsed["deep"], "value");
        assert_eq!(parsed.as_object().unwrap().len(), 3);
    }

    #[test]
    fn test_t_compound_mixed_types() {
        // Test with mixed value types
        let string_node = Term::Compound("t".to_string(), vec![
            Term::String("name".to_string()),
            Term::String("Alice".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let number_node = Term::Compound("t".to_string(), vec![
            Term::String("count".to_string()),
            Term::Integer(42.into()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let main_term = Term::Compound("t".to_string(), vec![
            Term::String("active".to_string()),
            Term::Atom("true".to_string()),
            Term::Atom("-".to_string()),
            string_node,
            number_node
        ]);
        
        let result = prolog_value_to_json_string(main_term);
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Valid JSON");
        
        assert_eq!(parsed["active"], true);
        assert_eq!(parsed["name"], "Alice");
        assert_eq!(parsed["count"], 42);
        assert_eq!(parsed.as_object().unwrap().len(), 3);
    }

    #[test]
    fn test_prolog_assoc_dictionary() {
        // Test Prolog assoc structures: -(key, value, left_tree, right_tree)
        let simple_assoc = Term::Compound("-".to_string(), vec![
            Term::String("author".to_string()),
            Term::String("Alice".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let result = prolog_value_to_json_string(simple_assoc);
        assert_eq!(result, r#"{"author": "Alice"}"#);
    }

    #[test]
    fn test_prolog_value_to_json_string_non_t_compound() {
        // Test compound terms with functors other than "t" should use the old format
        let term = Term::Compound(
            "some_functor".to_string(),
            vec![
                Term::String("arg1".to_string()),
                Term::Integer(42.into())
            ]
        );
        let result = prolog_value_to_json_string(term);
        assert_eq!(result, r#""some_functor": ["arg1", 42]"#);
    }

    #[test]
    fn test_prolog_assoc_complex_tree() {
        // Test complex assoc binary tree structure
        let left_subtree = Term::Compound("-".to_string(), vec![
            Term::String("created".to_string()),
            Term::String("2025-09-22T10:00:00Z".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let right_subtree = Term::Compound("-".to_string(), vec![
            Term::String("views".to_string()),
            Term::Integer(42.into()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let complex_assoc = Term::Compound("-".to_string(), vec![
            Term::String("author".to_string()),
            Term::String("Alice".to_string()),
            left_subtree,
            right_subtree
        ]);
        
        let result = prolog_value_to_json_string(complex_assoc);
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Valid JSON");
        
        assert_eq!(parsed["author"], "Alice");
        assert_eq!(parsed["created"], "2025-09-22T10:00:00Z");
        assert_eq!(parsed["views"], 42);
        assert_eq!(parsed.as_object().unwrap().len(), 3);
    }

    #[test]
    fn test_prolog_value_to_json_string_real_debug_case() {
        // Test the exact structure from the debug logs
        // Compound("t", [String("created"), String("2025-09-22T11:00:00Z"), Atom("-"), 
        //                Compound("t", [String("author"), String("Bob"), Atom("-"), Atom("t"), Atom("t")]), 
        //                Compound("t", [String("views"), Integer(15), Atom("-"), Atom("t"), Atom("t")])])
        
        let author_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("author".to_string()),
                Term::String("Bob".to_string()),
                Term::Atom("-".to_string()),
                Term::Atom("t".to_string()),
                Term::Atom("t".to_string())
            ]
        );
        
        let views_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("views".to_string()),
                Term::Integer(15.into()),
                Term::Atom("-".to_string()),
                Term::Atom("t".to_string()),
                Term::Atom("t".to_string())
            ]
        );
        
        let main_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("created".to_string()),
                Term::String("2025-09-22T11:00:00Z".to_string()),
                Term::Atom("-".to_string()),
                author_compound,
                views_compound
            ]
        );
        
        let result = prolog_value_to_json_string(main_compound);
        println!("Real debug case result: {}", result);
        
        // This should produce: {"created": "2025-09-22T11:00:00Z", "author": "Bob", "views": 15}
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        assert_eq!(parsed["created"], "2025-09-22T11:00:00Z");
        assert_eq!(parsed["author"], "Bob");
        assert_eq!(parsed["views"], 15);
        
        // Should contain all three properties
        if let Some(obj) = parsed.as_object() {
            assert_eq!(obj.len(), 3, "Should contain exactly 3 properties");
        }
    }

    #[test]
    fn test_non_t_compound_fallback() {
        // Test that non-"t" compounds use the legacy format
        let term = Term::Compound("some_functor".to_string(), vec![
            Term::String("arg1".to_string()),
            Term::Integer(42.into())
        ]);
        assert_eq!(prolog_value_to_json_string(term), r#""some_functor": ["arg1", 42]"#);
    }

    #[test]
    fn test_complex_expression_like_structure() {
        // Test the full Expression object structure using proper linked list format
        // This simulates how a complex Expression object gets parsed by Prolog
        
        // Create nested compounds for the "data" object using linked list structure
        let data_author = Term::Compound("t".to_string(), vec![
            Term::String("author".to_string()),
            Term::String("Alice".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let data_views = Term::Compound("t".to_string(), vec![
            Term::String("views".to_string()),
            Term::Integer(42.into()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let data_compound = Term::Compound("t".to_string(), vec![
            Term::String("created".to_string()),
            Term::String("2025-09-22T10:00:00Z".to_string()),
            Term::Atom("-".to_string()),
            data_author,
            data_views
        ]);
        
        // Create proof compound
        let proof_compound = Term::Compound("t".to_string(), vec![
            Term::String("key".to_string()),
            Term::String("did:key:test#test".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        // Create the main Expression object using linked list structure
        let timestamp_node = Term::Compound("t".to_string(), vec![
            Term::String("timestamp".to_string()),
            Term::String("2025-09-23T19:01:08.488Z".to_string()),
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let data_node = Term::Compound("t".to_string(), vec![
            Term::String("data".to_string()),
            data_compound,
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let proof_node = Term::Compound("t".to_string(), vec![
            Term::String("proof".to_string()),
            proof_compound,
            Term::Atom("-".to_string()),
            Term::Atom("t".to_string()),
            Term::Atom("t".to_string())
        ]);
        
        let expression_compound = Term::Compound("t".to_string(), vec![
            Term::String("author".to_string()),
            Term::String("did:key:z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf".to_string()),
            Term::Atom("-".to_string()),
            timestamp_node,
            data_node,
            proof_node
        ]);
        
        let result = prolog_value_to_json_string(expression_compound);
        
        // Parse the result to make sure it's valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        
        // Verify the structure - this tests the full Expression object parsing
        assert_eq!(parsed["author"], "did:key:z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf");
        assert_eq!(parsed["data"]["author"], "Alice");
        assert_eq!(parsed["data"]["created"], "2025-09-22T10:00:00Z");
        assert_eq!(parsed["data"]["views"], 42);
        assert!(parsed["proof"]["key"].as_str().unwrap().contains("did:key:"));
    }
}
