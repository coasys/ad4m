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

fn convert_assoc_to_json(functor: &str, args: &[Term]) -> String {
    // Handle Prolog assoc dictionary structures
    // The assoc library in Prolog creates binary trees with functor "-"
    // Pattern: -(Key, Value, Left, Right) where Left and Right are subtrees
    
    if functor != "-" {
        return "{}".to_string();
    }
    
    // Collect all key-value pairs from the binary tree
    let mut pairs = Vec::new();
    collect_assoc_pairs(&Term::Compound(functor.to_string(), args.to_vec()), &mut pairs);
    
    // Convert to JSON object
    let mut result = "{".to_string();
    for (i, (key, value)) in pairs.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format!("\"{}\": {}", key, prolog_value_to_json_string(value.clone())));
    }
    result.push('}');
    result
}

fn collect_assoc_pairs(term: &Term, pairs: &mut Vec<(String, Term)>) {
    match term {
        Term::Compound(functor, args) if functor == "-" && args.len() >= 4 => {
            // Pattern: -(Key, Value, Left, Right)
            let key = match &args[0] {
                Term::String(s) => s.clone(),
                Term::Atom(s) => s.clone(),
                _ => "unknown".to_string(),
            };
            let value = args[1].clone();
            pairs.push((key, value));
            
            // Recursively collect from left and right subtrees
            collect_assoc_pairs(&args[2], pairs);
            collect_assoc_pairs(&args[3], pairs);
        }
        Term::Atom(a) if a == "t" => {
            // Base case: empty tree (represented as atom 't')
            return;
        }
        _ => {
            // Handle unexpected structure - ignore
        }
    }
}

fn parse_t_compound_to_json(args: &[Term]) -> String {
    let pairs = extract_all_key_value_pairs(args);
    build_json_object(pairs)
}

fn extract_all_key_value_pairs(args: &[Term]) -> Vec<(String, String)> {
    let mut pairs = Vec::new();
    extract_t_compound_pairs(args, &mut pairs);
    pairs
}

fn extract_t_compound_pairs(args: &[Term], pairs: &mut Vec<(String, String)>) {
    if args.len() < 2 {
        return;
    }
    
    // Extract first key-value pair if valid
    if let (Some(key), Some(value)) = (extract_key(&args[0]), extract_value(&args[1])) {
        pairs.push((key, value));
    }
    
    // Process remaining elements (skip separators, recursively handle nested t compounds)
    for i in 2..args.len() {
        if let Term::Compound(functor, inner_args) = &args[i] {
            if functor == "t" {
                extract_t_compound_pairs(inner_args, pairs);
            }
        }
    }
}

fn extract_key(term: &Term) -> Option<String> {
    match term {
        Term::String(s) => Some(s.clone()),
        Term::Atom(s) if !is_separator_or_terminator(s) => Some(s.clone()),
        _ => None,
    }
}

fn extract_value(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) if is_separator_or_terminator(a) => None, // Skip separators/terminators
        _ => Some(prolog_value_to_json_string(term.clone())),
    }
}

fn is_separator_or_terminator(atom: &str) -> bool {
    matches!(atom, "t" | "-" | "<" | ">")
}

fn build_json_object(pairs: Vec<(String, String)>) -> String {
    if pairs.is_empty() {
        return "{}".to_string();
    }
    
    let mut result = "{".to_string();
    for (i, (key, value)) in pairs.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format!("\"{}\": {}", key, value));
    }
    result.push('}');
    result
}


fn convert_internal_structure_to_json(_functor: &str, args: &[Term]) -> String {
    // Handle Prolog internal structures like "<" and ">" that are part of JSON parsing
    // These seem to represent key-value pairs in a flattened format
    
    if args.is_empty() {
        return "{}".to_string();
    }
    
    // Collect valid key-value pairs first to avoid trailing commas
    let mut pairs = Vec::new();
    let mut i = 0;
    
    while i < args.len() {
        if i + 1 < args.len() {
            // Extract key
            let key = match &args[i] {
                Term::String(s) => s.clone(),
                Term::Atom(s) => s.clone(),
                _ => format!("key_{}", i),
            };
            
            // Extract value (could be complex)
            // Skip pairs where the value is just the atom "t" (represents empty/null in Prolog)
            let value_term = &args[i + 1];
            if let Term::Atom(a) = value_term {
                if a == "t" {
                    i += 2;
                    continue; // Skip this key-value pair
                }
            }
            
            let value = prolog_value_to_json_string(args[i + 1].clone());
            pairs.push((key, value));
            i += 2;
        } else {
            // Odd argument, treat as key with null value
            let key = match &args[i] {
                Term::String(s) => s.clone(),
                Term::Atom(s) => s.clone(),
                _ => format!("key_{}", i),
            };
            pairs.push((key, "null".to_string()));
            i += 1;
        }
    }
    
    // Build JSON string from collected pairs
    let mut result = "{".to_string();
    for (i, (key, value)) in pairs.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format!("\"{}\": {}", key, value));
    }
    result.push('}');
    result
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
}
