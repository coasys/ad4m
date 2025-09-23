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
    // Parse t(...) compound terms that represent JSON objects
    // Pattern: t(key1, value1, separator, key2, value2, separator, ...)
    // where separators are "<", "-", or "t" atoms
    
    let mut pairs = Vec::new();
    collect_t_compound_pairs(args, &mut pairs);
    
    // Build JSON object from collected pairs
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

fn collect_t_compound_pairs(args: &[Term], pairs: &mut Vec<(String, String)>) {
    if args.is_empty() {
        return;
    }
    
    // Pattern: t(key, value, "-", next_t_compound1, next_t_compound2, ...)
    // Where each t(...) compound represents one key-value pair
    
    // Extract the first key-value pair
    if args.len() >= 2 {
        if let Some(key) = extract_key(&args[0]) {
            // Skip terminator atoms as values
            if let Term::Atom(a) = &args[1] {
                if a == "t" || a == "-" {
                    // This is a terminator, skip this pair
                } else {
                    let value = prolog_value_to_json_string(args[1].clone());
                    pairs.push((key, value));
                }
            } else {
                let value = prolog_value_to_json_string(args[1].clone());
                pairs.push((key, value));
            }
        }
    }
    
    // Process remaining elements (starting from index 2, skipping separators)
    let mut i = 2;
    while i < args.len() {
        match &args[i] {
            Term::Atom(a) if a == "-" || a == "<" || a == "t" => {
                // Skip separator atoms
                i += 1;
            }
            Term::Compound(functor, inner_args) if functor == "t" => {
                // Recursively process the next t(...) compound
                collect_t_compound_pairs(inner_args, pairs);
                i += 1;
            }
            _ => {
                // Skip unexpected elements
                i += 1;
            }
        }
    }
}

fn extract_key(term: &Term) -> Option<String> {
    match term {
        Term::String(s) => Some(s.clone()),
        Term::Atom(s) if s != "<" && s != "-" && s != "t" => Some(s.clone()),
        _ => None,
    }
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
        // Test integer
        let term = Term::Integer(42.into());
        assert_eq!(prolog_value_to_json_string(term), "42");

        // Test float
        let term = Term::Float(3.14);
        assert_eq!(prolog_value_to_json_string(term), "3.14");

        // Test atom
        let term = Term::Atom("hello".to_string());
        assert_eq!(prolog_value_to_json_string(term), "\"hello\"");

        // Test string
        let term = Term::String("world".to_string());
        assert_eq!(prolog_value_to_json_string(term), "\"world\"");
    }

    #[test]
    fn test_prolog_value_to_json_string_list() {
        // Test simple list
        let term = Term::List(vec![
            Term::String("a".to_string()),
            Term::String("b".to_string()),
            Term::Integer(42.into())
        ]);
        assert_eq!(prolog_value_to_json_string(term), "[\"a\", \"b\", 42]");
    }

    #[test]
    fn test_prolog_value_to_json_string_compound() {
        // Test compound term - this should represent a JSON object
        let term = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("key1".to_string()),
                Term::String("value1".to_string()),
                Term::String("key2".to_string()),
                Term::Integer(42.into())
            ]
        );
        // This should produce a proper JSON object
        let result = prolog_value_to_json_string(term);
        assert_eq!(result, r#"{"key1": "value1", "key2": 42}"#);
        
        // Verify it's valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        assert_eq!(parsed["key1"], "value1");
        assert_eq!(parsed["key2"], 42);
    }

    #[test]
    fn test_prolog_value_to_json_string_nested_compound() {
        // Test nested compound terms representing nested JSON objects
        let inner_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("bio".to_string()),
                Term::String("Software developer passionate about decentralized systems".to_string())
            ]
        );
        
        let outer_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("location".to_string()),
                Term::String("San Francisco".to_string()),
                Term::String("profile".to_string()),
                inner_compound
            ]
        );
        
        let result = prolog_value_to_json_string(outer_compound);
        println!("Nested compound result: {}", result);
        
        // This should be valid JSON when parsed
        let expected = r#"{"location": "San Francisco", "profile": {"bio": "Software developer passionate about decentralized systems"}}"#;
        assert_eq!(result, expected);
        
        // Verify it's valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        assert_eq!(parsed["location"], "San Francisco");
        assert_eq!(parsed["profile"]["bio"], "Software developer passionate about decentralized systems");
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
    fn test_prolog_value_to_json_string_assoc_structure() {
        // Test Prolog assoc dictionary structures
        // Pattern: -(Key, Value, Left, Right) where Left and Right are subtrees
        
        // Create a simple assoc structure: -("author", "Alice", t, t)
        let simple_assoc = Term::Compound(
            "-".to_string(),
            vec![
                Term::String("author".to_string()),
                Term::String("Alice".to_string()),
                Term::Atom("t".to_string()), // empty left subtree
                Term::Atom("t".to_string())  // empty right subtree
            ]
        );
        
        let result = prolog_value_to_json_string(simple_assoc);
        assert_eq!(result, r#"{"author": "Alice"}"#);
        
        // Test nested assoc structure with multiple key-value pairs
        let left_subtree = Term::Compound(
            "-".to_string(),
            vec![
                Term::String("created".to_string()),
                Term::String("2025-09-22T10:00:00Z".to_string()),
                Term::Atom("t".to_string()),
                Term::Atom("t".to_string())
            ]
        );
        
        let right_subtree = Term::Compound(
            "-".to_string(),
            vec![
                Term::String("views".to_string()),
                Term::Integer(42.into()),
                Term::Atom("t".to_string()),
                Term::Atom("t".to_string())
            ]
        );
        
        let complex_assoc = Term::Compound(
            "-".to_string(),
            vec![
                Term::String("author".to_string()),
                Term::String("Alice".to_string()),
                left_subtree,
                right_subtree
            ]
        );
        
        let result = prolog_value_to_json_string(complex_assoc);
        println!("Complex assoc result: {}", result);
        
        // Parse the result to verify it's valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        assert_eq!(parsed["author"], "Alice");
        assert_eq!(parsed["created"], "2025-09-22T10:00:00Z");
        assert_eq!(parsed["views"], 42);
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
    fn test_prolog_value_to_json_string_complex_nested_like_expression() {
        // Test the exact scenario we're seeing in the AD4M test
        // This simulates how a complex Expression object gets parsed by Prolog
        
        // Create the nested "data" object: {"author": "Alice", "created": "2025-09-22T10:00:00Z", "views": 42}
        let data_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("author".to_string()),
                Term::String("Alice".to_string()),
                Term::String("created".to_string()),
                Term::String("2025-09-22T10:00:00Z".to_string()),
                Term::String("views".to_string()),
                Term::Integer(42.into())
            ]
        );
        
        // Create the outer Expression object containing the data
        let expression_compound = Term::Compound(
            "t".to_string(),
            vec![
                Term::String("author".to_string()),
                Term::String("did:key:z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf".to_string()),
                Term::String("timestamp".to_string()),
                Term::String("2025-09-23T19:01:08.488Z".to_string()),
                Term::String("data".to_string()),
                data_compound,
                Term::String("proof".to_string()),
                Term::Compound(
                    "t".to_string(),
                    vec![
                        Term::String("key".to_string()),
                        Term::String("did:key:z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf#z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf".to_string()),
                        Term::String("signature".to_string()),
                        Term::String("9bbb242314503158d4aede33dc1a7f4473d51c2fccc641ba75160822c5ae64357698284967d17a08d68e7bb6cb121aac0dc3a72f3fd64cdb13881414c547330c".to_string())
                    ]
                )
            ]
        );
        
        let result = prolog_value_to_json_string(expression_compound);
        println!("Complex nested result: {}", result);
        
        // Parse the result to make sure it's valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).expect("Should be valid JSON");
        
        // Verify the structure
        assert_eq!(parsed["author"], "did:key:z6MkiUs5qicZeaMVSbLCujGYbD3Qs2VGMnMGKtHHoyUjBVXf");
        assert_eq!(parsed["data"]["author"], "Alice");
        assert_eq!(parsed["data"]["created"], "2025-09-22T10:00:00Z");
        assert_eq!(parsed["data"]["views"], 42);
    }
}
