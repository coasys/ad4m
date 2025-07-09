/// Source filtering utilities for Prolog queries and statements
/// Handles pattern recognition, reachability analysis, and filtered fact generation
use rayon::prelude::*; // For parallel processing

/// Extract source filter from a Prolog query if it exists
pub fn extract_source_filter(query: &str) -> Option<String> {
    // Look for the primary Ad4mModel pattern: triple("source", "ad4m://has_child", Base)
    let ad4m_child_pattern = regex::Regex::new(
        r#"triple\s*\(\s*"([^"]+)"\s*,\s*"ad4m://has_child"\s*,\s*[A-Z_][a-zA-Z0-9_]*\s*\)"#,
    )
    .unwrap();

    if let Some(captures) = ad4m_child_pattern.captures(query) {
        if let Some(source) = captures.get(1) {
            return Some(source.as_str().to_string());
        }
    }

    // Also look for other common patterns where source is a literal (not a variable)
    let patterns = [
        // General triple patterns with literal sources
        regex::Regex::new(r#"triple\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
        // Link patterns with literal sources
        regex::Regex::new(r#"link\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
        // Reachable patterns with literal sources
        regex::Regex::new(r#"reachable\s*\(\s*"([^"]+)"\s*,"#).unwrap(),
    ];

    for pattern in &patterns {
        if let Some(captures) = pattern.captures(query) {
            if let Some(source) = captures.get(1) {
                let source_str = source.as_str();
                // Only consider it a valid filter if it's not a variable (starts with uppercase or _)
                if !source_str.starts_with(char::is_uppercase) && !source_str.starts_with('_') {
                    return Some(source_str.to_string());
                }
            }
        }
    }

    None
}

/// Extract source and target from an assert_link_and_triple statement
pub fn extract_source_target_from_statement(statement: &str) -> Option<(String, String)> {
    // Parse assert_link_and_triple("source", "predicate", "target", timestamp, author)
    if let Some(start) = statement.find("assert_link_and_triple(") {
        let args_start = start + "assert_link_and_triple(".len();
        if let Some(args_end) = statement[args_start..].find(')') {
            let args = &statement[args_start..args_start + args_end];

            // Simple parser for the arguments
            let mut parts = Vec::new();
            let mut current = String::new();
            let mut in_quotes = false;
            let mut escape_next = false;

            for ch in args.chars() {
                if escape_next {
                    current.push(ch);
                    escape_next = false;
                } else if ch == '\\' {
                    escape_next = true;
                    current.push(ch);
                } else if ch == '"' {
                    in_quotes = !in_quotes;
                    current.push(ch);
                } else if ch == ',' && !in_quotes {
                    parts.push(current.trim().to_string());
                    current.clear();
                } else {
                    current.push(ch);
                }
            }
            parts.push(current.trim().to_string());

            if parts.len() >= 3 {
                // Remove quotes from source and target
                let source = parts[0].trim_matches('"').to_string();
                let target = parts[2].trim_matches('"').to_string();
                return Some((source, target));
            }
        }
    }
    None
}

/// Filter assert statements to only include those relevant to a specific source
/// This implements batch-aware filtering that considers statement dependencies
pub fn filter_assert_statements_for_source(
    statements: &[String],
    source_filter: &str,
) -> Vec<String> {
    if statements.is_empty() {
        return Vec::new();
    }

    log::info!(
        "🔄 BATCH FILTERING: Analyzing {} statements for source filter '{}'",
        statements.len(),
        source_filter
    );

    // Parse all statements to extract source->target relationships
    let mut statement_relationships = Vec::new();
    for (idx, statement) in statements.iter().enumerate() {
        if let Some((source, target)) = extract_source_target_from_statement(statement) {
            log::info!(
                "🔄 BATCH FILTERING: Statement {}: {} -> {}",
                idx,
                source,
                target
            );
            statement_relationships.push((idx, source, target, statement.clone()));
        }
    }

    // Start with statements that directly involve the filter source
    let mut reachable_nodes = std::collections::HashSet::new();
    reachable_nodes.insert(source_filter.to_string());

    let mut relevant_statements = Vec::new();
    let mut changed = true;

    // Iteratively find statements that involve reachable nodes
    while changed {
        changed = false;

        for (idx, source, target, statement) in statement_relationships.iter() {
            // Skip if we already included this statement
            if relevant_statements
                .iter()
                .any(|(existing_idx, _)| existing_idx == idx)
            {
                continue;
            }

            // Include if source is reachable or target is reachable or statement directly mentions filter source
            let source_reachable = reachable_nodes.contains(source);
            let target_reachable = reachable_nodes.contains(target);
            let mentions_filter = statement.contains(&format!(r#""{}"#, source_filter));

            if source_reachable || target_reachable || mentions_filter {
                relevant_statements.push((*idx, statement.clone()));

                // Add both source and target to reachable set for next iteration
                if reachable_nodes.insert(source.clone()) {
                    changed = true;
                    log::info!(
                        "🔄 BATCH FILTERING: Added '{}' to reachable set via statement {}",
                        source,
                        idx
                    );
                }
                if reachable_nodes.insert(target.clone()) {
                    changed = true;
                    log::info!(
                        "🔄 BATCH FILTERING: Added '{}' to reachable set via statement {}",
                        target,
                        idx
                    );
                }
            }
        }
    }

    // Sort by original order and return statements
    relevant_statements.sort_by_key(|(idx, _)| *idx);
    let result: Vec<String> = relevant_statements
        .into_iter()
        .map(|(_, statement)| statement)
        .collect();

    log::info!(
        "🔄 BATCH FILTERING: Result: {} out of {} statements are relevant",
        result.len(),
        statements.len()
    );
    for (i, stmt) in result.iter().enumerate() {
        log::info!("🔄 BATCH FILTERING: Keeping statement {}: {}", i, stmt);
    }

    result
}

/// Helper method for sequential regex filtering
pub fn filter_facts_sequential_regex(all_facts: Vec<String>, regex: &regex::Regex) -> Vec<String> {
    log::info!(
        "🔍 FILTERING: Using sequential regex processing for {} facts",
        all_facts.len()
    );
    let mut kept_count = 0;
    let mut dropped_count = 0;

    let result: Vec<String> = all_facts
        .into_iter()
        .filter(|fact| {
            let is_relevant = regex.is_match(fact);

            if is_relevant {
                kept_count += 1;
            } else {
                dropped_count += 1;
            }

            if (kept_count + dropped_count) % 1000 == 0 {
                log::debug!(
                    "🔍 FILTERING: Processed {} facts so far ({} kept, {} dropped)",
                    kept_count + dropped_count,
                    kept_count,
                    dropped_count
                );
            }

            is_relevant
        })
        .collect();

    log::info!(
        "🔍 FILTERING: Sequential regex completed - {} kept, {} dropped",
        kept_count,
        dropped_count
    );
    result
}

/// Helper method for parallel regex filtering
pub fn filter_facts_parallel_regex(all_facts: Vec<String>, regex: &regex::Regex) -> Vec<String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let kept_count = AtomicUsize::new(0);
    let dropped_count = AtomicUsize::new(0);

    log::info!(
        "🔍 FILTERING: Using memory-efficient parallel regex processing for {} facts",
        all_facts.len()
    );

    use std::sync::Mutex;
    let debug_counter = Mutex::new(0usize);

    // Process in chunks to reduce memory usage - each thread gets a slice reference instead of copying data
    const CHUNK_SIZE: usize = 1000;
    let result: Vec<String> = all_facts
        .chunks(CHUNK_SIZE)
        .collect::<Vec<_>>()
        .into_par_iter()
        .flat_map(|chunk| {
            let mut local_result = Vec::new();
            let mut local_kept = 0;
            let mut local_dropped = 0;
            for fact in chunk {
                let is_relevant = regex.is_match(fact);
                if is_relevant {
                    local_result.push(fact.clone());
                    local_kept += 1;
                } else {
                    local_dropped += 1;
                }
            }

            // Update global counters
            kept_count.fetch_add(local_kept, Ordering::Relaxed);
            dropped_count.fetch_add(local_dropped, Ordering::Relaxed);

            // Batch logging to avoid performance hit in parallel
            {
                let mut counter = debug_counter.lock().unwrap();
                *counter += chunk.len();
                if *counter % 5000 == 0 {
                    let kept = kept_count.load(Ordering::Relaxed);
                    let dropped = dropped_count.load(Ordering::Relaxed);
                    log::debug!(
                        "🔍 FILTERING (parallel chunked): Processed {} facts so far ({} kept, {} dropped)",
                        kept + dropped,
                        kept,
                        dropped
                    );
                }
            }

            local_result
        })
        .collect();

    let final_kept = kept_count.load(Ordering::Relaxed);
    let final_dropped = dropped_count.load(Ordering::Relaxed);
    log::info!(
        "🔍 FILTERING: Memory-efficient parallel regex completed - {} kept, {} dropped",
        final_kept,
        final_dropped
    );

    result
}

/// Helper method for sequential chunked regex filtering
pub fn filter_facts_sequential_chunked_regex(
    all_facts: Vec<String>,
    regex_chunks: &[regex::Regex],
) -> Vec<String> {
    log::info!(
        "🔍 FILTERING: Using sequential chunked regex processing for {} facts with {} chunks",
        all_facts.len(),
        regex_chunks.len()
    );
    let mut kept_count = 0;
    let mut dropped_count = 0;

    let result: Vec<String> = all_facts
        .into_iter()
        .filter(|fact| {
            // Check if any of the regex chunks match
            let is_relevant = regex_chunks.iter().any(|regex| regex.is_match(fact));

            if is_relevant {
                kept_count += 1;
            } else {
                dropped_count += 1;
            }

            if (kept_count + dropped_count) % 1000 == 0 {
                log::debug!(
                    "🔍 FILTERING: Processed {} facts so far ({} kept, {} dropped)",
                    kept_count + dropped_count,
                    kept_count,
                    dropped_count
                );
            }

            is_relevant
        })
        .collect();

    log::info!(
        "🔍 FILTERING: Sequential chunked regex completed - {} kept, {} dropped",
        kept_count,
        dropped_count
    );
    result
}

/// Helper method for parallel chunked regex filtering
pub fn filter_facts_parallel_chunked_regex(
    all_facts: Vec<String>,
    regex_chunks: &[regex::Regex],
) -> Vec<String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let kept_count = AtomicUsize::new(0);
    let dropped_count = AtomicUsize::new(0);

    log::info!(
        "🔍 FILTERING: Using memory-efficient parallel chunked regex processing for {} facts with {} chunks",
        all_facts.len(),
        regex_chunks.len()
    );

    use std::sync::Mutex;
    let debug_counter = Mutex::new(0usize);

    // Process in chunks to reduce memory usage
    const CHUNK_SIZE: usize = 1000;
    let result: Vec<String> = all_facts
        .chunks(CHUNK_SIZE)
        .collect::<Vec<_>>()
        .into_par_iter()
        .flat_map(|chunk| {
            let mut local_result = Vec::new();
            let mut local_kept = 0;
            let mut local_dropped = 0;

            for fact in chunk {
                // Check if any of the regex chunks match
                let is_relevant = regex_chunks.iter().any(|regex| regex.is_match(fact));
                if is_relevant {
                    local_result.push(fact.clone());
                    local_kept += 1;
                } else {
                    local_dropped += 1;
                }
            }

            // Update global counters
            kept_count.fetch_add(local_kept, Ordering::Relaxed);
            dropped_count.fetch_add(local_dropped, Ordering::Relaxed);

            // Batch logging to avoid performance hit in parallel
            {
                let mut counter = debug_counter.lock().unwrap();
                *counter += chunk.len();
                if *counter % 5000 == 0 {
                    let kept = kept_count.load(Ordering::Relaxed);
                    let dropped = dropped_count.load(Ordering::Relaxed);
                    log::debug!("🔍 FILTERING (memory-efficient parallel chunked): Processed {} facts so far ({} kept, {} dropped)", 
                        kept + dropped, kept, dropped);
                }
            }
            local_result
        })
        .collect();

    let final_kept = kept_count.load(Ordering::Relaxed);
    let final_dropped = dropped_count.load(Ordering::Relaxed);
    log::info!(
        "🔍 FILTERING: Memory-efficient parallel chunked regex completed - {} kept, {} dropped",
        final_kept,
        final_dropped
    );

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_source_filter() {
        // Test the primary Ad4mModel pattern: triple("source", "ad4m://has_child", Base)
        assert_eq!(
            extract_source_filter(r#"triple("user123", "ad4m://has_child", Base)"#),
            Some("user123".to_string())
        );

        assert_eq!(
            extract_source_filter(r#"triple("root_node", "ad4m://has_child", Child)"#),
            Some("root_node".to_string())
        );

        // Test with extra whitespace
        assert_eq!(
            extract_source_filter(r#"triple( "user456" , "ad4m://has_child" , Base )"#),
            Some("user456".to_string())
        );

        // Test general triple patterns
        assert_eq!(
            extract_source_filter(r#"triple("user123", "likes", Target)"#),
            Some("user123".to_string()) // This should still work as general pattern
        );

        // Test link patterns
        assert_eq!(
            extract_source_filter(r#"link("user456", Predicate, Target, Timestamp, Author)"#),
            Some("user456".to_string())
        );

        // Test reachable patterns
        assert_eq!(
            extract_source_filter(r#"reachable("root_node", X)"#),
            Some("root_node".to_string())
        );

        // Test patterns that should NOT match (variables as sources)
        assert_eq!(
            extract_source_filter(r#"triple(Source, "ad4m://has_child", Base)"#),
            None
        );

        assert_eq!(
            extract_source_filter(r#"triple(_Source, "ad4m://has_child", Target)"#),
            None
        );

        // Test general patterns that should still work
        assert_eq!(
            extract_source_filter(r#"triple("user123", "some_other_predicate", Target)"#),
            Some("user123".to_string()) // This should still work as general pattern
        );
    }

    #[test]
    fn test_extract_source_target_from_statement() {
        let statement = r#"assert_link_and_triple("user1", "likes", "item1", "123", "author1")"#;
        let result = extract_source_target_from_statement(statement);
        assert_eq!(result, Some(("user1".to_string(), "item1".to_string())));

        let statement2 =
            r#"assert_link_and_triple("source", "predicate", "target", 456, "author")"#;
        let result2 = extract_source_target_from_statement(statement2);
        assert_eq!(result2, Some(("source".to_string(), "target".to_string())));

        // Test statement without proper format
        let invalid = "not_an_assert_statement";
        assert_eq!(extract_source_target_from_statement(invalid), None);
    }

    #[test]
    fn test_filter_assert_statements_for_source() {
        let statements = vec![
            r#"assert_link_and_triple("user1", "likes", "item1", "123", "author1")"#.to_string(),
            r#"assert_link_and_triple("user2", "likes", "item2", "124", "author2")"#.to_string(),
            r#"assert_link_and_triple("user1", "follows", "user2", "125", "author1")"#.to_string(),
        ];

        let user1_statements = filter_assert_statements_for_source(&statements, "user1");
        assert_eq!(user1_statements.len(), 3); // All are relevant due to batch-aware filtering

        let user2_statements = filter_assert_statements_for_source(&statements, "user2");
        assert_eq!(user2_statements.len(), 3); // All are relevant due to batch-aware filtering

        let user3_statements = filter_assert_statements_for_source(&statements, "user3");
        assert_eq!(user3_statements.len(), 0); // None relevant for user3
    }
}
