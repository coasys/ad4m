/// Utilities for recognizing and parsing assert queries
/// These functions help identify and extract assertion operations from Prolog queries

/// Check if a query contains assertion operations
pub fn is_assert_query(query: &str) -> bool {
    query.contains("assert_link_and_triple")
        || query.contains("assert(")
        || query.contains("assertz(")
        || query.contains("asserta(")
}

/// Check if a single statement is an assert operation
pub fn is_single_assert_statement(statement: &str) -> bool {
    statement.contains("assert_link_and_triple")
        || statement.starts_with("assert(")
        || statement.starts_with("assertz(")
        || statement.starts_with("asserta(")
}

/// Check if a string has commas outside of parentheses (indicating multiple statements)
pub fn has_comma_outside_parens(text: &str) -> bool {
    let mut paren_depth = 0;
    for ch in text.chars() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            ',' if paren_depth == 0 => return true,
            _ => {}
        }
    }
    false
}

/// Extract individual assert statements from a query
pub fn extract_assert_statements(query: &str) -> Vec<String> {
    let mut statements = Vec::new();

    // Remove the final period and trim
    let query_without_period = query.trim_end_matches('.').trim();

    // Check if this is a single assert statement (no commas outside parentheses)
    if is_single_assert_statement(query_without_period)
        && !has_comma_outside_parens(query_without_period)
    {
        statements.push(query_without_period.to_string());
        log::info!(
            "🔄 EXTRACT: Single statement query: '{}'",
            query_without_period
        );
        return statements;
    }

    // For multiple statements, we need to split more carefully
    // This is a simplified approach - for now we'll split by comma outside of parentheses
    let mut paren_depth = 0;
    let mut current_statement = String::new();

    for ch in query_without_period.chars() {
        match ch {
            '(' => {
                paren_depth += 1;
                current_statement.push(ch);
            }
            ')' => {
                paren_depth -= 1;
                current_statement.push(ch);
            }
            ',' if paren_depth == 0 => {
                // This comma is a statement separator
                let cleaned = current_statement.trim();
                if !cleaned.is_empty() && is_single_assert_statement(cleaned) {
                    statements.push(cleaned.to_string());
                }
                current_statement.clear();
            }
            _ => {
                current_statement.push(ch);
            }
        }
    }

    // Don't forget the last statement
    let cleaned = current_statement.trim();
    if !cleaned.is_empty() && is_single_assert_statement(cleaned) {
        statements.push(cleaned.to_string());
    }

    log::info!(
        "🔄 EXTRACT: From query '{}' extracted {} statements: {:?}",
        query,
        statements.len(),
        statements
    );

    statements
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_assert_query() {
        assert!(is_assert_query(
            "assert_link_and_triple(\"a\", \"b\", \"c\", 123, \"author\")."
        ));
        assert!(is_assert_query("assert(triple(\"a\", \"b\", \"c\"))."));
        assert!(is_assert_query("assertz(triple(\"a\", \"b\", \"c\"))."));
        assert!(is_assert_query("asserta(triple(\"a\", \"b\", \"c\"))."));
        assert!(!is_assert_query("triple(\"a\", \"b\", \"c\")."));
    }

    #[test]
    fn test_extract_assert_statements() {
        let query = "assert_link_and_triple(\"user1\", \"likes\", \"item1\", \"123\", \"author1\"),assert_link_and_triple(\"user2\", \"likes\", \"item2\", \"124\", \"author2\").";
        let statements = extract_assert_statements(query);
        assert_eq!(statements.len(), 2);
        assert!(statements[0].contains("user1"));
        assert!(statements[1].contains("user2"));

        // Test single statement
        let single_query =
            "assert_link_and_triple(\"user1\", \"likes\", \"item1\", \"123\", \"author1\").";
        let single_statements = extract_assert_statements(single_query);
        assert_eq!(single_statements.len(), 1);
        assert!(single_statements[0].contains("user1"));
    }

    #[test]
    fn test_has_comma_outside_parens() {
        assert!(has_comma_outside_parens("a, b"));
        assert!(!has_comma_outside_parens("func(a, b)"));
        assert!(has_comma_outside_parens("func(a, b), func(c, d)"));
    }
}
