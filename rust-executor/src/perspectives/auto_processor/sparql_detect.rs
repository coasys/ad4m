//! Pure syntactic detection of "partitioned" [`super::config::AutoProcessorConfig`]
//! scope queries.
//!
//! A scope query is *partitioned* when its projection binds `?partition` — the
//! signal the future `auto_processor_watch_loop` will use to dispatch to the
//! partitioned gather + interpret path (spec build-list item 1). Detection is
//! deliberately syntactic (not a full SPARQL parse): a valid query that binds
//! `?partition` in its projection always parses to `true`; a query that never
//! mentions `?partition` always parses to `false`. Ambiguous constructions
//! (e.g. `?partition` bound in `WHERE` but not projected) sit in the middle —
//! see the per-case notes below.
//!
//! Everything here is pure — one `&str` in, one `bool` out — so it can be
//! called from the watcher hot path without allocation guilt and unit-tested
//! without a perspective fixture.

use regex::Regex;
use std::sync::OnceLock;

/// The SPARQL variable a partitioned scope query must bind. Kept as a
/// `const` so any future rename (`?p`? `?bucket`?) is a one-place change.
pub const PARTITION_VAR: &str = "partition";

/// Return `true` iff `sparql` is a partitioned scope query — i.e. its
/// projection binds [`PARTITION_VAR`].
///
/// Recognises:
/// * Direct projection: `SELECT ?partition ?speaker ?text WHERE {...}` (with
///   any ordering, whitespace, or intervening projected vars).
/// * `SELECT DISTINCT` / `SELECT REDUCED` variants.
/// * Alias projection: `SELECT (?raw AS ?partition) ...` — the alias name
///   (post-`AS`) is what downstream consumers see.
/// * `SELECT *` — falls back to a whole-query scan for a `?partition` token
///   (the wildcard projects everything bound in `WHERE`, so any binding is
///   effectively projected).
///
/// Does NOT recognise (returns `false`):
/// * Queries where `?partition` is bound in `WHERE` but not projected under a
///   non-`*` SELECT list. That's a caller error — the future watcher can't
///   read a variable the SPARQL engine hasn't projected — so `false` is the
///   right answer.
/// * ASK / CONSTRUCT / DESCRIBE — those don't have a projection at all.
///
/// Comments (`# ... <newline>`) are stripped before scanning, so a commented-
/// out `# SELECT ?partition ...` does NOT trigger a false positive.
///
/// SPARQL keywords are case-insensitive per the spec, so `SELECT`, `select`,
/// `Select` are all recognised; SPARQL variables ARE case-sensitive, so only
/// `?partition` (lowercase) matches — a `?Partition` in the query returns
/// `false`.
pub fn is_partitioned_query(sparql: &str) -> bool {
    let stripped = strip_line_comments(sparql);

    let Some((select_end_ix, projection_end_ix)) = locate_projection(&stripped) else {
        return false;
    };
    let projection = &stripped[select_end_ix..projection_end_ix];

    if projection_is_star(projection) {
        // `SELECT *` projects every var bound in the query body; the future
        // watcher will get a `?partition` binding iff any triple pattern binds
        // it. Look at everything AFTER the SELECT-list — the projection itself
        // is just `*` so there's nothing to find there.
        return contains_partition_var(&stripped[projection_end_ix..]);
    }

    contains_partition_var(projection)
}

/// Strip SPARQL `# ... <newline>` line comments. Returns an owned `String`
/// because we may drop mid-line bytes; keeps the newlines so line-based
/// scanners downstream still work.
fn strip_line_comments(sparql: &str) -> String {
    let mut out = String::with_capacity(sparql.len());
    for line in sparql.split_inclusive('\n') {
        // Trim from the first `#` that isn't inside a string literal. SPARQL
        // scope queries in AutoProcessorConfig are hand-written and never
        // embed `#` in a literal (partition URIs are `<...>` triples, not
        // quoted strings), so a naive first-`#` trim is safe here.
        match line.find('#') {
            Some(hash_ix) => {
                out.push_str(&line[..hash_ix]);
                // Preserve a trailing newline (if the original line had one)
                // so line-numbering-sensitive downstream regex behaviour
                // stays byte-consistent with the input.
                if line.ends_with('\n') {
                    out.push('\n');
                }
            }
            None => out.push_str(line),
        }
    }
    out
}

/// Locate the projection list: return `(start, end)` byte indices where
/// `stripped[start..end]` is the substring between the `SELECT` keyword's
/// end and the first `WHERE` keyword / `{`.
///
/// `start` skips over `DISTINCT` / `REDUCED` modifiers so `projection_is_star`
/// and `contains_partition_var` see the actual var list, not the modifier.
fn locate_projection(stripped: &str) -> Option<(usize, usize)> {
    let re = select_regex();
    let m = re.find(stripped)?;
    // `m.end()` sits after the SELECT (+ optional DISTINCT/REDUCED) keyword —
    // exactly the start of the projection list.
    let start = m.end();

    // Projection ends at the FIRST of (a) whitespace-bounded `WHERE`,
    // (b) an opening `{`. Whichever comes earlier wins.
    let where_ix = where_regex().find(&stripped[start..]).map(|m| m.start());
    let brace_ix = stripped[start..].find('{');
    let end = match (where_ix, brace_ix) {
        (Some(w), Some(b)) => start + w.min(b),
        (Some(w), None) => start + w,
        (None, Some(b)) => start + b,
        (None, None) => return None,
    };
    Some((start, end))
}

/// Return `true` iff `projection` is just `*` (possibly surrounded by
/// whitespace). `SELECT DISTINCT *` reaches here as `" *"` because
/// [`locate_projection`] already consumed `DISTINCT`.
fn projection_is_star(projection: &str) -> bool {
    projection.trim() == "*"
}

/// Return `true` iff `haystack` contains the SPARQL variable
/// `?<PARTITION_VAR>` as a token boundary — a bare `?partition_id` does NOT
/// match, but `?partition` inside `(?raw AS ?partition)`, `SELECT ?partition
/// ...`, and `.  ?m <ns://p> ?partition .` all do.
fn contains_partition_var(haystack: &str) -> bool {
    partition_var_regex().is_match(haystack)
}

fn select_regex() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        // Case-insensitive SELECT + optional DISTINCT/REDUCED modifier. `\b`
        // pins the keyword to a word boundary so `SELECTED` doesn't match.
        Regex::new(r"(?i)\bSELECT\b(\s+(DISTINCT|REDUCED)\b)?").expect("select_regex is valid")
    })
}

fn where_regex() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bWHERE\b").expect("where_regex is valid"))
}

fn partition_var_regex() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        // `\?partition` followed by a non-varchar (or end of input). SPARQL
        // varchars are `[A-Za-z0-9_]` plus a few Unicode ranges we don't care
        // about here — the projection var is always ASCII.
        Regex::new(r"\?partition(?:[^A-Za-z0-9_]|$)").expect("partition_var_regex is valid")
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Positive cases: partitioned queries ===================================

    #[test]
    fn direct_projection_returns_true() {
        // The exact shape used by the partitioned e2e in interpretation_e2e.rs.
        let sparql = "SELECT ?partition ?speaker ?text WHERE { \
                      ?m <ns://in_subgroup> ?partition . \
                      ?m <ns://body> ?text . \
                      ?m <ns://author> ?speaker . \
                      } ORDER BY ?m";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn distinct_projection_returns_true() {
        let sparql = "SELECT DISTINCT ?partition ?speaker ?text WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn reduced_projection_returns_true() {
        let sparql = "SELECT REDUCED ?partition ?speaker ?text WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn alias_projection_returns_true() {
        // Bound as `?raw` in the WHERE clause, aliased into the projection
        // under the name `?partition`. Downstream consumers see `?partition`,
        // so the query IS partitioned.
        let sparql = "SELECT ?speaker ?text (?raw AS ?partition) WHERE { \
                      ?m <ns://raw> ?raw . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn star_projection_with_partition_bound_returns_true() {
        let sparql = "SELECT * WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn case_insensitive_select_keyword() {
        let sparql = "select ?partition ?speaker ?text where { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn newlines_and_extra_whitespace_do_not_break_detection() {
        let sparql = "PREFIX ex: <ns://>\n\n  SELECT\n     ?partition\n     ?speaker\n     ?text\n  WHERE {\n     ?m ex:in ?partition .\n     ?m ex:b ?text .\n     ?m ex:a ?speaker .\n  }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn partition_var_at_end_of_projection() {
        // The `?partition` regex must match a var flush against the trailing
        // whitespace-then-WHERE boundary (no non-varchar between `partition`
        // and the space).
        let sparql = "SELECT ?speaker ?text ?partition WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    #[test]
    fn open_brace_immediately_after_projection() {
        // No `WHERE` keyword — SPARQL allows the pattern group to open with
        // just `{`. Projection end must fall on the `{`.
        let sparql = "SELECT ?partition ?speaker ?text { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }

    // === Negative cases: non-partitioned queries ==============================

    #[test]
    fn plain_projection_returns_false() {
        let sparql = "SELECT ?speaker ?text WHERE { ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn partition_bound_in_where_but_not_projected_returns_false() {
        // `?partition` binds in WHERE but a non-`*` SELECT list omits it — the
        // watcher won't see it, so the query is functionally not partitioned.
        let sparql = "SELECT ?speaker ?text WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn similar_but_different_var_returns_false() {
        // `?partition_id` shares a prefix but is a distinct variable. The
        // trailing-varchar guard in `partition_var_regex` catches this.
        let sparql = "SELECT ?partition_id ?speaker ?text WHERE { ?m <ns://in> ?partition_id . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn different_case_var_returns_false() {
        // SPARQL variables ARE case-sensitive per §2.5, so `?Partition` is
        // literally a different variable from `?partition`.
        let sparql = "SELECT ?Partition ?speaker ?text WHERE { ?m <ns://in> ?Partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn commented_out_partition_projection_returns_false() {
        // The `?partition` mention lives inside a `#`-comment; after strip
        // it's gone. Detection must not false-positive.
        let sparql = "# SELECT ?partition ?speaker ?text WHERE { ... }\nSELECT ?speaker ?text WHERE { ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn commented_partition_var_inline_returns_false() {
        // Comment lives at end of the SELECT line; the visible projection
        // never binds `?partition`.
        let sparql = "SELECT ?speaker ?text # ?partition would go here\nWHERE { ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn empty_string_returns_false() {
        assert!(!is_partitioned_query(""));
    }

    #[test]
    fn ask_query_returns_false() {
        let sparql = "ASK WHERE { ?m <ns://in> ?partition . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn construct_query_returns_false() {
        let sparql = "CONSTRUCT { ?m <ns://out> ?partition } WHERE { ?m <ns://in> ?partition . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn describe_query_returns_false() {
        let sparql = "DESCRIBE ?partition WHERE { ?m <ns://in> ?partition . }";
        assert!(!is_partitioned_query(sparql));
    }

    // === Star-projection edge case ===========================================

    #[test]
    fn star_projection_without_partition_returns_false() {
        let sparql = "SELECT * WHERE { ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(!is_partitioned_query(sparql));
    }

    #[test]
    fn distinct_star_projection_with_partition_returns_true() {
        // `DISTINCT` MUST NOT confuse `projection_is_star` — `locate_projection`
        // is responsible for consuming it. If this ever regresses, star-based
        // detection silently mis-fires.
        let sparql = "SELECT DISTINCT * WHERE { ?m <ns://in> ?partition . ?m <ns://b> ?text . ?m <ns://a> ?speaker . }";
        assert!(is_partitioned_query(sparql));
    }
}
