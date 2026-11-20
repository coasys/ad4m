//! The seed: the first view of the graph that a harness pass gets (#1804).
//!
//! It replaces the full `existing` list. The model finds other rows with the
//! walk tools ([`super::walk_tools`]). The seed has four parts:
//!
//! 1. **Scope.** The rows this processor wrote (id under `base_prefix`), or,
//!    when the pass has an existing scope, each row in it. One outline line
//!    each.
//! 2. **Focus.** The scope rows with the newest activity, in full.
//! 3. **Ancestors.** Each node above a scope row, through a forward `hasOne`
//!    relation or `ad4m://has_child`, as one outline line. A parent from an
//!    earlier source stays in view.
//! 4. **Note.** "The seed shows N of M rows. ..."
//!
//! Activity is the newest link a row is the source or the target of
//! ([`crate::perspectives::model_query::ACTIVITY_ORDER_KEY`]): a row from an
//! earlier run is recent, and a node is recent again when a row attaches to
//! it.
//!
//! The budget of #1806 ([`super::existing_budget::existing_budget_tokens`])
//! applies. When the seed is too large, focus rows become outline lines, and
//! then the oldest scope lines leave.
//!
//! Roles and parents come from the SHACL of the classes only. A Social DNA
//! with no relation and no containment gives a flat seed: the scope rows by
//! activity.

use super::class_label;
use super::existing_budget::{existing_budget_tokens, timestamp_millis, ASSUMED_WINDOW_TOKENS};
use super::prompt::existing_entry_json;
use super::types::{ExistingInstances, ExistingLinks};
use crate::ai_service::estimate_token_count;
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap, HashSet};

/// The system message of a harness pass. The pass has no other instructions
/// than this, the tool list and the JSON input.
pub(crate) const HARNESS_GUIDE: &str = "\
You add the items of a transcript to a shared knowledge graph. You use tools.

The user message is JSON with `classes` (what you can create, with hints), \
`transcript`, and `seed`.

`seed` shows part of the graph. `seed.outline` has one line for each row: \
`- [Class] title (id)`. A row is indented under its parent. `seed.focus` shows \
the newest rows in full. `seed.note` tells how many rows the graph has.

The graph can hold the row that an item belongs under, also when the seed does \
not show it. Find it before you create a new one:
  - get_children(parent) shows the rows under a node, across classes.
  - instance_query(class_name, filter, order, limit) lists rows of one class. A \
filter on a relation gives the rows that point to a node. order `activity` \
shows the most recent rows first.
  - instance_get(class_name, base_uri) reads one row in full. Use it only when \
you must decide about that row.

Create each new item with `<class>_create`. Link it to its parent with \
`<class>_propose_link_child`, with the id of the parent exactly as a tool or the \
seed showed it. Do not create an item that is already in the graph.

When all items are written, stop. Do not write a summary.";

/// The most scope rows the seed shows in full.
pub(crate) const FOCUS_ROWS: usize = 8;

/// How many levels above a scope row the seed follows.
const ANCESTOR_DEPTH: usize = 4;

const HAS_CHILD: &str = "ad4m://has_child";

/// A row the seed names: its class and title.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SeedRow {
    pub class: String,
    pub title: String,
}

/// What the seed is built from, read from the graph once per pass.
#[derive(Debug, Clone, Default)]
pub(crate) struct SeedGraph {
    /// The scope rows, newest activity first.
    pub scope: Vec<String>,
    /// Parent ids of each row (scope rows and ancestors), in a stable order.
    pub parents: HashMap<String, Vec<String>>,
    /// Class and title of each ancestor that is not in `existing`.
    pub other_rows: HashMap<String, SeedRow>,
    /// Rows of the offered classes in the graph.
    pub rows_total: usize,
    /// Whether the pass checks new rows for duplicates against all rows
    /// (dedup on drain). The note tells the model only what is true.
    pub unshown_checked: bool,
}

/// The seed of one pass.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Seed {
    /// The `seed` object of the prompt.
    pub json: Value,
    /// Distinct rows the seed names (scope and ancestors).
    pub rows_shown: usize,
    pub rows_total: usize,
    /// Scope rows shown in full.
    pub focus: usize,
    /// Estimated tokens of `json`.
    pub tokens: usize,
}

/// The note that ends the seed.
pub(crate) fn seed_note(shown: usize, total: usize, unshown_checked: bool) -> String {
    let check = if unshown_checked {
        "Rows that you do not see are still checked for duplicates."
    } else {
        "Rows that you do not see are not checked for duplicates: find a row before you \
         create it."
    };
    format!("The seed shows {shown} of {total} rows. Use the tools to find other rows. {check}")
}

/// Pick and render the seed from `graph` within `budget_tokens`. Pure: the
/// reads are in [`read_seed_graph`].
pub(crate) fn build_seed(
    existing: &ExistingInstances,
    graph: &SeedGraph,
    budget_tokens: usize,
) -> Seed {
    let mut scope: Vec<&String> = graph.scope.iter().collect();
    let mut focus = scope.len().min(FOCUS_ROWS);
    loop {
        let seed = render(existing, graph, &scope, focus);
        if seed.tokens <= budget_tokens || scope.is_empty() {
            return seed;
        }
        if focus > 0 {
            focus -= 1;
        } else {
            scope.pop();
        }
    }
}

/// The title and class of a row, from `existing` or the ancestors read apart.
fn row_of<'a>(
    existing: &'a ExistingInstances,
    graph: &'a SeedGraph,
    id: &str,
) -> Option<(&'a str, &'a str)> {
    if let Some(row) = existing.get(id).and_then(|rows| rows.first()) {
        return Some((row.class.as_str(), row.title.as_str()));
    }
    graph
        .other_rows
        .get(id)
        .map(|r| (r.class.as_str(), r.title.as_str()))
}

fn outline_line(existing: &ExistingInstances, graph: &SeedGraph, id: &str, depth: usize) -> String {
    let indent = "  ".repeat(depth);
    match row_of(existing, graph, id) {
        Some((class, title)) => format!("{indent}- [{class}] {title} ({id})"),
        None => format!("{indent}- ({id})"),
    }
}

/// The seed for the first `scope` rows, the first `focus` of them in full.
fn render(
    existing: &ExistingInstances,
    graph: &SeedGraph,
    scope: &[&String],
    focus: usize,
) -> Seed {
    // Every row the outline names: the scope rows and their ancestors.
    let mut named: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for id in scope {
        if seen.insert((*id).clone()) {
            named.push((*id).clone());
        }
    }
    let mut frontier: Vec<String> = scope.iter().map(|id| (*id).clone()).collect();
    for _ in 0..ANCESTOR_DEPTH {
        let mut next = Vec::new();
        for id in &frontier {
            for parent in graph.parents.get(id).into_iter().flatten() {
                if seen.insert(parent.clone()) {
                    named.push(parent.clone());
                    next.push(parent.clone());
                }
            }
        }
        if next.is_empty() {
            break;
        }
        frontier = next;
    }

    // A tree: each row under its first parent that the outline names. A row
    // with no named parent is a root. Roots and siblings keep `named` order
    // (scope rows by activity, then ancestors).
    let named_set: HashSet<&String> = named.iter().collect();
    let parent_of: HashMap<&String, &String> = named
        .iter()
        .filter_map(|id| {
            graph
                .parents
                .get(id)?
                .iter()
                .find(|p| named_set.contains(p) && *p != id)
                .map(|p| (id, p))
        })
        .collect();
    let mut children: BTreeMap<usize, Vec<&String>> = BTreeMap::new();
    let position: HashMap<&String, usize> =
        named.iter().enumerate().map(|(i, id)| (id, i)).collect();
    let mut roots: Vec<&String> = Vec::new();
    for id in &named {
        match parent_of.get(id) {
            Some(parent) => children.entry(position[parent]).or_default().push(id),
            None => roots.push(id),
        }
    }
    let mut lines: Vec<String> = Vec::new();
    let mut emitted: HashSet<&String> = HashSet::new();
    let mut stack: Vec<(&String, usize)> = roots.iter().rev().map(|id| (*id, 0)).collect();
    while let Some((id, depth)) = stack.pop() {
        if !emitted.insert(id) {
            continue;
        }
        lines.push(outline_line(existing, graph, id, depth));
        if let Some(kids) = children.get(&position[id]) {
            for kid in kids.iter().rev() {
                stack.push((kid, depth + 1));
            }
        }
    }
    // A cycle of parents has no root: name its rows flat.
    for id in &named {
        if !emitted.contains(id) {
            lines.push(outline_line(existing, graph, id, 0));
        }
    }

    let focus_rows: Vec<Value> = scope
        .iter()
        .take(focus)
        .filter_map(|id| existing.get(*id))
        .flatten()
        .map(|row| {
            let mut entry = existing_entry_json(row);
            if let Some(parents) = graph.parents.get(&row.id).filter(|p| !p.is_empty()) {
                entry["parents"] = json!(parents);
            }
            entry
        })
        .collect();

    let rows_total = graph.rows_total.max(named.len());
    let json = json!({
        "note": seed_note(named.len(), rows_total, graph.unshown_checked),
        "outline": lines.join("\n"),
        "focus": focus_rows,
    });
    let tokens = estimate_token_count(&json.to_string());
    Seed {
        json,
        rows_shown: named.len(),
        rows_total,
        focus: focus.min(scope.len()),
        tokens,
    }
}

/// The scope rows of a pass: each row of `existing` when the pass has an
/// existing scope (the query already limited `existing` to it), else the
/// rows under `base_prefix`.
pub(crate) fn scope_ids(
    existing: &ExistingInstances,
    base_prefix: &str,
    scope: Option<&Scope>,
) -> Vec<String> {
    let mut ids: Vec<String> = existing
        .keys()
        .filter(|id| scope.is_some() || (!base_prefix.is_empty() && id.starts_with(base_prefix)))
        .cloned()
        .collect();
    ids.sort();
    ids
}

/// The forward `hasOne` relation predicates of the offered classes: the
/// parent links the seed follows up.
pub(crate) fn parent_predicates(shapes: &[ModelShape]) -> HashSet<String> {
    shapes
        .iter()
        .flat_map(|s| s.include_relations.iter())
        .filter(|r| {
            r.direction == "forward" && r.kind == "hasOne" && !r.target_class_name.is_empty()
        })
        .map(|r| r.predicate.clone())
        .collect()
}

/// A value from a SPARQL result row: a plain string, or `{value}`.
fn binding(row: &Value, key: &str) -> Option<String> {
    let v = row.get(key)?;
    let s = v
        .as_str()
        .or_else(|| v.get("value").and_then(Value::as_str))?;
    Some(s.to_string())
}

/// Milliseconds of a timestamp literal as a query returns it: bare, quoted,
/// or with a datatype.
fn literal_millis(raw: &str) -> i64 {
    let inner = raw
        .trim_start_matches('"')
        .split("\"^^")
        .next()
        .unwrap_or(raw)
        .trim_end_matches('"');
    timestamp_millis(inner)
}

fn values_block(ids: &[String]) -> String {
    ids.iter()
        .filter(|id| crate::perspectives::model_query::is_safe_iri_target(id))
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The activity of each of `ids` in epoch milliseconds (see the module doc).
/// An id that the query does not reach is absent.
pub(crate) fn activity_of(
    perspective: &PerspectiveInstance,
    ids: &[String],
) -> HashMap<String, i64> {
    let mut out = HashMap::new();
    for chunk in ids.chunks(200) {
        let values = values_block(chunk);
        if values.is_empty() {
            continue;
        }
        let query = format!(
            "SELECT ?s (MAX(?ts) AS ?a) WHERE {{ VALUES ?s {{ {values} }} \
             {{ ?s ?p ?o . ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?s ?p ?o )>> . \
                ?r <ad4m://ontology/timestamp> ?ts . }} UNION \
             {{ ?x ?q ?s . ?r2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?x ?q ?s )>> . \
                ?r2 <ad4m://ontology/timestamp> ?ts . }} }} GROUP BY ?s"
        );
        let rows: Vec<Value> = match perspective
            .sparql_query(query)
            .map_err(|e| e.to_string())
            .and_then(|r| serde_json::from_str(&r).map_err(|e| e.to_string()))
        {
            Ok(rows) => rows,
            Err(e) => {
                log::warn!("interpretation seed: activity query failed: {e}");
                continue;
            }
        };
        for row in rows {
            if let (Some(id), Some(ts)) = (binding(&row, "s"), binding(&row, "a")) {
                out.insert(id, literal_millis(&ts));
            }
        }
    }
    out
}

/// The containment parents of each of `ids` (`P ad4m://has_child id`).
fn containment_parents(
    perspective: &PerspectiveInstance,
    ids: &[String],
) -> HashMap<String, Vec<String>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for chunk in ids.chunks(200) {
        let values = values_block(chunk);
        if values.is_empty() {
            continue;
        }
        let query =
            format!("SELECT ?p ?c WHERE {{ VALUES ?c {{ {values} }} ?p <{HAS_CHILD}> ?c . }}");
        let rows: Vec<Value> = match perspective
            .sparql_query(query)
            .map_err(|e| e.to_string())
            .and_then(|r| serde_json::from_str(&r).map_err(|e| e.to_string()))
        {
            Ok(rows) => rows,
            Err(e) => {
                log::warn!("interpretation seed: containment query failed: {e}");
                continue;
            }
        };
        for row in rows {
            if let (Some(parent), Some(child)) = (binding(&row, "p"), binding(&row, "c")) {
                out.entry(child).or_default().push(parent);
            }
        }
    }
    for parents in out.values_mut() {
        parents.sort();
        parents.dedup();
    }
    out
}

/// Read what the seed needs: the scope rows by activity, the parents up to
/// [`ANCESTOR_DEPTH`] levels, the class and title of ancestors outside
/// `existing`, and the number of rows of the offered classes.
pub(crate) async fn read_seed_graph(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    existing: &ExistingInstances,
    existing_links: &ExistingLinks,
    base_prefix: &str,
    scope: Option<&Scope>,
) -> SeedGraph {
    let mut scope_rows = scope_ids(existing, base_prefix, scope);
    let activity = activity_of(perspective, &scope_rows);
    // Newest activity first. Without a store answer, the newest of updatedAt
    // and createdAt.
    let fallback = |id: &String| {
        existing
            .get(id)
            .into_iter()
            .flatten()
            .map(|r| timestamp_millis(&r.updated_at))
            .max()
            .unwrap_or(0)
    };
    scope_rows.sort_by(|a, b| {
        let ta = activity.get(a).copied().unwrap_or_else(|| fallback(a));
        let tb = activity.get(b).copied().unwrap_or_else(|| fallback(b));
        tb.cmp(&ta).then_with(|| a.cmp(b))
    });

    // Parent links: forward hasOne relation values, then containment.
    let predicates = parent_predicates(shapes);
    let mut relation_parents: HashMap<String, Vec<String>> = HashMap::new();
    for (source, predicate, target) in existing_links {
        if predicates.contains(predicate) {
            relation_parents
                .entry(source.clone())
                .or_default()
                .push(target.clone());
        }
    }
    let mut parents: HashMap<String, Vec<String>> = HashMap::new();
    let mut frontier = scope_rows.clone();
    let mut visited: HashSet<String> = frontier.iter().cloned().collect();
    for _ in 0..ANCESTOR_DEPTH {
        if frontier.is_empty() {
            break;
        }
        let held = containment_parents(perspective, &frontier);
        let mut next = Vec::new();
        for id in &frontier {
            let mut ps: Vec<String> = relation_parents.get(id).cloned().unwrap_or_default();
            ps.sort();
            for p in held.get(id).into_iter().flatten() {
                if !ps.contains(p) {
                    ps.push(p.clone());
                }
            }
            for p in &ps {
                if visited.insert(p.clone()) {
                    next.push(p.clone());
                }
            }
            if !ps.is_empty() {
                parents.insert(id.clone(), ps);
            }
        }
        frontier = next;
    }

    // Ancestors outside `existing`: their class (most specific) and title.
    let outside: Vec<String> = parents
        .values()
        .flatten()
        .filter(|id| !existing.contains_key(*id))
        .cloned()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    let other_rows = name_rows(perspective, shapes, &outside).await;

    let mut rows_total = 0usize;
    for shape in shapes {
        let class = class_label(&shape.target_class, shapes);
        match perspective
            .model_query(&class, r#"{"limit":0,"count":true}"#)
            .await
        {
            Ok(raw) => {
                rows_total += serde_json::from_str::<Value>(&raw)
                    .ok()
                    .and_then(|v| v["totalCount"].as_u64())
                    .unwrap_or(0) as usize;
            }
            Err(e) => log::warn!("interpretation seed: count of {class} failed: {e:#}"),
        }
    }
    let rows_total = rows_total.max(existing.len());

    if predicates.is_empty() && parents.is_empty() {
        log::info!(
            "interpretation seed: the offered classes declare no parent relation and hold no \
             containment; the seed is flat ({} scope rows)",
            scope_rows.len()
        );
    }
    SeedGraph {
        scope: scope_rows,
        parents,
        other_rows,
        rows_total,
        unshown_checked: false,
    }
}

/// Class and title of each of `ids`: its most specific class, and the class's
/// identity value when the class declares one.
async fn name_rows(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    ids: &[String],
) -> HashMap<String, SeedRow> {
    let mut out = HashMap::new();
    if ids.is_empty() {
        return out;
    }
    let classes = match perspective.subject_classes_of(ids) {
        Ok(classes) => classes,
        Err(e) => {
            log::warn!("interpretation seed: classes of ancestors failed: {e:#}");
            return out;
        }
    };
    let mut by_class: HashMap<String, Vec<String>> = HashMap::new();
    for (id, list) in classes {
        if let Some(class) = list.first() {
            by_class.entry(class.clone()).or_default().push(id);
        }
    }
    for (class, ids) in by_class {
        let title = perspective.get_shape(&class).ok().and_then(|s| {
            s.properties
                .iter()
                .find(|p| p.identity)
                .map(|p| p.name.clone())
        });
        let label = shapes
            .iter()
            .find(|s| s.target_class == class || class_label(&s.target_class, shapes) == class)
            .map(|s| class_label(&s.target_class, shapes))
            .unwrap_or_else(|| super::class_local_name(&class).to_string());
        let mut titles: HashMap<String, String> = HashMap::new();
        if let Some(title) = &title {
            let query = json!({ "where": { "id": ids }, "properties": [title] });
            if let Ok(raw) = perspective.model_query(&class, &query.to_string()).await {
                if let Ok(parsed) = serde_json::from_str::<Value>(&raw) {
                    for inst in parsed["instances"].as_array().into_iter().flatten() {
                        if let (Some(id), Some(t)) = (inst["id"].as_str(), inst[title].as_str()) {
                            titles.insert(id.to_string(), t.to_string());
                        }
                    }
                }
            }
        }
        for id in ids {
            let title = titles.get(&id).cloned().unwrap_or_default();
            out.insert(
                id,
                SeedRow {
                    class: label.clone(),
                    title,
                },
            );
        }
    }
    out
}

/// The token budget of the seed: #1806's budget for the `existing` block,
/// from the model's window and the prompt without the seed.
pub(crate) async fn seed_budget(
    task: &crate::types::AITask,
    prompt_without_seed: &str,
) -> (usize, Option<usize>) {
    let window = match crate::ai_service::AIService::global_instance().await {
        Ok(service) => service.context_window_tokens(&task.model_id).await,
        Err(_) => None,
    };
    let other = crate::ai_service::estimate_task_prompt_tokens(task, prompt_without_seed);
    (existing_budget_tokens(window, other), window)
}

/// The harness prompt: `prompt` (built with no `existing` rows) with each
/// class's empty `existing` list removed and `seed` added.
pub(crate) fn with_seed(prompt: &str, seed: &Seed) -> String {
    let Ok(mut value) = serde_json::from_str::<Value>(prompt) else {
        return prompt.to_string();
    };
    if let Some(classes) = value["classes"].as_array_mut() {
        for class in classes {
            if let Some(obj) = class.as_object_mut() {
                obj.remove("existing");
            }
        }
    }
    if let Some(obj) = value.as_object_mut() {
        obj.remove("existing_shown");
        obj.insert("seed".into(), seed.json.clone());
    }
    value.to_string()
}

/// One log line per seed.
pub(crate) fn log_seed(seed: &Seed, budget: usize, window: Option<usize>) {
    log::info!(
        "🧠 interpretation seed rows_total={} rows_shown={} focus={} tokens_est={} budget={} \
         window={}",
        seed.rows_total,
        seed.rows_shown,
        seed.focus,
        seed.tokens,
        budget,
        window.map_or_else(
            || format!("{ASSUMED_WINDOW_TOKENS} (assumed)"),
            |w| w.to_string()
        ),
    );
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::perspectives::interpretation::InstanceContext;
    use crate::perspectives::interpretation::{existing_instance_context, existing_relation_links};
    use crate::perspectives::interpretation_test_support::{
        existing_map, seed_instance, setup_perspective_no_llm,
    };
    use std::collections::BTreeMap;

    fn row(id: &str, class: &str, title: &str) -> InstanceContext {
        InstanceContext {
            id: id.to_string(),
            title: title.to_string(),
            class: class.to_string(),
            properties: BTreeMap::from([("summary".to_string(), "x".repeat(200))]),
            updated_at: String::new(),
        }
    }

    /// `n` scope rows `s://0` .. `s://n-1`, newest first.
    fn flat(n: usize) -> (ExistingInstances, SeedGraph) {
        let rows: Vec<InstanceContext> = (0..n)
            .map(|i| row(&format!("s://{i}"), "Note", &format!("Note {i}")))
            .collect();
        let graph = SeedGraph {
            scope: (0..n).map(|i| format!("s://{i}")).collect(),
            rows_total: 500,
            ..Default::default()
        };
        (existing_map(rows), graph)
    }

    #[test]
    fn a_seed_that_fits_shows_every_scope_row_and_the_newest_in_full() {
        let (existing, graph) = flat(12);
        let seed = build_seed(&existing, &graph, usize::MAX);
        assert_eq!(seed.rows_shown, 12);
        assert_eq!(seed.focus, FOCUS_ROWS);
        let focus: Vec<&str> = seed.json["focus"]
            .as_array()
            .unwrap()
            .iter()
            .map(|r| r["id"].as_str().unwrap())
            .collect();
        assert_eq!(focus[0], "s://0", "the newest activity first");
        assert_eq!(focus.len(), FOCUS_ROWS);
        assert!(
            seed.json["note"]
                .as_str()
                .unwrap()
                .starts_with("The seed shows 12 of 500 rows."),
            "{}",
            seed.json["note"]
        );
    }

    // The note says whether rows the model does not see are checked for
    // duplicates: only a pass that dedups on drain checks them.
    #[test]
    fn the_note_says_whether_unshown_rows_are_checked() {
        let (existing, mut graph) = flat(1);
        let note = |g: &SeedGraph| build_seed(&existing, g, usize::MAX).json["note"].clone();
        assert!(note(&graph).as_str().unwrap().contains("are not checked"));
        graph.unshown_checked = true;
        assert!(note(&graph).as_str().unwrap().contains("are still checked"));
    }

    #[test]
    fn a_seed_over_budget_drops_focus_rows_then_the_oldest_scope_rows() {
        let (existing, graph) = flat(40);
        let all = build_seed(&existing, &graph, usize::MAX);
        let no_focus = render(
            &existing,
            &graph,
            &graph.scope.iter().collect::<Vec<_>>(),
            0,
        );
        assert!(no_focus.tokens < all.tokens);

        // Room for the outline, not for the focus rows.
        let seed = build_seed(&existing, &graph, no_focus.tokens);
        assert!(seed.tokens <= no_focus.tokens);
        assert_eq!(seed.focus, 0, "focus rows go first");
        assert_eq!(seed.rows_shown, 40, "then no scope line has to go");

        // Room for about half of the outline.
        let budget = no_focus.tokens / 2;
        let seed = build_seed(&existing, &graph, budget);
        assert!(seed.tokens <= budget, "{} > {budget}", seed.tokens);
        assert!(seed.rows_shown < 40 && seed.rows_shown > 0);
        let outline = seed.json["outline"].as_str().unwrap();
        assert!(outline.contains("(s://0)"), "the newest row stays");
        assert!(!outline.contains("(s://39)"), "the oldest row goes");
    }

    #[test]
    fn an_ancestor_from_an_earlier_source_stays_in_view() {
        let (existing, mut graph) = flat(2);
        graph
            .parents
            .insert("s://1".to_string(), vec!["old://topic".to_string()]);
        graph.other_rows.insert(
            "old://topic".to_string(),
            SeedRow {
                class: "Topic".to_string(),
                title: "Budget".to_string(),
            },
        );
        let seed = build_seed(&existing, &graph, usize::MAX);
        let outline = seed.json["outline"].as_str().unwrap();
        assert_eq!(
            outline,
            "- [Note] Note 0 (s://0)\n- [Topic] Budget (old://topic)\n  - [Note] Note 1 (s://1)",
        );
        assert_eq!(seed.rows_shown, 3);
        let focus = seed.json["focus"].as_array().unwrap();
        assert_eq!(focus[1]["parents"], json!(["old://topic"]));
    }

    #[test]
    fn the_harness_prompt_has_the_seed_and_no_existing_list() {
        let prompt = json!({
            "classes": [{"name": "Note", "existing": []}],
            "existing_shown": {"shown": 0, "total": 3},
            "transcript": [],
        })
        .to_string();
        let (existing, graph) = flat(1);
        let seed = build_seed(&existing, &graph, usize::MAX);
        let out: Value = serde_json::from_str(&with_seed(&prompt, &seed)).unwrap();
        assert!(out["classes"][0].get("existing").is_none());
        assert!(out.get("existing_shown").is_none());
        assert_eq!(out["seed"], seed.json);
    }

    pub(crate) const TOPIC_SDNA: &str = r#"{
  "target_class":"ns://Topic",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://topic"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://topic","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

    pub(crate) const NOTE_SDNA: &str = r#"{
  "target_class":"ns://Note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://note"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://note","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
    {"path":"ns://about","name":"about","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape"}
  ]
}"#;

    /// A Note class with no relation: Fixture B of the design.
    pub(crate) const FLAT_NOTE_SDNA: &str = r#"{
  "target_class":"ns://Note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://note"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://note","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

    async fn pause() {
        tokio::time::sleep(std::time::Duration::from_millis(15)).await;
    }

    // Recency is activity: a row is recent again when a row attaches to it.
    // Topic A is older than topic B; a note that attaches to A after B was
    // made puts A first.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_node_is_first_in_the_seed_after_a_row_attaches_to_it() {
        let (mut p, shapes, ctx) =
            setup_perspective_no_llm(&[("Topic", TOPIC_SDNA), ("Note", NOTE_SDNA)]).await;
        let topic = &shapes[0];
        let note = &shapes[1];
        seed_instance(&mut p, &ctx, topic, "run://a", "Topic A").await;
        pause().await;
        seed_instance(&mut p, &ctx, topic, "run://b", "Topic B").await;
        pause().await;
        seed_instance(&mut p, &ctx, note, "other://n1", "A note").await;
        p.add_link(
            crate::types::Link {
                source: "other://n1".to_string(),
                predicate: Some("ns://about".to_string()),
                target: "run://a".to_string(),
            },
            crate::types::LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();

        let existing = existing_instance_context(&p, &shapes, None).await.unwrap();
        let links = existing_relation_links(&p, &shapes).await.unwrap();
        let graph = read_seed_graph(&p, &shapes, &existing, &links, "run://", None).await;
        assert_eq!(
            graph.scope,
            vec!["run://a", "run://b"],
            "A has the newest activity"
        );
        assert_eq!(graph.rows_total, 3);

        // The note is not in the scope; with the base prefix of its source,
        // its parent topic is an ancestor in the outline.
        let graph = read_seed_graph(&p, &shapes, &existing, &links, "other://", None).await;
        assert_eq!(graph.scope, vec!["other://n1"]);
        assert_eq!(graph.parents["other://n1"], vec!["run://a"]);
        let seed = build_seed(&existing, &graph, usize::MAX);
        assert_eq!(
            seed.json["outline"].as_str().unwrap(),
            "- [Topic] Topic A (run://a)\n  - [Note] A note (other://n1)"
        );
    }

    // Fixture B: classes with no relation and no containment. The seed is the
    // scope rows by activity; nothing fails.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_flat_social_dna_gives_a_flat_seed() {
        let (mut p, shapes, ctx) =
            setup_perspective_no_llm(&[("Topic", TOPIC_SDNA), ("Note", FLAT_NOTE_SDNA)]).await;
        seed_instance(&mut p, &ctx, &shapes[0], "run://t", "A topic").await;
        pause().await;
        seed_instance(&mut p, &ctx, &shapes[1], "run://n", "A note").await;
        assert!(parent_predicates(&shapes).is_empty());

        let existing = existing_instance_context(&p, &shapes, None).await.unwrap();
        let links = existing_relation_links(&p, &shapes).await.unwrap();
        let graph = read_seed_graph(&p, &shapes, &existing, &links, "run://", None).await;
        assert_eq!(graph.scope, vec!["run://n", "run://t"]);
        assert!(graph.parents.is_empty());
        let seed = build_seed(&existing, &graph, usize::MAX);
        assert_eq!(
            seed.json["outline"].as_str().unwrap(),
            "- [Note] A note (run://n)\n- [Topic] A topic (run://t)"
        );
        assert_eq!(seed.rows_total, 2);
    }
}
