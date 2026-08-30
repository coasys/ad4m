//! Static-tool → side-effect classification table.
//!
//! Every `#[tool]`-annotated method registered on `Ad4mMcpHandler`'s
//! `tool_router` gets one row here declaring whether the tool reads or
//! writes. [`side_effect_of`] looks up a name and returns the classification;
//! [`harness_bridge::rmcp_tool_to_schema`] calls it for every static tool
//! it hands to the harness.
//!
//! ## Why this exists
//!
//! James Weir's 2026-08-25 PR #911 review:
//!
//! > The security boundary of the whole design inferred from a verb match
//! > on tool names — the structural fix is `side_effect: Read | Write` on
//! > `ToolSchema`, populated at the `#[tool]` macro. False positives are
//! > equally silent and already present: a class named `Signal` produces
//! > `signal_query` / `signal_get` / `signal_list`, all of which classify
//! > as writes, because `signal` is the leading token and it's in
//! > `WRITE_VERBS`. The verb vocabulary and consumer class names share a
//! > namespace, and collisions resolve by accident in both directions.
//!
//! Verb-token inference (the pre-#911 shape) had two failure modes:
//!
//! * **False negatives** — writes slipping through the read-only cut when
//!   the verb list didn't cover a mutator (`_add_<coll>` vs
//!   `_add_to_<coll>`, plugged in `436477457`).
//! * **False positives** — reads misclassified as writes when a consumer
//!   class name collided with the verb vocabulary (the `Signal` example
//!   above). Silent — the LLM just couldn't read that class.
//!
//! Explicit declaration at each tool's emission point (this table for
//! static tools, `SideEffect::Write`/`Read` at construction site for
//! dynamic per-class + per-relation tools) fixes both directions
//! structurally.
//!
//! ## Adding a new `#[tool]`
//!
//! Add a row to [`STATIC_TOOL_SIDE_EFFECTS`] naming the tool + its
//! side-effect. The [`every_registered_static_tool_has_a_side_effect_row`]
//! test in this module enumerates `tool_router.list_all()` and asserts
//! every name has an entry — a `#[tool]` added without a matching row
//! fails CI. The compile-time table + runtime parity check is what makes
//! forgetting-to-classify unmergeable.
//!
//! ## Why not put this at the `#[tool]` macro site?
//!
//! Ideal shape would be `#[tool(name = "…", side_effect = Write)]` — one
//! declaration at the point of definition. That requires extending `rmcp`'s
//! macro, which we don't own. The table is the least-invasive intermediary:
//! declaration lives in this file, but the parity test wires it back to
//! the `#[tool]` set as tightly as we can without forking rmcp.

use crate::ai_service::harness::provider::SideEffect;

/// Canonical classification for every static `#[tool]` method registered
/// on `Ad4mMcpHandler`.
///
/// Missing names fall back to [`SideEffect::Read`] (the safer default);
/// the parity test in this module ensures no static tool actually hits
/// that fallback in practice.
///
/// Ordering matches the tool-file layout (perspectives → subjects →
/// profiles → flows → children → subscriptions → neighbourhoods → auth
/// → languages) for readability.
pub(crate) const STATIC_TOOL_SIDE_EFFECTS: &[(&str, SideEffect)] = &[
    // ── perspectives.rs ─────────────────────────────────────────────
    ("list_perspectives", SideEffect::Read),
    ("get_models", SideEffect::Read),
    ("add_perspective", SideEffect::Write),
    ("add_link", SideEffect::Write),
    ("query_links", SideEffect::Read),
    ("add_model", SideEffect::Write),
    // `infer` runs an LLM prompt and returns the text. No graph mutation —
    // the tool is a compute-and-return, not a state change. Billing is
    // handled per-completion inside AIService; the harness classifies on
    // the graph-state axis.
    ("infer", SideEffect::Read),
    // ── subjects.rs ─────────────────────────────────────────────────
    ("query_subjects", SideEffect::Read),
    ("get_subject_data", SideEffect::Read),
    ("create_subject", SideEffect::Write),
    // `execute_commands` runs SDNA-declared setter actions on an instance
    // — always a mutation.
    ("execute_commands", SideEffect::Write),
    ("set_subject_property", SideEffect::Write),
    ("get_subject_collection", SideEffect::Read),
    ("add_to_collection", SideEffect::Write),
    ("remove_from_collection", SideEffect::Write),
    ("get_subject_children", SideEffect::Read),
    ("delete_subject", SideEffect::Write),
    // ── profiles.rs ─────────────────────────────────────────────────
    ("get_my_did", SideEffect::Read),
    ("get_agent_profile", SideEffect::Read),
    ("set_agent_profile", SideEffect::Write),
    ("set_agent_profile_picture", SideEffect::Write),
    ("get_agent_public_perspective", SideEffect::Read),
    ("set_agent_public_perspective", SideEffect::Write),
    // ── flows.rs ────────────────────────────────────────────────────
    ("add_flow", SideEffect::Write),
    ("get_flows", SideEffect::Read),
    // `flow_state` and `flow_actions` inspect the current flow instance;
    // no mutation.
    ("flow_state", SideEffect::Read),
    ("flow_actions", SideEffect::Read),
    // `flow_start` mints a new flow instance node.
    ("flow_start", SideEffect::Write),
    // `flow_run_action` executes an action — writes the action's effect
    // + advances the flow's state.
    ("flow_run_action", SideEffect::Write),
    // ── children.rs ─────────────────────────────────────────────────
    ("add_child", SideEffect::Write),
    ("get_children", SideEffect::Read),
    ("get_children_body_parsed", SideEffect::Read),
    // ── subscriptions.rs ────────────────────────────────────────────
    // Both tools construct a query string / return config metadata; the
    // actual subscription is a separate transport-level concern outside
    // the tool's own effect.
    ("generate_waker_query", SideEffect::Read),
    ("get_mention_waker_config", SideEffect::Read),
    // ── neighbourhoods.rs ───────────────────────────────────────────
    ("list_link_language_templates", SideEffect::Read),
    ("neighbourhood_publish_from_perspective", SideEffect::Write),
    ("neighbourhood_join_from_url", SideEffect::Write),
    // ── auth.rs ─────────────────────────────────────────────────────
    // Every auth mutation creates or rotates a session / capability /
    // account — all writes. `auth_status` is the only pure read.
    ("login_email", SideEffect::Write),
    ("request_capability", SideEffect::Write),
    ("generate_jwt", SideEffect::Write),
    ("signup", SideEffect::Write),
    ("request_login_verification", SideEffect::Write),
    ("verify_email_code", SideEffect::Write),
    ("auth_status", SideEffect::Read),
    // ── languages.rs ────────────────────────────────────────────────
    ("language_meta", SideEffect::Read),
];

/// Look up the declared side-effect for a static `#[tool]` name, or
/// classify a dynamic per-class tool by its verb suffix.
///
/// Static tools take precedence — an explicit table row always wins over
/// suffix matching (defense in depth: a static tool renamed to collide
/// with a dynamic pattern doesn't silently reclassify).
///
/// Dynamic per-class tools follow the strict shape
/// `<class_lower>_<verb>[_<prop_or_coll>]` emitted by the `dynamic.rs::make_*`
/// generators:
///
/// | verb suffix           | side_effect | emitter |
/// |-----------------------|-------------|---------|
/// | `_query`              | Read        | `make_query_tool` |
/// | `_list`               | Read        | `make_list_tool` |
/// | `_get`                | Read        | `make_get_tool` |
/// | `_get_<coll>`         | Read        | `make_collection_get_tool` |
/// | `_create`             | Write       | `make_create_tool` |
/// | `_delete`             | Write       | `make_delete_tool` |
/// | `_set_<prop>`         | Write       | `make_set_property_tool` |
/// | `_add_<coll>`         | Write       | `make_collection_add_tool` |
/// | `_remove_<coll>`      | Write       | `make_collection_remove_tool` |
///
/// The classifier looks at the **second token only** (position 1 after
/// the class-name prefix), NOT at every token. That's how it stops
/// mis-classifying `signal_query` — a user class `Signal` puts `signal`
/// at position 0 (ignored) and `query` at position 1 (Read). James
/// Weir's 2026-08-25 false-positive example: fixed structurally by not
/// consulting position-0 tokens.
///
/// Unknown shapes fall back to [`SideEffect::Read`] — the safer default.
/// Propose-* wrappers (`<class>_create` / `<class>_propose_link_child`)
/// declare their own side_effect at construction site
/// (`ai_service::harness::propose`) and never come through this path.
pub(crate) fn side_effect_of(name: &str) -> SideEffect {
    if let Some(se) = STATIC_TOOL_SIDE_EFFECTS
        .iter()
        .find_map(|(n, se)| (*n == name).then_some(*se))
    {
        return se;
    }
    classify_dynamic_tool_by_verb_suffix(name)
}

/// Classify a dynamic per-class tool name by its position-1 verb.
///
/// The generators in `mcp/tools/dynamic.rs` always emit
/// `<class_lower>_<verb>[…]`. This function ignores position 0 (the class
/// name — user-controlled, can legitimately collide with the verb
/// vocabulary) and reads only position 1. That's the structural fix for
/// the `Signal` false-positive: `signal_query` → position 0 = `signal`
/// (ignored), position 1 = `query` → Read.
fn classify_dynamic_tool_by_verb_suffix(name: &str) -> SideEffect {
    let mut tokens = name.splitn(3, '_');
    let _class = tokens.next(); // position 0 — user-controlled, ignored
    let verb = tokens.next(); // position 1 — the classifier's only input

    match verb {
        Some("query" | "list" | "get") => SideEffect::Read,
        Some("create" | "delete" | "set" | "add" | "remove") => SideEffect::Write,
        // No verb (single-token name) or unknown verb → default Read.
        // An unknown-verb read never triggers an accidental mutation;
        // the failure direction is "misses the read cut for a would-be
        // legitimate tool the harness doesn't yet expose", not "silently
        // writes when it shouldn't". Safer failure mode.
        _ => SideEffect::Read,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn side_effect_of_looks_up_known_name() {
        assert_eq!(side_effect_of("add_perspective"), SideEffect::Write);
        assert_eq!(side_effect_of("list_perspectives"), SideEffect::Read);
        assert_eq!(side_effect_of("query_links"), SideEffect::Read);
        assert_eq!(side_effect_of("add_link"), SideEffect::Write);
        assert_eq!(side_effect_of("infer"), SideEffect::Read);
    }

    #[test]
    fn side_effect_of_defaults_to_read_for_unknown() {
        // The safer default. The parity test enforces that no static tool
        // actually hits this in practice.
        assert_eq!(
            side_effect_of("single_token_no_verb"),
            SideEffect::Read,
            "unknown static + unknown-verb dynamic → Read"
        );
    }

    #[test]
    fn dynamic_classifier_reads_position_one_verb() {
        // Standard dynamic-tool shape: `<class>_<verb>[_<prop>]`.
        // Position-1 verb decides.
        for (name, expected) in [
            ("belief_query", SideEffect::Read),
            ("intention_list", SideEffect::Read),
            ("task_get", SideEffect::Read),
            ("channel_get_messages", SideEffect::Read),
            ("belief_create", SideEffect::Write),
            ("task_delete", SideEffect::Write),
            ("task_set_title", SideEffect::Write),
            ("channel_add_messages", SideEffect::Write),
            ("channel_remove_messages", SideEffect::Write),
        ] {
            assert_eq!(
                side_effect_of(name),
                expected,
                "dynamic tool `{name}` should classify as {expected:?}"
            );
        }
    }

    /// The James Weir 2026-08-25 false-positive regression: a user
    /// declaring a `Signal` subject class produces `signal_query` /
    /// `signal_get` / `signal_list` tools. Under the pre-#911 verb-token
    /// classifier, `signal` was in `WRITE_VERBS` (it matched the
    /// `send_signal` / `signal_broadcast` static mutators), so every
    /// token position was scanned and `signal` at position 0 flipped
    /// the classification to Write. The Signal class's read tools then
    /// vanished from the harness's read-only surface — silent — and
    /// no interpretation pass could see that class.
    #[test]
    fn dynamic_classifier_ignores_class_name_verb_collisions() {
        // Every collision case: the class name is a word that ALSO
        // happens to be a write verb in the pre-#911 vocabulary.
        for user_class in [
            "signal", "send", "add", "remove", "delete", "create", "update", "set", "publish",
            "join", "grant", "revoke", "signup", "login", "logout", "install", "generate", "mint",
            "start", "run",
        ] {
            let read_tool = format!("{user_class}_query");
            assert_eq!(
                side_effect_of(&read_tool),
                SideEffect::Read,
                "user class `{user_class}` read tool `{read_tool}` must NOT classify as Write \
                 (pre-#911 false-positive James Weir's review called out)"
            );

            let list_tool = format!("{user_class}_list");
            assert_eq!(
                side_effect_of(&list_tool),
                SideEffect::Read,
                "user class `{user_class}` list tool `{list_tool}` must survive the read cut"
            );

            // Confirm the write tools for the same class still classify
            // correctly — the fix isn't over-inclusive.
            let write_tool = format!("{user_class}_create");
            assert_eq!(
                side_effect_of(&write_tool),
                SideEffect::Write,
                "user class `{user_class}` create tool `{write_tool}` still classifies as Write"
            );
        }
    }

    #[test]
    fn static_table_takes_precedence_over_dynamic_verb_suffix() {
        // Defense in depth: if a static tool ever collides with a
        // dynamic-shaped name, the table row is authoritative.
        // `add_link` is in the static table as Write. A hypothetical
        // dynamic tool called `add_link` (which can't actually happen
        // because dynamic tools are class-scoped, but the invariant is
        // worth pinning) would ALSO classify Write from the suffix — so
        // this test also confirms static+dynamic agree on the shared
        // verb vocabulary.
        assert_eq!(side_effect_of("add_link"), SideEffect::Write);
        assert_eq!(side_effect_of("query_links"), SideEffect::Read);
    }

    #[test]
    fn table_has_no_duplicate_names() {
        // A duplicated entry would silently hide the second row (find_map
        // stops at the first match). This test asserts the table is a
        // single-source-of-truth.
        let mut names: Vec<&str> = STATIC_TOOL_SIDE_EFFECTS.iter().map(|(n, _)| *n).collect();
        names.sort();
        let unique_count = names
            .iter()
            .zip(names.iter().skip(1))
            .filter(|(a, b)| a != b)
            .count()
            + 1;
        assert_eq!(
            names.len(),
            unique_count,
            "STATIC_TOOL_SIDE_EFFECTS contains duplicate names"
        );
    }

    /// Enumerate every static tool the router actually registers and assert
    /// each has an explicit row in [`STATIC_TOOL_SIDE_EFFECTS`]. This is
    /// the compile-time-adjacent enforcement James Weir's review asked for:
    /// a `#[tool]` added without a table row fails CI, so no static tool
    /// silently lands on the fallback classification.
    ///
    /// Runs at test time, not build time (Rust doesn't expose reflection
    /// over macro-registered items), but the test executes in `cargo test`
    /// which CI runs on every push — same practical guarantee.
    #[tokio::test]
    async fn every_registered_static_tool_has_a_side_effect_row() {
        use crate::mcp::server::McpContext;
        use crate::mcp::tools::Ad4mMcpHandler;
        use std::sync::Arc;
        use tokio::sync::RwLock;

        let handler = Ad4mMcpHandler::new(McpContext {
            admin_credential: None,
            auth_token: Arc::new(RwLock::new(None)),
        });

        let table_names: std::collections::BTreeSet<&str> =
            STATIC_TOOL_SIDE_EFFECTS.iter().map(|(n, _)| *n).collect();

        let mut missing: Vec<String> = Vec::new();
        for tool in handler.tool_router.list_all() {
            let name = tool.name.to_string();
            if !table_names.contains(name.as_str()) {
                missing.push(name);
            }
        }

        assert!(
            missing.is_empty(),
            "these #[tool]-registered methods have no STATIC_TOOL_SIDE_EFFECTS row \
             (add one — pick Read for query/list/get tools, Write for mutators): {missing:?}"
        );
    }

    /// Reverse of the above — no dead rows in the table pointing at tools
    /// that don't exist. Prevents drift when a `#[tool]` is renamed or
    /// removed but the table row gets left behind.
    #[tokio::test]
    async fn every_side_effect_row_matches_a_registered_static_tool() {
        use crate::mcp::server::McpContext;
        use crate::mcp::tools::Ad4mMcpHandler;
        use std::sync::Arc;
        use tokio::sync::RwLock;

        let handler = Ad4mMcpHandler::new(McpContext {
            admin_credential: None,
            auth_token: Arc::new(RwLock::new(None)),
        });

        let registered: std::collections::BTreeSet<String> = handler
            .tool_router
            .list_all()
            .iter()
            .map(|t| t.name.to_string())
            .collect();

        let mut stale: Vec<&str> = Vec::new();
        for (name, _) in STATIC_TOOL_SIDE_EFFECTS.iter() {
            if !registered.contains(*name) {
                stale.push(*name);
            }
        }

        assert!(
            stale.is_empty(),
            "these STATIC_TOOL_SIDE_EFFECTS rows point at tools that are no longer \
             registered (remove them or fix the name): {stale:?}"
        );
    }
}
