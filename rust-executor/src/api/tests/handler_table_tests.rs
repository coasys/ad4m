//! RPC contract export.
//!
//! Writes `RpcMethods.ts` — every method registered in the WS-RPC
//! `HandlerMap` with the TypeScript types of its params and result, and its
//! `read` / `long` flags — next to the ts-rs types it imports. Same trigger
//! and destination as ts-rs: `cargo test --lib` writes to `$TS_RS_EXPORT_DIR`
//! (default `bindings/`); `pnpm run generate:api-types` in `core/` points it
//! at `core/src/generated/api/`.
//!
//! Also writes `Events.ts` — every event the events socket emits, with the
//! TypeScript type of its payload, and the perspective-scoped ones.
//!
//! `request.cancel` and `ping` are handled inline by the socket reader, not
//! the map, so they are not in the table.

use std::collections::BTreeSet;
use std::path::Path;

use crate::api::events_ws::{event_specs, EventSpec};
use crate::api::ws_handler::{build_handler_map, HandlerMap};

const FILE_NAME: &str = "RpcMethods.ts";
const EVENTS_FILE_NAME: &str = "Events.ts";

fn quoted(names: impl Iterator<Item = String>) -> String {
    names.map(|n| format!("  \"{}\",\n", n)).collect::<String>()
}

/// Does the type expression `ty` mention the type `name`?
fn named_in(ty: &str, name: &str) -> bool {
    regex::Regex::new(&format!(r"\b{}\b", regex::escape(name)))
        .unwrap()
        .is_match(ty)
}

/// Deterministic render of `Events.ts`: events in table order, imports sorted.
pub(crate) fn render_events(specs: &[EventSpec]) -> String {
    let imports: BTreeSet<(String, String)> = specs
        .iter()
        .flat_map(|s| s.payload.deps.iter())
        .filter(|(name, _)| specs.iter().any(|s| named_in(&s.payload.name, name)))
        .map(|(name, path)| {
            let module = path.with_extension("").display().to_string();
            (name.clone(), format!("./{}", module))
        })
        .collect();

    let mut out = String::new();
    out.push_str(
        "// Auto-generated from the executor's event table (rust-executor/src/api/events_ws.rs).\n",
    );
    out.push_str("// Do NOT edit manually — regenerate with: pnpm run generate:api-types\n\n");
    for (name, module) in &imports {
        out.push_str(&format!(
            "import type {{ {} }} from \"{}\";\n",
            name, module
        ));
    }
    out.push_str("\n/** Every event the executor emits: its payload (the message without `type`). */\nexport interface EventMap {\n");
    for s in specs {
        out.push_str(&format!("  \"{}\": {};\n", s.name, s.payload.name));
    }
    out.push_str("}\n\nexport type EventName = keyof EventMap;\n\n");
    let scoped: Vec<String> = specs
        .iter()
        .filter(|s| s.scope.is_some())
        .map(|s| format!("\"{}\"", s.name))
        .collect();
    out.push_str("/** Events about one perspective: they carry `perspectiveUuid`. */\n");
    out.push_str(&format!(
        "export type ScopedEventName = {};\n\n",
        scoped.join(" | ")
    ));
    out.push_str("/** {@link ScopedEventName} as a set. */\n");
    out.push_str("export const SCOPED_EVENTS: ReadonlySet<EventName> = new Set<EventName>([\n");
    out.push_str(&quoted(
        specs
            .iter()
            .filter(|s| s.scope.is_some())
            .map(|s| s.name.to_string()),
    ));
    out.push_str("]);\n");
    out
}

/// Deterministic render: methods sorted by name, imports sorted, fixed header.
pub(crate) fn render_rpc_methods(map: &HandlerMap) -> String {
    let specs = map.specs();
    // Import only the types a contract names; the others reach it through those.
    let named = |name: &str| {
        specs
            .iter()
            .any(|s| named_in(&s.params.name, name) || named_in(&s.result.name, name))
    };
    let imports: BTreeSet<(String, String)> = specs
        .iter()
        .flat_map(|s| s.params.deps.iter().chain(s.result.deps.iter()))
        .filter(|(name, _)| named(name))
        .map(|(name, path)| {
            let module = path.with_extension("").display().to_string();
            (name.clone(), format!("./{}", module))
        })
        .chain(
            specs
                .iter()
                .any(|s| {
                    named_in(&s.params.name, "EventName") || named_in(&s.result.name, "EventName")
                })
                .then(|| ("EventName".to_string(), "./Events".to_string())),
        )
        .collect();

    let mut out = String::new();
    out.push_str("// Auto-generated from the executor's WS-RPC HandlerMap (rust-executor/src/api/ws_handler.rs).\n");
    out.push_str("// Do NOT edit manually — regenerate with: pnpm run generate:api-types\n\n");
    for (name, module) in &imports {
        out.push_str(&format!(
            "import type {{ {} }} from \"{}\";\n",
            name, module
        ));
    }
    out.push_str("\n/** Every executor RPC method: its params and result. */\nexport interface RpcMethods {\n");
    for s in &specs {
        out.push_str(&format!(
            "  \"{}\": {{ params: {}; result: {} }};\n",
            s.name, s.params.name, s.result.name
        ));
    }
    out.push_str("}\n\nexport type RpcMethod = keyof RpcMethods;\n\n");
    out.push_str("/** Idempotent reads: the client resends one once after a reconnect. */\n");
    out.push_str("export const READ_METHODS: ReadonlySet<RpcMethod> = new Set<RpcMethod>([\n");
    out.push_str(&quoted(
        specs.iter().filter(|s| s.read).map(|s| s.name.clone()),
    ));
    out.push_str("]);\n\n");
    out.push_str(
        "/** Calls that can run for minutes: the client's default timeout is its long one. */\n",
    );
    out.push_str("export const LONG_METHODS: ReadonlySet<RpcMethod> = new Set<RpcMethod>([\n");
    out.push_str(&quoted(
        specs.iter().filter(|s| s.long).map(|s| s.name.clone()),
    ));
    out.push_str("]);\n");
    out
}

/// Writes the table and every type it uses to the ts-rs export directory.
#[test]
fn export_rpc_methods() {
    let cfg = ts_rs::Config::from_env();
    let dir = std::env::var("TS_RS_EXPORT_DIR").unwrap_or_else(|_| "bindings".to_string());
    std::fs::create_dir_all(&dir).expect("create export dir");
    let map = build_handler_map();
    for spec in map.specs() {
        (spec.params.export)(&cfg).expect("export params types");
        (spec.result.export)(&cfg).expect("export result types");
    }
    std::fs::write(Path::new(&dir).join(FILE_NAME), render_rpc_methods(&map))
        .expect("write RpcMethods.ts");
    let events = event_specs();
    for spec in &events {
        (spec.payload.export)(&cfg).expect("export event payload types");
    }
    std::fs::write(
        Path::new(&dir).join(EVENTS_FILE_NAME),
        render_events(&events),
    )
    .expect("write Events.ts");
}

/// The committed SDK copy — the table and every type it imports — must match
/// the live map, so a contract cannot change without regenerating it.
#[test]
fn committed_sdk_rpc_methods_are_current() {
    if std::env::var("TS_RS_EXPORT_DIR").is_ok() {
        // Generation run: `export_rpc_methods` is rewriting the files.
        return;
    }
    let core = Path::new(env!("CARGO_MANIFEST_DIR")).join("../core");
    if !core.join("package.json").exists() {
        // Crate built outside the monorepo: nothing to compare against.
        return;
    }
    let dir = core.join("src/generated/api");
    let hint = "run `pnpm run generate:api-types` in core/";
    let map = build_handler_map();
    let on_disk = std::fs::read_to_string(dir.join(FILE_NAME))
        .unwrap_or_else(|e| panic!("core/src/generated/api/{FILE_NAME} is missing ({e}): {hint}"));
    assert_eq!(
        on_disk,
        render_rpc_methods(&map),
        "core/src/generated/api/{FILE_NAME} is stale: {hint}"
    );
    let events = event_specs();
    let events_on_disk = std::fs::read_to_string(dir.join(EVENTS_FILE_NAME)).unwrap_or_else(|e| {
        panic!("core/src/generated/api/{EVENTS_FILE_NAME} is missing ({e}): {hint}")
    });
    assert_eq!(
        events_on_disk,
        render_events(&events),
        "core/src/generated/api/{EVENTS_FILE_NAME} is stale: {hint}"
    );

    let cfg = ts_rs::Config::from_env();
    let stale: BTreeSet<String> = map
        .specs()
        .iter()
        .flat_map(|s| {
            (s.params.stale)(&cfg, &dir)
                .into_iter()
                .chain((s.result.stale)(&cfg, &dir))
        })
        .chain(events.iter().flat_map(|s| (s.payload.stale)(&cfg, &dir)))
        .collect();
    assert!(stale.is_empty(), "stale generated types {stale:?}: {hint}");
}

/// Dispatch checks params against the method's contract before the handler
/// runs (here `expression.get` without its required `url`).
#[tokio::test]
async fn dispatch_rejects_params_outside_the_contract() {
    let ctx = crate::api::tests::support::admin_ctx();
    let err = build_handler_map()
        .dispatch("expression.get", serde_json::json!({ "raw": true }), ctx)
        .await
        .expect_err("url is required");
    assert_eq!(err.code, 400);
    assert!(
        err.message.starts_with("Invalid params for expression.get"),
        "{}",
        err.message
    );
}
