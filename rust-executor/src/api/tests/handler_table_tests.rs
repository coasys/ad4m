//! Handler-table export.
//!
//! Writes `HandlerMethods.ts` — every method name registered in the WS-RPC
//! `HandlerMap` — next to the ts-rs request/response types. Same trigger and
//! destination as ts-rs: `cargo test --lib` writes to `$TS_RS_EXPORT_DIR`
//! (default `bindings/`); `pnpm run generate:api-types` in `core/` points it
//! at `core/src/generated/api/`.
//!
//! `request.cancel` and `ping` are handled inline by the socket reader, not
//! the map, so they are not in the table.

use crate::api::ws_handler::build_handler_map;

const FILE_NAME: &str = "HandlerMethods.ts";

/// Deterministic render: sorted names, fixed header, trailing newline.
pub(crate) fn render_handler_table(names: &[String]) -> String {
    let mut out = String::new();
    out.push_str("// Auto-generated from the executor's WS-RPC HandlerMap (rust-executor/src/api/ws_handler.rs).\n");
    out.push_str("// Do NOT edit manually — regenerate with: pnpm run generate:api-types\n\n");
    out.push_str("export const HANDLER_METHODS = [\n");
    for name in names {
        out.push_str(&format!("  \"{}\",\n", name));
    }
    out.push_str("] as const;\n\n");
    out.push_str("export type HandlerMethod = (typeof HANDLER_METHODS)[number];\n");
    out
}

#[test]
fn handler_table_is_sorted_and_unique() {
    let names = build_handler_map().method_names();
    assert!(
        names.len() > 100,
        "expected the full table, got {}",
        names.len()
    );
    for pair in names.windows(2) {
        assert!(pair[0] < pair[1], "not sorted/unique: {:?}", pair);
    }
    assert!(names.iter().any(|n| n == "runtime.info"));
}

/// Writes the table to the ts-rs export directory.
#[test]
fn export_handler_table() {
    let dir = std::env::var("TS_RS_EXPORT_DIR").unwrap_or_else(|_| "bindings".to_string());
    std::fs::create_dir_all(&dir).expect("create export dir");
    let path = std::path::Path::new(&dir).join(FILE_NAME);
    let rendered = render_handler_table(&build_handler_map().method_names());
    std::fs::write(&path, rendered).expect("write handler table");
}

/// The committed SDK copy must match the live map, so a new handler cannot
/// ship without regenerating it.
#[test]
fn committed_sdk_handler_table_is_current() {
    if std::env::var("TS_RS_EXPORT_DIR").is_ok() {
        // Generation run: `export_handler_table` is rewriting the file.
        return;
    }
    let core = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../core");
    if !core.join("package.json").exists() {
        // Crate built outside the monorepo: nothing to compare against.
        return;
    }
    let committed = core.join("src/generated/api").join(FILE_NAME);
    let on_disk = std::fs::read_to_string(&committed).unwrap_or_else(|e| {
        panic!(
            "core/src/generated/api/{FILE_NAME} is missing ({e}): run `pnpm run generate:api-types` in core/"
        )
    });
    assert_eq!(
        on_disk,
        render_handler_table(&build_handler_map().method_names()),
        "core/src/generated/api/{FILE_NAME} is stale: run `pnpm run generate:api-types` in core/"
    );
}
