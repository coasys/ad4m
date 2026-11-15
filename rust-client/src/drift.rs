//! Drift protection: every RPC method the client wraps must exist in the
//! executor's generated contract, `core/src/generated/api/RpcMethods.ts`
//! (itself generated from the executor's `HandlerMap`; `api/AGENTS.md`). A
//! rename or removal on either side fails here.
//!
//! Repo-only by design: the contract lives outside the crate, so the
//! packaged `ad4m-client` cannot see it — the tests skip there, and only
//! there: in a repo checkout a missing contract fails. A runtime
//! read, not `include_str!`, is what keeps the publish compiling.

use regex::Regex;
use std::collections::BTreeSet;

/// Read the generated contract off the repo tree; `None` when this is the
/// packaged crate. Panics in a checkout whose contract cannot be read.
fn contract_source() -> Option<String> {
    contract_source_at(std::path::Path::new(env!("CARGO_MANIFEST_DIR")))
        .unwrap_or_else(|e| panic!("{e}"))
}

/// `Ok(None)` only for the packaged crate: no workspace manifest next to the
/// crate directory (`cargo package`'s verify dir, the registry, a vendor
/// dir). In a checkout a read failure is an error, never a skip.
fn contract_source_at(crate_dir: &std::path::Path) -> Result<Option<String>, String> {
    let path = crate_dir.join("../core/src/generated/api/RpcMethods.ts");
    match std::fs::read_to_string(&path) {
        Ok(source) => Ok(Some(source)),
        Err(e) => {
            let in_checkout = std::fs::read_to_string(crate_dir.join("../Cargo.toml"))
                .map(|m| m.contains("[workspace]"))
                .unwrap_or(false);
            if in_checkout {
                Err(format!(
                    "repo checkout, but the generated contract {} is unreadable: {e}",
                    path.display()
                ))
            } else {
                Ok(None)
            }
        }
    }
}

/// Every `"<method>":` key of the `RpcMethods` interface.
fn generated_methods(source: &str) -> BTreeSet<String> {
    let re = Regex::new(r#"^\s*"([a-zA-Z][a-zA-Z0-9]*\.[a-zA-Z0-9]+)"\s*:"#).unwrap();
    source
        .lines()
        .filter_map(|line| re.captures(line).map(|c| c[1].to_string()))
        .collect()
}

/// The first string literal of every `call`/`call_void`/`call_value`/`send_watch`
/// invocation in `source`: the method name it wraps. Test-only code is
/// excluded item by item ([`production_code`]): the test module, test-only
/// functions and fields, and in-body test hooks, so mock `test.*` calls stay
/// out of the check while production calls after a hook are still read.
fn called_methods(source: &str) -> BTreeSet<String> {
    let re =
        Regex::new(r#"call(?:_void|_value|_inner)?(?:\s*::<[^>]*>)?\s*\(\s*"([^"]+)""#).unwrap();
    re.captures_iter(&production_code(source))
        .map(|c| c[1].to_string())
        .collect()
}

/// `source` without the items under a test-only `#[cfg]` (plain
/// `#[cfg(test)]` or the target-gated `#[cfg(all(test, ...))]`). An item
/// ends at a `;` or `,` outside brackets, or at the `}` that closes its
/// own block (unless a `.` chains on: `S { .. }.method();`), or where its
/// enclosing block closes. An inner `#![cfg(..)]` drops the rest of its
/// block. Strings, chars and comments are lexed so that a bracket or a
/// marker inside them counts for nothing.
fn production_code(source: &str) -> String {
    const MARKERS: [&str; 2] = ["#[cfg(test)]", "#[cfg(all(test,"];
    let b = source.as_bytes();
    let mut out = String::with_capacity(source.len());
    let mut i = 0;
    // `Some((depth, inner))` while inside a test-only item.
    let mut skip: Option<(i32, bool)> = None;
    while i < b.len() {
        let start = i;
        let rest = &source[i..];
        if rest.starts_with("//") {
            i += rest.find('\n').unwrap_or(rest.len());
        } else if rest.starts_with("/*") {
            i += rest.find("*/").map_or(rest.len(), |e| e + 2);
        } else if let Some(len) = string_literal_len(rest) {
            i += len;
        } else if let Some(len) = char_literal_len(rest) {
            i += len;
        } else if skip.is_none() && MARKERS.iter().any(|m| rest.starts_with(m)) {
            skip = Some((0, false));
            i += 1;
            continue;
        } else if skip.is_none()
            && MARKERS
                .iter()
                .any(|m| rest.starts_with(&m.replacen('#', "#!", 1)))
        {
            skip = Some((0, true));
            i += 1;
            continue;
        } else {
            let c = b[i];
            i += rest.chars().next().map_or(1, char::len_utf8);
            if let Some((depth, inner)) = skip.as_mut() {
                match c {
                    b'(' | b'[' | b'{' => *depth += 1,
                    b')' | b']' | b'}' if *depth == 0 => {
                        // The enclosing block closes: the item ended before it.
                        skip = None;
                        i -= 1;
                        continue;
                    }
                    b')' | b']' | b'}' => {
                        *depth -= 1;
                        if c == b'}' && *depth == 0 && !*inner {
                            let next = source[i..].trim_start();
                            if !next.starts_with('.') {
                                if next.starts_with(';') || next.starts_with(',') {
                                    i = source.len() - next.len() + 1;
                                }
                                skip = None;
                            }
                        }
                    }
                    b';' | b',' if *depth == 0 && !*inner => skip = None,
                    _ => {}
                }
                continue;
            }
        }
        if skip.is_none() {
            out.push_str(&source[start..i]);
        }
    }
    out
}

/// The byte length of the string literal (`"..."`, `b"..."`, `r#"..."#`)
/// at the start of `s`, if one starts there.
fn string_literal_len(s: &str) -> Option<usize> {
    let body = s.strip_prefix('b').unwrap_or(s);
    let prefix = s.len() - body.len();
    if let Some(raw) = body.strip_prefix('r') {
        let hashes = raw.len() - raw.trim_start_matches('#').len();
        if raw[hashes..].starts_with('"') {
            let close = format!("\"{}", "#".repeat(hashes));
            let from = prefix + 1 + hashes + 1;
            return Some(
                s[from..]
                    .find(&close)
                    .map_or(s.len(), |e| from + e + close.len()),
            );
        }
        return None;
    }
    if !body.starts_with('"') {
        return None;
    }
    let mut chars = body.char_indices().skip(1);
    while let Some((j, c)) = chars.next() {
        match c {
            '\\' => {
                chars.next();
            }
            '"' => return Some(prefix + j + 1),
            _ => {}
        }
    }
    Some(s.len())
}

/// The byte length of the char literal (`'x'`, `'\n'`, `'\u{..}'`) at the
/// start of `s`; `None` for a lifetime (`'a`).
fn char_literal_len(s: &str) -> Option<usize> {
    let body = s.strip_prefix('\'')?;
    if let Some(esc) = body.strip_prefix('\\') {
        // The escaped char itself may be a quote: `'\''`.
        return esc.get(1..)?.find('\'').map(|e| 3 + e + 1);
    }
    let c = body.chars().next()?;
    body[c.len_utf8()..]
        .starts_with('\'')
        .then(|| 1 + c.len_utf8() + 1)
}

#[test]
fn every_wrapped_rpc_exists_in_the_generated_contract() {
    let Some(source) = contract_source() else {
        eprintln!("SKIP: generated contract not present (packaged crate)");
        return;
    };
    let generated = generated_methods(&source);
    assert!(
        generated.len() > 150,
        "expected the generated contract's ~230 methods, got {}",
        generated.len()
    );

    // Every source file that wraps RPC methods.
    let sources: &[(&str, &str)] = &[
        ("agent.rs", include_str!("agent.rs")),
        ("batch.rs", include_str!("batch.rs")),
        ("connect.rs", include_str!("connect.rs")),
        ("expressions.rs", include_str!("expressions.rs")),
        ("flow.rs", include_str!("flow.rs")),
        ("instances.rs", include_str!("instances.rs")),
        ("languages.rs", include_str!("languages.rs")),
        ("model.rs", include_str!("model.rs")),
        ("neighbourhoods.rs", include_str!("neighbourhoods.rs")),
        ("perspective_proxy.rs", include_str!("perspective_proxy.rs")),
        ("perspectives.rs", include_str!("perspectives.rs")),
        ("runtime.rs", include_str!("runtime.rs")),
        ("subscription.rs", include_str!("subscription.rs")),
        ("ws_rpc.rs", include_str!("ws_rpc.rs")),
    ];

    let mut missing: Vec<String> = Vec::new();
    for (file, source) in sources {
        for method in called_methods(source) {
            if !generated.contains(&method) {
                missing.push(format!("{file}: `{method}`"));
            }
        }
    }
    // `events.watch` is sent inside `send_watch` (built as an RpcMessage, not
    // a `call` literal) — it is part of the contract all the same.
    for method in ["events.watch", "events.unwatch"] {
        if !generated.contains(method) {
            missing.push(format!("ws_rpc.rs (watch): `{method}`"));
        }
    }
    // The long-timeout list is method names too — a renamed LONG method
    // silently loses its budget.
    for method in crate::ws_rpc::LONG_METHODS {
        if !generated.contains(*method) {
            missing.push(format!("ws_rpc.rs LONG_METHODS: `{method}`"));
        }
    }

    assert!(
        missing.is_empty(),
        "wrapped RPC methods absent from the generated contract (a rename \
         or removal on either side must be reconciled):\n  {}",
        missing.join("\n  ")
    );
}

#[test]
fn the_generated_contract_is_fresh_enough_to_name_new_methods() {
    let Some(source) = contract_source() else {
        eprintln!("SKIP: generated contract not present (packaged crate)");
        return;
    };
    let generated = generated_methods(&source);
    // The methods this task relies on — a spot check that the compiled-in
    // contract is new enough (a stale one fails loudly here first).
    for method in [
        "perspective.instanceCreate",
        "perspective.instanceUpdate",
        "perspective.instanceRemove",
        "perspective.instanceAddToCollection",
        "perspective.instanceRemoveFromCollection",
        "perspective.modelQuery",
        "perspective.modelSubscribe",
        "perspective.resyncSubscription",
        "perspective.disposeQuery",
        "perspective.createBatch",
        "perspective.commitBatch",
        "perspective.discardBatch",
        "perspective.addSdna",
        "perspective.proposeFlowTransition",
        "perspective.acceptFlowProposal",
        "perspective.rejectFlowProposal",
        "perspective.flowStates",
        "perspective.flowFired",
        "perspective.verifyFlowReceipt",
        "perspective.flowValidOutputs",
        "perspective.mintFlowReceipt",
        "events.watch",
        "events.unwatch",
    ] {
        assert!(
            generated.contains(method),
            "`{method}` missing from the generated contract"
        );
    }
}

/// Only the packaged crate may skip. In a checkout (the workspace manifest
/// sits next to `rust-client/`) a contract that cannot be read is a broken
/// tree, and the drift check must fail rather than pass by skipping.
#[test]
fn a_checkout_without_the_contract_fails_instead_of_skipping() {
    let root = std::env::temp_dir().join(format!("ad4m-drift-{}", std::process::id()));
    let crate_dir = root.join("rust-client");
    std::fs::create_dir_all(&crate_dir).unwrap();

    // Packaged layout: nothing beside the crate. Skipping is right.
    assert_eq!(contract_source_at(&crate_dir), Ok(None));

    // Checkout layout: the workspace manifest is there, the contract is not.
    std::fs::write(root.join("Cargo.toml"), "[workspace]\n").unwrap();
    let missing = contract_source_at(&crate_dir);
    std::fs::remove_dir_all(&root).unwrap();
    assert!(
        missing.is_err(),
        "a checkout with no readable contract must fail, got {missing:?}"
    );
}

/// R2-2: an in-body test hook is not the test module. Production calls
/// after it are still scanned; the hook, test-only items and the test
/// module are not.
#[test]
fn test_only_items_are_skipped_and_later_production_calls_scanned() {
    let source = r#"
pub struct Shared {
    url: String,
    #[cfg(all(test, not(target_arch = "wasm32")))]
    park: Mutex<Option<u8>>,
}

impl Client {
    pub async fn early(&self) {
        self.ws.call_value("prod.early", json!({})).await;
    }

    pub async fn open(&self) {
        #[cfg(all(test, not(target_arch = "wasm32")))]
        self.ws.test_park("open:midway").await;
        let r = match frame {
            Frame::Text(t) => {
                #[cfg(test)]
                Client { shared: s.clone() }
                .call_void("test.inArm", json!({}));
                t
            }
        };
        self.ws.call_value("prod.afterHook", json!({ "a": "}" })).await;
    }

    #[cfg(all(test, not(target_arch = "wasm32")))]
    pub(crate) fn hook(&self) {
        let _ = ('{', '\'', "é}", "\"}", b"{");
        self.ws.call_value("test.hookFn", json!({}));
    }

    // A comment naming #[cfg(test)] is not an attribute.
    pub async fn late(&self) {
        self.ws.call::<Value>("prod.late", json!({})).await;
    }
}

#[cfg(test)]
mod tests {
    fn t() { ws.call_value("test.module", json!({})); }
}

pub fn after_tests() { ws.call_void("prod.afterTests", json!({})); }
"#;
    let found = called_methods(source);
    let expected: BTreeSet<String> = [
        "prod.early",
        "prod.afterHook",
        "prod.late",
        "prod.afterTests",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    assert_eq!(found, expected);

    // The real files: the call after model.rs's in-body hook is scanned,
    // and no mock `test.*` call leaks out of any test-only item.
    assert!(called_methods(include_str!("model.rs")).contains("perspective.instanceCreate"));
    for source in [
        include_str!("model.rs"),
        include_str!("subscription.rs"),
        include_str!("ws_rpc.rs"),
    ] {
        let methods = called_methods(source);
        assert!(
            methods.iter().all(|m| !m.starts_with("test.")),
            "{methods:?}"
        );
    }
}
