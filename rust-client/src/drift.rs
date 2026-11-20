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
/// invocation in `source`, and every literal `msg_type: "…"` of a frame
/// built by hand (`events.watch`): the method names it sends. Test modules are
/// excluded — a top-level `#[cfg(test)] mod … {` through its closing `}` at
/// column 0 — so mock `test.*` calls stay out of the check. Other
/// `#[cfg(test)]` items (fields, hooks inside function bodies) are kept:
/// production calls after them are still scanned.
fn called_methods(source: &str) -> BTreeSet<String> {
    let re =
        Regex::new(r#"call(?:_void|_value|_inner)?(?:\s*::<[^>]*>)?\s*\(\s*"([^"]+)""#).unwrap();
    let mod_start = Regex::new(r"^(?:pub(?:\([^)]*\))?\s+)?mod\s+\w+\s*\{").unwrap();
    let mut production = String::with_capacity(source.len());
    let mut lines = source.lines().peekable();
    while let Some(line) = lines.next() {
        if line == "#[cfg(test)]" && lines.peek().is_some_and(|next| mod_start.is_match(next)) {
            for skipped in lines.by_ref() {
                if skipped == "}" {
                    break;
                }
            }
            continue;
        }
        production.push_str(line);
        production.push('\n');
    }
    let frame = Regex::new(r#"msg_type:\s*"([^"]+)""#).unwrap();
    re.captures_iter(&production)
        .chain(frame.captures_iter(&production))
        .map(|c| c[1].to_string())
        .collect()
}

/// An in-body `#[cfg(test)]` hook must not hide the production calls after
/// it, and the test module must stay excluded wherever it sits.
#[test]
fn called_methods_scans_past_in_body_test_attributes() {
    let source = r#"
fn a(ws: &Ws) {
    ws.call_value("before.hook", json!({}));
    #[cfg(test)]
    ws.test_park("x");
    ws.call_value("after.hook", json!({}));
}

#[cfg(test)]
mod tests {
    fn t(ws: &Ws) {
        ws.call("test.mock", json!({}));
    }
}

fn b(ws: &Ws) {
    ws.call::<Value>("after.tests", json!({}));
    let msg = RpcMessage { id, msg_type: "events.wtach".to_string(), params };
}
"#;
    let methods: Vec<String> = called_methods(source).into_iter().collect();
    assert_eq!(
        methods,
        ["after.hook", "after.tests", "before.hook", "events.wtach"]
    );
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
    // `events.watch` is built as an `RpcMessage` (its `msg_type` literal
    // is scanned above); `events.unwatch` is part of the same contract.
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
