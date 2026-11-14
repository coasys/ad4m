//! Drift protection: every RPC method the client wraps must exist in the
//! executor's generated contract, `core/src/generated/api/RpcMethods.ts`
//! (itself generated from the executor's `HandlerMap`; `api/AGENTS.md`). A
//! rename or removal on either side fails here.
//!
//! Repo-only by design: the contract lives outside the crate, so the
//! packaged `ad4m-client` cannot see it — the tests skip there. A runtime
//! read, not `include_str!`, is what keeps the publish compiling.

use regex::Regex;
use std::collections::BTreeSet;

/// Read the generated contract off the repo tree; `None` when this is the
/// packaged crate (no `core/` next to `rust-client/`).
fn contract_source() -> Option<String> {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../core/src/generated/api/RpcMethods.ts");
    std::fs::read_to_string(path).ok()
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
/// invocation in `source`: the method name it wraps. Test code is excluded —
/// the test module's `#[cfg]` attribute (plain `#[cfg(test)]` or the
/// target-gated `#[cfg(all(test, ...))]` used since the wasm port) ends the
/// file in this crate, so scanning the prefix keeps mock `test.*` calls out
/// of the check.
fn called_methods(source: &str) -> BTreeSet<String> {
    let re =
        Regex::new(r#"call(?:_void|_value|_inner)?(?:\s*::<[^>]*>)?\s*\(\s*"([^"]+)""#).unwrap();
    let cut = ["#[cfg(test)]", "#[cfg(all(test,"]
        .iter()
        .filter_map(|attr| source.find(attr))
        .min()
        .unwrap_or(source.len());
    let production = &source[..cut];
    re.captures_iter(production)
        .map(|c| c[1].to_string())
        .collect()
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
