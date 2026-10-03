//! Built-in interfaces: checked-in contracts, generated SDK files, metering.

use std::path::Path;

use serde_json::{json, Value};

use super::{document, documents, start_all, Builtin};
use crate::services::builtin::{CallContext, Caller};
use crate::services::codegen;
use crate::services::host::ServiceHost;

fn manifest_dir() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

fn interface_path(b: Builtin) -> std::path::PathBuf {
    manifest_dir()
        .join("src/services/builtins/interfaces")
        .join(format!("{}.json", b.file_stem()))
}

fn pretty(v: &Value) -> String {
    serde_json::to_string_pretty(v).unwrap() + "\n"
}

/// The checked-in interface documents must match the Rust types, so a
/// contract cannot change without the reviewed JSON changing. Regenerate:
/// `UPDATE_SERVICE_FIXTURES=1 cargo test --lib services::builtins::tests::interfaces_are_current`.
#[test]
fn interfaces_are_current() {
    let update = std::env::var("UPDATE_SERVICE_FIXTURES").is_ok();
    for (b, doc) in documents() {
        let path = interface_path(*b);
        let built = pretty(&doc.raw);
        if update {
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(&path, &built).unwrap();
        }
        let committed = std::fs::read_to_string(&path).unwrap_or_default();
        assert_eq!(
            committed,
            built,
            "{} is stale; see this test's doc comment",
            path.display()
        );
    }
}

/// `ad4m service-gen`'s TypeScript for each built-in, as the SDK imports it.
pub(crate) fn sdk_module(b: Builtin) -> String {
    codegen::typescript(document(b)).replace(
        "from \"@coasys/ad4m\"",
        "from \"../../services/ServiceClient\"",
    )
}

/// The hashes `rust-client` addresses built-in methods by.
pub(crate) fn rust_client_constants() -> String {
    let mut out = String::from("// Generated from the executor's built-in service interfaces. Do not edit.\n// Regenerate with `pnpm run generate:api-types` in core/.\n\n");
    for (b, doc) in documents() {
        let name = b.file_stem().replace('.', "_").to_uppercase();
        out.push_str(&format!(
            "/// `{}` {}\npub const {}: &str = \"{}\";\n",
            doc.doc.name, doc.doc.version, name, doc.hash
        ));
    }
    out
}

fn sdk_dir() -> std::path::PathBuf {
    manifest_dir().join("../core/src/generated/services")
}

fn rust_client_file() -> std::path::PathBuf {
    manifest_dir().join("../rust-client/src/services.rs")
}

/// Writes the SDK service definitions and the `rust-client` constants when
/// `TS_RS_EXPORT_DIR` is set (`pnpm run generate:api-types`).
#[test]
fn export_service_definitions() {
    if std::env::var("TS_RS_EXPORT_DIR").is_err() {
        return;
    }
    std::fs::create_dir_all(sdk_dir()).unwrap();
    for b in Builtin::ALL {
        std::fs::write(
            sdk_dir().join(format!("{}.ts", b.file_stem())),
            sdk_module(b),
        )
        .unwrap();
    }
    std::fs::write(rust_client_file(), rust_client_constants()).unwrap();
}

/// The committed SDK definitions and `rust-client` constants match the interfaces.
#[test]
fn generated_service_files_are_current() {
    if std::env::var("TS_RS_EXPORT_DIR").is_ok() {
        return;
    }
    for b in Builtin::ALL {
        let path = sdk_dir().join(format!("{}.ts", b.file_stem()));
        let committed = std::fs::read_to_string(&path).unwrap_or_default();
        assert_eq!(
            committed,
            sdk_module(b),
            "{} is stale: run `pnpm run generate:api-types` in core/",
            path.display()
        );
    }
    let committed = std::fs::read_to_string(rust_client_file()).unwrap_or_default();
    assert_eq!(
        committed,
        rust_client_constants(),
        "rust-client/src/services.rs is stale: run `pnpm run generate:api-types` in core/"
    );
}

fn ctx(user: Option<&str>) -> CallContext {
    CallContext {
        caller: Caller::App,
        origin: vec![],
        agent_did: Some("did:key:z6MkBuiltinTest".into()),
        user: user.map(str::to_string),
        auth_token: None,
        is_admin: false,
        grants: vec![vec![
            crate::agent::capabilities::defs::ALL_CAPABILITY.clone()
        ]],
        deadline: None,
    }
}

#[tokio::test]
async fn builtins_register_and_start_once() {
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    start_all(&host).await.unwrap();
    let reg = host.registry();
    assert_eq!(reg.implementations().count(), 4);
    assert!(reg.implementations().all(|i| i.is_running()));
    for (_, doc) in documents() {
        assert!(reg.interface(&doc.hash).is_some());
        assert_eq!(doc.doc.author, super::AUTHOR);
    }
}

#[tokio::test]
async fn admin_only_methods_refuse_a_broad_grant() {
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    let ledger = &document(Builtin::BillingLedger).hash;
    // `ALL` grants every action, but host rates need the admin credential.
    let e = host
        .dispatch(
            &format!("{}.setRates", ledger),
            json!({ "rates": [] }),
            ctx(None),
        )
        .await
        .unwrap_err();
    assert_eq!(e.code, 403);
}

#[tokio::test]
async fn closed_params_reject_unknown_fields() {
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    let inference = &document(Builtin::AiInference).hash;
    let e = host
        .dispatch(
            &format!("{}.prompt", inference),
            json!({ "taskId": "t", "prompt": "p", "extra": 1 }),
            ctx(None),
        )
        .await
        .unwrap_err();
    assert_eq!(e.code, 400);
}

#[tokio::test]
async fn params_are_checked_before_the_service_runs() {
    // No conductor runs here: a 400 proves the host checked the contract first.
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    let conductor = &document(Builtin::HolochainConductor).hash;
    for bad in [
        json!({ "agentInfos": "only-one" }),
        json!({}),
        json!({ "agentInfos": [1] }),
    ] {
        let e = host
            .dispatch(&format!("{}.addAgentInfos", conductor), bad, ctx(None))
            .await
            .unwrap_err();
        assert_eq!(e.code, 400);
    }
}

fn ctx_with(grants: Vec<crate::agent::capabilities::Capability>, is_admin: bool) -> CallContext {
    CallContext {
        grants: vec![grants],
        is_admin,
        ..ctx(None)
    }
}

/// Host rates and the Unyt membrane proof: grants first, then the admin
/// credential. No case reaches the global DB, which other tests share.
#[tokio::test]
async fn host_rate_and_membrane_proof_check_access() {
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    let ledger = &document(Builtin::BillingLedger).hash;
    let unyt = &document(Builtin::UnytWallet).hash;
    for (method, params) in [
        (format!("{}.rates", ledger), json!({})),
        (format!("{}.setRates", ledger), json!({ "rates": [] })),
        (
            format!("{}.setMembraneProof", unyt),
            json!({ "proof": "cHJvb2Y=" }),
        ),
        (format!("{}.versionInfo", unyt), json!({})),
    ] {
        let e = host
            .dispatch(&method, params, ctx_with(vec![], true))
            .await
            .unwrap_err();
        assert_eq!(e.code, 403, "{method}");
    }
    // A grant is not enough for operator actions.
    let e = host
        .dispatch(
            &format!("{}.setMembraneProof", unyt),
            json!({ "proof": "cHJvb2Y=" }),
            ctx(None),
        )
        .await
        .unwrap_err();
    assert_eq!(e.code, 403);
}

#[tokio::test]
async fn host_rate_and_membrane_proof_setters_refuse_invalid_params() {
    let host = ServiceHost::new();
    start_all(&host).await.unwrap();
    let set_rates = format!("{}.setRates", document(Builtin::BillingLedger).hash);
    let set_proof = format!("{}.setMembraneProof", document(Builtin::UnytWallet).hash);
    let admin = || CallContext {
        is_admin: true,
        ..ctx(None)
    };
    // Outside the contract: 400 from the host.
    for (method, params) in [
        (&set_rates, json!({})),
        (&set_rates, json!({ "rates": "[]" })),
        (&set_rates, json!({ "rates": [{ "description": "a" }] })),
        (&set_proof, json!({})),
    ] {
        let e = host
            .dispatch(method, params.clone(), admin())
            .await
            .unwrap_err();
        assert_eq!(e.code, 400, "{method} {params}");
    }
    // Inside the contract but refused: the declared 422 with its name.
    for (method, params, name) in [
        (
            &set_rates,
            json!({ "rates": [{ "description": "a", "priceInHOT": -1 }] }),
            "InvalidRates",
        ),
        (
            &set_rates,
            json!({ "rates": [{ "description": "", "priceInHOT": 1 }] }),
            "InvalidRates",
        ),
        (
            &set_rates,
            json!({ "rates": [{ "description": "a", "priceInHOT": 1 }, { "description": "a", "priceInHOT": 2 }] }),
            "InvalidRates",
        ),
        (&set_proof, json!({ "proof": "" }), "InvalidProof"),
        (
            &set_proof,
            json!({ "proof": "not base64!" }),
            "InvalidProof",
        ),
    ] {
        let e = host
            .dispatch(method, params.clone(), admin())
            .await
            .unwrap_err();
        assert_eq!(
            (e.code, e.data.as_ref().map(|d| d["name"].clone())),
            (422, Some(json!(name))),
            "{method} {params}"
        );
    }
}

#[test]
fn users_get_the_service_grants_they_need() {
    let caps = crate::agent::capabilities::get_user_default_capabilities();
    let has = |b: Builtin, action: &str| {
        let need = super::capability(b, action);
        caps.iter()
            .any(|c| c.with.domain == need.with.domain && c.can == need.can)
    };
    for (b, action) in [
        (Builtin::AiInference, "PROMPT"),
        (Builtin::HolochainConductor, "PEERS"),
        (Builtin::BillingSettlement, "PAY"),
    ] {
        assert!(has(b, action), "{:?} {}", b, action);
    }
    // Operator-only actions are not granted to users.
    for (b, action) in [
        (Builtin::BillingLedger, "ADMIN"),
        (Builtin::UnytWallet, "ADMIN"),
        (Builtin::HolochainConductor, "ADMIN"),
    ] {
        assert!(!has(b, action), "{:?} {}", b, action);
    }
}

/// Outside the services and each service's own module, executor code reaches
/// AI, billing, Unyt and Holochain operations only through the service host.
/// Lifecycle (starting the AI service, stopping the conductor) is exempt.
#[test]
fn internal_callers_go_through_the_host() {
    const FORBIDDEN: &[&str] = &[
        "AIService::",
        "crate::billing::",
        "unyt_service::",
        "get_holochain_service()",
        "maybe_get_holochain_service()",
        "holochain_service_once_started()",
        "HolochainService::",
    ];
    /// The service modules themselves.
    const OWN: &[&str] = &[
        "ai_service/",
        "billing.rs",
        "unyt_service.rs",
        "holochain_service/",
        "services/",
    ];
    /// Lifecycle orchestration.
    const LIFECYCLE: &[(&str, &str)] = &[
        ("lib.rs", "AIService::init_global_instance()"),
        ("lib.rs", "holochain_service::maybe_get_holochain_service()"),
    ];
    fn is_test_file(rel: &str) -> bool {
        rel.ends_with("tests.rs")
            || rel.contains("test_support")
            || rel.contains("e2e")
            || rel.contains("real_llm")
    }
    let root = manifest_dir().join("src");
    let mut stack = vec![root.clone()];
    let mut offenders = Vec::new();
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|e| e.to_str()) != Some("rs") {
                continue;
            }
            let rel = path
                .strip_prefix(&root)
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/");
            if OWN.iter().any(|o| rel.starts_with(o)) || is_test_file(&rel) {
                continue;
            }
            let text = std::fs::read_to_string(&path).unwrap();
            // Code after `#[cfg(test)] mod tests` is test code.
            let code = text.split("#[cfg(test)]\nmod tests").next().unwrap();
            for (n, line) in code.lines().enumerate() {
                let trimmed = line.trim_start();
                if trimmed.starts_with("//") {
                    continue;
                }
                for needle in FORBIDDEN {
                    if line.contains(needle)
                        && !LIFECYCLE.iter().any(|(f, l)| rel == *f && line.contains(l))
                    {
                        offenders.push(format!("{}:{}: {}", rel, n + 1, trimmed));
                    }
                }
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "direct service calls outside the host:\n{}",
        offenders.join("\n")
    );
}
