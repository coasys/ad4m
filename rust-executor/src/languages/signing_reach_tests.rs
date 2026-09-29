//! What language code can sign, asked from inside a real language runtime.
//!
//! Every JS runtime is a language runtime, and language code sees every service extension
//! as a global. So these tests run JS through `LanguageRuntimeHandle` (bootstrap and all
//! extensions), as a managed user, instead of calling the ops' Rust bodies.

use super::language_runtime_handle::LanguageRuntimeHandle;
use crate::agent::{AgentContext, AgentService};
use serde_json::{json, Value};

fn init_v8_platform() {
    static V8_INIT: std::sync::Once = std::sync::Once::new();
    V8_INIT.call_once(|| {
        deno_core::v8::V8::set_flags_from_string("--max-opt=0");
        deno_core::JsRuntime::init_platform(None);
    });
}

/// Runs `script` (a JS expression that returns a JSON string) as `context`.
async fn run_as(handle: &LanguageRuntimeHandle, context: &AgentContext, script: &str) -> Value {
    let raw = handle
        .execute_with_context(script.to_string(), context.clone())
        .await
        .expect("the script runs");
    serde_json::from_str(&raw).expect("the script returns JSON")
}

#[tokio::test(flavor = "multi_thread")]
async fn a_language_signs_only_as_the_user_it_runs_for() {
    init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice = format!("alice.{}@example.org", uuid::Uuid::new_v4());
    let bob = format!("bob.{}@example.org", uuid::Uuid::new_v4());
    AgentService::ensure_user_key_exists(&alice).unwrap();
    AgentService::ensure_user_key_exists(&bob).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmSigningReachTest".into(), dir.path().to_path_buf(), false)
            .unwrap();
    let as_alice = AgentContext::for_user_email(alice.clone());

    let signed = run_as(
        &handle,
        &as_alice,
        &format!(
            r#"(() => {{
  const sign = (email) => {{
    try {{ return AGENT.createSignedExpressionForUser(email, {{ claim: "x" }}).author; }}
    catch (e) {{ return "refused"; }}
  }};
  return JSON.stringify({{ alice: sign({alice:?}), bob: sign({bob:?}) }});
}})()"#
        ),
    )
    .await;
    assert_eq!(
        signed["alice"],
        json!(AgentService::get_user_did_by_email(&alice).unwrap()),
        "a language signs as the user it runs for"
    );
    assert_eq!(
        signed["bob"],
        json!("refused"),
        "a language running for alice signed as bob"
    );

    // These signed as the node, over a string the language chose, whoever the call ran
    // for. A signature over `JSON.stringify(data) + timestamp` is a valid proof for a
    // node-authored Expression, so they forged the node's expressions.
    let reach = run_as(
        &handle,
        &as_alice,
        r#"(() => {
  const ops = (globalThis.Deno && Deno.core && Deno.core.ops) || {};
  const nodeSigningOps = ["sign_string", "sign_device_key", "generate_entanglement_proof",
    "add_entanglement_proofs", "delete_entanglement_proofs"];
  return JSON.stringify({
    entanglementService: typeof globalThis.ENTANGLEMENT_SERVICE,
    holochainSignString: typeof HOLOCHAIN_SERVICE.signString,
    holochainCallZome: typeof HOLOCHAIN_SERVICE.callZomeFunction,
    rawOps: nodeSigningOps.filter((name) => typeof ops[name] === "function"),
  });
})()"#,
    )
    .await;
    assert_eq!(
        reach["holochainCallZome"],
        json!("function"),
        "the holochain extension is loaded, so the next check is not vacuous"
    );
    assert_eq!(reach["entanglementService"], json!("undefined"));
    assert_eq!(reach["holochainSignString"], json!("undefined"));
    assert_eq!(reach["rawOps"], json!([]));

    let _ = handle.teardown().await;
}
