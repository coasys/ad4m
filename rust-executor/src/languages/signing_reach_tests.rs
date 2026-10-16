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
    "add_entanglement_proofs", "delete_entanglement_proofs", "wallet_sign"];
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

/// The server link language derives its X25519 key from the agent's signature over a
/// fixed message. Language code must not get that signature from any signing op, as
/// the node or as a user, raw bytes or string; it gets only the seed, for its own agent.
#[tokio::test(flavor = "multi_thread")]
async fn a_language_cannot_sign_the_room_key_seed_message() {
    init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice = format!("alice.{}@example.org", uuid::Uuid::new_v4());
    AgentService::ensure_user_key_exists(&alice).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmRoomKeySeedTest".into(), dir.path().to_path_buf(), false)
            .unwrap();

    let script = r#"(async () => {
  const message = "adam-server-link-language:x25519-seed:v1";
  const digest = new Uint8Array(
    await crypto.subtle.digest("SHA-256", new TextEncoder().encode(message)));
  const attempt = (f) => { try { f(); return "signed"; } catch (e) { return "refused"; } };
  let seed;
  try { seed = AGENT.roomKeySeed(); } catch (e) { seed = "error: " + e; }
  return JSON.stringify({
    signStringHex: attempt(() => AGENT.signStringHex(message)),
    sign: attempt(() => AGENT.sign(digest)),
    other: attempt(() => AGENT.signStringHex(message + ".")),
    seed,
  });
})()"#;

    for context in [
        AgentContext::main_agent(),
        AgentContext::for_user_email(alice.clone()),
    ] {
        let raw = handle
            .execute_with_context(script.to_string(), context.clone())
            .await
            .expect("the script runs");
        let reach: Value = serde_json::from_str(&raw).expect("the script returns JSON");
        assert_eq!(reach["signStringHex"], json!("refused"), "{context:?}");
        assert_eq!(reach["sign"], json!("refused"), "{context:?}");
        assert_eq!(reach["other"], json!("signed"), "{context:?}");
        let expected = hex::encode(crate::agent::room_key_seed_for_context(&context).unwrap());
        assert_eq!(
            reach["seed"],
            json!(expected),
            "AGENT.roomKeySeed() is the seed of the agent the language runs for ({context:?})"
        );
    }

    let _ = handle.teardown().await;
}
