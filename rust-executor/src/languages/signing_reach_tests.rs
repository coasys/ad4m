//! What language code can sign and open, asked from inside a real language runtime.
//!
//! Every JS runtime is a language runtime, and language code sees every service extension
//! as a global. So these tests run JS through `LanguageRuntimeHandle` (bootstrap and all
//! extensions), as a managed user, instead of calling the ops' Rust bodies.

use super::language_runtime_handle::LanguageRuntimeHandle;
use crate::agent::{sealed_box::seal_to_did, AgentContext};
use serde_json::{json, Value};

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
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice = format!("alice.{}@example.org", uuid::Uuid::new_v4());
    let bob = format!("bob.{}@example.org", uuid::Uuid::new_v4());
    let alice_did = crate::test_utils::create_test_user(&alice);
    crate::test_utils::create_test_user(&bob);

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmSigningReachTest".into(), dir.path().to_path_buf(), false)
            .unwrap();
    let as_alice = AgentContext::managed(alice_did.clone());

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
        json!(alice_did),
        "a language signs as the user it runs for"
    );
    assert_eq!(
        signed["bob"],
        json!("refused"),
        "a language running for alice signed as bob"
    );

    // These signed as the main agent, over a string the language chose, whoever the call
    // ran for. A signature over `JSON.stringify(data) + timestamp` is a valid proof for a
    // main-agent-authored Expression, so they forged the main agent's expressions.
    let reach = run_as(
        &handle,
        &as_alice,
        r#"(() => {
  const ops = (globalThis.Deno && Deno.core && Deno.core.ops) || {};
  const mainAgentSigningOps = ["sign_string", "sign_device_key", "generate_entanglement_proof",
    "add_entanglement_proofs", "delete_entanglement_proofs", "wallet_sign"];
  return JSON.stringify({
    entanglementService: typeof globalThis.ENTANGLEMENT_SERVICE,
    holochainSignString: typeof HOLOCHAIN_SERVICE.signString,
    holochainCallZome: typeof HOLOCHAIN_SERVICE.callZomeFunction,
    rawOps: mainAgentSigningOps.filter((name) => typeof ops[name] === "function"),
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

/// `AGENT.openSealed` opens only what was sealed to the agent the language runs for, the
/// node or a managed user.
#[tokio::test(flavor = "multi_thread")]
async fn a_language_opens_only_boxes_sealed_to_the_agent_it_runs_for() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice =
        crate::test_utils::create_test_user(&format!("alice.{}@example.org", uuid::Uuid::new_v4()));
    let main = AgentContext::main_agent();
    let as_alice = AgentContext::managed(alice.clone());
    let main_did = crate::agent::did_for_context(&main).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmSealedBoxTest".into(), dir.path().to_path_buf(), false)
            .unwrap();

    let sealed = json!({
        "toMain": seal_to_did(&main_did, &[0x00, 0xff, 0x10]).unwrap(),
        "toAlice": seal_to_did(&alice, &[0xc0, 0xff, 0xee]).unwrap(),
    });

    let open = |sealed: &Value| {
        format!(
            r#"(() => {{
  try {{ return JSON.stringify(AGENT.openSealed({sealed})); }}
  catch (e) {{ return JSON.stringify("refused"); }}
}})()"#
        )
    };
    for (context, own, other, plaintext) in [
        (&as_alice, "toAlice", "toMain", "c0ffee"),
        (&main, "toMain", "toAlice", "00ff10"),
    ] {
        assert_eq!(
            run_as(&handle, context, &open(&sealed[own])).await,
            json!(plaintext),
            "{context:?} opens the box sealed to its DID"
        );
        assert_eq!(
            run_as(&handle, context, &open(&sealed[other])).await,
            json!("refused"),
            "{context:?} opened a box sealed to another agent"
        );
    }

    let _ = handle.teardown().await;
}

/// A call's authority is its agent token. Code running in a call for Alice that clears the
/// async context (`Deno[Deno.internal].core.setAsyncContext`), or a timer it starts then, acts
/// as no agent: it neither signs, opens
/// a box nor names an agent, and above all never as the main agent.
#[tokio::test(flavor = "multi_thread")]
async fn an_agent_op_without_the_call_token_acts_as_no_agent() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice =
        crate::test_utils::create_test_user(&format!("alice.{}@example.org", uuid::Uuid::new_v4()));
    let main_did = crate::agent::did_for_context(&AgentContext::main_agent()).unwrap();
    let as_alice = AgentContext::managed(alice.clone());
    let sealed_to_main =
        serde_json::to_string(&seal_to_did(&main_did, &[0x00, 0xff, 0x10]).unwrap()).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmNoTokenTest".into(), dir.path().to_path_buf(), false)
            .unwrap();

    let seen = run_as(
        &handle,
        &as_alice,
        &format!(
            r#"(() => {{
  const attempt = (f) => {{ try {{ return f(); }} catch (e) {{ return "refused"; }} }};
  // `Deno.core` is gone from user code, but `Deno[Deno.internal].core` is not.
  const core = Deno[Deno.internal].core;
  const cleared = (f) => {{
    const saved = core.getAsyncContext();
    core.setAsyncContext(undefined);
    try {{ return attempt(f); }} finally {{ core.setAsyncContext(saved); }}
  }};
  const sealed = {sealed_to_main};
  const probes = {{
    did: () => AGENT.did(),
    signingKeyId: () => AGENT.signingKeyId(),
    createSignedExpression: () => AGENT.createSignedExpression({{ claim: "x" }}).author,
    sign: () => AGENT.sign(new Uint8Array([1, 2, 3])).length > 0 ? "signed" : "empty",
    signStringHex: () => AGENT.signStringHex("x"),
    openSealed: () => AGENT.openSealed(sealed),
  }};
  const out = {{ withToken: {{}}, cleared: {{}} }};
  for (const [name, f] of Object.entries(probes)) {{
    out.withToken[name] = attempt(f);
    out.cleared[name] = cleared(f);
  }}
  out.setAsyncContext = typeof core.setAsyncContext;
  // A timer started with the context cleared inherits no token either.
  return new Promise((resolve) => {{
    const saved = core.getAsyncContext();
    core.setAsyncContext(undefined);
    try {{
      setTimeout(() => {{
        out.clearedTimer = {{ did: attempt(probes.did), signStringHex: attempt(probes.signStringHex) }};
        resolve(JSON.stringify(out));
      }}, 0);
    }} finally {{ core.setAsyncContext(saved); }}
  }});
}})()"#
        ),
    )
    .await;

    assert_eq!(seen["withToken"]["did"], json!(alice), "{seen}");
    assert_eq!(
        seen["withToken"]["createSignedExpression"],
        json!(alice),
        "{seen}"
    );
    assert_eq!(seen["withToken"]["sign"], json!("signed"), "{seen}");
    assert_eq!(
        seen["setAsyncContext"],
        json!("function"),
        "language code can clear its async context, so the checks are not vacuous"
    );
    for group in ["cleared", "clearedTimer"] {
        for (op, value) in seen[group].as_object().unwrap() {
            assert_eq!(
                value,
                &json!("refused"),
                "{group}: {op} acted as an agent with no call token (main agent: {main_did})"
            );
        }
    }

    let _ = handle.teardown().await;
}

/// The host's own entries into a language act as the main agent: the module's evaluation, its
/// `init()`, callback registration and a Holochain signal (a host call), and the timers each of
/// them starts.
#[tokio::test(flavor = "multi_thread")]
async fn host_entries_into_a_language_act_as_the_main_agent() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let alice =
        crate::test_utils::create_test_user(&format!("alice.{}@example.org", uuid::Uuid::new_v4()));
    let main = AgentContext::main_agent();
    let main_did = crate::agent::did_for_context(&main).unwrap();
    let key_id = crate::agent::signing_key_id_for_context(&main).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let address = "QmHostEntriesTest".to_string();
    let handle =
        LanguageRuntimeHandle::spawn(address.clone(), dir.path().to_path_buf(), false).unwrap();
    let context = super::LanguageContext::new(
        main_did.clone(),
        key_id,
        None,
        dir.path().to_path_buf(),
        address,
    );
    let context_json = context.to_json();

    let bundle = r#"
const sign = (at) => {
  try { return AGENT.createSignedExpression({ at }).author; }
  catch (e) { return "refused: " + e; }
};
const later = (at) => setTimeout(() => { globalThis.__signed[at] = sign(at); }, 0);
globalThis.__signed = { load: sign("load") };
later("loadTimer");
export const name = "host-entries-test";
export async function init() {
  globalThis.__signed.init = sign("init");
  later("initTimer");
}
export function linkSyncAddCallback(_callback) {
  globalThis.__signed.register = sign("register");
  later("registerTimer");
}
export async function handleHolochainSignal(_signal) {
  await new Promise((resolve) => setTimeout(resolve, 0));
  globalThis.__signed.signal = sign("signal");
}
"#;
    handle
        .load_module(bundle.to_string(), context)
        .await
        .expect("the module loads");
    handle
        .load_language(context_json)
        .await
        .expect("the language initialises");
    handle
        .register_callbacks()
        .await
        .expect("the callbacks register");
    handle
        .execute(
            r#"await globalThis.__handleHolochainSignal__({ cell_id: [[], []], payload: null })"#
                .into(),
        )
        .await
        .expect("the signal reaches the language");
    tokio::time::sleep(std::time::Duration::from_millis(200)).await;

    let signed = run_as(
        &handle,
        &AgentContext::managed(alice),
        "JSON.stringify(globalThis.__signed)",
    )
    .await;
    let expected: Value = [
        "load",
        "loadTimer",
        "init",
        "initTimer",
        "register",
        "registerTimer",
        "signal",
    ]
    .iter()
    .map(|at| (at.to_string(), json!(main_did)))
    .collect::<serde_json::Map<_, _>>()
    .into();
    assert_eq!(signed, expected);

    let _ = handle.teardown().await;
}
