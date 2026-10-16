//! `registerDNAs` (language_bootstrap.js) against a conductor whose install fails, run inside
//! a real language runtime with `HOLOCHAIN_SERVICE` replaced by a fake.
//!
//! On a fresh or just-restarted executor an install can fail once and work on the next try:
//! the install timed out (the conductor is still compiling wasm and starting cells for every
//! language loading at once), and the app is not installed yet when `getAppInfo` looks. The
//! constructor must retry instead of failing the language until the next restart.

use super::language_runtime_handle::LanguageRuntimeHandle;
use serde_json::Value;

fn init_v8_platform() {
    static V8_INIT: std::sync::Once = std::sync::Once::new();
    V8_INIT.call_once(|| {
        deno_core::v8::V8::set_flags_from_string("--max-opt=0");
        deno_core::JsRuntime::init_platform(None);
    });
}

/// Installs a fake `HOLOCHAIN_SERVICE` whose first `failures` installs throw `error` without
/// installing anything, then calls `registerDNAs` and reports what happened as JSON.
fn script(failures: usize, error: &str, retry_delays_ms: &str) -> String {
    format!(
        r#"(async () => {{
  const installed = new Map();
  const calls = {{ install: 0, getAppInfo: 0 }};
  let failuresLeft = {failures};
  globalThis.HOLOCHAIN_SERVICE = {{
    getAgentKey: async () => new Uint8Array(39),
    installApp: async (payload) => {{
      calls.install += 1;
      if (failuresLeft > 0) {{
        failuresLeft -= 1;
        throw new Error({error:?});
      }}
      const info = {{ installed_app_id: payload.installed_app_id, cell_info: {{}} }};
      installed.set(payload.installed_app_id, info);
      return info;
    }},
    getAppInfo: async (appId) => {{
      calls.getAppInfo += 1;
      return installed.get(appId) ?? null;
    }},
  }};
  const delegate = createHolochainDelegate("QmRegisterDnasTest", {{ retryDelaysMs: {retry_delays_ms} }});
  try {{
    const infos = await delegate.registerDNAs([{{ nick: "main", file: [1, 2, 3] }}], () => {{}});
    return JSON.stringify({{ ok: true, appIds: infos.map((i) => i.installed_app_id), calls }});
  }} catch (e) {{
    return JSON.stringify({{ ok: false, error: String(e && e.message || e), calls }});
  }}
}})()"#
    )
}

async fn run(failures: usize, error: &str, retry_delays_ms: &str) -> Value {
    init_v8_platform();
    let dir = tempfile::tempdir().unwrap();
    let handle =
        LanguageRuntimeHandle::spawn("QmRegisterDnasTest".into(), dir.path().to_path_buf(), false)
            .unwrap();
    let raw = handle
        .execute(format!(
            "await {}",
            script(failures, error, retry_delays_ms)
        ))
        .await
        .expect("the script runs");
    serde_json::from_str(&raw).expect("the script returns JSON")
}

#[tokio::test(flavor = "multi_thread")]
async fn register_dnas_retries_an_install_that_failed_before_the_app_existed() {
    let result = run(1, "Timeout error; InstallApp call", "[10, 10, 10]").await;
    assert_eq!(
        result["ok"],
        Value::Bool(true),
        "the constructor failed instead of retrying the install: {result}"
    );
    assert_eq!(
        result["appIds"],
        serde_json::json!(["QmRegisterDnasTest-main"])
    );
    assert_eq!(result["calls"]["install"], 2);
}

#[tokio::test(flavor = "multi_thread")]
async fn register_dnas_gives_up_after_its_retries_and_names_the_install_error() {
    let result = run(usize::MAX >> 12, "conductor said no", "[10, 10]").await;
    assert_eq!(result["ok"], Value::Bool(false), "{result}");
    let error = result["error"].as_str().unwrap();
    assert!(
        error.contains("QmRegisterDnasTest-main") && error.contains("conductor said no"),
        "{error}"
    );
    // One first attempt plus one per retry delay.
    assert_eq!(result["calls"]["install"], 3);
}
