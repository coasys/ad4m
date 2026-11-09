//! The KV store of `ad4m:host` (host.js `storagePut`/`storageDelete`), run
//! inside a real language isolate with a persisted KV file.
//!
//! Every write used to rewrite the language's whole KV file: a 1,833-link
//! commit in the server link language made 7,352 rewrites and took 42.6 s.

use super::JsCore;

async fn language_isolate() -> (JsCore, tempfile::TempDir) {
    let dir = tempfile::tempdir().expect("tempdir");
    let js = JsCore::new_for_language(dir.path().to_path_buf(), false);
    js.set_language_context(
        dir.path().to_string_lossy().into_owned(),
        "kv-flush-test-language".to_string(),
        "{}".to_string(),
    );
    js.init_for_language().await.expect("bootstrap");
    js.load_module_from_source(
        "https://ad4m.language/kv-flush-test/bundle.js",
        r#"import { storagePut, storageDelete } from "ad4m:host";
           globalThis.__kv = { storagePut, storageDelete };"#
            .to_string(),
    )
    .await
    .expect("load test bundle");
    (js, dir)
}

/// Counts host.js's KV file rewrites while `body` runs, then lets the event
/// loop turn once so a deferred flush lands before the count is read.
fn counting(body: &str) -> String {
    format!(
        r#"(async () => {{
            const lc = globalThis.LANGUAGE_CONTROLLER;
            const write = lc.writeStorageFile;
            let writes = 0;
            lc.writeStorageFile = (path, content) => {{ writes++; write(path, content); }};
            {body}
            await new Promise((r) => setTimeout(r, 0));
            lc.writeStorageFile = write;
            return String(writes);
        }})()"#
    )
}

fn kv_file(dir: &tempfile::TempDir) -> serde_json::Map<String, serde_json::Value> {
    let raw = std::fs::read_to_string(dir.path().join("ad4m-language-kv.json")).expect("KV file");
    serde_json::from_str(&raw).expect("KV file is JSON")
}

#[tokio::test]
async fn many_writes_in_one_turn_rewrite_the_kv_file_once() {
    let (js, dir) = language_isolate().await;

    let writes = js
        .execute(&counting(
            r#"for (let i = 0; i < 1000; i++) globalThis.__kv.storagePut("k" + i, "v" + i);"#,
        ))
        .await
        .expect("puts ran");
    assert_eq!(
        writes, "1",
        "1,000 puts in one turn rewrote the KV file {writes} times"
    );
    let file = kv_file(&dir);
    assert_eq!(file.len(), 1000);
    assert_eq!(file["kv-flush-test-language::k999"], "v999");

    let writes = js
        .execute(&counting(
            r#"for (let i = 0; i < 500; i++) globalThis.__kv.storageDelete("k" + i);"#,
        ))
        .await
        .expect("deletes ran");
    assert_eq!(
        writes, "1",
        "500 deletes in one turn rewrote the KV file {writes} times"
    );
    let file = kv_file(&dir);
    assert_eq!(file.len(), 500);
    assert!(!file.contains_key("kv-flush-test-language::k0"));
}

/// The flush is deferred only to the end of the current turn: a write in a
/// later turn is on disk when that turn ends, not lost or batched further.
#[tokio::test]
async fn a_write_in_a_later_turn_is_flushed_too() {
    let (js, dir) = language_isolate().await;
    js.execute(&counting(r#"globalThis.__kv.storagePut("a", "1");"#))
        .await
        .expect("first put");
    let writes = js
        .execute(&counting(r#"globalThis.__kv.storagePut("b", "2");"#))
        .await
        .expect("second put");
    assert_eq!(writes, "1");
    let file = kv_file(&dir);
    assert_eq!(file["kv-flush-test-language::a"], "1");
    assert_eq!(file["kv-flush-test-language::b"], "2");
}
