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

// ---------------------------------------------------------------------------
// Sealed storage: the files go through the host's storage ops, and a
// non-system language has no file permission of its own.
// ---------------------------------------------------------------------------

use crate::at_rest::lang_storage;
use crate::at_rest::DataKeys;
use std::sync::Arc;

fn open_keys(folder: &std::path::Path) -> Arc<DataKeys> {
    let keys = DataKeys::load(crate::at_rest::keyring_file::path_in(folder)).unwrap();
    keys.unlock(zeroize::Zeroizing::new([7u8; 32])).unwrap();
    Arc::new(keys)
}

/// A language isolate on `folder`, its files sealed under `keys`.
async fn sealed_isolate(
    folder: &std::path::Path,
    address: &str,
    system: bool,
    keys: Arc<DataKeys>,
) -> JsCore {
    std::fs::create_dir_all(folder).unwrap();
    let js = JsCore::new_for_language(folder.to_path_buf(), system);
    js.set_language_context_with_keys(
        folder.to_string_lossy().into_owned(),
        address.to_string(),
        "{}".to_string(),
        Some(keys),
    );
    js.init_for_language().await.expect("bootstrap");
    js.load_module_from_source(
        &format!("https://ad4m.language/{address}/bundle.js"),
        r#"import { storagePut, readStorageFile, writeStorageFile } from "ad4m:host";
           globalThis.__host = { storagePut, readStorageFile, writeStorageFile };"#
            .to_string(),
    )
    .await
    .expect("load test bundle");
    js
}

/// Runs `body` (JS statements that may throw) and returns "ok: <value>" or
/// "error: <name>: <message>".
fn outcome(body: &str) -> String {
    format!(
        r#"(async () => {{
            try {{ const v = await (async () => {{ {body} }})(); return "ok: " + String(v); }}
            catch (e) {{ return "error: " + (e && e.name) + ": " + (e && e.message); }}
        }})()"#
    )
}

fn js_str(s: &std::path::Path) -> String {
    serde_json::to_string(&s.to_string_lossy()).unwrap()
}

#[tokio::test]
async fn the_kv_file_is_sealed_on_disk_and_does_not_open_in_another_languages_folder() {
    let data = tempfile::tempdir().unwrap();
    let keys = open_keys(data.path());
    let a = data.path().join("languages").join("QmSealedA");
    let b = data.path().join("languages").join("QmSealedB");
    std::fs::create_dir_all(&b).unwrap();
    let js = sealed_isolate(&a, "QmSealedA", false, keys.clone()).await;

    js.execute(&outcome(
        r#"globalThis.__host.storagePut("secret-key", "a private value");
           await new Promise((r) => setTimeout(r, 0));"#,
    ))
    .await
    .unwrap();
    let raw = std::fs::read(a.join("ad4m-language-kv.json")).expect("KV file");
    assert!(
        raw.starts_with(lang_storage::MAGIC),
        "the KV file starts with the magic"
    );
    assert!(
        !raw.windows(15).any(|w| w == b"a private value"),
        "the value is not on disk in the clear"
    );
    assert!(
        std::str::from_utf8(&raw).is_err()
            || serde_json::from_slice::<serde_json::Value>(&raw).is_err(),
        "the file is not readable JSON"
    );

    // The language reads its own file back through the host.
    let path = js_str(&a.join("ad4m-language-kv.json"));
    let read = js
        .execute(&outcome(&format!(
            "return globalThis.__host.readStorageFile({path});"
        )))
        .await
        .unwrap();
    assert!(read.contains("a private value"), "{read}");

    // Moved into another language's folder, it does not open there.
    std::fs::copy(
        a.join("ad4m-language-kv.json"),
        b.join("ad4m-language-kv.json"),
    )
    .unwrap();
    let other = lang_storage::Caller {
        folder: &b,
        address: "QmSealedB",
        system: false,
    };
    let err = lang_storage::read_file(Some(&keys), other, "ad4m-language-kv.json").unwrap_err();
    assert!(
        matches!(err, lang_storage::LangStorageError::DoesNotOpen { .. }),
        "{err}"
    );
}

#[tokio::test]
async fn a_non_system_language_has_no_direct_file_access_to_its_storage_folder() {
    let data = tempfile::tempdir().unwrap();
    let keys = open_keys(data.path());
    let a = data.path().join("QmNoFiles");
    let js = sealed_isolate(&a, "QmNoFiles", false, keys).await;
    std::fs::write(a.join("planted.txt"), "plain").unwrap();
    let file = js_str(&a.join("planted.txt"));
    let new_file = js_str(&a.join("direct.txt"));
    let folder = js_str(&a);

    for body in [
        format!("return Deno.readTextFileSync({file});"),
        format!("return await Deno.readTextFile({file});"),
        format!("Deno.writeTextFileSync({new_file}, 'x'); return 'written';"),
        format!("await Deno.writeTextFile({new_file}, 'x'); return 'written';"),
        format!("return [...Deno.readDirSync({folder})].length;"),
        format!(
            "const {{ DatabaseSync }} = await import('node:sqlite'); \
             new DatabaseSync({new_file}); return 'opened';"
        ),
    ] {
        let out = js.execute(&outcome(&body)).await.unwrap();
        assert!(
            out.starts_with("error: NotCapable") || out.starts_with("error: PermissionDenied"),
            "{body}: {out}"
        );
    }
    assert!(
        !a.join("direct.txt").exists(),
        "nothing was written directly"
    );
    assert_eq!(
        std::fs::read_to_string(a.join("planted.txt")).unwrap(),
        "plain"
    );
}

#[tokio::test]
async fn a_language_cannot_read_or_write_another_languages_files() {
    let data = tempfile::tempdir().unwrap();
    let keys = open_keys(data.path());
    let a = data.path().join("QmReaderA");
    let b = data.path().join("QmOwnerB");
    std::fs::create_dir_all(&b).unwrap();
    let owner = lang_storage::Caller {
        folder: &b,
        address: "QmOwnerB",
        system: false,
    };
    lang_storage::write_file(Some(&keys), owner, "notes.json", "b's notes").unwrap();
    let js = sealed_isolate(&a, "QmReaderA", false, keys.clone()).await;

    let theirs = js_str(&b.join("notes.json"));
    for body in [
        format!("return globalThis.__host.readStorageFile({theirs});"),
        "return globalThis.__host.readStorageFile('../QmOwnerB/notes.json');".to_string(),
        format!("globalThis.__host.writeStorageFile({theirs}, 'overwritten'); return 'written';"),
        format!("return Deno.readTextFileSync({theirs});"),
    ] {
        let out = js.execute(&outcome(&body)).await.unwrap();
        assert!(out.starts_with("error:"), "{body}: {out}");
        assert!(!out.contains("b's notes"), "{body}: {out}");
    }
    assert_eq!(
        lang_storage::read_file(Some(&keys), owner, "notes.json").unwrap(),
        "b's notes"
    );
}

#[tokio::test]
async fn a_system_language_keeps_its_file_access() {
    let data = tempfile::tempdir().unwrap();
    let keys = open_keys(data.path());
    let a = data.path().join("QmSystemLanguage");
    let js = sealed_isolate(&a, "QmSystemLanguage", true, keys).await;
    let file = js_str(&a.join("direct.txt"));
    let out = js
        .execute(&outcome(&format!(
            "Deno.writeTextFileSync({file}, 'direct'); return Deno.readTextFileSync({file});"
        )))
        .await
        .unwrap();
    assert_eq!(out, "ok: direct");
    let out = js
        .execute(&outcome(
            "globalThis.__host.writeStorageFile('kept.json', 'sealed'); \
             return globalThis.__host.readStorageFile('kept.json');",
        ))
        .await
        .unwrap();
    assert_eq!(out, "ok: sealed");
    assert!(std::fs::read(a.join("kept.json"))
        .unwrap()
        .starts_with(lang_storage::MAGIC));
}
