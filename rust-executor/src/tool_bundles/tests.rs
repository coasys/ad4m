//! The sandbox and the store, against fixture bundles.
//!
//! What is pinned here is what a bundle is allowed: the language, the host globals listed in
//! `bootstrap.js`, and nothing else — so a change that leaks a global, an op or a way to load code
//! fails here. And that the limits hold: a call that runs too long or allocates too much is
//! stopped, and the next call gets a fresh isolate.

use super::{BundleSandbox, SandboxError, SandboxLimits, ToolBundleStore};
use crate::test_utils::init_v8_platform;
use serde_json::json;
use std::time::{Duration, Instant};

async fn start(source: &str) -> BundleSandbox {
    start_with(source, SandboxLimits::default()).await
}

async fn start_with(source: &str, limits: SandboxLimits) -> BundleSandbox {
    init_v8_platform();
    BundleSandbox::start("test", source.to_string(), limits)
        .await
        .expect("fixture bundle loads")
}

const API: &str = r#"
export const api = {
  factor: 2,
  items: [1, 2, 3],
  helper() {},
  async double(n) { return n * this.factor; },
  nothing() {},
  fail() { throw new Error("it broke"); },
  async reject() { throw new TypeError("it was refused"); },
  never() { return new Promise(() => {}); },
};
"#;

#[tokio::test]
async fn calls_an_export_as_the_object_it_was_found_on() {
    let sandbox = start(API).await;
    assert_eq!(
        sandbox.call(&["api", "double"], vec![json!(21)]).await,
        Ok(json!(42))
    );
    assert_eq!(
        sandbox.call(&["api", "nothing"], vec![]).await,
        Ok(json!(null))
    );
}

#[tokio::test]
async fn reads_exported_data_and_leaves_functions_out() {
    let sandbox = start(API).await;
    assert_eq!(sandbox.read(&["api", "items"]).await, Ok(json!([1, 2, 3])));
    assert_eq!(
        sandbox.read(&["api"]).await,
        Ok(json!({ "factor": 2, "items": [1, 2, 3] }))
    );
    assert_eq!(sandbox.read(&["missing"]).await, Ok(json!(null)));
}

#[tokio::test]
async fn reports_what_a_bundle_threw_and_keeps_running() {
    let sandbox = start(API).await;
    let thrown = sandbox.call(&["api", "fail"], vec![]).await;
    assert!(
        matches!(&thrown, Err(SandboxError::Js(m)) if m.contains("it broke")),
        "{thrown:?}"
    );

    let rejected = sandbox.call(&["api", "reject"], vec![]).await;
    assert!(
        matches!(&rejected, Err(SandboxError::Js(m)) if m.contains("it was refused")),
        "{rejected:?}"
    );

    let missing = sandbox.call(&["api", "items"], vec![]).await;
    assert!(
        matches!(&missing, Err(SandboxError::Js(m)) if m.contains("is not a function")),
        "{missing:?}"
    );
    let nowhere = sandbox.call(&["nope", "deeper", "call"], vec![]).await;
    assert!(
        matches!(&nowhere, Err(SandboxError::Js(m)) if m.contains("exports nothing at")),
        "{nowhere:?}"
    );

    assert_eq!(
        sandbox.call(&["api", "double"], vec![json!(2)]).await,
        Ok(json!(4))
    );
}

#[tokio::test]
async fn a_promise_that_never_settles_is_an_error_not_a_hang() {
    let sandbox = start(API).await;
    let started = Instant::now();
    let outcome = sandbox.call(&["api", "never"], vec![]).await;
    assert!(matches!(outcome, Err(SandboxError::Js(_))), "{outcome:?}");
    assert!(started.elapsed() < Duration::from_secs(5));
}

/// What V8 provides with no host at all — ECMAScript's globals (`Temporal` included) and `Intl` —
/// less `WebAssembly`, which `bootstrap.js` removes: its memory is reserved outside anything the
/// memory cap can count.
const LANGUAGE_GLOBALS: &[&str] = &[
    "globalThis",
    "Infinity",
    "NaN",
    "undefined",
    "eval",
    "isFinite",
    "isNaN",
    "parseFloat",
    "parseInt",
    "decodeURI",
    "decodeURIComponent",
    "encodeURI",
    "encodeURIComponent",
    "escape",
    "unescape",
    "AggregateError",
    "Array",
    "ArrayBuffer",
    "AsyncDisposableStack",
    "Atomics",
    "BigInt",
    "BigInt64Array",
    "BigUint64Array",
    "Boolean",
    "DataView",
    "Date",
    "DisposableStack",
    "Error",
    "EvalError",
    "FinalizationRegistry",
    "Float16Array",
    "Float32Array",
    "Float64Array",
    "Function",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "Intl",
    "Iterator",
    "JSON",
    "Map",
    "Math",
    "Number",
    "Object",
    "Promise",
    "Proxy",
    "RangeError",
    "ReferenceError",
    "Reflect",
    "RegExp",
    "Set",
    "SharedArrayBuffer",
    "String",
    "SuppressedError",
    "Symbol",
    "SyntaxError",
    "TypeError",
    "URIError",
    "Uint8Array",
    "Uint8ClampedArray",
    "Uint16Array",
    "Uint32Array",
    "WeakMap",
    "WeakRef",
    "WeakSet",
    "Temporal",
];

/// What `bootstrap.js` adds. The whole of what a bundle gets from the host.
const HOST_GLOBALS: &[&str] = &[
    "TextEncoder",
    "TextDecoder",
    "atob",
    "btoa",
    "structuredClone",
    "crypto",
    "console",
];

#[tokio::test]
async fn a_bundle_sees_the_language_and_the_host_globals_and_nothing_else() {
    let sandbox =
        start("export const globals = () => Object.getOwnPropertyNames(globalThis);").await;
    let names: Vec<String> =
        serde_json::from_value(sandbox.call(&["globals"], vec![]).await.unwrap()).unwrap();
    let extra: Vec<&String> = names
        .iter()
        .filter(|n| !LANGUAGE_GLOBALS.contains(&n.as_str()) && !HOST_GLOBALS.contains(&n.as_str()))
        .collect();
    assert!(
        extra.is_empty(),
        "globals a bundle should not have: {extra:?}"
    );
    for host in HOST_GLOBALS {
        assert!(names.iter().any(|n| n == host), "missing {host}");
    }
}

#[tokio::test]
async fn the_host_globals_work_as_the_web_s_do() {
    let sandbox = start(
        r#"
        export const check = () => {
          const text = "héllo ✓ 😀";
          const bytes = new TextEncoder().encode(text);
          const random = crypto.getRandomValues(new Uint8Array(32));
          const nested = { a: [1, { b: 2 }], when: new Date(0) };
          const copy = structuredClone(nested);
          console.log("a line from the bundle", { ok: true });
          return {
            roundTrip: new TextDecoder().decode(bytes) === text,
            fromView: new TextDecoder().decode(bytes.subarray(0, 1)),
            bytes: bytes.length,
            base64: btoa("hello"),
            decoded: atob("aGVs bG8="),
            binary: atob(btoa("\u0000ÿ")) === "\u0000ÿ",
            wide: (() => { try { btoa("✓"); return "accepted"; } catch (e) { return e.name; } })(),
            random: random.some((b) => b !== 0),
            cloned: copy !== nested && copy.a[1].b === 2 && copy.when instanceof Date,
          };
        };
        "#,
    )
    .await;
    assert_eq!(
        sandbox.call(&["check"], vec![]).await,
        Ok(json!({
            "roundTrip": true,
            "fromView": "h",
            "bytes": 15,
            "base64": "aGVsbG8=",
            "decoded": "hello",
            "binary": true,
            "wide": "InvalidCharacterError",
            "random": true,
            "cloned": true,
        }))
    );
}

#[tokio::test]
async fn a_bundle_cannot_reach_the_host_load_code_or_wait_on_a_timer() {
    let sandbox = start(
        r#"
        export const probe = async () => {
          const attempt = async (f) => { try { await f(); return "reached"; } catch { return "refused"; } };
          const global = new Function("return this")();
          return {
            deno: typeof global.Deno,
            bootstrap: typeof global.__bootstrap,
            fetch: typeof fetch,
            timers: typeof setTimeout,
            wasm: typeof global.WebAssembly,
            dynamicFile: await attempt(() => import("file:///etc/passwd")),
            dynamicNet: await attempt(() => import("https://example.com/x.js")),
          };
        };
        "#,
    )
    .await;
    assert_eq!(
        sandbox.call(&["probe"], vec![]).await,
        Ok(json!({
            "deno": "undefined",
            "bootstrap": "undefined",
            "fetch": "undefined",
            "timers": "undefined",
            "wasm": "undefined",
            "dynamicFile": "refused",
            "dynamicNet": "refused",
        }))
    );
}

#[tokio::test]
async fn a_bundle_that_imports_does_not_load() {
    init_v8_platform();
    let outcome = BundleSandbox::start(
        "importer",
        "import x from 'https://example.com/x.js'; export const y = x;".to_string(),
        SandboxLimits::default(),
    )
    .await;
    assert!(
        matches!(outcome, Err(SandboxError::Load(_))),
        "{:?}",
        outcome.err()
    );
}

#[tokio::test]
async fn a_bundle_that_does_not_parse_or_throws_while_loading_does_not_load() {
    init_v8_platform();
    for source in ["export const = ;", "throw new Error('at load');"] {
        let outcome =
            BundleSandbox::start("broken", source.to_string(), SandboxLimits::default()).await;
        assert!(
            matches!(outcome, Err(SandboxError::Load(_))),
            "{source}: {:?}",
            outcome.err()
        );
    }
}

const COUNTER: &str = r#"
let count = 0;
export const next = () => ++count;
export const spin = () => { while (true) {} };
export const hoard = () => { const kept = []; while (true) kept.push(new Array(100000).fill(kept.length)); };
"#;

#[tokio::test]
async fn a_call_past_the_time_limit_is_stopped_and_the_next_gets_a_fresh_isolate() {
    let sandbox = start_with(
        COUNTER,
        SandboxLimits {
            call_timeout: Duration::from_millis(500),
            ..SandboxLimits::default()
        },
    )
    .await;
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(1)));
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(2)));

    let started = Instant::now();
    let outcome = sandbox.call(&["spin"], vec![]).await;
    assert_eq!(
        outcome,
        Err(SandboxError::Timeout(Duration::from_millis(500)))
    );
    assert!(started.elapsed() < Duration::from_secs(5));

    // A fresh isolate: the count starts again.
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(1)));
}

#[tokio::test]
async fn a_call_past_the_heap_cap_is_stopped_and_the_next_gets_a_fresh_isolate() {
    let limit = 64 * 1024 * 1024;
    let sandbox = start_with(
        COUNTER,
        SandboxLimits {
            max_heap_bytes: limit,
            ..SandboxLimits::default()
        },
    )
    .await;
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(1)));
    assert_eq!(
        sandbox.call(&["hoard"], vec![]).await,
        Err(SandboxError::OutOfMemory(limit))
    );
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(1)));
}

/// Memory a bundle holds outside the V8 heap: the backing stores of ArrayBuffers of every kind.
/// Resizable ones would be reserved by V8 itself, outside the allocator the cap counts, so a bundle
/// cannot make one at all.
const BUFFERS: &str = r#"
const MiB = 1024 * 1024;
const kept = [];
const keep = (buffer) => { new Uint8Array(buffer).fill(1); kept.push(buffer); return kept.length; };
const times = (count, make) => { for (let i = 0; i < count; i++) keep(make()); return kept.length; };
export const ping = () => "pong";
export const plain = (mib, count) => times(count, () => new ArrayBuffer(mib * MiB));
export const typed = (mib, count) => times(count, () => new Uint8Array(mib * MiB).buffer);
export const shared = (mib, count) => times(count, () => new SharedArrayBuffer(mib * MiB));
export const resizable = (mib, count) => times(count, () => {
  const buffer = new ArrayBuffer(0, { maxByteLength: mib * MiB });
  buffer.resize(mib * MiB);
  return buffer;
});
export const growable = (mib, count) => times(count, () => {
  const buffer = new SharedArrayBuffer(0, { maxByteLength: mib * MiB });
  buffer.grow(mib * MiB);
  return buffer;
});
export const encoded = (mib, count) => {
  const text = "x".repeat(4 * MiB);
  return times((count * mib) / 4, () => new TextEncoder().encode(text).buffer) / (mib / 4);
};
export const caught = (mib, count) => { try { return times(count, () => new ArrayBuffer(mib * MiB)); } catch (e) { return "caught: " + e; } };
"#;

#[tokio::test]
async fn buffers_of_every_kind_count_against_the_memory_cap() {
    let limit = 64 * 1024 * 1024;
    for kind in ["plain", "typed", "shared", "encoded", "caught"] {
        let sandbox = start_with(
            BUFFERS,
            SandboxLimits {
                max_heap_bytes: limit,
                ..SandboxLimits::default()
            },
        )
        .await;
        // Within the cap is fine, and is kept between calls.
        assert_eq!(
            sandbox.call(&[kind], vec![json!(16), json!(2)]).await,
            Ok(json!(2)),
            "{kind}"
        );
        assert_eq!(
            sandbox.call(&[kind], vec![json!(64), json!(8)]).await,
            Err(SandboxError::OutOfMemory(limit)),
            "{kind}"
        );
        assert_eq!(
            sandbox.call(&["ping"], vec![]).await,
            Ok(json!("pong")),
            "{kind}"
        );
    }
}

/// Tamper with what `TextEncoder.encode` could look up while it copies the bytes it got from Rust,
/// each way catching what it is handed, and encode 4 MiB `count` times.
const TAMPERED_ENCODER: &str = r#"
const MiB = 1024 * 1024;
const caught = [];
const text = "x".repeat(4 * MiB);
const encodeMany = (count) => {
  const encoder = new TextEncoder();
  for (let i = 0; i < count; i++) encoder.encode(text);
  return caught.length;
};
const typedArray = Object.getPrototypeOf(Uint8Array.prototype);
export const viaConstructor = (count) => {
  const real = globalThis.Uint8Array;
  globalThis.Uint8Array = function () { return { set(source) { caught.push(source); } }; };
  try { return encodeMany(count); } finally { globalThis.Uint8Array = real; }
};
export const viaSet = (count) => {
  const real = typedArray.set;
  typedArray.set = function (source) { caught.push(source); };
  try { return encodeMany(count); } finally { typedArray.set = real; }
};
export const viaLength = (count) => {
  const real = Object.getOwnPropertyDescriptor(typedArray, "length");
  Object.defineProperty(typedArray, "length", {
    configurable: true,
    get() { caught.push(this); return real.get.call(this); },
  });
  try { return encodeMany(count); } finally { Object.defineProperty(typedArray, "length", real); }
};
"#;

#[tokio::test]
async fn a_bundle_cannot_catch_what_text_encoder_copies_from() {
    let limit = 64 * 1024 * 1024;
    for route in ["viaConstructor", "viaSet", "viaLength"] {
        let sandbox = start_with(
            TAMPERED_ENCODER,
            SandboxLimits {
                max_heap_bytes: limit,
                ..SandboxLimits::default()
            },
        )
        .await;
        // 128 MiB encoded under a 64 MiB cap: the bundle keeps nothing it was not counted for.
        assert_eq!(
            sandbox.call(&[route], vec![json!(32)]).await,
            Ok(json!(0)),
            "{route}"
        );
    }
}

#[tokio::test]
async fn a_bundle_cannot_make_a_resizable_buffer_by_any_route() {
    let limit = 64 * 1024 * 1024;
    let sandbox = start_with(
        BUFFERS,
        SandboxLimits {
            max_heap_bytes: limit,
            ..SandboxLimits::default()
        },
    )
    .await;
    for kind in ["resizable", "growable"] {
        let outcome = sandbox.call(&[kind], vec![json!(64), json!(8)]).await;
        assert!(
            matches!(&outcome, Err(SandboxError::Js(m)) if m.contains("resizable")),
            "{kind}: {outcome:?}"
        );
    }
    let probe = start(
        r#"
        export const probe = () => {
          const resizable = (make) => { try { return make().resizable || make().growable || "fixed"; } catch { return "refused"; } };
          const options = { maxByteLength: 1024 };
          const viaInstance = new Uint8Array(1).buffer.constructor;
          const viaPrototype = Object.getPrototypeOf(new ArrayBuffer(1)).constructor;
          const viaSpecies = ArrayBuffer[Symbol.species];
          class Sub extends ArrayBuffer {}
          class SharedSub extends SharedArrayBuffer {}
          const plain = new ArrayBuffer(8);
          return {
            global: resizable(() => new ArrayBuffer(8, options)),
            instance: resizable(() => new viaInstance(8, options)),
            prototype: resizable(() => new viaPrototype(8, options)),
            species: resizable(() => new viaSpecies(8, options)),
            reflect: resizable(() => Reflect.construct(viaInstance, [8, options])),
            subclass: resizable(() => new Sub(8, options)),
            shared: resizable(() => new SharedArrayBuffer(8, options)),
            sharedInstance: resizable(() => new (new Int8Array(new SharedArrayBuffer(1)).buffer.constructor)(8, options)),
            sharedSubclass: resizable(() => new SharedSub(8, options)),
            stillBuffers: plain instanceof ArrayBuffer && new Sub(4) instanceof ArrayBuffer && ArrayBuffer.isView(new Uint8Array(plain)),
            sliced: plain.slice(2).byteLength,
            transferred: plain.transfer(4).byteLength,
            name: ArrayBuffer.name + "/" + SharedArrayBuffer.name,
          };
        };
        "#,
    )
    .await;
    assert_eq!(
        probe.call(&["probe"], vec![]).await,
        Ok(json!({
            "global": "refused",
            "instance": "refused",
            "prototype": "refused",
            "species": "refused",
            "reflect": "refused",
            "subclass": "refused",
            "shared": "refused",
            "sharedInstance": "refused",
            "sharedSubclass": "refused",
            "stillBuffers": true,
            "sliced": 6,
            "transferred": 4,
            "name": "ArrayBuffer/SharedArrayBuffer",
        }))
    );
}

/// How many isolate threads of the sandbox called `name` are still running, as the sandbox counts
/// them under test: portable, and exact whatever the name's length.
fn isolate_threads(name: &str) -> usize {
    super::sandbox::running::count(name)
}

/// Wait up to `within` for `name`'s isolate thread to end. Answers whether it did.
async fn isolate_thread_ends(name: &str, within: Duration) -> bool {
    let started = Instant::now();
    while started.elapsed() < within {
        if isolate_threads(name) == 0 {
            return true;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    isolate_threads(name) == 0
}

#[tokio::test]
async fn a_bundle_that_never_finishes_loading_is_stopped_at_the_time_limit() {
    init_v8_platform();
    let limit = Duration::from_millis(500);
    let started = Instant::now();
    let outcome = tokio::time::timeout(
        Duration::from_secs(10),
        BundleSandbox::start(
            "loadspin",
            "while (true) {}\nexport const x = 1;".to_string(),
            SandboxLimits {
                call_timeout: limit,
                ..SandboxLimits::default()
            },
        ),
    )
    .await
    .expect("start answers instead of hanging");
    assert_eq!(outcome.err(), Some(SandboxError::Timeout(limit)));
    assert!(started.elapsed() < Duration::from_secs(5));
    assert!(isolate_thread_ends("loadspin", Duration::from_secs(3)).await);
}

#[tokio::test]
async fn a_start_given_up_on_stops_the_isolate_it_was_loading() {
    init_v8_platform();
    let start = BundleSandbox::start(
        "loadgone",
        "while (true) {}\nexport const x = 1;".to_string(),
        SandboxLimits {
            // Far past the test, so only giving up on the start can stop it in time.
            call_timeout: Duration::from_secs(600),
            ..SandboxLimits::default()
        },
    );
    assert!(tokio::time::timeout(Duration::from_millis(300), start)
        .await
        .is_err());
    assert!(isolate_thread_ends("loadgone", Duration::from_secs(3)).await);
}

#[tokio::test]
async fn a_call_is_stopped_at_the_time_limit_even_when_its_caller_has_gone() {
    init_v8_platform();
    let sandbox = BundleSandbox::start(
        "callgone",
        COUNTER.to_string(),
        SandboxLimits {
            call_timeout: Duration::from_millis(500),
            ..SandboxLimits::default()
        },
    )
    .await
    .expect("fixture bundle loads");
    let waiting = tokio::spawn({
        let sandbox = sandbox.clone();
        async move { sandbox.call(&["spin"], vec![]).await }
    });
    tokio::time::sleep(Duration::from_millis(100)).await;
    waiting.abort();
    assert!(waiting.await.unwrap_err().is_cancelled());
    assert_eq!(isolate_threads("callgone"), 1, "the call is still running");

    // No second call: the limit alone has to stop it.
    assert!(isolate_thread_ends("callgone", Duration::from_secs(3)).await);
    assert_eq!(sandbox.call(&["next"], vec![]).await, Ok(json!(1)));
}

#[test]
fn a_bundle_s_log_line_is_bounded_and_never_above_debug() {
    use log::Level;
    let (level, line) = super::sandbox::log_line(0, "quiet");
    assert_eq!(
        (level, line.as_str()),
        (Level::Trace, "[tool-bundle] quiet")
    );
    for (asked, name) in [(1, "info"), (2, "warn"), (3, "error"), (200, "error")] {
        let (level, line) = super::sandbox::log_line(asked, "loud");
        assert_eq!(level, Level::Debug, "{asked}");
        assert_eq!(line, format!("[tool-bundle] {name}: loud"));
    }
    let long = "é".repeat(5000);
    let (_, line) = super::sandbox::log_line(1, &long);
    assert!(line.chars().count() < 2100, "{}", line.chars().count());
    assert!(line.ends_with('…'));
}

#[tokio::test]
async fn calls_from_many_tasks_are_answered_in_turn() {
    let sandbox = start(API).await;
    let calls = (0..20).map(|n| {
        let sandbox = sandbox.clone();
        tokio::spawn(async move { sandbox.call(&["api", "double"], vec![json!(n)]).await })
    });
    for (n, call) in calls.enumerate() {
        assert_eq!(call.await.unwrap(), Ok(json!(n * 2)));
    }
}

#[test]
fn the_store_keeps_a_bundle_by_its_content_address() {
    let dir = tempfile::tempdir().unwrap();
    let store = ToolBundleStore::new(dir.path().join("tool_bundles"));
    assert_eq!(store.list().unwrap(), Vec::<String>::new());

    let address = store.put("export const a = 1;").unwrap();
    assert!(address.starts_with("Qm"));
    assert_eq!(
        address,
        crate::js_core::utils::content_address("export const a = 1;")
    );
    assert_eq!(store.put("export const a = 1;").unwrap(), address);
    assert_eq!(
        store.get(&address).unwrap().as_deref(),
        Some("export const a = 1;")
    );

    let other = store.put("export const b = 2;").unwrap();
    let mut both = vec![address.clone(), other.clone()];
    both.sort();
    assert_eq!(store.list().unwrap(), both);

    assert!(store.remove(&address).unwrap());
    assert!(!store.remove(&address).unwrap());
    assert_eq!(store.get(&address).unwrap(), None);
    assert_eq!(store.list().unwrap(), vec![other]);
}

#[test]
fn putting_a_bundle_again_replaces_a_changed_file() {
    let dir = tempfile::tempdir().unwrap();
    let store = ToolBundleStore::new(dir.path());
    let address = store.put("export const a = 1;").unwrap();
    std::fs::write(
        dir.path().join(format!("{address}.js")),
        "export const a = 2;",
    )
    .unwrap();
    assert!(store.get(&address).is_err());

    assert_eq!(store.put("export const a = 1;").unwrap(), address);
    assert_eq!(
        store.get(&address).unwrap().as_deref(),
        Some("export const a = 1;")
    );
}

#[test]
fn the_store_refuses_a_changed_file_and_anything_that_is_not_an_address() {
    let dir = tempfile::tempdir().unwrap();
    let store = ToolBundleStore::new(dir.path());
    let address = store.put("export const a = 1;").unwrap();
    std::fs::write(
        dir.path().join(format!("{address}.js")),
        "export const a = 2;",
    )
    .unwrap();
    assert_eq!(
        store.get(&address).unwrap_err().kind(),
        std::io::ErrorKind::InvalidData
    );

    for bad in ["../secret", "Qm../x", "", "Qm", "notAnAddress", "Qm/abs"] {
        assert_eq!(
            store.get(bad).unwrap_err().kind(),
            std::io::ErrorKind::InvalidInput,
            "{bad}"
        );
        assert_eq!(
            store.remove(bad).unwrap_err().kind(),
            std::io::ErrorKind::InvalidInput
        );
    }
}

/// A real bundle, when one is named: `TOOL_BUNDLE=<path to a bundle.js> cargo test --release --lib
/// tool_bundles -- --ignored --test-threads=1`. Checks it loads under the sandbox's globals and
/// answers the reads and calls every toolset bundle offers, on the first toolset it exports.
#[tokio::test]
#[ignore]
async fn a_real_bundle_loads_and_answers() {
    let Ok(path) = std::env::var("TOOL_BUNDLE") else {
        panic!("set TOOL_BUNDLE to the path of a bundle to run this");
    };
    let source = std::fs::read_to_string(&path).expect("TOOL_BUNDLE is readable");
    let started = Instant::now();
    let sandbox = start(&source).await;
    println!("loaded {} bytes in {:?}", source.len(), started.elapsed());

    let toolsets = sandbox.read(&["toolsets"]).await.expect("exports toolsets");
    let name = toolsets
        .as_object()
        .and_then(|o| o.keys().next().cloned())
        .expect("at least one toolset");
    let tools = sandbox.read(&["toolsets", &name, "tools"]).await.unwrap();
    println!("{name}: {} tools", tools.as_array().map_or(0, |t| t.len()));

    let topics = sandbox
        .call(&["toolsets", &name, "docs", "list"], vec![])
        .await
        .expect("lists its documentation");
    let first = topics[0]["path"]
        .as_str()
        .expect("a first topic")
        .to_string();
    let text = sandbox
        .call(&["toolsets", &name, "docs", "read"], vec![json!(first)])
        .await
        .unwrap();
    println!(
        "{} topics; {first} is {} chars",
        topics.as_array().map_or(0, |t| t.len()),
        text.as_str().map_or(0, |t| t.len())
    );

    let unknown = sandbox
        .call(
            &["toolsets", &name, "call"],
            vec![json!("no_such_tool"), json!({ "record": {}, "args": {} })],
        )
        .await
        .unwrap();
    assert!(unknown["text"]
        .as_str()
        .unwrap_or_default()
        .starts_with("Error"));
}
