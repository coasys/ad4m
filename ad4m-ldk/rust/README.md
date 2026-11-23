# ad4m-ldk

**AD4M Language Development Kit — Rust, compiled to WebAssembly.**

[AD4M](https://ad4m.dev) is an agent-centric runtime for decentralized
applications. In AD4M, every storage backend and every communication
protocol is a pluggable **Language** — a small module with a
well-defined interface, loaded into a sandboxed isolate by the AD4M
executor. This crate is the SDK for authoring AD4M Languages in Rust
and compiling them to a single-file WASM bundle the executor can load
alongside Languages written in TypeScript.

- Full conceptual overview: **[docs.ad4m.dev/languages](https://docs.ad4m.dev/languages)**
- Normative interface spec (WIT):
  [`ad4m-lang.wit`](https://github.com/coasys/ad4m/blob/dev/docs-src/ad4m-lang.wit)
- Prose spec:
  [`language-interface-spec.md`](https://github.com/coasys/ad4m/blob/dev/docs-src/language-interface-spec.md)

## Add to your crate

```toml
[package]
name = "my-ad4m-language"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
ad4m-ldk = "0.13"
```

Build targets WASM via `wasm-bindgen`:

```sh
cargo build --target wasm32-unknown-unknown --release
wasm-bindgen --target deno --out-dir build \
    target/wasm32-unknown-unknown/release/my_ad4m_language.wasm
```

The AD4M repo's `bootstrap-languages/agent-language/rust-impl/` has a
worked `build.sh` you can copy.

## Minimal example

```rust
use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

pub struct NoteStore;

impl Language for NoteStore {
    fn name()    -> &'static str { "note-store" }
    fn version() -> &'static str { "1.0.0" }

    async fn init() -> LanguageResult<Self> {
        Ok(NoteStore)
    }
}

impl ExpressionCapability for NoteStore {
    async fn expression_create(
        &mut self,
        content: serde_json::Value,
    ) -> LanguageResult<Address> {
        let signed = rt::agent_create_signed_expression_typed(&content);
        let body = rt::http_post_json("https://example.com/notes", &signed)
            .await
            .map_err(|e| LanguageError::internal(format!("POST failed: {:?}", e)))?;
        Ok(body)
    }

    async fn expression_get(
        &mut self,
        address: Address,
    ) -> LanguageResult<Option<Expression>> {
        let body = rt::http_get(&format!("https://example.com/notes/{}", address))
            .await
            .map_err(|e| LanguageError::internal(format!("GET failed: {:?}", e)))?;
        Ok(serde_json::from_str(&body).ok())
    }
}

// Wires up wasm-bindgen exports for exactly the capabilities listed.
ad4m_language! {
    language: NoteStore,
    capabilities: [expression],
    holochain_signal: false,
}
```

The `ad4m_language!` macro expands into the wasm-bindgen `extern`s the
AD4M executor looks for — one per capability method. The WASM export
table ends up minimal: the executor introspects it at load time and
caches the capability set, so what you export is what the runtime
thinks your Language can do.

## What this crate gives you

- **Capability traits** (`Language`, `ExpressionCapability`,
  `PerspectiveCommitCapability`, `PerspectiveSyncCapability`,
  `PerspectiveQueryCapability`, `PeersCapability`,
  `TelepresenceCapability`, `LanguageSourceCapability`,
  `MountCapability`, `HolochainSignalHandler`). Implement only the ones
  your Language actually supports.
- **`#[derive(Ad4mModel)]`** — describes a mounted class on a struct.
  See [Mounted classes](#mounted-classes).
- **Typed Rust wrappers** around every `ad4m:host` import
  (`rt::agent_did()`, `rt::holochain_call()`, `rt::storage_get()`,
  `rt::http_post_json()`, `rt::emit_perspective_diff_typed()`, …).
  The typed variants route serialization through a maps-as-objects
  serializer so structured payloads actually make it across the
  wasm-bindgen boundary — don't use raw `serde_wasm_bindgen::to_value`.
- **`ad4m_language!{}` macro** — emits the wasm-bindgen exports for
  the capabilities you list, plus a thread-local instance holder that
  handles re-entrant async calls correctly.
- **`LanguageError` / `LanguageResult`** — error types that map
  cleanly onto the executor-side error taxonomy.

See `src/lib.rs` for the public surface.

## Mounted classes

A Language can mount its records into a perspective. The perspective
then shows them as instances of subject classes, and a write to an
instance runs one of your interactions. The full feature (mounting,
sync, how records become links) is in the
[Languages docs](https://github.com/coasys/ad4m/blob/dev/docs-src/pages/languages.mdx#mounted-languages).
This section is the author's side. The TypeScript LDK has the same
feature with the model decorators; the derive emits the same shape JSON
for the same class.

```rust
use ad4m_ldk::prelude::*;

#[derive(Ad4mModel)]
#[ad4m(
    name = "Thing",
    namespace = "things://",
    target_class = "things://Thing",
    constructor = call("create", source = "{language}://class/Thing"),
    destructor = call("delete"),
)]
struct Thing {
    #[ad4m_property(through = "rdf://type", flag = "things://Thing")]
    r#type: String,
    #[ad4m_property(through = "things://title", setter = call("update"))]
    title: String,
    #[ad4m_property(through = "things://tag", collection, setter = call("update"))]
    tags: Vec<String>,
}

// ThingStore implements `Language` (with `interactions` and
// `expression_interact`) and `ExpressionCapability`.
impl MountCapability for ThingStore {
    fn mount_shapes(&mut self) -> LanguageResult<MountShapes> {
        // Change the version when the classes change.
        Ok(MountShapes::from_classes("1", &[Thing::generate_shacl()]))
    }

    async fn mount_snapshot(&mut self) -> LanguageResult<MountSnapshot> {
        Ok(MountSnapshot {
            entries: vec![MountEntry {
                class: "Thing".into(),
                address: "t1".into(),
                data: serde_json::json!({ "title": "First", "tags": ["a"] }),
            }],
        })
    }
}

ad4m_language! {
    language: ThingStore,
    capabilities: [expression, mount],
    holochain_signal: false,
}
```

The struct's fields only carry the shape. The derive adds no instance
behaviour. `Thing::generate_shacl()` returns `{ name, shape }`, and
`MountShapes::from_classes` builds the `mountShapes()` answer from those.
It replaces `{language}` in a call's `source` or `target` with your
Language's address (the `language_address()` host import). Tests that
run outside the runtime use `MountShapes::from_classes_at(version,
classes, address)`.

### Attributes

- **Class, `#[ad4m(...)]`:** `name`, `namespace` (defaults to the scheme
  of the first property's `through`, else `ad4m://`), `target_class`,
  and repeatable `constructor = call(...)` / `destructor = call(...)`.
- **Property, `#[ad4m_property(...)]`:** `through` (required), `name`,
  `required`, `collection`, `flag = "iri"` (a fixed value, implies
  `required`), `read_only`, `writable`, `local`, `datatype`, `node_kind`,
  `relation_kind`, `alias(Key = "name")`, `setter = call(...)`.
- **Relation, `#[ad4m_relation(...)]`:** `kind = one | many` (required),
  `through`, the property options, and `adder = call(...)` /
  `remover = call(...)`.
- **`call("interaction", source = "...", params = "...")`:** `source`
  defaults to `"this"`, but a constructor must name it. `params` is a
  JSON object as text and defaults to `"{}"`.

### How a call finds your Language

1. The URL of a mounted record is your Language's own expression URL:
   `<language address>://<expression address>`.
2. For setters, adders, removers and destructors, `source` defaults to
   `"this"`, the instance URL. The executor splits it into the language
   address and the expression address, and runs the interaction on your
   Language for that expression. So `interactions(address)` must list the
   interaction for the record's address, and `expression_interact`
   handles it.
3. A constructor has no instance yet, so it names its `source`, normally
   `"{language}://class/<ClassName>"`. `interactions()` must answer for
   the address `class/<ClassName>`.

The executor merges the caller's values over `params` and passes the
merged object as the `parameters` of `expression_interact`.

### What an interaction returns

`expression_interact` returns a delta as `Some(json)`:

```rust
Ok(Some(serde_json::json!({
    "address": "t2",   // required from a create
    "changed": [{ "class": "Thing", "address": "t2", "data": { "title": "New" } }],
    "removed": [],
})))
```

The executor applies it at once. For a create, `address` is the new
record, and `createSubject` returns `<language address>://t2`.

### Pushing changes

When a record changes outside a call, send the same delta as a signal:

```rust
rt::emit_signal_typed(&serde_json::json!({
    "mountDelta": { "changed": [entry], "removed": [] }
}));
```

Signals are best effort. The executor also reads `mount_snapshot()`
again every 15 seconds, so a missed signal is corrected then.

### Rules

- Calls are not rolled back, and a write that calls your Language cannot
  run inside a batch.
- A call runs as the agent who made the write. On a multi-user executor
  that can be a managed user, so read the agent through the host imports
  (`rt::agent_did()`) instead of caching it.
- `mount_snapshot()` runs as the owner of the mounting perspective. A perspective
  with more than one owner can't mount a Language.

## Optional extensions

Beyond the core interface, runtimes may ship optional extensions. Your
Language can use them, but the runtime might not provide them — the
Rust wrappers return `Result` so you handle the missing-extension case
explicitly.

- **Holochain** — `rt::holochain_register_dnas()`, `rt::holochain_call()`,
  plus the `HolochainSignalHandler` trait for receiving DNA signals.
  Declare your Language in the `ad4m-language-holochain` WIT world.
- **Storage File I/O** — `rt::read_storage_file()`,
  `rt::write_storage_file()`. Raw path-based read/write. Prefer the
  core KV (`rt::storage_get()` / `rt::storage_put()`) unless you
  specifically need filesystem-like semantics.

## Reference Languages

The two end-to-end Rust Languages in the AD4M repo are the best
worked examples:

- [`agent-language`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/agent-language/rust-impl)
  — Holochain-backed agent profile store.
- [`centralized-agent-language`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/centralized-agent-language/rust-impl)
  — HTTP-backed agent profile store. The simpler of the two; good
  starting point.

Both ship as real bootstrap Languages inside the AD4M executor and
are exercised by the multi-user integration tests.

## Related

- [`@coasys/ad4m-ldk`](https://www.npmjs.com/package/@coasys/ad4m-ldk)
  — JS/TypeScript ALDK. Same runtime interface, different authoring
  language.
- [AD4M repo](https://github.com/coasys/ad4m) — executor, spec,
  bootstrap Languages.

## License

CAL-1.0. Same as AD4M.
