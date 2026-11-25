# @coasys/ad4m-ldk

**AD4M Language Development Kit — JavaScript / TypeScript.**

[AD4M](https://ad4m.dev) is an agent-centric runtime for decentralized
applications. In AD4M, every storage backend and every communication
protocol is a pluggable **Language** — a small ES module with a
well-defined interface, loaded into a sandboxed isolate by the AD4M
executor. This package is the SDK for authoring AD4M Languages in
JavaScript or TypeScript.

- Full conceptual overview: **[docs.ad4m.dev/languages](https://docs.ad4m.dev/languages)**
- Normative interface spec (WIT):
  [`ad4m-lang.wit`](https://github.com/coasys/ad4m/blob/dev/docs-src/ad4m-lang.wit)
- Prose spec:
  [`language-interface-spec.md`](https://github.com/coasys/ad4m/blob/dev/docs-src/language-interface-spec.md)

## Install

```sh
npm install @coasys/ad4m-ldk
# or
pnpm add @coasys/ad4m-ldk
```

## Minimal example

```ts
import {
    defineLanguage,
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "@coasys/ad4m-ldk";

const lang = defineLanguage({
    name: "note-store",
    version: "1.0.0",

    async init() {
        // Called once per instance. Use runtime imports here to read
        // settings, register DNAs, set up connections, etc.
    },

    expression: {
        async create(content) {
            const expr = agentCreateSignedExpression(content);
            const s = JSON.stringify(expr);
            const address = hash(s);          // canonical AD4M content hash
            storagePut(address, s);
            return address;
        },

        async get(address) {
            const raw = storageGet(address);
            return raw ? JSON.parse(raw) : null;
        },
    },
});

export default lang;
// Also re-export the flat shape so the executor sees the top-level
// named exports it expects:
export const { name, version, init, expressionCreate, expressionGet } = lang;
```

Bundle with esbuild (or any ES-module bundler) and you have a working
Language. The executor loads the bundle, introspects its exports to
decide which capabilities it supports, and starts calling them.

## What this package gives you

- **`defineLanguage({ … })`** — a grouped-spec helper that emits the
  flat named exports the AD4M runtime looks for. Organize your
  Language by capability (`expression`, `commit`, `sync`, `query`,
  `peers`, `telepresence`, …) and the helper wires up the export
  names correctly.
- **Typed wrappers** for every `ad4m:host` import — `agentDid()`,
  `agentSign()`, `agentCreateSignedExpression()`, `holochainCall()`,
  `httpFetch()`, `storageGet()`, `storagePut()`,
  `emitPerspectiveDiff()`, `emitSignal()`, and the rest. Re-exported
  from the package entry so auto-complete and type-check work without
  extra imports.
- **Capability interfaces** you can `implements`-check against when
  structuring your Language's internals.
- **Error types** that map cleanly onto the executor-side
  `LanguageError` variants.

See [`src/index.ts`](./src/index.ts) for the full API surface.

## Choosing a capability set

AD4M Languages are not monoliths. A Language declares which
capabilities it supports by exporting functions for them. The
runtime introspects the module at load time and caches the capability
set — presence is capability.

Common shapes:

| Shape                       | Capabilities to export                                |
|-----------------------------|-------------------------------------------------------|
| Expression store (mutable)  | `expression` (`create` + `get`)                       |
| Expression store (read-only)| `expression` (`addressOf` + `get`)                    |
| DM drop box (sender side)   | `commit`                                              |
| Full link Language          | `commit` + `sync` + `peers`                           |
| Remote query backend        | `query` (no `sync`, no local replica needed)          |
| Live-presence Language      | `telepresence` + `peers`                              |
| Records shown in a perspective | `mount` (see [Mounted classes](#mounted-classes)) |

Implement only what your Language actually does — the runtime routes
around capabilities you don't expose rather than synthesizing them.

## Mounted classes

A Language can mount its records into a perspective. The perspective
then shows them as instances of subject classes, and a write to an
instance runs one of your interactions. The full feature (mounting,
sync, how records become links) is in the
[Languages docs](https://github.com/coasys/ad4m/blob/dev/docs-src/pages/languages.mdx#mounted-languages).
This section is the author's side.

Describe each class with the model decorators from `@coasys/ad4m/model`
(the decorators and the SHACL generation, without the client) and pass
the classes to `defineLanguage`:

```typescript
import { defineLanguage, emitSignal } from "@coasys/ad4m-ldk";
import { Model, Flag, Optional } from "@coasys/ad4m/model";

@Model({
    name: "Thing",
    namespace: "things://",
    targetClass: "things://Thing",
    mount: true,
    constructorActions: { interaction: "create", source: "{language}://class/Thing" },
    destructorActions: { interaction: "delete" },
})
class Thing {
    @Flag({ through: "rdf://type", value: "things://Thing" })
    type!: string;

    @Optional({ through: "things://title", setter: { interaction: "update" } })
    title!: string;
}

export default defineLanguage({
    name: "thing-store",
    version: "1.0.0",
    mount: {
        classes: [Thing],
        version: "1", // change it when the classes change
        snapshot: async () => ({
            entries: [{ class: "Thing", address: "t1", data: { title: "First" } }],
        }),
    },
    // ... expression, interactions, etc.
});
```

`mount.classes` builds `mountShapes()` from each class's
`generateSHACL()`. It replaces `{language}` in a call's `source` or
`target` with your Language's address (the `languageAddress()` host
import). `mount.snapshot` becomes `mountSnapshot()`. If you prefer to
write the SHACL yourself, `mount: { shapes, snapshot }` still works.

`@Model({ mount: true })` changes the generated shape: no datatype
inference, no link actions, `writable` only on properties that have a
writer, and a `targetClass` that can differ from the class name.

### Call actions

A property, relation or lifecycle action of a mounted class runs one of
your interactions:

| Decorator option | Runs when |
|---|---|
| `@Property` / `@Optional` `setter: { interaction }` | a property is set |
| `@HasOne` / `@HasMany` `adder`, `remover` | an item is added or removed |
| `@Model` `constructorActions` | an instance is created |
| `@Model` `destructorActions` | an instance is removed |

A call option is `{ interaction, source?, params? }`. `params` is a JSON
object as text and defaults to `{}`. The executor merges the caller's
values over it and passes the merged object to your interaction as its
only argument. Properties that share a source and an interaction in one
write share one call.

### How a call finds your Language

1. The URL of a mounted record is your Language's own expression URL:
   `<language address>://<expression address>`.
2. For setters, adders, removers and destructors, `source` defaults to
   `"this"`, the instance URL. The executor splits it into the language
   address and the expression address, and runs the interaction on your
   Language for that expression. So `interactions(address)` must list the
   interaction for the record's address.
3. A constructor has no instance yet, so it must name its `source`,
   normally `"{language}://class/<ClassName>"`. `interactions()` must
   answer for the address `class/<ClassName>`.

### What an interaction returns

An interaction that a call runs returns a delta:

```typescript
{
    address: "t2",                                   // required from a create
    changed: [{ class: "Thing", address: "t2", data: { title: "New" } }],
    removed: [],                                     // addresses
}
```

The executor applies it at once. For a create, `address` is the new
record, and the create returns `<language address>://t2`.

### Pushing changes

When a record changes outside a call (another agent, a remote service),
send the same delta as a signal:

```typescript
emitSignal({ mountDelta: { changed: [entry], removed: [] } });
```

Signals are best effort. The executor also reads `mountSnapshot()` again
every 15 seconds, so a missed signal is corrected then.

### Rules

- A class's action list holds either calls or links, never both. The
  decorators make sure of this.
- Calls are not rolled back, and a write that calls your Language cannot
  run inside a batch.
- A call runs as the agent who made the write. On a multi-user executor
  that can be a managed user, so use the host imports (`agentDid()` and
  the rest) instead of a cached identity.
- `mountSnapshot()` runs as the owner of the mounting perspective. A perspective
  with more than one owner can't mount a Language.

## Optional extensions

Beyond the core interface, runtimes may ship optional extensions. Your
Language can import from them, but must tolerate runtimes that don't
provide them (the wrappers throw a clear error at call time).

- **Holochain** — `holochainRegisterDnas`, `holochainCall`,
  `holochainCallAsync`. Requires a runtime with a Holochain conductor.
- **Storage File I/O** — `readStorageFile`, `writeStorageFile`. Raw
  path-based read/write. Prefer the core KV (`storageGet` /
  `storagePut`) unless you specifically need filesystem-like
  semantics (custom layouts, large blobs).

## Reference Languages

The bootstrap Languages in the main repo are the worked examples:

- [`language-language`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/language-language)
  — the root of AD4M addressing, serves other Language bundles.
- [`perspective-diff-sync`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/p-diff-sync)
  — full-replica link Language with Holochain backing.
- [`centralized-p-diff-sync`](https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/centralized-p-diff-sync)
  — same shape, HTTP/socket.io backing instead of Holochain.

## Related

- [`ad4m-ldk`](https://crates.io/crates/ad4m-ldk) — Rust ALDK for
  Languages compiled to WASM. Same runtime interface, different
  authoring language.
- [`@coasys/ad4m`](https://www.npmjs.com/package/@coasys/ad4m) — the
  AD4M client library (use this if you're *consuming* AD4M from an
  app, not authoring a Language).
- [AD4M repo](https://github.com/coasys/ad4m) — executor, spec,
  bootstrap Languages.

## License

CAL-1.0. Same as AD4M.
