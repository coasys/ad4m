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
- **`mount.classes`** — describe the records your Language shows in a
  perspective with the SDK's model decorators. See
  [Mounted classes](#mounted-classes).

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

Implement only what your Language actually does — the runtime routes
around capabilities you don't expose rather than synthesizing them.

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

## Mounted classes

A Language can mount records into a perspective: the Language owns the
records, and the perspective shows them as instances of a class. You
describe such a class with the model decorators from
`@coasys/ad4m/model` (the decorators and SHACL generation, without the
client) and pass the classes to `defineLanguage`. `mountShapes()` is then
built from them.

```typescript
import { defineLanguage } from "@coasys/ad4m-ldk";
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
        version: "1",
        snapshot: () => ({ entries: [] }), // the records to show
    },
    // ... expression, interactions, etc.
});
```

The setter names only the interaction, `"update"`. It names no Language
and no expression. The executor finds both from the instance.

### How a call finds its language

1. The URL of a mounted record (its base expression) is your Language's
   own expression URL: `<language address>://<expression address>`.
2. A call action is `{ action: "call", source, predicate, target }`.
   `predicate` is the interaction name and `target` is a JSON object as
   text. For setters, adders, removers and destructors, `source` defaults
   to `"this"`, which is the instance URL. The executor splits that URL
   into the language address and the expression address
   (`LanguageController::parse_expr_url`) and runs `expression_interact`
   on that Language, for that expression. So your `interactions(address)`
   export must list the interaction for the record's address.
3. A constructor has no instance yet, so it must name its `source`.
   Normally that is `"{language}://class/<ClassName>"`. When
   `mount.classes` builds `mountShapes()`, it replaces `{language}` with
   your own Language's address (the `languageAddress()` host import).
   Outside a mount, `{language}` stays literal. Your `interactions()` must
   answer for the address `class/<ClassName>`. The call returns a delta
   whose `address` is the new record. `createSubject` returns the URL
   `<language address>://<that address>`.

More rules:

- The caller's values are merged over the action's JSON `target` object,
  and the merged object is the single argument of your interaction. Calls
  do not go through link-parameter substitution. When several properties
  of one write share a source and an interaction, they share one call.
  For a constructor, all initial values are the parameters.
- An action list must not mix call and link actions. The executor
  refuses it.
- Calls run before link actions and are not rolled back. A call cannot run
  inside a batch.
- `Ad4mModel.save()` and the relation helpers refuse a property or
  relation with a call writer. Write through the executor's shape actions:
  `createSubject`, `updateSubject` and `removeSubject`, or the MCP
  `instance_*` tools.
- Runtime dependency: the executor runs `action: "call"` only with the
  call-action branch `acorn/a2-call-action`, which is not merged yet.

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
