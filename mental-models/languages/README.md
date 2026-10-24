# Languages

A **language** is a JavaScript bundle that the executor runs in its own **language isolate**. It does some of these for the executor, one per [language capability](runtime/interface.md) it exports: create and fetch **expressions**; commit, sync, render and query **links**; list peers; carry telepresence. Its **language address** is the content hash of the bundle.

**Why:** The executor does run network services of its own: the Holochain conductor ([Holochain service](../agents/executor/services/holochain.md)) and the WebRTC [SFU](../agents/executor/services/sfu.md) with its TCP gossip. But it has no built-in transport for expressions and links. Each way to store or sync them (Holochain DNAs, a separate [link server](link-languages/link-server.md) process, an HTTP store, a local file) sits behind one small language interface. A perspective or an agent profile changes transport when it changes language. The option that lost was a fixed transport inside the executor.

**Parent:** [ADAM](../README.md)

## Parts
- [Expressions and addresses](expressions/README.md): the signed data unit, its URL, and how the executor reads one. Start here.
- [Language runtime](runtime/README.md): how the executor loads, calls, installs and boots languages.
- [Link languages](link-languages/README.md): the languages that sync a neighbourhood's shared links, over Holochain, a link server or no network.
- [Other language kinds](other-kinds/README.md): the agent, neighbourhood, perspective and file storage languages.

<details><summary>How it works</summary>

```
SDK / MCP / perspective ──► LanguageController ──► one language isolate per language address
                                  │                        │  imports "ad4m:host"
                                  │                        ▼
                                  │                 Rust ops (sign, holochain, http, storage, emit)
                                  ▼
                     language language (fetches other bundles by address)
```

1. When the main agent is generated or unlocked, the executor loads the **language language** from the **bootstrap seed**, then the agent, neighbourhood and perspective languages, then the known link languages ([Bootstrap languages](runtime/bootstrap-languages.md)).
2. A caller names a language by its address. The [language controller](runtime/controller.md) sends a JavaScript snippet to that language's isolate and waits for the result.
3. A link language pushes remote changes back with `emitPerspectiveDiff` (or a callback setter). The executor routes the diff to the perspective that uses this link language ([Link languages](link-languages/README.md)).
4. Every agent on the executor, main and managed, shares these isolates: the executor runs one isolate per language address, created with the main agent's DID. A call carries an `AgentContext` that names the agent it runs as ([Language isolates](runtime/isolates.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/](../../rust-executor/src/languages): controller, runtimes, capabilities.
- [rust-executor/src/js_core/](../../rust-executor/src/js_core): Deno isolates and the `ad4m:host` module.
- [bootstrap-languages/](../../bootstrap-languages): the languages that ship with ADAM.
- [ad4m-ldk/](../../ad4m-ldk): the ALDK, the kit to write languages in TypeScript or Rust.
- [docs-src/ad4m-lang.wit](../../docs-src/ad4m-lang.wit), [docs-src/language-interface-spec.md](../../docs-src/language-interface-spec.md): the language interface, version 1.0.
- [link-server/](../../link-server): the server for the server link language.

</details>
