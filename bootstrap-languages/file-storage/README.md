# Language: file-storage

Holochain-DHT-backed file storage Language for AD4M. Files are chunked,
compressed with pako, and stored in a Holochain DNA; retrieval
reassembles and decompresses them.

## Build

`./build.sh` produces `build/bundle.js`:

1. The Svelte *Icons* (`ConstructorIcon.svelte` / `Icon.svelte`) compile
   to `build/ConstructorIcon.js` / `build/Icon.js` as web components.
2. `esbuild.ts` bundles `index.ts` into `build/bundle.js`, inlining the
   two compiled icons and marking `ad4m:host` as external (the AD4M
   executor provides it at load time).

## Interface

`index.ts` uses `defineLanguage({ … })` from
[`@coasys/ad4m-ldk`](https://www.npmjs.com/package/@coasys/ad4m-ldk)
and exports the [Expression](https://docs.ad4m.dev/expressions)
capability. Runtime services (Holochain, signing) come in via named
imports from `ad4m:host` — see
[docs.ad4m.dev/languages](https://docs.ad4m.dev/languages) for the
full picture.
