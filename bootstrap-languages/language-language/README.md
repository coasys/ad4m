# Language: languages (the "language-language")

The root of AD4M addressing. This Language publishes and serves the
bundles of other Languages: publishing a new Language mints a new hash
that becomes that Language's address, and any AD4M node given that
hash can fetch and install the bundle via this Language.

Every AD4M node runs an instance of the language-language at boot.

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
and exports the Expression + language-source capabilities. Storage is
backed by a centralized Cloudflare Workers KV proxy in the reference
implementation; a Holochain-backed variant is possible by swapping the
transport. Runtime services come in via named imports from
`ad4m:host` — see
[docs.ad4m.dev/languages](https://docs.ad4m.dev/languages) for the
full picture.
