# Language: languages

This is a Language about Languages - a way to publish and make available Languages to others.

## Build overview

The whole project compiles and bundles to one file: `build/bundle.js`.

This happens in two steps:
1. The *Icons* (AD4M-speak for UI components) [ConstructorIcon.svelte](ConstructorIcon.svelte) and [Icon.svelte](Icon.svelte) get compiled to `build/ConstructorIcon.js` and `build/Icon.js` respectively as web components. This first step is configured with [rollup.config.icons.js](rollup.config.icons.js)
2. [index.ts](index.ts) gets compiled and bundled to `build/bundle.js`. During this bundling step, `rollup-plugin-string` is used to include the two web component files that represent the Icons as string literals inside the resulting bundle.

## Injected context

The `create()` function returned by the resulting `bundle.js` will receive a parameter when executed inside Perspectivism: an instance of [LanguageContext](../../ad4m/LanguageContext.ts). This includes the Agent object and interfaces to storage implementations - currently only IPFS but very soon Holochain and others.

Here, the IPFS object gets passed on to the constructor of the [ExpressionAdapter](adapter.ts) so it can be used to store expression in IPFS files.
