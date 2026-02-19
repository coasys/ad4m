/**
 * Post-install script for @coasys/ad4m-test
 *
 * Previously, this script downloaded pre-built system language bundles from
 * perspect3vism repos. These bundles were CJS and needed conversion to ESM
 * for the executor's Deno runtime.
 *
 * Now, the test runner uses the bootstrap seed (tests/js/bootstrapSeed.json)
 * which contains the language-language bundle inline. The language-language
 * fetches other system languages by hash from the bootstrap store (Cloudflare)
 * at runtime. No local language bundles are needed.
 */

// No-op — system languages are fetched at runtime via the bootstrap seed.
console.log('@coasys/ad4m-test: System languages will be fetched at runtime via bootstrap seed.');
