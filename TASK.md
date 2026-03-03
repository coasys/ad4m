# Task: Address PR #693 Review Comments

## Context
PR #693 ports the LanguageController from JS to Rust. Nico left review comments and CodeRabbit left 24 comments.

## Priority 1: Move bundle loading from JS to Rust

**Nico's core request:** Don't pass file paths to JS for loading. The JS sandbox should NOT be able to do file operations. Instead:
1. Read the language bundle file in Rust (in `language_runtime.rs` or `mod.rs`)
2. Pass the bundle SOURCE CODE into the JS runtime
3. Have JS evaluate the code directly instead of importing from a file path

### Files to change:
- `rust-executor/src/languages/language_runtime.rs` line 56: Currently passes path to JS. Instead, read file in Rust and pass code.
- `rust-executor/src/js_core/language_bootstrap.js`: The `loadLanguageBundle(path)` function currently does `import(url)`. Change it to accept source code and evaluate it (e.g., using `new Function` or `eval` or a blob URL approach). NOTE: Since these are ES modules with `export default`, we may need to use a data: URL or blob URL approach to handle module syntax.
- `rust-executor/src/js_core/languages_extension.rs` line 61: Nico asks if this is still needed (was for in-between phases)

### Approach for module loading without file access:
Since language bundles use ES module syntax (`export default`), we can't just `eval()` them. Options:
1. **data: URL** — Convert source to base64, use `import("data:text/javascript;base64,...")`
2. **Blob URL** — Create blob from source code and import it
3. **Pass source via Deno op** — Register an op that returns the source, use a custom module loader

The data: URL approach is simplest and works in Deno. In Rust, base64-encode the bundle source and pass it to JS.

## Priority 2: Nico's other comments

1. **`rust-executor/src/graphql/mutation_resolvers.rs` line 174**: DM language is a template, should be cloned for agent. "This should break tests. Need to check and likely reactivate those tests and actually do the cloning here." — **SKIP THIS** (Nico said don't fix missing DM language code)

2. **`rust-executor/src/graphql/mutation_resolvers.rs` line 384**: Duplicate HC config block. Should have `LocalConductorConfig::from(Ad4mConfig)` — Extract to a helper method.

3. **`rust-executor/src/languages/literal.rs` line 37**: "We have a Literal implementation in rust_client. This is redundant." — Replace with rust_client's Literal impl.

## Priority 3: CodeRabbit comments (24 total)

### Critical (🔴):
1. **`mod.rs` line ~705**: Hash verification hardcoded to `"asdf"` — bypasses integrity check
2. **`mod.rs` line ~1950**: Escape dynamic JS arguments before embedding in script strings (injection risk)
3. **`lib.rs` line ~133**: Raw string interpolation from signal data into JS (injection risk)
4. **`mod.rs` line ~469**: Verify fetched bundle hash matches requested address before loading
5. **`utils.rs` line ~17**: set_languages_directory issue

### Major (🟠):
6. **`agent/mod.rs`**: avoid panic in `ensure_agent_expression` — use `?` instead of `did()` which can panic
7. **`agent/mod.rs`**: don't report success when create fails
8. **`mutation_resolvers.rs` line ~222**: Don't silently ignore init failures in `agent_generate`/`agent_unlock`
9. **`mutation_resolvers.rs` line ~2462**: `runtime_set_status` returns success without performing mutation
10. **`query_resolvers.rs` line ~756**: Avoid returning success for unimplemented DM query paths
11. **`language_runtime.rs` line ~57**: Escape path before embedding into JS — WILL BE FIXED BY PRIORITY 1
12. **`mod.rs` line ~252**: Issue in mod.rs
13. **`mod.rs` line ~777**: `install_language` reports success when installation fails
14. **`mod.rs`**: `get_neighbourhood` queries wrong system language
15. **`mod.rs`**: Guard happ replacement when no `var happ` exists
16. **`perspective_instance.rs` line ~3612**: Fallback returns JSON-quoted strings
17. **`mutation_resolvers.rs` line ~2365**: Return explicit error for unported DM send path
18. **`query_resolvers.rs` line ~151**: Don't downgrade expression fetch failures to None
19. **`holochain_service_extension.rs` line ~109**: Avoid implicit array→Binary coercion
20. **`mod.rs` line ~492**: Don't signal "languages ready" when core language loads fail
21. **`perspectives/mod.rs` line ~387**: Invalidate link-language cache
22. **`lib.rs` line ~210**: Fail fast if required data directories can't be created

### Minor (🟡):
23. **`perspectives/mod.rs` line ~519**: Empty owners drops telepresence signals
24. **`NeighbourhoodClient.ts` line ~259**: Minor TS issue

## DO NOT FIX
- DM language template cloning (Nico explicitly said skip this)
- Anything that would change the fundamental architecture of the PR

## Build & Test
```bash
export PATH="$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"
# Check compilation
cargo check -p ad4m-executor 2>&1 | tail -30
# Build
cargo build -p ad4m-executor --release 2>&1 | tail -10
# Format
cargo fmt --all
```
