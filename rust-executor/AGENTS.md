# AGENTS.md — rust-executor/

## Overview

Rust 2021 edition. The executor binary — hosts Holochain, Deno JS runtime, Oxigraph SPARQL store, GraphQL API (Juniper), REST API (Actix-web).

## Build & Check

```bash
cargo check -p ad4m-executor       # Type check
cargo fmt                           # Format
cargo clippy -p ad4m-executor       # Lint
pnpm build-libs                     # Full build (from repo root)
```

## SPARQL Store (Oxigraph)

- Link storage in `src/perspectives/sparql_store.rs`
- Named graphs: each link stored as 7 RDF triples in its own graph
- Graph IRI: `link:<sha256(source+predicate+target+timestamp)[:32]>`

## Key Directories

- `src/graphql/` — Juniper GraphQL resolvers
- `src/rest/` — Actix-web REST API (`/api/v1/`)
- `src/perspectives/` — Perspective management + SPARQL store
- `holochain_service/` — Holochain integration
- `src/js_core/` — Deno runtime for language execution

## Feature Flags

- `--features sfu` — WebRTC SFU support (optional)

## Build Script

`build.rs` downloads alliance DNA from GitHub releases.

## Gotcha

`.cargo/config.toml` sets macOS linker path for CI (Xcode 15.4). If `cargo build` fails with linker errors on macOS, check this file.
