# AGENTS.md — core/ (TypeScript SDK)

## Overview

TypeScript SDK (`@coasys/ad4m`). Strict mode, no `any`. Published to npm.

## Build & Test

```bash
pnpm exec tsc && pnpm run bundle   # Build
pnpm test                           # Test (Jest, 348+ tests)
```

Done when: `lib/index.cjs` and `lib/index.js` exist.

**After any changes:** MUST rebuild `connect/` — esbuild re-bundles core inline.

## Key Files

- `Ad4mClient.ts` — Main client, wraps GraphQL
- `PerspectiveProxy.ts` — Perspective operations, link CRUD, subscriptions
- `Ad4mModel.ts` — ORM base class with decorator-driven SHACL/SPARQL
- `hydration.ts` — Model instance hydration from query results
- `model/shacl.ts` — SHACL shape generation from decorators
- `model/query-builder.ts` — SPARQL query generation
- `model/query-cache.ts` — Per-perspective query cache with TTL invalidation

## Model System

Decorator-based: `@Model`, `@Property`, `@HasMany`, `@BelongsTo`, etc.

Decorators → SHACL shapes (registered with executor) → SPARQL queries (generated at runtime).

Subscription pool uses fingerprint deduplication to avoid duplicate watchers.

## Style

- TypeScript strict mode
- No `any` — use `unknown` + type guards
- Decorators use `reflect-metadata`
