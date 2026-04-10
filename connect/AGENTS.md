# AGENTS.md — connect/

## Overview

Web component (`<ad4m-connect>`) for auth UI and executor detection. Published as `@coasys/ad4m-connect`.

## Build

```bash
pnpm run build
# Done when: dist/ updated
```

## CRITICAL: esbuild Bundling

`scripts/esbuild_index.js` uses `bundle: true` with **NO externals**. `@coasys/ad4m` is inlined into the bundle.

This means:
- pnpm overrides and `file:` links do NOT affect the bundled copy
- **MUST run `pnpm run build` after any `core/` changes**
- Forgetting this is the #1 source of "my SDK changes aren't working" bugs

## Auth Flow

1. Detect executor (localhost scan)
2. `requestCapability()` — app requests permissions
3. Security code displayed — user confirms in executor UI
4. `permitCapability()` — executor grants JWT
5. Client uses JWT for all subsequent API calls

## Key Details

- All UI renders in **shadow DOM** — `document.querySelector` won't find elements
- DevTools bridge init happens in `core.ts` `buildClient()`
- Built with Lit web components
