# Developer Scripts

Utility scripts for building, testing, and debugging AD4M + Flux.

## `ad4m-flux-rebuild.sh`

Rebuild the full AD4M SDK → Flux dependency chain, ensuring changes propagate correctly.

**Why this exists:** `@coasys/ad4m-connect` bundles `@coasys/ad4m` inline via esbuild (`bundle: true`, no externals). This means `pnpm link` and `file:` overrides do NOT propagate core changes into connect's bundle. You must rebuild connect after every core change. This script handles the full chain.

**Build order:** `core` (tsc + rollup) → `connect` (esbuild, re-bundles core) → `hooks` → Flux (symlink + overrides + cache clear + build)

```bash
# Rebuild SDK only (from ad4m repo root)
scripts/ad4m-flux-rebuild.sh

# Rebuild SDK + Flux
scripts/ad4m-flux-rebuild.sh --flux ../flux

# Full rebuild with executor + serve Flux
scripts/ad4m-flux-rebuild.sh --flux ../flux --executor --serve 3030
```

## `ad4m-connect-auth.sh`

Automate the ad4m-connect authentication flow via Chrome DevTools Protocol (CDP).

Useful for E2E testing, CI pipelines, and AI agent automation where manual security code entry isn't possible.

**What it does:**
1. Launches Chrome (or attaches to existing)
2. Opens Flux URL
3. Pierces ad4m-connect's shadow DOM to click "Connect"
4. Watches executor stdout log for the 6-digit security code
5. Enters the code and clicks Authorize
6. Verifies JWT token is stored in localStorage

**Prerequisites:** Chrome/Chromium, python3, either `websocat` or python3 `websockets` package.

```bash
# Full auto — launch Chrome, complete auth
scripts/ad4m-connect-auth.sh --executor-log /tmp/executor-stdout.log

# Attach to existing Chrome
scripts/ad4m-connect-auth.sh --no-launch --cdp-port 9222

# Headless CI mode
scripts/ad4m-connect-auth.sh --headless --executor-log /tmp/executor-stdout.log
```

## `ci-logs.sh`

Fetch CircleCI failure logs for any GitHub PR without leaving the terminal.

Chains: `gh pr checks` → CircleCI API v2 (workflow → jobs) → CircleCI API v1.1 (presigned output URLs) → parses JSON log entries.

```bash
# Show failing jobs + error summary
scripts/ci-logs.sh coasys/ad4m 760

# Show all job statuses
scripts/ci-logs.sh coasys/ad4m 760 --all

# Full log for a specific job
scripts/ci-logs.sh coasys/ad4m 760 --job integration-tests-model --tail 100
```
