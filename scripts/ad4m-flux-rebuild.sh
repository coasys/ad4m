#!/usr/bin/env bash
# ad4m-flux-rebuild.sh — Rebuild AD4M SDK + Flux together
#
# Ensures SDK changes propagate correctly through the dependency chain:
#   ad4m/core → ad4m/connect → flux (via pnpm overrides)
#
# Why this exists:
#   @coasys/ad4m-connect's esbuild config bundles @coasys/ad4m inline
#   (bundle: true, no external). This means pnpm overrides and symlinks
#   do NOT propagate core changes into connect's bundle. You must rebuild
#   connect after every core change, then clear Flux caches.
#
# Usage:
#   ad4m-flux-rebuild.sh --ad4m <DIR> [--flux DIR] [--executor] [--serve PORT]
#
# Options:
#   --ad4m DIR       AD4M repo root (required unless script is inside the repo)
#   --flux DIR       Flux repo root
#   --executor       Also build the Rust executor
#   --serve PORT     After building, serve Flux on PORT
#   --skip-install   Skip pnpm install (use when lockfile hasn't changed)
#   --help           Show this help
#
# Examples:
#   ad4m-flux-rebuild.sh --ad4m /tmp/ad4m-rest --flux ~/workspaces/coasys/flux
#   ad4m-flux-rebuild.sh --ad4m /tmp/ad4m-rest --flux ~/workspaces/coasys/flux --executor --serve 3030
#   ad4m-flux-rebuild.sh --ad4m ~/workspaces/coasys/ad4m  # SDK only, no Flux

set -euo pipefail

# --- Helpers (defined first so option parser can use err()) ---
log() { echo -e "\033[1;36m→ $1\033[0m"; }
warn() { echo -e "\033[1;33m⚠ $1\033[0m" >&2; }
err() { echo -e "\033[1;31m✗ $1\033[0m" >&2; exit 1; }
ok()  { echo -e "\033[1;32m✓ $1\033[0m"; }

# --- Defaults ---
# Try to auto-detect AD4M dir from script location (works when script is inside the repo)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AD4M_DIR=""
if [[ -d "$SCRIPT_DIR/../core" ]]; then
    AD4M_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
fi

FLUX_DIR=""
BUILD_EXECUTOR=false
SERVE_PORT=""
SKIP_INSTALL=false

# --- Parse options ---
while [[ $# -gt 0 ]]; do
    case $1 in
        --ad4m) [[ $# -ge 2 ]] || err "--ad4m requires a directory argument"; AD4M_DIR="$2"; shift 2;;
        --flux) [[ $# -ge 2 ]] || err "--flux requires a directory argument"; FLUX_DIR="$2"; shift 2;;
        --executor) BUILD_EXECUTOR=true; shift;;
        --serve) [[ $# -ge 2 ]] || err "--serve requires a port argument"; SERVE_PORT="$2"; shift 2;;
        --skip-install) SKIP_INSTALL=true; shift;;
        -h|--help)
            sed -n '2,/^$/p' "$0" | sed 's/^# //' | sed 's/^#//'
            exit 0;;
        *) err "Unknown option: $1";;
    esac
done

# --- Validate ---
[[ -n "$AD4M_DIR" ]] || err "AD4M repo not specified. Use --ad4m <DIR> or run from inside the repo."
AD4M_DIR="$(cd "$AD4M_DIR" && pwd)"  # resolve to absolute path
[[ -d "$AD4M_DIR/core" ]] || err "AD4M core not found at $AD4M_DIR/core"

# Step 1: Build AD4M core
log "Building AD4M core ($AD4M_DIR/core)..."
cd "$AD4M_DIR/core"
pnpm exec tsc
# buildSchema may not exist on all branches; skip if missing
pnpm run buildSchema >/dev/null 2>&1 || true
pnpm run bundle
ok "AD4M core built"

# Step 2: Build AD4M connect (re-bundles core via esbuild)
log "Building AD4M connect ($AD4M_DIR/connect)..."
cd "$AD4M_DIR/connect"
pnpm run build
ok "AD4M connect built (core re-bundled)"

# Step 3: Build hooks if they exist
for hook_dir in "$AD4M_DIR/ad4m-hooks/helpers" "$AD4M_DIR/ad4m-hooks/react" "$AD4M_DIR/ad4m-hooks/vue"; do
    if [[ -f "$hook_dir/tsconfig.json" ]]; then
        log "Building $(basename "$(dirname "$hook_dir")")/$(basename "$hook_dir")..."
        cd "$hook_dir" && pnpm exec tsc || warn "Hook build failed: $hook_dir (non-fatal)"
    fi
done

# Step 4: Build executor if requested
if $BUILD_EXECUTOR; then
    log "Building AD4M executor..."
    cd "$AD4M_DIR"
    pnpm build-libs
    ok "Executor built: $AD4M_DIR/target/release/ad4m-executor"
fi

# If no Flux dir, done
if [[ -z "$FLUX_DIR" ]]; then
    ok "AD4M SDK build complete"
    echo ""
    echo "To also rebuild Flux, pass --flux <path-to-flux-repo>"
    exit 0
fi

# --- Flux build ---
FLUX_DIR="$(cd "$FLUX_DIR" && pwd)"
[[ -d "$FLUX_DIR/app" ]] || err "Flux app not found at $FLUX_DIR/app"

# Step 5: Link Flux → AD4M
log "Linking Flux → AD4M SDK..."
cd "$FLUX_DIR"

if [[ -e ad4m && ! -L ad4m ]]; then
    err "'$FLUX_DIR/ad4m' exists and is not a symlink; remove or rename it first."
fi
rm -f ad4m
ln -sfn "$AD4M_DIR" ad4m

node -e "
const pkg = require('./package.json');
pkg.pnpm = pkg.pnpm || {};
pkg.pnpm.overrides = pkg.pnpm.overrides || {};
pkg.pnpm.overrides['@coasys/ad4m'] = 'file:./ad4m/core';
pkg.pnpm.overrides['@coasys/ad4m-connect'] = 'file:./ad4m/connect';
require('fs').writeFileSync('./package.json', JSON.stringify(pkg, null, 2) + '\n');
"
ok "Overrides set"

# Step 6: Install
if ! $SKIP_INSTALL; then
    log "Installing Flux dependencies..."
    pnpm install --no-frozen-lockfile
fi

# Step 7: Clear caches
log "Clearing build caches..."
rm -rf .turbo app/dist node_modules/.cache
find . -name '.turbo' -type d -not -path './ad4m/*' -not -path './node_modules/*' -exec rm -rf {} + 2>/dev/null || true
find . -name '.vite' -type d -path '*/node_modules/.vite' -exec rm -rf {} + 2>/dev/null || true

# Step 8: Build
log "Building Flux..."
NODE_OPTIONS='--max-old-space-size=4096' pnpm build
ok "Flux built"

# Step 9: Serve if requested (NO -s flag — it makes all paths return 200, breaking /health detection)
if [[ -n "$SERVE_PORT" ]]; then
    log "Serving Flux on http://localhost:$SERVE_PORT"
    cd app/dist
    exec npx serve -p "$SERVE_PORT"
fi

ok "Done!"
echo ""
echo "  AD4M SDK:  $AD4M_DIR"
echo "  Flux:      $FLUX_DIR"
echo "  Serve:     cd $FLUX_DIR/app/dist && npx serve -p 3030"
if $BUILD_EXECUTOR; then
    echo "  Executor:  $AD4M_DIR/target/release/ad4m-executor"
fi
