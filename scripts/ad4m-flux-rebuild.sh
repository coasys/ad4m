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
#   scripts/ad4m-flux-rebuild.sh [--flux DIR] [--executor] [--serve PORT]
#
# Options:
#   --flux DIR       Flux repo root (required if not in default location)
#   --executor       Also build the Rust executor (cargo build via pnpm build-libs)
#   --serve PORT     After building, serve Flux on PORT
#   --skip-install   Skip pnpm install (use when lockfile hasn't changed)
#   --help           Show this help
#
# Examples:
#   # From inside the ad4m repo:
#   scripts/ad4m-flux-rebuild.sh --flux ../flux
#
#   # Build everything including executor, then serve:
#   scripts/ad4m-flux-rebuild.sh --flux ~/workspaces/coasys/flux --executor --serve 3030
#
#   # Quick rebuild (no install, no executor):
#   scripts/ad4m-flux-rebuild.sh --flux ../flux --skip-install

set -euo pipefail

# Resolve AD4M repo root (directory containing this script's parent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AD4M_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

FLUX_DIR=""
BUILD_EXECUTOR=false
SERVE_PORT=""
SKIP_INSTALL=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --flux) FLUX_DIR="$2"; shift 2;;
        --executor) BUILD_EXECUTOR=true; shift;;
        --serve) SERVE_PORT="$2"; shift 2;;
        --skip-install) SKIP_INSTALL=true; shift;;
        -h|--help)
            sed -n '/^# /,/^$/p' "$0" | sed 's/^# //' | sed 's/^#//'
            exit 0;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done

log() { echo -e "\033[1;36m→ $1\033[0m"; }
err() { echo -e "\033[1;31m✗ $1\033[0m" >&2; exit 1; }
ok()  { echo -e "\033[1;32m✓ $1\033[0m"; }

# Validate AD4M repo
[[ -d "$AD4M_DIR/core" ]] || err "AD4M core not found at $AD4M_DIR/core. Run this script from the ad4m repo root."

# Step 1: Build AD4M core (TypeScript compile + rollup bundle)
log "Building AD4M core ($AD4M_DIR/core)..."
cd "$AD4M_DIR/core"
pnpm exec tsc
pnpm run bundle
ok "AD4M core built"

# Step 2: Build AD4M connect (re-bundles core via esbuild — this is the critical step)
log "Building AD4M connect ($AD4M_DIR/connect)..."
cd "$AD4M_DIR/connect"
pnpm run build
ok "AD4M connect built (core re-bundled)"

# Step 3: Build hooks packages if they exist
for hook_dir in "$AD4M_DIR/ad4m-hooks/helpers" "$AD4M_DIR/ad4m-hooks/react" "$AD4M_DIR/ad4m-hooks/vue"; do
    if [[ -f "$hook_dir/tsconfig.json" ]]; then
        log "Building $(basename "$(dirname "$hook_dir")")/$(basename "$hook_dir")..."
        cd "$hook_dir" && pnpm exec tsc 2>/dev/null || true
    fi
done

# Step 4: Build Rust executor if requested
if $BUILD_EXECUTOR; then
    log "Building AD4M executor (this takes a few minutes)..."
    cd "$AD4M_DIR"
    pnpm build-libs
    ok "Executor built: $AD4M_DIR/target/release/ad4m-executor"
fi

# If no Flux dir specified, we're done with AD4M-only build
if [[ -z "$FLUX_DIR" ]]; then
    ok "AD4M SDK build complete"
    echo ""
    echo "To also rebuild Flux, pass --flux <path-to-flux-repo>"
    exit 0
fi

# Validate Flux repo
[[ -d "$FLUX_DIR/app" ]] || err "Flux app not found at $FLUX_DIR/app"

# Step 5: Link Flux to this AD4M repo
log "Linking Flux → AD4M SDK..."
cd "$FLUX_DIR"

# Create/update symlink
rm -f ad4m 2>/dev/null
ln -sf "$AD4M_DIR" ad4m

# Set pnpm overrides to use local AD4M builds
node -e "
const pkg = require('./package.json');
pkg.pnpm = pkg.pnpm || {};
pkg.pnpm.overrides = pkg.pnpm.overrides || {};
pkg.pnpm.overrides['@coasys/ad4m'] = 'file:./ad4m/core';
pkg.pnpm.overrides['@coasys/ad4m-connect'] = 'file:./ad4m/connect';
require('fs').writeFileSync('./package.json', JSON.stringify(pkg, null, 2) + '\n');
"
ok "Overrides set in package.json"

# Step 6: Install dependencies
if ! $SKIP_INSTALL; then
    log "Installing Flux dependencies..."
    pnpm install --no-frozen-lockfile
fi

# Step 7: Clear ALL caches (critical when swapping SDK versions)
log "Clearing build caches..."
rm -rf .turbo app/dist node_modules/.cache
find . -name '.turbo' -type d -not -path './ad4m/*' -not -path './node_modules/*' -exec rm -rf {} + 2>/dev/null || true
find . -name '.vite' -type d -path '*/node_modules/.vite' -exec rm -rf {} + 2>/dev/null || true

# Step 8: Build Flux
log "Building Flux..."
NODE_OPTIONS='--max-old-space-size=4096' pnpm build
ok "Flux built successfully"

# Step 9: Serve if requested
if [[ -n "$SERVE_PORT" ]]; then
    log "Serving Flux on http://localhost:$SERVE_PORT"
    cd app/dist
    exec npx serve -p "$SERVE_PORT" -s
fi

ok "Done!"
echo ""
echo "  AD4M SDK:  $AD4M_DIR"
echo "  Flux:      $FLUX_DIR"
echo "  Serve:     cd $FLUX_DIR/app/dist && npx serve -p 3030 -s"
if $BUILD_EXECUTOR; then
    echo "  Executor:  $AD4M_DIR/target/release/ad4m-executor"
fi
