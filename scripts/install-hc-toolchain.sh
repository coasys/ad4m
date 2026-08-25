#!/usr/bin/env bash
# Build per-workspace Holochain CLI tools (`hc`, `kitsune2-bootstrap-srv`)
# that MATCH the revisions pinned in this workspace's Cargo.lock.
#
# Rationale: the CI runner host may have a global `hc` on $PATH (e.g.
# installed via `cargo install holochain_cli`) at some other version. When
# the workspace Cargo.lock pins a different `holochain_cli_bundle` revision,
# the two speak incompatible manifest schemas, and `install_app_bundle`
# blows up at runtime with errors like `unknown field 'signal_url'`.
# Isolating the tool per repo also means multiple concurrent CI jobs on
# the same self-hosted runner (each on a different branch that may pin
# different HC versions) don't stomp on each other's `$HOME/.cargo/bin/hc`.
#
# Strategy: we can't use `cargo install --git` because the standalone
# holochain_cli crate graph has conflicting `libsqlite3-sys` versions with
# `links = "sqlite3"` (holochain_data pulls a newer libsqlite3-sys than
# sqlx-sqlite in the same tree, and cargo refuses to link two versions).
#
# Instead we clone the coasys/holochain repo at the exact rev pinned in our
# Cargo.lock into $REPO/.hc-toolchain/src/ and run `cargo build --release`
# from THAT repo's own workspace. Its own Cargo.lock resolves cleanly
# (that's the tree the fork was published against), and the resulting
# binary is byte-compatible with our `holochain_cli_bundle` link because
# it was built from the same source tree. We then copy the release binary
# into $REPO/.hc-toolchain/bin/hc.
#
# The same script also installs `kitsune2-bootstrap-srv` at the version
# our workspace actually links against (see the kitsune2 section below).
#
# Consumers:
#   - tests/js/scripts/prepareTestDirectory.sh (symlinks tst-tmp/hc → this hc)
#   - bootstrap-languages/*/hc-dna/build.sh   (uses this hc to pack DNAs/hApps)
#   - tests/js/utils/utils.ts                 (spawns local bootstrap-srv)
#
# NEVER install into ~/.cargo/bin from this script — that would clobber the
# host `hc` used by other branches / other PRs on the same runner.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CARGO_LOCK="$REPO_ROOT/Cargo.lock"
TOOLCHAIN_DIR="$REPO_ROOT/.hc-toolchain"
BIN_DIR="$TOOLCHAIN_DIR/bin"
STAMP_FILE="$TOOLCHAIN_DIR/.installed_rev"

if [[ ! -f "$CARGO_LOCK" ]]; then
    echo "install-hc-toolchain: Cargo.lock not found at $CARGO_LOCK" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# 1) hc  — pinned to the coasys/holochain rev from Cargo.lock.
# ---------------------------------------------------------------------------
# Extract the git source URL + revision for `holochain_cli_bundle` from
# Cargo.lock. We key off `holochain_cli_bundle` (not `holochain_cli`)
# because that's the crate the ad4m-executor actually links against
# (via `holochain_cli_bundle::pack::<AppManifest>` etc.) — whatever version
# packs our happs at build time must be the same one the executor unpacks
# at install time.
SOURCE_LINE=$(awk '
    /^\[\[package\]\]/ { in_block = 0 }
    /^name = "holochain_cli_bundle"[[:space:]]*$/ { in_block = 1; next }
    in_block && /^source = / { print; exit }
' "$CARGO_LOCK")

if [[ -z "$SOURCE_LINE" ]]; then
    echo "install-hc-toolchain: could not find holochain_cli_bundle source in Cargo.lock" >&2
    exit 1
fi

# Parse: source = "git+<url>?branch=<branch>#<rev>"
SOURCE_VAL="${SOURCE_LINE#source = \"}"
SOURCE_VAL="${SOURCE_VAL%\"}"
GIT_URL="${SOURCE_VAL#git+}"
REV="${GIT_URL##*#}"
GIT_URL="${GIT_URL%#*}"

BRANCH=""
TAG=""
if [[ "$GIT_URL" == *"?"* ]]; then
    QUERY="${GIT_URL#*\?}"
    GIT_URL="${GIT_URL%%\?*}"
    case "$QUERY" in
        branch=*) BRANCH="${QUERY#branch=}" ;;
        tag=*)    TAG="${QUERY#tag=}" ;;
    esac
fi

echo "install-hc-toolchain: hc target = $GIT_URL"
[[ -n "$BRANCH" ]] && echo "install-hc-toolchain: hc branch = $BRANCH"
[[ -n "$TAG"    ]] && echo "install-hc-toolchain: hc tag    = $TAG"
echo "install-hc-toolchain: hc rev    = $REV"
echo "install-hc-toolchain: dest      = $BIN_DIR"

mkdir -p "$BIN_DIR"

# Ensure cargo is on PATH (CI setup_env normally adds ~/.cargo/bin).
if ! command -v cargo >/dev/null 2>&1; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
if ! command -v cargo >/dev/null 2>&1; then
    echo "install-hc-toolchain: cargo not found on PATH" >&2
    exit 1
fi

HC_INSTALL_NEEDED=0
if [[ -f "$STAMP_FILE" && -x "$BIN_DIR/hc" ]]; then
    INSTALLED_REV=$(cat "$STAMP_FILE" 2>/dev/null || echo "")
    if [[ "$INSTALLED_REV" == "$REV" ]]; then
        echo "install-hc-toolchain: hc already at rev $REV — skipping"
        "$BIN_DIR/hc" --version
    else
        echo "install-hc-toolchain: hc rev drift ($INSTALLED_REV → $REV) — reinstalling"
        HC_INSTALL_NEEDED=1
    fi
else
    HC_INSTALL_NEEDED=1
fi

if [[ "$HC_INSTALL_NEEDED" == "1" ]]; then
    # Clone (or reuse) the coasys/holochain source tree at the exact rev.
    #
    # We DO NOT build inside the cloned tree with plain `cargo build` because
    # that tree's Cargo.lock has a libsqlite3-sys `links="sqlite3"` conflict
    # (holochain_data pulls libsqlite3-sys 0.38, sqlx-sqlite 0.9 caps at
    # <0.38, and cargo refuses to link two versions with the same `links`
    # value). Our workspace's [patch.crates-io] for sqlx + lair aligns the
    # chain, but those patches don't propagate into a standalone clone.
    #
    # Instead we OVERLAY the workspace's [patch.crates-io] block onto the
    # clone's root Cargo.toml before building. The clone is isolated per
    # workspace and only used as a build environment for `hc` — no one else
    # consumes it. Overlay is idempotent via a marker comment.
    SRC_DIR="$TOOLCHAIN_DIR/src"
    if [[ ! -d "$SRC_DIR/.git" ]]; then
        echo "install-hc-toolchain: cloning $GIT_URL -> $SRC_DIR"
        rm -rf "$SRC_DIR"
        mkdir -p "$TOOLCHAIN_DIR"
        if [[ -n "$BRANCH" ]]; then
            git clone --depth 50 --branch "$BRANCH" "$GIT_URL" "$SRC_DIR" || git clone "$GIT_URL" "$SRC_DIR"
        elif [[ -n "$TAG" ]]; then
            git clone --depth 1 --branch "$TAG" "$GIT_URL" "$SRC_DIR" || git clone "$GIT_URL" "$SRC_DIR"
        else
            git clone "$GIT_URL" "$SRC_DIR"
        fi
    fi

    cd "$SRC_DIR"
    if ! git cat-file -e "$REV^{commit}" 2>/dev/null; then
        echo "install-hc-toolchain: fetching full history to reach $REV"
        git fetch --unshallow 2>/dev/null || git fetch --depth 500
    fi
    git checkout -- Cargo.toml 2>/dev/null || true
    git checkout "$REV"

    AD4M_PATCH_MARKER="# BEGIN AD4M PATCH OVERLAY"
    AD4M_PATCH_END="# END AD4M PATCH OVERLAY"
    python3 - <<PYEOF
import re, pathlib
toml_path = pathlib.Path("$SRC_DIR/Cargo.toml")
src = toml_path.read_text()

src = re.sub(
    r"\n$AD4M_PATCH_MARKER.*?$AD4M_PATCH_END\n?",
    "",
    src,
    flags=re.DOTALL,
)
src = re.sub(
    r"\n\[patch\.crates-io\][^\[]*(?=\n\[|\Z)",
    "",
    src,
    flags=re.DOTALL,
)

workspace_toml = pathlib.Path("$REPO_ROOT/Cargo.toml").read_text()
m = re.search(
    r"\n\[patch\.crates-io\](.+?)(?=\n\[[a-zA-Z]|\Z)",
    workspace_toml,
    flags=re.DOTALL,
)
if not m:
    raise SystemExit("install-hc-toolchain: no [patch.crates-io] in workspace Cargo.toml")
overlay_body = m.group(1).rstrip() + "\n"

src = src.rstrip() + f"\n\n{'$AD4M_PATCH_MARKER'}\n[patch.crates-io]\n{overlay_body}{'$AD4M_PATCH_END'}\n"
toml_path.write_text(src)
print(f"install-hc-toolchain: patched {toml_path}")
PYEOF

    echo "install-hc-toolchain: cargo build --release --bin hc (in $SRC_DIR)"
    cargo build --release --bin hc

    cp -f "$SRC_DIR/target/release/hc" "$BIN_DIR/hc"
    chmod +x "$BIN_DIR/hc"
    cd "$REPO_ROOT"
    echo "$REV" > "$STAMP_FILE"
fi

# ---------------------------------------------------------------------------
# 2) kitsune2-bootstrap-srv  — matched to the version linked by the workspace.
# ---------------------------------------------------------------------------
# tests/js/utils/utils.ts spawns `kitsune2-bootstrap-srv` from $PATH to run
# a local bootstrap+relay server for integration tests. If that binary is
# at a different kitsune2 version than the `kitsune2_transport_iroh` /
# `kitsune2_core` the executor is linked against, the wire protocol drifts
# and signals fail to route between agents. Symptoms observed on my
# self-hosted runner:
#   - host had kitsune2_bootstrap_srv 0.4.0-dev.3
#   - executor linked crates.io kitsune2 0.5.0
#   - multi-user signal-routing tests timing out / send_signal errors
#
# The historical `[patch.crates-io]` for kitsune2 in Cargo.toml pointed at
# holochain/kitsune2 rev 320a4d9e (0.4.0-dev.5), was declared in the wrong
# Cargo.toml (rust-executor's), and was silently ignored by cargo. Those
# patches have been removed (2026-08-25, plan A per Nico). We now use
# crates.io kitsune2 0.5.0 throughout the workspace.
#
# Resolution: parse Cargo.lock for the ACTUALLY-LINKED (crates.io) version
# of kitsune2_bootstrap_srv and cargo-install that same version into
# .hc-toolchain/bin/.
KITSUNE_VERSION=$(awk '
    /^\[\[package\]\]/          { in_pkg = 1; in_patch = 0; in_kit = 0; ver = "" }
    /^\[\[patch\.unused\]\]/    { in_pkg = 0; in_patch = 1; in_kit = 0; ver = "" }
    in_pkg && /^name = "kitsune2_bootstrap_srv"[[:space:]]*$/ { in_kit = 1; next }
    in_kit && /^version = / { gsub(/^version = "|"$/, ""); ver = $0; next }
    in_kit && /^source = "registry\+/ { print ver; exit }
' "$CARGO_LOCK")

if [[ -z "$KITSUNE_VERSION" ]]; then
    echo "install-hc-toolchain: WARNING no crates.io kitsune2_bootstrap_srv in Cargo.lock; skipping" >&2
else
    echo "install-hc-toolchain: kitsune2 version = $KITSUNE_VERSION (crates.io)"
    K_STAMP_FILE="$TOOLCHAIN_DIR/.installed_kitsune_version"
    K_INSTALL_NEEDED=0
    if [[ -f "$K_STAMP_FILE" && -x "$BIN_DIR/kitsune2-bootstrap-srv" ]]; then
        K_INSTALLED=$(cat "$K_STAMP_FILE" 2>/dev/null || echo "")
        if [[ "$K_INSTALLED" == "$KITSUNE_VERSION" ]]; then
            echo "install-hc-toolchain: kitsune2-bootstrap-srv already at $KITSUNE_VERSION — skipping"
            "$BIN_DIR/kitsune2-bootstrap-srv" --version
        else
            echo "install-hc-toolchain: kitsune2 version drift ($K_INSTALLED → $KITSUNE_VERSION) — reinstalling"
            K_INSTALL_NEEDED=1
        fi
    else
        K_INSTALL_NEEDED=1
    fi

    if [[ "$K_INSTALL_NEEDED" == "1" ]]; then
        echo "install-hc-toolchain: cargo install kitsune2_bootstrap_srv --version $KITSUNE_VERSION --root $TOOLCHAIN_DIR"
        cargo install \
            --version "$KITSUNE_VERSION" \
            --root "$TOOLCHAIN_DIR" \
            kitsune2_bootstrap_srv
        echo "$KITSUNE_VERSION" > "$K_STAMP_FILE"
    fi
fi

# ---------------------------------------------------------------------------
# Final report
# ---------------------------------------------------------------------------
echo ""
echo "install-hc-toolchain: installed:"
"$BIN_DIR/hc" --version
if [[ -x "$BIN_DIR/kitsune2-bootstrap-srv" ]]; then
    "$BIN_DIR/kitsune2-bootstrap-srv" --version
fi
