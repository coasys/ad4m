#!/usr/bin/env bash
# Build a per-workspace Holochain `hc` binary that MATCHES the revision
# pinned in this workspace's Cargo.lock.
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
# Consumers:
#   - tests/js/scripts/prepareTestDirectory.sh (symlinks tst-tmp/hc → this hc)
#   - bootstrap-languages/*/hc-dna/build.sh   (uses this hc to pack DNAs/hApps)
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

# Extract the git source URL + revision for `holochain_cli_bundle` from
# Cargo.lock. We key off `holochain_cli_bundle` (not `holochain_cli`)
# because that's the crate the ad4m-executor actually links against
# (via `holochain_cli_bundle::pack::<AppManifest>` etc.) — whatever version
# packs our happs at build time must be the same one the executor unpacks
# at install time. In our workspace `holochain_cli` isn't a separate
# workspace package entry in Cargo.lock, but `holochain_cli_bundle` is,
# and it comes from the same coasys/holochain repo/tree as the `hc` binary.
#
# The relevant block looks like:
#
#   [[package]]
#   name = "holochain_cli_bundle"
#   version = "0.7.0"
#   source = "git+https://github.com/coasys/holochain.git?branch=0.7.0-space-override-coasys#678f683e..."
#
# awk grabs the `source =` line inside the `holochain_cli_bundle` block.
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
# or:    source = "git+<url>?tag=<tag>#<rev>"
# or:    source = "git+<url>#<rev>"
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

echo "install-hc-toolchain: target = $GIT_URL"
[[ -n "$BRANCH" ]] && echo "install-hc-toolchain: branch = $BRANCH"
[[ -n "$TAG"    ]] && echo "install-hc-toolchain: tag    = $TAG"
echo "install-hc-toolchain: rev    = $REV"
echo "install-hc-toolchain: dest   = $BIN_DIR"

# Idempotency: if the stamp file matches the target rev AND the binary exists,
# skip the (expensive) cargo install.
if [[ -f "$STAMP_FILE" && -x "$BIN_DIR/hc" ]]; then
    INSTALLED_REV=$(cat "$STAMP_FILE" 2>/dev/null || echo "")
    if [[ "$INSTALLED_REV" == "$REV" ]]; then
        echo "install-hc-toolchain: already at rev $REV — skipping install"
        "$BIN_DIR/hc" --version
        exit 0
    fi
    echo "install-hc-toolchain: rev drift ($INSTALLED_REV → $REV) — reinstalling"
fi

mkdir -p "$BIN_DIR"

# Build the --git ref flag.
REF_ARG=()
if [[ -n "$TAG" ]]; then
    REF_ARG=(--tag "$TAG")
elif [[ -n "$BRANCH" ]]; then
    # Use --rev for exact match rather than --branch — branches drift.
    REF_ARG=(--rev "$REV")
else
    REF_ARG=(--rev "$REV")
fi

# Ensure cargo is on PATH (CI setup_env normally adds ~/.cargo/bin).
if ! command -v cargo >/dev/null 2>&1; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
if ! command -v cargo >/dev/null 2>&1; then
    echo "install-hc-toolchain: cargo not found on PATH" >&2
    exit 1
fi

# Clone (or reuse) the source tree at the exact rev.
#
# We DO NOT build inside the cloned tree with plain `cargo build` because
# that tree's Cargo.lock has a libsqlite3-sys `links="sqlite3"` conflict
# (holochain_data pulls libsqlite3-sys 0.38, sqlx-sqlite 0.9 caps at <0.38,
# and cargo refuses to link two versions with the same `links` value). Our
# top-level workspace Cargo.toml has [patch.crates-io] entries for sqlx +
# lair that align the whole chain on libsqlite3-sys 0.38, but those patches
# don't propagate into a standalone clone.
#
# Instead we OVERLAY the workspace's [patch.crates-io] block onto the clone's
# root Cargo.toml before building. The clone's tree is isolated per workspace
# (in $REPO/.hc-toolchain/src) and is only used as a build environment for
# `hc` itself — no one else consumes it. Overlay is idempotent: the marker
# comment `# BEGIN AD4M PATCH OVERLAY` is used to detect and re-apply.
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
# Reset any prior overlay before checkout so `git checkout` doesn't
# complain about local modifications, then re-apply below.
git checkout -- Cargo.toml 2>/dev/null || true
git checkout "$REV"

# Overlay: replace the clone's [patch.crates-io] block with ours, so the
# resolver sees the same libsqlite3-sys alignment we use in our own
# workspace. We can't just `cat` — the clone may already have its own
# [patch.crates-io] block, and duplicates make cargo fail. So we strip any
# existing block first, then append.
AD4M_PATCH_MARKER="# BEGIN AD4M PATCH OVERLAY"
AD4M_PATCH_END="# END AD4M PATCH OVERLAY"
python3 - <<PYEOF
import re, pathlib
toml_path = pathlib.Path("$SRC_DIR/Cargo.toml")
src = toml_path.read_text()

# Strip any prior overlay block
src = re.sub(
    r"\n$AD4M_PATCH_MARKER.*?$AD4M_PATCH_END\n?",
    "",
    src,
    flags=re.DOTALL,
)

# Strip clone's own [patch.crates-io] section (from the header line to the
# next top-level [ ... ] section or EOF).
src = re.sub(
    r"\n\[patch\.crates-io\][^\[]*(?=\n\[|\Z)",
    "",
    src,
    flags=re.DOTALL,
)

# Append our overlay copied from the workspace root.
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

# Record the installed rev so future re-runs on the same workdir short-circuit.
echo "$REV" > "$STAMP_FILE"

echo "install-hc-toolchain: installed:"
"$BIN_DIR/hc" --version
