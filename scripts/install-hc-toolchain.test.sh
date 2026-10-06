#!/usr/bin/env bash
# Tests for scripts/install-hc-toolchain.sh. They run it against a local
# stand-in for the coasys/holochain repo and a fake `cargo`, so they need no
# network and no Rust build. Run: bash scripts/install-hc-toolchain.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/install-hc-toolchain.sh"
FAILED=0

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# Builds a fresh fixture under $TMP and sets UPSTREAM, WORKSPACE, SRC, REV.
make_fixture() {
    TMP="$(mktemp -d)"
    UPSTREAM="$TMP/holochain"
    WORKSPACE="$TMP/ad4m"
    SRC="$WORKSPACE/.hc-toolchain/src"

    # Stand-in for coasys/holochain: a workspace with one member crate.
    git init -q -b hc-branch "$UPSTREAM"
    mkdir -p "$UPSTREAM/crates/fixt"
    printf '[workspace]\nmembers = ["crates/fixt"]\n' > "$UPSTREAM/Cargo.toml"
    printf '[package]\nname = "fixt"\n' > "$UPSTREAM/crates/fixt/Cargo.toml"
    printf 'target/\n' > "$UPSTREAM/.gitignore"
    git -C "$UPSTREAM" add -A
    git -C "$UPSTREAM" -c user.name=t -c user.email=t@t commit -qm init
    REV="$(git -C "$UPSTREAM" rev-parse HEAD)"

    # Workspace that pins that rev in Cargo.lock, with the script copied in
    # (the script finds the workspace as its own parent directory).
    mkdir -p "$WORKSPACE/scripts"
    cp "$SCRIPT" "$WORKSPACE/scripts/"
    cat > "$WORKSPACE/Cargo.lock" <<EOF
[[package]]
name = "holochain_cli_bundle"
version = "0.7.0"
source = "git+file://$UPSTREAM?branch=hc-branch#$REV"
EOF
    printf '[workspace]\nmembers = []\n\n[patch.crates-io]\nfoo = { path = "foo" }\n' > "$WORKSPACE/Cargo.toml"

    # Fake cargo: fails like the real one when a workspace member's manifest
    # is missing, otherwise "builds" target/release/hc.
    mkdir -p "$TMP/bin"
    cat > "$TMP/bin/cargo" <<'EOF'
#!/usr/bin/env bash
if [[ ! -f crates/fixt/Cargo.toml ]]; then
    echo "error: failed to read \`$PWD/crates/fixt/Cargo.toml\`" >&2
    exit 101
fi
mkdir -p target/release
printf '#!/bin/sh\necho "holochain_cli fake"\n' > target/release/hc
chmod +x target/release/hc
EOF
    chmod +x "$TMP/bin/cargo"
}

install_toolchain() {
    PATH="$TMP/bin:$PATH" bash "$WORKSPACE/scripts/install-hc-toolchain.sh" > "$TMP/install.log" 2>&1 || {
        tail -n 20 "$TMP/install.log" | sed 's/^/    /'
        return 1
    }
}

test_fresh_install() {
    install_toolchain || fail "install from a fresh clone failed"
    [[ "$(cat "$WORKSPACE/.hc-toolchain/.installed_rev" 2>/dev/null)" == "$REV" ]] \
        || fail "stamp file does not hold $REV"
    "$WORKSPACE/.hc-toolchain/bin/hc" --version 2>/dev/null | grep -q 'holochain_cli fake' \
        || fail "bin/hc is not the built binary"
}

# #1313: a runner workdir kept .hc-toolchain/src/.git but lost every other
# file (and the stamp and bin/). `git checkout <rev>` keeps deleted tracked
# files deleted, so the build failed on crates/fixt/Cargo.toml.
test_restores_deleted_tracked_files() {
    install_toolchain || { fail "first install failed"; return; }

    mkdir -p "$SRC/target/release"
    echo cached > "$SRC/target/release/marker"
    git -C "$SRC" ls-files -z | (cd "$SRC" && xargs -0 rm -f)
    rm -rf "$SRC/crates" "$WORKSPACE/.hc-toolchain/bin" "$WORKSPACE/.hc-toolchain/.installed_rev"

    install_toolchain || fail "install over a checkout with deleted files failed"
    [[ -f "$SRC/crates/fixt/Cargo.toml" ]] || fail "crates/fixt/Cargo.toml was not restored"
    [[ -z "$(git -C "$SRC" ls-files --deleted)" ]] || fail "tracked files are still deleted"
    [[ -f "$SRC/target/release/marker" ]] || fail "the build cache under target/ was removed"
    [[ "$(cat "$WORKSPACE/.hc-toolchain/.installed_rev" 2>/dev/null)" == "$REV" ]] \
        || fail "stamp file does not hold $REV"
}

for t in test_fresh_install test_restores_deleted_tracked_files; do
    echo "$t"
    make_fixture
    "$t"
    rm -rf "$TMP"
done

if [[ "$FAILED" != 0 ]]; then
    echo "install-hc-toolchain tests: FAILED"
    exit 1
fi
echo "install-hc-toolchain tests: passed"
