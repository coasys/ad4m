#!/usr/bin/env bash
# Tests for rust-executor/scripts/build-deno-snapshot.sh. They run it in a
# scratch copy of the workspace layout with a fake `cargo` that "builds" the
# snapshot from the JS it finds, so they need no Rust build and no network.
# Run: bash rust-executor/scripts/build-deno-snapshot.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/build-deno-snapshot.sh"
FAILED=0
# Each test's workspace sits under ROOT; the tests run in subshells, which do
# not run this shell's EXIT trap, so ROOT is the one thing to remove.
ROOT="$(mktemp -d)"
trap 'rm -rf "$ROOT"' EXIT

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# A workspace under a fresh temp dir with every input the script hashes, and
# a fake cargo on PATH. Sets WS (the workspace), EXE (its rust-executor) and
# CARGO_LOG (one line per cargo run).
make_workspace() {
    local tmp
    tmp="$(mktemp -d "$ROOT/ws.XXXXXX")"
    WS="$tmp/ws"
    EXE="$WS/rust-executor"
    CARGO_LOG="$tmp/cargo.log"
    mkdir -p "$EXE/scripts" "$EXE/src/bin" "$EXE/src/holochain_service" \
        "$EXE/src/runtime_service" "$EXE/src/js_core" "$tmp/bin"
    cp "$SCRIPT" "$EXE/scripts/"
    echo lock >"$WS/Cargo.lock"
    echo ws >"$WS/Cargo.toml"
    echo exe >"$EXE/Cargo.toml"
    echo gen >"$EXE/src/bin/generate_snapshot.rs"
    for f in holochain_service/holochain_service_extension runtime_service/runtime_service_extension; do
        echo rs >"$EXE/src/$f.rs"
        echo js >"$EXE/src/$f.js"
    done
    echo "ops v1" >"$EXE/src/js_core/agent_extension.js"
    : >"$CARGO_LOG"
    # The fake generate_snapshot: each output names the JS it was made from.
    cat >"$tmp/bin/cargo" <<EOF
#!/usr/bin/env bash
echo run >>"$CARGO_LOG"
for f in CUSTOM_DENO_SNAPSHOT.bin CUSTOM_DENO_SNAPSHOT.residual_esm.rs CUSTOM_DENO_SNAPSHOT.residual_js.rs; do
    echo "snapshot of \$(cat src/js_core/agent_extension.js)" >"\$f"
done
EOF
    chmod +x "$tmp/bin/cargo"
    PATH="$tmp/bin:$PATH"
}

run_script() {
    OUT="$(bash "$EXE/scripts/build-deno-snapshot.sh" 2>&1)" || fail "script failed: $OUT"
}

builds() { wc -l <"$CARGO_LOG" | tr -d ' '; }

snapshot() { cat "$EXE/CUSTOM_DENO_SNAPSHOT.bin"; }

# Puts another commit's outputs over this workdir's, as CI's
# restore_rust_compile_inputs does, leaving the stamp alone.
overwrite_outputs() {
    local f
    for f in CUSTOM_DENO_SNAPSHOT.bin CUSTOM_DENO_SNAPSHOT.residual_esm.rs CUSTOM_DENO_SNAPSHOT.residual_js.rs; do
        echo "snapshot of $1" >"$EXE/$f"
    done
}

echo "test: builds once, then the stamp vouches for the outputs"
(
    unset CI_ARTIFACT_DIR FORCE_DENO_SNAPSHOT
    make_workspace
    run_script
    [[ "$(builds)" == 1 ]] || fail "first run made $(builds) builds"
    run_script
    [[ "$(builds)" == 1 ]] || fail "an unchanged workdir built again"
    [[ "$OUT" == *"up to date"* ]] || fail "second run: $OUT"
    [[ "$(snapshot)" == "snapshot of ops v1" ]] || fail "snapshot: $(snapshot)"
    exit "$FAILED"
) || FAILED=1

echo "test: outputs another commit put in the workdir are made again"
(
    unset CI_ARTIFACT_DIR FORCE_DENO_SNAPSHOT
    make_workspace
    run_script
    overwrite_outputs "ops of another branch"
    run_script
    [[ "$(builds)" == 2 ]] || fail "the foreign snapshot was kept ($OUT)"
    [[ "$(snapshot)" == "snapshot of ops v1" ]] || fail "snapshot: $(snapshot)"
    exit "$FAILED"
) || FAILED=1

echo "test: with the CI cache, foreign outputs are restored from it, not kept"
(
    unset FORCE_DENO_SNAPSHOT
    make_workspace
    export CI_ARTIFACT_DIR="$WS/../artifacts"
    run_script
    overwrite_outputs "ops of another branch"
    run_script
    [[ "$OUT" == *"restored from"* ]] || fail "expected a restore from the cache: $OUT"
    [[ "$(builds)" == 1 ]] || fail "built $(builds) times; the cache held the snapshot"
    [[ "$(snapshot)" == "snapshot of ops v1" ]] || fail "snapshot: $(snapshot)"
    run_script
    [[ "$OUT" == *"up to date"* ]] || fail "after the restore: $OUT"
    exit "$FAILED"
) || FAILED=1

echo "test: a stamp without checksums (older script) vouches for nothing"
(
    unset CI_ARTIFACT_DIR FORCE_DENO_SNAPSHOT
    make_workspace
    run_script
    head -n 1 "$EXE/CUSTOM_DENO_SNAPSHOT.inputs" >"$EXE/stamp.tmp"
    mv "$EXE/stamp.tmp" "$EXE/CUSTOM_DENO_SNAPSHOT.inputs"
    run_script
    [[ "$(builds)" == 2 ]] || fail "a hash-only stamp was trusted ($OUT)"
    run_script
    [[ "$(builds)" == 2 ]] || fail "the rewritten stamp was not trusted ($OUT)"
    exit "$FAILED"
) || FAILED=1

echo "test: changed JS builds again"
(
    unset CI_ARTIFACT_DIR FORCE_DENO_SNAPSHOT
    make_workspace
    run_script
    echo "ops v2" >"$EXE/src/js_core/agent_extension.js"
    run_script
    [[ "$(builds)" == 2 ]] || fail "changed JS did not build ($OUT)"
    [[ "$(snapshot)" == "snapshot of ops v2" ]] || fail "snapshot: $(snapshot)"
    exit "$FAILED"
) || FAILED=1

if [[ "$FAILED" == 0 ]]; then
    echo "all tests passed"
else
    echo "some tests failed"
fi
exit "$FAILED"
