#!/usr/bin/env bash
# Tests for rust-executor/scripts/build-deno-snapshot.sh. They run a copy of it
# in scratch workdirs (stand-in inputs, no real sources) with a fake `cargo`
# that "builds" outputs whose content the test picks (FAKE_SNAPSHOT). No
# network, no Rust build; the real snapshot is never touched.
# Run: bash rust-executor/scripts/build-deno-snapshot.test.sh
# SNAPSHOT_SCRIPT=<path> runs the cases against another copy of the script.

set -uo pipefail

SCRIPT="${SNAPSHOT_SCRIPT:-$(cd "$(dirname "$0")" && pwd)/build-deno-snapshot.sh}"
FAILED=0

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# Sets TMP (scratch root), CACHE (an artifact dir, unused unless a case passes
# it) and the fake cargo in $TMP/bin. Each cargo call appends to $TMP/cargo.calls.
make_fixture() {
    TMP="$(mktemp -d)"
    CACHE="$TMP/artifacts"
    mkdir -p "$TMP/bin"
    cat > "$TMP/bin/cargo" <<'EOF'
#!/usr/bin/env bash
echo "$*" >> "$FAKE_CARGO_LOG"
if [[ "$*" != "run --release --features generate_snapshot --bin generate_snapshot" ]]; then
    echo "fake cargo: unexpected arguments: $*" >&2
    exit 2
fi
printf 'snapshot %s\n' "$FAKE_SNAPSHOT" > CUSTOM_DENO_SNAPSHOT.bin
printf '// esm %s\n' "$FAKE_SNAPSHOT" > CUSTOM_DENO_SNAPSHOT.residual_esm.rs
printf '// js %s\n' "$FAKE_SNAPSHOT" > CUSTOM_DENO_SNAPSHOT.residual_js.rs
EOF
    chmod +x "$TMP/bin/cargo"
}

# A workspace with every input the script hashes, and the script in place.
# Workdirs made by this hold the same inputs, so they share one input hash.
make_workdir() {
    local w="$TMP/$1" r
    r="$w/rust-executor"
    mkdir -p "$r/scripts" "$r/src/bin" "$r/src/holochain_service" "$r/src/runtime_service" "$r/src/js_core/sub"
    echo lock > "$w/Cargo.lock"
    echo '[workspace]' > "$w/Cargo.toml"
    echo '[package]' > "$r/Cargo.toml"
    cp "$SCRIPT" "$r/scripts/build-deno-snapshot.sh"
    echo 'fn main() {}' > "$r/src/bin/generate_snapshot.rs"
    for f in holochain_service/holochain_service_extension runtime_service/runtime_service_extension; do
        echo "// $f" > "$r/src/$f.rs"
        echo "// $f" > "$r/src/$f.js"
    done
    echo '// mod' > "$r/src/js_core/mod.rs"
    echo '// sub' > "$r/src/js_core/sub/x.js"
    echo "$r"
}

# run_snapshot <rust-executor dir> <FAKE_SNAPSHOT> [cache dir]
# Output goes to $TMP/run.log; the status is the script's.
run_snapshot() {
    local dir=$1 content=$2 cache=${3:-}
    : > "$TMP/cargo.calls"
    # CI exports BASH_ENV (real cargo back in front), and maybe CI_ARTIFACT_DIR.
    env -u BASH_ENV -u CI_ARTIFACT_DIR -u FORCE_DENO_SNAPSHOT \
        ${cache:+CI_ARTIFACT_DIR="$cache"} \
        PATH="$TMP/bin:$PATH" FAKE_SNAPSHOT="$content" FAKE_CARGO_LOG="$TMP/cargo.calls" \
        bash "$dir/scripts/build-deno-snapshot.sh" > "$TMP/run.log" 2>&1
}

show_log() { sed 's/^/    /' "$TMP/run.log"; }

cargo_calls() { wc -l < "$TMP/cargo.calls" | tr -d ' '; }

said() { grep -q "$1" "$TMP/run.log"; }

# outputs_are <dir> <content>: every output in <dir> is the fake build of <content>.
outputs_are() {
    [[ "$(cat "$1/CUSTOM_DENO_SNAPSHOT.bin" 2>/dev/null)" == "snapshot $2" ]] \
        && [[ "$(cat "$1/CUSTOM_DENO_SNAPSHOT.residual_esm.rs" 2>/dev/null)" == "// esm $2" ]] \
        && [[ "$(cat "$1/CUSTOM_DENO_SNAPSHOT.residual_js.rs" 2>/dev/null)" == "// js $2" ]]
}

# The single cache entry (the fixture's inputs give one hash).
entry() {
    local e
    for e in "$CACHE"/deno-snapshot/*; do
        [[ "$e" == *.tmp.* ]] || { echo "$e"; return; }
    done
}

test_build_then_up_to_date() {
    local d
    d=$(make_workdir w)
    run_snapshot "$d" Y || { show_log; fail "first run failed"; return; }
    [[ "$(cargo_calls)" == 1 ]] || fail "first run called cargo $(cargo_calls) times, want 1"
    said "Deno snapshot built" || fail "first run did not say built"
    [[ -s "$d/CUSTOM_DENO_SNAPSHOT.inputs" ]] || fail "first run wrote no stamp"
    outputs_are "$d" Y || fail "outputs are not the build's"

    run_snapshot "$d" Y || { show_log; fail "second run failed"; return; }
    [[ "$(cargo_calls)" == 0 ]] || fail "second run called cargo"
    said "Deno snapshot up to date" || { show_log; fail "second run did not say up to date"; }
}

# The fault: a kept workdir got another commit's outputs (a restored CI stash)
# under its own stamp, and the next build embedded them.
test_foreign_outputs_rebuilt() {
    local d
    d=$(make_workdir w)
    run_snapshot "$d" Y || { show_log; fail "first run failed"; return; }
    printf 'snapshot X\n' > "$d/CUSTOM_DENO_SNAPSHOT.bin"
    printf '// esm X\n' > "$d/CUSTOM_DENO_SNAPSHOT.residual_esm.rs"
    printf '// js X\n' > "$d/CUSTOM_DENO_SNAPSHOT.residual_js.rs"

    run_snapshot "$d" Y || { show_log; fail "run over foreign outputs failed"; return; }
    said "up to date" && fail "foreign outputs were taken as up to date"
    [[ "$(cargo_calls)" == 1 ]] || fail "foreign outputs were not rebuilt"
    outputs_are "$d" Y || fail "the foreign outputs are still in place"
}

# The same with a cache: the foreign outputs are not stored, the entry restores.
test_foreign_outputs_with_cache() {
    local d e
    d=$(make_workdir w)
    run_snapshot "$d" Y "$CACHE" || { show_log; fail "first run failed"; return; }
    printf 'snapshot X\n' > "$d/CUSTOM_DENO_SNAPSHOT.bin"
    printf '// esm X\n' > "$d/CUSTOM_DENO_SNAPSHOT.residual_esm.rs"
    printf '// js X\n' > "$d/CUSTOM_DENO_SNAPSHOT.residual_js.rs"

    run_snapshot "$d" Y "$CACHE" || { show_log; fail "run over foreign outputs failed"; return; }
    said "up to date" && fail "foreign outputs were taken as up to date"
    outputs_are "$d" Y || fail "the foreign outputs are still in place"
    e=$(entry)
    outputs_are "$e" Y || fail "the foreign outputs reached the cache entry"
    [[ "$(cargo_calls)" == 0 ]] || fail "the good cache entry was not restored"
    said "restored from" || fail "did not say restored"
}

test_cache_store_and_restore() {
    local a b e
    a=$(make_workdir a)
    b=$(make_workdir b)
    run_snapshot "$a" Y "$CACHE" || { show_log; fail "build run failed"; return; }
    e=$(entry)
    [[ -n "$e" ]] || { fail "no cache entry stored"; return; }
    [[ -s "$e/digest" ]] || fail "the cache entry has no digest file"
    outputs_are "$e" Y || fail "the cache entry does not hold the build"
    [[ -z "$(find "$CACHE/deno-snapshot" -maxdepth 1 -name '*.tmp.*')" ]] || fail "a tmp dir was left"

    run_snapshot "$b" Z "$CACHE" || { show_log; fail "restore run failed"; return; }
    [[ "$(cargo_calls)" == 0 ]] || fail "a fresh workdir called cargo instead of restoring"
    said "restored from" || { show_log; fail "did not say restored"; }
    outputs_are "$b" Y || fail "the fresh workdir does not hold the cached outputs"

    run_snapshot "$b" Z "$CACHE" || { show_log; fail "run after restore failed"; return; }
    said "up to date" || { show_log; fail "a restored workdir is not up to date"; }
}

# An entry stored before digests existed may hold anything: never restore it.
test_entry_without_digest_replaced() {
    local a b e
    a=$(make_workdir a)
    b=$(make_workdir b)
    run_snapshot "$a" X "$CACHE" || { show_log; fail "seed run failed"; return; }
    e=$(entry)
    rm -f "$e/digest"

    run_snapshot "$b" Y "$CACHE" || { show_log; fail "run failed"; return; }
    [[ "$(cargo_calls)" == 1 ]] || fail "an entry without digest was restored"
    outputs_are "$b" Y || fail "the workdir holds the old entry's outputs"
    [[ -s "$e/digest" ]] || fail "the old entry was not replaced (no digest)"
    outputs_are "$e" Y || fail "the old entry was not replaced (old files)"
}

# An entry whose files changed since it was stored is not used.
test_entry_not_matching_digest_unused() {
    local a b e
    a=$(make_workdir a)
    b=$(make_workdir b)
    run_snapshot "$a" Y "$CACHE" || { show_log; fail "seed run failed"; return; }
    e=$(entry)
    printf 'snapshot X\n' > "$e/CUSTOM_DENO_SNAPSHOT.bin"

    run_snapshot "$b" Y "$CACHE" || { show_log; fail "run failed"; return; }
    said "restored from" && fail "a damaged entry was restored"
    [[ "$(cargo_calls)" == 1 ]] || fail "a damaged entry did not lead to a build"
    outputs_are "$b" Y || fail "the workdir holds the damaged entry's outputs"
    outputs_are "$e" Y || fail "the damaged entry was not replaced"

    run_snapshot "$b" Y "$CACHE" || { show_log; fail "run after rebuild failed"; return; }
    said "up to date" || { show_log; fail "not up to date after the rebuild"; }
}

test_missing_input_fails() {
    local d
    d=$(make_workdir w)
    rm "$d/src/bin/generate_snapshot.rs"
    if run_snapshot "$d" Y; then
        fail "a missing input did not fail the script"
    fi
    [[ "$(cargo_calls)" == 0 ]] || fail "cargo ran despite a missing input"
}

for t in test_build_then_up_to_date test_foreign_outputs_rebuilt test_foreign_outputs_with_cache \
    test_cache_store_and_restore test_entry_without_digest_replaced \
    test_entry_not_matching_digest_unused test_missing_input_fails; do
    echo "$t"
    make_fixture
    "$t"
    rm -rf "$TMP"
done

if [[ "$FAILED" != 0 ]]; then
    echo "build-deno-snapshot tests: FAILED"
    exit 1
fi
echo "build-deno-snapshot tests: passed"
