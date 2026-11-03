#!/usr/bin/env bash
# Regression for the WE agent tools that rust-executor/build.rs downloads into OUT_DIR and
# tool_bundles/builtin.rs compiles in with include_str!: a cached file that changes after the
# build script checked it must not reach the compiled crate unchecked.
#
# It does an ordinary incremental `cargo check` of the executor, appends a marker to the cached
# agent-tools.bundle.js (no pin change, build.rs untouched), checks again, and fails if the
# marker is in the crate's metadata. It deletes the cached bundle and checks again, which must
# fetch it. Then it checks twice more with nothing changed, and fails if either of those re-runs
# the build script.
#
# It checks the real executor crate, so it needs that crate's dependencies built (a warm target
# dir) and, the first time or after the corruption, the network to fetch the release.
# Run: bash scripts/we-agent-tools-cache.test.sh [extra cargo args, default --release]

set -uo pipefail

cd "$(dirname "$0")/.."
ARGS=("$@")
[ ${#ARGS[@]} -eq 0 ] && ARGS=(--release)
FAILED=0

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

check() {
    cargo check -p ad4m-executor --lib "${ARGS[@]}" "$@"
}

# Whether the last check's verbose output ran ad4m-executor's build script.
ran_build_script() {
    grep -E 'Running `.*build/ad4m-executor-[0-9a-f]+/build-script-build' "$1" >/dev/null
}

echo "warm check"
json="$(check --message-format=json 2>/dev/null)" || { echo "the warm check failed"; exit 1; }
out_dir="$(jq -r 'select(.reason=="build-script-executed" and (.package_id|test("ad4m-executor"))) | .out_dir' <<<"$json")"
rmeta="$(jq -r 'select(.reason=="compiler-artifact" and .target.name=="rust_executor") | .filenames[] | select(endswith(".rmeta"))' <<<"$json")"
bundle="$out_dir/agent-tools.bundle.js"
[ -f "$bundle" ] && [ -f "$rmeta" ] || { echo "no cached bundle or no metadata: $bundle $rmeta"; exit 1; }
good="$(sha256sum "$bundle" | cut -d' ' -f1)"
backup="$(mktemp)"
cp -p "$bundle" "$backup"
# Leave the cache as it was found, whatever the build made of it. A fresh mtime, so the next
# build compiles the restored file in again if the corrupted one got in.
restore() {
    cmp -s "$backup" "$bundle" || cp "$backup" "$bundle"
    rm -f "$backup"
}
trap restore EXIT

echo "corrupt the cached bundle, then check again"
marker="we-agent-tools-cache-regression-$$-$RANDOM"
printf '\n/* %s */\n' "$marker" >>"$bundle"
log="$(mktemp)"
check -v >"$log" 2>&1
status=$?
if grep -F "$marker" "$rmeta" >/dev/null; then
    fail "the corrupted bundle was compiled in ($rmeta has $marker)"
fi
if ! ran_build_script "$log"; then
    fail "the build script did not re-check the changed cache"
fi
if [ $status -eq 0 ] && [ "$(sha256sum "$bundle" | cut -d' ' -f1)" != "$good" ]; then
    fail "the check passed with a cached bundle that does not have the pinned hash"
fi

echo "delete the cached bundle, then check again"
rm -f "$bundle"
check >"$log" 2>&1 || fail "the check failed with the cached bundle deleted"
[ "$(sha256sum "$bundle" 2>/dev/null | cut -d' ' -f1)" = "$good" ] || fail "the deleted bundle was not fetched again"

# Both steps above leave a freshly downloaded file, which is the case that can re-run the script
# on every build: cargo compares a rerun-if-changed file's mtime with the start of the run.
echo "two checks with nothing changed"
for i in 1 2; do
    check -v >"$log" 2>&1 || fail "no-op check $i failed"
    if ran_build_script "$log"; then
        fail "no-op check $i re-ran the build script"
    fi
done
rm -f "$log"

[ $FAILED -eq 0 ] && echo "PASS" || echo "FAILED"
exit $FAILED
