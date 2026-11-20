#!/usr/bin/env bash
# The web executor's build check (S8, milestone M1). From the repo root:
#   bash scripts/web-executor/check.sh
#
# 1. wasm32 check: `cargo check -p ad4m-executor --lib --target wasm32-unknown-unknown`,
#    the real crate with ring 2 real and the other modules stubbed (src/lib_wasm.rs).
#    Prints `wasm32 check: <n> errors`.
# 2. wasm32 link: a release build of web-executor-link-test, a cdylib whose one export
#    reaches every real RPC handler, SQLite and Oxigraph, so a missing symbol fails it.
#    Prints `wasm32 link: ok` or `wasm32 link: failed`.
# 3. The size of that Wasm, raw and gzip (-9), as cargo writes it: no wasm-bindgen and no
#    wasm-opt pass.
# 4. The link test crate's wasm-bindgen tests under Node (web-executor-link-test/tests/),
#    when wasm-bindgen-test-runner is on the PATH; skipped otherwise.
#
# Exit 0 only when the check, the link and the tests (if run) pass. Uses CARGO_TARGET_DIR
# when it is set. The compiler output of a failing step is in $LOG_DIR (default: a temporary
# directory, printed on failure).

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT" || exit 2
LOG_DIR="${LOG_DIR:-$(mktemp -d)}"
TARGET=wasm32-unknown-unknown
STATUS=0

# 1. Type check.
cargo check -p ad4m-executor --lib --target "$TARGET" --message-format=short \
    >"$LOG_DIR/check.log" 2>&1
CHECK_EXIT=$?
ERRORS=$(grep -cE '^[^ ]+: error(\[E[0-9]+\])?: ' "$LOG_DIR/check.log")
if [ "$CHECK_EXIT" -ne 0 ] && [ "$ERRORS" -eq 0 ]; then
    # Failed before any located error (a build script, the resolver): count it as one.
    ERRORS=1
fi
echo "wasm32 check: $ERRORS errors"
if [ "$ERRORS" -ne 0 ]; then
    echo "  log: $LOG_DIR/check.log"
    STATUS=1
fi

# 2. Link.
if cargo build --release -p web-executor-link-test --target "$TARGET" \
    >"$LOG_DIR/link.log" 2>&1; then
    echo "wasm32 link: ok"
else
    echo "wasm32 link: failed"
    grep -oE 'undefined symbol: [A-Za-z_0-9]+' "$LOG_DIR/link.log" | sort -u | head -20 \
        | sed 's/^/  /'
    echo "  log: $LOG_DIR/link.log"
    exit 1
fi

# 3. Size.
TARGET_DIR="${CARGO_TARGET_DIR:-$ROOT/target}"
WASM="$TARGET_DIR/$TARGET/release/web_executor_link_test.wasm"
RAW=$(wc -c <"$WASM")
GZ=$(gzip -9 -c "$WASM" | wc -c)
mib() { awk -v b="$1" 'BEGIN { printf "%.1f MiB", b / 1048576 }'; }
echo "wasm32 size: raw $RAW bytes ($(mib "$RAW")), gzip $GZ bytes ($(mib "$GZ"))"

# 4. Tests under Node.
if command -v wasm-bindgen-test-runner >/dev/null 2>&1; then
    if CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner \
        cargo test -p web-executor-link-test --target "$TARGET" >"$LOG_DIR/test.log" 2>&1; then
        PASSED=$(grep -oE '[0-9]+ passed' "$LOG_DIR/test.log" | awk '{ n += $1 } END { print n + 0 }')
        echo "wasm32 tests: $PASSED passed"
    else
        echo "wasm32 tests: failed"
        echo "  log: $LOG_DIR/test.log"
        STATUS=1
    fi
else
    echo "wasm32 tests: skipped (no wasm-bindgen-test-runner)"
fi

exit "$STATUS"
