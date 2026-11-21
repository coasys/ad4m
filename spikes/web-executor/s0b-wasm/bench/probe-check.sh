#!/bin/bash
# Usage: probe-check.sh <run-name> [extra cargo args]  -> wasm + native error lists and the diff
# Env: S = the s0b-wasm dir (default: parent of this script), CARGO_TARGET_DIR (default: $S/target).
set -u
name=$1; shift
S=${S:-$(cd "$(dirname "$0")/.." && pwd)}
B=${B:-$S/bench}
export CARGO_TARGET_DIR=${CARGO_TARGET_DIR:-$S/target}
cd "$S/probe"
cargo +1.96.1 check --target wasm32-unknown-unknown --message-format short "$@" > "$B/$name-wasm.log" 2>&1
cargo +1.96.1 check --message-format short "$@" > "$B/$name-native.log" 2>&1
grep -E "^(src/|/).*: error" "$B/$name-wasm.log" | sort -u > "$B/$name-wasm.err"
grep -E "^(src/|/).*: error" "$B/$name-native.log" | sort -u > "$B/$name-native.err"
echo "wasm errors: $(wc -l < "$B/$name-wasm.err")  native errors: $(wc -l < "$B/$name-native.err")"
grep -E "^error: could not compile" "$B/$name-wasm.log" "$B/$name-native.log"
echo "--- wasm-only:"; comm -23 "$B/$name-wasm.err" "$B/$name-native.err"
echo "--- native-only:"; comm -13 "$B/$name-wasm.err" "$B/$name-native.err"
