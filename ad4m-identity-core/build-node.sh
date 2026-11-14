#!/usr/bin/env bash
# Builds the identity core's wasm-bindgen exports (feature `wasm`: `KelChain`, `KelHandle`
# with `trustedSeq`/`judgeLink`/`authorOf`, `MembershipReplay`, `judgeLinkWithoutLog`,
# `wireLinkHash`, `wireSignatureHolds`, `roomKeySigningBytes`, `verifyRoomKey`) for Node:
#   --target nodejs -> link-server/identity-core/ (CommonJS + the .wasm file beside it)
# The output is generated and gitignored; run this before building or testing the link server.
#
# The crate is an rlib; `cargo rustc --crate-type cdylib` makes the wasm module without
# changing what other crates link. The wasm-bindgen CLI must be the version Cargo.lock locks,
# looked up as in ad4m-keyring/wasm/build.sh: $WASM_BINDGEN, target/wasm-tools/bin, then PATH.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TARGET_DIR="${CARGO_TARGET_DIR:-$REPO_ROOT/target}"
NAME=ad4m_identity_core
STAGE="$TARGET_DIR/identity-core-wasm"

LOCKED="$(awk '/^name = "wasm-bindgen"$/ { getline; gsub(/version = |"/, ""); print; exit }' "$REPO_ROOT/Cargo.lock")"
WASM_BINDGEN="${WASM_BINDGEN:-}"
if [ -z "$WASM_BINDGEN" ]; then
    if [ -x "$TARGET_DIR/wasm-tools/bin/wasm-bindgen" ]; then
        WASM_BINDGEN="$TARGET_DIR/wasm-tools/bin/wasm-bindgen"
    elif [ -x "$REPO_ROOT/target/wasm-tools/bin/wasm-bindgen" ]; then
        WASM_BINDGEN="$REPO_ROOT/target/wasm-tools/bin/wasm-bindgen"
    else
        WASM_BINDGEN="$(command -v wasm-bindgen || true)"
    fi
fi
if [ -z "$WASM_BINDGEN" ] || [ "$("$WASM_BINDGEN" --version | awk '{print $2}')" != "$LOCKED" ]; then
    echo "!! wasm-bindgen $LOCKED is needed (Cargo.lock). Install it with:" >&2
    echo "     cargo install wasm-bindgen-cli --version $LOCKED --locked --root $REPO_ROOT/target/wasm-tools" >&2
    exit 1
fi

export RUSTFLAGS="${RUSTFLAGS:-} --remap-path-prefix=$REPO_ROOT=/ad4m --remap-path-prefix=${CARGO_HOME:-$HOME/.cargo}=/cargo"

echo "==> cargo rustc -p ad4m-identity-core --features wasm --crate-type cdylib (wasm32, release)"
(cd "$REPO_ROOT" && cargo rustc -p ad4m-identity-core --lib --features wasm \
    --target wasm32-unknown-unknown --release --crate-type cdylib)
WASM_IN="$TARGET_DIR/wasm32-unknown-unknown/release/$NAME.wasm"

mkdir -p "$STAGE"
if command -v wasm-opt >/dev/null 2>&1; then
    echo "==> wasm-opt -Oz"
    wasm-opt -Oz --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext \
        --enable-mutable-globals "$WASM_IN" -o "$STAGE/$NAME.wasm"
else
    echo "==> wasm-opt not on PATH: the wasm is not shrunk further"
    cp "$WASM_IN" "$STAGE/$NAME.wasm"
fi

echo "==> wasm-bindgen --target nodejs"
SERVER_OUT="$REPO_ROOT/link-server/identity-core"
"$WASM_BINDGEN" --target nodejs --out-dir "$SERVER_OUT" "$STAGE/$NAME.wasm"
# The output is CommonJS inside an ES-module package.
printf '{ "type": "commonjs" }\n' > "$SERVER_OUT/package.json"

echo "==> identity core binding built:"
ls -l "$SERVER_OUT/$NAME.js" "$SERVER_OUT/${NAME}_bg.wasm"
