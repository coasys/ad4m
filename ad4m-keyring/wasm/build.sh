#!/usr/bin/env bash
# Builds the keyring's JS binding (AR-08 K9) twice from one wasm:
#   --target deno   -> bootstrap-languages/server-link-language/keyring/keyring.js (one ES module,
#                      the wasm inlined as base64, so the language bundle carries it)
#   --target nodejs -> link-server/keyring/ (CommonJS + the .wasm file beside it)
# Both outputs are generated and gitignored; run this before building or testing either package.
#
# The wasm-bindgen CLI must be the version Cargo.lock locks. It is looked up as $WASM_BINDGEN,
# then target/wasm-tools/bin/wasm-bindgen (where AGENTS.md installs it), then PATH:
#   cargo install wasm-bindgen-cli --version <locked version> --locked --root target/wasm-tools
# wasm-opt -Oz runs when it is on PATH (binaryen); without it the wasm stays as cargo built it.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TARGET_DIR="${CARGO_TARGET_DIR:-$REPO_ROOT/target}"
NAME=ad4m_keyring_wasm
STAGE="$TARGET_DIR/keyring-wasm"

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

# The wasm carries the source paths of its panic locations; remapped, they do not depend on
# where the repo and the cargo registry are.
export RUSTFLAGS="${RUSTFLAGS:-} --remap-path-prefix=$REPO_ROOT=/ad4m --remap-path-prefix=${CARGO_HOME:-$HOME/.cargo}=/cargo"

echo "==> cargo build -p ad4m-keyring-wasm --target wasm32-unknown-unknown --release"
(cd "$REPO_ROOT" && cargo build -p ad4m-keyring-wasm --target wasm32-unknown-unknown --release)
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

echo "==> wasm-bindgen --target deno"
"$WASM_BINDGEN" --target deno --out-dir "$STAGE/deno" "$STAGE/$NAME.wasm"
LANG_OUT="$REPO_ROOT/bootstrap-languages/server-link-language/keyring"
mkdir -p "$LANG_OUT"
node "$SCRIPT_DIR/inline.mjs" "$STAGE/deno" "$NAME" "$LANG_OUT/keyring.js"
cp "$STAGE/deno/$NAME.d.ts" "$LANG_OUT/keyring.d.ts"

echo "==> wasm-bindgen --target nodejs"
SERVER_OUT="$REPO_ROOT/link-server/keyring"
"$WASM_BINDGEN" --target nodejs --out-dir "$SERVER_OUT" "$STAGE/$NAME.wasm"
# The output is CommonJS inside an ES-module package.
printf '{ "type": "commonjs" }\n' > "$SERVER_OUT/package.json"

echo "==> keyring binding built:"
ls -l "$LANG_OUT/keyring.js" "$SERVER_OUT/$NAME.js" "$SERVER_OUT/${NAME}_bg.wasm"
