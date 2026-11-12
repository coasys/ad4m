#!/usr/bin/env bash
# Tests for scripts/release-checksum.sh: the line it prints checks a downloaded asset with
# `sha256sum -c` (and `shasum -a 256 -c` where that exists), and refuses an altered one.
# No network. Run: bash scripts/release-checksum.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/release-checksum.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail() {
  echo "  FAIL: $*"
  FAILED=1
}

# The checkers present on this machine; each test runs with every one.
CHECKERS=()
command -v sha256sum >/dev/null && CHECKERS+=("sha256sum")
command -v shasum >/dev/null && CHECKERS+=("shasum -a 256")
if [ "${#CHECKERS[@]}" -eq 0 ]; then
  echo "neither sha256sum nor shasum is installed"
  exit 1
fi

mkdir -p "$TMP/build" "$TMP/download"
head -c 100000 /dev/urandom > "$TMP/build/ad4m-executor"
ASSET=ad4m-cli-executor-linux-1.2.3-x64

echo "test: the line names the asset and holds the file's SHA-256"
line="$(bash "$SCRIPT" "$TMP/build/ad4m-executor" "$ASSET")" || fail "the script failed"
if ! printf '%s\n' "$line" | grep -Eq "^[0-9a-f]{64}  $ASSET\$"; then
  fail "unexpected line: $line"
fi
expected="$(${CHECKERS[0]} "$TMP/build/ad4m-executor" | cut -d' ' -f1)"
[ "${line%% *}" = "$expected" ] || fail "hash ${line%% *} is not $expected"

echo "test: a download under the asset's name checks"
cp "$TMP/build/ad4m-executor" "$TMP/download/$ASSET"
printf '%s\n' "$line" > "$TMP/download/$ASSET.sha256"
for checker in "${CHECKERS[@]}"; do
  (cd "$TMP/download" && $checker -c "$ASSET.sha256" >/dev/null 2>&1) \
    || fail "$checker -c refused an intact download"
done

echo "test: an altered download does not check"
printf 'x' >> "$TMP/download/$ASSET"
for checker in "${CHECKERS[@]}"; do
  if (cd "$TMP/download" && $checker -c "$ASSET.sha256" >/dev/null 2>&1); then
    fail "$checker -c accepted an altered download"
  fi
done

echo "test: refuses a missing or empty file"
if bash "$SCRIPT" "$TMP/build/nothing-here" "$ASSET" >/dev/null 2>&1; then
  fail "accepted a missing file"
fi
: > "$TMP/build/empty"
if bash "$SCRIPT" "$TMP/build/empty" "$ASSET" >/dev/null 2>&1; then
  fail "accepted an empty file"
fi

echo "test: refuses an asset name that is a path"
if bash "$SCRIPT" "$TMP/build/ad4m-executor" "dir/$ASSET" >/dev/null 2>&1; then
  fail "accepted a path as the asset name"
fi
if bash "$SCRIPT" "$TMP/build/ad4m-executor" "" >/dev/null 2>&1; then
  fail "accepted an empty asset name"
fi

echo "test: refuses a wrong number of arguments"
if bash "$SCRIPT" "$TMP/build/ad4m-executor" >/dev/null 2>&1; then
  fail "accepted one argument"
fi

if [ "$FAILED" -ne 0 ]; then
  echo "release-checksum tests FAILED"
  exit 1
fi
echo "release-checksum tests passed"
