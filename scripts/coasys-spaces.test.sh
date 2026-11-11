#!/usr/bin/env bash
# Tests for the decisions in scripts/coasys-spaces.sh that need no build and no network: taking the
# executor from the publish dir (the layout the build job's "Publish the executor on this host"
# step writes), refusing one that does not match its manifest, skipping a commit pair that already
# passed, and the model server check. A fake executor, a local git repository as WE and a small
# Node server as the model server; about two seconds.
# Run: bash scripts/coasys-spaces.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/coasys-spaces.sh"
FAILED=0
SHA=0123456789abcdef0123456789abcdef01234567

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# Fresh dirs under $TMP, and the script's environment pointed at them.
setup() {
    TMP="$(mktemp -d)"
    export AD4M_SHA="$SHA"
    export COASYS_SPACES_WORK="$TMP/work"
    export COASYS_SPACES_ARTIFACTS="$TMP/artifacts"
    export COASYS_SPACES_STATE="$TMP/state"
    export EXECUTOR_PUBLISH_DIR="$TMP/publish"
    export COASYS_SPACES_NO_BUILD=1
    unset COASYS_SPACES_FORCE WE_REF
    mkdir -p "$EXECUTOR_PUBLISH_DIR"
}

# Publishes a fake executor for $SHA as the build job does. $1: zst or gz.
publish() {
    local dest="$EXECUTOR_PUBLISH_DIR/$SHA" file
    mkdir -p "$dest"
    head -c 100000 /dev/urandom >"$TMP/ad4m-executor"
    if [ "$1" = zst ] && command -v zstd >/dev/null; then
        file=ad4m-executor.zst
        zstd -19 -q -c "$TMP/ad4m-executor" >"$dest/$file"
    else
        file=ad4m-executor.gz
        gzip -6 -c "$TMP/ad4m-executor" >"$dest/$file"
    fi
    (cd "$dest" && sha256sum "$file" >"$file.sha256")
    printf '{\n  "commit": "%s",\n  "platform": "linux-x64",\n  "file": "%s",\n  "sha256": "%s",\n  "glibc": "%s",\n  "size": %s\n}\n' \
        "$SHA" "$file" "$(cut -d' ' -f1 "$dest/$file.sha256")" "2.39" \
        "$(stat -c %s "$TMP/ad4m-executor")" >"$dest/manifest.json"
}

# Rewrites one field of the manifest.
manifest_set() {
    local m="$EXECUTOR_PUBLISH_DIR/$SHA/manifest.json"
    jq "$1" "$m" >"$m.new" && mv "$m.new" "$m"
}

test_executor_from_zst() {
    # The publish step falls back to gzip without zstd; so does this test.
    if ! command -v zstd >/dev/null; then
        echo "  skipped: zstd not installed"
        return
    fi
    publish zst
    out="$(bash "$SCRIPT" executor 2>&1)" || fail "executor step failed: $out"
    cmp -s "$TMP/ad4m-executor" "$COASYS_SPACES_WORK/ad4m-executor" || fail "the executor is not the published one"
    [ -x "$COASYS_SPACES_WORK/ad4m-executor" ] || fail "the executor is not executable"
    [ "$(cat "$COASYS_SPACES_WORK/executor.source" 2>/dev/null)" = published ] || fail "source is not 'published'"
}

test_executor_from_gz() {
    publish gz
    out="$(bash "$SCRIPT" executor 2>&1)" || fail "executor step failed: $out"
    cmp -s "$TMP/ad4m-executor" "$COASYS_SPACES_WORK/ad4m-executor" || fail "the executor is not the published one"
}

# $1: what the output must say. The step must fail (no build allowed) and leave no executor.
expect_refused() {
    out="$(bash "$SCRIPT" executor 2>&1)" && fail "executor step passed: $out"
    grep -qF -- "$1" <<<"$out" || fail "output does not say '$1': $out"
    grep -qF "COASYS_SPACES_NO_BUILD=1" <<<"$out" || fail "output does not say why it did not build: $out"
    [ ! -e "$COASYS_SPACES_WORK/ad4m-executor" ] || fail "an executor was left at $COASYS_SPACES_WORK/ad4m-executor"
}

test_executor_missing() {
    expect_refused "no published executor"
}

test_executor_checksum_mismatch() {
    publish zst
    manifest_set '.sha256 = "0000000000000000000000000000000000000000000000000000000000000000"'
    expect_refused "does not match the manifest's sha256"
}

test_executor_other_commit() {
    publish zst
    manifest_set '.commit = "ffffffffffffffffffffffffffffffffffffffff"'
    expect_refused "manifest names commit"
}

test_executor_size_mismatch() {
    publish zst
    manifest_set '.size = 1'
    expect_refused "the manifest says 1"
}

test_executor_unknown_file() {
    publish zst
    manifest_set '.file = "../../etc/passwd"'
    expect_refused "unknown file"
}

# A local repository stands in for WE; $WE_HEAD is its dev.
make_we() {
    export WE_REPO="$TMP/we.git"
    git init -q -b dev "$WE_REPO"
    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m one
    WE_HEAD="$(git -C "$WE_REPO" rev-parse HEAD)"
}

# $1: the expected exit status of preflight.
preflight_exits() {
    local rc=0
    out="$(bash "$SCRIPT" preflight 2>&1)" || rc=$?
    [ "$rc" = "$1" ] || fail "preflight exited $rc, not $1: $out"
}

test_preflight() {
    make_we
    mkdir -p "$COASYS_SPACES_ARTIFACTS" && touch "$COASYS_SPACES_ARTIFACTS/old.png"
    preflight_exits 0
    [ ! -e "$COASYS_SPACES_ARTIFACTS/old.png" ] || fail "preflight left the previous run's artifacts"

    # record notes the WE commit the clone had.
    mkdir -p "$COASYS_SPACES_WORK" && echo "$WE_HEAD" >"$COASYS_SPACES_WORK/we.sha"
    bash "$SCRIPT" record >/dev/null || fail "record failed"
    [ "$(cat "$COASYS_SPACES_STATE/last-success")" = "$SHA $WE_HEAD" ] || fail "record wrote the wrong pair"

    preflight_exits 10
    COASYS_SPACES_FORCE=1 preflight_exits 0
    AD4M_SHA=1111111111111111111111111111111111111111 preflight_exits 0
    WE_REF="$WE_HEAD" preflight_exits 10
    WE_REF=no-such-branch preflight_exits 0

    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m two
    preflight_exits 0
}

test_record_needs_we_sha() {
    bash "$SCRIPT" record >/dev/null 2>&1 && fail "record passed without a cloned WE commit"
    [ ! -e "$COASYS_SPACES_STATE/last-success" ] || fail "record wrote a pair without a WE commit"
}

# A model server on a free port that lists the given model ids at /v1/models.
serve_models() {
    node -e '
      const ids = process.argv.slice(1);
      const s = require("http").createServer((req, res) => {
        if (req.url !== "/v1/models") { res.writeHead(404); return res.end(); }
        res.setHeader("content-type", "application/json");
        res.end(JSON.stringify({ object: "list", data: ids.map((id) => ({ id, object: "model" })) }));
      });
      s.listen(0, "127.0.0.1", () => console.log(s.address().port));
    ' "$@" >"$TMP/port" &
    SERVER=$!
    for _ in $(seq 50); do [ -s "$TMP/port" ] && break; sleep 0.1; done
    COASYS_SPACES_LLM_BASE_URL="http://127.0.0.1:$(cat "$TMP/port")/v1"
    export COASYS_SPACES_LLM_BASE_URL
}

test_llm_check() {
    serve_models "llama3:8b" "gemma3:12b"
    COASYS_SPACES_LLM_MODEL=gemma3:12b bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "llm-check failed with the model served"
    out="$(COASYS_SPACES_LLM_MODEL=gemma3:27b bash "$SCRIPT" llm-check 2>&1)" && fail "llm-check passed without the model"
    grep -qF "ollama pull gemma3:27b" <<<"$out" || fail "a missing model does not say what to pull: $out"
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null
    out="$(bash "$SCRIPT" llm-check 2>&1)" && fail "llm-check passed with no server"
    grep -qF "does not answer" <<<"$out" || fail "no server is not reported as such: $out"
    COASYS_SPACES_TIER=stub bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "the stub tier needs no model server"
    unset COASYS_SPACES_LLM_BASE_URL
}

for t in test_executor_from_zst test_executor_from_gz test_executor_missing \
    test_executor_checksum_mismatch test_executor_other_commit test_executor_size_mismatch \
    test_executor_unknown_file test_preflight test_record_needs_we_sha test_llm_check; do
    echo "$t"
    setup
    "$t"
    rm -rf "$TMP"
done

if [[ "$FAILED" != 0 ]]; then
    echo "coasys-spaces tests: FAILED"
    exit 1
fi
echo "coasys-spaces tests: passed"
