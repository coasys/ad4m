#!/usr/bin/env bash
# Tests for the decisions in scripts/coasys-spaces.sh that need no build and no network: taking the
# executor from the publish dir (the layout the build job's "Publish the executor on this host"
# step writes), refusing one that does not match its manifest, skipping a commit pair that already
# passed, and the model server check. A fake executor, a local git repository as WE and a small
# Node server as the model server; about two seconds.
# Run: bash scripts/coasys-spaces.test.sh

set -uo pipefail

SOURCE="$(cd "$(dirname "$0")" && pwd)/coasys-spaces.sh"
FAILED=0
SHA=0123456789abcdef0123456789abcdef01234567

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# Fresh dirs under $TMP, and the script's environment pointed at them. The script runs from a
# clean git repository of its own ($TMP/ad4m), which stands in for the ad4m checkout.
setup() {
    TMP="$(mktemp -d)"
    mkdir -p "$TMP/ad4m/scripts"
    cp "$SOURCE" "$TMP/ad4m/scripts/"
    git init -q "$TMP/ad4m"
    git -C "$TMP/ad4m" add -A
    git -C "$TMP/ad4m" -c user.name=t -c user.email=t@t commit -q -m ad4m
    SCRIPT="$TMP/ad4m/scripts/coasys-spaces.sh"
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
    [ "$(cat "$COASYS_SPACES_STATE/last-success")" = "$SHA $WE_HEAD real qwen3.8-27b-48k ctx16384" ] ||
        fail "record wrote the wrong key: $(cat "$COASYS_SPACES_STATE/last-success")"

    preflight_exits 10
    COASYS_SPACES_FORCE=1 preflight_exits 0
    AD4M_SHA=1111111111111111111111111111111111111111 preflight_exits 0
    WE_REF="$WE_HEAD" preflight_exits 10
    WE_REF=no-such-branch preflight_exits 0
    # Another tier, model or context window is another run.
    COASYS_SPACES_TIER=stub preflight_exits 0
    COASYS_SPACES_LLM_MODEL=gemma3:27b preflight_exits 0
    COASYS_SPACES_LLM_NUM_CTX=8192 preflight_exits 0

    # An annotated tag resolves to the commit the clone checks out, so it skips too.
    git -C "$WE_REPO" -c user.name=t -c user.email=t@t tag -a -m v1 v1
    WE_REF=v1 preflight_exits 10

    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m two
    preflight_exits 0
}

# A checkout with changes runs, but neither skips nor records a pass of the commit.
test_dirty_checkout_is_not_recorded() {
    make_we
    mkdir -p "$COASYS_SPACES_WORK" && echo "$WE_HEAD" >"$COASYS_SPACES_WORK/we.sha"
    preflight_exits 0
    bash "$SCRIPT" record >/dev/null || fail "record failed"
    preflight_exits 10

    echo '# an edit' >>"$SCRIPT"
    preflight_exits 0
    grep -qF "will not be recorded" <<<"$out" || fail "preflight does not say the run is not recorded: $out"
    rm "$COASYS_SPACES_STATE/last-success"
    out="$(bash "$SCRIPT" record 2>&1)" || fail "record failed on a dirty checkout: $out"
    [ ! -e "$COASYS_SPACES_STATE/last-success" ] || fail "a dirty checkout was recorded as a pass"

    git -C "$TMP/ad4m" checkout -q -- scripts/coasys-spaces.sh
    touch "$TMP/ad4m/untracked"
    preflight_exits 0
    grep -qF "will not be recorded" <<<"$out" || fail "an untracked file does not count as a change: $out"
}

test_record_needs_we_sha() {
    bash "$SCRIPT" record >/dev/null 2>&1 && fail "record passed without a cloned WE commit"
    [ ! -e "$COASYS_SPACES_STATE/last-success" ] || fail "record wrote a pair without a WE commit"
}

test_we_ref_must_be_a_name() {
    make_we
    for ref in --upload-pack=touch "dev x" 'dev;true'; do
        out="$(WE_REF="$ref" bash "$SCRIPT" preflight 2>&1)" && fail "preflight took WE_REF '$ref'"
        grep -qF "is not a branch, tag or commit name" <<<"$out" || fail "WE_REF '$ref' not refused as such: $out"
    done
    WE_REF=release/v0.12.0-rc.1 preflight_exits 0
}

# The SDK is packed from the checkout, so it refuses an AD4M_SHA the checkout is not at.
test_sdk_needs_the_checkout_at_ad4m_sha() {
    out="$(bash "$SCRIPT" sdk 2>&1)" && fail "sdk passed with AD4M_SHA $SHA"
    grep -qF "not AD4M_SHA $SHA" <<<"$out" || fail "the mismatch is not reported: $out"
}

# A WE without the suite fails the test step; pnpm --filter would run nothing and pass.
test_test_needs_the_suite() {
    mkdir -p "$COASYS_SPACES_WORK/we/apps/we-web"
    : >"$COASYS_SPACES_WORK/ad4m-executor" && chmod 755 "$COASYS_SPACES_WORK/ad4m-executor"
    for pkg in '{"name":"@we/app-web","scripts":{}}' '{"name":"@we/web","scripts":{"test:spaces":"x"}}'; do
        echo "$pkg" >"$COASYS_SPACES_WORK/we/apps/we-web/package.json"
        out="$(bash "$SCRIPT" test 2>&1)" && fail "test passed with $pkg"
        grep -qF "has no @we/app-web with a test:spaces script" <<<"$out" || fail "not reported for $pkg: $out"
    done
}

# A model server on a free port that lists the given model ids at /v1/models. With OLLAMA=1 it
# also answers Ollama's /api/version, /api/show (num_ctx $OWN_CTX when set) and /api/create, and
# writes each create request to $TMP/created.
serve_models() {
    OLLAMA="${OLLAMA:-}" OWN_CTX="${OWN_CTX:-}" CREATED="$TMP/created" node -e '
      const ids = process.argv.slice(1);
      const s = require("http").createServer((req, res) => {
        res.setHeader("content-type", "application/json");
        if (req.url === "/v1/models") {
          return res.end(JSON.stringify({ object: "list", data: ids.map((id) => ({ id, object: "model" })) }));
        }
        if (process.env.OLLAMA && req.url === "/api/version") return res.end(JSON.stringify({ version: "0.12.0" }));
        if (process.env.OLLAMA && req.url === "/api/show" && req.method === "POST") {
          const p = process.env.OWN_CTX ? "num_ctx                        " + process.env.OWN_CTX + "\ntemperature 0.6" : "temperature 0.6";
          return res.end(JSON.stringify({ parameters: p }));
        }
        if (process.env.OLLAMA && req.url === "/api/create" && req.method === "POST") {
          let body = "";
          req.on("data", (c) => (body += c));
          return req.on("end", () => {
            require("fs").appendFileSync(process.env.CREATED, body + "\n");
            res.end(JSON.stringify({ status: "success" }));
          });
        }
        res.writeHead(404);
        res.end();
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
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null
    # Ollama lists a model with its tag: a name without one is its `:latest`, and only that.
    serve_models "qwen3.8-27b-48k:latest" "gemma3:12b"
    COASYS_SPACES_LLM_MODEL=qwen3.8-27b-48k bash "$SCRIPT" llm-check >/dev/null 2>&1 ||
        fail "llm-check did not take qwen3.8-27b-48k:latest for qwen3.8-27b-48k"
    COASYS_SPACES_LLM_MODEL=gemma3 bash "$SCRIPT" llm-check >/dev/null 2>&1 &&
        fail "llm-check took gemma3:12b for gemma3 (gemma3:latest)"
    out="$(COASYS_SPACES_LLM_MODEL=gemma3:27b bash "$SCRIPT" llm-check 2>&1)" && fail "llm-check passed without the model"
    grep -qF "ollama pull gemma3:27b" <<<"$out" || fail "a missing model does not say what to pull: $out"
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null
    out="$(bash "$SCRIPT" llm-check 2>&1)" && fail "llm-check passed with no server"
    grep -qF "does not answer" <<<"$out" || fail "no server is not reported as such: $out"
    COASYS_SPACES_TIER=stub bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "the stub tier needs no model server"
    unset COASYS_SPACES_LLM_BASE_URL
}

# On Ollama, llm-check makes a variant of the model with a larger context window, and names it for
# `test`; on another server the model stays as it is.
test_llm_context_window() {
    OLLAMA=1 serve_models "gemma3:12b"
    COASYS_SPACES_LLM_MODEL=gemma3:12b bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "llm-check failed on Ollama"
    jq -e '.model == "coasys-spaces-gemma3-12b:ctx16384" and .from == "gemma3:12b" and .parameters.num_ctx == 16384' \
        "$TMP/created" >/dev/null || fail "the variant was not asked for as expected: $(cat "$TMP/created" 2>/dev/null)"
    [ "$(cat "$COASYS_SPACES_WORK/llm-model" 2>/dev/null)" = "coasys-spaces-gemma3-12b:ctx16384" ] ||
        fail "test is not told to use the variant"
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null

    # A model whose own num_ctx is large enough runs as it is: no second load on the GPU.
    rm -f "$TMP/created"
    OLLAMA=1 OWN_CTX=49152 serve_models "qwen3.8-27b-48k"
    COASYS_SPACES_LLM_MODEL=qwen3.8-27b-48k bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "llm-check failed on a 48k model"
    [ ! -e "$TMP/created" ] || fail "a variant was made of a model with a 48k context: $(cat "$TMP/created")"
    [ ! -e "$COASYS_SPACES_WORK/llm-model" ] || fail "test is told to use a variant of a 48k model"
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null

    serve_models "gemma3:12b"
    COASYS_SPACES_LLM_MODEL=gemma3:12b bash "$SCRIPT" llm-check >/dev/null 2>&1 || fail "llm-check failed on a plain server"
    [ ! -e "$COASYS_SPACES_WORK/llm-model" ] || fail "a plain server's model was replaced (stale variant name kept)"
    kill "$SERVER"
    wait "$SERVER" 2>/dev/null
    unset COASYS_SPACES_LLM_BASE_URL
}

for t in test_executor_from_zst test_executor_from_gz test_executor_missing \
    test_executor_checksum_mismatch test_executor_other_commit test_executor_size_mismatch \
    test_executor_unknown_file test_preflight test_dirty_checkout_is_not_recorded test_record_needs_we_sha test_we_ref_must_be_a_name \
    test_sdk_needs_the_checkout_at_ad4m_sha test_test_needs_the_suite test_llm_check \
    test_llm_context_window; do
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
