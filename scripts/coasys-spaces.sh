#!/usr/bin/env bash
# The coasys-spaces product suite: WE's `test:spaces` (apps/we-web/tests/live/coasys-spaces/)
# against the executor and the SDK of one ad4m commit. The nightly job `coasys-spaces` in
# .circleci/config.yml runs one step per subcommand; `all` runs them in order, for a run by hand.
#
#   scripts/coasys-spaces.sh preflight   exit 10 when this ad4m commit and this WE commit already
#                                        passed (the job then halts green); clears the artifacts
#   scripts/coasys-spaces.sh executor    the executor for the commit: the published one, or a build
#   scripts/coasys-spaces.sh sdk         build core and connect, and pack them
#   scripts/coasys-spaces.sh we          clone WE at WE_REF, override its ad4m pin with the packed
#                                        SDK, install with WE's pnpm, install Chromium
#   scripts/coasys-spaces.sh llm-check   the model server answers and has the model (tier `real`)
#   scripts/coasys-spaces.sh build-web   build we-web with the Coasys seed
#   scripts/coasys-spaces.sh test        run test:spaces, log into the artifacts
#   scripts/coasys-spaces.sh record      note this commit pair as passed (for preflight)
#   scripts/coasys-spaces.sh all         every step above, preflight forced
#
# Environment (all optional):
#   AD4M_SHA                    the ad4m commit (CIRCLE_SHA1, else HEAD)
#   WE_REPO, WE_REF             WE's repository and the branch, tag or full commit (dev)
#   COASYS_SPACES_WORK          work dir: WE clone, packed SDK, executor, WE's pnpm
#                               (~/.cache/ad4m-ci/coasys-spaces). Outside the ad4m checkout, so
#                               WE's modules never resolve from ad4m's node_modules; not /tmp,
#                               which is a shared tmpfs on the CI host
#   COASYS_SPACES_ARTIFACTS     screenshots, traces and logs (<work>/artifacts); preflight empties it
#   COASYS_SPACES_STATE         the last passed commit pair (~/.local/state/ad4m-ci/coasys-spaces)
#   COASYS_SPACES_FORCE=1       preflight never skips
#   COASYS_SPACES_NO_BUILD=1    fail instead of building an executor that is not published
#   EXECUTOR_PUBLISH_DIR        the published executors (/mnt/hdd/executor-artifacts), the layout
#                               the build job's "Publish the executor on this host" step writes
#   COASYS_SPACES_TIER          real (host model, Whisper) or stub; default real
#   COASYS_SPACES_LLM_BASE_URL  OpenAI-compatible base URL (http://localhost:11434/v1)
#   COASYS_SPACES_LLM_MODEL     model id (gemma3:12b)
#   SOCIAL_DNA_PACKAGE          passed to the suite when set
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
AD4M_SHA="${AD4M_SHA:-${CIRCLE_SHA1:-$(git -C "$REPO" rev-parse HEAD)}}"
WE_REPO="${WE_REPO:-https://github.com/coasys/we.git}"
WE_REF="${WE_REF:-dev}"
WORK="${COASYS_SPACES_WORK:-${XDG_CACHE_HOME:-$HOME/.cache}/ad4m-ci/coasys-spaces}"
ARTIFACTS="${COASYS_SPACES_ARTIFACTS:-$WORK/artifacts}"
STATE="${COASYS_SPACES_STATE:-${XDG_STATE_HOME:-$HOME/.local/state}/ad4m-ci/coasys-spaces}"
PUBLISH_DIR="${EXECUTOR_PUBLISH_DIR:-/mnt/hdd/executor-artifacts}"
TIER="${COASYS_SPACES_TIER:-real}"
LLM_BASE_URL="${COASYS_SPACES_LLM_BASE_URL:-http://localhost:11434/v1}"
LLM_MODEL="${COASYS_SPACES_LLM_MODEL:-gemma3:12b}"

WE_DIR="$WORK/we"
SDK_DIR="$WORK/sdk"
EXECUTOR="$WORK/ad4m-executor"
WE_PACKAGE="@we/app-web"
SKIP=10

log() { echo "coasys-spaces: $*"; }
die() {
    echo "coasys-spaces: ERROR: $*" >&2
    exit 1
}

is_sha() { printf '%s' "$1" | grep -Eq '^[0-9a-f]{40}$'; }

# The commit WE_REF names on WE_REPO now, without a clone.
we_remote_sha() {
    if is_sha "$WE_REF"; then
        echo "$WE_REF"
        return
    fi
    git ls-remote "$WE_REPO" "refs/heads/$WE_REF" "refs/tags/$WE_REF" | awk 'NR == 1 {print $1}'
}

cmd_preflight() {
    rm -rf "$ARTIFACTS"
    mkdir -p "$ARTIFACTS"
    local we_sha last
    we_sha="$(we_remote_sha || true)"
    if [ -z "$we_sha" ]; then
        log "cannot resolve WE ref '$WE_REF' on $WE_REPO; running"
        return 0
    fi
    if [ "${COASYS_SPACES_FORCE:-}" = 1 ]; then
        log "COASYS_SPACES_FORCE=1; running ad4m $AD4M_SHA with WE $we_sha"
        return 0
    fi
    last="$(cat "$STATE/last-success" 2>/dev/null || true)"
    if [ "$last" = "$AD4M_SHA $we_sha" ]; then
        log "ad4m $AD4M_SHA with WE $we_sha already passed; skipping"
        return "$SKIP"
    fi
    log "ad4m $AD4M_SHA with WE $we_sha (last passed: ${last:-none}); running"
}

# Copies the executor published for AD4M_SHA to $EXECUTOR. Returns 1, with the reason, when there
# is none or it does not match its manifest.
fetch_published_executor() {
    local dir="$PUBLISH_DIR/$AD4M_SHA" manifest file sum size commit tmp
    manifest="$dir/manifest.json"
    if [ ! -r "$manifest" ]; then
        log "no published executor at $manifest"
        return 1
    fi
    commit="$(jq -r '.commit // ""' "$manifest")"
    file="$(jq -r '.file // ""' "$manifest")"
    sum="$(jq -r '.sha256 // ""' "$manifest")"
    size="$(jq -r '.size // ""' "$manifest")"
    if [ "$commit" != "$AD4M_SHA" ]; then
        log "manifest names commit '$commit', not $AD4M_SHA"
        return 1
    fi
    case "$file" in
        ad4m-executor.zst | ad4m-executor.gz) ;;
        *)
            log "manifest names an unknown file '$file'"
            return 1
            ;;
    esac
    if [ ! -r "$dir/$file" ]; then
        log "manifest names $file, which is not in $dir"
        return 1
    fi
    if ! echo "$sum  $dir/$file" | sha256sum -c --status -; then
        log "$dir/$file does not match the manifest's sha256 $sum"
        return 1
    fi
    tmp="$EXECUTOR.tmp"
    case "$file" in
        *.zst) zstd -d -q -c "$dir/$file" >"$tmp" ;;
        *.gz) gzip -d -c "$dir/$file" >"$tmp" ;;
    esac || {
        log "cannot decompress $dir/$file"
        rm -f "$tmp"
        return 1
    }
    if [ -n "$size" ] && [ "$(stat -c %s "$tmp")" != "$size" ]; then
        log "decompressed executor is $(stat -c %s "$tmp") bytes, the manifest says $size"
        rm -f "$tmp"
        return 1
    fi
    chmod 755 "$tmp"
    mv "$tmp" "$EXECUTOR"
    log "executor from $dir (glibc $(jq -r '.glibc // "?"' "$manifest"))"
}

# The executor half of the config's `build_adam`: dapp, Deno snapshot, executor. No Holochain CLIs
# and no bootstrap languages: the executor does not need them, and the publish step publishes
# exactly this binary.
build_executor() {
    log "building the executor (pnpm run build-libs)"
    cd "$REPO"
    pnpm install --no-frozen-lockfile
    pnpm build-dapp
    (cd core && pnpm install --no-frozen-lockfile)
    rm -f target/release/ad4m target/release/ad4m-executor
    CI_ARTIFACT_DIR="${CI_ARTIFACT_DIR:-/var/lib/ci-artifacts}" pnpm run build-libs
    test -s target/release/ad4m-executor || die "build-libs made no target/release/ad4m-executor"
    cp target/release/ad4m-executor "$EXECUTOR.tmp"
    mv "$EXECUTOR.tmp" "$EXECUTOR"
}

cmd_executor() {
    is_sha "$AD4M_SHA" || die "AD4M_SHA '$AD4M_SHA' is not 40 hex"
    mkdir -p "$WORK"
    rm -f "$EXECUTOR" "$EXECUTOR.tmp"
    if fetch_published_executor; then
        echo published >"$WORK/executor.source"
    elif [ "${COASYS_SPACES_NO_BUILD:-}" = 1 ]; then
        die "no usable published executor for $AD4M_SHA, and COASYS_SPACES_NO_BUILD=1"
    else
        build_executor
        echo built >"$WORK/executor.source"
    fi
    ls -l "$EXECUTOR"
}

cmd_sdk() {
    cd "$REPO"
    pnpm install --no-frozen-lockfile
    (cd core && pnpm install --no-frozen-lockfile)
    pnpm --filter @coasys/ad4m run build
    pnpm --filter @coasys/ad4m-connect run build
    rm -rf "$SDK_DIR"
    mkdir -p "$SDK_DIR"
    # pnpm pack writes connect's `workspace:` range on @coasys/ad4m as the version; WE's override
    # sends that to the core tarball as well.
    (cd core && pnpm pack --pack-destination "$SDK_DIR")
    (cd connect && pnpm pack --pack-destination "$SDK_DIR")
    sdk_tarball ad4m >/dev/null
    sdk_tarball ad4m-connect >/dev/null
    ls -l "$SDK_DIR"
}

# The packed tarball of @coasys/<name>.
sdk_tarball() {
    local found
    found="$(find "$SDK_DIR" -maxdepth 1 -regextype posix-extended \
        -regex ".*/coasys-$1-[0-9][^/]*\\.tgz" -print | head -1)"
    [ -n "$found" ] || die "no packed @coasys/$1 in $SDK_DIR (run the sdk step)"
    echo "$found"
}

# WE's own pnpm (its packageManager), first on PATH so that scripts calling `pnpm` get it too.
# Versions are read in WE's dir: pnpm 10 runs the version the current dir's packageManager names,
# so in the ad4m checkout it would report ad4m's 9.15.0.
use_we_pnpm() {
    local want bin="$WORK/pnpm/node_modules/.bin"
    want="$(node -p "(require('$WE_DIR/package.json').packageManager || '').replace(/^pnpm@/, '').split('+')[0]")"
    [ -n "$want" ] || die "WE's package.json names no packageManager"
    if [ "$(cd "$WE_DIR" && "$bin/pnpm" --version 2>/dev/null || true)" != "$want" ]; then
        log "installing pnpm $want into $WORK/pnpm"
        rm -rf "$WORK/pnpm"
        npm install --silent --no-audit --no-fund --prefix "$WORK/pnpm" "pnpm@$want"
    fi
    export PATH="$bin:$PATH"
    local have
    have="$(cd "$WE_DIR" && pnpm --version)"
    [ "$have" = "$want" ] || die "pnpm in $WE_DIR is $have, not $want"
}

cmd_we() {
    local core connect
    core="$(sdk_tarball ad4m)"
    connect="$(sdk_tarball ad4m-connect)"
    rm -rf "$WE_DIR"
    mkdir -p "$WE_DIR"
    git -C "$WE_DIR" init -q
    git -C "$WE_DIR" remote add origin "$WE_REPO"
    git -C "$WE_DIR" fetch -q --depth 1 origin "$WE_REF"
    git -C "$WE_DIR" checkout -q FETCH_HEAD
    git -C "$WE_DIR" rev-parse HEAD >"$WORK/we.sha"
    log "WE $WE_REF at $(cat "$WORK/we.sha")"

    # The pin is the root pnpm.overrides (WE scripts/ad4m-pin.mjs). Pointing both at this commit's
    # tarballs makes the client and the executor one commit.
    CORE_TGZ="$core" CONNECT_TGZ="$connect" node -e '
      const fs = require("fs");
      const file = process.argv[1];
      const pkg = JSON.parse(fs.readFileSync(file, "utf8"));
      pkg.pnpm = pkg.pnpm || {};
      pkg.pnpm.overrides = pkg.pnpm.overrides || {};
      pkg.pnpm.overrides["@coasys/ad4m"] = "file:" + process.env.CORE_TGZ;
      pkg.pnpm.overrides["@coasys/ad4m-connect"] = "file:" + process.env.CONNECT_TGZ;
      fs.writeFileSync(file, JSON.stringify(pkg, null, 2) + "\n");
    ' "$WE_DIR/package.json"

    use_we_pnpm
    cd "$WE_DIR"
    pnpm install --no-frozen-lockfile
    check_override ad4m "$core"
    check_override ad4m-connect "$connect"
    PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=1 pnpm --filter "$WE_PACKAGE" exec playwright-core install chromium
}

# Fails unless we-web resolves @coasys/<name> to the packed tarball.
check_override() {
    local resolved
    # Not require.resolve: connect's exports do not list ./package.json.
    resolved="$(realpath "$WE_DIR/apps/we-web/node_modules/@coasys/$1")" ||
        die "we-web has no @coasys/$1"
    case "$resolved" in
        */node_modules/.pnpm/@coasys+$1@file+*) ;;
        *) die "we-web resolves @coasys/$1 to $resolved, not the packed tarball" ;;
    esac
    # Every file, not only the version: an SDK change between two commits often keeps it.
    rm -rf "$WORK/unpacked"
    mkdir -p "$WORK/unpacked"
    tar -xzf "$2" -C "$WORK/unpacked"
    diff -rq "$WORK/unpacked/package" "$resolved" ||
        die "we-web's @coasys/$1 ($resolved) differs from $2"
    rm -rf "$WORK/unpacked"
    log "we-web uses @coasys/$1 from $(basename "$2")"
}

cmd_llm_check() {
    if [ "$TIER" != real ]; then
        log "tier $TIER: no model server needed"
        return 0
    fi
    local models
    if ! models="$(curl -sf --max-time 15 "$LLM_BASE_URL/models")"; then
        die "the model server at $LLM_BASE_URL does not answer (GET /models). On the runner: systemctl status ollama"
    fi
    if ! jq -e --arg m "$LLM_MODEL" '[.data[]?.id] | index($m)' >/dev/null <<<"$models"; then
        die "the model server at $LLM_BASE_URL has no model '$LLM_MODEL' (it has: $(jq -r '[.data[]?.id] | join(", ")' <<<"$models")). On the runner: ollama pull $LLM_MODEL"
    fi
    log "$LLM_BASE_URL serves $LLM_MODEL"
}

cmd_build_web() {
    use_we_pnpm
    cd "$WE_DIR"
    WE_SEED=seeds/coasys.seed.json pnpm build:web
}

cmd_test() {
    [ -x "$EXECUTOR" ] || die "no executor at $EXECUTOR (run the executor step)"
    use_we_pnpm
    mkdir -p "$ARTIFACTS"
    {
        echo "ad4m $AD4M_SHA"
        echo "we $(cat "$WORK/we.sha" 2>/dev/null || echo "?") ($WE_REF)"
        echo "executor $(cat "$WORK/executor.source" 2>/dev/null || echo "?")"
        echo "tier $TIER"
        [ "$TIER" = real ] && echo "model $LLM_MODEL at $LLM_BASE_URL"
        echo "social dna ${SOCIAL_DNA_PACKAGE:-fixture}"
    } | tee "$ARTIFACTS/run.txt"
    cd "$WE_DIR"
    AD4M_EXECUTOR="$EXECUTOR" \
        COASYS_SPACES_TIER="$TIER" \
        COASYS_SPACES_LLM_BASE_URL="$LLM_BASE_URL" \
        COASYS_SPACES_LLM_MODEL="$LLM_MODEL" \
        COASYS_SPACES_ARTIFACTS="$ARTIFACTS" \
        pnpm --filter "$WE_PACKAGE" test:spaces 2>&1 | tee "$ARTIFACTS/test-spaces.log"
}

cmd_record() {
    local we_sha
    we_sha="$(cat "$WORK/we.sha")"
    is_sha "$we_sha" || die "no tested WE commit in $WORK/we.sha"
    mkdir -p "$STATE"
    echo "$AD4M_SHA $we_sha" >"$STATE/last-success.tmp"
    mv "$STATE/last-success.tmp" "$STATE/last-success"
    log "recorded ad4m $AD4M_SHA with WE $we_sha as passed"
}

case "${1:-}" in
    preflight) cmd_preflight ;;
    executor) cmd_executor ;;
    sdk) cmd_sdk ;;
    we) cmd_we ;;
    llm-check) cmd_llm_check ;;
    build-web) cmd_build_web ;;
    test) cmd_test ;;
    record) cmd_record ;;
    all)
        COASYS_SPACES_FORCE=1 cmd_preflight
        (cmd_executor)
        (cmd_sdk)
        (cmd_we)
        (cmd_llm_check)
        (cmd_build_web)
        (cmd_test)
        cmd_record
        ;;
    *)
        awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0" >&2
        exit 2
        ;;
esac
