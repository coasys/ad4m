#!/usr/bin/env bash
# Tests for .circleci/scripts/paths-changed.sh. Scratch git repos (a bare one is
# `origin`), no network. The skip list is the real one: read from
# integration-tests-sfu-e2e's `skip_paths` default in .circleci/config.yml, so a
# change to that list is tested here. About 6 seconds.
# Run: bash .circleci/scripts/paths-changed.test.sh

set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$HERE/paths-changed.sh"
HEAVY="$HERE/should-run-heavy.sh"
CONFIG="$HERE/../config.yml"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail() { echo "  FAIL: $*"; FAILED=1; }

# paths-changed.sh bounds its fetch with GNU `timeout` (not on stock macOS).
if ! command -v timeout >/dev/null 2>&1; then
  echo "skip: GNU timeout is not installed (macOS: brew install coreutils); paths-changed.sh needs it"
  exit 0
fi

# The `default: >-` block of `skip_paths:` under the job integration-tests-sfu-e2e.
SKIP=()
read -ra SKIP <<<"$(awk '
  /^  integration-tests-sfu-e2e:$/ { job = 1; next }
  job && /^  [^ ]/ { exit }
  job && /^      skip_paths:$/ { param = 1; next }
  param && /^        default: >-$/ { block = 1; next }
  block && /^          [^ ]/ { printf "%s ", $0; next }
  block { exit }
' "$CONFIG")"

echo "paths-changed.sh"
if [ "${#SKIP[@]}" -lt 10 ]; then
  fail "read ${#SKIP[@]} skip_paths entries from $CONFIG (want the job's list): ${SKIP[*]}"
else
  echo "  ok: ${#SKIP[@]} skip_paths entries from config.yml"
fi
# The job must hand that list to the script, unglobbed.
grep -qF 'bash .circleci/scripts/paths-changed.sh << parameters.skip_paths >>' "$CONFIG" \
  || fail "config.yml does not pass << parameters.skip_paths >> to paths-changed.sh"

G=(git -c user.name=t -c user.email=t@example.org -c commit.gpgsign=false -c init.defaultBranch=dev)

# origin: a bare repository with dev. repo: a clone, on PR branches.
"${G[@]}" init -q --bare "$TMP/origin.git"
"${G[@]}" clone -q "$TMP/origin.git" "$TMP/repo" 2>/dev/null
cd "$TMP/repo" || exit 1
mkdir -p rust-executor/src/sfu docs
echo 'mod room;' > rust-executor/src/sfu/mod.rs
echo 'page' > docs/index.md
"${G[@]}" add -A
"${G[@]}" commit -qm "feat: first"
"${G[@]}" push -q origin dev 2>/dev/null

# change <path> <text> <message>: a new commit that changes one file.
change() {
  mkdir -p "$(dirname "$1")"
  echo "$2" >> "$1"
  "${G[@]}" add -A
  "${G[@]}" commit -qm "$3"
}

# check <name> <expected exit> <expected output pattern> [VAR=value ...]
# Runs the script with the job's skip list in a clean environment.
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  local out rc=0
  out="$(env -i PATH="$PATH" HOME="$TMP" \
    "$@" bash "$SCRIPT" "${SKIP[@]}" 2>&1)" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    fail "$name: exit $rc (want $want_rc), output: $out"
  else
    echo "  ok: $name -> $(head -1 <<<"$out")"
  fi
}

# pr <want: run|skip> <path>...: a fresh PR branch off dev that changes exactly
# these files, checked against the job's skip list.
pr() {
  local want="$1" f
  shift
  "${G[@]}" checkout -q -B case dev
  for f in "$@"; do change "$f" "x" "change $f"; done
  if [ "$want" = run ]; then
    check "runs: $*" 0 '^run: ' CIRCLE_BRANCH=case
  else
    check "skips: $*" 10 '^skip: ' CIRCLE_BRANCH=case
  fi
}

# Branches and overrides, before any diff is read.
check "push to dev runs (the 2-hourly tier)" 0 '^run: push to dev' CIRCLE_BRANCH=dev
check "push to staging runs" 0 '^run: push to staging' CIRCLE_BRANCH=staging
check "push to main runs" 0 '^run: push to main' CIRCLE_BRANCH=main
check "tag runs" 0 '^run: tag v1' CIRCLE_TAG=v1
check "no branch runs" 0 '^run: no CIRCLE_BRANCH'
check "heavy_gate's full-ci decision (CI_FULL_CI) runs" 0 '^run: full-ci: PR #7 has the full-ci label' \
  CIRCLE_BRANCH=feat/x CI_FULL_CI='PR #7 has the full-ci label'

# Every entry of the list skips on its own (one sample file per entry).
for p in "${SKIP[@]}"; do
  case "$p" in
    '!'*) continue ;;
    */) f="${p}sample.txt" ;;
    *) f="${p//\*/sample}" ;;
  esac
  pr skip "$f"
done

# Lal's review of round 1: changes that alter the suite's behaviour must run it.
# The executor, the SDK, the Rust build inputs, the test harness.
for f in rust-executor/src/lib.rs rust-executor/src/api/ws_handler.rs \
  rust-executor/src/api/events_ws.rs rust-executor/src/events.rs rust-executor/src/pubsub.rs \
  rust-executor/src/config.rs rust-executor/src/agent/mod.rs core/src/apiClient.ts \
  Cargo.toml Cargo.lock rust-toolchain.toml .cargo/config.toml rust-executor/Cargo.toml \
  ad4m-hash-chain/src/lib.rs ad4m-identity-core/src/lib.rs ad4m-ldk/rust/src/lib.rs rust-client/src/lib.rs \
  cli/src/ad4m_executor.rs tests/js/utils/utils.ts tests/js/helpers/ports.ts \
  tests/js/scripts/prepareTestDirectory.sh pnpm-lock.yaml; do
  pr run "$f"
done
# The SFU, its suite and CI itself.
for f in rust-executor/src/sfu/mod.rs rust-executor/src/api/sfu_ws.rs core/src/neighbourhood/SfuManager.ts \
  tests/js/tests/sfu-e2e.test.ts tests/js/helpers/sfu/harness.ts tests/js/package.json \
  .circleci/config.yml .circleci/scripts/paths-changed.sh; do
  pr run "$f"
done
# Files inside a skipped directory that the build still reads: workspace and
# crate manifests (they change what pnpm install and cargo resolve), the
# executor's embedded files and Markdown below the top level (the executor
# include_str!s rust-executor/src/mcp/docs/*.md).
for f in ui/package.json ui/src-tauri/Cargo.toml ui/src-tauri/build.rs docs-src/package.json \
  ad4m-hooks/react/package.json ai-wind-tunnel/recommendations.json \
  rust-executor/src/mcp/docs/overview.md bootstrap-languages/agent-language/index.ts \
  dapp/src/main.ts link-server/src/index.ts connect/src/core.ts; do
  pr run "$f"
done
# Unknown and new paths run.
pr run brand-new-dir/thing.rs
pr run tests/js/new-helper.ts
pr run tests/js/sdna/subject.pl
# Several skipped files together still skip; one other file among them runs.
pr skip README.md docs/index.md ui/src/App.tsx tests/js/tests/mcp-http.test.ts .github/workflows/x.yml
pr run README.md docs/index.md tests/js/tests/mcp-http.test.ts rust-executor/src/pubsub.rs
# A file moved from SFU code into docs/: the deletion counts (no rename detection).
"${G[@]}" checkout -q -B case dev
"${G[@]}" mv rust-executor/src/sfu/mod.rs docs/sfu-mod.rs
"${G[@]}" commit -qm "move"
check "a move out of SFU code into docs/ runs" 0 '^run: .*rust-executor/src/sfu/mod.rs' CIRCLE_BRANCH=case

# The full-ci label, decided once by heavy_gate: a labelled PR that touches only
# skipped paths runs. heavy_gate (should-run-heavy.sh) writes its decision to
# $BASH_ENV; the next step (this script) reads it from there.
mkdir -p "$TMP/bin"
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
printf '%s' "$FAKE_BODY"
EOF
chmod +x "$TMP/bin/curl"
"${G[@]}" checkout -q -B labelled dev
change docs/index.md "label" "docs: labelled"
: > "$TMP/bash_env"
for label in "" full-ci; do
  : > "$TMP/bash_env"
  labels='[]'
  [ -n "$label" ] && labels="[{\"name\":\"$label\"}]"
  body="[{\"number\":7,\"state\":\"open\",\"draft\":false,\"base\":{\"ref\":\"dev\"},\"labels\":$labels}]"
  env -i PATH="$TMP/bin:$PATH" HOME="$TMP" CIRCLE_BRANCH=labelled HEAVY_GATE_COMMIT_MSG="docs: x" \
    HEAVY_GATE_CACHE_DIR="$TMP/cache" HEAVY_GATE_EXPORT="$TMP/bash_env" FAKE_BODY="$body" \
    bash "$HEAVY" >/dev/null 2>&1
  out="$(env -i PATH="$PATH" HOME="$TMP" BASH_ENV="$TMP/bash_env" CIRCLE_BRANCH=labelled \
    bash "$SCRIPT" "${SKIP[@]}" 2>&1)"; rc=$?
  if [ -n "$label" ]; then
    if [ "$rc" = 0 ] && grep -q '^run: full-ci: PR #7 has the full-ci label' <<<"$out"; then
      echo "  ok: full-ci-labelled PR touching only skipped paths runs -> $out"
    else fail "full-ci-labelled PR touching only skipped paths: exit $rc, output: $out"; fi
  else
    if [ "$rc" = 10 ]; then echo "  ok: the same PR without the label skips -> $(head -1 <<<"$out")"
    else fail "unlabelled docs-only PR: exit $rc, output: $out"; fi
  fi
done

# Only the PR's own changes count: dev moves on (SFU change on dev), the PR merges
# dev in, and the PR itself touches only skipped paths.
"${G[@]}" checkout -q -B feat/y dev
change docs/index.md "y" "docs: y"
"${G[@]}" checkout -q dev
change rust-executor/src/sfu/mod.rs "mod cascade;" "feat: sfu cascade on dev"
"${G[@]}" push -q origin dev 2>/dev/null
"${G[@]}" checkout -q feat/y
"${G[@]}" merge -q --no-edit dev 2>/dev/null
check "merge of dev that brings SFU changes, PR touches only docs: skips" 10 '^skip:' CIRCLE_BRANCH=feat/y
check "skip names the base it compared with" 10 'origin/dev' CIRCLE_BRANCH=feat/y

# A PR with no changes of its own (only merges of dev) skips.
"${G[@]}" checkout -q -B empty dev
check "no changes of its own skips" 10 '^skip: no changes' CIRCLE_BRANCH=empty

# The script fetches dev itself: a stale local origin/dev must not count dev's
# own changes as the PR's.
"${G[@]}" checkout -q dev
change docs/index.md "z" "docs: z"
"${G[@]}" checkout -q -b feat/z
change docs/index.md "z2" "docs: z2"
"${G[@]}" checkout -q dev
change rust-executor/src/sfu/mod.rs "mod pipe;" "feat: sfu pipe on dev"
"${G[@]}" push -q origin dev 2>/dev/null
"${G[@]}" update-ref refs/remotes/origin/dev HEAD~2
"${G[@]}" checkout -q feat/z
"${G[@]}" merge -q --no-edit dev 2>/dev/null
check "fetches dev before the diff" 10 '^skip:' CIRCLE_BRANCH=feat/z

# Fails open.
check "fetch failure runs" 0 '^run: could not fetch' CIRCLE_BRANCH=feat/z PATHS_GATE_REMOTE=nowhere
check "unknown base runs" 0 '^run: could not fetch' CIRCLE_BRANCH=feat/z PATHS_GATE_BASE=no-such-branch
"${G[@]}" checkout -q --orphan unrelated
"${G[@]}" rm -rqf . >/dev/null
echo x > x
"${G[@]}" add x
"${G[@]}" commit -qm "unrelated"
check "no merge base runs" 0 '^run: no merge base' CIRCLE_BRANCH=unrelated
out="$(cd "$TMP" && env -i PATH="$PATH" HOME="$TMP" CIRCLE_BRANCH=feat/x bash "$SCRIPT" docs/ 2>&1)"; rc=$?
if [ "$rc" = 0 ] && grep -q '^run: ' <<<"$out"; then echo "  ok: outside a git repository runs -> $out"
else fail "outside a git repository: exit $rc, output: $out"; fi

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
