#!/usr/bin/env bash
# Tests for .circleci/scripts/dev-tier.sh. A scratch git repo stands in for dev; a
# fake `curl` on PATH answers CircleCI and GitHub from fixture files and records
# every request, so they need no network. About a second.
# Run: bash .circleci/scripts/dev-tier.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/dev-tier.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

CIRCLE="https://circle.example/api/v1.1/project/p"
GH="https://gh.example"

# Fake curl. Each line of $FAKE_ROUTES is "<METHOD> <URL substring> <file>"; the
# first line whose method and substring match prints the file. No match fails
# like `curl -f` on a 404. Every request goes to $FAKE_LOG as "METHOD URL", and a
# request body as "  data: <body>".
mkdir -p "$TMP/bin"
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
method=GET url="" data=""
while [ $# -gt 0 ]; do
  case "$1" in
    -X) method="$2"; shift ;;
    -d|--data|--data-binary) data="$2"; [ "$method" = GET ] && method=POST; shift ;;
    http*) url="$1" ;;
  esac
  shift
done
echo "$method $url" >> "$FAKE_LOG"
[ -n "$data" ] && echo "  data: $data" >> "$FAKE_LOG"
while read -r m sub file; do
  [ -z "$m" ] && continue
  if [ "$m" = "$method" ] && [[ "$url" == *"$sub"* ]]; then
    cat "$FAKE_DIR/$file"
    exit 0
  fi
done < "$FAKE_ROUTES"
exit 22
EOF
chmod +x "$TMP/bin/curl"
FIX="$TMP/fixtures"
mkdir -p "$FIX"

# dev: green commit, a merged PR, a squash-merged PR, a direct push, red commit.
REPO="$TMP/repo"
G=(git -c user.name=t -c user.email=t@example.org -c commit.gpgsign=false -c init.defaultBranch=dev)
mkdir -p "$REPO"
cd "$REPO" || exit 1
"${G[@]}" init -q .
commit() { echo "$1" >> f; "${G[@]}" add f; "${G[@]}" commit -qm "$1"; }
commit "feat: base"
GREEN="$(git rev-parse HEAD)"
"${G[@]}" checkout -q -b feat/a
commit "feat: a"
"${G[@]}" checkout -q dev
"${G[@]}" merge -q --no-ff -m "Merge pull request #101 from coasys/feat/a" feat/a
commit "fix: thing (#102)"
commit "chore: direct push"
RED="$(git rev-parse HEAD)"
DIRECT="$(git rev-parse --short HEAD)"

# CircleCI v1.1 tree rows (newest first). `row <workflow> <job> <sha> [build_num]`.
row() {
  printf '{"build_num":%s,"status":"success","vcs_revision":"%s","build_url":"https://ci.example/%s","workflows":{"workflow_name":"%s","job_name":"%s"}}' \
    "${4:-1}" "$3" "${4:-1}" "$1" "$2"
}
# Page 1: a halted build-and-test run of the same job on the red commit (must not
# count), another tier job, then this job's last green tier run.
echo "[$(row build-and-test integration-tests-sfu-e2e "$RED" 9),$(row dev-2-hourly integration-tests-mcp "$RED" 8),$(row dev-2-hourly integration-tests-sfu-e2e "$GREEN" 7)]" > "$FIX/tree-green.json"
echo "[$(row dev-2-hourly integration-tests-sfu-e2e "$RED" 7)]" > "$FIX/tree-same.json"
echo "[$(row build-and-test integration-tests-sfu-e2e "$RED" 9)]" > "$FIX/tree-other.json"
echo "[]" > "$FIX/empty.json"
echo '{"message":"Unauthorized"}' > "$FIX/unauthorized.json"
# GitHub answers.
echo '{"total_count":0,"items":[]}' > "$FIX/search-none.json"
echo '{"total_count":2,"items":[{"number":40,"title":"dev red: integration-tests-sfu-e2e-old"},{"number":41,"title":"dev red: integration-tests-sfu-e2e"}]}' > "$FIX/search-open.json"
echo '{"number":42,"html_url":"https://github.com/coasys/ad4m/issues/42"}' > "$FIX/issue.json"
echo '{"id":1,"html_url":"https://github.com/coasys/ad4m/issues/41#c1"}' > "$FIX/comment.json"
echo '{"number":101,"title":"feat: a","user":{"login":"alice"}}' > "$FIX/pr101.json"
echo '{"number":102,"title":"fix: thing","user":{"login":"bob"}}' > "$FIX/pr102.json"

# routes <line>...: the fake curl's answers for the next check.
routes() { printf '%s\n' "$@" > "$TMP/routes"; }
STD_PRS=("GET /repos/coasys/ad4m/pulls/101 pr101.json" "GET /repos/coasys/ad4m/pulls/102 pr102.json")

# check <name> <expected exit> <expected output pattern> <mode> [VAR=value ...]
# Runs the script for job integration-tests-sfu-e2e on the red commit of dev.
check() {
  local name="$1" want_rc="$2" want_out="$3" mode="$4"
  shift 4
  : > "$TMP/curl.log"
  local out rc=0
  out="$(env -i PATH="$TMP/bin:$PATH" HOME="$TMP" FAKE_LOG="$TMP/curl.log" FAKE_DIR="$FIX" \
    FAKE_ROUTES="$TMP/routes" DEV_TIER_CIRCLE_API="$CIRCLE" DEV_TIER_GITHUB_API="$GH" \
    CIRCLE_BRANCH=dev CIRCLE_SHA1="$RED" CIRCLE_BUILD_URL="https://ci.example/10" \
    CIRCLECI_API_TOKEN=ctok GITHUB_TOKEN=ghtok \
    "$@" bash "$SCRIPT" "$mode" integration-tests-sfu-e2e 2>&1)" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    echo "  FAIL: $name: exit $rc (want $want_rc), output: $out"
    FAILED=1
  else
    echo "  ok: $name -> $(head -1 <<<"$out")"
  fi
  OUT="$out"
}
logged() { # <name> <pattern>: the fake curl's log must match
  grep -qE -- "$2" "$TMP/curl.log" || { echo "  FAIL: $1: no request matching $2 in: $(cat "$TMP/curl.log")"; FAILED=1; }
}
not_logged() {
  if grep -qE -- "$2" "$TMP/curl.log"; then echo "  FAIL: $1: unexpected request $2 in: $(cat "$TMP/curl.log")"; FAILED=1; fi
}

echo "dev-tier.sh"

check "no mode fails" 2 'usage' ""
check "unknown mode fails" 2 'usage' bogus

# --- moved: halt when this commit already passed this tier's job.
routes "GET /tree/dev tree-same.json"
check "moved: commit already green in this tier skips" 10 "^skip: dev @ ${RED:0:12}.* already passed integration-tests-sfu-e2e in dev-2-hourly" moved
logged "moved asks for successful runs of dev" "GET $CIRCLE/tree/dev\?filter=successful&limit=100&shallow=true&offset=0$"
routes "GET /tree/dev tree-green.json"
check "moved: dev moved since the last green tier run runs" 0 "^run: dev @ ${RED:0:12}, last green in dev-2-hourly ${GREEN:0:12}" moved
routes "GET /tree/dev tree-other.json"
check "moved: a halted build-and-test run of the same job does not count" 0 '^run: no successful dev-2-hourly run of integration-tests-sfu-e2e' moved
routes "GET offset=0 tree-other.json" "GET offset=100 tree-same.json"
check "moved: reads further pages" 10 '^skip:' moved
logged "second page" "offset=100$"
routes "GET /tree/dev empty.json"
check "moved: no history runs" 0 '^run: no successful' moved
not_logged "empty page ends the paging" "offset=100"
routes
check "moved: CircleCI API failure runs" 0 '^run: no successful' moved
routes "GET /tree/dev unauthorized.json"
check "moved: CircleCI error object runs" 0 '^run: no successful' moved
routes "GET /tree/feat/x tree-same.json"
check "moved: a run by hand on another branch asks for that branch" 10 '^skip: feat/x @' moved CIRCLE_BRANCH=feat/x
check "moved: no CIRCLE_SHA1 runs" 0 '^run: no CIRCLE_SHA1' moved CIRCLE_SHA1=

# --- red: open an issue, or comment on the open one.
routes "GET /tree/dev tree-green.json" "GET /search/issues search-none.json" \
  "POST /repos/coasys/ad4m/issues issue.json" "${STD_PRS[@]}"
check "red: no open issue opens one" 0 '^opened https://github.com/coasys/ad4m/issues/42' red
logged "searches open issues by title" 'GET https://gh.example/search/issues\?q=repo%3Acoasys%2Fad4m%20is%3Aissue%20is%3Aopen%20in%3Atitle%20%22dev%20red%3A%20integration-tests-sfu-e2e%22'
logged "posts the issue" "POST $GH/repos/coasys/ad4m/issues$"
logged "title" '"title":"dev red: integration-tests-sfu-e2e"'
logged "first red commit" "First red commit: $RED"
logged "last green commit" "Last green commit of this tier: $GREEN"
logged "compare link" "compare/$GREEN\.\.\.$RED"
logged "merge-commit PR with author" '#101 feat: a \(@alice\)'
logged "squash PR with author" '#102 fix: thing \(@bob\)'
logged "direct push listed by commit" "$DIRECT chore: direct push"
logged "run link" 'https://ci.example/10'
not_logged "no assignees yet" 'assignees'

routes "GET /tree/dev tree-green.json" "GET /search/issues search-open.json" \
  "POST /repos/coasys/ad4m/issues/41/comments comment.json" "${STD_PRS[@]}"
check "red: open issue with the exact title gets a comment" 0 '^commented on #41' red
logged "comment posted on #41" "POST $GH/repos/coasys/ad4m/issues/41/comments$"
logged "comment says still red" "Still red at $RED"
not_logged "no new issue" "POST $GH/repos/coasys/ad4m/issues$"

routes "GET /tree/dev empty.json" "GET /search/issues search-none.json" "POST /repos/coasys/ad4m/issues issue.json"
check "red: no green run on record still opens the issue" 0 '^opened' red
logged "says none on record" 'Last green commit of this tier: none on record'

routes "GET /tree/dev tree-green.json" "GET /search/issues search-none.json" "POST /repos/coasys/ad4m/issues issue.json"
check "red: PR lookup fails, PRs listed from commit subjects" 0 '^opened' red
logged "PR from subject" '#101 Merge pull request #101 from coasys/feat/a'

printf '[%s]\n' "$(row dev-2-hourly integration-tests-sfu-e2e 0123456789abcdef0123456789abcdef01234567 7)" > "$FIX/tree-unknown.json"
routes "GET /tree/dev tree-unknown.json" "GET /search/issues search-none.json" "POST /repos/coasys/ad4m/issues issue.json"
check "red: green commit not in this checkout still opens the issue" 0 '^opened' red
logged "says the range is unavailable" 'not in this checkout'

routes "GET /tree/dev tree-green.json" "GET /search/issues search-none.json" "${STD_PRS[@]}"
check "red: GitHub refuses the issue: exit 1, body in the log" 1 'could not open the issue' red
grep -q 'First red commit' <<<"$OUT" || { echo "  FAIL: body not printed on failure"; FAILED=1; }
routes "GET /tree/dev tree-green.json"
check "red: search fails: exit 1, no blind second issue" 1 'could not search' red
not_logged "no POST after a failed search" "^POST"

routes "GET /tree/dev tree-green.json" "${STD_PRS[@]}"
check "red: no GITHUB_TOKEN prints the body, opens nothing" 0 'GITHUB_TOKEN is not set' red GITHUB_TOKEN=
grep -q "First red commit: $RED" <<<"$OUT" || { echo "  FAIL: body not printed"; FAILED=1; }
not_logged "nothing sent to GitHub issues" "issues"
routes "GET /tree/feat/x tree-green.json"
check "red: a run on another branch opens nothing" 0 'not dev' red CIRCLE_BRANCH=feat/x
not_logged "nothing sent to GitHub issues" "issues"
check "red: DEV_TIER_NO_ISSUE (no build stash) opens nothing" 0 '^no issue: no build stash' red DEV_TIER_NO_ISSUE="no build stash"
not_logged "nothing sent" "."

# --- green: note it on the open issue; never opens or closes one.
routes "GET /search/issues search-open.json" "POST /repos/coasys/ad4m/issues/41/comments comment.json"
check "green: open issue gets a comment" 0 '^commented on #41' green
logged "green comment" "Green again at $RED"
not_logged "does not close" "PATCH"
routes "GET /search/issues search-none.json"
check "green: no open issue does nothing" 0 '^no open issue' green
not_logged "no POST" "^POST"
check "green: no GITHUB_TOKEN does nothing" 0 'GITHUB_TOKEN is not set' green GITHUB_TOKEN=
routes
check "green: GitHub failure does not fail the job" 0 'could not search' green

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
