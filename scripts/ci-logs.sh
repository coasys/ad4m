#!/bin/bash
# ci-logs.sh — Fetch CircleCI CI failure logs for a GitHub PR
#
# Usage:
#   ci-logs.sh <owner/repo> <pr-number> [--all] [--job <name>] [--tail <lines>]
#
# Examples:
#   ci-logs.sh coasys/ad4m 760                    # Show failing jobs + errors
#   ci-logs.sh coasys/ad4m 760 --all              # Show all job statuses
#   ci-logs.sh coasys/ad4m 760 --job integration-tests-model  # Full log for one job
#   ci-logs.sh coasys/ad4m 760 --tail 50          # Last 50 lines of each failing job

set -euo pipefail

REPO="${1:?Usage: ci-logs.sh <owner/repo> <pr-number>}"
PR="${2:?Usage: ci-logs.sh <owner/repo> <pr-number>}"
SHOW_ALL=false
TARGET_JOB=""
TAIL_LINES=30

shift 2
while [[ $# -gt 0 ]]; do
  case "$1" in
    --all) SHOW_ALL=true; shift ;;
    --job) TARGET_JOB="$2"; shift 2 ;;
    --tail) TAIL_LINES="$2"; shift 2 ;;
    *) echo "Unknown flag: $1"; exit 1 ;;
  esac
done

# 1. Get PR check statuses from GitHub
echo "=== PR #${PR} CI Status ==="
CHECKS=$(gh pr checks "$PR" -R "$REPO" 2>/dev/null || true)
if [ -z "$CHECKS" ]; then
  echo "No checks found for PR #${PR}"
  exit 1
fi

# Extract the CircleCI workflow URL
WORKFLOW_URL=$(echo "$CHECKS" | grep 'circleci' | head -1 | awk '{print $NF}')
if [ -z "$WORKFLOW_URL" ]; then
  echo "No CircleCI checks found"
  echo "$CHECKS"
  exit 1
fi

WORKFLOW_ID=$(echo "$WORKFLOW_URL" | sed -E 's|.*/workflow/([a-f0-9-]+).*|\1|')

# 2. Get project slug from workflow
PROJECT_SLUG=$(curl -sL "https://circleci.com/api/v2/workflow/${WORKFLOW_ID}" -H "Accept: application/json" | python3 -c "import sys,json; print(json.load(sys.stdin).get('project_slug',''))" 2>/dev/null)

# 3. Get jobs
JOBS_JSON=$(curl -sL "https://circleci.com/api/v2/workflow/${WORKFLOW_ID}/job" -H "Accept: application/json")

python3 -c "
import sys, json
data = json.loads('''${JOBS_JSON}''')
items = data.get('items', [])
for j in items:
    name = j['name']
    status = j['status']
    num = j.get('job_number', '?')
    icon = '✅' if status == 'success' else '❌' if status == 'failed' else '⏳' if status == 'running' else '⏸️'
    print(f'{icon} {name}: {status} (#{num})')
"
echo ""

# 4. Fetch logs for failing jobs using v1.1 API presigned URLs
FAILING_JOBS=$(python3 -c "
import json
data = json.loads('''${JOBS_JSON}''')
show_all = '${SHOW_ALL}' == 'true'
target = '${TARGET_JOB}'
for j in data.get('items', []):
    if target and j['name'] != target:
        continue
    if show_all or j['status'] == 'failed' or target:
        print(f'{j[\"name\"]}|{j[\"job_number\"]}|{j[\"status\"]}')
")

if [ -z "$FAILING_JOBS" ]; then
  echo "No failing jobs found! 🎉"
  exit 0
fi

while IFS='|' read -r JOB_NAME JOB_NUM JOB_STATUS; do
  echo "━━━ ${JOB_NAME} (#${JOB_NUM}) — ${JOB_STATUS} ━━━"

  # Get step details with presigned output URLs via v1.1 API
  STEP_DATA=$(curl -sL "https://circleci.com/api/v1.1/project/${PROJECT_SLUG}/${JOB_NUM}" -H "Accept: application/json" 2>/dev/null)

  # Find the failed step's output URL
  OUTPUT_URL=$(python3 -c "
import json, sys
try:
    data = json.loads(sys.stdin.read())
    for step in data.get('steps', []):
        for action in step.get('actions', []):
            if action.get('failed') or action.get('status') == 'failed':
                url = action.get('output_url', '')
                if url:
                    print(url)
                    sys.exit(0)
    # If no failed step, get the last step's output
    for step in reversed(data.get('steps', [])):
        for action in step.get('actions', []):
            url = action.get('output_url', '')
            if url:
                print(url)
                sys.exit(0)
except:
    pass
" <<< "$STEP_DATA")

  if [ -z "$OUTPUT_URL" ]; then
    echo "(Could not find output URL)"
    continue
  fi

  # Fetch the actual log content
  LOG=$(curl -sL "$OUTPUT_URL" 2>/dev/null)

  if [ -z "$LOG" ]; then
    echo "(Empty log output)"
    continue
  fi

  # The output is a JSON array of {type, message} objects
  CLEAN=$(python3 -c "
import json, sys, re
try:
    data = json.loads(sys.stdin.read())
    text = ''
    for entry in data:
        if isinstance(entry, dict):
            text += entry.get('message', '')
        elif isinstance(entry, str):
            text += entry
    # Strip ANSI codes
    text = re.sub(r'\x1b\[[0-9;]*m', '', text)
    print(text)
except:
    # Not JSON — treat as plain text
    text = sys.stdin.read() if not data else str(data)
    text = re.sub(r'\x1b\[[0-9;]*m', '', text)
    print(text)
" <<< "$LOG")

  if [ -n "$TARGET_JOB" ]; then
    echo "$CLEAN" | tail -"$TAIL_LINES"
  else
    # Summary: counts + error messages
    echo "$CLEAN" | grep -E 'passing|failing|pending' | tail -5
    echo ""
    echo "$CLEAN" | grep -E '^\s+[0-9]+\) |AssertionError|Error:' | head -20
  fi
  echo ""
done <<< "$FAILING_JOBS"
