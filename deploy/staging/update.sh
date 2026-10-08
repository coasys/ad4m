#!/usr/bin/env bash
# Deploys origin/staging to the staging executor (ad4m-staging.service) when
# it has moved. Run by ad4m-staging-update.service; runbook in
# docs-src/headless-executor.md.
#
#   fetch the staging branch head -- deployed, or failed last time? -- yes --> exit
#     | no
#   build in ~/ad4m-staging-src (detached worktree, own target/)
#     | ok                        (fails --> last_failed_sha; the old build keeps running)
#   stop -- snapshot data dir -- current -> releases/<sha>
#     |                           (fails --> current back, old build started again)
#   init -- start -- gate: /health and the agent unlocked, within 180 s
#     | ok                        (fails --> stop, restore the snapshot,
#   deployed_sha = <sha>                     current -> the old build, start;
#                                 restore fails --> nothing runs, no current)
#
# A deploy killed after the swap leaves `current` != deployed_sha; the next
# run rolls that build back first, as if it had failed the gate. One killed
# while the deployed build was stopped (before the swap, or after a
# rollback pointed `current` back) leaves in_flight: deploy with `current`
# = deployed_sha; the next run starts the deployed build again. An
# unfinished rollback by hand (in_flight: rollback, from its first step on),
# or a `current` that status.json does not account for, is left to the
# operator. `rollback` itself refuses to start while `current` is not
# deployed_sha, or while an unfinished one holds the newer data aside.
#
# Every outcome is written to status.json in the state dir and copied to
# $AD4M_STAGING_PUBLIC_STATUS (served as https://staging.ad4m.dev/status.json),
# so no message names a local path. The script reads no secret value itself:
# agent.mjs reads the admin credential from its file for the unlock check.
#
# Environment (defaults in brackets):
#   AD4M_STAGING_REPO           repo the source worktree is created from [~/nico/ad4m]
#   AD4M_STAGING_SRC            source worktree [~/ad4m-staging-src]
#   AD4M_STAGING_STATE          releases, snapshots, logs, status.json [~/.local/share/ad4m-staging]
#   AD4M_STAGING_CONFIG         config dir with executor-config.json and secrets/ [~/.config/ad4m-staging]
#   AD4M_STAGING_DATA           executor data dir [~/.ad4m-staging]
#   AD4M_STAGING_PUBLIC_STATUS  public copy of status.json [/var/www/ad4m-staging/status.json]
#   AD4M_STAGING_BRANCH         branch to track [staging]
#   AD4M_STAGING_UNIT           executor unit [ad4m-staging.service]
#   AD4M_STAGING_URL            executor URL [http://127.0.0.1:12400]
#   AD4M_STAGING_BUILD_TIMEOUT  build time limit, timeout(1) syntax [3h]
#   AD4M_STAGING_GATE_SECONDS   time the new build has to become healthy [180]
#   AD4M_STAGING_RETRY=1        build the last failed SHA again
#   AD4M_STAGING_KEEP_DATA=1    rollback: go back without a snapshot, on the current data
#
# Usage: update.sh [deploy]   deploy the staging branch if it moved (the timer runs this)
#        update.sh rollback   go back to the previous build and its data snapshot
set -euo pipefail
umask 022

COMMAND=${1:-deploy}
case $COMMAND in
  deploy | rollback) ;;
  *) echo "usage: update.sh [deploy|rollback]" >&2; exit 2 ;;
esac

REPO=${AD4M_STAGING_REPO:-$HOME/nico/ad4m}
SRC=${AD4M_STAGING_SRC:-$HOME/ad4m-staging-src}
STATE=${AD4M_STAGING_STATE:-$HOME/.local/share/ad4m-staging}
CONFIG=${AD4M_STAGING_CONFIG:-$HOME/.config/ad4m-staging}
DATA=${AD4M_STAGING_DATA:-$HOME/.ad4m-staging}
PUBLIC_STATUS=${AD4M_STAGING_PUBLIC_STATUS:-/var/www/ad4m-staging/status.json}
BRANCH=${AD4M_STAGING_BRANCH:-staging}
UNIT=${AD4M_STAGING_UNIT:-ad4m-staging.service}
URL=${AD4M_STAGING_URL:-http://127.0.0.1:12400}
BUILD_TIMEOUT=${AD4M_STAGING_BUILD_TIMEOUT:-3h}
GATE_SECONDS=${AD4M_STAGING_GATE_SECONDS:-180}
RETRY=${AD4M_STAGING_RETRY:-0}
KEEP_DATA=${AD4M_STAGING_KEEP_DATA:-0}

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
STATUS=$STATE/status.json
REQUIRED_SECRETS=(admin-credential unlock-passphrase)
# The branch head is fetched into a ref of its own and always named in full:
# a short name like origin/staging also matches a tag of that name, which
# anyone who can push a tag could point at any commit.
REF=refs/ad4m-staging/$BRANCH

log() { echo "update: $*"; }

# A CI runner's checkout pins its branch when a worktree hangs off it.
case $REPO in
  *ci-workdir*) log "refusing to create worktrees from a CI workdir: $REPO"; exit 1 ;;
esac

mkdir -p "$STATE/releases" "$STATE/snapshots" "$STATE/logs"
exec 9>"$STATE/update.lock"
if ! flock -n 9; then
  log "another update is running"
  # A rollback by hand that did nothing must not look like one that worked.
  [[ $COMMAND == rollback ]] && exit 1
  exit 0
fi

[[ -s $STATUS ]] || echo '{"deployed_sha":null,"subject":null,"deployed_at":null,"previous_sha":null,"last_result":null,"last_failed_sha":null}' >"$STATUS"
field() { jq -r --arg k "$1" '.[$k] // empty' "$STATUS"; }

# set_status key value [key value ...]: merges into status.json and copies it
# to the public path. A value "null" is JSON null. Returns 1 if either write
# fails; status.json then keeps its last content.
set_status() {
  local args=() filter='.' i=0
  while (($# >= 2)); do
    if [[ $2 == null ]]; then
      filter+=" | .[\"$1\"] = null"
    else
      args+=(--arg "v$i" "$2")
      filter+=" | .[\"$1\"] = \$v$i"
    fi
    shift 2
    i=$((i + 1))
  done
  filter+=' | .checked_at = (now | todate)'
  if ! { jq "${args[@]}" "$filter" "$STATUS" >"$STATUS.tmp" && mv "$STATUS.tmp" "$STATUS"; }; then
    rm -f "$STATUS.tmp"
    log "could not write status.json"
    return 1
  fi
  if [[ -d $(dirname "$PUBLIC_STATUS") ]]; then
    cp "$STATUS" "$PUBLIC_STATUS.tmp" && mv "$PUBLIC_STATUS.tmp" "$PUBLIC_STATUS" || return 1
  else
    log "no directory for $PUBLIC_STATUS; status.json not published"
  fi
}

# fail <message> [key value ...]: records the message as last_result, with
# the other fields given, and exits 1.
fail() {
  log "$1"
  set_status last_result "$1" "${@:2}" || true
  exit 1
}

# --- Preconditions: config and secrets in place
[[ -f $CONFIG/executor-config.json ]] || fail "error: executor-config.json is missing from the config directory"
for name in "${REQUIRED_SECRETS[@]}"; do
  file=$CONFIG/secrets/$name
  [[ -s $file ]] || fail "error: secret file $name is missing or empty"
  mode=$(stat -c %a "$file")
  [[ $mode == 600 || $mode == 400 ]] || fail "error: secret file $name has mode $mode, needs 600 or 400"
done

# --- Deploy steps shared by `deploy` and `rollback`
link() { ln -sfn "releases/$2" "$STATE/$1.new" && mv -T "$STATE/$1.new" "$STATE/$1"; }

# Snapshots are named <time>-<sha>, <sha> being the build whose data they
# hold. prune_snapshots <sha>...: keeps the newest snapshot of each SHA
# given, deletes every other one.
prune_snapshots() {
  local name sha kept=' '
  find "$STATE/snapshots" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort -r >"$STATE/snapshots.list"
  while read -r name; do
    sha=${name##*-}
    if [[ " $* " == *" $sha "* && $kept != *" $sha "* ]]; then
      kept+="$sha "
    else
      rm -rf "${STATE:?}/snapshots/$name"
    fi
  done <"$STATE/snapshots.list"
  rm -f "$STATE/snapshots.list"
}

# `init` writes the network seed of this build and clears state the build
# can no longer read, as the launcher does on every start.
init_data() { "$STATE/current/ad4m-executor" init --data-path "$DATA"; }

# /health only shows that the HTTP server is up; the agent has to unlock from
# the passphrase file too. A node that never had an agent passes on /health
# alone; once an agent has been seen unlocked, a build that finds none fails.
agent_state=
# It probes at least once, so a slow machine cannot use up the time before
# the first probe.
gate() {
  local deadline accept=unlocked
  [[ $(field agent) == unlocked ]] || accept='unlocked no-agent'
  agent_state=
  deadline=$((SECONDS + GATE_SECONDS))
  while :; do
    if curl -fsS --max-time 5 "$URL/health" >/dev/null 2>&1; then
      agent_state=$(AD4M_URL=$URL AD4M_ADMIN_CREDENTIAL_FILE=$CONFIG/secrets/admin-credential \
        timeout 30 node "$HERE/agent.mjs" status 2>/dev/null) || agent_state=
      [[ -n $agent_state && " $accept " == *" $agent_state "* ]] && return 0
    fi
    ((SECONDS < deadline)) || return 1
    sleep 5
  done
}

start_and_gate() {
  init_data || return 1
  systemctl --user start "$UNIT" || return 1
  gate
}

# roll_back <failed sha> <data> <sha to run instead or "">: stops the failed
# build, restores the data dir and starts the other build. <data> is a
# snapshot to restore, "none" when there was no data dir before, or "" to
# keep the data dir as it is. Returns 1 if the other build fails the gate too,
# 2 if the data dir could not be restored; then nothing runs and `current` is
# gone, so neither a restart nor the next deploy runs on a torn data dir.
# failed_data names the build whose data is in $DATA.failed; it is null
# until the move is done, so an operator can tell a rollback killed before
# it from one killed after it.
# Callers run it as a condition, which turns errexit off inside it: every
# step checks its own result. A failed status write does not stop the
# restore: the old build and its data matter more, and without the record
# the next run only tries the failed build again.
roll_back() {
  local ran
  ran=$(readlink "$STATE/current") || ran=
  systemctl --user stop "$UNIT" || true
  set_status last_failed_sha "$1" failed_data null || log "could not record $1 as failed; the next run tries it again"
  if [[ -n $2 && -d $DATA ]]; then
    # Keep what the failed build left, for debugging, until the next rollback.
    if ! { rm -rf "${DATA:?}.failed" && mv "$DATA" "$DATA.failed"; }; then
      rm -f "$STATE/current"
      return 2
    fi
    set_status failed_data "${ran#releases/}" || true
  fi
  if [[ -n $2 && $2 != none ]] && ! cp -a --reflink=auto "$2" "$DATA"; then
    rm -rf "${DATA:?}"
    rm -f "$STATE/current"
    return 2
  fi
  if [[ -z $3 ]]; then
    rm -f "$STATE/current"
    return 0
  fi
  link current "$3" || return 1
  start_and_gate
}
restore_failed() { fail "error: could not restore the data of $1; staging is stopped" "${@:2}"; }

# --- rollback: back to the previous build, by hand
if [[ $COMMAND == rollback ]]; then
  current=$(field deployed_sha)
  previous=$(field previous_sha)
  [[ -n $current && -n $previous && -x $STATE/releases/$previous/ad4m-executor ]] ||
    fail "error: no previous build to roll back to"
  # Refusals below leave status.json alone: its last_result says how the
  # unfinished step ended. A deploy killed in its gate, a rollback that
  # moved `current` or a failed restore leave `current` != deployed_sha;
  # the timer's next run or the operator resolves those (runbook: Roll back).
  live=$(readlink "$STATE/current" || true)
  if [[ ${live#releases/} != "$current" ]]; then
    log "error: current is ${live:-missing}, not the deployed $current; an unfinished deploy or rollback comes first (runbook: Roll back)"
    exit 1
  fi
  # An unfinished rollback that moved the data aside holds the only copy of
  # the newer build's data in $DATA.failed; a second one would replace it.
  if [[ $(field in_flight) == rollback && -n $(field failed_data) ]]; then
    log "error: the unfinished rollback by hand keeps the data of $(field failed_data) in $DATA.failed; resolve it by hand first (runbook: Roll back)"
    exit 1
  fi
  # The snapshot taken when $current replaced $previous holds $previous's data.
  snapshot=$(find "$STATE/snapshots" -mindepth 1 -maxdepth 1 -type d -name "*-$previous" | sort | tail -n 1)
  if [[ -z $snapshot && $KEEP_DATA != 1 ]]; then
    fail "error: no snapshot of the data of $previous; AD4M_STAGING_KEEP_DATA=1 rolls back on the current data"
  fi
  log "rolling back from $current to $previous (data: ${snapshot:+snapshot }${snapshot:-kept})"
  # The staging head counts as failed, so the timer stays on $previous
  # until staging moves.
  failed=$(field staging_sha)
  # in_flight tells the next run that this rollback did not finish, whether
  # it was killed before `current` moved or after: every run stops until an
  # operator has looked (runbook: Roll back).
  set_status in_flight rollback failed_data null last_result "rolling back by hand from $current to $previous"
  rc=0
  roll_back "${failed:-$current}" "$snapshot" "$previous" || rc=$?
  ((rc != 2)) || restore_failed "$previous"
  ((rc == 0)) || fail "rolled_back by hand: $current; $previous failed the gate too (agent: ${agent_state:-no answer})"
  rm -f "$STATE/previous"
  set_status deployed_sha "$previous" subject "$(git -C "$SRC" log -1 --format=%s "$previous" 2>/dev/null || true)" \
    deployed_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" previous_sha null in_flight null \
    last_result "rolled_back by hand: $current; $previous runs again"
  log "$previous runs again"
  exit 0
fi

# --- An earlier run that ended between the swap and its result
# `current` names the build that runs. If it is not deployed_sha, the last
# deploy or rollback by hand was killed during its gate (a reboot, the OOM
# killer), a rollback by hand failed its gate, or a rollback could not
# restore the data. A snapshot taken now would label the unchecked build's
# data as the deployed build's.
deployed=$(field deployed_sha)
live=$(readlink "$STATE/current" || true)
live=${live#releases/}
# A rollback by hand moves `current` only after it has stopped staging and
# restored the data, so one killed before that leaves `current` =
# deployed_sha on a stopped node and maybe a half-copied data dir. Its
# target is previous_sha: `current` may still be the newer build. Without a
# `current`, the failed restore below keeps its own message.
if [[ -n $live && $(field in_flight) == rollback ]]; then
  previous=$(field previous_sha)
  log "the rollback by hand from $deployed to $previous did not finish; staging is left as it is for an operator (runbook: Roll back)"
  # A rollback whose gate failed has written its result; keep it.
  [[ $(field last_result) == "rolled_back by hand: "* ]] && exit 1
  fail "error: the rollback by hand from $deployed to $previous was interrupted; left to the operator"
fi
if [[ $live != "$deployed" ]]; then
  if [[ -z $live ]]; then
    # status.json keeps the error of the failed restore.
    log "no build is current while $deployed is deployed: a restore failed; staging stays stopped until an operator restores the data (runbook: Roll back)"
    exit 1
  fi
  # Without a deployed build there is no data to go back to, and a lost
  # status.json looks the same as an interrupted first deploy: touch nothing.
  [[ -n $deployed ]] ||
    fail "error: current is $live, but status.json names no deployed build (an interrupted first deploy, or a lost status.json); left to the operator"
  log "the deploy of $live was interrupted before its check; rolling back"
  snapshot=$(find "$STATE/snapshots" -mindepth 1 -maxdepth 1 -type d -name "*-$deployed" | sort | tail -n 1)
  [[ -n $snapshot ]] ||
    fail "error: the deploy of $live was interrupted, and there is no snapshot of the data of $deployed; left to the operator"
  rc=0
  roll_back "$live" "$snapshot" "$deployed" || rc=$?
  ((rc != 2)) || restore_failed "$deployed" in_flight null
  ((rc == 0)) || fail "rolled_back: the deploy of $live was interrupted, and $deployed failed the gate after the rollback" in_flight null
  fail "rolled_back: the deploy of $live was interrupted; $deployed runs again" in_flight null
fi

# --- An earlier run that was killed while the deployed build was stopped
# A deploy sets in_flight before it stops the node and clears it with its
# result. A kill in between leaves `current` = deployed_sha with nothing
# running: either before the swap (during the snapshot, say), or after a
# failed gate's rollback has pointed `current` back but before it started
# the build (roll_back above has the same window). Without this, every run
# would pass the checks above, find the head deployed or failed, and exit 0
# with the node down until the branch moves. The data dir is the deployed
# build's and whole in both cases: a kill before the swap never touches it,
# and roll_back links `current` only after the restore. So start it again
# and go on; a head that failed is still skipped below.
if [[ -n $live && $(field in_flight) == deploy ]]; then
  interrupted=$(field staging_sha)
  log "the deploy of ${interrupted:-a build} was interrupted while $deployed was stopped; starting $deployed again"
  systemctl --user start "$UNIT" || fail "error: the deploy of ${interrupted:-a build} was interrupted while $deployed was stopped, and $deployed could not be started"
  set_status in_flight null last_result "error: the deploy of ${interrupted:-a build} was interrupted while $deployed was stopped; $deployed runs again" || true
fi

# --- Fetch
fetch() { git -C "$1" fetch --no-tags --quiet origin "+refs/heads/$BRANCH:$REF"; }
if [[ ! -e $SRC/.git ]]; then
  fetch "$REPO" || fail "error: git fetch of $BRANCH failed"
  git -C "$REPO" worktree add --quiet --detach "$SRC" "$(git -C "$REPO" rev-parse --verify "$REF^{commit}")"
fi
fetch "$SRC" || fail "error: git fetch of $BRANCH failed"
sha=$(git -C "$SRC" rev-parse --verify "$REF^{commit}")
subject=$(git -C "$SRC" log -1 --format=%s "$sha")

if [[ $sha == "$(field deployed_sha)" ]]; then
  log "$BRANCH is $sha, already deployed"
  set_status staging_sha "$sha"
  exit 0
fi
if [[ $sha == "$(field last_failed_sha)" && $RETRY != 1 ]]; then
  log "$BRANCH is $sha, which failed before; AD4M_STAGING_RETRY=1 builds it again"
  set_status staging_sha "$sha"
  exit 0
fi

# The unit needs `run --config`, AD4M_*_FILE secrets and the unlock at
# startup, all added by coasys/ad4m#1215 PR1. An older staging cannot run it.
if ! git -C "$SRC" grep -q AD4M_UNLOCK_PASSPHRASE_FILE "$sha" -- cli/src; then
  log "$BRANCH ($sha) predates #1215 PR1; nothing deployed"
  set_status staging_sha "$sha" last_result "waiting: origin/$BRANCH lacks #1215 PR1 (--config)"
  exit 0
fi

# --- Build
log "building $sha: $subject"
set_status staging_sha "$sha" last_result "building $sha"
build_log=$STATE/logs/build-$sha.log
# The same steps as the CircleCI build job, in the source worktree ($1) at $2.
# shellcheck disable=SC2016 # expanded by the inner bash
if ! timeout "$BUILD_TIMEOUT" bash -euo pipefail -c '
  cd "$1"
  git checkout --quiet --force --detach "$2"
  [[ $(git rev-parse HEAD) == "$2" ]]
  export CARGO_TARGET_DIR=$1/target
  pnpm install --no-frozen-lockfile
  pnpm build-dapp
  (cd core && pnpm install --no-frozen-lockfile)
  pnpm run build-deno-snapshot
  (cd cli && cargo build --release)
' build "$SRC" "$sha" >"$build_log" 2>&1; then
  set_status last_failed_sha "$sha"
  fail "build failed for $sha (logs/build-$sha.log in the state directory)"
fi
# Keep the logs of the last five builds.
find "$STATE/logs" -name 'build-*.log' -printf '%T@ %p\n' | sort -rn | tail -n +6 | cut -d' ' -f2- | xargs -r rm -f

mkdir -p "$STATE/releases/$sha"
install -m 0755 "$SRC/target/release/ad4m-executor" "$SRC/target/release/ad4m" "$STATE/releases/$sha/"

# --- Swap: stop, snapshot, point current at the new build
deployed=$(field deployed_sha)
before=$(field previous_sha)
snapshot=
partial=
# Between the stop and the gate, a failing step (a full disk during the
# snapshot, say) must not leave staging down: put the old build back.
# shellcheck disable=SC2317 # run by the EXIT trap
restore_on_error() {
  local rc=$?
  trap - EXIT
  ((rc == 0)) && return
  set +e
  log "deploying $sha failed before its check (exit $rc); starting ${deployed:-nothing} again"
  [[ -n $partial ]] && rm -rf "$partial"
  if [[ -n $deployed ]]; then
    link current "$deployed"
    systemctl --user start "$UNIT"
  else
    rm -f "$STATE/current"
  fi
  set_status last_failed_sha "$sha" in_flight null \
    last_result "error: deploying $sha failed before its check; ${deployed:-nothing} runs again" ||
    log "could not record $sha as failed"
  exit "$rc"
}
# Only this deploy's own outcomes clear it; nothing reads the value
# `deploy`, it tells a reader of status.json what the stop was for.
set_status in_flight deploy
log "stopping $UNIT"
systemctl --user stop "$UNIT"
trap restore_on_error EXIT
if [[ -d $DATA ]]; then
  snapshot=$STATE/snapshots/$(date -u +%Y%m%dT%H%M%S.%NZ)-${deployed:-none}
  log "snapshot of $DATA to $snapshot"
  partial=$snapshot
  cp -a --reflink=auto "$DATA" "$snapshot"
  partial=
  # The snapshot of the running build's data is needed if this deploy fails,
  # the one of the build before it for a rollback by hand.
  prune_snapshots "${deployed:-none}" "$before"
fi
link current "$sha"
trap - EXIT

# --- Gate
if start_and_gate; then
  note=
  [[ $agent_state == no-agent ]] && note=" (no agent yet)"
  log "deployed $sha$note"
  if [[ -n $deployed ]]; then link previous "$deployed"; fi
  set_status deployed_sha "$sha" subject "$subject" deployed_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    previous_sha "${deployed:-null}" in_flight null last_result "deployed$note"
  if [[ $agent_state == unlocked ]]; then set_status agent unlocked; fi
  # Keep what a rollback by hand to $deployed needs: its build and its data.
  prune_snapshots "${deployed:-none}"
  find "$STATE/releases" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' |
    while read -r old; do
      [[ $old == "$sha" || $old == "$deployed" ]] || rm -rf "${STATE:?}/releases/$old"
    done
  exit 0
fi

# --- Roll back
log "$sha failed the gate (agent: ${agent_state:-no answer}); rolling back"
rc=0
roll_back "$sha" "${snapshot:-none}" "$deployed" || rc=$?
((rc != 2)) || restore_failed "${deployed:-the node before $sha}" in_flight null
[[ -n $deployed ]] || fail "rolled_back: $sha failed the gate; there is no previous build, staging is stopped" in_flight null
((rc == 0)) || fail "rolled_back: $sha failed the gate, and $deployed failed it again after the rollback" in_flight null
fail "rolled_back: $sha failed the gate; $deployed runs again" in_flight null
