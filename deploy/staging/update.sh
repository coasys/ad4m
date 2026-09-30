#!/usr/bin/env bash
# Deploys origin/staging to the staging executor (ad4m-staging.service) when
# it has moved. Run by ad4m-staging-update.service; runbook in
# docs-src/headless-executor.md.
#
#   fetch origin/staging -- deployed, or failed last time? -- yes --> exit
#     | no
#   build in ~/ad4m-staging-src (detached worktree, own target/)
#     | ok                        (fails --> last_failed_sha; the old build keeps running)
#   stop -- snapshot data dir -- init -- current -> releases/<sha> -- start
#     |
#   gate: /health, and the agent unlocked (or no agent yet), within 180 s
#     | ok                        (fails --> stop, restore the snapshot,
#   deployed_sha = <sha>                     current -> previous, start)
#
# Every outcome is written to status.json in the state dir and next to it at
# $AD4M_STAGING_PUBLIC_STATUS (served as https://staging.ad4m.dev/status.json).
# The script reads no secret value itself: agent.mjs reads the admin credential
# from its file for the unlock check.
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
#
# Usage: update.sh [deploy]   deploy origin/staging if it moved (the timer runs this)
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

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
STATUS=$STATE/status.json
REQUIRED_SECRETS=(admin-credential unlock-passphrase)

log() { echo "update: $*"; }

# A CI runner's checkout pins its branch when a worktree hangs off it.
case $REPO in
  *ci-workdir*) log "refusing to create worktrees from a CI workdir: $REPO"; exit 1 ;;
esac

mkdir -p "$STATE/releases" "$STATE/snapshots" "$STATE/logs"
exec 9>"$STATE/update.lock"
if ! flock -n 9; then
  log "another update is running"
  exit 0
fi

[[ -s $STATUS ]] || echo '{"deployed_sha":null,"subject":null,"deployed_at":null,"previous_sha":null,"last_result":null,"last_failed_sha":null}' >"$STATUS"
field() { jq -r --arg k "$1" '.[$k] // empty' "$STATUS"; }

# set_status key value [key value ...]: merges into status.json and copies it
# to the public path. A value "null" is JSON null.
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
  jq "${args[@]}" "$filter" "$STATUS" >"$STATUS.tmp"
  mv "$STATUS.tmp" "$STATUS"
  if [[ -d $(dirname "$PUBLIC_STATUS") ]]; then
    cp "$STATUS" "$PUBLIC_STATUS.tmp" && mv "$PUBLIC_STATUS.tmp" "$PUBLIC_STATUS"
  else
    log "no directory for $PUBLIC_STATUS; status.json not published"
  fi
}

fail() {
  log "$1"
  set_status last_result "$1"
  exit 1
}

# --- Preconditions: config and secrets in place
[[ -f $CONFIG/executor-config.json ]] || fail "error: $CONFIG/executor-config.json is missing"
for name in "${REQUIRED_SECRETS[@]}"; do
  file=$CONFIG/secrets/$name
  [[ -s $file ]] || fail "error: secret file $file is missing or empty"
  mode=$(stat -c %a "$file")
  [[ $mode == 600 || $mode == 400 ]] || fail "error: secret file $file has mode $mode, needs 600 or 400"
done

# --- Deploy steps shared by `deploy` and `rollback`
link() { ln -sfn "releases/$2" "$STATE/$1.new" && mv -T "$STATE/$1.new" "$STATE/$1"; }

# `init` writes the network seed of this build and clears state the build
# can no longer read, as the launcher does on every start.
init_data() { "$STATE/current/ad4m-executor" init --data-path "$DATA"; }

# /health only shows that the HTTP server is up; the agent has to unlock from
# the passphrase file too. A node with no agent yet passes on /health alone.
agent_state=
gate() {
  local deadline=$((SECONDS + GATE_SECONDS))
  agent_state=
  while ((SECONDS < deadline)); do
    if curl -fsS --max-time 5 "$URL/health" >/dev/null 2>&1; then
      agent_state=$(AD4M_URL=$URL AD4M_ADMIN_CREDENTIAL_FILE=$CONFIG/secrets/admin-credential \
        timeout 30 node "$HERE/agent.mjs" status 2>/dev/null) || agent_state=
      [[ $agent_state == unlocked || $agent_state == no-agent ]] && return 0
    fi
    sleep 5
  done
  return 1
}

start_and_gate() {
  init_data || return 1
  systemctl --user start "$UNIT" || return 1
  gate
}

# roll_back <failed sha> <data> <sha to run instead or "">: stops the failed
# build, restores the data dir and starts the other build. <data> is a
# snapshot to restore, "none" when there was no data dir before, or "" to
# keep the data dir as it is. Returns 1 if the other build fails the gate too.
roll_back() {
  systemctl --user stop "$UNIT" || true
  if [[ -n $2 && -d $DATA ]]; then
    # Keep what the failed build left, for debugging, until the next rollback.
    rm -rf "${DATA:?}.failed"
    mv "$DATA" "$DATA.failed"
  fi
  if [[ -n $2 && $2 != none ]]; then cp -a --reflink=auto "$2" "$DATA"; fi
  set_status last_failed_sha "$1"
  if [[ -z $3 ]]; then
    rm -f "$STATE/current"
    return 0
  fi
  link current "$3"
  start_and_gate
}

# --- rollback: back to the previous build, by hand
if [[ $COMMAND == rollback ]]; then
  current=$(field deployed_sha)
  previous=$(field previous_sha)
  [[ -n $current && -n $previous && -x $STATE/releases/$previous/ad4m-executor ]] ||
    fail "error: no previous build to roll back to"
  # The snapshot taken when $current replaced $previous holds $previous's data.
  snapshot=$(find "$STATE/snapshots" -mindepth 1 -maxdepth 1 -type d -name "*-$previous" | sort | tail -n 1)
  log "rolling back from $current to $previous (data: ${snapshot:-kept, no snapshot of $previous})"
  if ! roll_back "$current" "$snapshot" "$previous"; then
    fail "rolled_back by hand: $current; $previous failed the gate too (agent: ${agent_state:-no answer})"
  fi
  rm -f "$STATE/previous"
  set_status deployed_sha "$previous" subject "$(git -C "$SRC" log -1 --format=%s "$previous" 2>/dev/null || true)" \
    deployed_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" previous_sha null last_result "rolled_back by hand: $current; $previous runs again"
  log "$previous runs again"
  exit 0
fi

# --- Fetch
if [[ ! -e $SRC/.git ]]; then
  git -C "$REPO" fetch --quiet origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH" ||
    fail "error: git fetch of origin/$BRANCH failed"
  git -C "$REPO" worktree add --quiet --detach "$SRC" "origin/$BRANCH"
fi
git -C "$SRC" fetch --quiet origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH" ||
  fail "error: git fetch of origin/$BRANCH failed"
sha=$(git -C "$SRC" rev-parse "origin/$BRANCH")
subject=$(git -C "$SRC" log -1 --format=%s "$sha")

if [[ $sha == "$(field deployed_sha)" ]]; then
  log "origin/$BRANCH is $sha, already deployed"
  set_status staging_sha "$sha"
  exit 0
fi
if [[ $sha == "$(field last_failed_sha)" && $RETRY != 1 ]]; then
  log "origin/$BRANCH is $sha, which failed before; AD4M_STAGING_RETRY=1 builds it again"
  set_status staging_sha "$sha"
  exit 0
fi

# The unit needs `run --config`, AD4M_*_FILE secrets and the unlock at
# startup, all added by coasys/ad4m#1215 PR1. An older staging cannot run it.
if ! git -C "$SRC" grep -q AD4M_UNLOCK_PASSPHRASE_FILE "$sha" -- cli/src; then
  log "origin/$BRANCH ($sha) predates #1215 PR1; nothing deployed"
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
  export CARGO_TARGET_DIR=$1/target
  pnpm install --no-frozen-lockfile
  pnpm build-dapp
  (cd core && pnpm install --no-frozen-lockfile)
  pnpm run build-deno-snapshot
  (cd cli && cargo build --release)
' build "$SRC" "$sha" >"$build_log" 2>&1; then
  set_status last_failed_sha "$sha"
  fail "build failed for $sha (log: $build_log)"
fi
# Keep the logs of the last five builds.
find "$STATE/logs" -name 'build-*.log' -printf '%T@ %p\n' | sort -rn | tail -n +6 | cut -d' ' -f2- | xargs -r rm -f

mkdir -p "$STATE/releases/$sha"
install -m 0755 "$SRC/target/release/ad4m-executor" "$SRC/target/release/ad4m" "$STATE/releases/$sha/"

# --- Swap: stop, snapshot, point current at the new build
previous=$(field deployed_sha)
log "stopping $UNIT"
systemctl --user stop "$UNIT"
snapshot=
if [[ -d $DATA ]]; then
  snapshot=$STATE/snapshots/$(date -u +%Y%m%dT%H%M%S.%NZ)-${previous:-none}
  log "snapshot of $DATA to $snapshot"
  cp -a --reflink=auto "$DATA" "$snapshot"
  # Keep the two newest snapshots.
  find "$STATE/snapshots" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort -r | tail -n +3 |
    while read -r old; do rm -rf "${STATE:?}/snapshots/$old"; done
fi
link current "$sha"
if [[ -n $previous ]]; then link previous "$previous"; fi

# --- Gate
if start_and_gate; then
  note=
  [[ $agent_state == no-agent ]] && note=" (no agent yet)"
  log "deployed $sha$note"
  set_status deployed_sha "$sha" subject "$subject" deployed_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    previous_sha "${previous:-null}" last_result "deployed$note"
  # Drop releases that are neither current nor previous.
  find "$STATE/releases" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' |
    while read -r old; do
      [[ $old == "$sha" || $old == "$previous" ]] || rm -rf "${STATE:?}/releases/$old"
    done
  exit 0
fi

# --- Roll back
log "$sha failed the gate (agent: ${agent_state:-no answer}); rolling back"
if [[ -z $previous ]]; then
  roll_back "$sha" "${snapshot:-none}" ""
  fail "rolled_back: $sha failed the gate; there is no previous build, staging is stopped"
fi
if roll_back "$sha" "${snapshot:-none}" "$previous"; then
  fail "rolled_back: $sha failed the gate; $previous runs again"
fi
fail "rolled_back: $sha failed the gate, and $previous failed it again after the rollback"
