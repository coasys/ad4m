#!/usr/bin/env bash
# Runs deploy/staging/update.sh against a throwaway git remote, with pnpm,
# cargo, systemctl, curl, node and sleep replaced by stubs, and checks each
# decision it makes: wait, deploy, skip, roll back, keep a failed build out,
# recover from a failed step after the stop, ignore a tag named like the branch,
# start nothing when a restore fails, clean up after a deploy killed in its gate,
# leave a lost status.json, a failed status write or an unfinished rollback by
# hand (killed in its gate, in its restore or before it) to the operator,
# refuse a rollback by hand over any of these, and start the deployed build
# again after a deploy killed while it was stopped.
# Needs bash, git, jq, flock and GNU coreutils. Usage: update.test.sh
set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
UPDATE=$HERE/../update.sh
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

failures=0
check() { # check <description> <command...>
  local what=$1
  shift
  if "$@"; then
    echo "ok - $what"
  else
    echo "not ok - $what"
    sed 's/^/#   /' "$T/out"
    failures=$((failures + 1))
  fi
}
status() { jq -r ".$1 // empty" "$T/state/status.json"; }

# --- Stubs. They log to $T/calls and keep the "running" build in $T/running.
mkdir -p "$T/stubs"
cat >"$T/stubs/pnpm" <<'EOF'
#!/usr/bin/env bash
echo "pnpm $*" >>"$STUB_DIR/calls"
EOF
cat >"$T/stubs/cargo" <<'EOF'
#!/usr/bin/env bash
# The build of commit <sha> produces an ad4m-executor that answers `init`.
echo "cargo $*" >>"$STUB_DIR/calls"
sha=$(git rev-parse HEAD)
if [[ -f $STUB_DIR/fail-build-$sha ]]; then echo "error[E0000]: stub build failure"; exit 101; fi
mkdir -p "$CARGO_TARGET_DIR/release"
printf '#!/usr/bin/env bash\necho "init %s $*" >>"$STUB_DIR/calls"\n' "$sha" >"$CARGO_TARGET_DIR/release/ad4m-executor"
printf '#!/usr/bin/env bash\n' >"$CARGO_TARGET_DIR/release/ad4m"
chmod +x "$CARGO_TARGET_DIR/release/ad4m-executor" "$CARGO_TARGET_DIR/release/ad4m"
EOF
cat >"$T/stubs/systemctl" <<'EOF'
#!/usr/bin/env bash
# `start` runs whatever `current` points at; the build writes into the data dir.
# With $STUB_DIR/kill-stop, update.sh is killed as it stops the executor,
# which keeps running.
echo "systemctl $*" >>"$STUB_DIR/calls"
case $2 in
  stop)
    if [[ -f $STUB_DIR/kill-stop ]]; then
      rm "$STUB_DIR/kill-stop"
      kill -9 "$(cat "$STUB_DIR/update.pid")"
      exit 1
    fi
    rm -f "$STUB_DIR/running" ;;
  start)
    # With a count in $STUB_DIR/kill-start, update.sh is killed as it makes
    # that many-th `start`, before the unit comes up.
    if [[ -f $STUB_DIR/kill-start ]]; then
      n=$(($(cat "$STUB_DIR/kill-start") - 1))
      if ((n == 0)); then
        rm "$STUB_DIR/kill-start"
        kill -9 "$(cat "$STUB_DIR/update.pid")"
        exit 1
      fi
      echo "$n" >"$STUB_DIR/kill-start"
    fi
    sha=$(basename "$(readlink "$AD4M_STAGING_STATE/current")")
    echo "$sha" >"$STUB_DIR/running"
    mkdir -p "$AD4M_STAGING_DATA"
    echo "$sha" >>"$AD4M_STAGING_DATA/written-by" ;;
esac
EOF
cat >"$T/stubs/curl" <<'EOF'
#!/usr/bin/env bash
[[ -f $STUB_DIR/running ]]
EOF
cat >"$T/stubs/node" <<'EOF'
#!/usr/bin/env bash
# agent.mjs status: a build marked bad never unlocks. With $STUB_DIR/kill-gate,
# update.sh is killed during the gate, as by a reboot or the OOM killer.
if [[ -f $STUB_DIR/kill-gate ]]; then
  rm "$STUB_DIR/kill-gate"
  kill -9 "$(cat "$STUB_DIR/update.pid")"
  exit 1
fi
sha=$(cat "$STUB_DIR/running")
if [[ -f $STUB_DIR/bad-$sha ]]; then echo locked; else cat "$STUB_DIR/agent"; fi
EOF
cat >"$T/stubs/cp" <<'EOF'
#!/usr/bin/env bash
# With $STUB_DIR/fail-snapshot, copying into snapshots/ fails half-way, as on a
# full disk; with $STUB_DIR/fail-restore, so does copying a snapshot back.
if [[ -f $STUB_DIR/fail-snapshot && ${*: -1} == */snapshots/* ]] ||
  [[ -f $STUB_DIR/fail-restore && ${*: -1} == "$AD4M_STAGING_DATA" ]]; then
  mkdir -p "${*: -1}"
  echo partial >"${*: -1}/written-by"
  echo "cp: error writing: No space left on device" >&2
  exit 1
fi
# With $STUB_DIR/kill-restore, update.sh is killed half-way through copying a
# snapshot back into the data dir; with $STUB_DIR/kill-snapshot, half-way
# through copying the data dir into snapshots/.
if [[ -f $STUB_DIR/kill-restore && ${*: -1} == "$AD4M_STAGING_DATA" ]] ||
  [[ -f $STUB_DIR/kill-snapshot && ${*: -1} == */snapshots/* ]]; then
  rm -f "$STUB_DIR/kill-restore" "$STUB_DIR/kill-snapshot"
  mkdir -p "${*: -1}"
  echo partial >"${*: -1}/written-by"
  kill -9 "$(cat "$STUB_DIR/update.pid")"
  exit 1
fi
exec /bin/cp "$@"
EOF
cat >"$T/stubs/mv" <<'EOF'
#!/usr/bin/env bash
# With $STUB_DIR/fail-move, moving the data dir aside fails.
if [[ -f $STUB_DIR/fail-move && $* == "$AD4M_STAGING_DATA $AD4M_STAGING_DATA.failed" ]]; then
  echo "mv: cannot move: Input/output error" >&2
  exit 1
fi
# With $STUB_DIR/kill-move, update.sh is killed after the stop, before it moves
# the data dir aside.
if [[ -f $STUB_DIR/kill-move && $* == "$AD4M_STAGING_DATA $AD4M_STAGING_DATA.failed" ]]; then
  rm "$STUB_DIR/kill-move"
  kill -9 "$(cat "$STUB_DIR/update.pid")"
  exit 1
fi
exec /bin/mv "$@"
EOF
# With $STUB_DIR/fail-jq, a status.json write of last_failed_sha fails, as on
# a full disk. With $STUB_DIR/slow-jq, reading the agent field takes 1.2 s, as
# on a loaded machine: longer than the tests' whole gate time.
cat >"$T/stubs/jq" <<EOF
#!/usr/bin/env bash
if [[ -f \$STUB_DIR/slow-jq && \$* == *'--arg k agent'* ]]; then $(command -v sleep) 1.2; fi
if [[ -f \$STUB_DIR/fail-jq && \$* == *'.["last_failed_sha"] ='* ]]; then
  echo "jq: error: No space left on device" >&2
  exit 2
fi
exec $(command -v jq) "\$@"
EOF
printf '#!/bin/sh\n' >"$T/stubs/sleep"
chmod +x "$T/stubs/"*
echo unlocked >"$T/agent"

# --- A remote with a staging branch, and the clone update.sh makes its worktree from.
git init -q --bare -b staging "$T/remote.git"
git clone -q "$T/remote.git" "$T/work" 2>/dev/null
git -C "$T/work" config user.email test@example.org
git -C "$T/work" config user.name test
commit() { # commit <message> [file content]: commits and pushes to staging, prints the SHA
  mkdir -p "$T/work/cli/src" "$T/work/core"
  touch "$T/work/core/package.json"
  if (($# > 1)); then echo "$2" >"$T/work/cli/src/run_config.rs"; fi
  echo "$1" >>"$T/work/log"
  git -C "$T/work" add -A
  git -C "$T/work" commit -qm "$1"
  git -C "$T/work" push -q origin HEAD:staging
  git -C "$T/work" rev-parse HEAD
}
commit "before PR1" >/dev/null
git clone -q "$T/remote.git" "$T/repo" 2>/dev/null

mkdir -p "$T/config/secrets" "$T/www"
echo '{}' >"$T/config/executor-config.json"
(umask 077 && echo cred >"$T/config/secrets/admin-credential" && echo pass >"$T/config/secrets/unlock-passphrase")

export STUB_DIR=$T PATH=$T/stubs:$PATH
export AD4M_STAGING_REPO=$T/repo AD4M_STAGING_SRC=$T/src AD4M_STAGING_STATE=$T/state
export AD4M_STAGING_CONFIG=$T/config AD4M_STAGING_DATA=$T/data
export AD4M_STAGING_PUBLIC_STATUS=$T/www/status.json AD4M_STAGING_GATE_SECONDS=1
run() {
  : >"$T/calls"
  "$UPDATE" "$@" >"$T/out" 2>&1 &
  echo $! >"$T/update.pid"
  { wait $!; } 2>/dev/null || echo "exit $?" >>"$T/out"
}
called() { grep -q "$1" "$T/calls"; }
exited_non_zero() { grep -q '^exit [1-9]' "$T/out"; }

# 1. A staging without #1215 PR1: record why, build and start nothing.
run
check "pre-PR1 staging waits" [ "$(status last_result)" = "waiting: origin/staging lacks #1215 PR1 (--config)" ]
check "pre-PR1 staging is not built" bash -c "! grep -q cargo '$T/calls'"
check "pre-PR1 staging starts nothing" bash -c "! grep -q systemctl '$T/calls'"
check "status.json is published" cmp -s "$T/state/status.json" "$T/www/status.json"

# 2. PR1 lands: build, init, start, pass the gate.
first=$(commit "PR1" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "first deploy succeeds" [ "$(status last_result)" = deployed ]
check "deployed_sha is the staging head" [ "$(status deployed_sha)" = "$first" ]
check "subject is recorded" [ "$(status subject)" = PR1 ]
check "first deploy has no previous_sha" [ -z "$(status previous_sha)" ]
check "current points at the release" [ "$(readlink "$T/state/current")" = "releases/$first" ]
check "init ran with the new build" called "init $first init --data-path $T/data"
check "public status shows the deployed SHA" [ "$(jq -r .deployed_sha "$T/www/status.json")" = "$first" ]

# 3. Nothing new: no build, no restart.
run
check "same SHA is skipped" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"

# 4. A build that never unlocks: roll back binary and data.
bad=$(commit "breaks unlock" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$bad"
run
check "failed gate is recorded" [ "$(status last_failed_sha)" = "$bad" ]
check "result says rolled back" bash -c "[[ '$(status last_result)' == rolled_back:* ]]"
check "deployed_sha stays the good build" [ "$(status deployed_sha)" = "$first" ]
check "current points back at the good build" [ "$(readlink "$T/state/current")" = "releases/$first" ]
check "the good build runs again" [ "$(cat "$T/running")" = "$first" ]
check "data dir is the snapshot, without the bad build's writes" \
  [ "$(cat "$T/data/written-by")" = "$(printf '%s\n%s' "$first" "$first")" ]
check "the bad build's data is kept aside" grep -q "$bad" "$T/data.failed/written-by"
check "a failed gate leaves no deploy in flight" [ "$(jq -r .in_flight "$T/www/status.json")" = null ]

# 5. The failed SHA is not built again, unless asked to.
run
check "last failed SHA is skipped" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
rm "$T/bad-$bad"
AD4M_STAGING_RETRY=1 run
check "AD4M_STAGING_RETRY=1 deploys it" [ "$(status deployed_sha)" = "$bad" ]
check "previous_sha is the build before" [ "$(status previous_sha)" = "$first" ]
check "previous points at the build before" [ "$(readlink "$T/state/previous")" = "releases/$first" ]

# 6. A build that fails to compile: the running build is not touched.
broken=$(commit "does not compile" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/fail-build-$broken"
run
check "build failure is recorded" [ "$(status last_failed_sha)" = "$broken" ]
check "result names the build failure" bash -c "[[ '$(status last_result)' == 'build failed for $broken'* ]]"
check "the running build is not stopped" bash -c "! grep -q systemctl '$T/calls'"
check "deployed_sha is unchanged" [ "$(status deployed_sha)" = "$bad" ]

# 7. Housekeeping: keep the release and the data snapshot of the previous build.
last=$(commit "next" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "a later deploy succeeds" [ "$(status deployed_sha)" = "$last" ]
check "only the previous build's snapshot is kept" \
  [ "$(find "$T/state/snapshots" -mindepth 1 -maxdepth 1 -printf '%f\n' | sed 's/.*-//')" = "$bad" ]
check "only current and previous releases are kept" \
  [ "$(find "$T/state/releases" -mindepth 1 -maxdepth 1 -printf '%f\n' | sort | tr '\n' ' ')" = "$(printf '%s\n' "$bad" "$last" | sort | tr '\n' ' ')" ]

# 8. Once an agent was unlocked, a build that finds no agent fails the gate.
echo no-agent >"$T/agent"
lost=$(commit "loses the agent" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "a build without the agent is rolled back" [ "$(status deployed_sha)/$(status last_failed_sha)" = "$last/$lost" ]
check "an automatic rollback leaves previous at the build before" [ "$(readlink "$T/state/previous")" = "releases/$bad" ]
echo unlocked >"$T/agent"

# 9. Two failed deploys in a row, then a rollback by hand: the build before
# comes back with its own data, not with the newer build's.
worse=$(commit "fails too" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$worse"
run
check "the second failed deploy is rolled back" [ "$(status deployed_sha)/$(status last_failed_sha)" = "$last/$worse" ]
snap=$(find "$T/state/snapshots" -mindepth 1 -maxdepth 1 -name "*-$bad")
check "the snapshot of the build before survives failed deploys" [ -n "$snap" ]
run rollback
check "rollback runs the previous build" [ "$(cat "$T/running")" = "$bad" ]
check "rollback records it" [ "$(status deployed_sha)" = "$bad" ]
check "rollback result" [ "$(status last_result)" = "rolled_back by hand: $last; $bad runs again" ]
check "rollback restores the previous build's data" \
  [ "$(head -n -1 "$T/data/written-by")" = "$(cat "$snap/written-by")" ]
check "the rolled-back build's data is kept aside" [ "$(tail -n 1 "$T/data.failed/written-by")" = "$last" ]
run
check "the timer does not deploy the staging head after a rollback" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
run rollback
check "a second rollback has nothing to go back to" [ "$(status last_result)" = "error: no previous build to roll back to" ]

# 9b. No snapshot of the previous build's data: refuse, unless told to keep the data.
rm "$T/bad-$worse"
AD4M_STAGING_RETRY=1 run
check "a retried SHA deploys" [ "$(status deployed_sha)/$(status previous_sha)" = "$worse/$bad" ]
rm -rf "$T/state/snapshots/"*
run rollback
check "rollback without a snapshot is refused" [ "$(status last_result)" = "error: no snapshot of the data of $bad; AD4M_STAGING_KEEP_DATA=1 rolls back on the current data" ]
check "and changes nothing" [ "$(cat "$T/running")" = "$worse" ]
AD4M_STAGING_KEEP_DATA=1 run rollback
check "AD4M_STAGING_KEEP_DATA=1 rolls back on the current data" \
  [ "$(cat "$T/running")/$(tail -n 2 "$T/data/written-by" | head -n 1)" = "$bad/$worse" ]

# 9c. A tag named like the branch does not decide what is deployed.
git -C "$T/work" checkout -q -b side
git -C "$T/work" commit -q --allow-empty -m "not on staging"
git -C "$T/work" push -q origin "HEAD:refs/tags/origin/staging"
git -C "$T/work" checkout -q staging
git -C "$T/repo" fetch -q --tags origin
head=$(commit "the real head" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "the branch head is deployed, not the tag" [ "$(status deployed_sha)" = "$head" ]

# 9d. A step between the stop and the gate fails (the snapshot, on a full
# disk): the running build comes back, the SHA counts as failed.
touch "$T/fail-snapshot"
full=$(commit "disk full" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
rm "$T/fail-snapshot"
check "the old build runs again" [ "$(cat "$T/running")" = "$head" ]
check "current points at it" [ "$(readlink "$T/state/current")" = "releases/$head" ]
check "the SHA is recorded as failed" [ "$(status last_failed_sha)" = "$full" ]
check "the result says so" [ "$(status last_result)" = "error: deploying $full failed before its check; $head runs again" ]
check "and leaves no deploy in flight" [ "$(jq -r .in_flight "$T/state/status.json")" = null ]
check "no partial snapshot is left" bash -c "! find '$T/state/snapshots' -mindepth 1 -maxdepth 1 -name '*-$head' | grep -q ."

# 10. A secret file others can read stops the update before anything else.
chmod 644 "$T/config/secrets/unlock-passphrase"
commit "after chmod" "AD4M_UNLOCK_PASSPHRASE_FILE" >/dev/null
run
check "a readable secret file is refused" bash -c "[[ '$(status last_result)' == *'unlock-passphrase has mode 644'* ]]"
check "and nothing is built" bash -c "! grep -q cargo '$T/calls'"

# 11. Never from a CI runner's checkout.
AD4M_STAGING_REPO=$HOME/ci-workdir-1 run
check "a CI workdir as source repo is refused" grep -q "refusing to create worktrees from a CI workdir" "$T/out"

# 12. The very first build fails the gate: staging stays stopped, no data dir.
export AD4M_STAGING_STATE=$T/state2 AD4M_STAGING_DATA=$T/data2 AD4M_STAGING_SRC=$T/src2
chmod 600 "$T/config/secrets/unlock-passphrase"
touch "$T/bad-$(git -C "$T/work" rev-parse HEAD)"
run
check "a failed first deploy stops staging" [ "$(jq -r .last_result "$T/state2/status.json")" = \
  "rolled_back: $(git -C "$T/work" rev-parse HEAD) failed the gate; there is no previous build, staging is stopped" ]
check "and leaves no current build" [ ! -e "$T/state2/current" ]
check "and no data dir" [ ! -e "$T/data2" ]

# 13. A node that never had an agent passes the gate on /health alone.
export AD4M_STAGING_STATE=$T/state3 AD4M_STAGING_DATA=$T/data3 AD4M_STAGING_SRC=$T/src3
echo no-agent >"$T/agent"
rm "$T/bad-$(git -C "$T/work" rev-parse HEAD)"
run
check "a node without an agent deploys" [ "$(jq -r .last_result "$T/state3/status.json")" = "deployed (no agent yet)" ]

# 14. A failed gate, and then the restore of the snapshot fails: start
# nothing, rather than the old build on a torn data dir.
export AD4M_STAGING_STATE=$T/state4 AD4M_STAGING_DATA=$T/data4 AD4M_STAGING_SRC=$T/src4
echo unlocked >"$T/agent"
one=$(git -C "$T/work" rev-parse HEAD)
run
check "a fresh node deploys" [ "$(jq -r .deployed_sha "$T/state4/status.json")" = "$one" ]
torn=$(commit "fails the gate, restore fails" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$torn" "$T/fail-restore"
run
rm "$T/fail-restore"
check "a failed restore exits non-zero" exited_non_zero
check "a failed restore starts nothing" [ ! -e "$T/running" ]
check "a failed restore leaves no current build" [ ! -e "$T/state4/current" ]
check "a failed restore leaves no partial data dir" [ ! -e "$T/data4" ]
check "a failed restore is recorded" [ "$(jq -r .last_result "$T/state4/status.json")" = \
  "error: could not restore the data of $one; staging is stopped" ]
check "the failed build's data is kept aside" grep -q "$torn" "$T/data4.failed/written-by"
check "the snapshot is kept" bash -c "find '$T/state4/snapshots' -mindepth 1 -maxdepth 1 -name '*-$one' | grep -q ."
# The next commit is not deployed while current names no build.
commit "after the failed restore" "AD4M_UNLOCK_PASSPHRASE_FILE" >/dev/null
run
check "no deploy after a failed restore" exited_non_zero
check "and nothing is built or started" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
check "and the restore error stays in status.json" [ "$(jq -r .last_result "$T/state4/status.json")" = \
  "error: could not restore the data of $one; staging is stopped" ]

# 15. A rollback by hand whose restore fails: exit non-zero, start nothing.
export AD4M_STAGING_STATE=$T/state5 AD4M_STAGING_DATA=$T/data5 AD4M_STAGING_SRC=$T/src5
push_staging() { git -C "$T/work" push -q -f origin "$1:refs/heads/staging"; }
push_staging "$one"
run
two=$(commit "second build" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "two builds deploy" [ "$(jq -r '.deployed_sha + "/" + .previous_sha' "$T/state5/status.json")" = "$two/$one" ]
touch "$T/fail-restore"
run rollback
rm "$T/fail-restore"
check "a rollback with a failed restore exits non-zero" exited_non_zero
check "and starts nothing" [ ! -e "$T/running" ]
check "and leaves no current build" [ ! -e "$T/state5/current" ]
check "and says so" [ "$(jq -r .last_result "$T/state5/status.json")" = \
  "error: could not restore the data of $one; staging is stopped" ]
check "and deployed_sha stays" [ "$(jq -r .deployed_sha "$T/state5/status.json")" = "$two" ]
run
check "the next run after it exits non-zero" exited_non_zero
check "and keeps the restore error" [ "$(jq -r .last_result "$T/state5/status.json")" = \
  "error: could not restore the data of $one; staging is stopped" ]

# 16. A rollback by hand that cannot move the data dir aside: the snapshot is
# not copied into it, nothing starts.
export AD4M_STAGING_STATE=$T/state6 AD4M_STAGING_DATA=$T/data6 AD4M_STAGING_SRC=$T/src6
push_staging "$one"
run
push_staging "$two"
run
touch "$T/fail-move"
run rollback
rm "$T/fail-move"
check "a failed move exits non-zero" exited_non_zero
check "and starts nothing" [ ! -e "$T/running" ]
check "and copies no snapshot into the data dir" \
  [ "$(find "$T/data6" -mindepth 1 -maxdepth 1 -type d | wc -l)" = 0 ]
check "and says so" [ "$(jq -r .last_result "$T/state6/status.json")" = \
  "error: could not restore the data of $one; staging is stopped" ]

# 17. A deploy killed during its gate: the next run does not snapshot the new
# build's data under the old build's name; it rolls back as for a failed gate.
export AD4M_STAGING_STATE=$T/state7 AD4M_STAGING_DATA=$T/data7 AD4M_STAGING_SRC=$T/src7
run
killed=$(commit "killed during the gate" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/kill-gate"
run
flock "$T/state7/update.lock" true
check "the killed deploy left the new build running" \
  [ "$(cat "$T/running")/$(jq -r .deployed_sha "$T/state7/status.json")" = "$killed/$two" ]
run
check "the next run rolls the interrupted deploy back" [ "$(cat "$T/running")" = "$two" ]
check "and records it as failed" [ "$(jq -r .last_failed_sha "$T/state7/status.json")" = "$killed" ]
check "and says so" [ "$(jq -r .last_result "$T/state7/status.json")" = \
  "rolled_back: the deploy of $killed was interrupted; $two runs again" ]
check "and restores the old build's data" bash -c "! grep -q '$killed' '$T/data7/written-by'"
check "and leaves no deploy in flight" [ "$(jq -r .in_flight "$T/state7/status.json")" = null ]
next=$(commit "fails after the interrupted one" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$next"
run
check "a later failed gate rolls back to data without the interrupted build's writes" \
  bash -c "[ \"\$(cat '$T/running')\" = '$two' ] && ! grep -q '$killed' '$T/data7/written-by'"

# 18. A rollback by hand while an update runs fails, instead of reporting success.
(
  flock 9
  run rollback
) 9>"$T/state7/update.lock"
check "a rollback while an update runs exits non-zero" exited_non_zero

# 19. status.json is lost on a healthy node: the next run touches nothing
# and leaves it to the operator, rather than taking `current` for an
# interrupted first deploy and moving the live data aside.
export AD4M_STAGING_STATE=$T/state8 AD4M_STAGING_DATA=$T/data8 AD4M_STAGING_SRC=$T/src8
push_staging "$one"
run
push_staging "$two"
run
rm "$T/state8/status.json"
before8=$(cat "$T/data8/written-by")
run
check "a lost status.json exits non-zero" exited_non_zero
check "and says why" [ "$(jq -r .last_result "$T/state8/status.json")" = \
  "error: current is $two, but status.json names no deployed build (an interrupted first deploy, or a lost status.json); left to the operator" ]
check "and stops nothing" bash -c "! grep -q systemctl '$T/calls'"
check "and keeps current" [ "$(readlink "$T/state8/current")" = "releases/$two" ]
check "and keeps the live data in place" [ "$(cat "$T/data8/written-by")" = "$before8" ]
check "and moves nothing aside" [ ! -e "$T/data8.failed" ]
commit "after status.json was lost" "AD4M_UNLOCK_PASSPHRASE_FILE" >/dev/null
run
check "the next commit is not deployed on top" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
check "and the live data stays" [ "$(cat "$T/data8/written-by")" = "$before8" ]

# 20. A status.json write fails during a rollback (errexit is off there):
# status.json keeps its last good content, and the next run keeps the
# deployed build and its data.
export AD4M_STAGING_STATE=$T/state9 AD4M_STAGING_DATA=$T/data9 AD4M_STAGING_SRC=$T/src9
push_staging "$one"
run
push_staging "$two"
run
nowrite=$(commit "fails the gate, status write fails" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$nowrite" "$T/fail-jq"
run
rm "$T/fail-jq"
check "a failed status write exits non-zero" exited_non_zero
check "and leaves status.json whole" [ "$(jq -r .deployed_sha "$T/state9/status.json")" = "$two" ]
check "and says it could not record the failed build" grep -q "could not record $nowrite as failed" "$T/out"
check "and still rolls back" [ "$(cat "$T/running")/$(readlink "$T/state9/current")" = "$two/releases/$two" ]
before9=$(cat "$T/data9/written-by")
run
check "the next run keeps the deployed build's data" \
  [ "$(head -n -1 "$T/data9/written-by" 2>/dev/null)" = "$before9" ]
check "and has the failed build recorded after its second gate" \
  [ "$(jq -r '.deployed_sha + "/" + .last_failed_sha' "$T/state9/status.json")" = "$two/$nowrite" ]
rm "$T/bad-$nowrite"

# 21. A rollback by hand killed during its gate is labelled as a rollback on
# the next run, and nothing is touched.
export AD4M_STAGING_STATE=$T/state10 AD4M_STAGING_DATA=$T/data10 AD4M_STAGING_SRC=$T/src10
push_staging "$one"
run
push_staging "$two"
run
touch "$T/kill-gate"
run rollback
flock "$T/state10/update.lock" true
before10=$(cat "$T/data10/written-by")
run
check "an interrupted rollback exits non-zero" exited_non_zero
check "and is named as a rollback" [ "$(jq -r .last_result "$T/state10/status.json")" = \
  "error: the rollback by hand from $two to $one was interrupted; left to the operator" ]
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and the data stays" [ "$(cat "$T/data10/written-by")" = "$before10" ]

# 22. A rollback by hand whose build fails the gate: the next run keeps that
# result instead of calling it an interrupted deploy.
export AD4M_STAGING_STATE=$T/state11 AD4M_STAGING_DATA=$T/data11 AD4M_STAGING_SRC=$T/src11
push_staging "$one"
run
push_staging "$two"
run
touch "$T/bad-$one"
run rollback
check "a rollback whose build fails the gate says so" [ "$(jq -r .last_result "$T/state11/status.json")" = \
  "rolled_back by hand: $two; $one failed the gate too (agent: locked)" ]
run
rm "$T/bad-$one"
check "the next run exits non-zero" exited_non_zero
check "and keeps the rollback's result" [ "$(jq -r .last_result "$T/state11/status.json")" = \
  "rolled_back by hand: $two; $one failed the gate too (agent: locked)" ]
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
# The newer build's data is in .failed; a second rollback would replace it.
echo marker11 >>"$T/data11.failed/written-by"
run rollback
check "a second rollback after a failed one is refused" exited_non_zero
check "and keeps the newer build's data aside" grep -q marker11 "$T/data11.failed/written-by"
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and keeps the rollback's result" [ "$(jq -r .last_result "$T/state11/status.json")" = \
  "rolled_back by hand: $two; $one failed the gate too (agent: locked)" ]

# 23. A rollback by hand killed while it copies the snapshot back: `current`
# is still the newer build, the data dir is half a snapshot. The next runs
# name it as an unfinished rollback and touch nothing, and a new commit is
# not deployed onto the half-copied data.
export AD4M_STAGING_STATE=$T/state12 AD4M_STAGING_DATA=$T/data12 AD4M_STAGING_SRC=$T/src12
push_staging "$one"
run
push_staging "$two"
run
touch "$T/kill-restore"
run rollback
flock "$T/state12/update.lock" true
check "the killed rollback left current at the newer build" \
  [ "$(readlink "$T/state12/current")/$(jq -r .deployed_sha "$T/state12/status.json")" = "releases/$two/$two" ]
run
check "a rollback killed in the restore stops the next run" exited_non_zero
check "and is named as a rollback" [ "$(jq -r .last_result "$T/state12/status.json")" = \
  "error: the rollback by hand from $two to $one was interrupted; left to the operator" ]
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and the half-copied data dir stays" [ "$(cat "$T/data12/written-by")" = partial ]
check "and status.json names the build whose data was moved aside" \
  [ "$(jq -r .failed_data "$T/state12/status.json")" = "$two" ]
commit "after the killed rollback" "AD4M_UNLOCK_PASSPHRASE_FILE" >/dev/null
run
check "the next commit is not deployed onto the half-copied data" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
check "and the newer build's data stays aside" grep -q "$two" "$T/data12.failed/written-by"
echo marker12 >>"$T/data12.failed/written-by"
run rollback
check "a second rollback after a killed one is refused" exited_non_zero
check "and keeps the newer build's data aside" grep -q marker12 "$T/data12.failed/written-by"
check "and leaves the half-copied data dir" [ "$(cat "$T/data12/written-by")" = partial ]
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and status.json still names the data aside" [ "$(jq -r .failed_data "$T/state12/status.json")" = "$two" ]
# The refusal must not overwrite how the unfinished rollback ended.
check "and keeps the interrupted rollback's result" [ "$(jq -r .last_result "$T/state12/status.json")" = \
  "error: the rollback by hand from $two to $one was interrupted; left to the operator" ]

# 24. A rollback by hand killed between the stop and moving the data aside:
# the data is whole, staging is down. The next run does not say "already
# deployed" and exit 0.
export AD4M_STAGING_STATE=$T/state13 AD4M_STAGING_DATA=$T/data13 AD4M_STAGING_SRC=$T/src13
push_staging "$one"
run
push_staging "$two"
run
before13=$(cat "$T/data13/written-by")
# failed_data left from an earlier rollback of the same build must not make
# this look like a rollback killed after the move.
seed_failed_data() { jq --arg s "$2" '.failed_data = $s' "$1/status.json" >"$1/status.json.new" && mv "$1/status.json.new" "$1/status.json"; }
seed_failed_data "$T/state13" "$two"
touch "$T/kill-move"
run rollback
flock "$T/state13/update.lock" true
run
check "a rollback killed before the move stops the next run" exited_non_zero
check "and is named as a rollback" [ "$(jq -r .last_result "$T/state13/status.json")" = \
  "error: the rollback by hand from $two to $one was interrupted; left to the operator" ]
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and the data stays in place" [ "$(cat "$T/data13/written-by")" = "$before13" ]
check "and status.json names no data moved aside" [ "$(jq -r .failed_data "$T/state13/status.json")" = null ]
# The runbook's third case: the data was not moved, so rolling back again
# is allowed and finishes.
run rollback
check "a rollback again after a kill before the move rolls back" \
  [ "$(jq -r '.deployed_sha + "/" + (.in_flight | tostring)' "$T/state13/status.json")/$(cat "$T/running")" = "$one/null/$one" ]
check "and keeps the newer build's data aside" [ "$(cat "$T/data13.failed/written-by")" = "$before13" ]

# 25. After a rollback by hand that worked, the timer deploys the next commit.
export AD4M_STAGING_STATE=$T/state14 AD4M_STAGING_DATA=$T/data14 AD4M_STAGING_SRC=$T/src14
push_staging "$one"
run
push_staging "$two"
run
run rollback
check "a rollback by hand leaves nothing in flight" [ "$(jq -r .in_flight "$T/state14/status.json")" = null ]
after=$(commit "after a rollback by hand" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "the next commit deploys after a rollback by hand" [ "$(jq -r .deployed_sha "$T/state14/status.json")" = "$after" ]

# 26. A slow status.json read before the gate (a loaded machine) does not use
# up the gate time: a healthy build is probed at least once.
export AD4M_STAGING_STATE=$T/state15 AD4M_STAGING_DATA=$T/data15 AD4M_STAGING_SRC=$T/src15
push_staging "$one"
touch "$T/slow-jq"
run
rm "$T/slow-jq"
check "a healthy build passes the gate after a slow read" [ "$(jq -r .last_result "$T/state15/status.json")" = deployed ]

# 27. A rollback by hand while a deploy killed in its gate is unfinished
# (`current` is the unchecked build) is refused: moving that data aside and
# rolling back past the deployed build would leave the deployed build's data
# only in a snapshot the next deploy prunes. The timer's next run recovers.
export AD4M_STAGING_STATE=$T/state16 AD4M_STAGING_DATA=$T/data16 AD4M_STAGING_SRC=$T/src16
push_staging "$one"
run
push_staging "$two"
run
three=$(commit "killed during the gate, then a rollback by hand" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/kill-gate"
run
flock "$T/state16/update.lock" true
before16=$(cat "$T/data16/written-by")
run rollback
check "a rollback during an unfinished deploy is refused" exited_non_zero
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and current stays" [ "$(readlink "$T/state16/current")" = "releases/$three" ]
check "and the data stays in place" [ "$(cat "$T/data16/written-by")" = "$before16" ]
check "and nothing is moved aside" [ ! -e "$T/data16.failed" ]
run
check "the timer's next run still rolls the deploy back" \
  [ "$(jq -r .last_result "$T/state16/status.json")" = "rolled_back: the deploy of $three was interrupted; $two runs again" ]

# 28. A rollback by hand after an automatic rollback could not restore the
# data is refused: the deployed build's data is only in its snapshot, which
# the operator restores (runbook: Roll back).
export AD4M_STAGING_STATE=$T/state17 AD4M_STAGING_DATA=$T/data17 AD4M_STAGING_SRC=$T/src17
push_staging "$one"
run
push_staging "$two"
run
torn17=$(commit "fails the gate, restore fails, then a rollback by hand" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$torn17" "$T/fail-restore"
run
rm "$T/fail-restore" "$T/bad-$torn17"
check "the automatic restore failed" [ ! -e "$T/state17/current" ]
run rollback
check "a rollback after a failed restore is refused" exited_non_zero
check "and nothing is stopped or started" bash -c "! grep -q systemctl '$T/calls'"
check "and starts no build" [ ! -e "$T/state17/current" ]
check "and keeps the restore error" [ "$(jq -r .last_result "$T/state17/status.json")" = \
  "error: could not restore the data of $two; staging is stopped" ]
check "and keeps the failed build's data aside" grep -q "$torn17" "$T/data17.failed/written-by"

# 29. Neither kind of rollback leaves a stale failed_data in place until its
# own move: a rollback by hand killed as it stops staging (which keeps
# running), and a failed gate's rollback killed before it moves the data.
export AD4M_STAGING_STATE=$T/state18 AD4M_STAGING_DATA=$T/data18 AD4M_STAGING_SRC=$T/src18
push_staging "$one"
run
push_staging "$two"
run
seed_failed_data "$T/state18" "$two"
touch "$T/kill-stop"
run rollback
flock "$T/state18/update.lock" true
check "a rollback by hand killed at the stop leaves staging running" [ "$(cat "$T/running")" = "$two" ]
check "and status.json names no data moved aside" [ "$(jq -r .failed_data "$T/state18/status.json")" = null ]
export AD4M_STAGING_STATE=$T/state19 AD4M_STAGING_DATA=$T/data19 AD4M_STAGING_SRC=$T/src19
push_staging "$one"
run
bad19=$(commit "fails the gate, killed before the move" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$bad19"
seed_failed_data "$T/state19" "$bad19"
touch "$T/kill-move"
run
flock "$T/state19/update.lock" true
rm "$T/bad-$bad19"
check "a failed gate's rollback killed before the move names no data moved aside" \
  [ "$(jq -r .failed_data "$T/state19/status.json")" = null ]

# 30. A deploy killed while the deployed build was stopped: after a failed
# gate's rollback pointed `current` back but before it started the build,
# and before the swap, during the snapshot. The next run starts the
# deployed build again instead of skipping the head and leaving staging
# down until the branch moves.
export AD4M_STAGING_STATE=$T/state20 AD4M_STAGING_DATA=$T/data20 AD4M_STAGING_SRC=$T/src20
push_staging "$one"
run
bad20=$(commit "fails the gate, rollback killed before the start" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/bad-$bad20"
echo 2 >"$T/kill-start"
run
flock "$T/state20/update.lock" true
check "a rollback killed before its start leaves current at the deployed build, stopped" \
  bash -c "[ \"\$(readlink '$T/state20/current')\" = 'releases/$one' ] && [ ! -e '$T/running' ]"
check "and a deploy in flight" \
  [ "$(jq -r '.in_flight + "/" + .last_failed_sha' "$T/state20/status.json")" = "deploy/$bad20" ]
run
check "the next run starts the deployed build again" [ "$(cat "$T/running")" = "$one" ]
check "and says so" [ "$(jq -r .last_result "$T/state20/status.json")" = \
  "error: the deploy of $bad20 was interrupted while $one was stopped; $one runs again" ]
check "and leaves no deploy in flight" [ "$(jq -r .in_flight "$T/state20/status.json")" = null ]
check "and does not build the failed head again" bash -c "! grep -q cargo '$T/calls'"
check "and the data is the deployed build's, without the failed build's writes" \
  bash -c "! grep -q '$bad20' '$T/data20/written-by'"
run
check "the run after that changes nothing" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
rm "$T/bad-$bad20"
snap20=$(commit "killed during the snapshot" "AD4M_UNLOCK_PASSPHRASE_FILE")
touch "$T/kill-snapshot"
run
flock "$T/state20/update.lock" true
check "a deploy killed during the snapshot leaves the deployed build stopped" \
  bash -c "[ \"\$(readlink '$T/state20/current')\" = 'releases/$one' ] && [ ! -e '$T/running' ]"
run
check "the next run starts the deployed build before it builds" \
  [ "$(head -n 1 "$T/calls")" = "systemctl --user start ad4m-staging.service" ]
check "and deploys the head" [ "$(jq -r '.deployed_sha + "/" + .previous_sha' "$T/state20/status.json")" = "$snap20/$one" ]
check "and the partial snapshot is gone" \
  [ "$(find "$T/state20/snapshots" -mindepth 1 -maxdepth 1 -name "*-$one" | wc -l)" = 1 ]
check "and the kept snapshot is whole" \
  bash -c "! grep -q partial '$T/state20/snapshots/'*-$one/written-by"

echo "$failures failed"
((failures == 0))
