#!/usr/bin/env bash
# Runs deploy/staging/update.sh against a throwaway git remote, with pnpm,
# cargo, systemctl, curl, node and sleep replaced by stubs, and checks each
# decision it makes: wait, deploy, skip, roll back, keep a failed build out.
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
echo "systemctl $*" >>"$STUB_DIR/calls"
case $2 in
  stop) rm -f "$STUB_DIR/running" ;;
  start)
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
# agent.mjs status: a build marked bad never unlocks.
sha=$(cat "$STUB_DIR/running")
if [[ -f $STUB_DIR/bad-$sha ]]; then echo locked; else cat "$STUB_DIR/agent"; fi
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
run() { : >"$T/calls"; "$UPDATE" "$@" >"$T/out" 2>&1 || echo "exit $?" >>"$T/out"; }
called() { grep -q "$1" "$T/calls"; }

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

# 7. Housekeeping after several deploys: two snapshots, releases current + previous.
last=$(commit "next" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "a later deploy succeeds" [ "$(status deployed_sha)" = "$last" ]
check "two snapshots are kept" [ "$(find "$T/state/snapshots" -mindepth 1 -maxdepth 1 | wc -l)" = 2 ]
check "only current and previous releases are kept" \
  [ "$(find "$T/state/releases" -mindepth 1 -maxdepth 1 -printf '%f\n' | sort | tr '\n' ' ')" = "$(printf '%s\n' "$bad" "$last" | sort | tr '\n' ' ')" ]

# 8. No agent yet passes the gate on /health alone.
echo no-agent >"$T/agent"
fresh=$(commit "fresh" "AD4M_UNLOCK_PASSPHRASE_FILE")
run
check "a node without an agent deploys" [ "$(status last_result)" = "deployed (no agent yet)" ]
check "and records the SHA" [ "$(status deployed_sha)" = "$fresh" ]

# 9. Rollback by hand: the previous build with the data it had.
run rollback
check "rollback runs the previous build" [ "$(cat "$T/running")" = "$last" ]
check "rollback records it" [ "$(status deployed_sha)/$(status last_failed_sha)" = "$last/$fresh" ]
check "rollback result" [ "$(status last_result)" = "rolled_back by hand: $fresh; $last runs again" ]
snap=$(find "$T/state/snapshots" -mindepth 1 -maxdepth 1 -name "*-$last")
check "rollback restores the previous build's data" \
  [ "$(head -n -1 "$T/data/written-by")" = "$(cat "$snap/written-by")" ]
check "the rolled-back build's data is kept aside" [ "$(tail -n 1 "$T/data.failed/written-by")" = "$fresh" ]
run
check "the timer does not redeploy the rolled-back SHA" bash -c "! grep -qE 'cargo|systemctl' '$T/calls'"
run rollback
check "a second rollback has nothing to go back to" [ "$(status last_result)" = "error: no previous build to roll back to" ]

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

echo "$failures failed"
((failures == 0))
