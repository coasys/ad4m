#!/usr/bin/env bash
# Runs deploy/coasys-cloud/coasys-cloud against a scratch tree with the release
# download, systemctl, runuser, useradd, curl, node and the executor replaced by
# stubs, and checks each decision it makes: install with no data and nothing
# started; a first deploy (sealed start, unlock, gate); a second deploy (previous
# kept, rollback copy taken); a failed gate (previous release back, data
# restored, the failed run's data set aside, unlocked again); a release whose
# sha256 does not match (nothing stopped); a secret of the wrong shape (nothing
# stopped); a hand rollback; `unlock` on a sealed executor and on an open one;
# `status`; a settings file with an unset value; a tag that is a path; the
# unit's restart and hardening lines. Everywhere: the storage secret is in no
# argument, no environment and no file the script or its stubs write.
# Needs bash, flock and GNU coreutils. Usage: coasys-cloud.test.sh
set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
SCRIPT=$HERE/../coasys-cloud
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

SECRET=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
WRONG=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

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

# --- Stubs: they log to $T/calls and never write the secret down.
mkdir -p "$T/stubs" "$T/fixtures" "$T/state"
cat >"$T/stubs/systemctl" <<'EOF'
#!/usr/bin/env bash
# `start` runs whatever `current` points at, sealed; `stop` ends it.
echo "systemctl $*" >>"$STUB_DIR/calls"
case $1 in
  stop) rm -f "$STUB_DIR/running" "$STUB_DIR/open" ;;
  start)
    tag=$(basename "$(readlink "$COASYS_CLOUD_OPT/current")")
    echo "$tag" >"$STUB_DIR/running"
    rm -f "$STUB_DIR/open"
    mkdir -p "$COASYS_CLOUD_DATA"
    echo "$tag" >>"$COASYS_CLOUD_DATA/written-by" ;;
esac
EOF
cat >"$T/stubs/runuser" <<'EOF'
#!/usr/bin/env bash
# runuser -u <user> -- <command...>
echo "runuser $2" >>"$STUB_DIR/calls"
shift 3
exec "$@"
EOF
cat >"$T/stubs/useradd" <<'EOF'
#!/usr/bin/env bash
echo "useradd $*" >>"$STUB_DIR/calls"
EOF
cat >"$T/stubs/id" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF
cat >"$T/stubs/chown" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
cat >"$T/stubs/curl" <<'EOF'
#!/usr/bin/env bash
# /health: up while a release runs. A download: a fixture of the tag in the URL.
if [[ $* == */health ]]; then [[ -f $STUB_DIR/running ]]; exit; fi
out=""
while (($#)); do
  case $1 in -o) out=$2; shift 2 ;; *) url=$1; shift ;; esac
done
echo "curl $url" >>"$STUB_DIR/calls"
fixture=$STUB_DIR/../fixtures/${url#"$COASYS_CLOUD_RELEASE_BASE"/}
[[ -f $fixture ]] || exit 22
cp "$fixture" "$out"
EOF
cat >"$T/stubs/node" <<'EOF'
#!/usr/bin/env bash
# agent.mjs status: unlocked once storage is open, unless the release is marked bad.
tag=$(cat "$STUB_DIR/running")
if [[ -f $STUB_DIR/open && ! -f $STUB_DIR/bad-$tag ]]; then echo unlocked; else echo locked; fi
EOF
# The release binary: storage status and unlock, checking the secret came by a pipe.
cat >"$T/fixtures/ad4m-executor.stub" <<'EOF'
#!/usr/bin/env bash
if [[ $1 == storage && $2 == status ]]; then
  if [[ -f $STUB_DIR/open ]]; then echo "Storage open (keyring k1)"; else echo "Storage sealed (keyring k1): waiting"; fi
  exit 0
fi
if [[ $1 == storage && $2 == unlock ]]; then
  read -r given
  if (($# != 4)) || [[ $3 != --executor-url ]]; then echo "unlock args: $*" >>"$STUB_DIR/calls"; fi
  if [[ $* == *"$STUB_SECRET"* ]] || env -u STUB_SECRET | grep -q "$STUB_SECRET"; then echo "SECRET LEAKED" >>"$STUB_DIR/calls"; fi
  if [[ $given != "$STUB_SECRET" ]]; then echo "unlock refused" >>"$STUB_DIR/calls"; exit 1; fi
  echo "unlock ok $(cat "$STUB_DIR/running")" >>"$STUB_DIR/calls"
  touch "$STUB_DIR/open"
  echo "Storage open (keyring k1), secret fingerprint abc."
  exit 0
fi
exit 2
EOF
chmod +x "$T/stubs/"* "$T/fixtures/ad4m-executor.stub"

ASSET=ad4m-executor-x86_64-unknown-linux-gnu
release() { # release <tag> [sha]: a fixture release whose sha256 is the stub's (or <sha>)
  mkdir -p "$T/fixtures/$1"
  { cat "$T/fixtures/ad4m-executor.stub"; echo "# $1"; } >"$T/fixtures/$1/$ASSET"
  echo "${2:-$(sha256sum "$T/fixtures/$1/$ASSET" | awk '{print $1}')}  $ASSET" >"$T/fixtures/$1/$ASSET.sha256"
}

export STUB_DIR=$T/state STUB_SECRET=$SECRET
export PATH=$T/stubs:$PATH
export COASYS_CLOUD_OPT=$T/opt COASYS_CLOUD_HOME=$T/home COASYS_CLOUD_DATA=$T/home/data
export COASYS_CLOUD_ROLLBACK=$T/home/rollback COASYS_CLOUD_CONFIG=$T/etc
export COASYS_CLOUD_UNIT_DIR=$T/units COASYS_CLOUD_NGINX_DIR=$T/nginx
export COASYS_CLOUD_RELEASE_BASE=https://releases.test
export COASYS_CLOUD_GATE_SECONDS=2 COASYS_CLOUD_POLL_SECONDS=0.1
export COASYS_CLOUD_LOCK=$T/lock COASYS_CLOUD_AGENT_MJS=$T/agent.mjs
export COASYS_CLOUD_SECRET_INPUT=$T/secret-input
printf '%s\n' "$SECRET" >"$COASYS_CLOUD_SECRET_INPUT"
mkdir -p "$T/opt"

run() { "$SCRIPT" "$@" >"$T/out" 2>&1; }
fails() { ! run "$@"; }
tag_of() { basename "$(readlink "$COASYS_CLOUD_OPT/$1")"; }
calls() { cat "$T/state/calls" 2>/dev/null || true; }
called() { calls | grep -q -- "$1"; }
reset_calls() { : >"$T/state/calls"; }
mark() { calls | wc -l; }

release v1
release v2
release v3
release vbad-sha bad
touch "$T/state/bad-v3"

# --- install: user, directories, unit, vhost, release; no data, nothing started.
check "install makes the user, the layout and the release" run install v1
check "install made a system user with no login shell" \
  grep -q "useradd --system.*--shell /usr/sbin/nologin coasys-cloud" "$T/state/calls"
check "install put the release under releases/v1" test -x "$T/opt/releases/v1/ad4m-executor"
check "install pointed no link and started nothing" bash -c "
  [[ ! -e $T/opt/current ]] && ! grep -q 'systemctl start' $T/state/calls"
check "install wrote the unit, the settings and the vhost" bash -c "
  [[ -f $T/units/coasys-cloud.service && -f $T/etc/executor-config.json && -f $T/nginx/coasys-cloud ]]"
check "install left the data directory empty and private" bash -c "
  [[ -d $T/home/data && -z \$(ls -A $T/home/data) && \$(stat -c %a $T/home/data) == 700 ]]"

# --- A settings file with an unset value stops a deploy before it touches anything.
check "deploy refuses a settings file with a REPLACE-WITH value" fails deploy v1
check "  ...and stopped nothing" bash -c "! grep -q 'systemctl' $T/state/calls || ! grep -q 'systemctl stop' $T/state/calls"
config=$(<"$T/etc/executor-config.json")
printf '%s\n' "${config//REPLACE-WITH-THE-STORE-HEAD-KEY/z6MkTestKey}" >"$T/etc/executor-config.json"

# --- Refusals that stop nothing.
reset_calls
check "a tag that is a path is refused" fails deploy ../v1
check "a release whose sha256 differs is not installed" fails deploy vbad-sha
check "  ...and nothing was stopped or started" bash -c "! grep -q 'systemctl' $T/state/calls"
check "  ...and it left no release" bash -c "[[ ! -e $T/opt/releases/vbad-sha ]]"
printf 'not-a-secret\n' >"$COASYS_CLOUD_SECRET_INPUT"
check "a secret of the wrong shape is refused" fails deploy v1
check "  ...before anything was stopped" bash -c "! grep -q 'systemctl stop' $T/state/calls"
printf '%s\n' "$SECRET" >"$COASYS_CLOUD_SECRET_INPUT"

# --- First deploy: no data, no previous; starts sealed, unlocks, passes the gate.
reset_calls
check "the first deploy passes its gate" run deploy v1
check "  current is v1 and there is no previous" bash -c "
  [[ \$(basename \$(readlink $T/opt/current)) == v1 && ! -e $T/opt/previous ]]"
check "  the executor was unlocked once, with the secret by a pipe" bash -c "
  [[ \$(grep -c 'unlock ok v1' $T/state/calls) == 1 ]]"
check "  no argument or environment held the secret, and nothing was asked of the executor but its url" bash -c "
  ! grep -q 'SECRET LEAKED\|unlock args' $T/state/calls"
check "  the secret was written to no file" bash -c "
  ! grep -rq $SECRET $T --exclude=secret-input --exclude=ad4m-executor.stub --exclude=ad4m-executor --exclude=$ASSET"

# --- Second deploy: previous kept, the data copied first.
reset_calls
check "the second deploy passes its gate" run deploy v2
check "  current is v2, previous is v1" bash -c "
  [[ \$(basename \$(readlink $T/opt/current)) == v2 && \$(basename \$(readlink $T/opt/previous)) == v1 ]]"
check "  the rollback copy holds the data as v1 left it" bash -c "
  [[ \$(cat $T/home/rollback/written-by) == v1 ]]"
check "  the data was copied while the executor was stopped" bash -c "
  [[ \$(grep -n 'systemctl stop' $T/state/calls | head -1 | cut -d: -f1) -lt \$(grep -n 'systemctl start' $T/state/calls | head -1 | cut -d: -f1) ]]"
check "deploying the deployed tag is refused" fails deploy v2

# --- status.
check "status names the tags, health, storage and the rollback copy" bash -c "
  $SCRIPT status | tr -s ' ' | grep -q 'deployed: v2' &&
  $SCRIPT status | tr -s ' ' | grep -q 'previous: v1' &&
  $SCRIPT status | grep -q 'health: *ok' &&
  $SCRIPT status | grep -q 'storage: *open' &&
  $SCRIPT status | grep -q 'rollback copy: *yes'"

# --- A failed gate: v1's predecessor is v2; v3 never unlocks the agent.
reset_calls
check "a release that fails its gate is rolled back" fails deploy v3
check "  current is v2 again" bash -c "[[ \$(basename \$(readlink $T/opt/current)) == v2 ]]"
check "  v2 runs again and was unlocked with the secret held in memory" bash -c "
  [[ \$(cat $T/state/running) == v2 ]] && grep -q 'unlock ok v2' $T/state/calls"
check "  the data is the copy taken before v3: no v3 writes" bash -c "
  ! grep -q v3 $T/home/data/written-by && grep -q v2 $T/home/data/written-by"
check "  the failed run's data is set aside, not deleted" bash -c "grep -q v3 $T/home/data.set-aside/written-by"
check "  the message says it rolled back" grep -q "rolled back to v2" "$T/out"

# --- A hand rollback: v2 -> v1 with the data copied before v2... which the failed v3 deploy consumed,
# so a new deploy first gives a copy to come back to.
check "rollback refuses without a rollback copy" fails rollback
reset_calls
check "a deploy of v3 after marking it good passes" bash -c "
  rm $T/state/bad-v3; $SCRIPT deploy v3 >$T/out 2>&1"
check "  rollback by hand returns to v2 with the data before v3" bash -c "
  $SCRIPT rollback >$T/out 2>&1 &&
  [[ \$(basename \$(readlink $T/opt/current)) == v2 && \$(basename \$(readlink $T/opt/previous)) == v3 ]] &&
  ! grep -q v3 $T/home/data/written-by"
check "  and unlocked it" bash -c "grep -q 'unlock ok v2' $T/state/calls && [[ -f $T/state/open ]]"

# --- unlock on a sealed executor, and on an open one.
bash -c "systemctl stop x; systemctl start x" >/dev/null 2>&1
reset_calls
check "unlock opens a sealed executor" run unlock
check "  it sent the secret" bash -c "grep -q 'unlock ok' $T/state/calls"
reset_calls
check "unlock on an open executor sends nothing" run unlock
check "  no unlock call" bash -c "! grep -q 'unlock ok' $T/state/calls"
bash -c "systemctl stop x; systemctl start x" >/dev/null 2>&1
check "status says when the executor is sealed" bash -c "$SCRIPT status | grep -q 'storage: *sealed'"
printf '%s\n' "$WRONG" >"$COASYS_CLOUD_SECRET_INPUT"
check "a wrong secret is refused by the executor" fails unlock
check "  and it stays sealed" bash -c "[[ ! -f $T/state/open ]]"
printf '%s\n' "$SECRET" >"$COASYS_CLOUD_SECRET_INPUT"

# --- The unit.
unit=$HERE/../coasys-cloud.service
check "the unit restarts on failure, forces 75, never restarts 78 and 79" bash -c "
  grep -q '^Restart=on-failure' $unit && grep -q '^RestartForceExitStatus=75' $unit &&
  grep -q '^RestartPreventExitStatus=78 79' $unit"
check "the unit is hardened as AR-09 4.12.1 says" bash -c "
  grep -q '^ProtectSystem=strict' $unit && grep -q '^ProtectHome=yes' $unit &&
  grep -q '^PrivateTmp=yes' $unit && grep -q '^NoNewPrivileges=yes' $unit &&
  grep -q '^MemoryDenyWriteExecute=no' $unit && grep -q '^LimitCORE=0' $unit &&
  grep -q '^User=coasys-cloud' $unit"
check "the unit gives the executor no storage secret and no unlock passphrase" bash -c "
  ! grep -v '^#' $unit | grep -q 'AD4M_STORAGE_SECRET\|AD4M_UNLOCK_PASSPHRASE'"
check "the unit takes the admin credential from an encrypted credential" \
  grep -q '^LoadCredentialEncrypted=admin-credential:' "$unit"

if ((failures > 0)); then
  echo "$failures check(s) failed"
  exit 1
fi
echo "all checks passed"
