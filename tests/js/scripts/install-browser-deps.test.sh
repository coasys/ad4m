#!/bin/bash
# Tests for install-browser-deps.sh with a fake sudo, apt-get and apt-cache; no root, no
# network, about a second. The point: a run that finds apt busy (another job on the runner
# host holds its lock) waits and goes on, and any other apt error still fails it.
set -uo pipefail

SCRIPT="${INSTALL_BROWSER_DEPS:-$(cd "$(dirname "$0")" && pwd)/install-browser-deps.sh}"
FAILED=0
fail() {
  echo "  FAIL: $*"
  FAILED=1
}

setup() {
  TMP="$(mktemp -d)"
  mkdir -p "$TMP/bin" "$TMP/browsers"
  printf '#!/bin/bash\nexec "$@"\n' >"$TMP/bin/sudo"
  printf '#!/bin/bash\necho "  Candidate: 1.0"\n' >"$TMP/bin/apt-cache"
  # Busy for the first FAKE_APT_BUSY calls, then FAKE_APT_ERROR (an error other than a lock) or success.
  cat >"$TMP/bin/apt-get" <<'EOF'
#!/bin/bash
n=$(($(cat "$TMP/calls" 2>/dev/null || echo 0) + 1))
echo "$n" >"$TMP/calls"
echo "$*" >>"$TMP/apt.log"
if [ "$n" -le "${FAKE_APT_BUSY:-0}" ]; then
  echo "E: Could not get lock /var/lib/apt/lists/lock. It is held by process 587776 (apt-get)"
  echo "E: Unable to lock directory /var/lib/apt/lists/"
  exit 100
fi
if [ -n "${FAKE_APT_ERROR:-}" ]; then echo "E: $FAKE_APT_ERROR"; exit 100; fi
EOF
  chmod +x "$TMP/bin/"*
  export TMP PATH="$TMP/bin:$PATH" PLAYWRIGHT_BROWSERS_PATH="$TMP/browsers" APT_BUSY_WAIT_S=0
}

test_waits_while_apt_is_busy() {
  out="$(FAKE_APT_BUSY=2 bash "$SCRIPT" 2>&1)" || fail "a busy apt failed the install: $out"
  [ "$(grep -c "apt is busy" <<<"$out")" = 2 ] || fail "did not wait twice: $out"
  grep -q "^-o DPkg::Lock::Timeout=600 update" "$TMP/apt.log" || fail "no apt-get update: $(cat "$TMP/apt.log")"
  grep -q "^-o DPkg::Lock::Timeout=600 install .*xvfb" "$TMP/apt.log" || fail "no apt-get install: $(cat "$TMP/apt.log")"
}

test_another_error_fails_at_once() {
  out="$(FAKE_APT_ERROR="Unable to fetch some archives" bash "$SCRIPT" 2>&1)" && fail "an apt error passed: $out"
  [ "$(cat "$TMP/calls")" = 1 ] || fail "an apt error was tried $(cat "$TMP/calls") times"
  grep -q "Unable to fetch some archives" <<<"$out" || fail "the apt error is not shown: $out"
}

test_gives_up_when_apt_stays_busy() {
  out="$(FAKE_APT_BUSY=1000 bash "$SCRIPT" 2>&1)" && fail "a lock that never freed passed"
  [ "$(cat "$TMP/calls")" = 60 ] || fail "tried $(cat "$TMP/calls") times, not 60"
  grep -q "Could not get lock" <<<"$out" || fail "the lock error is not shown: $out"
}

for t in test_waits_while_apt_is_busy test_another_error_fails_at_once test_gives_up_when_apt_stays_busy; do
  echo "$t"
  setup
  "$t"
  rm -rf "$TMP"
done

if [[ "$FAILED" != 0 ]]; then
  echo "install-browser-deps tests: FAILED"
  exit 1
fi
echo "install-browser-deps tests: passed"
