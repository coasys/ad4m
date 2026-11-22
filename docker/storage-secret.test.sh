#!/bin/bash
# Each case sets its variables in a subshell of its own, on purpose.
# shellcheck disable=SC2030,SC2031
# Tests of docker/storage-secret.sh (the storage secret on the data volume):
# bash docker/storage-secret.test.sh
# `ad4m-executor` here is a stub printing a fixed secret for `storage new-secret` (and
# recording it ran), so each case reads what the entrypoint would have done.
set -u

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="${STORAGE_SECRET_SCRIPT:-${here}/storage-secret.sh}"
failures=0
secret="0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

state=""
fresh_state() {
    state=$(mktemp -d)
    mkdir -p "${state}/bin" "${state}/data"
    cat >"${state}/bin/ad4m-executor" <<EOF
#!/bin/sh
echo "\$*" >> "${state}/calls"
printf '%s\nKeep this secret safe, away from the data folder'"'"'s disk.\n' "${secret}"
EOF
    chmod +x "${state}/bin/ad4m-executor"
    PATH="${state}/bin:${PATH}"
    : > "${state}/calls"
    unset AD4M_STORAGE_SECRET_FILE AD4M_ALLOW_SECRET_ON_DATA_DISK
}

check() {
    local what="$1" expected="$2" actual="$3"
    if [ "${expected}" = "${actual}" ]; then
        echo "ok - ${what}"
    else
        echo "not ok - ${what}"
        echo "  expected: ${expected}"
        echo "  actual:   ${actual}"
        failures=$((failures + 1))
    fi
}

# ── A first start makes the secret on the data volume ───────────────────────
fresh_state
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
    echo "file=${AD4M_STORAGE_SECRET_FILE:-} disk=${AD4M_ALLOW_SECRET_ON_DATA_DISK:-}"
) >"${state}/out"
check "the secret file is exported, allowed on the data disk" \
    "file=${state}/data/storage-secret disk=true" "$(tail -n 1 "${state}/out")"
check "the file holds the secret" "${secret}" "$(cat "${state}/data/storage-secret")"
check "the file is owner-only" "600" "$(stat -c %a "${state}/data/storage-secret")"
check "new-secret ran once" "storage new-secret" "$(cat "${state}/calls")"
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
    echo "file=${AD4M_STORAGE_SECRET_FILE:-} disk=${AD4M_ALLOW_SECRET_ON_DATA_DISK:-}"
) >"${state}/out"
check "a restart reuses the file" \
    "file=${state}/data/storage-secret disk=true" "$(cat "${state}/out")"
check "  and makes no new secret" "1" "$(grep -c 'new-secret' "${state}/calls")"

# ── An existing file is never overwritten ───────────────────────────────────
fresh_state
printf 'deadbeef%.0s' {1..8} >"${state}/data/storage-secret"
printf '\n' >>"${state}/data/storage-secret"
chmod 600 "${state}/data/storage-secret"
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
)
check "the existing secret is kept" "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef" \
    "$(cat "${state}/data/storage-secret")"
check "  and no new secret was made" "" "$(cat "${state}/calls")"

# ── Sealed data without the secret refuses the start ────────────────────────
fresh_state
mkdir -p "${state}/data/ad4m"
printf '{}' >"${state}/data/ad4m/keyring.json"
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
) >"${state}/out" 2>&1
rc=$?
check "a sealed data folder without the file is refused" "1" "${rc}"
check "the refusal says why" \
    "1" "$(grep -c 'sealed data exists' "${state}/out")"
check "no new secret was made" "" "$(cat "${state}/calls")"
check "no file was written" "no" \
    "$([ -e "${state}/data/storage-secret" ] && echo yes || echo no)"
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
    echo "file=${AD4M_STORAGE_SECRET_FILE:-unset}"
) >"${state}/out" 2>/dev/null
check "the variables stay unset" "file=unset" "$(cat "${state}/out")"

# ── An operator's AD4M_STORAGE_SECRET_FILE wins ─────────────────────────────
fresh_state
mkdir -p "${state}/secrets"
printf '%s\n' "${secret}" >"${state}/secrets/storage-secret"
(
    export AD4M_STORAGE_SECRET_FILE="${state}/secrets/storage-secret"
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
    echo "file=${AD4M_STORAGE_SECRET_FILE:-} disk=${AD4M_ALLOW_SECRET_ON_DATA_DISK:-unset}"
) >"${state}/out"
check "the operator's file is kept" \
    "file=${state}/secrets/storage-secret disk=unset" "$(cat "${state}/out")"
check "  and nothing was made on the volume" "" "$(cat "${state}/calls")"
check "no file was written to /data" "no" \
    "$([ -e "${state}/data/storage-secret" ] && echo yes || echo no)"

# ── A failing new-secret refuses the start ──────────────────────────────────
fresh_state
printf '#!/bin/sh\nexit 1\n' >"${state}/bin/ad4m-executor"
(
    # shellcheck source=/dev/null
    . "${script}"
    ensure_storage_secret "${state}/data"
) >"${state}/out" 2>&1
rc=$?
check "a failing new-secret is refused" "1" "${rc}"
check "  and no file stays behind" "0" \
    "$(find "${state}/data" -name '*storage-secret*' | wc -l)"

if [ "${failures}" -gt 0 ]; then
    echo "${failures} failed"
    exit 1
fi
echo "all passed"
