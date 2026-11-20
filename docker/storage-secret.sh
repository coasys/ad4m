# The executor's storage secret on the data volume. Sourced by docker-entrypoint.sh, which
# defines ad4m-executor on PATH. Tested by docker/storage-secret.test.sh.
#
# `ad4m-executor run` starts sealed without a storage secret: the API then answers only
# `runtime.storageStatus` and `storage.unlock`, so the entrypoint's own agent setup
# (`agent.status`, `agent.generate`, `agent.unlock`) would fail and the node would stay down
# until a manual `storage unlock`.
#
# The image keeps the secret in a file on the data volume beside the data it seals: whoever
# has the volume has the data either way, and losing the container layer does not lose the
# secret. `AD4M_ALLOW_SECRET_ON_DATA_DISK` is exported so the executor takes a file on the
# data folder's disk. The file is mode 0600 and is never logged.
#
# An operator who wants the secret elsewhere mounts a file and sets
# `AD4M_STORAGE_SECRET_FILE`; this script then does nothing.
#
# ensure_storage_secret <data dir>

ensure_storage_secret() {
    local data="$1"
    if [ -n "${AD4M_STORAGE_SECRET_FILE:-}" ]; then
        # The operator provides the secret: leave the variables alone.
        return 0
    fi

    local file="${data}/storage-secret"
    if [ ! -f "${file}" ]; then
        if [ -f "${data}/ad4m/keyring.json" ]; then
            echo "docker-entrypoint: sealed data exists (${data}/ad4m/keyring.json) but no storage secret at ${file}." >&2
            echo "docker-entrypoint: a new secret cannot open it. Restore the storage secret file (64 hex characters, mode 0600) or set AD4M_STORAGE_SECRET_FILE." >&2
            return 1
        fi
        echo "docker-entrypoint: making a storage secret at ${file}; the data volume keeps it"
        local tmp
        tmp=$(mktemp "${data}/.storage-secret.XXXXXX") || return 1
        if ! ad4m-executor storage new-secret >"${tmp}"; then
            rm -f "${tmp}"
            echo "docker-entrypoint: 'ad4m-executor storage new-secret' failed" >&2
            return 1
        fi
        # The first line is the secret; what follows is advice for a terminal.
        local secret
        secret=$(head -n 1 "${tmp}")
        if ! [[ ${secret} =~ ^[0-9a-f]{64}$ ]]; then
            rm -f "${tmp}"
            echo "docker-entrypoint: 'ad4m-executor storage new-secret' did not print a secret" >&2
            return 1
        fi
        printf '%s\n' "${secret}" >"${tmp}"
        chmod 600 "${tmp}"
        # No-clobber: a file written meanwhile wins; the temp copy is dropped.
        mv -n "${tmp}" "${file}"
        rm -f "${tmp}"
    fi

    export AD4M_STORAGE_SECRET_FILE="${file}"
    export AD4M_ALLOW_SECRET_ON_DATA_DISK=true
}
