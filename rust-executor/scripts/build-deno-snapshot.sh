#!/usr/bin/env bash
# Builds the Deno snapshot (CUSTOM_DENO_SNAPSHOT.bin and the two residual
# tables) only when one of its inputs changed.
#
# The snapshot records the extensions in `ad4m_language_extensions()` (their
# ops, in order, and their ESM) on top of deno_runtime's own extensions. So it
# depends only on the files below, the locked crate versions and the platform.
# A hit skips the `generate_snapshot` compile of the whole executor lib.
#
# The stamp (CUSTOM_DENO_SNAPSHOT.inputs) holds "<input hash> <output digest>":
# the outputs' digest is the sha256 of their content hashes. The outputs are
# "up to date" only when both match, so outputs that something else put here
# (another commit's, restored from a CI stash into a kept workdir) are rebuilt
# and never embedded: their op count would not be the binary's, and deno
# panics at startup.
#
#   FORCE_DENO_SNAPSHOT=1   build even when the stamp matches.
#   CI_ARTIFACT_DIR=<dir>   also keep and reuse the outputs in
#                           <dir>/deno-snapshot/<hash>/ (host-wide CI cache).
#                           Each entry has a `digest` file; an entry without
#                           one, or whose files do not match it, is never
#                           restored and is replaced by the next store.
#
# A new `extension!` in the snapshot list, or a new file that one of them
# reads, must go into `inputs` below. A listed file that is gone stops the
# script.
set -euo pipefail

cd "$(dirname "$0")/.."

outputs=(
  CUSTOM_DENO_SNAPSHOT.bin
  CUSTOM_DENO_SNAPSHOT.residual_esm.rs
  CUSTOM_DENO_SNAPSHOT.residual_js.rs
)
stamp=CUSTOM_DENO_SNAPSHOT.inputs

inputs() {
  {
    printf '%s\n' \
      ../Cargo.lock \
      ../Cargo.toml \
      Cargo.toml \
      scripts/build-deno-snapshot.sh \
      src/bin/generate_snapshot.rs \
      src/holochain_service/holochain_service_extension.rs \
      src/holochain_service/holochain_service_extension.js \
      src/runtime_service/runtime_service_extension.rs \
      src/runtime_service/runtime_service_extension.js
    find src/js_core -type f \( -name '*.rs' -o -name '*.js' \)
  } | LC_ALL=C sort
}

if command -v sha256sum >/dev/null; then
  sha256() { sha256sum "$@"; }
else
  sha256() { shasum -a 256 "$@"; }
fi

# One line per input (content hash and path), plus the platform: a V8
# snapshot is tied to the CPU architecture it was made for. The paths hold no
# spaces; one sha256 call fails the script when a listed file is gone.
# shellcheck disable=SC2046
hash=$( { uname -sm; sha256 $(inputs); } | sha256 | cut -c1-64)

have_outputs() {
  local dir=$1 f
  for f in "${outputs[@]}"; do
    [[ -s "$dir/$f" ]] || return 1
  done
}

# The digest of the outputs in a directory: the same files give the same
# digest wherever they are. Fails when an output is missing or empty.
digest() {
  have_outputs "$1" || return 1
  (cd "$1" && sha256 "${outputs[@]}") | sha256 | cut -c1-64
}

# A cache entry is whole when its files match the digest it was stored with.
entry_ok() {
  local entry=$1 want have
  want=$(cat "$entry/digest" 2>/dev/null) && [[ -n "$want" ]] \
    && have=$(digest "$entry") && [[ "$have" == "$want" ]]
}

cache=${CI_ARTIFACT_DIR:+$CI_ARTIFACT_DIR/deno-snapshot}

# Puts the outputs (whose digest is $1) in the host-wide cache (CI only) and
# drops the entries that no build used for a day (about 6 MB each). A cache
# that cannot be written (disk full) costs the next run a build, never this
# one: the snapshot is already made.
store() {
  [[ -n "$cache" ]] || return 0
  local sum=$1 entry=$cache/$hash
  if entry_ok "$entry"; then
    touch "$entry" || true
  else
    # An entry from before digests, or one that lost or changed an output,
    # would otherwise stay, touched by every build that falls back to building.
    rm -rf "${cache:?}/$hash" || true
    local tmp
    # mktemp: runners in separate containers can share a PID.
    # Another runner may have stored the same hash first; its copy is as good.
    # A partial copy is never published: only a complete $tmp is renamed.
    # mktemp makes it 0700; runners under other users must read the entry.
    if mkdir -p "$cache" && tmp=$(mktemp -d "$cache/$hash.tmp.XXXXXX") \
      && chmod 755 "$tmp" && cp "${outputs[@]}" "$tmp/" \
      && echo "$sum" >"$tmp/digest"; then
      mv -T "$tmp" "$entry" 2>/dev/null || rm -rf "$tmp"
    else
      rm -rf "${tmp:-}"
    fi
  fi
  find "$cache" -mindepth 1 -maxdepth 1 -mmin +1440 -exec rm -rf {} + 2>/dev/null || true
}

if [[ "${FORCE_DENO_SNAPSHOT:-}" != 1 ]]; then
  if sum=$(digest .) && [[ "$(cat "$stamp" 2>/dev/null)" == "$hash $sum" ]]; then
    store "$sum"
    echo "Deno snapshot up to date ($hash)"
    exit 0
  fi
  rm -f "$stamp"
  # Only an entry with a digest is copied, and the copy must match it. A copy
  # that fails (another runner pruned the entry meanwhile) or does not match
  # builds, and the store after the build replaces a damaged entry.
  if [[ -n "$cache" ]] && want=$(cat "$cache/$hash/digest" 2>/dev/null) && [[ -n "$want" ]] \
    && have_outputs "$cache/$hash" && cp "${outputs[@]/#/$cache/$hash/}" .; then
    if sum=$(digest .) && [[ "$sum" == "$want" ]]; then
      echo "$hash $sum" >"$stamp"
      store "$sum"
      echo "Deno snapshot restored from $cache/$hash"
      exit 0
    fi
    echo "Deno snapshot cache entry $cache/$hash does not match its digest; building"
  fi
fi

# A stamp left from a run that failed half-way would vouch for mixed outputs.
rm -f "$stamp"
cargo run --release --features generate_snapshot --bin generate_snapshot
sum=$(digest .)
echo "$hash $sum" >"$stamp"
store "$sum"
echo "Deno snapshot built ($hash)"
