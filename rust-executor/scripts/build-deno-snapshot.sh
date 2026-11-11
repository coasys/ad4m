#!/usr/bin/env bash
# Builds the Deno snapshot (CUSTOM_DENO_SNAPSHOT.bin and the two residual
# tables) only when one of its inputs changed.
#
# The snapshot records the extensions in `ad4m_language_extensions()` (their
# ops, in order, and their ESM) on top of deno_runtime's own extensions. So it
# depends only on the files below, the locked crate versions and the platform.
# A hit skips the `generate_snapshot` compile of the whole executor lib.
#
#   FORCE_DENO_SNAPSHOT=1   build even when the stamp matches.
#   CI_ARTIFACT_DIR=<dir>   also keep and reuse the outputs in
#                           <dir>/deno-snapshot/<hash>/ (host-wide CI cache).
#
# A new `extension!` in the snapshot list, or a new file that one of them
# reads, must go into `inputs` below. A listed file that is gone stops the
# script.
#
# The stamp holds the inputs' hash and the checksums of the outputs made from
# them. It vouches only for those exact outputs: outputs that something else
# put here (CI's restore of another commit's compile inputs, a snapshot copied
# by hand) do not match it, and the script makes or restores them again.
# Without the checksums a CI workdir kept a foreign commit's snapshot under its
# own stamp, and the executor built against it called ops that did not match
# its JS ("A language signs only as the user it runs for" from a plain sign).
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

# The stamp: the inputs' hash, then the outputs' checksums (sha256sum format).
write_stamp() {
  { echo "$hash"; sha256 "${outputs[@]}"; } >"$stamp"
}

# Whether the stamp names these inputs and the outputs here are the ones it
# was written for: the whole stamp equals the one write_stamp would write now,
# so a stamp that lacks a checksum (an older script, a write cut short)
# vouches for none.
stamp_vouches() {
  have_outputs . \
    && [[ "$(cat "$stamp" 2>/dev/null)" == "$(echo "$hash"; sha256 "${outputs[@]}")" ]]
}

cache=${CI_ARTIFACT_DIR:+$CI_ARTIFACT_DIR/deno-snapshot}

# Puts the outputs in the host-wide cache (CI only) and drops the entries that
# no build used for a day (about 6 MB each). A cache that cannot be written
# (disk full) costs the next run a build, never this one: the snapshot is
# already made.
store() {
  [[ -n "$cache" ]] || return 0
  if have_outputs "$cache/$hash"; then
    touch "$cache/$hash" || true
  else
    # An entry that lost an output (not one this script published) would
    # otherwise stay, touched by every build that falls back to building.
    rm -rf "${cache:?}/$hash" || true
    local tmp
    # mktemp: runners in separate containers can share a PID.
    # Another runner may have stored the same hash first; its copy is as good.
    # A partial copy is never published: only a complete $tmp is renamed.
    # mktemp makes it 0700; runners under other users must read the entry.
    if mkdir -p "$cache" && tmp=$(mktemp -d "$cache/$hash.tmp.XXXXXX") \
      && chmod 755 "$tmp" && cp "${outputs[@]}" "$tmp/"; then
      mv -T "$tmp" "$cache/$hash" 2>/dev/null || rm -rf "$tmp"
    else
      rm -rf "${tmp:-}"
    fi
  fi
  find "$cache" -mindepth 1 -maxdepth 1 -mmin +1440 -exec rm -rf {} + 2>/dev/null || true
}

if [[ "${FORCE_DENO_SNAPSHOT:-}" != 1 ]]; then
  if stamp_vouches; then
    store
    echo "Deno snapshot up to date ($hash)"
    exit 0
  fi
  # A copy that fails (another runner pruned the entry meanwhile) builds.
  if [[ -n "$cache" ]] && have_outputs "$cache/$hash" && cp "${outputs[@]/#/$cache/$hash/}" .; then
    write_stamp
    store
    echo "Deno snapshot restored from $cache/$hash"
    exit 0
  fi
fi

# A stamp left from a run that failed half-way would vouch for mixed outputs.
rm -f "$stamp"
cargo run --release --features generate_snapshot --bin generate_snapshot
write_stamp
store
echo "Deno snapshot built ($hash)"
