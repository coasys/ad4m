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

cache=${CI_ARTIFACT_DIR:+$CI_ARTIFACT_DIR/deno-snapshot}

# Puts the outputs in the host-wide cache (CI only) and drops the entries that
# no build used for a day (about 6 MB each).
store() {
  [[ -n "$cache" ]] || return 0
  if [[ -d "$cache/$hash" ]]; then
    touch "$cache/$hash"
  else
    local tmp=$cache/$hash.tmp.$$
    mkdir -p "$tmp"
    cp "${outputs[@]}" "$tmp/"
    # Another runner may have stored the same hash first; its copy is as good.
    mv -T "$tmp" "$cache/$hash" 2>/dev/null || rm -rf "$tmp"
  fi
  find "$cache" -mindepth 1 -maxdepth 1 -mmin +1440 -exec rm -rf {} + 2>/dev/null || true
}

if [[ "${FORCE_DENO_SNAPSHOT:-}" != 1 ]]; then
  if [[ "$(cat "$stamp" 2>/dev/null)" == "$hash" ]] && have_outputs .; then
    store
    echo "Deno snapshot up to date ($hash)"
    exit 0
  fi
  # A copy that fails (another runner pruned the entry meanwhile) builds.
  if [[ -n "$cache" ]] && have_outputs "$cache/$hash" && cp "${outputs[@]/#/$cache/$hash/}" .; then
    echo "$hash" >"$stamp"
    store
    echo "Deno snapshot restored from $cache/$hash"
    exit 0
  fi
fi

# A stamp left from a run that failed half-way would vouch for mixed outputs.
rm -f "$stamp"
cargo run --release --features generate_snapshot --bin generate_snapshot
echo "$hash" >"$stamp"
store
echo "Deno snapshot built ($hash)"
