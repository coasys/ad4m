#!/usr/bin/env bash
# Prints the SHA-256 line of a release asset, as `sha256sum` writes it, under the name the asset
# is published with (not the path it was built at). The release workflows upload the line as
# `<asset>.sha256` beside the asset, so a download checks with
#
#   sha256sum -c <asset>.sha256          # Linux
#   shasum -a 256 -c <asset>.sha256      # macOS
#
# run in the directory that holds both files.
#
# Usage: scripts/release-checksum.sh <file> <asset-name> > <asset-name>.sha256

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <file> <asset-name>" >&2
  exit 2
fi
file=$1
name=$2

if [ ! -f "$file" ] || [ ! -s "$file" ]; then
  echo "release-checksum: $file is missing or empty" >&2
  exit 1
fi
# The name is the whole second field of the line: a path or a line break in it would make
# `sha256sum -c` look for another file.
case "$name" in
  '' | */* | *$'\n'*)
    echo "release-checksum: asset name must be a plain file name, got '$name'" >&2
    exit 1
    ;;
esac

if command -v sha256sum >/dev/null; then
  hash=$(sha256sum "$file" | cut -d' ' -f1)
else
  hash=$(shasum -a 256 "$file" | cut -d' ' -f1)
fi
if ! printf '%s' "$hash" | grep -Eq '^[0-9a-f]{64}$'; then
  echo "release-checksum: no SHA-256 for $file" >&2
  exit 1
fi
printf '%s  %s\n' "$hash" "$name"
