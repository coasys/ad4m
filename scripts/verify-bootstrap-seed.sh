#!/usr/bin/env bash
# Checks that every language a network seed pins can be installed by a node
# that runs the seed: its signed meta is in the bootstrap store, the meta's
# author is one of the seed's trustedAgents, and the bundle is in the store.
#
# Usage: scripts/verify-bootstrap-seed.sh <mainnet_seed.json>
# BOOTSTRAP_STORE_URL overrides the store (the language-language's PROXY_URL).
#
# Store keys are write-once and a publish of a key that exists counts as
# stored, so a language someone else published first keeps their meta: the
# trusted publish of the same bundle is then a silent no-op and nodes refuse
# the language. This script is where that shows up.

set -euo pipefail

SEED="${1:?usage: $0 <mainnet_seed.json>}"
STORE="${BOOTSTRAP_STORE_URL:-https://bootstrap-store-gateway.perspect3vism.workers.dev}"

# GET a store key: the store answers with a presigned URL for the object.
fetch() {
    local url
    url=$(curl -fsS --retry 3 "$STORE?key=$1" | jq -r .url) || return 1
    [[ -n "$url" && "$url" != null ]] || return 1
    curl -fsS --retry 3 "$url"
}

failed=0
fail() {
    echo "FAIL $1"
    failed=1
}

mapfile -t trusted < <(jq -r '.trustedAgents[]' "$SEED")
mapfile -t pinned < <(jq -r '[.agentLanguage, .perspectiveLanguage, .neighbourhoodLanguage, .directMessageLanguage, .knownLinkLanguages[]] | .[] | select(. != "")' "$SEED")
[[ ${#trusted[@]} -gt 0 ]] || fail "the seed trusts no agent"
[[ ${#pinned[@]} -gt 0 ]] || fail "the seed pins no language"

echo "| address | name | author |"
echo "|---|---|---|"
for address in "${pinned[@]}"; do
    if ! meta=$(fetch "meta-$address"); then
        fail "$address: no meta in the store"
        continue
    fi
    author=$(jq -r .author <<<"$meta")
    name=$(jq -r .data.name <<<"$meta")
    echo "| $address | $name | $author |"
    if [[ $(jq -r .data.address <<<"$meta") != "$address" ]]; then
        fail "$address: the meta names another address"
    fi
    if ! printf '%s\n' "${trusted[@]}" | grep -qxF "$author"; then
        fail "$address: published by $author, who is not in the seed's trustedAgents. The key is write-once: change the bundle (e.g. its version string) to get a new address and publish again"
    fi
    if ! bytes=$(fetch "$address" | wc -c) || [[ $bytes -eq 0 ]]; then
        fail "$address: no bundle in the store"
    fi
done

if [[ $failed -ne 0 ]]; then
    echo "The seed must not be released: nodes would refuse the languages above."
    exit 1
fi
echo "All ${#pinned[@]} pinned languages are in the store, signed by a trusted agent."
