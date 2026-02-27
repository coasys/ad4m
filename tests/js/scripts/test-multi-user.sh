#!/bin/bash
set -e

SUITES=(
  test-multi-user-auth
  test-multi-user-config
  test-multi-user-isolation
  test-multi-user-sdna
  test-multi-user-profiles
  test-multi-user-neighbourhood
  test-multi-user-multi-node
  test-multi-user-subscriptions
  test-multi-user-notifications
)

# Guarantee cleanup runs even if a suite fails or the script is interrupted.
trap 'node scripts/cleanup.js' EXIT

# Run prepare-test once before any suite so that tst-tmp/languages/ is
# populated with the language bundles (bundle-{hash}.js / meta-{hash}.json).
# Without this, executors started by the multi-user tests can't install the
# agent language via the language-language and byDID / updatePublicPerspective
# calls fail with "No Agent Language installed!" on a fresh CI workdir.
echo ""
echo "▶ Running prepare-test..."
node scripts/cleanup.js
pnpm run prepare-test

for suite in "${SUITES[@]}"; do
  echo ""
  echo "▶ Running $suite..."
  node scripts/cleanup.js
  pnpm run "$suite"
done
