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

for suite in "${SUITES[@]}"; do
  echo ""
  echo "▶ Running $suite..."
  node scripts/cleanup.js
  pnpm run "$suite"
done
