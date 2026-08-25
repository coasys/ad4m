#!/bin/bash

# Script to run multi-user test with proper setup
echo "🚀 Email-Verification Test with Setup"
echo "============================="

# Function to kill any AD4M processes belonging to THIS test run (by port, not by name).
# Do NOT use pkill/killall by name — that would kill executors from other concurrent CI jobs
# running on the same machine. Each test suite uses a unique port range.
cleanup_processes() {
    echo "🧹 Killing any existing AD4M processes on our ports..."
    # publishTestLangs.ts now uses random ports (getFreePorts) and cleans
    # up its own executor in a finally block — no fixed setup ports to kill.
    # email-verification.test.ts: 15920-15922
    lsof -ti:15920 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15921 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15922 | xargs -r kill -9 2>/dev/null || true
    sleep 1
}

# Function to clean up test directories
cleanup_directories() {
    echo "🧹 Cleaning up test directories..."
    rm -rf tst-tmp 2>/dev/null || true
    rm -rf .ad4m 2>/dev/null || true
}

# Trap to ensure cleanup on script exit
trap 'cleanup_processes; cleanup_directories' EXIT

# Step 1: Initial cleanup
cleanup_processes
cleanup_directories

echo "🧹 Cleaning testing data..."
node scripts/cleanTestingData.js

# Step 2: Prepare test environment
echo "🔧 Preparing test environment..."
echo "  - Building test languages..."
./scripts/build-test-language.sh

echo "  - Preparing test directory..."
./scripts/prepareTestDirectory.sh

# Pass --local flag through to get-builtin-test-langs if provided
LANG_FLAGS=""
if [[ "$*" == *"--local"* ]]; then
    LANG_FLAGS="--local"
    echo "  - Using LOCAL bootstrap languages (no Holochain)"
fi

echo "  - Getting builtin test languages..."
deno run --allow-all scripts/get-builtin-test-langs.js $LANG_FLAGS

echo "  - Injecting language language..."
pnpm run inject-language-language

echo "  - Publishing test languages..."
pnpm run publish-test-languages

echo "  - Injecting publishing agent..."
pnpm run inject-publishing-agent

echo "✅ Test environment prepared"

# Step 3: Ensure executor is killed before running test
echo "🧪 Preparing to run email-verification test..."
cleanup_processes
sleep 3

# Step 4: Run the email-verification test
echo "🧪 Running Email-Verification test..."
pnpm run test-email-verification
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -ne 0 ]; then
    echo "❌ Email-verification test failed with exit code $TEST_EXIT_CODE"
else
    echo "✅ Email-verification test with setup complete!"
fi

exit $TEST_EXIT_CODE

