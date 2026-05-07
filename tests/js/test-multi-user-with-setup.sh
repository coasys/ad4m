#!/bin/bash

# Script to run multi-user test with proper setup
echo "🚀 Multi-User Test with Setup"
echo "============================="

# Function to kill any AD4M processes belonging to THIS test run (by port, not by name).
# Do NOT use pkill/killall by name — that would kill executors from other concurrent CI jobs
# running on the same machine. Each test suite uses a unique port range.
cleanup_processes() {
    echo "🧹 Killing any existing AD4M processes on our ports..."
    # Kill only processes on the ports used by THIS test suite.
    # setup (publishTestLangs): 15703-15705  ← unique to this job
    lsof -ti:15703 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15704 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15705 | xargs -r kill -9 2>/dev/null || true
    # multi-user-simple.test.ts: 15900-15902
    lsof -ti:15900 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15901 | xargs -r kill -9 2>/dev/null || true
    lsof -ti:15902 | xargs -r kill -9 2>/dev/null || true
    sleep 1
}

# Unique setup port range for this CI job so it doesn't conflict with
# integration-tests-js (15700-15702) or integration-tests-email-verification (15706-15708).
export AD4M_SETUP_PORT=15703
export AD4M_SETUP_HC_ADMIN_PORT=15704
export AD4M_SETUP_HC_APP_PORT=15705

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

echo "  - Getting builtin test languages..."
deno run --allow-all scripts/get-builtin-test-langs.js

echo "  - Injecting language language..."
pnpm run inject-language-language

echo "  - Publishing test languages..."
pnpm run publish-test-languages

echo "  - Injecting publishing agent..."
pnpm run inject-publishing-agent

echo "✅ Test environment prepared"

# Step 3: Ensure executor is killed before running test
echo "🧪 Preparing to run multi-user test..."
cleanup_processes
sleep 3

# Step 4: Run the multi-user test
echo "🧪 Running multi-user test..."
pnpm run test-multi-user-simple
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -ne 0 ]; then
    echo "❌ Multi-user test failed with exit code $TEST_EXIT_CODE"
else
    echo "✅ Multi-user test with setup complete!"
fi

exit $TEST_EXIT_CODE

