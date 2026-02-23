#!/bin/bash

# Script to run multi-user test with proper setup
echo "🚀 Multi-User Test with Setup"
echo "============================="

# Function to kill any running AD4M processes
cleanup_processes() {
    echo "🧹 Killing any existing AD4M processes..."
    # Kill any processes using the AD4M ports
    pkill -f "rust-executor" 2>/dev/null || true
    pkill -f "ad4m" 2>/dev/null || true
    
    # Wait a moment for processes to die
    sleep 2
    
    # Force kill if still running
    lsof -ti:15700 | xargs kill -9 2>/dev/null || true
    lsof -ti:15701 | xargs kill -9 2>/dev/null || true
    lsof -ti:15702 | xargs kill -9 2>/dev/null || true
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

echo "✅ Multi-user test with setup complete!"

