#!/bin/sh

# Create the hooks directory if it doesn't exist
mkdir -p .git/hooks

# Copy the pre-push hook to the Git hooks directory
cp hooks/pre-push .git/hooks/pre-push
chmod +x .git/hooks/pre-push

# Configure Git to use the custom hooks directory
git config core.hooksPath .git/hooks

echo "Git Hooks installed and configured."
