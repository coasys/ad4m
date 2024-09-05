@echo off

echo Creating hooks directory if it doesn't exist...
if not exist .git\hooks (
    mkdir .git\hooks
)

echo Copying pre-push hook to the Git hooks directory...
copy hooks\pre-push.bat .git\hooks\pre-push.bat

echo Configuring Git to use the custom hooks directory...
git config core.hooksPath .git\hooks

echo Git Hooks installed and configured.