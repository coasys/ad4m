@echo off

echo Running pre-push hook...

:: Run cargo fmt to format the code
cargo fmt -- --check
if %ERRORLEVEL% NEQ 0 (
    echo Code is not formatted. Please run 'cargo fmt' to format the code.
    exit /b 1
)

:: Run cargo clippy to check for linting issues
cargo clippy -- -D warnings
if %ERRORLEVEL% NEQ 0 (
    echo Clippy found warnings. Please fix them before pushing.
    exit /b 1
)

:: If both checks pass, allow the push to proceed
exit /b 0