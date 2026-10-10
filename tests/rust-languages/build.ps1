# Build all Rust WASM test languages against the wasm32 target.
#
# Requires:
#   rustup target add wasm32-unknown-unknown
$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$languages = @("test-wasm-language")

foreach ($lang in $languages) {
    Write-Host "==> Building $lang (wasm32-unknown-unknown, release)"
    Push-Location (Join-Path $scriptDir $lang)
    try {
        cargo build --target wasm32-unknown-unknown --release
        if ($LASTEXITCODE -ne 0) { throw "cargo build failed for $lang" }
    }
    finally {
        Pop-Location
    }
}

Write-Host "==> Done."
