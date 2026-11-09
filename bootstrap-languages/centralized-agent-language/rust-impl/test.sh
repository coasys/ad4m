#!/usr/bin/env bash
# The Rust port's tests: unit tests and the language suite (tests/language.rs), run natively
# against a real identity-store that the suite builds (cargo build -p identity-store in the
# repo root) and starts on a temporary SQLite file.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

cargo test "$@"
