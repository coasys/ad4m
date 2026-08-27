#!/bin/bash
# Thin per-language shim over bootstrap-languages/hc-dna-build.sh.
# See the shared script for the workspace-local-hc / RUSTFLAGS rationale.
set -euo pipefail
HC_DNA_NAME=agent-language exec "$(cd "$(dirname "$0")/../.." && pwd)/hc-dna-build.sh" \
    -p agent_store -p agent_store_integrity
