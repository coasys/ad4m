#!/bin/bash
# Build only the zome packages for WASM (exclude sweettest which has native-only dependencies)
CARGO_TARGET_DIR=target RUSTFLAGS='--cfg getrandom_backend="custom"' cargo build --release --target wasm32-unknown-unknown -p perspective_diff_sync -p perspective_diff_sync_integrity && hc dna pack workdir && hc app pack workdir