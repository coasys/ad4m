#!/bin/bash
CARGO_TARGET_DIR=target RUSTFLAGS='--cfg getrandom_backend="custom"' cargo build --release --target wasm32-unknown-unknown && hc dna pack workdir && hc app pack workdir