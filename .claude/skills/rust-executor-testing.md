# Rust Executor Testing

## How to Run Tests

The rust-executor tests MUST be run with one of these methods:

```bash
# Recommended: Using pnpm (runs with proper configuration)
pnpm test

# Alternative: Using cargo directly with required flags
cargo test --release -- --test-threads=1
```

**IMPORTANT**: Do NOT run `cargo test` without these flags. The tests require:
- `--release` mode for proper performance
- `--test-threads=1` to avoid race conditions between tests

## Why These Flags Are Required

- **`--release`**: Some tests involve heavy computations or time-sensitive operations that only work correctly in release mode
- **`--test-threads=1`**: The tests share global state and must run sequentially to avoid conflicts

## Common Test Failures

If tests fail when run with plain `cargo test`, try running with the proper flags above.
