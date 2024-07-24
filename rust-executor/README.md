# Rust-Executor (= main ADAM Layer Runtime)

This Rust crate; provides a lib with exposed functions for 'init' & 'run' of an AD4M Agent and also exposes a bin which will by default `run` an AD4M Agent.

This AD4M implementation includes:
 - a [Holochain](https://github.com/holochain/holochain) conductor/agent, [included as a library](https://github.com/coasys/ad4m/blob/dev/rust-executor/src/holochain_service/mod.rs) dependency into this crate
 - key storage for the users agent keys, build around DID
 - a warp based GraphQL server, conforming to the ADAM Layer spec
 - a [Deno](https://github.com/denoland/deno) runtime that runs the [AD4M Languages](https://github.com/coasys/ad4m/blob/dev/core/src/language/Language.ts) as well as some JS based remainders of the legacy executor implementation the core [`executor`](https://github.com/coasys/ad4m/tree/dev/executor), this happens inside [`js_core`](https://github.com/coasys/ad4m/blob/dev/rust-executor/src/js_core/mod.rs)
 - [Scryer Prolog](https://github.com/mthom/scryer-prolog/) for Social DNA queries of Perspectives (https://github.com/coasys/ad4m/blob/dev/rust-executor/src/prolog_service/mod.rs)
 - SQLite database for storing Perspective data
