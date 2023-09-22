# Rust-Executor (= main ADAM Layer Runtime)

This Rust crate; provides a lib with exposed functions for 'init' & 'run' of an AD4M Agent and also exposes a bin which will by default `run` an AD4M Agent.

Running an AD4M Agent entails the creation of a warp based GraphQL server, conforming to the ADAM Layer spec. Spawning of a Deno runtime that runs the core [`executor`](https://github.com/coasys/ad4m/tree/rust-refactor/executor), this happens inside [`js_core`](https://github.com/coasys/ad4m/blob/rust-refactor/rust-executor/src/js_core/mod.rs). `js_core` also handles the requests from the JS to call into Rust functions. Currently exposed Rust functions allow communication with `Scryer-Prolog`, `Holochain` & `JwtService`.