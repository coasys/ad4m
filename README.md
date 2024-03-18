[![Project](https://img.shields.io/badge/Project-AD4M-brightgreen.svg)](http://ad4m.dev/)
[![Docs](https://img.shields.io/badge/Docs-AD4M-blue.svg)](http://docs.ad4m.dev/)
[![License: CAL 1.0](https://img.shields.io/badge/License-CAL%201.0-blue.svg)](https://github.com/holochain/cryptographic-autonomy-license)

![Logo](docs/public/images/ad4mlogo_green_angle2_colouremblem.png)

# AD4M

_The **A**gent-Centric **D**istributed **A**pplication **M**eta-ontology_
or just:
**\*A**gent-Centric **DA**pp **M**eta-ontology\*

- A new **meta-ontology** for interoperable, decentralized application design
- A **spanning-layer** to enable seamless integration between Holochain DNAs, blockchains, linked-data structures/ontologies and centralized back-ends
- The basis for turning distinct, monolithic and siloed apps into a global, open and interoperable **sense-making network**

## Core Docs

The main core documentation for AD4M exists [here](https://docs.ad4m.dev)

## Repo Structure

This is a mono-repository with all components making up a whole ADAM Layer implementation.

- `core`: Holds the core ontology types, the `Ad4mClient` and the GraphQL schema build automatically from the core types. This package gets published as https://www.npmjs.com/package/@coasys/ad4m and is thus holding most of the app/UI facing coding.
- `bootstrap-languages`: Holds all the core Languages used to make the ADAM Layer operate. These languages can be thought of as the mainnet configuration for the ADAM Layer.
- `executor`: Holds the core JS code, which manages an Agents ADAM Layer state. Their Perspectives, Languages and Expressions.
- `rust-executor`: Rust binary & library which facilitates the running of a GraphQL server, spawning of Deno runtime to execute the `executor` JS code. Running and communication with in built holochain conductor. Running and communication with Scryer Prolog engine to facilitate Prolog queries over Perspective data.
- `dapp`: UI which provides a connection to MetaMask allowing AD4M Layer to create a connection to blockchain systems. UI server by the `rust-executor` on `http://127.0.0.1:<configured_port>`
- `rust-client`: Rust based `Ad4mClient`. Wraps GraphQL and provides the same high-level interface to AD4M as the TypeScript based version in core. Published to Crates.io as `ad4m-client`: https://crates.io/crates/ad4m-client.
- `tests`: JS testing suit integration testing a built `rust-executor`. Contains 50+ integration tests covering most function calls to an Adam Layer GraphQL server.
- `cli`: Rust based CLI for either init'ing `ad4m init`, running `ad4m run` or communciating with a running AD4M GraphQL server. See [it's readme](cli/README.md) for more details. Published to Crates.io as `ad4m`: https://crates.io/crates/ad4m
- `ui`: Tauri based system-tray icon launcher UI which is the end-user deployment; includes the rust-executor & UI to interact with given executor.

---

- `connect`: Convenience library to connect to a (local or via proxy) AD4M-executor, potentially requesting or reusing capability tokens and creating an `Ad4mClient` ready for the app/UI to use.
- `docs-src` / `docs`: Documentation hosted under https://docs.ad4m.dev

## History

The project started in August 2020 in https://github.com/lucksus/perspectivism, then got broken down into the `ad4m` (core), `ad4m-executor` and [perspect3ve](https://github.com/perspect3vism/perspect3ve) repositories for simultaneuous use in [Flux](https://github.com/fluxsocial) and Perspect3ve. Other components got added over the years 2021, 2022.

In October/November 2022, these componentes were collected into a mono-repo again to avoid PR-chains across multiple repositories and to have version numbers be in lock-step.

## Building AD4M

### Prerequisites

- Install Rust by visiting [here](https://www.rust-lang.org/tools/install) (ADAM Layer currently uses rust version 1.71.1)
- Install Deno by visiting [here](https://deno.land/manual@v1.36.4/getting_started/installation)
- Install Go by visiting [here](https://go.dev/doc/install) (ADAM Layer currently uses go version: go1.18)
- Install Node by visiting [here](https://nodejs.org/en/download)
- Install PNPM by visiting [here](https://pnpm.io/installation)

### Build (CLI & Libs):

```
pnpm install
pnpm run build
```

## Testing (Full test run of all packages)

```
pnpm install
pnpm test
```

## Building ad4m launcher

```
pnpm install
pnpm run package-ad4m
```
