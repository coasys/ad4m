# AD4M client library in Rust

This is a library for interacting with the AD4M executor / agent,
analogous to https://www.npmjs.com/package/@perspect3vism/ad4m.

## Usage

This is a functional interface with each function wrapping one GraphQL query.
Like the JS client, functions are grouped into domains represented by submodules: agent, languages, perspectives, neighbourhoods and runtime.

All functions require a capability token passed as parameter (plus potential other parameters)
and return a future.

Note that the executor URL needs to be set globally first by calling `set_executor_url(url String)`:

```Rust
use ad4m_client::{set_executor_url, agent};

set_executor_url("https://localhost:3000/graphql".to_string());
agent::me(cap_token).await?;
```

Until documentation will be added, have a look at the AD4M cli code for a full usage example: https://github.com/perspect3vism/ad4m/tree/main/cli.
