# AD4M client library in Rust

This is a library for interacting with the AD4M executor / agent,
analogous to https://www.npmjs.com/package/@perspect3vism/ad4m.

## Usage

```Rust
use ad4m_client::Ad4mClient;

let executor_url = "http://localhost:4000".to_string();
let cap_token = read_capability_token_from_file();

let ad4m_client = Ad4mClient::new(executor_url, cap_token);

println!("{:?}", ad4m_client.agent.me()?);
```

Until documentation will be added, have a look at the AD4M cli code for a full usage example: https://github.com/perspect3vism/ad4m/tree/main/cli.
