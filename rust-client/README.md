# AD4M client library in Rust

This is a library for interacting with the AD4M executor / agent,
analogous to https://www.npmjs.com/package/@coasys/ad4m.

## Usage

```Rust
use ad4m_client::Ad4mClient;

let executor_url = "http://localhost:4000".to_string();
let cap_token = read_capability_token_from_file();

let ad4m_client = Ad4mClient::new(executor_url, cap_token);

println!("{:?}", ad4m_client.agent.me()?);
```

## Minimum executor

Batched instance writes (`Batch`, `Model::transaction`, `Instance::save`
with a `batch_id`) need an executor that resolves each write in a batch
against the batch's effective graph: the committed links plus the changes
already staged in that batch. That arrived in executor commits `06cd80b7f`,
`c54e7267b` and `258af061c` (branch `feat/validated-write-rpc`).

- Minimum: an executor built from commit `87ba32b90` or a later commit, or
  the first release that contains it.
- The version string does not tell builds apart: executors before and
  after this change both report `0.13.1-staging.11`.
- With an older executor, two saves of one collection in one batch (`[]`,
  then `[a]`, then `[]`) both succeed and the instance snapshots clean, but
  the commit writes `[a]`. The client does not detect this and does not
  check the executor's capabilities.

Until documentation will be added, have a look at the AD4M cli code for a full usage example: https://github.com/perspect3vism/ad4m/tree/main/cli.
