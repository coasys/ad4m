# Link-server directory

The **link-server directory** (`LinkServerDirectory`) tells the executor which link server a new neighbourhood uses. It reads three sources at start, in this order, and takes the first that has a value: the **operator**'s setting, the **platform**'s deployment config, and the **seed**'s `linkServers`. Only the creating executor needs it: the address goes into the link language's template, and every joiner reads it from there.

**Why:** The desktop app's executor has no gateway that could hand it an address, so the seed carries a default that needs no extra service and cannot fail at start. The option that lost was a signed directory document fetched at start: it adds a request and a failure point. Because every neighbourhood bakes the address into its link language, the seed's host name must stay stable; a change needs a release.

**Parent:** [Server link language](README.md)

<details><summary>How it works</summary>

| Order | Source (`LinkServerSource`) | Set by |
|---|---|---|
| 1 | `Operator` | the operator, through `runtime.setConfig` with the `restart`-class key `link_server.url`, or the CLI flag `--link-server-url` |
| 2 | `Platform` | the deployment's config, for Coasys cloud |
| 3 | `Seed` | `linkServers` in the bootstrap seed: `[{ url, name }]` |

1. `default_entry()` answers the first source with a value; `all()` lists every source with a value. Neither makes a network call.
2. `check_operator_url(url)` runs before a write of `link_server.url`, and before `neighbourhood.create` with `SyncChoice::OwnLinkServer { url }`: HTTPS (or a loopback address) and a `/.well-known/coasys-link-server` answer with protocol 2 or later. A new value takes effect at the next start and only for neighbourhoods made after it.
3. `neighbourhood.create` with `SyncChoice::Default` fails with a clear error when no source has a value. Every path that publishes the server link language fills `SERVER_URL` itself; none relies on the bundle's default.

| RPC | Who may call | Answers |
|---|---|---|
| `runtime.linkServers` | any session | `Vec<LinkServerEntry { url, name, source }>`; the apps' new-neighbourhood screen labels the default from it |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/link_servers.rs](../../../../rust-executor/src/link_servers.rs) (new): `LinkServerDirectory`, `LinkServerEntry`, `LinkServerSource`, `check_operator_url`.
- [rust-executor/src/mainnet_seed.json](../../../../rust-executor/src/mainnet_seed.json), [cli/seed_proto.json](../../../../cli/seed_proto.json): the seed's `linkServers` ([Bootstrap languages](../../runtime/bootstrap-languages.md)).
- [rust-executor/src/config.rs](../../../../rust-executor/src/config.rs): the config key `link_server.url` and the CLI flag.
- [rust-executor/src/api/runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs): `runtime.linkServers`.
- [rust-executor/src/mcp/tools/neighbourhoods.rs](../../../../rust-executor/src/mcp/tools/neighbourhoods.rs): the clone tool, which creates through `neighbourhood.create`.

</details>
