# Hosting-index client

The **hosting index** is Coasys's private list of approved hosts, a service on the platform. The executor's **index client** is its only caller from an executor: it signs in with a key statement that the executor's main device key signs, with the index as the audience; the index resolves the key to the operator's identity through the key index and checks its allow list. Signed in, the client publishes and withdraws the executor's **listing** (name, place, address, AI models, per-operation rates, sign-up setting, free access), republishes when rates or models change, and requests a **membrane proof** from Unyt's joining service through the index, which the executor then installs on its alliance cell.

**Why:** The executor signs in, not the browser, because the executor holds the device key: one credential, the same as every other identity call, ties a listing to an operator, and the old index's own email and password lost. The client builds the whole statement itself and refuses an audience other than the index that its configured URL answered, so no caller can make the executor sign a login for another executor. The membrane proof is issued for the executor's Unyt agent key, which is separate from its device keys; the index's record of which device key and operator it asked for is the one place that ties the two.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

1. `index.status` (operator): `GET /v1/index/info` gives the index's DID; `POST /challenge` with the device key; build `KeyStatement { purpose: login, scid: <operator record>, key, executor: <index DID>, challenge }` and sign it under `coasys:session`; `POST /session` gives a token that lives 1 hour, in memory only. A `401` or a near expiry signs in again.
2. `index.publish { name, description, location, url }`: `PUT /listing`. The index checks the host's public `host.info` and refuses a host whose charge mode is not `Free`. The listing's last state sits in the `index_listing` settings row; `index-listing-changed` reports changes.
3. `index.withdraw`: `DELETE /listing`.
4. `index.requestMembraneProof`: refuses with `holochain-off` when Holochain does not run; `get_or_create_agent_key`; `POST /membrane-proofs { agentKey }`; store the proof (`set_membrane_proof`), install the alliance DNA in the background, record `ProofState` (`None`, `Requested`, `Stored`, `Installed`, `Failed`, `Revoked`) in `unyt_proof_state`, emit `unyt-state-changed`. The cell joins Unyt's test network.
5. An index outage changes nothing installed: the stored proof and the cell keep working; only a new request needs the index.

| Refusal | Meaning |
|---|---|
| `index-disabled` | `hosting_index.enabled` is false |
| `index-unreachable` | the index did not answer |
| `not-allowed` | the operator is not on the allow list |
| `no-operator` | the executor has no operator record, so it cannot sign in |
| `holochain-off`, `unexpected-roles`, `proof-revoked` | the membrane-proof path |

A person adds an unlisted host by its URL with no index: the app reads `host.info` and joins ([Tenants](../managed-agents/tenants.md)).

The index decides whether a host shows as "run by Coasys". `host.display.run_by_coasys` in the host's config is a label that the apps do not trust.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/hosting/index_client.rs](../../../../rust-executor/src/hosting/index_client.rs) (new): `IndexConfig`, `LocalListing`, `IndexSession`, `ProofState`, `sign_index_statement`.
- `index.status`, `index.publish`, `index.withdraw`, `index.requestMembraneProof`: new RPC handlers, all with the `Operator` guard.
- [rust-executor/src/unyt_service.rs](../../../../rust-executor/src/unyt_service.rs): `get_or_create_agent_key`, `set_membrane_proof`, `reinstall`, `version_info`.
- [rust-executor/src/config_file.rs](../../../../rust-executor/src/config_file.rs): the `hosting_index` keys.

</details>
