# Device linking

On the executor's side, linking is a **claim**: the executor answers an **offer** with one of its wallet keys and that key's encryption key, co-signs the claim, and waits until the holder's `add` lists the key. `ClaimService::claim_for_main` claims with the wallet key `main` and first applies the **first-link rule**: the offer's identity must equal the executor's operator record. `claim_for_tenant` claims with a tenant's new wallet key and skips that rule. Revocation is a `revoke` event that the master key signs; a revoked executor makes a new main key with `identity.rekey`, on the machine only.

**Why:** The first link to an executor's main agent must come from the operator that its first run named: "the first identity to link becomes the operator" would let a stranger who reaches a fresh server first take it. The check runs inside `claim_for_main`, not in an RPC handler, because a server with no address claims from its pre-join inbox, where no RPC runs. A revoked key never returns (one key, one place, for ever), so re-keying is the only way back, and it stays on the machine so that no remote session can swap the executor's identity.

**Parent:** [Devices](README.md)

<details><summary>How it works</summary>

| Method | Guard | Does |
|---|---|---|
| `identity.prepareDevice` | operator, local only | this executor's main key, a fresh pre-join inbox address and the device string `coasys:device:1:…`; polls that inbox for 5 minutes; emits `link-ticket` |
| `identity.claimOffer { offer, canKeepMaster?, hint? }` | operator | `claim_for_main`: `403 no-operator` with no operator record, `403 first-link` when the main key is in no valid log and the offer's identity is not the operator; answers `{did, offerId, matchCode, kind, expires}` |
| `identity.joinStatus { offerId }` | own session | the offer's state and the key index for this executor's key; on `spent`, records the `did:scid` for the main agent |
| `identity.openOffer`, `identity.offers`, `identity.cancelOffer` | own session | pass offers and list requests that an app signed with the master key |
| `identity.devices` | own session | the log's keys joined with the private record (`DeviceView`) |
| `identity.rekey` | operator, local only; `ad4m-executor rekey` on a server | a new main wallet key; the old one stays only to read history; nothing enters the log |

| Device | Route on the executor |
|---|---|
| Desktop app, first run | the custody service builds `incept` listing the main key as `d1`; `identity.cosignIncept`, then `identity.publish` ([Key-log cache and resolver](../key-log/cache.md)) |
| Desktop app linked from another app | the shell sets the operator record to the offer's identity, then calls `identity.claimOffer` |
| Server with an HTTPS address | the holder logs in by key as the operator, opens a `node` offer, calls `identity.claimOffer` on the server |
| Server with no address | `ad4m-executor init` (or `link-code`) prints the device string and a terminal QR code; the holder pushes a ticket into the pre-join inbox; the executor claims through `claim_for_main` |
| Host, for a tenant | `host.join` calls `claim_for_tenant` ([Tenants](../../executor/managed-agents/tenants.md)) |
| Bot | the bot's executor claims in the bot's agent context ([Bots](bots.md)) |

Revocation on this executor: links by the revoked key at or after the revoking position become `Invalid`, late ones `Flagged` ([Link verdicts](../../../perspectives/links/verdicts.md)); `KeyResolver::subscribe` ends the key's sessions; a host's tenant sweep detaches the tenant. An app without the master key cannot revoke: it posts a request to the request box and records a pending revocation ([Device inbox and private record](inbox-and-record.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/identity/claim.rs](../../../../rust-executor/src/identity/claim.rs) (new): `ClaimService::{claim_for_main, claim_for_tenant}`; both return the claim's `kind` and `expires`.
- `OperatorRecord::admits_first_link` (new, in the operator record of [Operator and main agent](../../access/operator-and-main-agent.md)).
- [rust-executor/src/identity/service.rs](../../../../rust-executor/src/identity/service.rs) (new): the handlers above; events `link-ticket` and `inbox-message`.
- [ad4m-identity-core/src/link.rs](../../../../ad4m-identity-core/src/link.rs) (new): offer, claim, offer link, device code, match code; `build::add_claimed`.
- [cli/src/ad4m_executor.rs](../../../../cli/src/ad4m_executor.rs): `init --operator <did:scid>` prints the device string and QR code; new subcommands `link-code` and `rekey`.

</details>
