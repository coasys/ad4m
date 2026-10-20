# Devices

A **device** is anything that signs as the person: a phone, a computer, a browser, a server, a host, a bot. Each holds its own device key and appears in the key log only as that key. A device joins in six steps that always start from a **holder**, an app that holds the current master key: offer, hand-over, claim, match code, `add`, first signature. Devices talk to each other only through the **device inbox** in the shared store. An executor is a device too: it claims for its main agent or for a tenant, and never keeps the master key. Devices do not sync data with each other: each executor holds the neighbourhoods the person made or joined on it.

**Why:** Linking starts from the holder, so nothing enters a person's queue unless an app with the master key opened it; the first design, which started from the new device's QR code, let anyone flood the queue. The person confirms a match code on both screens, so a scan alone never adds a key. One inbox per device carries every message, so a rotation reaches a holder that joined weeks ago or is switched off; a short-lived relay channel for linking lost for that reason. A server, a host and a bot never receive the master key, so linking one gives it no power over the log.

**Parent:** [Identity](../README.md)

## Parts
- [Device linking](linking.md): offers and claims, the match code, how each kind of device links, revocation and re-keying, on the executor's side.
- [Device inbox and private record](inbox-and-record.md): sealed messages between devices, the request box, the sealed record of device names and addresses.
- [Master key](master-key.md): where the master key rests on each surface, how it reaches an app, and the executor's one moment with it.
- [Bots](bots.md): bot keys in the owner's log, pause records, the bot activity log.

<details><summary>How it works</summary>

```
 holder app (master key n)          shared store (via agent language)        new device (its own device key)
 1. open offer: commit(s), 5 min ─►  offer beside the log
 2. offer link / QR code with s ─────────────────────────────────────────►  
                                                                   ◄──────  3. claim: s, sign key, enc key, canKeepMaster, coSig
 4. match code on both screens; the person confirms on the holder
 5. append add (spends the claim) ─► key log seq N
                                                                   ◄──────  6. sees its key in the key index; signs with anchors
 (7. an app only, if ticked: master n sealed to the device's enc key, into its inbox)
```

| Device | Device key held by | May keep the master key | How it links |
|---|---|---|---|
| Desktop app | its executor's wallet key `main` | yes (the custody service keeps it) | as an app with its own executor |
| Phone app | the phone shell's vault | yes | as an app; no executor |
| Browser | the Connect frame on the auth domain, 30 days | no | at web sign-up in `incept`; later by a self-claimed offer |
| Server the person runs | its executor's wallet key `main` | no | `node` offer; the executor's operator record must name the identity first |
| Host (community host, Coasys cloud) | the host's wallet key for the tenant | no | `node` offer passed in `host.join` ([Tenants](../../executor/managed-agents/tenants.md)) |
| Bot | its executor's wallet key for the bot | no | `bot` offer with actions and expiry ([Bots](bots.md)) |

On the wire, the offer kind for an executor is `node` (`OfferKind::Node`); the name stays for format stability.

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/src/link.rs](../../../../ad4m-identity-core/src/link.rs) (new): `Offer`, `OfferKind { Device, Node, Bot }`, `Claim`, the offer link, the device code, the match code.
- [rust-executor/src/identity/claim.rs](../../../../rust-executor/src/identity/claim.rs) (new): `ClaimService` (`claim_for_main`, `claim_for_tenant`).
- [rust-executor/src/identity/inbox.rs](../../../../rust-executor/src/identity/inbox.rs) (new), [record.rs](../../../../rust-executor/src/identity/record.rs) (new), [requests.rs](../../../../rust-executor/src/identity/requests.rs) (new): the executor's inbox pull and its record and request-box calls.
- The holder flows (open offer, approve, push the master key, rotate, revoke, restore) live in the identity client core, [identity-client/](../../../../identity-client) (new), which the apps run ([Master key](master-key.md)).
- The store routes for offers, claims, inboxes, records and requests: [Shared store](../../../languages/other-kinds/agent-language/shared-store.md).

</details>
