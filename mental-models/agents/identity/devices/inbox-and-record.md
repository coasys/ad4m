# Device inbox and private record

The **device inbox** is one mailbox per device in the shared store. A sender seals a message to the receiver's encryption key (`enc`) and pushes it; the receiver pulls it. Before a device joins, its inbox address comes from its device code and lives 5 minutes; after it joins, only the identity's current master key may write to it, only the device's own key may read it, and a message waits up to 30 days. The **request box** holds short signed requests between the person's own devices, which change nothing until an app with the master key approves them. The **private record** is one sealed record per identity: device names, kinds, places, addresses, which devices hold the master key, and pending revocations.

**Why:** One push-and-pull channel serves linking, master-key hand-over and rotation, so a holder that was switched off for weeks still gets the next master key. The store sees only commitments, public keys and ciphertext. Names and addresses stay out of the public log and inside the private record, which is wrapped to every valid device and to a recovery key from the 12 words; without that recovery key, a person who lost every device could restore the identity but not see which devices to lock out.

**Parent:** [Devices](README.md)

<details><summary>How it works</summary>

| Message key | Read with | Sealed to |
|---|---|---|
| before the device joins (`InboxAddress::Pre`) | the device's signing key from its device string | its `enc` key |
| after it joins (`InboxAddress::Joined { scid, key_id }`) | its device key in the log | its `enc` key in the log, for every message including a master key |

For an executor, `enc` is the X25519 key that `did_key` derives from the Ed25519 wallet key; the secret never leaves the wallet (`WalletBackend::open_sealed`). The executor:

1. Pulls its joined inbox at start and every 15 minutes, and emits `inbox-message { id, type }` to the main agent's local operator sessions.
2. Lists messages by id and body type only (`identity.inbox`, local only); never a body.
3. Treats a `Master` body by where it runs: the desktop app's executor zeroes the plaintext and leaves the message for `identity.takeMaster` ([Master key](master-key.md)); every other executor deletes it, zeroes it and logs an alarm.
4. Acknowledges with `identity.inboxAck` (local only), and passes master-signed writes with `identity.inboxPut`.

| Store | Life | Executor calls |
|---|---|---|
| Inbox message | 5 minutes before join; 30 days after | `identity.inbox`, `identity.inboxAck`, `identity.inboxPut` |
| Request (for example "confirm this revocation") | 7 days | `identity.requests`, `identity.postRequest` (signed with this agent's device key), `identity.deleteRequest` |
| Private record | until the next write; each write that removes a device uses a new record key | `identity.record` (the sealed bytes; the app opens them), `identity.recordPut` |

A pending revocation in the record carries a reason: `left-host` (a tenant left a host from an app without the master key) or `account-deleted`. Every app with the master key offers it at its next unlock.

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/src/inbox.rs](../../../../ad4m-identity-core/src/inbox.rs) (new): `InboxAddress`, `pre_address`, the message envelope, HPKE sealing, `check_payload`.
- [ad4m-identity-core/src/record.rs](../../../../ad4m-identity-core/src/record.rs) (new), [requests.rs](../../../../ad4m-identity-core/src/requests.rs) (new): `SealedRecord`, `DeviceEntry`, `Place`, `RevocationReason`; the request bodies.
- [rust-executor/src/identity/inbox.rs](../../../../rust-executor/src/identity/inbox.rs) (new): the pull loop and the `Master` rule.
- [rust-executor/src/identity/record.rs](../../../../rust-executor/src/identity/record.rs) (new), [requests.rs](../../../../rust-executor/src/identity/requests.rs) (new): the record and request-box handlers.
- [rust-executor/src/wallet.rs](../../../../rust-executor/src/wallet.rs): `WalletBackend` gains `open_sealed(name, sealed, info)`.

The executor never opens the private record: `identity.record` returns sealed bytes, and `identity.devices` adds names only when the caller passes the opened record.

</details>
