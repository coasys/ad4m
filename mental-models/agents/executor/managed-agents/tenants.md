# Tenants

A **tenant** is a person who uses an executor that they do not run. The host keeps one **tenant record** per person, keyed by the person's `did:scid`, with its own wallet key (a device key in the person's log) and its own data key. `host.join` admits a person through the **join gate**: the host checks the identity, its **sign-up setting** (anyone, by invite, or no one) and capacity, reads `vouched: true` from the person's key log, then makes the managed agent and claims the person's `node` offer with its new key. The record turns **active** when the person's app appends the `add`. A **held** tenant is a guest who kept their work on a host whose setting does not admit them: it can read, export and leave, not write.

**Why:** A tenant is a device of the person, so the host holds only a device key and never a master key, and the person can lock it out from any app. The host stores no email address: the email check happens once on the platform, and the host reads only the "vouched" flag from the store's signed log page. A new executor starts at "no one" with a notice to its operator, so no host takes strangers by accident. A held state, not a refusal, after a guest's "Keep this", because the guest's data already sits on that host. A usage meter separate from billing counts each tenant's neighbourhoods, storage and AI runs for the tenant view; no per-tenant limit reads it.

**Parent:** [Managed agents](README.md)

<details><summary>How it works</summary>

The join gate, in order (the first failure answers its code):

| # | Check | Refusal |
|---|---|---|
| 1 | multi-user mode on | `not-a-host` |
| 2 | rate limit per network address | `rate-limited` |
| 3 | the `did:scid` has a key log | `unknown-identity` |
| 4–5 | a detached record rejoins; an active one refuses; a held one turns active; a pending one is replaced | `already-a-tenant` |
| 6 | sign-up setting; for "by invite", an unused, unrevoked, unexpired invite | `signups-closed`, `invite-invalid` |
| 7 | `max_tenants` (active + pending) | `host-full` |
| 8 | `vouched: true` in the store's signed `LogPage` | `email-check-required` |
| 9 | the offer link names this `did:scid`; after the claim, `kind = node` and not expired | `offer-invalid` |

Then: one transaction (invite used, `TenantRecord` pending, the `users` row), a person data key, a wallet key, `ClaimService::claim_for_tenant`. The host polls the key log every 5 seconds; at `add` the record turns active and `tenant-joined` goes to the operator. A pending record expires after 10 minutes and is removed with its keys.

The tenant's `did:scid` lives only in `tenants.did_scid`. The tenant's `users` row is found by `username = tenants.id`. Code that keys on `user_email` holds the tenant id there as an opaque value.

| State | Sessions | Next |
|---|---|---|
| `Pending` | none | `Active`, or removed |
| `Active` | key login | `Detached` (leave with keep, key revoked), `Deleted` |
| `Held` | read, export, `host.usage`, `host.leave` only (`not-admitted` otherwise) | `Active` (a passing `host.join`), `Detached`, `Deleted` |
| `Detached` | none; the key is destroyed, the data stays | `Active` (rejoin, new key), `Deleted` |
| `Deleted` | none; a tombstone with no personal data | — |

| Method | Guard | Does |
|---|---|---|
| `host.info`, `host.join`, `host.joinStatus` | open; CORS for any origin, no credentials | `HostInfo` (name, operator, executor DID, sign-up, `admitsGuests`, free access, full); the join; its state |
| `host.usage` | own tenant | `TenantUsage`: held neighbourhoods, private perspectives, storage, AI runs today, spend |
| `host.policy`, `host.setPolicy` | operator | the setting (seeded from the config's `host` section; `host.pinned` fields refuse with `pinned-by-config`); `choiceNeeded` while no choice exists |
| `host.createInvite`, `host.listInvites`, `host.revokeInvite` | operator | invite links `https://<host>/join#invite=<secret>`; the host stores only the secret's hash |
| `host.tenants`, `host.removeTenant` | operator | the tenant list; deletes a detached tenant's data |

Coasys cloud is one such host: its config file pins `host.signup` to `"by_invite"`, sets `max_tenants`, free access and Holochain on, and the operator record names a Coasys identity.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/hosting/tenants.rs](../../../../rust-executor/src/hosting/tenants.rs) (new): `TenantRecord`, `TenantState`, `JoinRoute`, the join gate, activation, `admit_claimed_guest`.
- [rust-executor/src/hosting/policy.rs](../../../../rust-executor/src/hosting/policy.rs) (new): `HostPolicy`, `SignupPolicy`, `PinnedFields`, `HostDisplay`, `Invite`; the `host_policy` settings row.
- [rust-executor/src/hosting/meter.rs](../../../../rust-executor/src/hosting/meter.rs) (new): `StorageMeter`, the `tenant_ai_runs` count.
- [rust-executor/src/api/host_ws.rs](../../../../rust-executor/src/api/host_ws.rs) (new): `host.*`, `HostInfo`, `JoinRequest`, `JoinResult`; events `tenant-joined`, `tenant-left`, `tenant-usage-changed`, `host-policy-changed`.
- [rust-executor/src/db.rs](../../../../rust-executor/src/db.rs): tables `tenants`, `host_invites`, `tenant_ai_runs`.
- [core/src/hosting/HostClient.ts](../../../../core/src/hosting/HostClient.ts) (new): the SDK client.

</details>
