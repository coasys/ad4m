# Leaving a host

A tenant leaves a host in four steps: **export** (offered, not forced), **keep or delete** the data there, the host's `host.leave { keepData }` through the tenant's own session, then the **revocation** of the host's device key, which the person's app signs with the master key. `host.leave` revokes the tenant's grants, destroys its wallet key, and then keeps the data (`Detached`) or deletes it and its data key (`Deleted`). A revocation from elsewhere reaches the host through its **revocation sweep**, which detaches the tenant as if it had left with "keep".

**Why:** The host's leave call runs before the revocation, because the revocation ends the session that the leave call needs. The host destroys the device key before the revocation, so nothing signs as the person in between, whether or not the revocation arrives. Moving neighbourhoods between devices does not exist, so leaving means export first. Kept data has no timer: it stays until the person rejoins or asks, or the operator removes it, because the person decided what this host holds.

**Parent:** [Managed agents](README.md)

<details><summary>How it works</summary>

`host.leave`, in this order:

1. Mark the record `leaving`: refuse new writes.
2. Revoke every grant of the tenant, which closes its sockets.
3. Destroy the wallet key and save the wallet.
4. **Keep:** state `Detached`; perspectives, the tenant's AI models and notifications stay; the data key stays wrapped under the root key.
5. **Delete:** remove each perspective the tenant owns alone (rows, Oxigraph store directory, subscriptions); remove the tenant from `owners` of shared ones; remove its notifications, tasks, AI models, `compute_log` and `tenant_ai_runs` rows and its `users` row; `DataKeys::destroy_person`. State `Deleted`.
6. Emit `tenant-left { kept, cause }` to the operator; return `LeaveResult` with `keyDestroyedAt` and `shreddedEverywhereAt` (equal: no backups exist).

| Path | Trigger | Result |
|---|---|---|
| Revocation sweep | every 10 minutes, and at each key login: an active or held tenant's key reads revoked | `Detached`, wallet key destroyed, grants revoked, `tenant-left { cause: "revoked" }` |
| Rejoin | `host.join` by the same `did:scid` for a `Detached` record that passes the gate | a new wallet key and claim; `Active` with the old perspectives; their old links keep the old author key and verify as before |
| Operator removal | `host.removeTenant { tenantId }` on a detached tenant | the delete of step 5 |
| No master key on this app | the app cannot sign the revocation | steps 1–3 still run; the app records a pending revocation `left-host` in the private record |

A failed `host.leave` (the host is down) followed by the revocation ends as "keep": the sweep detaches the record, and the data stays until the person asks again.

A deleted tenant's Oxigraph store files are deleted, not shredded ([Encrypted stores](../data/encrypted-stores.md)). Neighbourhood data stays with the other members through the link language.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/hosting/tenants.rs](../../../../rust-executor/src/hosting/tenants.rs) (new): `host.leave`, the sweep, rejoin, `host.removeTenant`, `LeaveResult`.
- [rust-executor/src/at_rest/data_keys.rs](../../../../rust-executor/src/at_rest/data_keys.rs) (new): `destroy_person`, `remove_owner`, `destroy_store`.
- [rust-executor/src/agent/grants.rs](../../../../rust-executor/src/agent/grants.rs) (new): `Grants::revoke_agent`.
- [rust-executor/src/identity/resolver.rs](../../../../rust-executor/src/identity/resolver.rs) (new): the key verdicts the sweep reads.

</details>
