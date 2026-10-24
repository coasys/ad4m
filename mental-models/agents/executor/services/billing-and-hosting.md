# Billing and hosting

**Billing** charges **compute credits** to managed agents for link writes and AI calls. Credits sit on the agent's `users` row, found by DID, and every charge goes into `compute_log`. **Hosting** is the executor's offer to managed agents: **host rates** per operation, a hot wallet, and credit top-ups paid through the **Unyt** DNA on the [Holochain service](holochain.md). **Free hosting** is on by default, and then nothing is billed.

**Why:** A hosted executor serves many managed agents on the operator's hardware and AI keys. Billing makes that cost visible and bounded per agent. Free hosting as the default means a personal executor never bills. Payments use a Holochain currency DNA, so they need no payment provider.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

Charging a link write:

1. Before the write, `check_compute_credits(did)` allows it when free hosting is on, the agent has free access, or credits > 0.
2. After the write, `bill_compute(did, amount, operation, summary)` deducts and logs (`deduct_credits_and_log`).
3. Only managed agents are billed. `add_link` and the other write paths call billing only when `AgentContext::managed_did()` is set.

Only local link writes (`add_link`, `add_links`, `link_mutations`, `update_link`), the AI handlers and the OpenAI-compatible routes bill. Links that arrive from link languages, Holochain gossip, SFU media and language `httpFetch` are not billed.

| Operation | Amount |
|---|---|
| link write | `host_rates["link write"]`, default 0.25 |
| AI call (`bill_ai_operation`) | units × `host_rates[model name]`; 0 if the model has no price |

Top-up:

1. Unyt signals from the alliance cell go to `unyt_service::handle_signal`.
2. That handler and the 30 s poll `check_pending_payments` read transactions where this executor is the payee, and `payment_requests` rows. They credit users.
3. The transaction's counterparty is the payer's wallet address (its agent key). `get_user_by_hot_wallet_address` finds the `users` row with that address; a payment from an unknown address credits nobody.

Operator settings: `runtime.setFreeHostingEnabled`, `runtime.setHostRates`; `hosting.info`, `hosting.wallet`, `hosting.setHotWallet`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/billing.rs](../../../../rust-executor/src/billing.rs): `check_compute_credits`, `bill_compute`, `bill_ai_operation`, `get_link_write_rate`.
- [rust-executor/src/unyt_service.rs](../../../../rust-executor/src/unyt_service.rs): alliance DNA install, proposals, `handle_signal`, `check_pending_payments`, `ALLIANCE_CALL_LOCK`.
- [rust-executor/src/api/hosting_ws.rs](../../../../rust-executor/src/api/hosting_ws.rs): `hosting.*` RPC.
- [rust-executor/src/api/runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs): free hosting and host-rate RPC.
- [rust-executor/src/perspectives/perspective_instance.rs](../../../../rust-executor/src/perspectives/perspective_instance.rs): billing on link writes.
- [rust-executor/src/db.rs](../../../../rust-executor/src/db.rs): `host_rates`, `compute_log`, `payment_requests`, credits on `users`.

</details>
