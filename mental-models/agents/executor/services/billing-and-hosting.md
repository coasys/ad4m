# Billing and hosting

**Billing** prices each link write and AI call of a managed agent by the host's **host rates**, and writes it into `compute_log`. One setting, the **charge mode**, decides whether anyone pays. In `Free`, the meter still prices and logs each operation (`amount = 0`, `list_amount = price`), and nothing is deducted or paid; a build with the `closed-beta` cargo feature pins `Free`. **Unyt** is the hosting currency: the executor's alliance cell on the [Holochain service](holochain.md) holds the host's earnings under its own Unyt agent key, and a payment to it would credit a tenant's **compute credits**. Every Unyt and billing call works; only the two that move money refuse under `Free`.

**Why:** A host serves many managed agents on the operator's hardware and AI keys, and the paid path must not need a rebuild later, so it is built and switched off. Pricing every operation under `Free` tests the rates with real usage and gives the tenant view its "would have cost" figure. A failed settings read under `Charge` refuses, where it used to turn charging off. Rates stay per operation, not per hour, because a new hourly meter would add work with no use while nobody pays. Payments use a Holochain currency DNA, so they need no payment provider.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

| Part | `Free` (pinned by the `closed-beta` feature) | `Charge` |
|---|---|---|
| `check_compute_credits` | passes | refuses at 0 credits; a failed read refuses |
| `bill_compute`, `bill_ai_operation` | price by `host_rates`; log `amount = 0`, `list_amount = price` | deduct and log the price |
| link writes | priced once, in `PerspectiveInstance`; the second RPC-side charge is gone | charged once |
| AI harness gate (`WsHarnessCreditGate`) | passes | bills through `bill_compute` |
| incoming Unyt payment (`handle_signal`, 30 s poll) | credits the payer's managed agent (a test transfer shows) | the same |
| `hosting.requestPayment`, `runtime.unytSendHot` | `409 free-in-closed-beta` | a proposal to the tenant's linked wallet; a payment from the host's ledger |
| `hosting.info`, `host.usage.spend` | `freeAccess: true` | the tenant's real state |

Where the mode comes from: the config key `billing.charge_mode` (default `"free"`), seeding the runtime row `free_hosting_enabled`. With the cargo feature `closed-beta`, `"charge"` stops the start (exit 78) and `runtime.setFreeHostingEnabled { enabled: false }` answers `409`. `compute_log` rows written under `Free` are deleted after 90 days.

Only managed agents are billed: the write paths call billing only when `AgentContext::managed_did()` is set. Only local link writes (`add_link`, `add_links`, `link_mutations`, `update_link`), the AI handlers and the OpenAI-compatible routes bill. Links that arrive from link languages, Holochain gossip, SFU media and language `httpFetch` are not billed.

| Balance | Held by | Stored in |
|---|---|---|
| a managed agent's credits | the host | `users.remaining_credits` of its `users` row; 1 credit = 1 HOT |
| a host's earnings | the host's alliance cell | the Unyt ledger under `unyt_agent_key` (`hosting.wallet`, `hosting.walletHistory`, `runtime.unytWalletBalance`) |

An incoming payment names the payer's wallet address (its agent key). `get_user_by_hot_wallet_address` finds the `users` row with that address; a payment from an unknown address credits nobody.

Operator calls: `runtime.hostRates`, `runtime.setHostRates` (also republishes the listing), `runtime.unytAgentKey`, `runtime.unytHotAgentPubkey`, `runtime.unytWalletBalance`, `runtime.unytWalletHistory`, `runtime.unytSendHot`, `runtime.unytReinstallDna`, `runtime.setUnytMembraneProof`, `runtime.unytVersionInfo`. The proof normally comes through the [hosting-index client](hosting-index.md).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/billing.rs](../../../../rust-executor/src/billing.rs): `check_compute_credits`, `bill_compute`, `bill_ai_operation`, `get_link_write_rate`; `ChargeMode`.
- [rust-executor/src/unyt_service.rs](../../../../rust-executor/src/unyt_service.rs): alliance DNA install, `get_or_create_agent_key`, `get_ledger`, `get_history`, `send_hot`, `create_proposal`, `handle_signal`, `check_pending_payments`.
- [rust-executor/src/api/hosting_ws.rs](../../../../rust-executor/src/api/hosting_ws.rs): `hosting.*` RPC.
- [rust-executor/src/api/runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs): rates, free access and the `runtime.unyt*` calls.
- [rust-executor/src/perspectives/perspective_instance.rs](../../../../rust-executor/src/perspectives/perspective_instance.rs): billing on link writes.
- [rust-executor/src/db.rs](../../../../rust-executor/src/db.rs): `host_rates`, `compute_log` (gains `list_amount`), `payment_requests`, credits on `users`.
- [rust-executor/src/config_file.rs](../../../../rust-executor/src/config_file.rs): the `billing.charge_mode` key.

</details>
