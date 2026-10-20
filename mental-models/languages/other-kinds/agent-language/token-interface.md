# Token interface

The **token interface** (`TokenSpec` in TypeScript, `TokenCapability` in Rust) is the set of `token*` calls through which an agent language would list assets, read a balance, send a transfer and list received transfers. It exists as an interface only. The default agent language does not implement it, and the executor answers every token call with `{ code: "disabled" }`. No executor feature calls it.

**Why:** Fixing the shape before any implementation exists lets several implementations plug in behind the agent language with no change to apps or to the executor. Accounts are DIDs (`did:scid` or `did:key`). Amounts are decimal strings with a `decimals` count, so no float and no 2^53 limit. The interface carries no signature: the language signs through its agent context, as it does for expressions. The hosting index's rates and membrane proofs stay on their own executor calls ([Billing and hosting](../../../agents/executor/services/billing-and-hosting.md)).

**Parent:** [Agent language](README.md)

<details><summary>How it works</summary>

| Call | Answers |
|---|---|
| `assets()` | asset ids, for example `unyt:<id>` |
| `balance(account, asset)` | `TokenBalance { account, asset, amount, asOf }` |
| `send(to, asset, amount, memo?)` | `TokenTransfer { id, from, to, asset, amount, memo, time, state }`, state `pending`, `settled` or `refused` |
| `received(cursor, limit)` | a page of transfers to the agent, newest last |
| `onTransfer(callback)` | each new transfer and each state change, as a language signal tagged `token-transfer` |

1. The executor routes `Capability::TokenAssets`, `TokenBalance`, `TokenSend` and `TokenReceived` only to the agent language, as it does the identity calls.
2. Errors: `disabled`, `unknown-asset`, `unknown-account`, `insufficient`, `refused`, `unavailable`.

</details>

<details><summary>Where in the code</summary>

- [ad4m-ldk/js/src/types.ts](../../../../ad4m-ldk/js/src/types.ts): `TokenSpec`, `Amount`, `TokenBalance`, `TokenTransfer`, `TokenErrorCode`.
- [ad4m-ldk/rust/src/traits.rs](../../../../ad4m-ldk/rust/src/traits.rs): `TokenCapability`.
- [rust-executor/src/languages/capability.rs](../../../../rust-executor/src/languages/capability.rs): `Capability::Token*`, and the `disabled` answer when the agent language lacks them.

</details>
