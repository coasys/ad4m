# Expressions and addresses

An **expression** is data signed by an agent: `{author, timestamp, data, proof}`. An **expression URL** names it as `<language address or alias>://<expression address>`. To read it, the executor asks that language for the expression and checks the signature.

**Why:** The URL carries the address of the code that can resolve it. An executor that does not have the language can fetch it by address from the language language and check the hash; `expression.get` resolves only through a language that is already loaded. The signature lets any reader check the author without trusting the store.

**Parent:** [Languages](../README.md)

## Parts
- [Language addresses](addresses.md): how the bundle hash is computed, the four aliases (`did`, `lang`, `neighbourhood`, `perspective`), and how the executor parses a URL.
- [Literal expressions](literals.md): the `literal:` URLs that carry a small value in the URL itself, with no language behind them.

<details><summary>How it works</summary>

`expression.get(url)`:

1. `LanguageController::parse_expr_url` splits the URL into language and expression address.
2. `literal:` URLs decode in Rust. Other URLs go to the language: `expressionGet(address)` in its isolate. `get_expression` uses `execute_on_language`, so every read runs as the main agent.
3. If the language says `isImmutableExpression(address)`, the executor reads the SQLite `expression` cache first, and writes to it after a fetch.
4. `verify_expression_proof` sorts the keys of `data`, checks the signature with `agent::signatures::verify`, and sets `proof.valid` / `proof.invalid`.

`expression.create(content, language)`:

1. The executor calls `expressionCreate(content)` (or `expressionAddressOf`) in the isolate, with the caller's `AgentContext`.
2. The language signs with `agentCreateSignedExpression` and stores the expression.
3. The executor returns `<alias or address>://<address>`. For the `did` alias it returns the address alone (a DID).

**Interactions** are named actions that a language offers on one of its expressions:

| RPC | Does |
|---|---|
| `expression.interactions(url)` | lists the actions, from the language's optional `interactions` export |
| `expression.interact(url, call)` | runs one action: the interaction's own `execute` (TypeScript languages) or the `expressionInteract(address, name, params)` export (Rust ALDK languages) |

The parameters must parse as JSON before the executor puts them into the call script.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/types/core.rs](../../../rust-executor/src/types/core.rs): `Expression<T>`, `ExpressionProof`, `ExpressionRef`, `LanguageRef`.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `get_expression`, `verify_expression_proof`, `expression_create`, `is_immutable_expression`, `expression_interactions`, `expression_interact`.
- [rust-executor/src/api/expressions_ws.rs](../../../rust-executor/src/api/expressions_ws.rs): RPC `expression.get`, `getRaw`, `getMany`, `create`, `interactions`, `interact`.
- [core/src/expression/](../../../core/src/expression): SDK types and client.
- [docs-src/pages/expressions.mdx](../../../docs-src/pages/expressions.mdx): user docs.

</details>