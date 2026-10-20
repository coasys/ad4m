# Neighbourhood and perspective languages

The **neighbourhood language** (alias `neighbourhood`) stores **neighbourhood expressions**. A neighbourhood expression is a signed `Neighbourhood { link_language, meta }`: it names the link language of a shared perspective and carries its public metadata links. A neighbourhood URL is `neighbourhood://<address>`. The **perspective language** (alias `perspective`) stores nothing: `get` returns `null` and `create` returns `""`. Every executor still loads it as a system language and registers the `perspective` alias.

**Why:** Joining needs only one string. The URL leads to the neighbourhood expression. The expression names the link language. The link-language address leads to the code ([Installing languages](../runtime/installing/README.md)). Publishing and joining as a whole: [Publishing and joining](../../perspectives/neighbourhoods/publishing-and-joining.md).

**Parent:** [Other language kinds](README.md)

<details><summary>How it works</summary>

The publish and join steps are on [Publishing and joining](../../perspectives/neighbourhoods/publishing-and-joining.md). This page covers only how the neighbourhood expression is stored and found:

| Step | Executor call | In `neighbourhood-language` |
|---|---|---|
| store | `create_neighbourhood_with_context` → `expression_create(<neighbourhood language>, Neighbourhood, ctx)`, signed as the caller | address = `hash(JSON.stringify(neighbourhood))`; the signed expression goes to the Cloudflare KV proxy; an existing key is refused |
| address | the URL is `neighbourhood://<address>` | — |
| read | `get_neighbourhood(address)` → `get_expression` → `DecoratedNeighbourhoodExpression` | `expressionGet(address)` reads the KV proxy |

The address hashes the unsigned content. Two agents who publish the same `Neighbourhood` JSON get the same address; the store refuses the existing key, so the second `expressionCreate` throws.

The local variant (`local/neighbourhood-language.js`) keeps the same shape in the `ad4m:host` key-value store or a shared directory.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/neighbourhood-language/index.ts](../../../bootstrap-languages/neighbourhood-language/index.ts): default neighbourhood language (`neighbourhood-store`).
- [bootstrap-languages/local/neighbourhood-language.js](../../../bootstrap-languages/local/neighbourhood-language.js): local one.
- [bootstrap-languages/perspective-language/index.ts](../../../bootstrap-languages/perspective-language/index.ts), [bootstrap-languages/local/perspective-language.js](../../../bootstrap-languages/local/perspective-language.js): perspective language.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `create_neighbourhood_with_context`, `get_neighbourhood`.
- [rust-executor/src/neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): publish and join.
- [rust-executor/src/types/core.rs](../../../rust-executor/src/types/core.rs): `Neighbourhood`, `NeighbourhoodExpression`.

</details>