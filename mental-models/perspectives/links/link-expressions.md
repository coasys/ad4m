# Links

A **link** (`Link`) is a triple of strings, usually URIs: `source`, optional `predicate`, `target`. A **link expression** (`LinkExpression`) is a link signed as an expression: it adds the author's DID, an RFC 3339 timestamp, a signature proof and a link status. A **literal target** (`literal:string:…`, `literal:number:…`, `literal:boolean:…`, `literal:json:…`) carries a value instead of an address.

**Why:** A link is the smallest signed statement an agent can make. One signature per link, not per perspective, lets links sync one by one through any link language, and every reader can still check each link. Values are encoded into the target string, so a link stays a plain triple on the wire. The store turns them back into typed RDF literals.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

1. A caller sends a `Link` (`perspective.addLink`). `Link::validate` refuses an empty source or target; it does not require URIs, so bare strings such as `"root"` are valid. `Link::normalize` turns an empty predicate into `None`.
2. `create_signed_expression(link, context)` signs the normalized link as the agent in the caller's `AgentContext` (main agent or a managed agent; see [Signing and verification](../../agents/identity/signing-and-verification.md)). The result is a `LinkExpression`.
3. On read the executor returns a `DecoratedLinkExpression`: the same fields plus `proof.valid` / `proof.invalid` and `status`.
4. The store computes the signature verdict on every insert (`LinkExpression::compute_proof_valid`) and stores it next to the link. It never trusts a verdict from the caller. A link that fails verification is stored, not refused. Model queries leave it out by default; link queries and SPARQL return it.
5. `Literal` ([Literal expressions](../../languages/expressions/literals.md)) encodes a value as `literal:<type>:<percent-encoded value>`.

</details>

<details><summary>Where in the code</summary>

- [types/core.rs](../../../rust-executor/src/types/core.rs): `Link` (`validate`, `normalize`), `LinkExpression` (`compute_proof_valid`), `DecoratedLinkExpression`, `DecoratedExpressionProof`, `PerspectiveDiff`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `LinkInput`, `LinkExpressionInput`, `LinkQuery`, `LinkMutations`, `LinkStatus`, `DecoratedPerspectiveDiff`.
- [agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `create_signed_expression`.
- [languages/literal.rs](../../../rust-executor/src/languages/literal.rs) and [core/src/Literal.ts](../../../core/src/Literal.ts): literal encoding.
- [perspectives/model_query/utils.rs](../../../rust-executor/src/perspectives/model_query/utils.rs): `parse_literal_value`, the literal decoder of model queries.
- SDK: [core/src/links/](../../../core/src/links).

</details>
