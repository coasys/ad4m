# Links

A **link** (`Link`) is a triple of strings, usually URIs: `source`, optional `predicate`, `target`. A **link expression** (`LinkExpression`) is a link signed as an expression: it adds the author, an RFC 3339 timestamp, a signature proof and a link status. The author is the `did:key` of the **device key** that signed the link. When that key sits in a person's key log, the proof also carries an **anchor**: the log position at which the key signed. A **literal target** (`literal:string:…`, `literal:number:…`, `literal:boolean:…`, `literal:json:…`) carries a value instead of an address.

**Why:** A link is the smallest signed statement an agent can make. One signature per link, not per perspective, lets links sync one by one through any link language, and every reader can still check each link. The author field names the device key, not the person's `did:scid`: links keep their format, and the resolver maps the key to the person ([Link verdicts](verdicts.md)). The signature covers the anchor, so nobody who relays a link can move it to a log position where the key fails. Values are encoded into the target string, so a link stays a plain triple on the wire. The store turns them back into typed RDF literals.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

1. A caller sends a `Link` (`perspective.addLink`). `Link::validate` refuses an empty source or target; it does not require URIs, so bare strings such as `"root"` are valid. `Link::normalize` turns an empty predicate into `None`.
2. `create_signed_expression(link, context)` signs the normalized link with the device key of the agent in the caller's `AgentContext` (main agent or a managed agent; see [Signing and verification](../../agents/identity/signing-and-verification.md)).
   - The agent has a person (`identity` is set): the proof carries `Anchor { kel_did, key_id, kel_seq, kel_hash }`, and the signed bytes are `signing_bytes("coasys:link", {data, timestamp, anchor})`.
   - A **device-only identity** (no key log: a guest, an agent made before identities): no anchor, and the signed bytes are the same as before identities.
3. The store checks the signature on every insert, also for links that arrive from the link language, and stores a link verdict next to the link. It never trusts a verdict from the caller. A link that fails is stored, not refused ([Link verdicts](verdicts.md)).
4. On read the executor returns a `DecoratedLinkExpression`: the same fields plus the link's verdict and `status`.
5. `Literal` ([Literal expressions](../../languages/expressions/literals.md)) encodes a value as `literal:<type>:<percent-encoded value>`.

`agent.get` keeps `did` as the device key, because apps compare it with link authors; the new field `identity` carries the person's `did:scid`. The field `author` always holds a device key. Code that needs the person calls `author_of` ([Link verdicts](verdicts.md)); code that compares `author` with `agent.did` compares devices.

</details>

<details><summary>Where in the code</summary>

- [types/core.rs](../../../rust-executor/src/types/core.rs): `Link` (`validate`, `normalize`), `LinkExpression`, `ExpressionProof` (gains `anchor: Option<Anchor>`), `DecoratedLinkExpression`, `PerspectiveDiff`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `LinkInput`, `LinkExpressionInput`, `LinkQuery`, `LinkMutations`, `LinkStatus`, `DecoratedPerspectiveDiff`.
- [agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `create_signed_expression`. [agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): `verify`, the one dispatch for anchored and unanchored links.
- [ad4m-identity-core/](../../../ad4m-identity-core) (new): `signing_bytes` and the signing-domain registry (`coasys:link`).
- [languages/literal.rs](../../../rust-executor/src/languages/literal.rs) and [core/src/Literal.ts](../../../core/src/Literal.ts): literal encoding.
- [perspectives/model_query/utils.rs](../../../rust-executor/src/perspectives/model_query/utils.rs): `parse_literal_value`, the literal decoder of model queries.
- SDK: [core/src/links/](../../../core/src/links).

</details>
