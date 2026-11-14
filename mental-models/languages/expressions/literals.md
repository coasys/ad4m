# Literal expressions

A **literal expression** is a value encoded in the URL itself: `literal:string:<text>`, `literal:number:<n>`, `literal:boolean:<b>` or `literal:json:<percent-encoded JSON>`. No language and no isolate takes part: the executor encodes and decodes the value in Rust.

**Why:** Links need small values (names, numbers, timestamps, settings) as targets. A literal needs no network round trip and no store. It can still carry an author: `expression.create(content, "literal")` wraps the content in a signed expression first.

**Parent:** [Expressions and addresses](README.md)

<details><summary>How it works</summary>

1. `expression_create(…, "literal")` calls `agent::create_signed_expression(content, ctx)` and returns `literal:` + `literal_encode(signed_expression)`, a `json:` literal.
2. `get_expression("literal", part)` calls `literal_decode(part)` and then `verify_expression_proof`, so a signed literal gets `proof.valid`.
3. The percent-encoding keeps only RFC 3986 "unreserved" characters (`A–Z a–z 0–9 - _ . ~`), as the SDK's `Literal.toUrl()` does.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/literal.rs](../../../rust-executor/src/languages/literal.rs): `literal_encode`, `literal_decode`, `RFC3986_COMPONENT_ENCODE`.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): the `"literal"` branches in `get_expression`, `expression_create`, `is_immutable_expression`, `parse_expr_url`.
- [core/src/Literal.ts](../../../core/src/Literal.ts): SDK `Literal.from(…).toUrl()`, `Literal.fromUrl(…).get()`.

</details>