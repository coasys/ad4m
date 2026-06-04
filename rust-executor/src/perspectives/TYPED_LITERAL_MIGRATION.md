# Typed RDF literals on the wire

Design notes for moving `literal:*` URI-encoded targets to native RDF
literals carrying an `xsd:` datatype. Lives alongside the code so it
moves with the implementation.

## Today

Property writes through `resolve_property_value` emit IRI-shaped targets:

```
<src> <pred> <literal:string:hello>
<src> <pred> <literal:number:42>
<src> <pred> <literal:boolean:true>
<src> <pred> <literal:json:%7B...%7D>
```

Oxigraph treats each as a `NamedNode`. WHERE filters compare against the
same encoded form, which works but two costs leak:

1. The store carries the percent-encoding overhead on every value.
2. The custom `fn/parse_literal` SPARQL function exists solely to
   round-trip these IRIs back to typed values for `Ops` filters
   (`gt`/`lt`/`between`/`contains`/`not`) and for the legacy envelope
   unwrap path.

## Target

```
<src> <pred> "hello"
<src> <pred> "42"^^xsd:integer
<src> <pred> "true"^^xsd:boolean
<src> <pred> "{\"k\":\"v\"}"^^ad4m:json
```

- `xsd:string` is the default for unannotated strings, so the IRI tail
  vanishes for the common case.
- `xsd:integer` / `xsd:decimal` / `xsd:boolean` carry their native types,
  which means `FILTER(?x > 5)` works without manual casting.
- A custom `ad4m:json` datatype keeps JSON payloads first-class without
  invading `xsd:string` semantics.

This unblocks:

1. **Native typed comparison.** Oxigraph already implements xsd:integer
   ordering, xsd:boolean equality, etc. The `Ops` branch can compile to
   plain `FILTER(?x > 5)` without `fn/parse_literal`.
2. **No percent-encoding overhead.** Strings round-trip as-is.
3. **Index-friendly equality** stays unchanged — typed literals are still
   POS-indexable; the WHERE builder just emits `"hello"` instead of
   `<literal:string:hello>`.

## Migration path

`migration_version` 4 — runs after `migrate_signed_envelopes_to_plain_literals` (v3):

1. Walk every reifier.
2. If the triple-term object is `Term::NamedNode` whose IRI matches
   `literal:(string|number|boolean|json):.*`, decode it to the typed
   value and rebuild the link as a typed literal triple. The reifier
   IRI hashes the target string form (`literal_encode` output) so the
   hash stays stable — we keep the same reifier identity.
3. Persist `migration_version = 4`.

Idempotent on the same store; safe to run repeatedly.

## Compatibility

- `parse_literal_fn` keeps both branches active indefinitely so external
  callers writing the old form continue to query correctly.
- The WHERE builders emit the new form *and* a UNION fallback to the old
  IRI form during a transition window, then drop the fallback when
  migration confidence is high.
- `Literal.toUrl()` / `Literal.fromUrl()` in core stay as-is; they are
  a separate serialisation layer for non-storage purposes
  (`SmartLiteral`, SHACL action payloads).

## Open questions

- Should JSON go on a custom `ad4m:json` datatype, or `xsd:string` with
  the value being the JSON text? Custom datatype is cleaner; xsd:string
  has zero ecosystem cost.
- What does `:string` survive when a caller writes a value that happens
  to parse as a number? Per `literal_encode`, that's caller-decided —
  `Literal.from("42").toUrl()` → `literal:string:42` (always string).
  Migration should respect that explicit type marker.

## Cleanup after migration confidence

Once v4 is the baseline in production:

- Delete the envelope-unwrap branch in `parse_literal_fn` and
  `parse_literal_value`.
- Delete the `literal:` IRI emission paths in the WHERE builders.
- Drop the `fn/parse_literal` registration entirely. The `Ops` branch
  uses native xsd comparisons by then.

## SPARQL 1.2 window functions

Blocked on Oxigraph 0.6 (not released yet — latest is 0.5.8 as of
2026-04-28). Once available, the last-write-wins / scalar aggregation
work currently done in Rust (`model_query/hydration.rs::hydrate_one`)
can move into the SPARQL query itself via
`SAMPLE(?target) OVER (PARTITION BY ?source ?predicate ORDER BY ?timestamp DESC)`,
removing the Rust-side fold over rows.
