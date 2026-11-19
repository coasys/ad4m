# RFC 8785 test data

`input/*.json` and `output/*.json` are the JCS test data that RFC 8785 points to, from
<https://github.com/cyberphone/json-canonicalization/tree/master/testdata> (commit
`32a48195b9b20fe67b1795e39e4509f14d860b45`). `tests/rfc8785.rs` checks that
`canonical_json` of each input, parsed, gives the output byte for byte.

This crate refuses every floating-point number, so two files differ from the source:

- `values.json`: the `numbers` member (all floats) is left out of the input and the output.
- `structures.json`: the input's `56.0` reads `56` (the output already holds `56`).
