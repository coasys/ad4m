# Language addresses

A **language address** is the content hash of a language's `bundle.js`: `Qm` followed by the base58btc multibase string of a CIDv1 (codec `0x00`) over the SHA-256 of the bundle text. That multibase string starts with `z`, so every address starts with `Qmz`. The executor keys every runtime, settings file and storage directory by this address. Four **aliases** (`did`, `lang`, `neighbourhood`, `perspective`) stand for the addresses of the system languages.

**Why:** A hash address makes a changed bundle visible: its hash no longer matches the address. No registry decides which code an address means.

**Parent:** [Expressions and addresses](README.md)

<details><summary>How it works</summary>

| Form | Example | Resolves to |
|---|---|---|
| `<hash>://<addr>` | `QmzSYwd…://abc` | the language with that hash |
| alias `did` | `did:key:z6Mk…`, `did:scid:coasys:1:Qm…` | the agent language; the whole DID is the expression address. A `did:key` names a device key; a `did:scid` names a person ([Agent language](../other-kinds/agent-language/README.md)) |
| alias `lang` | `lang://<hash>` | the language language |
| alias `neighbourhood` | `neighbourhood://<addr>` | the neighbourhood language |
| alias `perspective` | `perspective://<addr>` | the perspective language |
| `literal:` | `literal:string:hello` | no language: see [Literal expressions](literals.md) |

1. `load_core_system_languages` registers the four aliases after it loads the system languages.
2. Controller entry points (`execute_on_language`, `get_expression`, `unload_language`, `language_by_ref`, …) resolve an alias to the hash first.
3. `expression_create` maps a hash back to its alias, so it returns `neighbourhood://…` rather than `<hash>://…`.
4. A `did:` URL (`did:key` or `did:scid`) has no `://`. Both URL parsers, `parse_expr_url` and `ExpressionRef::try_from`, treat it as a special case.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `calculate_language_hash`, `language_aliases`, `load_core_system_languages` (registers the aliases), `parse_expr_url`.
- [rust-executor/src/js_core/utils_extension.rs](../../../rust-executor/src/js_core/utils_extension.rs): op `hash`, the same function for language code (`hash` in `ad4m:host`).
- [rust-executor/src/types/core.rs](../../../rust-executor/src/types/core.rs): `ExpressionRef::try_from` (regexes for `<lang>://…`, `did:…`, `literal:`).
- [rust-executor/src/utils.rs](../../../rust-executor/src/utils.rs): `languages_directory`, `language_storage_directory`.

</details>
