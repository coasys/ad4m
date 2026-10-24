# File storage languages

A **file storage language** is an expression language whose expressions are files. `expressionCreate({name, file_type, data_base64})` stores the file and returns an address. `expressionGet(address)` returns a signed `FileExpression` with the data. Apps use these languages for profile pictures and attachments; links then point at the file's expression URL.

**Why:** Files are too large for links and literals. A separate language lets an app choose a store per file: a Holochain DHT, a central bucket, or local. The executor only sees an expression URL.

**Parent:** [Other language kinds](README.md)

<details><summary>How it works</summary>

| Package | Store | Notes |
|---|---|---|
| `file-storage` | Holochain DNA `file_storage` | the data is compressed with pako and split into chunks; the expression holds `chunks_hashes` |
| `centralized-file-storage` | Cloudflare Workers KV proxy | one signed expression with the base64 data inline, keyed by the hash of `{name, size, file_type, data_base64}`; reads go through a presigned URL |
| `local/file-storage-language.js` | `ad4m:host` KV | local seed only |

1. The default seed publishes `file-storage` and `centralized-file-storage` (`alsoPublish`), so executors can install them by address. Neither is a system language and neither has an alias.
2. The MCP profile tool (`set_agent_profile_picture`) installs one file storage language with `language_by_ref` (trust check) before it uploads a picture. It names that language by a fixed address constant in its source.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/file-storage/index.ts](../../../bootstrap-languages/file-storage/index.ts), [bootstrap-languages/file-storage/hc-dna/](../../../bootstrap-languages/file-storage/hc-dna): Holochain variant.
- [bootstrap-languages/centralized-file-storage/index.ts](../../../bootstrap-languages/centralized-file-storage/index.ts): central variant.
- [bootstrap-languages/local/file-storage-language.js](../../../bootstrap-languages/local/file-storage-language.js): local variant.
- [rust-executor/src/mcp/tools/profiles.rs](../../../rust-executor/src/mcp/tools/profiles.rs): the MCP caller.

</details>