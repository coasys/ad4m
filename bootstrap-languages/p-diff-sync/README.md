# Perspective Diff Sync

Git like holochain syncing DNA for sharing of mutation to a shared perspective.

## Link proofs

A link's proof is `{ signature, key, anchor? }`. The executor puts `anchor` (a key-log seq) on
links that an agent bound to a `did:scid` identity signs; such a link's signature covers
`coasys:link` `{data, timestamp, anchor}`. The DNA stores and returns `anchor` unchanged, and
leaves it out when the link has none, so an unanchored link and an entry written before anchors
existed encode to the same bytes as before (msgpack reads the missing field as `None`).

Adding the field changed the DNA hash, and with it the language address. A neighbourhood made
with the old DNA keeps the old language. That language drops `anchor`, so an anchored link
synced through it reaches the other members without `anchor` and fails their signature check.
Unanchored links sync there as before.

## Tests

- `cd hc-dna && bash build.sh` builds the DNA and hApp into `hc-dna/workdir`.
- `cd hc-dna && cargo test --lib -- --test-threads=1` runs the zome unit tests.
- `cd hc-dna/tests/sweettest && cargo test --test integration` runs the conductor tests
  against the built DNA.
- `pnpm test` runs the TypeScript tests.
