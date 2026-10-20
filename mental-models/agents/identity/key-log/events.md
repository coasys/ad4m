# Key-log events

A key log holds five kinds of event, each one canonical JSON (RFC 8785, JCS) with Data Integrity proofs under the domain `coasys:kel`. `fold` replays them, in order, into a `KeyState`: the masters, the next-key commitment, and every device key with the position where it was added and, if so, revoked. `key_valid_at(key, seq)` answers whether a key could sign at one position of the log. Master keys come from the **12 words**; device keys come from random bytes.

**Why:** Each event is signed by the key that the state before it names, so replay alone proves the log, and any copy can be checked offline. A revoked key stays valid for positions before its revocation, so links that it signed earlier keep their meaning. Only the words produce master key #n+1, so a rotation always replaces master key #n, whoever else holds it. Format 1 refuses unknown fields, so no personal data can enter the log by accident.

**Parent:** [Key log](README.md)

<details><summary>How it works</summary>

| `op` | Signed by | Does |
|---|---|---|
| `incept` | master key #0 and each listed device key | names the identity (`kind: "human"`), its first master key, the next-key commitment, its first device keys; no bot key |
| `add` | the current master key | adds exactly one device key; spends a confirmed claim (`offer`, `coSig`; [Device linking](../devices/linking.md)); a bot key carries `kind: "bot"`, `can`, `expires` |
| `revoke` | the current master key | ends one or more device keys from this position |
| `rotate` | the revealed next master key | makes it master #n+1, commits to the next, may revoke keys; `next: null` ends the identity |
| `scope` | the current master key | changes a bot key's `can` and `expires` |

| Item | Encoding |
|---|---|
| SCID, event hash | base58btc of a SHA-256 multihash of `JCS(event without proof)`; the SCID is event #0's hash with `"scid": "{SCID}"` in place |
| Signing key, encryption key | multikey `z6Mk…` (Ed25519), `z6LS…` (X25519) |
| A device key as an author | `did:key:<signing multikey>`, as before identities |
| Key ids | `m0`, `m1`, … for masters; `d1`, `d2`, … for device keys, in order, never reused |
| Next-key commitment | hash of `JCS({key: master n+1, salt: salt n+1})` |
| Key life (`KeyLife`) | `Standing`; `Until(expires)` (the web app's 30-day browser key); `Bot { can, expires }` |

Keys from the words (`words` feature): 12 BIP-39 English words → seed → master key #n by SLIP-0010 Ed25519 (`m/7133'/0'/n'`) and `salt_n` by HKDF. A restore from the words reproduces every commitment. `recovery_secret(seed)` gives the key of the private record ([Device inbox and private record](../devices/inbox-and-record.md)). No executor RPC creates, returns or accepts the words.

Replay rules that callers see: the `KeyState` reads no clock; the caller passes the time it trusts to `key_valid_at_time`. One signing multikey appears at most once in a log, and the store refuses it in any second log. `fold` errors are `ChainError::{Seq, Prev, WrongChain}` from the module, or `ChainError::Policy(FoldError)` from the KEL rules.

An existing agent's `did:key` never becomes a master key. It joins an identity as a device key that co-signs the `incept`, so its earlier links show under the person with no rewrite. Other existing DIDs connect through [entanglement proofs](../signing-and-verification.md).

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/src/lib.rs](../../../../ad4m-identity-core/src/lib.rs) (new): `Event`, `Op`, `DeviceKey`, `KeyLife`, `BotScope`, `KeyState`, `KeyRecord`, `KelPolicy`, `KelChain`, `fold`, `derive_scid`, `event_hash`, `FoldError`, `signing_bytes`.
- `words` module (feature `words`): `generate`, `parse`, `seed`, `master_key`, `next_commitment`, `recovery_secret`.
- `build` module (feature `build`): `Signer`, `incept_human`, `cosign`, `revoke`, `scope`, `rotate`, `add_claimed`.
- `pow` module: `solve`, `check` (the brake's proof of work, bound to the SCID).
- [docs-src/identity/did-scid-coasys-1.md](../../../../docs-src/identity/did-scid-coasys-1.md) (new): every field, every signing domain, the test vectors.

</details>
