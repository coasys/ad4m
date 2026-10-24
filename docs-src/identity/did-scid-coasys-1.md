# `did:scid:coasys:1` — format 1 of the Coasys key log

Status: draft for review.
Reference implementation: `ad4m-identity-core` on `ad4m-hash-chain`. Test vectors:
`ad4m-identity-core/vectors/*.json`. Every build of the core (native, `wasm32`, with and
without features) must give the answers the vectors hold.

## 1. Why a new format

An identity here is one person with one master key that several apps may hold, and many device
keys that the master adds through an offer and a claim. KERI has the rules this needs (a current
key and a committed next key, so the current key alone cannot rotate), and did:webvh has the
encoding the web already reads (JCS, SHA-256 multihash in base58btc, multikeys, Data Integrity
proofs). Neither format carries both: KERI's encodings are its own, and did:webvh has no
device keys added by offer and no committed next key held by the words alone. `coasys/1` is
KERI's rules in did:webvh's encoding, with no witnesses, no consensus and no service endpoints.

## 2. Identifier and encodings

| Item | Encoding |
|---|---|
| Identity | `did:scid:coasys:1:<scid>` |
| SCID, event hash | base58btc of a SHA-256 multihash (`0x12 0x20` + 32 bytes), no multibase prefix (`Qm…`) |
| Signing key | multikey `z` + base58btc(`0xed 0x01` + 32-byte Ed25519 public key) (`z6Mk…`) |
| Encryption key | multikey `z` + base58btc(`0xec 0x01` + 32-byte X25519 public key) (`z6LS…`) |
| Passkey key (reserved) | multikey `0x80 0x24` + 33-byte P-256 key (`zDn…`); format 1 refuses it |
| Device key as an author | `did:key:<signing multikey>` |
| Key id | `m<n>` for master key *n*; `d<n>` for device keys, in order of addition, never reused |
| Verification method | `<did>#<keyId>` |
| Proof value | `z` + base58btc(64-byte Ed25519 signature) |
| Salt in a commitment | base58btc of 16 bytes, no multibase prefix |

Canonical form: RFC 8785 (JCS) for every object that is hashed or signed. A hashed or signed
object holds no floating-point number and no integer at or above 2^53.

## 3. Events

A log is a list of JSON objects, one per event, in `seq` order. Every event has exactly these
members, and no others:

| Member | Type | Meaning |
|---|---|---|
| `format` | string | `"coasys/1"` |
| `scid` | string | the SCID of the log |
| `seq` | integer | 0 for the first event, then +1 |
| `prev` | string or null | the hash of the event before; `null` at seq 0 |
| `op` | string | one of `incept`, `add`, `revoke`, `rotate`, `scope` |
| *op members* | | below |
| `proof` | array | Data Integrity proofs; left out only on an unsigned event, never empty |

| `op` | Members | Signed by |
|---|---|---|
| `incept` | `kind: "human"`, `master` (signing multikey), `next` (commitment), `add` (one or more device keys, no bot key) | master #0 and each listed device key |
| `add` | `add` (exactly one device key), `offer` (offer id), `coSig` (the claim co-signature of device linking) | the current master key |
| `revoke` | `revoke` (one or more device key ids) | the current master key |
| `rotate` | `master` (the revealed key), `salt`, `next` (commitment or `null`), `revoke` (device key ids; left out when empty) | the revealed key |
| `scope` | `key` (a bot key id), `can`, `expires` | the current master key |

A device key has one of three flat forms, and no other member:

```json
{ "id": "d1", "sign": "z6Mk…", "enc": "z6LS…" }
{ "id": "d2", "sign": "z6Mk…", "enc": "z6LS…", "expires": 1800000000 }
{ "id": "d3", "sign": "z6Mk…", "enc": "z6LS…", "kind": "bot", "can": ["read"], "expires": 1750000000 }
```

The first is valid until revoked; the second (`KeyLife::Until`) until revoked or until
`expires` (unix seconds); the third is a bot key with its actions (`read`, `write`, `join`) and
its expiry. A log holds nothing else: no names, device models, addresses, endpoints, emails
or free text. The private record, kept outside the log, holds names. `fold` does not read `offer` or
`coSig`; the store checks them against the claim (the claim check, listed in
`identity-store/AGENTS.md`).

## 4. Hashes, the SCID and proofs

- **Event hash**: `multihash_b58(sha256(JCS(event without "proof")))`. A receiver always computes
  it itself.
- **SCID** (did:webvh's method): build event #0 with `"scid": "{SCID}"` and no `proof`;
  `scid = multihash_b58(sha256(JCS(event)))`; put the value into `scid`. Event #0's hash then
  covers the final value. Every later event repeats it.
- **Proof** (`eddsa-jcs-2022`): `{ type: "DataIntegrityProof", cryptosuite: "eddsa-jcs-2022",
  verificationMethod: "<did>#<keyId>", proofPurpose: "assertionMethod", domain: "coasys:kel",
  proofValue }`. The key signs `sha256(JCS(proof without proofValue)) ‖ sha256(JCS(event without
  proof))` with Ed25519; the verifier uses strict verification. A proof has no other member.
- **Next-key commitment**: `next = multihash_b58(sha256(JCS({"key": master_n, "salt":
  base58btc(salt_n)})))`.

## 5. Replay rules

A verifier replays a log from #0 and refuses the whole log at the first event that breaks a
rule. Each rule names the error that the reference implementation returns
(`ChainError` from the hash-chain module, or `ChainError::Policy(FoldError)`).

**Parsing** (an event given as JSON):

- **P1** No floating-point number (`Format(Float)`) and no integer at or above 2^53
  (`Format(BigInteger)`) anywhere in the event.
- **P2** `format` is `"coasys/1"` (`Format(Version)`).
- **P3** `op` is one of the five (`Format(UnknownOp)`); the event holds only the members of section 3
  for its op (`Format(UnknownField)`); each member has its type (`Format(Missing)`,
  `Format(Type)`).
- **P4** A device key holds only `id`, `sign`, `enc`, `kind`, `can`, `expires`
  (`Format(UnknownField)`); `can` without `kind: "bot"` fails (`CanWithoutBotMark`); `kind` is
  `bot` or absent (`Format(UnsupportedKind)`); a bot key carries `can` and `expires`.
- **P5** `kind` of an `incept` is `human`; `group` is reserved (`Format(UnsupportedKind)`).
- **P6** The event serialises back to exactly the JSON it came from: an empty `proof` or an
  empty `revoke` on `rotate` is left out, never written as `[]` (`Format(NotCanonical)`).
  Without this, a receiver's hash could differ from the sender's.

**Order and links** (the hash-chain module, before any rule below):

- **L1** The log is not empty (`Empty`). Every event has a canonical form (`Malformed`).
- **L2** #0 has `seq: 0` and `prev: null` (`Seq`, `Malformed`). Each later event has `seq` = the
  previous + 1 (`Seq`), `prev` = the previous event's hash (`Prev`) and the log's `scid`
  (`WrongChain`).

**Event #0**:

- **K1** It is an `incept` (`Format(NotIncept)`), and its `scid` is the SCID of section 4
  (`Format(Scid)`).
- **K2** `master` is an Ed25519 signing key (`UnsupportedKeyType`); `next` is a SHA-256
  multihash (`Format(BadHash)`); `add` lists at least one key (`Format(EmptyAdd)`).
- **K3** No listed key carries the bot mark (`BotInIncept`).
- **K4** It carries one proof by `m0` and one by each listed key, and no other
  (`MissingProof`, `BadProof`).

**Every event**:

- **K5** Nothing follows a `rotate` with `next: null` (`Ended`).
- **K6** `format` is `coasys/1` (`Format(Version)`); an `incept` appears only at #0
  (`Format(InceptAfterGenesis)`).
- **K7** Key ids: device keys take `d1`, `d2`, … in order of addition across the whole log
  (`KeyIdOrder`); masters take `m0`, `m1`, … in order of rotation. No id returns.
- **K8** One key, one place: a signing multikey appears at most once in a log, as a master or a
  device key, revoked or not (`DuplicateKey`).
- **K9** Key types: every `sign` and `master` is an Ed25519 signing multikey, every `enc` an
  X25519 multikey (`UnsupportedKeyType`).
- **K10** Proofs: each required signer has exactly one proof naming `<did>#<keyId>`, with
  `proofPurpose: "assertionMethod"`, domain `coasys:kel` and a valid signature by its key
  (`MissingProof(id)` when absent, `BadProof(id)` otherwise). A proof by any other key fails
  (`BadProof`). A device key never signs its own identity's log after #0.

**Per op**:

- **K11** `add` lists exactly one key (`AddNotOne`) and is signed by the current master only.
- **K12** `revoke` names at least one key (`Format(EmptyRevoke)`), each a device key of this
  log (`UnknownKey`) that no earlier event ended (`RevokedKey`); signed by the current master.
- **K13** `rotate`: `master` and `salt` hash to the stored `next` (`NextMismatch`); the revealed
  key signs as `m<n+1>`; its `revoke` follows K12; `next` is a multihash or `null`.
- **K14** `scope` names a device key (`UnknownKey`) that no event ended (`RevokedKey`) and that
  carries the bot mark (`NotABot`); signed by the current master. An `Until` key is never
  extended: its app adds a new key.
- **K15** A bot scope (in `add` or `scope`) names at least one action (`EmptyScope`), each one of
  `read`, `write`, `join` (`Format(UnknownAction)`). `expires` is checked only as an integer
  below 2^53; the core reads no clock.

**Effects**:

- `incept`: master #0 and the listed keys take effect at seq 0; `next` is stored.
- `add`: the key takes effect at its seq; a bot key's scope is recorded at that seq.
- `revoke`: each key stops at its seq.
- `rotate`: master *n* stops and master *n*+1 starts at its seq; listed keys stop; `next` is
  replaced. With `next: null` the log ends: every key stops at that seq.
- `scope`: the bot key's scope is replaced from its seq.

## 6. Resolution and key validity

`fold` gives the state after the head: every master and device key with the seq that added it
and the seq that ended it, the bot scopes per seq, `next`, and `ended`.

- `key_valid_at(k, s)`: `k` (a key id or a signing multikey) was added at or before `s` and not
  revoked, replaced or ended at or before `s`. A key revoked at *N* stays valid below *N*.
- `key_valid_at_time(k, s, t)`: `key_valid_at(k, s)`, and for a key with `expires` (`Until`)
  `t < expires`, and for a bot key `t < expires` of the scope in force at `s`. `t` is the
  caller's trusted time.
- The current master is the last master, until the log ends.

## 7. Signing domains

Every signature names its domain and every verifier states the domain it expects. Log events
sign with the Data Integrity proof of section 4 under `coasys:kel`. Every other domain signs
`JCS(value + {"domain": domain})` with Ed25519 (the value is an object without a `domain`
member). The registry is `ad4m_identity_core::domain::SIGNING_DOMAINS`:
`coasys:kel`, `coasys:link`, `coasys:store-request`, `coasys:head`, `coasys:vouch`,
`coasys:offer`, `coasys:claim`, `coasys:pause`, `coasys:inbox`, `coasys:session`,
`coasys:account`, `coasys:room-key`, `coasys:bot-grant`. These labels enter a hash or a key
derivation and are never signing domains; every signing API refuses them: `coasys:pow`,
`coasys:pair-id:1`, `coasys:pair-key:1`, `coasys:match`, `coasys:wrap:1`, `coasys:auth:1`.

## 8. The words and master keys

- 12 BIP-39 English words (128 bits of entropy); the BIP-39 seed with an empty passphrase.
- Master key *n*: SLIP-0010 Ed25519 at the hardened path `m/7133'/0'/n'` (provisional until
  this spec is accepted).
- `salt_n = HKDF-SHA256(ikm = seed, salt = "coasys/1", info = "next-salt/" ‖ n)[0..16]`, with *n*
  in decimal ASCII (`next-salt/1`).
- #0 commits to master 1 with `salt_1`; the rotation to master *n* reveals master *n* and
  `salt_n` and commits to master *n*+1 with `salt_{n+1}`. The words alone reproduce every
  commitment; no device stores the next key or its salt.
- The private record's recovery key: `HKDF-SHA256(ikm = seed, salt = "coasys/1", info =
  "record-recovery")`, 32 bytes, as an X25519 secret, so the words alone recover the private
  record.
- Device keys and their X25519 keys come from random bytes, never from the words.

## 9. Proof of work for a log with no vouching note

A 16-byte `nonce` such that `sha256("coasys:pow" ‖ scid ‖ nonce)` has at least `bits` leading
zero bits, with `scid` as its UTF-8 text. On the wire: `{ "nonce": base58btc(16 bytes), "bits" }`;
the store checks against its own `bits` (or `previousBits` for 10 minutes after a change),
never the proof's. The proof binds to the SCID, which covers master #0, the commitment and every
listed key, so one proof admits one log.

## 10. Security and privacy

- A store can withhold events but cannot add one: every event carries proofs by keys the state
  before it names. A verifier re-checks everything a store serves.
- Forks (two valid events at one seq) are detected, never resolved, by the hash-chain module;
  a shorter peer log never truncates a held one.
- Only the words make master *n*+1, so whoever holds master *n* cannot rotate alone.
- `fold` refuses unknown fields so that no personal data enters a log by accident. `offer` and
  `coSig` are opaque to `fold`.
- The core reads no clock. Expiries are compared with the caller's trusted time; the store
  refuses an `expires` in the past or more than 400 days ahead.

## 11. Test vectors

| File | Holds |
|---|---|
| `log.json` | a valid log with every op and every key life: events, SCID, DID, hashes, the folded state, `key_valid_at` and `key_valid_at_time` for every key, seq and two times |
| `fold.json` | one invalid log per rule with the pinned error `{code, reason, format}` |
| `proofs.json` | every proof of `log.json` with its proof config, config hash, signing input and event hash; a tampered body, a wrong key, another domain |
| `commitment.json` | entropy, words, seed, masters 0–3 with path, key, salt and commitment, and the recovery public key |
| `pow.json` | proofs at 8, 12 and 16 bits for the SCID of `log.json`, with their hashes |
| `domains.json` | the registry and the labels; one domain-field signature per domain, which verifies only under its own domain; the refused names |

The vectors are regenerated by
`AD4M_IDENTITY_WRITE_VECTORS=1 cargo test -p ad4m-identity-core --all-features --test vectors_fresh`;
a changed hash in `log.json` or `proofs.json` is a format change.
