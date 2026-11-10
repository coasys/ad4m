# `did:scid:coasys:1` — format 1 of the Coasys key log

Status: draft for review.
Reference implementation: `ad4m-identity-core` on `ad4m-hash-chain`. Test vectors:
`ad4m-identity-core/vectors/*.json`. Every build of the core (native, `wasm32`, with and
without features) must give the answers the vectors hold.

The spec has three parts, each binding a different party:

- **Part A — Format** (sections 1–12) is normative for every verifier and resolver: the
  identifier, the events, the hashes and proofs, the replay rules, key validity, resolution of
  a DID URL and the DID document. A party that implements Part A can verify and resolve any
  `did:scid:coasys:1` identity without trusting the host it read the log from.
- **Part B — Host profile** (sections 13–14) is normative for hosts: the HTTPS route a host
  serves logs on. Which logs a host admits is host policy; a verifier never checks it.
- **Part C — Wallet profile** (sections 15–17) is informative: how a Coasys wallet derives
  master keys from the 12 words, the signing domains of the rest of the system, and the
  sealed messages and records between a person's devices. A verifier of a log needs none of
  it; it lets wallets and apps interoperate.

The DID method is did:scid (ToIP DID SCID Method Specification, Working Draft 03,
27 April 2025) with the format code `coasys` and version `1`. Documents and metadata follow
W3C DID Core v1.0.

# Part A — Format (normative for every verifier)

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

A device key has one of two flat forms, and no other member:

```json
{ "id": "d1", "sign": "z6Mk…", "enc": "z6LS…" }
{ "id": "d2", "sign": "z6Mk…", "enc": "z6LS…", "kind": "bot", "can": ["read"], "expires": 1750000000 }
```

The first is a standing key, valid until an event ends it. The second is a bot key with its
actions (`read`, `write`, `join`) and its expiry (unix seconds). Only a bot key expires: a
bot grant is meant to lapse, while a device key's validity over time comes from the log's
ordered events, never from a reader's clock. A log holds nothing else: no names, device
models, addresses, endpoints, emails or free text. The private record, kept outside the log,
holds names. `fold` does not read `offer` or
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
  (`Format(UnknownField)`); `can` without `kind: "bot"` fails (`CanWithoutBotMark`), and so
  does `expires` without it (`ExpiresWithoutBotMark`); `kind` is
  `bot` or absent (`Format(UnsupportedKind)`); a bot key carries `can` and `expires`.
- **P5** `kind` of an `incept` is `human`; `group` is reserved (`Format(UnsupportedKind)`).
- **P6** The event serialises back to exactly the JSON it came from: an empty `proof` or an
  empty `revoke` on `rotate` is left out, never written as `[]` (`Format(NotCanonical)`).
  Without this, a receiver's hash could differ from the sender's. The event's text is I-JSON
  (RFC 7493): no object in it names a member twice (`Format(DuplicateMember)`), checked on the
  text before it is parsed into a value, since a parser keeping the first of two members and one
  keeping the last would read two different events from the same bytes.
- **P7** The canonical JSON text of the event (section 4), proofs included, is at most
  65 536 bytes (`Format(TooLarge)`). `offer`, `coSig` and `can` have no length limit of their
  own (`salt` has: K13), so this limit stops one signed event from growing without bound.

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
- **K13** `rotate`: `salt` is base58btc of 16 bytes (`Format(BadSalt)`); `master` and `salt`
  hash to the stored `next` (`NextMismatch`); the revealed key signs as `m<n+1>`; its `revoke`
  follows K12; `next` is a multihash or `null`.
- **K14** `scope` names a device key (`UnknownKey`) that no event ended (`RevokedKey`) and that
  carries the bot mark (`NotABot`); signed by the current master.
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

## 6. Key validity

`fold` gives the state after the head: every master and device key with the seq that added it
and the seq that ended it, the bot scopes per seq, `next`, and `ended`.

- `key_valid_at(k, s)`: `k` (a key id or a signing multikey) was added at or before `s` and not
  revoked, replaced or ended at or before `s`. A key revoked at *N* stays valid below *N*.
- `key_valid_at_time(k, s, t)`: `key_valid_at(k, s)`, and for a bot key `t < expires` of the
  scope in force at `s`. `t` is the caller's trusted time. For every other key it equals
  `key_valid_at(k, s)`.
- The current master is the last master, until the log ends.

## 7. The `coasys:kel` domain

Every signature names its domain and every verifier states the domain it expects. Log events
sign with the Data Integrity proof of section 4 under `coasys:kel`. A verifier of a log needs
no other domain: a log proof under any other domain fails (K10), and a signature under
`coasys:kel` never verifies in another domain. The other domains of the system are listed in
Part C (section 16).

## 8. Resolution

A DID URL of this format is the DID followed by optional DID parameters and an optional
fragment: `did:scid:coasys:1:<scid>[?<params>][#<keyId>]`. The resolver reads two
parameters: `src` (where to look) and `versionId` (which version). The DID itself is the part
before `?` and `#`; it is what the document's `id` holds, whatever `src` the URL carried.

1. **Sources.** The resolver takes the sources in this order: the `src` of the DID URL, then
   each `src` the caller supplies, then the resolver's configured hosts. A parameter value is
   percent-decoded first.
   - A `src` without a scheme is an HTTPS base URL with its `https://` left out:
     `store.example` is `https://store.example`, and `store.example:8443/logs` is
     `https://store.example:8443/logs`. A `src` has a scheme when it starts with an RFC 3986
     scheme and a `:` that is not followed by a port number (digits, then `/` or the end).
   - `https:` is the HTTPS base URL as written.
   - Any other scheme names an adapter (another DID method, a peer-to-peer transport). The
     resolver uses only the adapters it has; a source with an unsupported scheme is skipped,
     as if it held no log, and the resolver tries the next one.
2. **Retrieval over HTTPS.** For an HTTPS base `B` (without a trailing `/`), the resolver
   requests `GET B/v1/logs/<scid>?from=0`, then pages with `from` = the seq after the last
   event it received, until a page holds no more events. The response body is a `LogPage`
   (section 13). `404` means the source holds no log. Any other status, or a body that is not a
   `LogPage`, means the source gave no valid log. A resolver bounds the pages and bytes it
   reads from one source.
3. **Verification.** The resolver folds the events with the rules of section 5, each event
   parsed as JSON under P1–P7. Event #0 must give the SCID of the DID. Any failure means the
   source gave no valid log: the resolver skips it and tries the next source. It may stop at
   the first source that gives a valid log or read further sources for a longer one. Of two
   valid logs where one is a prefix of the other, it keeps the longer. Two valid logs that
   differ at one seq are a fork: the resolver keeps the log it held first, whichever is longer,
   and reports the fork in the DID resolution metadata (`coasys:fork`: the seq and the two
   event hashes). Both must be valid: the competing event must replay on the prefix below it,
   proofs included. A different event there that does not is no fork (anyone can write one
   without a key); the source gave no valid log and is skipped.
   A source that answers less than the resolver holds (a head below its own, or no log) is no
   fork by itself: a host restored from a backup does so. Its head is compared with the held
   log at the head's own seq. The same hash there: the source is behind on the held branch; the
   resolver keeps its log, records nothing and tries the source again later. Another hash: a
   head names a hash, not an event, so the resolver reads the source's events up to that seq;
   one that differs from the held log and replays on the prefix below it is a fork (reported
   with that seq and the two event hashes). Without such an event the source is skipped as
   conflicting, and nothing is recorded.
4. **The DID document.** The resolver derives the DID document (section 9) from the state
   after the head. With `versionId=<seq>`, it derives it from the state after that seq.
   DID Core defines `versionId` only as an identifier of a version of the DID document and
   leaves its form to the method. In this method a version is a seq of the log: version *n*
   is the DID document of the state after event *n*, so every event makes one version, and
   versions are ordered as the log is. A
   `versionId` that is not a decimal integer is `invalidDidUrl`; a seq past the head of the
   log the resolver holds is `notFound`. A fragment `#<keyId>` dereferences the verification
   method with that id in the chosen document: `?versionId=4#d3` is `#d3` as of seq 4, which is
   what a signature anchored at seq 4 needs.
5. **Metadata.** DID document metadata holds `versionId`, the seq of the version used;
   `nextVersionId`, the seq after it when the log holds one; and `deactivated`, true when the
   log has ended (a `rotate` with `next: null`). `deactivated` describes the DID, not the
   version: an earlier version of an ended log still lists its keys, with `deactivated: true`.
   DID Core makes `versionId` and `nextVersionId` strings, so seq 4 is `"4"`. The log holds no
   time, so the resolver does not support `versionTime` and refuses it with `invalidDidUrl`;
   `created` and `updated` are absent for the same reason. A host's `appendedAt` is the host's
   claim, not part of the verifiable history.
6. **What a host can do.** A host can withhold events or serve an older head, signed or not (a
   signed head proves the host held it, not that it is the latest, section 13). It cannot forge
   an event: every event carries proofs by keys the state before it names, and event #0 hashes to
   the SCID in the DID. The resolver must not trust a host for anything except the bytes it
   returns, and checks every byte itself.

## 9. DID document

The DID document of a log at seq *s* is a pure function of the state after *s* and of the
`src` list the resolver's caller gives, which fills only `alsoKnownAs`. It reads no clock: a bot scope's expiry is a property of its verification method, never a filter, and a
verifier compares it with its own trusted time (`key_valid_at_time`, section 6). The
reference is `ad4m_identity_core::did_document`.

| Log item at *s* | DID document | Why |
|---|---|---|
| Standing device key `dN` valid at *s* (`key_valid_at`) | `verificationMethod` `#dN`: `Multikey`, `publicKeyMultibase` = its `sign`. Listed in `authentication` and `assertionMethod` | devices sign as the person: links, records, logins |
| Bot key valid at *s* | `verificationMethod` `#dN` with `"coasys:bot": {"can": […], "expires": …}`, the scope in force at *s*. Listed only in `capabilityInvocation`, never in `authentication` or `assertionMethod` | a bot acts within a scope, not as the person: a generic verifier must not take a bot's statement as the person's |
| The `enc` key of each standing key | `verificationMethod` `#dN-enc`: `Multikey`, `publicKeyMultibase` = its `enc`. Listed in `keyAgreement` | where messages to the person are sealed. A bot's `enc` key is absent: a bot has no inbox, and a message to the person must not reach one |
| A key revoked, replaced or ended at or before *s* | absent | a document describes one state; a document of an earlier version lists it |
| Masters `mN` | absent | masters control the log, not the subject's statements; the log never lets them sign for the person |
| Next-key commitment | absent | a log parameter, not a key |
| Ended log | no verification methods, every relationship empty; metadata `deactivated: true` | DID Core's deactivation |
| `id`, `controller`, each method's `controller` | the DID | self-controlled |
| `alsoKnownAs` | the DID URLs with `src` the caller gives (other hosts of this DID) | where else to look |

A content signature by the person (a link, a record, a login) is valid only under a device key.
A master key signs log events and offers only, so a verifier that finds the signer of content
must not accept a master key (`KeyState::device_key_id_of`, not `key_id_of`).

Methods are listed in key order (`d1`, `d2`, …, by number), each key's signing method before
its `-enc` method. Method ids are relative DID URLs (`#d1`), resolved against `id`. The
JSON-LD form has `@context` first, then `id`, `controller`, `verificationMethod`,
`authentication`, `assertionMethod`, `keyAgreement`, `capabilityInvocation` (each written even
when empty) and `alsoKnownAs` (left out when empty). A method has `id`, `type`, `controller`,
`publicKeyMultibase`, then `coasys:bot` on a bot key.

Example: the log of `vectors/log.json` at `versionId=3` (`vectors/document.json`):

```json
{
  "@context": [
    "https://www.w3.org/ns/did/v1",
    "https://w3id.org/security/multikey/v1",
    "https://id.coasys.org/ns/did-scid-coasys/v1"
  ],
  "id": "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c",
  "controller": "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c",
  "verificationMethod": [
    {
      "id": "#d1",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c",
      "publicKeyMultibase": "z6MkoLDHstjfesQKtngXMequRUhP96dNkWrGDghVxxCoksYQ"
    },
    {
      "id": "#d1-enc",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c",
      "publicKeyMultibase": "z6LSoKsNQ3ZWym1Qsf7fL6ajAHqjpzaudw4hEvtzA8ejGDgz"
    },
    {
      "id": "#d3",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c",
      "publicKeyMultibase": "z6Mkmu4QQQajcBEcm7r2iumaUyNZ45t5tUuojTUUSzdX3kYr",
      "coasys:bot": { "can": ["read"], "expires": 1900000000 }
    }
  ],
  "authentication": ["#d1"],
  "assertionMethod": ["#d1"],
  "keyAgreement": ["#d1-enc"],
  "capabilityInvocation": ["#d3"],
  "alsoKnownAs": [
    "did:scid:coasys:1:QmRr6vq8rZSbrmxT9tpkFVuB3j4zt4vZTuJWMRbRNCPi2c?src=store.example"
  ]
}
```

Its metadata: `{"versionId": "3", "nextVersionId": "4", "deactivated": true}` (the log of
the vectors ends at seq 6).

The context `https://id.coasys.org/ns/did-scid-coasys/v1` defines the extension property. The
identity site serves the file `docs-src/identity/did-scid-coasys-v1.jsonld` of the AD4M
repository, unchanged, at that URL (media type `application/ld+json`):

```json
{
  "@context": {
    "@version": 1.1,
    "@protected": true,
    "coasys": "https://id.coasys.org/ns/did-scid-coasys/v1#",
    "coasys:bot": {
      "@id": "coasys:bot",
      "@context": {
        "@protected": true,
        "can": { "@id": "coasys:can", "@container": "@set" },
        "expires": {
          "@id": "coasys:expires",
          "@type": "http://www.w3.org/2001/XMLSchema#integer"
        }
      }
    }
  }
}
```

`coasys:bot` is a bot key's scope: `can` (actions from `read`, `write`, `join`) and `expires`
(unix seconds, the IRI `coasys:expires`). No other method carries an expiry.

## 10. Where `src` goes

Signed or long-lived data carries the bare DID, never `src`: log events, proofs, link anchors,
agent records, expressions. A `src` there would disclose one host choice in every copy of the
data and freeze it in signed bytes, so a later move of the log would leave stale hints that
nobody can correct. A DID URL with `src` is for telling a reader where to look: a shared link,
a QR code, a profile card, the `alsoKnownAs` of the DID document. A resolver that gets a bare
DID uses its caller's sources and its configured hosts.

### Anchors

A content anchor is one integer that a signed piece of content (an agent record, a link)
carries with its signature: the seq of the signer's identity log that the signer knew when it
signed. It places the signature at a position in the log, so revocation is checked without
clocks.

- A signature by a device key `k` of identity X, anchored at seq `s`, holds only when X's log
  lists `k` and `key_valid_at(k, s)` (section 6). A key revoked at *N* signs nothing valid
  anchored at or after *N*; content anchored below *N* stays valid.
- An anchor past the head of the log the verifier holds is refused.
- The signer picks the anchor, so the anchor alone cannot stop a holder of a revoked key from
  backdating new content to a seq below *N*. A host that admits signed content can order
  arrivals, as host policy (section 14): when the content arrives it also checks `k` listed
  and valid at the head of the log it holds, and refuses the content otherwise, whatever
  anchor it names. The Coasys identity store does so for agent records. A revoked key so
  cannot backdate new content past such a host; readers check at the anchor and may rely on
  its admission for arrival order. Admission never changes whether a signature holds.
- A reader that first sees a signature after the key's revocation, anchored before it, and
  did not get it through such a host, may flag it. That is the reader's policy; the signature
  holds.

The anchor carries no DID, key id or hash. The identity is known from the content: a record is
keyed by its did:scid, and a link's author key maps to its identity through a key lookup that
the verifier confirms against the log (a key enters a log only with its own co-signature). The
key id is read from the log, which lists the signing key. A hash would only tell forked
branches apart, and a fork is only detected, never resolved (sections 8 and 11): the store
keeps the branch it held, and a fork by the current master freezes the identity there until a
`rotate` (section 14), so a hash would add bytes to every record and link and enable no check.

The anchor is not a parameter of the signer's `did:key`. A `did:key` has one fixed DID
document, so a version of it means nothing, and the `did:key` method defines no DID parameters.
The anchor is a version of the identity's log. As a DID URL it is
`did:scid:coasys:1:<scid>?versionId=<s>#<keyId>`: the signing key's verification method in the
DID document at version *s* (section 8). That is the DID URL a W3C Data Integrity proof names
in `verificationMethod`, so a system that verifies signatures by DID URL gets the same check.
Content carries only the integer, inside the signed bytes. A verifier or an exporter derives the
DID URL from the identity, the anchor and the key id that the log lists for the signing key.

## 11. Security and privacy

- A store can withhold events but cannot add one: every event carries proofs by keys the state
  before it names. A verifier re-checks everything a store serves.
- Forks (two valid events at one seq) are detected, never resolved, by the hash-chain module;
  a shorter peer log never truncates a held one, and is no fork unless it holds a valid
  different event at a seq both hold.
- Only the words make master *n*+1, so whoever holds master *n* cannot rotate alone.
- `fold` refuses unknown fields so that no personal data enters a log by accident. `offer` and
  `coSig` are opaque to `fold`.
- Revocation burns a key for good. Rule K8 refuses a key that the log ever held, revoked or
  not, so a revoked key can never return to this identity. A host that requires one key per
  identity (section 14) also refuses it in every other log, so it can never speak for another
  identity there either.
- The match code (6 base32 characters, 30 bits) is for a person comparing two screens, not an
  authentication against grinding: someone who could claim an offer could try device keys
  offline until one shows the code the person expects. What keeps a stranger's key out of a
  link is the offer secret, which only the offer string or the sealed ticket carries, the
  single claim an offer takes, and, for a scanned device string, the offer bound to that
  device's key; the match code catches a claim by another device than the one in front of
  the person.
- The core reads no clock. Expiries are compared with the caller's trusted time. The window a
  host accepts for a new `expires` is host policy (section 14).
- A generic DID Core verifier ignores `coasys:bot`. Bot keys sit outside `authentication` and
  `assertionMethod`, so such a verifier never takes a bot's signature as the person's, and
  every key it does use is valid without a clock.
- A `src` fetched from untrusted data is a request to an address someone else chose. A
  resolver on a server limits it (HTTPS only, public addresses only, no redirect to another
  host).
- A key lookup (section 13, "Key lookup") is a hint, never proof. A host that answers "this
  signing key belongs to `did:scid:coasys:1:<scid>`" may be wrong or hostile. A resolver that
  uses the answer resolves that DID's log and accepts the answer only when the log lists the
  key.
- A log listing a key does not prove that the key's holder agreed. An `incept` carries a
  signature by every key it lists (rule K4), so replay proves their consent. An `add` is
  signed by the master key only: its `coSig` covers the claim, which holds the offer secret and
  a private hint that never enter the log, so replay cannot check it (`fold` does not read it,
  section 3). Only the host that admitted the `add` checked the claim (section 14). So a
  controller can write a log that replays correctly and lists another person's device key. A
  resolver therefore takes a key lookup only from a host whose admission it trusts (one that
  checks each claim and holds every key in one log), and confirms the answer against the log.
  For the same reason, a verifier that maps a signing key to a person reads that person's log
  from such a host: a log fetched from any other `src` can show the person's keys, but does not
  prove that a key it lists speaks for that person.

## 12. Test vectors

| File | Holds |
|---|---|
| `log.json` | a valid log with every op and every key life: events, SCID, DID, hashes, the folded state, `key_valid_at` and `key_valid_at_time` for every key, seq and two times |
| `fold.json` | one invalid log per rule with the pinned error `{code, reason, format}`; in `textCases` the events are JSON text (a duplicate member name, P6) |
| `proofs.json` | every proof of `log.json` with its proof config, config hash, signing input and event hash; a tampered body, a wrong key, another domain |
| `commitment.json` | entropy, words, seed, masters 0–3 with path, key, salt and commitment, and the recovery public key |
| `pow.json` | proofs at 8, 12 and 16 bits for the SCID of `log.json`, with their hashes |
| `domains.json` | the registry and the labels; one domain-field signature per domain, which verifies only under its own domain; the refused names |
| `link.json` | device linking: the offer commit, the claim and offer signing bytes, the match code, the offer and device strings, and the `add` events that spend a claim |
| `document.json` | the log of `log.json` with a fixed `alsoKnownAs`, the DID document and metadata at the head and after every seq, and a version past the head refused (`VersionPastHead`) |
| `devices.json` | section 17: a pre-join address and its hash input, an inbox payload and its signing bytes, an HPKE seal with fixed keys and RNG stream, a pre-join read, a private record sealed to two device keys and a recovery key (not to the log's bot key) with its AAD, its signing bytes, the recovery key's attestation bytes and a store receipt with its envelope hash input and signing bytes, and a request body |

The vectors are regenerated by
`AD4M_IDENTITY_WRITE_VECTORS=1 cargo test -p ad4m-identity-core --all-features --test vectors_fresh`;
a changed hash in `log.json` or `proofs.json` is a format change.

# Part B — Host profile (normative for hosts)

A host stores logs and answers resolution requests. A host that serves logs over HTTPS follows
section 13. A host may also answer key lookups (section 13, "Key lookup"). Everything else it
offers (appending, signed heads, feeds) is outside this profile.

## 13. Retrieval over HTTPS

```
GET /v1/logs/<scid>?from=<n>
  200 application/json   LogPage
  404                    the host holds no log for <scid>
  Access-Control-Allow-Origin: *
  Cache-Control: public, max-age=31536000, immutable   a full page that ends below the head
  Cache-Control: public, max-age=10                    the page that holds the head
```

- `LogPage` is a JSON object with at least these members:
  - `events`: at most 256 events from seq `n`, in seq order, each exactly as appended. A `from`
    past the head gives an empty `events`.
  - `head`: the host's head `{scid, seq, hash, …}`. A host may sign it (the Coasys identity
    store signs it under `coasys:head` with `time`, `store` and `sig`); a resolver that does
    not trust the host's key reads it only as a hint of how far to page. A signed head proves
    authenticity, not freshness: its `time` is when the host stored that head, not when it was
    read, so a host (or anyone replaying its answer) can serve an older signed head, and with
    the prefix of the log it covers it verifies as well as the current one. A reader that
    trusts the key learns that the host held at least that log, never that the log ends there;
    it reads over an authenticated channel (HTTPS) and never takes a head below one it holds. A
    signed head below the reader's, over the event the reader holds at that seq, shows the host
    behind (restored from a backup, or not caught up), not a fork (section 8, step 3).
  - `appendedAt`: one unix time per event, when the host stored it. It is the host's claim
    and no part of the verifiable history.

  A host may add members; a resolver ignores members it does not know.
- The route allows cross-origin reads, so a browser resolves with no server of its own.
- A full page (256 events) that ends below the head never changes, because a host never
  replaces a stored event: it may be cached as immutable. The page that holds the head changes
  with every append and is cached briefly.

### Key lookup (optional)

A device signs as its `did:key`. A reader that holds only that key finds the person through a
host's key index:

```
GET /v1/keys/<signing multikey>?wait=<0..8>
  200 application/json   { did, keyId, … }   the DID whose log lists the key, and its id there
  404                    the host knows no log that lists the key
```

- The answer is a hint (section 11). A resolver asks only a host whose admission it trusts,
  and confirms the answer against the named DID's log.
- A host that requires one key per identity (section 14) gives one answer per key. A host that
  does not may know several logs for one key, and then answers none (`404`), so a lookup never
  picks between identities.
- `wait` holds the request open up to that many seconds for a key that is not listed yet, so a
  device that just claimed an offer learns its identity as soon as the `add` lands.

## 14. Admission (host policy)

Which logs and events a host accepts is host policy. A verifier never checks it, and a host may
choose otherwise without breaking verification. A host re-runs `fold` on every append. The
Coasys identity store adds these checks:

- **One key per identity.** A host may require that a signing key appears in at most one log
  it holds (rule K8 already holds within one log). Revoked keys count: a log that ever listed
  the key keeps it, so a revoked key can never enter another log on that host.
- **Signers valid at the head.** The store takes an agent record only when its signing key is
  valid at the head of the log it holds when the record arrives (section 10, anchors).
- **Expiry window.** The store refuses a bot scope's `expires` in the past or more than 400
  days ahead.
- **Forks by the current master freeze.** An append whose event sits at a seq the store holds,
  differs from the stored event and replays on the events before it is a fork (section 11).
  The store keeps a fork as evidence with the competing event, one per signing master (a
  further competing event by a master that already has a recorded fork is answered with that
  fork), answers it with the fork, and serves the log it held. The identity is frozen only
  while the competing event's signing master is the current master at the head of the held
  log; a competing `rotate` is signed by the master it names, so it freezes once the held log
  rotates onto that master, until a further `rotate`. A fork signed by a master the held
  log has rotated away from (at the fork's seq or later) freezes nothing: rotation is the remedy
  for a stolen master, and a retired master must not be able to lock the identity out. While
  frozen the store takes no signed request and one kind of append only: a `rotate`, which the
  committed next key signs (a holder of the current master alone cannot). Once it is appended
  the competing event's signer is no longer current, so the freeze ends by the same rule. The
  store derives the freeze from the recorded fork and the held log; it stores no flag. Against
  someone who holds the 12 words (the recovery secret, and so the committed next key) a freeze
  lasts only until they append that `rotate` themselves; this is inherent to pre-rotation, where
  whoever holds the next key is the owner as far as the log can tell.
- **Length cap.** The store holds at most 16 384 events per log (seqs 0 to 16 383), the most
  the executor's resolver reads (64 pages of 256 events, section 13), and refuses an append
  whose last event would lie past it (`400 log-full`). A log that reaches the cap can take no
  further event, a `rotate` included.
- **Per-log budget.** Events differ a hundredfold in the state they add (a `scope` may name its
  actions any number of times within the event size limit), so the length cap does not bound
  what a log costs a host to hold or replay. The budget (default 16 MiB, configurable; a log
  of 16 384 rotations takes under 7 MB) has two bounds on the replayed state of the log in
  memory: its live part, the state without the records of keys revoked at the head (device
  keys revoked, masters replaced by a `rotate`), may take at most the budget, and the whole
  state, revoked keys included, at most twice it. The store refuses an append after which
  either bound would be passed, and stores none of the append's events (`400 log-too-large`).
  A log that reaches the budget may still take an event small enough to fit. Revoking keys
  frees the live part: someone holding a stolen master can pad the live part to the budget,
  and once the owner rotates and revokes the keys that padded it, the owner appends again, up
  to the whole bound. A log stolen and padded twice can reach the whole bound and then takes
  no event but a `rotate`. A `rotate` is never refused by the budget: an append whose events are
  all `rotate`s is not measured. Rotation is the recovery from a stolen master, and someone
  holding the current master could otherwise pad the log up to the budget and lock the
  owner's `rotate` out. A `rotate` adds one master key to the state, and the length cap
  bounds how many there are. An append with any other event is measured, a `rotate` in it
  included, so a wallet sends a recovery `rotate` on its own.
- **Replay of stored events.** A host checks every event in full (`fold`, every proof) before
  it stores it. When it replays a log from its own storage, it may leave out the checks that
  every stored event already passed and that only refuse: the proofs' signatures and the
  duplicate-key check (rule K8). The state is the same. This trusts the host's own storage
  only; events from anyone else are always checked in full.
- **Signed before read.** Before it replays a stored log for an append, the store checks the
  first event's proof against the key that signs at its seq, from its key index at the head
  (the master current there; for a `rotate`, the key the stored next-key commitment names).
  Someone without a master key of the log cannot make the store replay it.
- **Proof of work for a log with no vouching note**, below.

### Proof of work for a log with no vouching note

A 16-byte `nonce` such that `sha256("coasys:pow" ‖ scid ‖ nonce)` has at least `bits` leading
zero bits, with `scid` as its UTF-8 text. On the wire: `{ "nonce": base58btc(16 bytes), "bits" }`;
the store checks against its own `bits` (or `previousBits` for 10 minutes after a change),
never the proof's. The proof binds to the SCID, which covers master #0, the commitment and every
listed key, so one proof admits one log.

# Part C — Wallet profile (informative)

A verifier needs none of this part. It fixes how Coasys wallets make keys and what the rest
of the system signs, so that wallets and apps interoperate.

## 15. The words and master keys

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

## 16. Other signing domains

Every domain other than `coasys:kel` (section 7) signs `JCS(value + {"domain": domain})` with
Ed25519 (the value is an object without a `domain` member). The registry is
`ad4m_identity_core::domain::SIGNING_DOMAINS`:
`coasys:kel`, `coasys:link`, `coasys:agent-record`, `coasys:store-request`, `coasys:head`,
`coasys:vouch`, `coasys:offer`, `coasys:claim`, `coasys:pause`, `coasys:inbox`,
`coasys:private-record`, `coasys:recovery-key`, `coasys:record-receipt`, `coasys:session`, `coasys:account`, `coasys:room-key`, `coasys:bot-grant`. These labels enter a hash or a key
derivation and are never signing domains; every signing API refuses them: `coasys:pow`,
`coasys:pair-id:1`, `coasys:pair-key:1`, `coasys:match`, `coasys:wrap:1`, `coasys:auth:1`,
`coasys:record`. Two signing domains also serve as a hash or `info` prefix: `coasys:offer`
(the offer commit) and `coasys:inbox` (the inbox seal, section 17).

## 17. The device inbox, the private record and the request box

Messages between a person's own devices, kept by a host beside the log and never in it. A host
keeps only ciphertext and the outer fields named here; every receiver checks the inner
signature against its own copy of the log.

**Seals.** One construction for every sealed value: HPKE (RFC 9180) base mode, single shot,
DHKEM(X25519, HKDF-SHA256), HKDF-SHA256, ChaCha20-Poly1305, empty AAD. A sealed text is
`base64url(enc ‖ ciphertext)` without padding (32-byte encapsulated key, then the ciphertext
with its 16-byte tag). The recipient is an X25519 key: a device key's `enc` in the log (for an
executor's key, the Montgomery form of its Ed25519 key), or the recovery key (section 15).

**Inbox addresses.** Before a device joins, its device string gives the pre-join address
`base58btc(sha256(signing_bytes("coasys:inbox", {"sign": sign, "r": r})))` (raw digest), written
`pre:<address>`; after, the device's key id: `<scid>/<keyId>`. A key with the bot mark has no
inbox.

**Inbox payloads.** The plaintext of a message is `JCS(InboxPayload)`:

```
InboxPayload { to, body, sentAt, by: {keyId, kelSeq, kelHash}, sig }
body = { type: "offer-ticket", store, scid, offerId, s, signedOffer }
     | { type: "master", did, index, secret }          // secret: base58btc of the 32-byte Ed25519 secret
sig  = a master key over signing_bytes("coasys:inbox", {to, body, sentAt, by})
```

It is sealed to the receiving key with `info = "coasys:inbox" ‖ 0x00 ‖ address` (the address
text above), so a message sealed for one inbox does not open at another. The receiver accepts
it only when `to` is its own signing key; `by.keyId` names a master key of the identity that is
valid at `by.kelSeq`, a position its log holds with hash `by.kelHash`; and the signature
verifies. An offer ticket's `signedOffer` must verify against the log at the head, name the
ticket's offer, be signed by the same master key, be bound to `to`, and commit to `s`. A
`master` body's `secret` must be the key `masters[index]` of the log of `did`; when `index` is
not the current master the message is stale: the device acknowledges it and drops it. A payload
has no single-use mark, so a host can deliver an exact copy again and, while its master is
current, the copy passes these checks: a device acts on a message idempotently.

**Pre-join reads.** A device whose key is in no log yet reads its pre-join inbox with
`PreInboxAuth { address, sign, r, store, method, path, nonce, expires, sig }`, `sig` by `sign`
over `signing_bytes("coasys:inbox", every field but sig)`; `address` must be the pre-join address
of `sign` and `r`. `store` is the `did:key` of the host's store key, as in a signed store
request: a host refuses a read signed for another store (`403 store`), so a read cannot be
replayed to a second host. A host checks the store, the method, the path (without the query),
`expires` (at most 5 minutes ahead) and that the nonce is new.

**The private details record.** `SealedRecord { version, wrapped, recoveryKey?,
recoveryAttestation?, nonce, ciphertext, signer, sig }`. The record (`PrivateRecord { devices, wordsCheckedAt, pendingRevocations,
safeguards }`, no other field: person settings stay per device) is encrypted with
XChaCha20-Poly1305 under a fresh 32-byte record key for every version, nonce 24 bytes
(base58btc), `aad = JCS({"scid": scid, "version": version, "recoveryKey": recoveryKey or
null})`, ciphertext in base64url. The writer signs the whole envelope: `signer` is its key id,
a device key without the bot mark valid at the head or the current master, and `sig` (base58btc
multibase) is that key over `signing_bytes("coasys:private-record", {scid, version, wrapped,
recoveryKey, recoveryAttestation, nonce, ciphertext, signer})`, an absent `recoveryKey` or
`recoveryAttestation` as `null`. A host checks the signature against the log at its head before
it keeps a write: anyone can seal to the log's public keys, so an unsigned or wrongly signed
envelope, or one by a key revoked, replaced or with the bot mark, is refused.

When a host keeps a write it signs a receipt with its store key, the key that signs its heads:
`RecordReceipt { scid, version, envelope, seq, store, sig }`, where `envelope` is the hash
(section 4) of `JCS({scid, version, wrapped, recoveryKey, recoveryAttestation, nonce,
ciphertext, signer, sig})` (the signed envelope with its signature), `seq` is the log's head
the host checked the write against, `store` is the `did:key` of the store key, and `sig`
(base58btc multibase) is that key over `signing_bytes("coasys:record-receipt", {scid, version,
envelope, seq, store})`. The host keeps the receipt beside the record, answers a write with
`{version, receipt}` and a read with `{record, receipt}`. A reader checks, before it uses any
field, that the receipt names this identity, the record's version and this envelope, that a
store key it trusts for heads signed it, and that its log has reached `seq` (else it reads
the log first); then that the envelope's `signer` was a device key without the bot mark or the
master key valid at `seq`, and signed it. So a record a device wrote before its revocation, or
the old master before a rotation, still opens for every reader the record is wrapped to; a key
revoked at or before `seq` does not count. A new write is always checked at the head, so a
revoked key cannot write again by naming an old seq: the host names the seq. The receipt is a
statement by the store about when it admitted a record that is not in the log, so it extends
the trust a reader already gives the store for heads: a dishonest trusted store could sign an
older `seq` for a fresh envelope signed by a key revoked since. It carries a `seq` and no head
hash, so a reader relies on its own fork policy for the log at `seq`, and it does not replace
the reader's high-water version for the record.

The recovery key needs the master's authority, since a device key can sign an envelope:
`recoveryAttestation = {keyId, sig}` is present exactly when `recoveryKey` is, and `sig` is master
key `keyId` over `signing_bytes("coasys:recovery-key", {scid, recoveryKey, keyId})`. A recovery
key counts only while `keyId` is the log's current master and the signature verifies. A host
refuses a write whose attestation does not (`400 attestation`). A device-signed write copies the
stored record's counting recovery key and attestation unchanged, or carries none when it has
none (`400 recovery-key`). Only a master-signed write installs, replaces or drops it, so a
device cannot put back a key the master replaced. After a rotation the old master's attestation
no longer counts. A host refuses a write that carries it, and a device writer refuses to
write rather than seal the record without the recovery key, until the new master attests it
again. A reader still opens a record written before the
rotation (its receipt names a seq where the writer was valid), but does not trust its recovery
key. A reader refuses outright a record whose attestation no master of the log signed. So a
rotation is followed promptly by a master-signed write that attests the recovery key with the
new master and rewraps the record to the log's readers at the new head; until then the words
still open the stored record, and a device write waits on the new master's attestation. A
revocation published while the pair is stale stands all the same; only the record rewrite
after it is refused until the re-attestation. The re-attesting master-signed write carries the
recovery pair only: it marks no device as holding the master key, since custody is asserted
only by the operation that keeps the key, never by an earlier unlock — a vault unlock's signer
outlives the kept slot it was taken from, so a mark written from it could re-advertise a copy
a removal on the shared vault already took. The record
key is sealed (above) with `info = "coasys:record" ‖ 0x00 ‖ scid` to the `enc` key of every
device key valid at the head without the bot mark (`wrapped[keyId]`), and to `recoveryKey` when
set (`wrapped["recovery"]`). A writer reads the `enc` keys from the log, so a removed or revoked
device cannot read a later version. `version` rises by one with each write (a host refuses any
other than the stored version plus one; with no record stored the version is 0, so the first write
is version 1); a reader refuses a version lower than one it has read. A host can serve back an
older version with its valid receipt, so that high-water mark is the reader's own state: the
client keeps the highest version it has read per identity; the core functions keep none. A device entry's `holdsMaster` is `false`, `"offered"` or
`true`; with `true` it carries `masterIndex`, the index *n* of the master key the device's
vault keeps, written by the device (or the flow that put the key there), and `vault`, the id of
the physical vault that keeps the key. A vault id is 16 random bytes in base58btc. The vault
draws it the first time an identity asks and keeps it, one per identity, for every device key,
app and process that uses that vault. A forget of the master key keeps the id; it goes only
with the vault. Two apps on one shared vault are two device keys and one copy: their entries
name the same vault id. An entry with `false` or `"offered"` carries no `vault`. When a device
marks index *n* on vault V, every other `true` entry that names V with a lower index or no
index moves to *n*, because V keeps one master key. The vault keeps a
custody note per identity and device key. The note is a stack of layers: each operation puts
one layer on top and keeps the note below it (`restores`). Each layer carries a random 16-byte
token. An operation changes or removes only its own layer, found by its token, with a
compare-and-swap on the whole note. It never puts back or removes a layer that another
operation wrote, even one with the same index and epoch. There are three kinds of layer:

- `MarkOwed {index, epoch, token, acquired: false}` is an intent. The device reads the vault's
  custody epoch, writes the intent, and only then stores or promotes the key. An intent proves
  nothing: the store can fail, or another app on the same vault can install the key.
- `MarkOwed {…, acquired: true}` is a receipt. The operation writes it only when its own store
  or promote answered that it installed the key. Then it acknowledges the inbox and writes
  the mark. It clears the receipt, and the layers below it, once a mark has met it.
- `Released {epoch, token}` is written before a removal forgets the key and publishes `false`.
  The removal reads the custody epoch again after the release. When the epoch moved, a store
  came between: it removes the release and starts again. It writes `false` only while its
  release is still the top layer. A removal that fails removes only its own release. A
  removal that finished retires the releases below its own: every operation that can remove
  its release either removes them too or puts a receipt above them that supersedes them, so
  no reader sees them again. Thus a repeated removal keeps one release, not one per removal.
  It keeps the intents and receipts below it.

The standing of the note comes from the top layer down: open intents are passed over, and the
first release or receipt decides. A receipt defeats a release below it, and a release above it
whose epoch is not later than the receipt's intent: the store moved the custody epoch past such
a release, so its forget cannot take the new key. A device writes `holdsMaster: true` only
from a receipt for the index its vault keeps. It writes `false` when the note stands released
or the vault keeps no key. Otherwise it leaves its holding as the record says: a key in a
shared vault without a receipt of this device can be another device's. A device repairs its
own entry only from its note: when the note is a `MarkOwed`, a later check of its custody
first takes a master key still waiting in its inbox; else, when the note stands on a receipt
for the index the vault keeps, it writes the mark (re-attesting a stale recovery pair with the
master first). A note with open intents only (a crash before the receipt) it clears, and it
writes nothing. When the note stands released it never writes `true`; with no note it repairs
nothing, apart from a device the record already lists as holding the key on this vault (its
entry names this vault's id), which re-attests a stale pair and keeps its mark. A device the record does not list, or one whose note stands
released, is never asked to unlock its master for a repair. Every write of the device's
`holdsMaster`, a rotation's hand-over included, reads the custody epoch, then the note, and
re-reads the note inside the record's version check. The record's versions order the writes: a
removal that wrote `Released` before that check makes the write fail and retry with `false`,
and one that wrote it after publishes its own `false` on a later version; a `false` never
turns back to `true` without a new receipt. An entry without
`masterIndex` (written before it existed) is read as an unknown index. A `true` entry without
`vault` (written before it existed) names no vault and proves no copy; the device's next mark
writes it. A pending revocation's `reason` is `left-host` or `account-deleted`.

**The request box.** A device key without the bot mark posts `{body, expires}` with `body`
`{kind: "send-master"}` or `{kind: "confirm-revoke", keyIds, rotate}`; `expires` lies at most 7
days ahead. The host keeps the post with the signed store request it came with, whose
`bodyHash` is the hash of `JCS({body, expires})`, so a reader checks the poster itself. A
request changes no key and no log event by itself.

**An executor as a device.** An AD4M executor is a device whose key is its wallet key. It
reads its joined inbox when it starts, when its wallet unlocks, when its key joins an
identity, and every 15 minutes, one read at a time. It reads a pre-join inbox only while a
device string it showed is valid (until `exp`, five minutes), at most once a second; showing
a new string ends the read of the last one.

- A master key is the only body a joined inbox carries. An executor started without the
  custody channel (a server, a host) keeps no master key: it deletes each message of its
  joined inbox unopened and logs an error. Its claims never say `canKeepMaster`.
- An executor with the custody channel (the desktop app's) opens each new message to learn
  its type, wipes the plaintext, leaves the message at the host and tells the operator on its
  machine (`inbox-message {id, kind}`, never a body). Its custody service then calls
  `identity.takeMaster`: the executor opens the messages again, oldest first, checks each
  against its refreshed copy of the log as above, and answers the first current master key
  (`{messageId, did, index, secret}`) or `null`. A stale key, or one that fails a check, is
  deleted. The message stays until the custody service acknowledges it
  (`identity.inboxAck`, which on such an executor only the custody credential may call) after
  storing the key. Only the custody credential (which only the
  custody service holds) on a loopback listener of an executor started with
  `--custody-channel` gets this answer. The custody service hands the executor that
  credential as the first line of its stdin at start, and the executor keeps it in memory
  only: never in its environment or on its command line, which other processes of the user
  can read (`/proc/<pid>/environ`), as can the admin credential, which reads host files. An
  executor with `AD4M_CUSTODY_CREDENTIAL` or `AD4M_CUSTODY_CREDENTIAL_FILE` set refuses to start. The admin credential, an app,
  a network listener, a device-key login and an operator without a credential are refused (403
  `no-custody-channel`). The key reaches the answer only: no log line, event, store or
  language, and the executor wipes its copy.
- A master key in a pre-join inbox is dropped: a master key never travels before the device
  joined. A valid offer ticket is claimed under the executor's first-link rule (a record
  naming another identity refuses it; so does no record, except on a desktop's first run: an
  executor with the custody channel, no record and no identity may claim any offer, and records
  an identity as operator, `FirstRun`, only when its main agent joins it: the holder confirmed
  the match code and the add listing the device key is in the log; until then it may claim
  again, for the same or another identity), with `canKeepMaster` exactly when the
  executor has the custody channel; only a claim made shows the identity and the match code
  (`link-ticket`). The ticket is deleted either way; one whose log cannot be read now waits
  for the next read.
- The executor never decrypts the private record. It passes the record and request-box calls
  with a store request the caller signed with the master key, or signs one with its own key
  (always for a post). For the app on its machine whose device it is (the desktop's custody
  service, which has no `enc` key of its own), the operator there may have it open the record
  key of its own entry of `wrapped` (`identity.unwrapRecordKey`, local only); the app
  decrypts the record with that key after it verified the envelope's signature and the
  recovery key's attestation against the log. That app's writes are its device's: it seals
  the envelope naming the device key as `signer` with an empty `sig`, and the executor signs
  it with the device key before the put (it refuses an unsigned envelope naming any other
  key). A device write copies the recovery key and its attestation that still count, unchanged,
  and while the stored attestation is stale after a rotation it is refused rather than seal
  the record without it.

**The holder's side.** The 12 words are the root authority of the identity: they derive
every master key, the salts that open each rotation's commitment, and the recovery secret
that opens the private record, so their holder rotates, revokes and enrols a device without
any device of the log — and a rotation revokes no one's copy of them. An app that holds the
current master key:

- *Links with the holder scanning.* It reads a device string, refuses one past its `exp` or
  naming another host, opens an offer bound to the string's `sign`, and puts the offer ticket,
  signed by the master key and sealed to the string's `enc`, at the string's pre-join address
  with `bind {scid, offerId, sign, r}`. Then it waits for the claim and confirms with the match
  code as for any link.
- *Gives the master key to a device that keeps it*, only to a device key valid at the head
  that the log lets keep it: a standing key of the `incept`, or a standing key whose `add`
  carries a co-signature over a claim that said `canKeepMaster: true` (the co-signature covers
  the flag, so the log proves it; at a link, only on a `device` offer). A key the log does not
  list, or a revoked one, gets nothing. The offer's kind counts only at the link: an `add`
  that spent a `node` offer, whose claim said `canKeepMaster` (an executor with the custody
  channel claimed it), lets the key keep the master key, so a later yes of the person (sending
  it, or answering its `send-master` request) gives the master key to that node device. That
  is by design: the device's co-signature said its custody service may keep it, and each send
  is the holder's own decision. The record's device kind decides nothing here: any
  linked device can write the record, so its kind is shown to people and grants nothing. After
  the `add`, the master key seals itself into a `master` body, signed by itself, to the `enc` key the log
  lists for the device key, and writes it to the device's joined inbox. Before the write the
  app keeps the push on a retry list of its own; it forgets it once the host takes it, or when
  the host refuses it for good, or when the device key has left the log, and retries what is
  left at each start and unlock. The holder marks the device `holdsMaster: "offered"`; the
  device marks itself `true` with the key's `masterIndex` once its vault keeps the key, and
  only the operation that keeps it does so, fenced by the vault's custody epoch: the mark
  names the index the vault keeps, and once it lands the epoch is read again — moved, a
  removal won and the entry is written to `false` (or to the index the vault keeps then).
  A take whose mark is refused while the recovery pair waits on the new master is owed the
  mark still: the key is stored and the message acknowledged either way, and the caller
  completes it — there is no persistent repair queue, and a later refresh that finds the
  current key kept repairs nothing on its own. An
  `"offered"` never replaces `true`: the holder writes it only on a version where the device's
  entry is not `true`, and checks again on every newer version it reads after a stale one, so
  a device that took the key before the holder's write landed stays `true`. At a first link
  the device's entry (`false`) is written before the push, so a device that takes the key at
  once finds its entry to mark.
- *Removes the master key from a device* only while another copy of the current master key
  remains in another vault: another device valid at the head that the log lets keep it, listed
  `holdsMaster: true` with `masterIndex` equal to the index of the log's current master and a
  `vault` that is not this device's vault. An entry offered, without an index, with a replaced
  index or without a vault id, an entry of another device key on this vault, and the 12 words,
  are no copy. The app checks this on the record version its write builds on, again on each
  re-read after a stale version, and the vault forgets the key only after the write; of two
  devices removing it at once, the second write builds on the first's version and is refused.
  The write marks this device `holdsMaster: false` and, while the custody epoch has not moved
  since the release, every other entry that names this vault: the forget takes the vault's one
  copy from all of them. When the epoch moved (a store came between), it marks this device
  only. After the forget, while the vault keeps no key, the app writes `false` on the entries
  that still name this vault.
- *Lists itself at creation.* The record's first version, written right after the `incept`
  lands, lists the creating device as `d1` with `holdsMaster: true`, `masterIndex` 0 and the
  `vault` id of the vault that keeps master #0, so it
  counts as a copy and a rotation on another device sends it the new master key.
- *Keeps a rotated master key before the rotation is public.* A rotation (from the 12 words,
  in a restore too) stages master *n*+1 in the vault before it publishes the `rotate`: staged,
  the key is durable but signs nothing, and the vault's kept key is still master *n*. A vault
  that refuses to stage stops the rotation before anything is published. A publish the host
  refuses for good discards the key that rotation staged, and revokes nothing; the vault keeps
  an opaque owner token beside each staged key, so a rotation discards only the key it staged
  itself, never one another app on the device staged after it; after a publish whose answer
  never came the key stays staged, since that publish may still land. Once the `rotate`
  landed, the vault promotes the staged key to its kept key; a failed promotion is reported
  with the rotation, which stands. Every app on the device shares the vault, so a refresh in
  any of them may find a staged key, and judges it by its master index against the log's
  current master index: **equal**, it promotes it (and lists the device holding it);
  **ahead**, it leaves it staged (a rotation's publish may be on its way); **behind**, or the
  identity has ended, it discards it (a later rotation superseded it, or its publish was
  refused long ago). A rotation whose own promotion finds the key already promoted by such a
  refresh keeps it all the same. Promotion goes by master index only: the words derive the
  key at each index, and the staged key is not checked against the commitment in the log.
- *Finishes a restore without the words again.* A restore whose rotation landed but whose
  enrolment (the vault's promotion, the link of this device) failed reports the rotation as
  standing with the failure; the restore finishes from the master key it already holds, without a second
  rotation.
- *Refreshes a replaced master key* by taking the current one from its joined inbox, else
  forgetting its own key and asking a holder with `send-master`. It forgets its key only when
  the vault can tell the key's index (authenticated with the key, not a note beside it) and
  that index is behind the log's current master; a key whose index it cannot tell is kept.
- *Delivers at least once.* Delivery of a master key is at least once: a push whose answer
  was lost stays on the retry list and is put again, so a device may find the same master
  key in two messages, or take an older one after a newer one is already kept; a request
  answered whose delete failed stays in the box and may be answered again. Both sides act
  idempotently: a device keeps a master key it already keeps without change and never lets
  an older one replace a newer kept one (its vault forgets or replaces a key only on the
  index it checked, a shared custody folder is generation-aware), acknowledges each
  message, and the holder refuses a `confirm-revoke` naming a key no longer valid at the
  head before it signs anything.
- *Writes the record* by reading the newest version, applying its own changes (a device's
  entry, a name, `holdsMaster`, a pending revocation) and dropping the entries of keys no
  longer valid at the head, sealing the result as the next version to every reader of the
  log, and writing it. When the host answers a stale version, or that the readers changed, it
  reads and merges again. The holder writes after each link and each revocation.
- *Answers the request box* only on the person's yes, after checking that the device named
  posted the request (its signed store request, by that device key at a position the log
  holds, covers `JCS({body, expires})`) and that it still holds (not past `expires`; a
  `confirm-revoke` names keys valid at the head): `send-master` as above, `confirm-revoke` with a
  `revoke` signed by the master key (one with `rotate` needs the 12 words). It then deletes the
  request with a store request the master key signs (after a rotation, master *n*+1; a delete
  that fails then is reported with the rotation, which stands).
