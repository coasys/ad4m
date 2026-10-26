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
- **Part C — Wallet profile** (sections 15–16) is informative: how a Coasys wallet derives
  master keys from the 12 words, and the signing domains of the rest of the system. A
  verifier of a log needs none of it; it lets wallets and apps interoperate.

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
- **P7** The canonical JSON text of the event (section 4), proofs included, is at most
  65 536 bytes (`Format(TooLarge)`). `offer`, `coSig`, `salt` and `can` have no length limit
  of their own, so this limit stops one signed event from growing without bound.

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

## 6. Key validity

`fold` gives the state after the head: every master and device key with the seq that added it
and the seq that ended it, the bot scopes per seq, `next`, and `ended`.

- `key_valid_at(k, s)`: `k` (a key id or a signing multikey) was added at or before `s` and not
  revoked, replaced or ended at or before `s`. A key revoked at *N* stays valid below *N*.
- `key_valid_at_time(k, s, t)`: `key_valid_at(k, s)`, and for a key with `expires` (`Until`)
  `t < expires`, and for a bot key `t < expires` of the scope in force at `s`. `t` is the
  caller's trusted time.
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
   the first source that gives a valid log or read further sources for a longer one; either
   way it keeps the longest valid log it holds. A shorter valid log never replaces a longer one
   (it is a prefix, or a fork). Two valid logs that differ at one seq are a fork: the resolver
   answers with the log it held first, and reports the fork in the DID resolution metadata
   (`coasys:fork`: the seq and the two event hashes).
4. **The DID document.** The resolver derives the DID document (section 9) from the state
   after the head. With `versionId=<seq>`, it derives it from the state after that seq. A
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
6. **What a host can do.** A host can withhold events or serve an older head. It cannot forge
   one: every event carries proofs by keys the state before it names, and event #0 hashes to
   the SCID in the DID. The resolver must not trust a host for anything except the bytes it
   returns, and checks every byte itself.

## 9. DID document

The DID document of a log at seq *s* is a pure function of the state after *s*. It reads no
clock: an expiry is a property of a verification method, never a filter, and a verifier
compares it with its own trusted time (`key_valid_at_time`, section 6). The reference is
`ad4m_identity_core::did_document`.

| Log item at *s* | DID document | Why |
|---|---|---|
| Standing device key `dN` valid at *s* (`key_valid_at`) | `verificationMethod` `#dN`: `Multikey`, `publicKeyMultibase` = its `sign`. Listed in `authentication` and `assertionMethod` | devices sign as the person: links, records, logins |
| `Until` device key valid at *s* | as a standing key, plus `"coasys:expires": <expires>` on the method | DID Core has no expiry on a method. A verifier that ignores the property sees a key that the log refuses after `expires` |
| Bot key valid at *s* | `verificationMethod` `#dN` with `"coasys:bot": {"can": […], "expires": …}`, the scope in force at *s*. Listed only in `capabilityInvocation`, never in `authentication` or `assertionMethod` | a bot acts within a scope, not as the person: a generic verifier must not take a bot's statement as the person's |
| The `enc` key of each standing or `Until` key | `verificationMethod` `#dN-enc`: `Multikey`, `publicKeyMultibase` = its `enc`. Listed in `keyAgreement` | where messages to the person are sealed. A bot's `enc` key is absent: a bot has no inbox, and a message to the person must not reach one |
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
`publicKeyMultibase`, then `coasys:expires` and `coasys:bot` when they apply.

Example: the log of `vectors/log.json` at `versionId=3` (`vectors/document.json`):

```json
{
  "@context": [
    "https://www.w3.org/ns/did/v1",
    "https://w3id.org/security/multikey/v1",
    "https://coasys.org/ns/did-scid-coasys/v1"
  ],
  "id": "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt",
  "controller": "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt",
  "verificationMethod": [
    {
      "id": "#d1",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt",
      "publicKeyMultibase": "z6MkoLDHstjfesQKtngXMequRUhP96dNkWrGDghVxxCoksYQ"
    },
    {
      "id": "#d1-enc",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt",
      "publicKeyMultibase": "z6LSoKsNQ3ZWym1Qsf7fL6ajAHqjpzaudw4hEvtzA8ejGDgz"
    },
    {
      "id": "#d3",
      "type": "Multikey",
      "controller": "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt",
      "publicKeyMultibase": "z6Mkmu4QQQajcBEcm7r2iumaUyNZ45t5tUuojTUUSzdX3kYr",
      "coasys:bot": { "can": ["read"], "expires": 1900000000 }
    }
  ],
  "authentication": ["#d1"],
  "assertionMethod": ["#d1"],
  "keyAgreement": ["#d1-enc"],
  "capabilityInvocation": ["#d3"],
  "alsoKnownAs": [
    "did:scid:coasys:1:QmXRT2vLohNRbSeywATckzZPEow3qznARyeHUY6sXwurHt?src=store.example"
  ]
}
```

Its metadata: `{"versionId": "3", "nextVersionId": "4", "deactivated": true}` (the log of
the vectors ends at seq 6).

The context `https://coasys.org/ns/did-scid-coasys/v1` defines the two extension properties:

```json
{
  "@context": {
    "@version": 1.1,
    "@protected": true,
    "coasys": "https://coasys.org/ns/did-scid-coasys/v1#",
    "coasys:expires": {
      "@id": "coasys:expires",
      "@type": "http://www.w3.org/2001/XMLSchema#integer"
    },
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

`coasys:expires` is unix seconds: the `expires` of an `Until` key. `coasys:bot` is a bot key's
scope: `can` (actions from `read`, `write`, `join`) and `expires` (unix seconds).

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
  anchored at or after *N*; content anchored below *N* that was admitted before the revocation
  stays valid.
- An anchor past the head of the log the verifier holds is refused.
- The signer picks the anchor, so the anchor alone cannot stop a holder of a revoked key from
  backdating new content to a seq below *N*. The host that admits signed content (for an agent
  record, the store that takes the `PUT`) orders arrivals: when the content arrives it also
  checks `k` listed and valid at the head of the log it holds, and refuses the content
  otherwise, whatever anchor it names. A revoked key so cannot backdate new content past that
  host. Readers check at the anchor and rely on that host's admission for arrival order.
- A reader that first sees a signature after the key's revocation, anchored before it, and
  did not get it through such a host, may flag it. That is the reader's policy; the signature
  holds.

The anchor carries no DID, key id or hash. The identity is known from the content: a record is
keyed by its did:scid, and a link's author key maps to its identity through a key lookup that
the verifier confirms against the log (a key enters a log only with its own co-signature). The
key id is read from the log, which lists the signing key. A hash would only tell forked
branches apart, and a fork is only detected, never resolved (sections 8 and 11): it already
freezes the identity, so a hash would add bytes to every record and link and enable no check.

## 11. Security and privacy

- A store can withhold events but cannot add one: every event carries proofs by keys the state
  before it names. A verifier re-checks everything a store serves.
- Forks (two valid events at one seq) are detected, never resolved, by the hash-chain module;
  a shorter peer log never truncates a held one.
- Only the words make master *n*+1, so whoever holds master *n* cannot rotate alone.
- `fold` refuses unknown fields so that no personal data enters a log by accident. `offer` and
  `coSig` are opaque to `fold`.
- Revocation burns a key for good. Rule K8 refuses a key that the log ever held, revoked or
  not, so a revoked key can never return to this identity. A host that requires one key per
  identity (section 14) also refuses it in every other log, so it can never speak for another
  identity there either.
- The core reads no clock. Expiries are compared with the caller's trusted time. The window a
  host accepts for a new `expires` is host policy (section 14).
- A generic DID Core verifier ignores `coasys:expires` and `coasys:bot`. Bot keys sit outside
  `authentication` and `assertionMethod`, so such a verifier never takes a bot's signature as
  the person's; it does not apply an `Until` expiry, which the log and a `coasys/1` resolver
  still enforce.
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
| `fold.json` | one invalid log per rule with the pinned error `{code, reason, format}` |
| `proofs.json` | every proof of `log.json` with its proof config, config hash, signing input and event hash; a tampered body, a wrong key, another domain |
| `commitment.json` | entropy, words, seed, masters 0–3 with path, key, salt and commitment, and the recovery public key |
| `pow.json` | proofs at 8, 12 and 16 bits for the SCID of `log.json`, with their hashes |
| `domains.json` | the registry and the labels; one domain-field signature per domain, which verifies only under its own domain; the refused names |
| `link.json` | device linking: the offer commit, the claim and offer signing bytes, the match code, the offer and device strings, and the `add` events that spend a claim |
| `document.json` | the log of `log.json` with a fixed `alsoKnownAs`, the DID document and metadata at the head and after every seq, and a version past the head refused (`VersionPastHead`) |

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
    not trust the host's key reads it only as a hint of how far to page.
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
- **Expiry window.** The store refuses an `expires` in the past or more than 400 days ahead.
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
`coasys:kel`, `coasys:link`, `coasys:store-request`, `coasys:head`, `coasys:vouch`,
`coasys:offer`, `coasys:claim`, `coasys:pause`, `coasys:inbox`, `coasys:session`,
`coasys:account`, `coasys:room-key`, `coasys:bot-grant`. These labels enter a hash or a key
derivation and are never signing domains; every signing API refuses them: `coasys:pow`,
`coasys:pair-id:1`, `coasys:pair-key:1`, `coasys:match`, `coasys:wrap:1`, `coasys:auth:1`.
