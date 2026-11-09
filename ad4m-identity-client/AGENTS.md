# ad4m-identity-client — agent guide

The master-key side of `did:scid:coasys:1` for apps.
The executor holds device keys only; an app (the desktop custody service, later the phone and
the auth domain) holds the words and master keys through this crate, builds the events, and
lets its executor co-sign and publish them. No custody key: no service keeps a copy of a
master key for the person.

| File | What |
|---|---|
| `src/lib.rs` | `Clock`, `Random` (the surface passes both: no clock or randomness of its own), `SystemClock` (not wasm32), `OsRandom`, `RandomRng` (a `Random` as the core's seal RNG), `ClientError` |
| `src/secret.rs` | `Secret<T>`: zeroed on drop, `Debug` prints `<redacted>`, no `Serialize`, no `Clone` |
| `src/vault.rs` | `Vault`, `Unlocked` (one unlock per flow, `MasterUse::ttl_s`: a link's lasts `OFFER_TTL_S + CONFIRM_GRACE_S`, so `confirm` works through the store's grace; a revocation's and a master-key push's 300 s; a vault passes `why` to `Unlocked::new`), `MasterSigner` (`Signer` + `index` + `seal_self`: the key seals itself into an inbox `master` body), `KeySigner` (a master key in memory, for a surface's vault too), `MasterUse` (`Link`, `Revoke`, `SendMaster`), `VaultError`, `PendingPush`; the trait has `store_master`, `unlock`, the pending incept of a `create` (`store_pending_incept`, `pending_incepts`, `incept_published`) the retry list of master-key pushes (`keep_push`, `pending_pushes`, `push_done`), the record's per-identity high-water (`record_seen`, `record_seen_reset`), `master_index` (no unlock; authenticated, from the item: the index sealed with the key, `None` when the vault keeps no key or cannot tell its index) and `forget_master(scid, index)` (a compare-and-delete: forgets only a kept key still at `index`, answers `Forgot::{Forgot, Kept(kept)}`; a caller never forgets on a `master_index` of `None`); the kept key never goes back a generation — `store_master` and `promote_master` answer `Stored::{Stored, AlreadyNewer{kept}}`, a kept key at a higher index is never replaced by a lower one (`promote` then discards the staged key; `NotFound` when none is staged at `index`), and everything reporting a held master names the index the vault keeps; the staging of a rotation's key (`stage_master(scid, index, secret, owner: &StageToken)`: durable, never signs, not given by `unlock`, replaces any staged key, the owner token kept beside it; `staged_index`, no unlock; `discard_staged(scid, index, owner: Option<&StageToken>)`: forgets the staged key at `index` only when `owner` is `None` or matches its token, else a no-op; `StageToken`: 16 opaque bytes a rotating flow draws once, not secret, so a flow discards only the key it staged and never one another app on the computer staged after it); `MemoryVault` (tests and feature `test-vault`, never in a release build) |
| `src/executor.rs` | `ExecutorRpc` (one RPC call), `ViaExecutor` (the typed `identity.*` calls the flows make: status (`IdentityStatus`, with the executor's `store: Option<StoreInfo>`), log, cosignIncept, publish, openOffer, offers, inboxPut, takeMaster (`TakenMaster`, its secret a `Secret`), inboxAck, record, recordPut (and `record_with`/`record_put_with`, which pass a store request the master key signed: the restore's reads and writes before this device is in the log), unwrapRecordKey, requests, postRequest, deleteRequest, keyOwner (`KeyOwner`: the key index's identity of a key), claimOffer (`OfferClaimed`; the restore claims its own offer)); feature `ws`: `ExecutorRpc` for `ad4m-client`'s `WsRpcClient` |
| `src/request.rs` | (crate) the log folded, the store the executor accepted the head from, the master key's position at the head, `master_request` (a store request the master key signs) |
| `src/record.rs` | The private details record, the one place that opens, merges and writes it: `read` (the executor's `{record, receipt}` answer, the receipt checked against the store key of the head the executor accepted — `store_of` — and the envelope's writer at the receipt's seq, its entry's key unwrapped by the executor, decrypted here; each read keeps the identity's highest version in `Vault::record_seen` and refuses a lower one), `merge` (the `Change`s, then the entries and pending revocations of keys no longer valid at the head go), `write` (newest version + changes, sealed to the log's readers as the next version; a stale version or changed readers read and merge again, at most 4 times, then `RecordBusy`; a `HoldsMaster` for a device the record does not list is refused `NotListed`), `write_if` (`write` while a `Precondition` holds on the version it builds on, checked again on every version read after a stale one); `Change::HoldsMaster` clears an entry's `masterIndex`, `Change::Holds { key_id, index }` sets `holdsMaster: true` with it; `read_as`/`write_as` take a `Reader`: `Executor` (as `read`/`write`), `Master` (opened as `Executor`, but the master key the vault keeps signs the envelope and the store requests — the re-attesting write a device holding the new master makes), or `Recovery` (the words' recovery secret opens the `recovery` entry, and the current master signs the envelope and the store requests). A device write keeps the recovery key and its attestation, and is refused `RecoveryNeedsReattest` while the stored pair is stale after a rotation, since it would seal the record without the key; only a master-signed write installs, replaces or drops it — it attests the stored key again when the pair is stale |
| `src/flows/create.rs` | `CreateFlow` (words shown, as `CreateScreen::ShowWords(Secret<Vec<String>>)`, zeroed on drop → two typed back → `finish`): master #0, `incept` listing the executor's key as `d1`, `identity.cosignIncept`, fold, the vault keeps master #0 and the co-signed incept, then `identity.publish`; `resume` publishes a kept incept again after a failed or unanswered publish; then record version 1 with the words' recovery key, listing this device (`finish`'s `DeviceEntry`) as `d1` with `holdsMaster: true` and `masterIndex` 0 (`Created::record`) |
| `src/flows/rotate.rs` | `rotate` (the words derive master *n*+1, its salt and the commitment to *n*+2; the words must derive the current master, else `NotTheseWords`; a `rotate` signed by master *n*+1 alone, revoking the given keys; a `409` or a lost answer reads the log again: an event of ours found there by its hash stands, otherwise it is built again on the new head (*n* from that head, revoking only keys still valid there), at most `MAX_TRIES` builds, then `LogBusy`; the vault stages *n*+1 before each publish (`publish_rotation`; a vault that refuses publishes nothing, a publish refused for good discards the key this flow staged (with its `StageToken`; another app's later stage survives), after any unanswered one it stays staged for `refresh_master` since that answer may still land), then promotes it once the rotation landed (`rotate::promote`: a `NotFound` while the vault keeps *n*+1 or later is another app's refresh or a later rotation keeping it first, and counts as kept; `Rotated::kept` answers the index the vault keeps — *n*+1 or a later one — `Err` on a failed promote: the rotation stands, this device's entry says `holdsMaster: false`); *n*+1 is pushed to each other device valid at the head that the record lists with `holdsMaster` yes or offered and the post-rotation log lets keep it (`link::may_keep_master`, never the record's kind), with the retry list; the record: the master-signed write comes first — it re-attests the recovery key by *n*+1 and lists this device — then the pushes, then pushed apps `offered`). Answers `Rotated` |
| `src/flows/restore.rs` | `RestoreFlow`: `start` (parse the words, a wrong word named as `WordsError::UnknownWord`; master #0 → `identity.keyOwner` → the identity whose `m0` it is; the words must derive its current master; the record read with the recovery secret; `devices()`: the device keys valid at the head, named from the record, with `locked_out_by_default`: apps and unnamed devices ticked, servers, community hosts, Coasys cloud and bots not), `finish` (one rotation revoking the chosen keys, *n*+1 staged before the publish as in `rotate`, then `enrol`; refused once the flow rotated), `enrol` (promotes the staged *n*+1 or stores it from the words held by the flow, this executor linked by an offer bound to its device key that it claims itself, *n*+1 to the kept apps, the record written with the recovery key listing this device as the holder; a device already in the log is not linked again). Answers `Restored`, whose `enrolled` carries the enrolment's outcome: an `Err` means the rotation stands and `enrol` on the same flow finishes it, no words typed again |
| `src/flows/link.rs` | `LinkFlow`: `open` (offer signed by the current master at the head, offer link; with a device key, bound to it; refuses `OfferKind::Bot`), `open_bot` (a bot offer naming the `BotScope`, the actions the bot's key may ever take and until when; the `add` that spends its claim gives the key the bot mark and that scope; an executor claims it with `bot.createAgent`), `scan` (the holder read a device string: an offer bound to its key, the ticket sealed to its `enc` into its pre-join inbox; refuses a string past `exp` or for another store), `wait_for_claim` (master-signed list requests; past the offer's `expires` it answers `OfferEnded("expired")` before any request; `link::verify_claim_view` refuses a claim view by another key than a bound offer's (`LinkError::Bound`) or whose device co-signature does not cover its `encKey` and `canKeepMaster`, before anything is signed; the match code; `Claimed::can_keep_master`), `may_keep`, `confirm` (the `add` that spends the claim; the device's `DeviceEntry` into the record (`holdsMaster: false`), then with `keep_master` the master key into the device's inbox and `mark_offered`: the entry goes in before the key, so a device that takes it at once ends `true`; answers `Linked`; takes `&self`, so after a failed publish the flow confirms again while the claim is still `claimed`: the push and the record write run only after the publish succeeded; when the store took the add but its answer or the read after it was lost, a repeat finds this offer's `add` of this claim's key and co-signature in the refreshed log and runs the push and the record write once without publishing; a repeat after those steps ran, or whose log's add is another key's or offer's, goes to the store and is refused before either) |
| `src/flows/master.rs` | `send_master` (`check_keeper` before the unlock: a device key valid at the head that the log lets keep it, `keeper` over `link::may_keep_master`, never the record's kind; one unlock, the push, `holdsMaster: "offered"`), `mark_offered` (crate: `"offered"` through `write_if`, never over `true`; a device that says `true` is left out, `HoldsMaster` when none is left; a device the record does not list is left out the same way, `NotListed`), `retry_pushes` (the vault's retry list for one identity; a push whose device the record does not list writes its entry first (`write_if`, `Listed` when it landed in between) and is skipped when that write fails), `take_master` (the device's side: `identity.takeMaster`, checked against its own fold, `store_master` — an `AlreadyNewer` is kept: the answer and the record name the index the vault keeps — `inboxAck`, `holdsMaster` as the vault's custody through `hold_mark` — crate: the entry ends `true` with the index `master_index` authenticates, `false` when the vault keeps none, the custody epoch re-read after the write and the entry mirrored again while it moved, so a `true` never stands for a copy a `remove_master` took; the key and the ack stand when the write is refused `RecoveryNeedsReattest`, and the take answers the marking owed, `record_pending` — no persistent repair queue: a caller must act on it, since a later refresh answers `Current` without repairing), `reattest_recovery` (the device holding the current master re-attests the record's recovery key with it — `Reader::Master`, one unlock — and writes no holder mark: a re-attestation asserts no custody, since an unlocked signer outlives its kept slot), `complete_pending_mark` (the repair a pending take or another refused `holdsMaster` write owes: one unlock, the master-signed fenced `hold_mark`, which also re-attests a stale pair), `refresh_master` (a staged key, by its master index against the log's current one: equal, promoted and this device listed holding it (`Promoted`; the write goes as the device through `hold_mark`, or as the promoted master re-attesting when the stored pair is stale; promoted first or a newer key kept by another app counts — `index` is the index the vault keeps, which the record write names); ahead, left staged, since a rotation's publish may be on its way in any app sharing the vault; behind, or the identity ended, discarded. Promotion goes by index only: the words derive each index's key, and the staged key is not checked against the log's commitment; a vault on a replaced master takes the current one from the inbox, else forgets its key — `forget_master_at` the custody epoch read with the index, so a key another app installed since, the same one included, survives — only when the vault's `master_index` is known and behind the current one, and posts `send-master`; an unknown index forgets nothing (`Unknown`); answers `Refreshed`), `remove_master` (refused with `OnlyCopy` unless another device valid at the head holds the current master: the record lists it as `holdsMaster: true` with `masterIndex` = the log's current master index, and the log lets keep it (`link::may_keep_master`, never the record's kind); the check runs inside the record write on the version it builds on (`record::write_if`), again after each stale version, so of two holders removing at once one is refused; the record says `holdsMaster: false` before the vault forgets it — `forget_master_at` the custody epoch read before the check, so a key another app installed in between, the same one included, survives); `push` (crate): `keeper` on the log's events, on the retry list while the put runs, kept on a 5xx or 429 |
| `src/flows/revoke.rs` | `revoke` (a `revoke` signed by the current master, then the record without the revoked devices; answers `Revoked`; the write reads the newest version with its receipt, so a version the revoked key wrote before the publish still opens and the write keeps what it says), `revoke_and_rotate` ("Also replace my master key": the keys revoked in a `rotate` from the words), `key_id_of` |
| `src/flows/requests.rs` | `check_poster` (the request's signed store request: its device key, valid at the head and at the position it names, over `JCS({body, expires})`), `answer` (refuses an expired request and a `confirm-revoke` naming a key not valid at the head; `send-master` as `send_master`; `confirm-revoke` without `rotate`; then the master-signed delete; `rotate` is `NeedsWords`), `answer_with_words` (a `confirm-revoke` with `rotate` as a rotation; deleted with master *n*+1; `Answered::Rotated { rotated, deleted }`: a failed delete is reported beside the rotation, not as an error), the device's side: `ask_for_master`, `ask_confirm_revoke` (refuses an empty list or a key not valid at the head) |
| `examples/holder.rs` | the flows over JSON lines against a real executor (features `ws,test-vault`), for `tests/js/tests/identity.test.ts` |
| `tests/flows.rs` | the flows against a real `identity-store` (router in process) through a test executor |

## Rules

- **The 12 words are the root authority.** They derive every master key, the salts and the
  recovery secret, so they rotate, revoke and enrol without any device of the log; a rotation
  revokes no one's copy of them. Master keys live in a `Vault`; a flow reads one only through
  `Unlocked`. **The words and master keys never reach an executor or a log.** No `ViaExecutor`
  call takes or returns one.
- **The vault's writes are atomic and generation-aware** (`Vault`): `store_master` and
  `promote_master` never replace a kept key with a lower index (they answer
  `Stored::AlreadyNewer{kept}` instead; a promote then discards the dead staged key), and
  `forget_master(scid, index)` is a compare-and-delete (`Forgot::{Forgot, Kept(kept)}`).
  The vault is shared by every app on the computer, so no flow reads the kept index and then
  writes or deletes unconditionally — every read-then-write goes through the vault's rule,
  and every answer and record write names the index the vault keeps.
- **One unlock per flow** (`Vault::unlock`): linking signs the offer, the list requests and the
  `add` under one unlock. A callback (`with_unlocked(callback)`) would fit a synchronous flow; the
  flows are async across network calls, so the unlock is a guard the flow holds instead.
- The client re-checks what it reads: every log goes through `fold` here too.
- The core (no features) builds for `wasm32-unknown-unknown`; keep `tokio`, sockets and the
  system clock out of it (they sit behind `ws`, in examples and tests).
- **Custody before publish.** `create` stores master #0 and the co-signed incept in the vault
  before `identity.publish`, and forgets the incept only when the publish answers. A vault that
  refuses leaves nothing public; a lost answer is finished by `resume` (or `finish` with the
  same words), which publishes the identical incept again (the executor answers a repeat of a
  held log with the store's head).
  A rotation (`rotate`, `revoke_and_rotate`, `answer_with_words`, `RestoreFlow`) stages
  master *n*+1 in the vault before `identity.publish` and promotes it once the rotation
  landed: a vault that refuses publishes nothing; a publish refused for good discards the
  key that flow staged (its `StageToken`, so two apps rotating at once never discard each
  other's) and revokes nothing; a flow cut off after the publish, or whose answer was lost,
  leaves the key staged. The vault is shared by every app on the computer, so any app's
  `refresh_master` may meet a staged key mid-publish: staged index equal to the log's current
  master index, promoted; ahead of it, left alone (the publish may still land); behind it,
  discarded. A flow's own promote that finds the key already promoted (`NotFound`, the vault
  keeping that index or a later one) reports the index the vault keeps.
- `create` publishes no proof of work: the store has no brake yet and admits a `#0` without
  one; the `pow` step comes with the brake.
- **A master key leaves the vault only sealed.** `MasterSigner::seal_self` builds, signs and
  seals the `master` body inside the signer; the payload's copy of the secret is wiped there.
  It goes only to a device key valid at the head, without the bot mark, that the log lets
  keep it (`ad4m_identity_core::link::may_keep_master`: a standing key of the incept, or an
  `add` whose claim co-signature said `canKeepMaster`), checked by `master::keeper` on every
  push (link, `send_master`, a `send-master` request, the retry list); at a link, only on a
  `device` offer (`LinkFlow::may_keep`). **The record's device kind never decides it**: any
  linked device can write the record, so its kind is for people to read and grants nothing.
  The offer's kind counts only at the link: `keeper` does not read it, so a key that claimed a
  `node` offer with `canKeepMaster` (an executor with the custody channel) gets the master key
  on a later yes of the person (`send_master`, or answering its `send-master` request). That
  is accepted by design: the claim's co-signature says its custody service may keep the key,
  and each send is the holder's decision.
- **One place merges the record** (`record::write`). Every flow that changes it passes
  `Change`s; nothing else seals or writes a record. A step after a publish or a push (the
  record write, the push after a link) reports its own outcome in the answer (`Linked`,
  `Sent`, `Revoked`, `Taken`): the published event stands either way.
- **Custody before the push.** A push is on the vault's retry list before the inbox write and
  forgotten once the store takes it, or refuses it for good (any 4xx but 429), or its device
  key leaves the log; `retry_pushes` runs at each start and unlock.
- **At least once.** A push whose answer was lost is put again, and a request whose delete
  failed stays in the box and may be answered again: delivery of a master key is at least
  once, so a device may take the same key twice or an older key after a newer one is kept —
  `take_master` and `answer` act idempotently (the vault rule keeps the newer index, each
  message acknowledged; a `confirm-revoke` of a key no longer valid refused before anything
  is signed).
- The match code (30 bits) is for the person to compare two screens, not an authentication:
  a claimer could grind device keys to a code. The offer secret, the single claim and a bound
  offer keep strangers out; never treat a matching code as proof of the key.
- The app reads the record as its executor's device (`identity.unwrapRecordKey` opens the
  record key of the executor's entry, local only; `decrypt_record` checks the store's receipt
  and the recovery attestation against the log first, the writer at the receipt's seq, and the
  version against the vault's per-identity high-water) and writes it as that device:
  `seal_unsigned` names the executor's device key id as `signer`, and `identity.recordPut`
  signs the envelope with the device key in the wallet. A write carries
  `Opened::recovery` (`recovery_of` of the version read: the recovery key that the current
  master attested) unchanged; only a master-signed write installs, replaces or drops it. When
  the stored pair's attestation is stale after a rotation (`Opened::recovery_key` set,
  `Opened::recovery` none), a device write is refused `RecoveryNeedsReattest` — sealing the
  record without the key would leave the words unable to open it — and a device holding the
  new master re-attests it (`reattest_recovery`, or `refresh_master`'s promote and pending
  take through `complete_pending_mark`). A re-attestation writes the recovery pair only and
  no `holdsMaster` mark — an unlocked signer outlives its kept slot, so the holder mark comes
  only from the operation that kept the key, fenced by the custody epoch (`hold_mark`). A
  revoke under a stale pair still publishes the `revoke`; only its record rewrite is refused
  until a re-attest. The store's own rule is unchanged: it still takes a device write
  carrying the stale
  pair as none, so the refusal is the client's, not the store's. A surface with an `enc` key of its own (the
  phone) reads the store directly once that client lands.
- **A rotation needs the words, not a master key.** `rotate` is signed by master *n*+1 alone,
  derived from the words; no flow rotates from a vault key. The flows that rotate (`rotate`,
  `revoke_and_rotate`, `answer_with_words`, `RestoreFlow`) never unlock the vault.
- Restore reads and writes the record before this device is in the log: the store requests
  are signed by the current master, and the record is opened with the words' recovery
  secret. The executor only passes those requests on (`identity.record`/`recordPut` with
  `request`).
- Not here yet: the brake (proof of work, rate limits) on a `409` loop: the client stops
  after `MAX_TRIES` builds with `LogBusy`; password steps.

## Tests

```bash
cargo test -p ad4m-identity-client --features ws,test-vault
cargo clippy -p ad4m-identity-client --all-targets --features test-vault -- -D warnings
cargo fmt -p ad4m-identity-client --check
cargo build -p ad4m-identity-client --target wasm32-unknown-unknown
cargo build -p ad4m-identity-client --example holder --features ws,test-vault
```

The flows against a real executor: `cd tests/js && pnpm run test-main-identity`.
