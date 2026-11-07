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
| `src/vault.rs` | `Vault`, `Unlocked` (one unlock per flow, `MasterUse::ttl_s`: a link's lasts `OFFER_TTL_S + CONFIRM_GRACE_S`, so `confirm` works through the store's grace; a revocation's and a master-key push's 300 s; a vault passes `why` to `Unlocked::new`), `MasterSigner` (`Signer` + `index` + `seal_self`: the key seals itself into an inbox `master` body), `KeySigner` (a master key in memory, for a surface's vault too), `MasterUse` (`Link`, `Revoke`, `SendMaster`), `VaultError`, `PendingPush`; the trait has `store_master`, `unlock`, the pending incept of a `create` (`store_pending_incept`, `pending_incepts`, `incept_published`) the retry list of master-key pushes (`keep_push`, `pending_pushes`, `push_done`), and `master_index` (no unlock) and `forget_master`; `MemoryVault` (tests and feature `test-vault`, never in a release build) |
| `src/executor.rs` | `ExecutorRpc` (one RPC call), `ViaExecutor` (the typed `identity.*` calls the flows make: status (`IdentityStatus`, with the executor's `store: Option<StoreInfo>`), log, cosignIncept, publish, openOffer, offers, inboxPut, takeMaster (`TakenMaster`, its secret a `Secret`), inboxAck, record, recordPut (and `record_with`/`record_put_with`, which pass a store request the master key signed: the restore's reads and writes before this device is in the log), unwrapRecordKey, requests, postRequest, deleteRequest, keyOwner (`KeyOwner`: the key index's identity of a key), claimOffer (`OfferClaimed`; the restore claims its own offer)); feature `ws`: `ExecutorRpc` for `ad4m-client`'s `WsRpcClient` |
| `src/request.rs` | (crate) the log folded, the store the executor accepted the head from, the master key's position at the head, `master_request` (a store request the master key signs) |
| `src/record.rs` | The private details record, the one place that opens, merges and writes it: `read` (the executor's sealed record, its entry's key unwrapped by the executor, decrypted here), `merge` (the `Change`s, then the entries and pending revocations of keys no longer valid at the head go), `write` (newest version + changes, sealed to the log's readers as the next version; a stale version or changed readers read and merge again, at most 4 times, then `RecordBusy`); `read_as`/`write_as` take a `Reader`: `Executor` (as `read`/`write`), or `Recovery` (the words' recovery secret opens the `recovery` entry, and the current master signs the store requests). A write keeps the recovery key, or sets it once when given |
| `src/flows/create.rs` | `CreateFlow` (words shown, as `CreateScreen::ShowWords(Secret<Vec<String>>)`, zeroed on drop → two typed back → `finish`): master #0, `incept` listing the executor's key as `d1`, `identity.cosignIncept`, fold, the vault keeps master #0 and the co-signed incept, then `identity.publish`; `resume` publishes a kept incept again after a failed or unanswered publish; then record version 1 with the words' recovery key (`Created::record`) |
| `src/flows/rotate.rs` | `rotate` (the words derive master *n*+1, its salt and the commitment to *n*+2; the words must derive the current master, else `NotTheseWords`; a `rotate` signed by master *n*+1 alone, revoking the given keys; a `409` or a lost answer reads the log again: an event of ours found there by its hash stands, otherwise it is built again on the new head (*n* from that head, revoking only keys still valid there), at most `MAX_TRIES` builds, then `LogBusy`; the vault keeps *n*+1; *n*+1 is pushed to each other desktop or phone app with `holdsMaster` yes or offered, with the retry list; the record: pushed apps `offered`, this device `true`, the recovery key kept). Answers `Rotated` |
| `src/flows/restore.rs` | `RestoreFlow`: `start` (parse the words, a wrong word named as `WordsError::UnknownWord`; master #0 → `identity.keyOwner` → the identity whose `m0` it is; the words must derive its current master; the record read with the recovery secret; `devices()`: the device keys valid at the head, named from the record, with `locked_out_by_default`: apps and unnamed devices ticked, servers, community hosts, Coasys cloud and bots not), `finish` (one rotation revoking the chosen keys, the vault keeps *n*+1, this executor linked by an offer bound to its device key that it claims itself, *n*+1 to the kept apps, the record written with the recovery key listing this device as the holder). Answers `Restored` |
| `src/flows/link.rs` | `LinkFlow`: `open` (offer signed by the current master at the head, offer link; with a device key, bound to it), `scan` (the holder read a device string: an offer bound to its key, the ticket sealed to its `enc` into its pre-join inbox; refuses a string past `exp` or for another store), `wait_for_claim` (master-signed list requests; past the offer's `expires` it answers `OfferEnded("expired")` before any request; `link::verify_claim_view` refuses a claim view by another key than a bound offer's (`LinkError::Bound`) or whose device co-signature does not cover its `encKey` and `canKeepMaster`, before anything is signed; the match code; `Claimed::can_keep_master`), `may_keep`, `confirm` (the `add` that spends the claim; with `keep_master` the master key into the device's inbox; the device's `DeviceEntry` into the record; answers `Linked`; takes `&self`, so after a failed publish the flow confirms again while the claim is still `claimed`: the push and the record write run only after the publish succeeded; when the store took the add but its answer or the read after it was lost, a repeat finds this offer's `add` of this claim's key and co-signature in the refreshed log and runs the push and the record write once without publishing; a repeat after those steps ran, or whose log's add is another key's or offer's, goes to the store and is refused before either) |
| `src/flows/master.rs` | `send_master` (`check_app` before the unlock: a device key that can take it, listed in the record as a desktop or phone app; one unlock, the push, `holdsMaster: "offered"`), `retry_pushes` (the vault's retry list for one identity), `take_master` (the device's side: `identity.takeMaster`, checked against its own fold, `store_master`, `inboxAck`, `holdsMaster: true`), `refresh_master` (a vault on a replaced master: take the current one from the inbox, else forget the old one and post `send-master`; answers `Refreshed`), `remove_master` (refused with `OnlyCopy` unless another desktop or phone app valid at the head holds it per the record; the record says `holdsMaster: false` before the vault forgets it); `push` (crate): on the retry list while the put runs, kept on a 5xx or 429 |
| `src/flows/revoke.rs` | `revoke` (a `revoke` signed by the current master, then the record without the revoked devices; answers `Revoked`), `revoke_and_rotate` ("Also replace my master key": the keys revoked in a `rotate` from the words), `key_id_of` |
| `src/flows/requests.rs` | `check_poster` (the request's signed store request: its device key, valid at the head and at the position it names, over `JCS({body, expires})`), `answer` (refuses an expired request and a `confirm-revoke` naming a key not valid at the head; `send-master` as `send_master`; `confirm-revoke` without `rotate`; then the master-signed delete; `rotate` is `NeedsWords`), `answer_with_words` (a `confirm-revoke` with `rotate` as a rotation; deleted with master *n*+1), the device's side: `ask_for_master`, `ask_confirm_revoke` (refuses an empty list or a key not valid at the head) |
| `examples/holder.rs` | the flows over JSON lines against a real executor (features `ws,test-vault`), for `tests/js/tests/identity.test.ts` |
| `tests/flows.rs` | the flows against a real `identity-store` (router in process) through a test executor |

## Rules

- **The words and master keys never reach an executor or a log.** No `ViaExecutor` call takes
  or returns one. Master keys live in a `Vault`; a flow reads one only through `Unlocked`.
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
- `create` publishes no proof of work: the store has no brake yet and admits a `#0` without
  one; the `pow` step comes with the brake.
- **A master key leaves the vault only sealed.** `MasterSigner::seal_self` builds, signs and
  seals the `master` body inside the signer; the payload's copy of the secret is wiped there.
  It goes only to a device key valid at the head, without the bot mark, and then only to a
  claim with `canKeepMaster` on a `device` offer (`LinkFlow::may_keep`), or to a device the
  record lists as a desktop or phone app (`send_master`, a `send-master` request).
- **One place merges the record** (`record::write`). Every flow that changes it passes
  `Change`s; nothing else seals or writes a record. A step after a publish or a push (the
  record write, the push after a link) reports its own outcome in the answer (`Linked`,
  `Sent`, `Revoked`, `Taken`): the published event stands either way.
- **Custody before the push.** A push is on the vault's retry list before the inbox write and
  forgotten once the store takes it, or refuses it for good (any 4xx but 429), or its device
  key leaves the log; `retry_pushes` runs at each start and unlock.
- The app reads the record as its executor's device (`identity.unwrapRecordKey` opens the
  record key of the executor's entry, local only); a surface with an `enc` key of its own (the
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
