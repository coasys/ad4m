# SFU

The **SFU** (selective forwarding unit) is a WebRTC media server inside the executor, built on `str0m`. It holds **SFU rooms**, keyed by neighbourhood URL and room name. Each participant sends its media once, and the SFU forwards it to the others. A neighbourhood's **call policy** decides whether its calls use the SFU. The SFUs of several executors can form a **cascade** and pipe media between them.

**Why:** In a **mesh** call every participant uploads its stream to every other participant, which fails beyond a few people. An SFU on an always-on executor needs one upload per participant. Media needs native UDP sockets and real-time code, so the SFU is in the executor, not in a language. How an app opens a call is on [Neighbourhood calls](../../../perspectives/neighbourhoods/calls.md).

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

1. The call policy is a link `<neighbourhood url> ad4m://sfu_config <literal:json:{mode, maxMeshParticipants}>`. Only validly signed links by the **neighbourhood creator** count; the newest wins. The link syncs like any shared link.
2. `mode` takes one of four values:

   | `mode` | Media path |
   |---|---|
   | `mesh` (default) | directly between participants; the executor forwards no media |
   | `designated` | this executor's SFU |
   | `gateway` | this executor's SFU, as for `designated`: the SDK takes every non-`mesh` mode as "sfu" and joins its own executor's SFU ([Session.ts](../../../../core/src/neighbourhood/Session.ts)). `sfu.sfuPeer[s]ForNeighbourhood` return the string `"gateway"` (`sfu_peer_for_neighbourhood` in [service.rs](../../../../rust-executor/src/sfu/service.rs)) |
   | `cascaded` | this SFU and its configured cascade peers |

3. A member calls `sfu.callJoin(neighbourhoodUrl, roomName, sdpOffer)`. The handler checks that the caller is a neighbourhood member (`member_did`) and returns an SDP answer. With `acceptRedirect`, the SFU may send the caller to the least-loaded SFU in the cascade.
4. Cascade SFUs announce their capacity over TCP gossip (`sfu_cascade_listen`, `sfu_cascade_peers`, a static list of peers). They move media over str0m-to-str0m pipe transports. An SFU's identity in the cascade is the config string `sfu_local_did`, read in `run()` ([lib.rs](../../../../rust-executor/src/lib.rs)); it is separate from the executor's device key and from its operator's identity.
5. Every `sfu.*` method declares its guard; a tenant joins calls on its host as any member does. A host that serves calls needs its media UDP range reachable, so it cannot sit behind an HTTP-only tunnel.
6. The SFU's limits and `member_did` use the caller's device `did:key`. Neighbourhoods count members by person through `author_of` ([Counts by person](../../../perspectives/neighbourhoods/membership/counts-by-person.md)).

Two checks decide who controls calls:

| What | Who | Code |
|---|---|---|
| the call policy | the **neighbourhood creator**, the author of the neighbourhood expression | `config_store.rs` |
| SFU room and config writes on one executor (`require_owner`) | the first local **owner** of the neighbourhood's perspective on that executor | `sfu_ws.rs` |

`sfu.ensureMembership` lets the operator register any DID as a neighbourhood member, for tests and bridges.

| Limit | Value |
|---|---|
| rooms per executor | 256 |
| rooms per neighbourhood | 16 |
| calls per DID | 4 |
| mesh size | ≤ 32 |
| empty room closes after | 10 minutes |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/sfu/mod.rs](../../../../rust-executor/src/sfu/mod.rs): module map.
- [rust-executor/src/sfu/service.rs](../../../../rust-executor/src/sfu/service.rs): `SfuService`, limits, `call_join`, `sfu_peer[s]_for_neighbourhood`.
- [rust-executor/src/sfu/server.rs](../../../../rust-executor/src/sfu/server.rs), [room.rs](../../../../rust-executor/src/sfu/room.rs), [simulcast.rs](../../../../rust-executor/src/sfu/simulcast.rs): media loop.
- [rust-executor/src/sfu/config_store.rs](../../../../rust-executor/src/sfu/config_store.rs): `CallPolicy` as a link.
- [rust-executor/src/sfu/cascade.rs](../../../../rust-executor/src/sfu/cascade.rs), [gossip/](../../../../rust-executor/src/sfu/gossip), [relay.rs](../../../../rust-executor/src/sfu/relay.rs), [reachability.rs](../../../../rust-executor/src/sfu/reachability.rs).
- [rust-executor/src/api/sfu_ws.rs](../../../../rust-executor/src/api/sfu_ws.rs): `sfu.*` RPC, `member_did`, `require_owner`.
- [rust-executor/src/sfu/types.rs](../../../../rust-executor/src/sfu/types.rs): `SfuConfig` and wire types.

</details>
