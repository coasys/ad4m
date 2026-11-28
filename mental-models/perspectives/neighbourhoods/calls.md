# Neighbourhood calls

A **call** is an audio and video connection among members of a neighbourhood, opened by an app with `NeighbourhoodProxy.createSession(roomName)`. A call runs in one of two **call topologies**. In **mesh**, every participant connects to every other over WebRTC, signalled through neighbourhood signals. In **SFU**, participants connect to the executor's selective forwarding unit. The mesh roster comes from online status, not from join messages.

**Why:** A small call needs no server: mesh uses only the neighbourhood's telepresence, which every link language with a telepresence adapter provides. Mesh cost grows with every participant's upload, so larger rooms use the SFU in the executor. A crashed peer never sends a "leave" message. With online status as the roster, it drops out when the link language stops reporting it online, or when its beats stop.

**Parent:** [Neighbourhoods](README.md)

<details><summary>How it works</summary>

1. `createSession(roomName, { topology: "mesh" | "sfu" | "auto", sfuConfig? })` builds a call (SDK class `Session`, not the auth session). The **call topology** is the client's choice for one call:

   | `topology` | Result |
   |---|---|
   | `mesh` / `sfu` | that topology |
   | `auto` with an `sfuConfig` | decided by the config's mode |
   | `auto` without one | SFU only when the executor's SFU is public and `sfu.servesAllMembers` says every member is hosted on this executor; else mesh |

   The neighbourhood's **call policy** (`SfuConfig`, stored as a link) is a second setting. It names the mode (`mesh`, `designated`, `gateway`, `cascaded`) and the mesh size limit, and so which SFUs serve the neighbourhood. An SFU join with `acceptRedirect` may then move to the least-loaded SFU of a cascade ([SFU](../../agents/executor/services/sfu.md)).
2. On join, each device adds a link `<mesh peer id> ad4m://session/in-call <roomId>` to its agent's online status ([Presence and signals](presence-and-signals.md)). A mesh peer id (`meshPeerId`) names an agent and a device. In mesh, the device also adds a marker link `ad4m://session/in-call-beat` and sends a `beat` signal every 20 s; beats are never stored.
3. **Mesh** (`MeshManager`): every 3 s the session polls `onlineAgents()` ([NeighbourhoodClient.ts](../../../core/src/neighbourhood/NeighbourhoodClient.ts)) for other agents and `ownOnlineStatus()` for this agent's other devices; devices whose link names the room form the roster. A device with the marker that this session has not heard beat for 90 s leaves the roster, and the session takes its own stale devices out of its status. `MeshManager.setRoster` connects to roster members that appear and drops those that disappear. SDP and ICE travel as neighbourhood signals (`createTelepresenceChannel`). The mesh code is a port of an older app's call module and keeps its wire format (header of [MeshManager.ts](../../../core/src/neighbourhood/MeshManager.ts)).
4. **SFU** (`SfuManager`): negotiate with the executor's SFU over `sfu.*` RPC (`sfu.callJoin`, `sfu.callAnswerServerOffer`, `sfu.addIceCandidate`, `sfu.callResume`, `sfu.callLeave`). These handlers admit only the neighbourhood's members on this executor (`member_did`). When the room's `sfu-room-roster` leaves this session out for 3 s (`noticeRoster`), the session asks `sfu.callResume`; it rebuilds the call when the reply says it is not listed, or when its connection fails. The SFU and cascading between executors are in [SFU](../../agents/executor/services/sfu.md).

</details>

<details><summary>Where in the code</summary>

- SDK: [core/src/neighbourhood/Session.ts](../../../core/src/neighbourhood/Session.ts) (`createSession`, `resolveTopology`, call-presence predicate), [MeshManager.ts](../../../core/src/neighbourhood/MeshManager.ts), [SfuManager.ts](../../../core/src/neighbourhood/SfuManager.ts), [SfuTypes.ts](../../../core/src/neighbourhood/SfuTypes.ts), `createSession` in [NeighbourhoodProxy.ts](../../../core/src/neighbourhood/NeighbourhoodProxy.ts).
- Executor: [api/sfu_ws.rs](../../../rust-executor/src/api/sfu_ws.rs) (`sfu.*` RPC, `member_did`, `require_owner`), [sfu/](../../../rust-executor/src/sfu).

</details>
