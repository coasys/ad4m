# Presence and signals

**Telepresence** is the ephemeral side of a neighbourhood: it is not stored as links and does not sync. An agent sets an **online status** (a signed perspective expression) and reads the **online agents** of a neighbourhood. It sends a **signal** to one agent or a **broadcast** to all. The executor passes these through the link language's telepresence adapter. To agents on the same executor it delivers them itself.

**Why:** Online status, typing indicators and call signalling change too often and matter too briefly to store as links. The link language's adapter keeps one transport per neighbourhood. Co-owners on one executor share one link-language isolate. That isolate does not report its own executor's agents and holds only one status per executor, so the executor delivers locally and bundles its agents' statuses.

**Parent:** [Neighbourhoods](README.md)

<details><summary>How it works</summary>

| RPC | Behaviour |
|---|---|
| `neighbourhood.setOnlineStatus` | sign the status as the caller; keep it per agent in `hosted_status`; publish one **bundle** with the signed statuses of all active managed agents through the link language |
| `neighbourhood.onlineAgents` | the link language's online agents, by the link language's own rule (bundles unpacked, each bundled status kept only if its signature verifies), plus co-owners online by `last_seen` (`presence::is_online`: less than 600 s old, `MANAGED_USER_ONLINE_WINDOW_S`); an authenticated API call writes `last_seen` at most once per 200 s (`LAST_SEEN_WRITE_THROTTLE_S`). It waits at most 2 s for the link language (`ONLINE_AGENTS_LANGUAGE_TIMEOUT`); a timeout or a failed call is an error, because a roster of the co-owners alone would drop every remote peer |
| `neighbourhood.ownOnlineStatus` | the caller's last status on this executor |
| `neighbourhood.otherAgents` | the link language's `others()` plus all `owners`, without the caller; needs neighbourhood READ on a perspective the caller reaches (`Owns { Perspective }`, as every method here) |
| `neighbourhood.sendSignal` | signed payload; if the perspective is shared and the recipient is one of its owners (the main agent for an unowned perspective), publish locally; else send through the link language |
| `neighbourhood.sendBroadcast` | publish locally to each owner (the main agent for an unowned perspective), the sender's own agent included, then answer; the send through the link language runs after that, in order, from a per-perspective queue of at most 8 (`MAX_PENDING_BROADCASTS`; a full queue fails the RPC; a newer call presence beat of the same sender, device and call replaces a queued one), and is skipped when every member is verified local; with `loopback` it is also published to every owner as if it came from the link language |
| `neighbourhood.hasTelepresence` | whether the link language has a telepresence adapter (`has_telepresence_adapter`); `false` before the link language loads |

Payloads: with `signed: true` (default) the caller sends link expressions and the executor signs the perspective around them. With `signed: false` the caller sends bare links; the executor signs each link and then the perspective (`build_signed_perspective`).

`hosted_status` is a process-wide map in memory. After a restart, it is empty until each agent sets a status again.

Inbound: the link language calls `handle_telepresence_signal_from_link_language`, which marks each signature valid or invalid (`verify_signatures`) and publishes a `signal` event to the recipient, or one copy to each owner when there is none (`publish_telepresence_signal`). A recipient that is not an owner gets nothing. A cached language → perspective mapping keeps this path fast.

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `set_online_status`, `online_agents`, `online_status_of`, `others`, `has_telepresence_adapter`, `send_signal`, `send_broadcast`, `telepresence_signal_from_link_language`, `ONLINE_AGENTS_LANGUAGE_TIMEOUT`; [perspective_instance/language_broadcasts.rs](../../../rust-executor/src/perspectives/perspective_instance/language_broadcasts.rs): `broadcast_remotely`, `MAX_PENDING_BROADCASTS`.
- [presence.rs](../../../rust-executor/src/presence.rs): `is_online`, `MANAGED_USER_ONLINE_WINDOW_S`, `LAST_SEEN_WRITE_THROTTLE_S`.
- [perspectives/hosted_status.rs](../../../rust-executor/src/perspectives/hosted_status.rs): per-agent statuses, `to_publish` (bundle), `unpack`.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `handle_telepresence_signal_from_link_language`, `publish_telepresence_signal`.
- [api/neighbourhoods_ws.rs](../../../rust-executor/src/api/neighbourhoods_ws.rs): handlers, `build_signed_perspective`.
- SDK: telepresence section of [core/src/neighbourhood/NeighbourhoodProxy.ts](../../../core/src/neighbourhood/NeighbourhoodProxy.ts).

</details>
