# SFU wind tunnel — agent guide

Run commands from `tests/js`. Port conflicts and rebuild requirements: `../../AGENTS.md`.

| Suite | Languages | CI |
|---|---|---|
| `test-sfu-e2e`: single executor, end to end (about 4 min) | local, no Holochain: `pnpm run prepare-test-local` (`test-main-sfu-e2e` does both) | every push, `integration-tests-sfu-e2e` |
| `test-sfu-browsers`: SFU and mesh calls in real Chromium, Firefox and WebKit, mixed (about 1 min past the world's start) | local, no Holochain: `pnpm run prepare-test-local` (`test-main-sfu-browsers` does both) | nightly, `integration-tests-sfu-browsers` |
| `test-sfu-capacity`: the executor's CPU, forwarded packets and media-loop passes per second for calls of 3, 8 and 16 participants in Chromium (under 2 min after the prepare step) | local, no Holochain: `pnpm run prepare-test-local` (`test-main-sfu-capacity` does both) | nightly, `integration-tests-sfu-capacity` |
| `test-sfu`: `sfu-cascade`, `sfu-session` (T20, T21), `sfu-mesh` (network baselines) | Holochain: `pnpm run prepare-test` | nightly, `integration-tests-sfu` |

- **Browser suite (`tests/js/tests/sfu-browsers.test.ts`, helpers in
  `tests/js/helpers/sfu/browser/`).** Each user calls from its own
  Playwright browser context, loading `core/lib/index.js` from a loopback
  page server, and joins through `createSession` as an app does. Per
  topology: Chromium with each browser, a Chromium + Firefox + WebKit call,
  and a Firefox tab lost mid-call that rejoins. Asserts come from getStats
  (packets and decoded frames growing), a Web Audio analyser (audible) and a
  canvas (not black; headless WebKit never plays a `<video>`, so not there).
  On the SFU, Chromium + Firefox also asserts the payload types differ per
  leg and each receiver gets its own numbers (#1684).
- **Capacity measurement (`tests/js/tests/sfu-capacity.test.ts`).** Not a
  pass/fail suite: it fails only when a call does not converge, a tab stops
  receiving in the window or the counters do not move. Per call size, once every
  tab receives every other, it reads `sfu.status` and the executor's `/proc`
  CPU time before and after `SFU_CAP_STEADY_S` (30) seconds and records, per
  size (`SFU_CAP_SIZES`, default `3,8,16`; camera `SFU_CAP_VIDEO`,
  `320x240`): `sfu_capacity.cpu_percent` (of one core), `packets_per_s`
  (RTP packets written to receivers), `loop_iterations_per_s` (passes of
  `SfuServer::event_loop`) and `cpu_ms_per_kpacket`. The counters are
  `MediaCounters` in `rust-executor/src/sfu/server.rs`, reported as
  `loopIterations` and `packetsForwarded` by `sfu.status`: monotonic since the
  SFU started, so a reader divides a difference by the time. The window runs on
  one host with the browsers, so the CPU figure holds the executor only
  (`/proc/<pid>/stat`), not the tabs. 16 tabs need two Chromium processes: one
  headless Chromium starts at most 16 fake microphones, the test puts 8 tabs to
  a process. Set `PERF_RESULTS_DIR` to get `sfu-capacity.json`.
- The page keeps one muted `<video>` per session (`participantId`), as an
  app keeps one tile per participant, and removes it when the session
  leaves the list. Never one per agent DID: an agent listed twice (two
  devices, or a lost tab beside its successor) then swaps two streams on
  one element at every render, and headless WebKit's tab hangs or crashes
  (`Target crashed`). A tab that crashes or does not report within 30 s
  fails with its user, browser and last console errors.
- On a host with no audio server (CI's runner: no PulseAudio or PipeWire),
  Firefox keeps every `AudioContext` `suspended` (`currentTime` 0) and its
  analyser reads silence, though the audio arrives and decodes. No Firefox
  pref fixes it (`media.cubeb.force_null_context` makes any host silent;
  `media.cubeb.backend` does not help). So where the page's context does not
  run, `audioPeak` is the receiver's `inbound-rtp` `audioLevel` instead
  (`audioMeter: "rtp-stats"`, shown in the failure). To reproduce on a
  desktop, run the suite with `XDG_RUNTIME_DIR` set to an empty directory
  and `PULSE_SERVER=unix:/nonexistent PIPEWIRE_REMOTE=/nonexistent`.
- The suite runs the executor's default SFU binding: loopback plus the
  LAN address, both offered. Firefox needs the LAN one: it gathers no
  loopback candidate and never pairs its LAN candidates with an SFU on
  127.0.0.1 only (ICE stays `checking`, the SFU's DTLS times out), while
  Chromium and WebKit connect. `SFU_BROWSERS_SFU_ADDR` binds the SFU there
  instead (`AD4M_SFU_BIND_ADDR`); `127.0.0.1` shows the loopback case.
- Install the browsers with `pnpm exec playwright-core install --with-deps
  chromium firefox webkit` in `tests/js`. On an Ubuntu release Playwright has
  no builds for (CI's runner is 25.04), it falls back to the 24.04 builds,
  whose WebKit needs ICU 74: set `PLAYWRIGHT_HOST_PLATFORM_OVERRIDE` to the
  build with the host's library versions (`debian13-x64` on 25.04), with its
  own `PLAYWRIGHT_BROWSERS_PATH`. A browser that cannot start makes
  its tests pending with the reason; `SFU_BROWSERS_REQUIRED=1` (CI) fails
  instead. Without root for WebKit's system libraries, extract the missing
  packages (`apt-get download`, `dpkg -x`) and list the `.so` files,
  dependencies first, in `WEBKIT_LD_PRELOAD`. `SFU_BROWSERS_DEBUG=1` dumps
  every tab's SDPs when a wait times out.
- Each test hangs up (`session.leave()`) before closing its tabs: a tab
  closed mid-call keeps its SFU slot until the server drops it, and a DID
  is in at most 4 calls per node.

- **E2E worlds use one executor without Holochain.** Users of one executor
  share a neighbourhood's perspective as its owners, so links, the call
  policy, presence and signals stay on the executor; the local link language
  (a fresh `uid` per neighbourhood) only gives each neighbourhood its own
  address. The world refuses to start on Holochain-prepared languages.
- **E2E scenarios (`tests/js/helpers/sfu/e2e/`) call only through the SDK.**
  Each participant is a managed user in its own Node process
  (`participant-child.ts`) running `createSession` on `@roamhq/wrtc`, as WE's
  adapter does. Test code must not use raw call RPCs, `ensureMembership` or
  `WebRtcPeer`; the first test in `sfu-e2e.test.ts` fails if it does (E11's
  outsider is the one exemption). The admin client only builds the world.
  One exception: a `WorldSpec` user with `main: true` is the executor's main
  agent, called with the operator credential, still only through the SDK.
  E23 uses it, since only the main agent's own devices show how the executor
  routes the main agent's broadcasts.
- Each user sends its own tone (`toneFor(i)`) and a video of known size, so
  receivers check *who* they hear and at which simulcast layer.
- E6 shapes the loopback with `tc netem`, only for its world's ports
  (`setNetemOnPorts`: executor API and SFU UDP), and skips without
  passwordless `sudo -n tc`. Never impair the whole of `lo` in a PR job:
  machine runners on one host share it, and every other job's localhost
  RPC times out (seen as `executeCommands`/`otherAgents` 30 s timeouts).
- E14 runs the SFU behind a simulated NAT: the executor binds 127.0.0.1 on a
  fixed port and advertises 127.0.0.2 (`executorEnv` in the world spec);
  `reachOnlyThrough` DNATs 127.0.0.2:P to 127.0.0.1:P and drops direct UDP
  to 127.0.0.1:P, for that port only. It asserts each caller's selected
  candidate pair (`Snapshot.selectedRemotes`, from WebRTC stats) is the
  advertised address. Skips without passwordless `sudo -n iptables -t nat`.
- E17 switches one participant's network: `socketPorts(pid)` reads the
  participant process's TCP and UDP sockets from `/proc`, and the lease
  drops all their traffic (netem 100 % loss, matched by protocol and port)
  while sockets opened later work. Skips without passwordless `sudo -n tc`.
  E18 makes the same cut, then fires `online` in the participant process
  (`Participant.networkChanged`). Both assert the call came back in place:
  u2 saw no departures, so no rebuild.
- `@roamhq/wrtc` fires `iceconnectionstatechange` when libwebrtc's legacy
  ICE state changes, but `iceConnectionState` returns the standardized
  state, which libwebrtc changes just after. On a loaded host a listener
  reads the old state, and no event comes for the new one: E17's cut read
  "completed", the SDK did not restart ICE, and the call stayed silent.
  `specPeerConnection` (`e2e/webrtc.ts`) looks at the state again 10 ms,
  100 ms and 1 s after each event and fires one more event when it changed.
  A browser fires the event for the state it reports. Do not take this out,
  and do not read a state change from the binding's event alone.
- A participant process killed by a signal nobody sent (the libwebrtc binding
  crashes rarely while closing) reruns the scenario once; an assertion failure
  never does.
- Set `E2E_DEBUG=1` to trace each description (connection, SDP version,
  DTLS role, refusals), tracks, sent simulcast layers, peer states and
  recoveries per participant, and world phase timings. `E2E_DEBUG=stack`
  adds the caller of each local description.

SFU behaviour the scenarios pin: one session per device. `sfu.callJoin`
names a `deviceId`; a join from the same DID and device (another tab)
replaces the earlier session, which gets `sfu-session-replaced` and stops
without rejoining, even mid-reconnect (E20); another device joins beside it as a participant of
its own (E22; at most `MAX_DEVICES_PER_DID` = 4 per room). Every call about
a session (`callLeave`, `callAnswerServerOffer`, `addIceCandidate`,
`callSetQualityPreference`, `callRestartIce`, `sendData`) names the `participantId` its join
returned, and acts only on that session of the caller's DID; the SDK sends
none of them without one, so a tab that never joined cannot end another
tab's call (E19, E21). The executor publishes `sfu-room-roster` (to the
listed DIDs, plus the departed session's DID on departure) after every join
and departure, listing sessions (`participantId` + `agentDid`) and a
`revision` (one sequence for every roster this process publishes: 1 the
first time, then one more on every publish, a resume included; a later
room, and a room created again, continues it, so a roster from an earlier
life of the room is older), so a client can drop a roster whose
`revision` is older than one it already applied. The SDK
keys participants by participant id from it; when the roster stops listing
this session (its `participantId`) for 3 s and `sfu.callResume` does not list
it either, the SDK rebuilds the call, but a replaced session never does; tracks are attributed only by the `trackMapping`
mids (which name
the participant id); a session never receives its own media or its own
`sfu-data` (`senderParticipantId`), but does receive its agent's other
device; room capacity comes from the
space's policy (`maxMeshParticipants × 4`, 128 without one), and cascade
per-node capacity only drives redirects; the server drops a peer after
10 s of ICE `Disconnected`, or, once the client asked for an ICE restart
(`sfu.callRestartIce`), 15 s after the request. The restart's answer ends
that grace and the 10 s disconnect clock starts again. An unanswered server
offer is resent at 10 s and 30 s, then every 30 s, and never drops a client
peer (#1802 dropped it at 70 s, also while its media flowed). A client keeps
that offer. An established pipe whose answer str0m rejects locally, or
whose offer is still unanswered after those two resends, is disconnected.
That rejection is local: a pipe answer carries no refusal, and a node that
rejects an incoming offer only logs it, so the sender waits until the offer
goes unanswered. The other end drops the pipe when its ICE fails. The end
that dials (the lexically higher DID) redials at once the first time,
whether it ended the offer or only saw the drop. The next consecutive
failure waits 30 s, then 60 s, then 120 s, and stays at 120 s. A pipe that
stayed connected for 120 s clears that count, so the next drop is free
again. A timeout, a pipe that never connected, does not wait. When the new
pipe is usable it is offered the origins already in the room, so media resumes
without a new track. An Announce arrives every 10 s, so the announce alone
would dial again on the next heartbeat. A crossed offer (`ChangesOutOfOrder`)
keeps the pipe and queues the tracks again. An initial pipe that never connects stays
through the second resend plus the 15 s ICE-restart grace, then is dropped.
The offer carries
an `offerId` that a resend and `sfu.callResume` repeat; the answer echoes
it. `ParticipantId` (`P{n}`) and that offer id (`o{n}`) are minted from a
counter that starts, once per process, at a random value at least `2^63`,
so a restarted process does not hand a client's old id to a new session. `sfu.callAnswerServerOffer` answers `{ accepted: true }`, or
`{ accepted: false, reason }` (`stale`, `no-pending`, `unknown-session`),
after the event loop applies the answer. A repeat of the id accepted last
for that session is `{ accepted: true }` and changes nothing; an older
accepted id is `stale`. `stale` also covers an answer that does not parse,
or whose mids are not the pending offer's, under the pending offer's own
id: that offer stays pending. A refused answer is that value, not an RPC
error. The RPC errors are 403 (a session of another agent) and 408 (the
event loop did not answer, or its command channel is closed).
`sfu.callResume` (the session's
`participantId`) sends the pending offer and the roster again at once, for
a client whose socket came back, and answers `{ listed: true, offer?,
roster: { revision, participants } }` with the same offer, the same sessions,
and the same `revision` as the roster event that resume just published, or
`{ listed: false }` when the room or the session is not there. A session of
another agent is 403. `sfu.callAnswerServerOffer` and `sfu.callResume` are
408 when they have not finished within 5 s. That wait starts before the
rooms lock, so a join that holds the lock counts, and it covers the resume
roster publish as well as the command waiting for a slot in the bounded queue.
on ICE `disconnected` or a network change the SDK restarts ICE in place (the
SFU stays the only offerer: the restart comes as a server renegotiation
offer), and rebuilds the call if that has not reconnected within 15 s; after
a socket reconnect with no network change it restarts ICE only if ICE is not
connected 2 s later; a failed connection is rebuilt at once, a rebuild
retries the join for 120 s, and either terminal give-up — the 120 s spent
without a rejoin, or a 4th rebuild inside 2 min — ends the call, in this
order: `participant-left` for each participant, then `error`, then the
connection closes, the `sfu.callLeave` request starts (the end does not wait
for its reply) and `call-ended` (`{ reason: "gave-up" }`, also on the
`Session`, which goes idle) is emitted for the UI to show; a new `join`
starts over. A rebuild keeps the participants and clears only their media:
it refreshes `hasAudio` and `hasVideo`, then emits one `stream-removed` per
track, and a track that ends later does not emit a second one. Only an SDP the connection
cannot apply rebuilds from a renegotiation: the answer names its offer's
`offerId`, and the server refuses one naming another — `stale` or
`no-pending` (a settled or superseded offer, or the server's own SDP step
refusing it, as `ChangesOutOfOrder`) is non-acceptance: repair is left to
media and session evidence, not a rebuild; `unknown-session` rebuilds the
call. The client keeps its last answer (`lastAnswer`) and resends
nothing by itself: an answer a socket fault lost (503, 408) goes out
again from the cache when the executor resends its offer or the resume
reply carries it. Once the socket is back, `sfu.callResume` returns what went by — the
session's pending offer and the room's roster, applied like the events
that carry them; a resume the socket fails keeps the call, and a 403 ends
it (`call-ended`, `{ reason: "refused" }`: the executor refused the
resume — the session is another agent's, or this agent is no longer a
member). Of the server offers waiting, only the newest
is applied; no RPC runs on the offer queue (an answer is sent without
waiting for its reply) and SDP steps run to their end; a resume's ask
waits off the queue, bounded by `RESUME_STEP_TIMEOUT_MS` (10 s) and
by the restart's remaining window when one is in flight, so the restart
offer it triggers is answered while the reply is still out. The restart names the session's `participantId` (only that session
restarts, never another device of the agent), and a manager with no session
(join not back, left, replaced) sends none. A room roster that leaves this
session out for 3 s asks `sfu.callResume` and rebuilds the call when its
reply says the session is not listed — an ask the socket fails is asked
again within the grace while the roster still omits it; a roster that
lists it, as during a restart in place, changes nothing.
`ApiClient` drops an executor socket whose ping stays unanswered for a ping
interval (10 s, `PING_INTERVAL_MS`; longer while a large write it sent may
still be crossing a 16 kB/s link) and dials a new one, so a
socket a network switch left OPEN is noticed in 10 to 20 s. `onNetworkChange`
fires only for platform network events: `online` redials at once and tells
it once the new socket is open, and `change` on `navigator.connection`
marks a change — told once an open socket is proven — only when its
`type` or `navigator.onLine` moved: the local path changed, so media must
restart. On a platform that reports no `type` (desktop Chromium) a `change`
with the reported state unmoved only has the socket judged — a real
switch kills it and its drop redials; a moved estimate keeps it and marks
nothing. The socket is judged by socket evidence through
`checkConnection`, which also runs when a request
has had no reply for 4 s (`PENDING_PROBE_MS`) on a socket nothing
arrived on since it was sent. A request that cannot wait that long goes
out urgent (`CallOptions.urgent`; `sfu.callRestartIce`, `sfu.callResume`
and `sfu.callAnswerServerOffer`): its wait is 1 s
(`URGENT_PENDING_PROBE_MS`) and the probe it starts judges a silent
socket in 3 s (`URGENT_CHECK_CONNECTION_DEADLINE_MS`, drain allowance
kept) instead of 8 s — it shortens only that request's timing, and a probe
already running gets its verdict moved earlier to that wait, never later.
The probe sends no ping when the socket
delivered a message in the last 5 s and drops a socket that answered
nothing for 8 s after one probe (at least 8 s: while a large write it sent
may still be crossing the link, the wait grows by that write's drain time);
any frame proves the socket, so a slow
request on a socket that keeps delivering fails as an RPC timeout, not a
dead socket. A probe that gives up only redials, at once and without
backoff — socket evidence is not a network change, so no `onNetworkChange`
fires for it; `onReconnect` on the new socket is its signal, and the SFU
answers that with `socketBack` (resend what a fault kept, `sfu.callResume`,
`checkIceSoon`), not an immediate ICE restart. The call layer never probes
the socket: ICE `disconnected` is media evidence, and its whole answer is
the ICE restart.

SFU simulcast layer choice (`rust-executor/src/sfu/simulcast.rs`). Each client
peer (not pipes) runs str0m's bandwidth estimator, started at 800 kbit/s
(`BWE_START_KBPS`) and fed by the caller's transport-wide-cc reports; REMB is
ignored. E is the receiver's estimate, C the cost of the video forwarded to
it, F its fixed traffic. Constants (measured with the e2e source below):

- `RATE_WINDOW` 2 s: a layer's cost is its bytes over this window, payload
  plus 32 bytes per RTP packet (`seq_range`), keyframes included (cadence,
  loss recovery, PLIs and the selector's switches all cross the link). A rate
  is known once the layer has flowed a whole window; until then it costs 0,
  so a resumed high stream can stay high through an estimate drop for up to
  2 s. 1 s read the high layer up to 1.7 times nominal at p99.
- `LAYER_TIMEOUT` 2 s: a layer silent this long is gone, noticed on the
  track's next frame. Senders pause up to 1.6 s when an encoder starts.
- `KEYFRAME_REQUEST_INTERVAL` 500 ms: at most one request per (track, layer),
  from the selector or a gap. A receiver's own PLI is not limited.
- F: per audio track the larger of `AUDIO_RESERVE_BPS` 64 kbit/s and its
  measured rate, plus each video without simulcast; it is never touched.
- `UPGRADE_HEADROOM` H = 1.25, `UPGRADE_HOLD` 4 s: the band between the
  step-down line (C + F > E) and the upgrade line (E >= H * (C + F + step)).
  The step-down line is the traffic itself: with no bottleneck, str0m's
  estimate settles near the traffic it carries (about 1.5 times at full
  state), and a line below the estimate (the earlier 0.85 * E) stepped
  receivers down after almost every upgrade.

Rules, per receiver, run on an estimate event, a track-set or ceiling change,
a committed switch, a change in a track's set of live layers (a layer starts,
or stops after `LAYER_TIMEOUT`), a layer or video that resumes after
`LAYER_TIMEOUT`, or a deadline. There is no periodic tick: the earliest
deadline (the hold's end, or the moment a live layer's rate is first measured)
lowers the loop's timeout.

- When C + F > E, one video track drops one live layer (largest saving first,
  ties by stable id), one per run.
- Upgrades: the receiver has one hold (`hold_since`). It runs while the
  cheapest step fits E >= H * (C + F + step), is re-checked at every run, and
  a dip, ceiling change, track-set change or completed switch resets it. When
  it ends, every step the headroom covers is granted in one run, cheapest
  first, one layer per track, each checked with the steps granted before it;
  then the hold starts again. Upgrades wait while the target layer, a
  forwarded video or an audio track has no measured rate (its first packet +
  `RATE_WINDOW` wakes the allocation), while a switch is pending, before the
  first estimate and while the last one has `can_probe: false`. A peer that
  never reports stays on the lowest layer.
- A switch commits on a keyframe of the new layer. The old layer's cost counts
  until then, and a track with a pending switch is not switched again.
- New tracks start on the lowest live layer, which is also the floor (video is
  never paused).
- When a switch commits on a layer below the receiver's cap (the layer it
  wanted stopped), the cap drops to that layer: a layer that returns is
  measured again (2 s) and earns its upgrade again (4 s hold, `can_probe`).
- `SetQualityPreference` is a ceiling, not a choice: `high`, `auto` and unset
  let the estimate decide up to high. The estimate and the hold choose the
  layer at or below the ceiling; with no live layer there, the receiver gets
  the lowest live one. A lowered ceiling applies on the next keyframe. For a peer the loop no longer holds it is dropped (ids
  are never reused).
- The receiver's probing target (`bwe().set_desired_bitrate`) is the estimate
  that satisfies the upgrade condition at the permitted maximum layers:
  `H * (highest + fixed)`, rounded up (fixed: audio and non-simulcast
  video; highest: per track the most a layer up to its permitted top costs,
  as a middle layer can measure above the top one).
- A cap change is logged at `debug` (`rust_executor::sfu::simulcast`:
  receiver, track, old and new layer, estimate, fixed, spent, layer rates).

The e2e video source (`videoSource`, `tests/js/helpers/sfu/e2e/media.ts`) is
a drifting gradient with soft moving discs and light noise: it compresses like
camera video, so each layer runs near its `maxBitrate` at a steady frame rate
and keyframes stay small. Hard-edged moving patterns make VP8 send huge
keyframes and then drop frames for seconds, which reads as layer stalls and
rate spikes. The e2e shaper (`limitDownlink`, `tests/js/helpers/sfu/net.ts`)
is a `tbf` with a 3 kB burst and a 300 ms queue: a larger burst lets str0m's
probe clusters through at line rate (the estimate read up to 4.7 Mbit/s on a
700 kbit/s cap), a smaller queue drops so much that the estimate collapses.

SFU media addresses (`rust-executor/src/sfu/{hosts,server,reachability}.rs`):
with no `sfu_bind_addr` (the default, whatever `--localhost` says) the SFU
binds two sockets on one port: 127.0.0.1 and the LAN address
(`detect_outbound_ip`, the default route's interface), and every answer
offers both as host candidates. Firefox needs the LAN one (it gathers no
loopback candidate); other machines on the LAN reach it too. In localhost
mode the plain RPC port stays on loopback: the SFU adds only its media UDP
port on the LAN, guarded by ICE credentials and DTLS (a `tls_config`
listener binds `0.0.0.0` on its own). The LAN address is detected again
for every answer and pipe: after a move to another network the next call
binds the new address on the same port (old sockets stay, at most 8, in case
the address comes back). A call in progress keeps the addresses it was
offered: a LAN pair whose address went away fails its ICE restart in place
and the SDK rebuilds the call, which gets the new address; a pair over
loopback carries on. No LAN (offline): answers offer loopback only, logged,
and each call looks again. One socket per address, never `0.0.0.0`: str0m
matches inbound STUN to a host candidate by destination, which a socket
bound to one address knows (the event loop passes the receiving socket's
address), and replies leave from the address the caller sent to. An
explicit `sfu_bind_addr` binds that address only, as before (malformed or
unspecified: loopback).

SFU behind NAT: the SFU binds on `sfu_udp_port` (0/unset: ephemeral; a fixed
port that is taken, on loopback or the LAN address, stops startup), runs the
STUN probe on the LAN (or explicit) socket before the event loop owns it,
and `assess` decides the advertised address (`sfu_public_addr`, else the
probe's reflexive IP on the fixed port) and the reachability (`public` once
STUN sees the advertised IP). Answers carry the host candidates and, when
advertised, a srflx candidate whose base is the LAN (or explicit) socket.
Cascade pipes offer the host candidates only. The public address is read at
startup: an IP change needs a restart. The four `sfu_*` keys live in the
config file too; a build without them refuses them as unknown keys.

Mesh signalling rules the scenarios pin (`core/src/neighbourhood/MeshManager.ts`):
offers are numbered per connection and answers name theirs, so a late answer
or an overtaken offer is dropped; `onnegotiationneeded` only ever offers and
the answering path only ever answers (an implicit description picked from the
state swaps them during a rollback); the answerer holds its own first offer
until the pair connects; `join` waits for the channel to listen before the
session announces presence; the buffer for not-yet-listed peers never drops a
description.

Mesh presence (`core/src/neighbourhood/Session.ts`): each device in a mesh
call puts two links into its agent's one online status, once, at join: a
presence link (source `did#device`, `ad4m://session/in-call`, target the
room) and a marker (same source, `ad4m://session/in-call-beat`, target the
room) that says the device beats. The beat itself is never persisted: every
20 s, and at once on join, the device sends a `beat` message on the call's
telepresence channel (`{v, call, to: "*", kind: "beat"}` through
`MeshManager.beat`, so it is counted like any send), on its own timer, so a
roster poll that hangs cannot stop it. Never write liveness into the online
status: p-diff-sync keeps every `set_online_status` as a permanent chain
entry and `get_online_status` scans them all. A receiver ties a beat to the
link author the executor verified plus the device in the payload, so an
agent can vouch only for its own devices, and a beat never makes a device: a
device is in the call only through its own agent's presence link. A reader
records, on its own clock, when it last heard each device with the marker
(or first saw it, if it never heard it) and leaves it out after 90 s with no
beat; a new beat brings it back at the next poll (3 s). A poll that finds
the reader's pair with the device connected counts as hearing it, so a call
whose signals fail while media flows keeps its peers. The executor routes a
broadcast to every agent it hosts in the neighbourhood, the sender's agent
too (main or managed), so two devices of one agent on one executor hear each
other; each device drops its own messages. So a device that
died without leaving (a crash, a closed browser, a lost network) leaves every
roster within about 90 s + a poll, even while another device of its agent
keeps the status alive, a live device survives three lost beats in a row,
and a reader that joins late keeps a device it has never heard for up to
90 s. A device with no marker (an older SDK, which drops the unknown `beat`
kind without a word) never goes stale. Status writes are join, leave, and
one repair when a poll finds a stale device of this agent (its links go) or
this device's presence link or marker missing (a sibling took them out while it missed
beats; they go back), at most once per 90 s; nothing is written over a
status that reads as none. The beat stops on leave, destroy and `replaced`.
A tab reopened in the same browser profile keeps its device id
(`localStorage`) and simply takes its links over. `sfu-browsers.test.ts` pins
both: the same-profile rejoin lists the user once, and a dead second device
leaves everyone's list within `GHOST_GONE_MS`.

