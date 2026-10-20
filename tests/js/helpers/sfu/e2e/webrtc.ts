/**
 * Browser WebRTC for the SDK under Node, from `@roamhq/wrtc`.
 *
 * The SDK builds its connections from the globals a browser provides. The
 * binding lacks three behaviours every current browser has, and the mesh's
 * perfect negotiation relies on all three, so the connection it hands out
 * adds them:
 *
 * - `setLocalDescription()` with no argument creates the offer or answer
 *   the signalling state calls for when it runs;
 * - an offer arriving in `have-local-offer` rolls the local one back;
 * - operations, `createOffer` and `createAnswer` included, run one at a
 *   time on one chain.
 *
 * It refuses a description with a type but no `sdp` (other than a
 * rollback), with the parse error a Chromium client reported for it, so the
 * SDK must create its offers and answers explicitly. That is stricter than
 * the spec, which reads such a description as implicit, as Chrome 151 and
 * Firefox 153 do.
 *
 * It also guards two ways the binding crashes where a browser would throw:
 * an operation on a closed connection, and closing under a running
 * operation. ICE uses host candidates only: every agent here shares a
 * machine, and test hosts may have no route to a STUN server.
 */

import { wrtc } from "./media.js";

/** The peer connections this process holds open. */
const open = new Set<RTCPeerConnection>();

/**
 * Where this process's media goes: for each open connection, the remote
 * address (`ip:port`) of its selected candidate pair, read from WebRTC's own
 * stats.
 */
export async function selectedRemoteAddresses(): Promise<string[]> {
  const addresses: string[] = [];
  for (const pc of open) {
    const stats = (await pc.getStats().catch(() => new Map())) as Map<string, any>;
    const transport = [...stats.values()].find((s) => s.type === "transport");
    const pair =
      stats.get(transport?.selectedCandidatePairId) ??
      [...stats.values()].find((s) => s.type === "candidate-pair" && s.nominated && s.state === "succeeded");
    const remote = pair && stats.get(pair.remoteCandidateId);
    if (remote) addresses.push(`${remote.ip ?? remote.address}:${remote.port}`);
  }
  return addresses;
}

export function specPeerConnection(configuration: RTCConfiguration = {}): RTCPeerConnection {
  const pc = new wrtc.RTCPeerConnection({ ...configuration, iceServers: [] });
  open.add(pc);
  const setLocal = pc.setLocalDescription.bind(pc);
  const createOffer = pc.createOffer.bind(pc);
  const createAnswer = pc.createAnswer.bind(pc);
  const setRemote = pc.setRemoteDescription.bind(pc);
  const addCandidate = pc.addIceCandidate.bind(pc);
  const closeNative = pc.close.bind(pc);
  const restartIce = pc.restartIce.bind(pc);

  let operations: Promise<unknown> = Promise.resolve();
  const chain = <T>(operation: () => Promise<T>): Promise<T> => {
    const result = operations.then(operation);
    operations = result.catch(() => {});
    return result;
  };
  let closed = false;
  const guarded = <A extends unknown[], T>(op: (...args: A) => Promise<T>) => (...args: A): Promise<T> =>
    closed ? Promise.reject(new Error("InvalidStateError: the connection is closed")) : op(...args);

  pc.setLocalDescription = guarded((description?: RTCSessionDescriptionInit) =>
    chain(async () => {
      if (description?.type === "rollback") return setLocal({ type: "rollback" });
      if (description?.type) {
        if (!description.sdp) {
          throw new Error(
            "OperationError: Failed to execute 'setLocalDescription' on 'RTCPeerConnection': " +
              "Failed to parse SessionDescription.  Expect line: v=",
          );
        }
        return setLocal(description);
      }
      return setLocal(pc.signalingState === "have-remote-offer" ? await createAnswer() : await createOffer());
    }));
  pc.createOffer = guarded((options?: RTCOfferOptions) => chain(() => createOffer(options)));
  pc.createAnswer = guarded((options?: RTCAnswerOptions) => chain(() => createAnswer(options)));
  pc.setRemoteDescription = guarded((description: RTCSessionDescriptionInit) =>
    chain(async () => {
      if (description.type === "offer" && pc.signalingState === "have-local-offer") {
        await setLocal({ type: "rollback" });
      }
      return setRemote(description);
    }));
  pc.addIceCandidate = guarded((candidate: RTCIceCandidateInit) => chain(() => addCandidate(candidate)));
  pc.restartIce = () => {
    if (!closed) restartIce();
  };
  pc.close = () => {
    if (closed) return;
    closed = true;
    open.delete(pc);
    void operations.then(closeNative);
  };
  if (process.env.E2E_DEBUG) trace(pc);
  return pc as RTCPeerConnection;
}

/**
 * With `E2E_DEBUG` set: log each description's media sections (with their
 * simulcast rids), each arriving track, and every 5 s the video layers sent.
 */
let traced = 0;

function trace(pc: any): void {
  const who = process.env.E2E_WHO ?? String(process.pid);
  const sections = (sdp?: string) =>
    (sdp ?? "").split(/^m=/m).slice(1).map((s) => {
      const rids = [...s.matchAll(/^a=rid:(\S+) (send|recv)/gm)].map((m) => `${m[1]}/${m[2]}`).join(",");
      return `${s.split(" ")[0]}:${/a=mid:(\S+)/.exec(s)?.[1]}:${/a=(sendrecv|sendonly|recvonly|inactive)/.exec(s)?.[1]}:${/a=msid:(\S+)/.exec(s)?.[1]?.slice(0, 8) ?? "-"}${rids ? `[${rids}]` : ""}`;
    }).join(" ");
  const layers: NodeJS.Timeout = setInterval(async () => {
    if (pc.connectionState === "closed") return clearInterval(layers);
    const sent: string[] = [];
    (await pc.getStats().catch(() => new Map())).forEach((s: any) => {
      if (s.type === "outbound-rtp" && s.kind === "video") sent.push(`${s.rid ?? "-"}:${s.frameWidth ?? "?"}x${s.frameHeight ?? "?"}:${s.framesSent ?? 0}f`);
    });
    if (sent.length) console.log(`[e2e ${who}] video sent ${sent.join(" ")}`);
  }, 5_000);
  layers.unref();
  // Which connection, which session version, which DTLS role: enough to tell
  // a description applied to the wrong offer.
  const id = `pc${(traced += 1)}`;
  const origin = (sdp?: string) =>
    `o=${/^o=\S+ (\S+) (\S+)/m.exec(sdp ?? "")?.slice(1).join("/") ?? "?"} setup=${/a=setup:(\S+)/.exec(sdp ?? "")?.[1] ?? "?"}`;
  const setLocal = pc.setLocalDescription;
  pc.setLocalDescription = async (d?: RTCSessionDescriptionInit) => {
    const result = await setLocal(d);
    const local = pc.localDescription;
    console.log(`[e2e ${who}] ${id} local ${local?.type} ${origin(local?.sdp)}: ${sections(local?.sdp)}`);
    if (process.env.E2E_DEBUG === "stack") console.log(`[e2e ${who}] ${id} caller:${new Error().stack?.split("\n").slice(2, 7).join(" <")}`);
    return result;
  };
  const setRemote = pc.setRemoteDescription;
  pc.setRemoteDescription = async (d: RTCSessionDescriptionInit) => {
    console.log(`[e2e ${who}] ${id} remote ${d.type} ${origin(d.sdp)} in ${pc.signalingState}: ${sections(d.sdp)}`);
    try {
      return await setRemote(d);
    } catch (error) {
      console.log(`[e2e ${who}] ${id} remote ${d.type} refused: ${(error as Error).message}`);
      throw error;
    }
  };
  let handler: any = null;
  pc.addEventListener("track", (e: any) => {
    console.log(`[e2e ${who}] track ${e.track.kind} mid=${e.transceiver?.mid} stream=${e.streams[0]?.id?.slice(0, 8)}`);
    handler?.(e);
  });
  Object.defineProperty(pc, "ontrack", {
    configurable: true,
    get: () => handler,
    set: (fn) => { handler = fn; },
  });
}

/** Give this process the WebRTC globals the SDK expects of a browser. */
export function installWebRtc(): void {
  const g = globalThis as any;
  // A function returning an object works with `new`, which is how the SDK calls it.
  g.RTCPeerConnection = function RTCPeerConnection(configuration?: RTCConfiguration) {
    return specPeerConnection(configuration);
  };
  g.RTCSessionDescription = wrtc.RTCSessionDescription;
  g.RTCIceCandidate = wrtc.RTCIceCandidate;
  g.MediaStream = wrtc.MediaStream;
  g.MediaStreamTrack = wrtc.MediaStreamTrack;
}
