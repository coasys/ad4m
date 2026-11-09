/**
 * Call sessions (`NeighbourhoodProxy.createSession`) driven from Node.
 *
 * The SDK's mesh builds peer connections from the global `RTCPeerConnection`
 * and `MediaStream`. Node has neither, so `installNodeWebRtc` puts
 * `@roamhq/wrtc`'s there. `joinCall` joins with default options — the path an
 * app takes — and records who the session's roster reports, which is what
 * these tests assert on: a participant appears when presence says it is in the
 * call, before any media flows.
 */

import { createRequire } from "node:module";
import type { NeighbourhoodProxy, Session } from "@coasys/ad4m";

const require_ = createRequire(import.meta.url);

export function installNodeWebRtc(): void {
    const g = globalThis as any;
    if (g.RTCPeerConnection && g.MediaStream) return;
    const wrtc = require_("@roamhq/wrtc");
    // Host candidates only: every agent in these tests is on this machine.
    g.RTCPeerConnection = class extends wrtc.RTCPeerConnection {
        constructor(configuration: RTCConfiguration = {}) {
            super({ ...configuration, iceServers: [] });
        }
    };
    g.MediaStream = wrtc.MediaStream;
}

export interface JoinedCall {
    session: Session;
    /** DIDs currently in this session's roster. */
    roster: Set<string>;
    /** DIDs this session saw leave. */
    left: Set<string>;
}

export async function joinCall(proxy: NeighbourhoodProxy, room: string): Promise<JoinedCall> {
    installNodeWebRtc();
    const session = await proxy.createSession(room);
    const roster = new Set<string>();
    const left = new Set<string>();
    session.on("participant-joined", (p: { did: string }) => { roster.add(p.did); left.delete(p.did); });
    session.on("participant-left", (p: { did: string }) => { roster.delete(p.did); left.add(p.did); });
    await session.join(new (globalThis as any).MediaStream());
    return { session, roster, left };
}
