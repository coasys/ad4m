import type { SfuNeighbourhoodApi } from "./SfuManager"
import type { SfuConfig, IceServer } from "./SfuTypes"
import { createSession, type Session, type SessionCreateOptions, type SessionJoinOptions } from "./Session"

/** Creates Session instances bound to a neighbourhood's SFU surface. */
export interface SessionFactory {
    /** Create a session and immediately join with local media. */
    join(roomName: string, localStream: MediaStream, options?: SessionJoinOptions): Promise<Session>
    /** Create a session in idle state. Call session.join() to connect. */
    create(roomName: string, options?: SessionCreateOptions): Session
}

export function createSessionFactory(
    api: SfuNeighbourhoodApi,
    agentDid: string,
    defaultNeighbourhoodUrl: string,
    sfuConfig?: SfuConfig,
    iceServers?: IceServer[],
): SessionFactory {
    return {
        async join(roomName, localStream, options?) {
            const session = createSession({
                api,
                roomId: roomName,
                agentDid,
                neighbourhoodUrl: options?.neighbourhoodUrl ?? defaultNeighbourhoodUrl,
                topology: options?.topology ?? "auto",
                sfuConfig,
                iceServers,
            })
            await session.join(localStream)
            return session
        },

        create(roomName, options?) {
            return createSession({
                api,
                roomId: roomName,
                agentDid,
                neighbourhoodUrl: options?.neighbourhoodUrl ?? defaultNeighbourhoodUrl,
                topology: options?.topology ?? "auto",
                sfuConfig,
                iceServers,
            })
        },
    }
}
