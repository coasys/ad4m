import type { FastifyBaseLogger } from "fastify";
import type { AuthManager } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import { endKeyInRoom } from "./room-events.js";
import { StoreUnavailable, type StoreClient } from "./store-client.js";
import type { WsManager } from "./ws.js";

export interface RefreshContext {
  db: LinkServerDB;
  auth: AuthManager;
  ws: WsManager;
  store: StoreClient;
  log: FastifyBaseLogger;
}

/**
 * Asks the store again whom each key speaks for: every key that authenticated to a room and is
 * not revoked there (with or without a socket or a live session: such a key is still one a room
 * key is sealed to), and every key with an open socket. A key its log no longer lists as a
 * standing device key (revoked, expired) is revoked in every room: its sessions and sockets end
 * (4005), and each room it was in hears `rotate-needed` (`revoked`), so the next room key is not
 * sealed to it. A key the store cannot answer for keeps its access: an outage of the store ends
 * no one's access, it only stops new admissions. Any error for one key (the store unavailable,
 * or anything else) is logged and the sweep goes on with the next key.
 *
 * Returns the keys revoked.
 */
export async function refreshKeys(ctx: RefreshContext): Promise<string[]> {
  const dids = [...new Set([...ctx.db.getUnrevokedKeys(), ...ctx.ws.connectedKeys().map((k) => k.did)])];
  const revoked: string[] = [];
  for (const did of dids) {
    let standing: boolean;
    try {
      const resolution = await ctx.store.resolve(did, { fresh: true });
      standing = resolution.kind === "unlisted" || resolution.author !== null;
    } catch (e) {
      if (e instanceof StoreUnavailable) {
        ctx.log.warn({ did, err: e.message }, "key refresh: identity store unavailable; the key keeps its access");
      } else {
        ctx.log.error({ did, err: e }, "key refresh: resolving the key failed; the key keeps its access");
      }
      continue;
    }
    if (standing) continue;
    revokeKey(ctx, did);
    revoked.push(did);
  }
  return revoked;
}

/** Revokes a key in every room it authenticated to (see `refreshKeys`). */
export function revokeKey(ctx: Pick<RefreshContext, "db" | "auth" | "ws" | "log">, did: string): void {
  const rooms = new Set([
    ...ctx.db.getRoomsOfKey(did),
    ...ctx.ws.connectedKeys().filter((k) => k.did === did).map((k) => k.roomId),
  ]);
  ctx.db.markKeyRevoked(did);
  ctx.db.deleteSessionsForKey(did);
  for (const roomId of rooms) {
    endKeyInRoom(ctx, roomId, did, "key revoked");
    ctx.ws.broadcast(roomId, { type: "rotate-needed", reason: "revoked", removedKeys: [did] });
  }
  ctx.log.info({ did, rooms: [...rooms] }, "key revoked by its log");
}
