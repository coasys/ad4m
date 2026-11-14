import type { AuthManager } from "./auth.js";
import type { LinkServerDB, MemberKeyRow } from "./db.js";
import type { MembershipChange } from "./identity-core.js";
import type { ApplyOutcome } from "./membership.js";
import type { WsManager } from "./ws.js";

/**
 * What follows a change of who is in a room: the keys admitted now, ending a removed person's
 * sessions and sockets, and the messages members act on (`membership`, `rotate-needed`).
 * Shared by the routes, the operator and the key refresh.
 */

export interface RoomEventsContext {
  db: LinkServerDB;
  auth: AuthManager;
  ws: WsManager;
}

/**
 * The keys admitted to a room now: every key that authenticated to it, is not revoked, and
 * whose person (or bot) is a current member (every such key on an auto-admit server). These
 * are the keys a room key is sealed to.
 */
export function admittedKeys(ctx: Pick<RoomEventsContext, "db" | "auth">, roomId: string): MemberKeyRow[] {
  return ctx.db
    .getMemberKeys(roomId)
    .filter(
      (k) =>
        k.revoked === 0 &&
        ctx.auth.admitted(roomId, { did: k.did, person: k.person ?? undefined, byBot: k.by_bot === 1 })
    );
}

/** Ends a key's access to a room at once: its sessions there and its open sockets (4005). */
export function endKeyInRoom(ctx: RoomEventsContext, roomId: string, did: string, reason: string): void {
  ctx.auth.revokeSessionsForDid(roomId, did);
  ctx.ws.disconnectDid(roomId, did, reason);
}

/**
 * Ends the access of every key of a removed person (or the removed bot key) in a room. Returns
 * the keys ended.
 */
export function endPerson(ctx: RoomEventsContext, roomId: string, person: string): string[] {
  const keys = ctx.db.getKeysOfPerson(roomId, person).map((k) => k.did);
  for (const did of keys) endKeyInRoom(ctx, roomId, did, "removed from room");
  return keys;
}

/**
 * Tells a room what counted: the stored links (`membership`), then for each change that alters
 * who holds the room key, `rotate-needed`. A removal first ends the removed keys' access, so
 * the next key is never sealed to them.
 */
export function announce(ctx: RoomEventsContext, roomId: string, outcome: Pick<ApplyOutcome, "stored" | "changes">): void {
  if (outcome.stored.length > 0) {
    ctx.ws.broadcast(roomId, {
      type: "membership",
      links: outcome.stored.map((s) => s.link),
      seq: outcome.stored[outcome.stored.length - 1].seq,
    });
  }
  announceChanges(ctx, roomId, outcome.changes);
}

/** `rotate-needed` for joins and removals (see `announce`). */
export function announceChanges(ctx: RoomEventsContext, roomId: string, changes: MembershipChange[]): void {
  let joined = false;
  const removedKeys: string[] = [];
  for (const change of changes) {
    if (change.kind === "joined") joined = true;
    if (change.kind === "removed") removedKeys.push(...endPerson(ctx, roomId, change.person));
  }
  if (removedKeys.length > 0 || changes.some((c) => c.kind === "removed")) {
    ctx.ws.broadcast(roomId, { type: "rotate-needed", reason: "removed", removedKeys });
  } else if (joined) {
    ctx.ws.broadcast(roomId, { type: "rotate-needed", reason: "joined", removedKeys: [] });
  }
}
