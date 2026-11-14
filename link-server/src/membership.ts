import { createHash } from "node:crypto";
import { base58 } from "@scure/base";
import type { LinkServerDB } from "./db.js";
import {
  isCoreError,
  judgeLinkWithoutLog,
  MembershipReplay,
  wireLinkHash,
  type Member,
  type MembershipChange,
  type WireMembershipLink,
} from "./identity-core.js";
import type { StoreClient } from "./store-client.js";

/**
 * Who belongs to each room: the replay (the identity core's `MembershipReplay`) of the room's
 * counted membership links, kept in memory and rebuilt from the database when needed.
 *
 * Only counted links are stored. A refused link is not stored, but the replay keeps some of
 * them (to count them should an earlier link arrive later); so after such a refusal the room's
 * replay is rebuilt from the stored links, and a link no one stored never counts.
 */

/** The room id of the space `creator` made with `salt`: `base58(sha256(creator ‖ 0x00 ‖ salt))`. */
export function roomIdOf(creator: string, salt: string): string {
  return base58.encode(new Uint8Array(createHash("sha256").update(creator).update(new Uint8Array([0])).update(salt).digest()));
}

/** Refusals after which the replay holds no trace of the link: no rebuild needed. */
const NOT_KEPT = new Set(["notCounted", "backdated", "repeated"]);

/** One link's outcome, as `POST /membership` answers it. */
export interface LinkResult {
  hash: string;
  counted: boolean;
  reason?: string;
}

/** A link judged and ready to apply. */
export interface Judged {
  link: WireMembershipLink;
  hash: string;
  /** The `JudgedLink` JSON the replay takes; absent when the link was refused before judging. */
  judged?: string;
  /** Why it was refused before judging (malformed, not admitted). */
  refused?: string;
}

/** What applying a batch did. */
export interface ApplyOutcome {
  results: LinkResult[];
  /** The links counted now, with the seq each was stored at. */
  stored: Array<{ seq: number; link: WireMembershipLink }>;
  changes: MembershipChange[];
}

/** Judges a link as it arrives: against its signer's log, or as a device-only key's. */
export async function judgeLink(store: StoreClient, link: WireMembershipLink, now: number): Promise<string> {
  const json = JSON.stringify(link);
  const resolution = await store.resolve(link.author, { now });
  if (resolution.kind === "unlisted") return judgeLinkWithoutLog(json, now);
  return resolution.handle.judgeLink(json, resolution.trusted, now);
}

/** The hash of a link, or null when the core finds it malformed. */
export function hashOf(link: WireMembershipLink): string | null {
  try {
    return wireLinkHash(JSON.stringify(link));
  } catch {
    return null;
  }
}

/** What `replay.apply` made of one judged link: the changes, or the refusal. */
function applyOne(replay: MembershipReplay, judged: string): { changes: MembershipChange[] } | { reason: string } {
  try {
    return { changes: JSON.parse(replay.apply(judged)) as MembershipChange[] };
  } catch (e) {
    if (isCoreError(e)) return { reason: e.reason };
    throw e;
  }
}

export class MembershipRooms {
  private readonly replays = new Map<string, MembershipReplay>();

  constructor(private readonly db: LinkServerDB) {}

  /** The room's replay, built from its stored links on first use. */
  replay(roomId: string): MembershipReplay | undefined {
    const held = this.replays.get(roomId);
    if (held) return held;
    const room = this.db.getRoom(roomId);
    if (!room || room.legacy) return undefined;
    return this.rebuild(roomId, room.admin_did);
  }

  private rebuild(roomId: string, creator: string): MembershipReplay {
    this.replays.get(roomId)?.free();
    const replay = new MembershipReplay(creator);
    for (const row of this.db.getMembershipJudged(roomId)) applyOne(replay, row);
    this.replays.set(roomId, replay);
    return replay;
  }

  /** Whether `person` (a person DID, or a bot's did:key) is a current member of the room. */
  isMember(roomId: string, person: string): boolean {
    return this.replay(roomId)?.isMember(person) ?? false;
  }

  /** The room's current members. */
  members(roomId: string): Member[] {
    const replay = this.replay(roomId);
    if (!replay) return [];
    return (JSON.parse(replay.members()) as Member[]).filter((m) => m.until === null);
  }

  /**
   * Applies judged links in order and stores the counted ones, in one go (no await: no other
   * request sees a half-applied batch). A link already stored counts again without change.
   */
  apply(roomId: string, batch: Judged[]): ApplyOutcome {
    const room = this.db.getRoom(roomId)!;
    let replay = this.replay(roomId)!;
    const results: LinkResult[] = [];
    const counted: Array<{ link: WireMembershipLink; hash: string; judged: string }> = [];
    const changes: MembershipChange[] = [];
    const stored: Array<{ seq: number; link: WireMembershipLink }> = [];
    const inBatch = new Set<string>();
    for (const item of batch) {
      if (this.db.hasMembershipLink(roomId, item.hash) || inBatch.has(item.hash)) {
        results.push({ hash: item.hash, counted: true });
        continue;
      }
      if (item.refused || !item.judged) {
        results.push({ hash: item.hash, counted: false, reason: item.refused ?? "malformed" });
        continue;
      }
      const outcome = applyOne(replay, item.judged);
      if ("reason" in outcome) {
        results.push({ hash: item.hash, counted: false, reason: outcome.reason });
        if (!NOT_KEPT.has(outcome.reason)) {
          // Store what counted so far, then rebuild without the refused link.
          stored.push(...this.store(roomId, counted.splice(0)));
          replay = this.rebuild(roomId, room.admin_did);
        }
        continue;
      }
      inBatch.add(item.hash);
      counted.push({ link: item.link, hash: item.hash, judged: item.judged });
      changes.push(...outcome.changes);
      results.push({ hash: item.hash, counted: true });
    }
    stored.push(...this.store(roomId, counted));
    return { results, stored, changes };
  }

  private store(
    roomId: string,
    links: Array<{ link: WireMembershipLink; hash: string; judged: string }>
  ): Array<{ seq: number; link: WireMembershipLink }> {
    if (links.length === 0) return [];
    const seqs = this.db.insertMembershipLinks(
      roomId,
      links.map((l) => ({ hash: l.hash, link: JSON.stringify(l.link), judged: l.judged }))
    );
    return links.map((l, i) => ({ seq: seqs[i], link: l.link }));
  }

  /**
   * Creates a room from its genesis batch: a fresh replay of the creator, the links applied in
   * order, and the room stored only when the creator's `rules` link counted (the creator is
   * then its first admin). Returns the results and, when created, the stored links.
   */
  genesis(roomId: string, creator: string, batch: Judged[]): { created: boolean; results: LinkResult[] } {
    const replay = new MembershipReplay(creator);
    const results: LinkResult[] = [];
    const counted: Array<{ hash: string; link: string; judged: string }> = [];
    for (const item of batch) {
      if (item.refused || !item.judged) {
        results.push({ hash: item.hash, counted: false, reason: item.refused ?? "malformed" });
        continue;
      }
      const outcome = applyOne(replay, item.judged);
      if ("reason" in outcome) {
        results.push({ hash: item.hash, counted: false, reason: outcome.reason });
        continue;
      }
      counted.push({ hash: item.hash, link: JSON.stringify(item.link), judged: item.judged });
      results.push({ hash: item.hash, counted: true });
    }
    const created = replay.isMember(creator) && this.db.createRoomWithMembership(roomId, creator, counted);
    replay.free();
    if (created) this.replays.get(roomId)?.free();
    if (created) this.replays.delete(roomId);
    return { created, results };
  }

  /** Forgets a room's replay (it is rebuilt from the database on next use). */
  forget(roomId: string): void {
    this.replays.get(roomId)?.free();
    this.replays.delete(roomId);
  }

  close(): void {
    for (const replay of this.replays.values()) replay.free();
    this.replays.clear();
  }
}
