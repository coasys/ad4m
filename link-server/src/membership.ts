import { createHash } from "node:crypto";
import { base58 } from "@scure/base";
import { AUTH_SKEW_MS } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import {
  isCoreError,
  judgeLinkWithoutLog,
  MembershipReplay,
  wireLinkHash,
  wireSignatureHolds,
  type JudgedLink,
  type Member,
  type MembershipChange,
  type WireMembershipLink,
} from "./identity-core.js";
import type { StoreClient } from "./store-client.js";

/**
 * Who belongs to each room: the replay (the identity core's `MembershipReplay`) of the room's
 * counted membership links, kept in memory and rebuilt from the database when needed.
 *
 * Only counted links are stored. A link arriving is applied with the core's `tryApply`, which
 * leaves the replay as it was when the link is refused (the core's `apply` keeps some refused
 * links, to count them should an earlier link arrive later): so the replay held is always the
 * replay of the stored links, a link no one stored never counts, and no refusal costs a
 * rebuild from the database.
 *
 * A self-join (a `member` link whose speaker is its source) that is not stored yet counts only
 * when its timestamp is within `AUTH_SKEW_MS` of the server's clock (`stale` otherwise). The
 * replay judges a self-join, and an invite's expiry and first use, at the link's own
 * timestamp; a self-join dated into an earlier open period, before an invite expired or before
 * another person used it would otherwise count. Links written by admins (invites, adds,
 * removals, rules) keep their timestamps: an admin may write them offline and sync later, and
 * an admin is trusted with the room anyway.
 */

/** The room id of the space `creator` made with `salt`: `base58(sha256(creator ‖ 0x00 ‖ salt))`. */
export function roomIdOf(creator: string, salt: string): string {
  return base58.encode(new Uint8Array(createHash("sha256").update(creator).update(new Uint8Array([0])).update(salt).digest()));
}

/** The `member` predicate: the self-join's. */
const MEMBER_PREDICATE = "coasys://membership/member";

/** The most links one `POST /membership` (genesis included) may carry. */
export const MAX_MEMBERSHIP_BATCH = 100;

/** Why a self-join not stored yet is refused when its timestamp is off the server's clock. */
export const STALE = "stale";

/** The reason a link already stored (or already counted earlier in the batch) is answered with. */
export const REPEATED = "repeated";

/** One link's outcome, as `POST /membership` answers it. */
export interface LinkResult {
  hash: string;
  /** Whether this request counted the link (false for a link already stored). */
  counted: boolean;
  /** True when the link is stored: counted now, or already (`reason: "repeated"`). */
  stored?: boolean;
  reason?: string;
}

/**
 * Whether a judged link is a self-join: a `member` link whose speaker (the person its key speaks
 * for, not as a bot) is its source.
 */
export function isSelfJoin(judged: Pick<JudgedLink, "predicate" | "source" | "person" | "byBot">): boolean {
  return judged.predicate === MEMBER_PREDICATE && !judged.byBot && judged.person !== null && judged.person === judged.source;
}

/** Why a judged link is refused before the replay sees it (a stale self-join), if it is. */
function refusedOnArrival(judged: string, now: number): string | undefined {
  const link = JSON.parse(judged) as JudgedLink;
  if (isSelfJoin(link) && Math.abs(link.timestamp - now) > AUTH_SKEW_MS) return STALE;
  return undefined;
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
  // A signature that does not hold counts for no one, whoever's log lists the key: judged
  // without a log, so a forged link costs no identity-store lookup.
  if (!wireSignatureHolds(json)) return judgeLinkWithoutLog(json, now);
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

/**
 * What the replay made of one judged link: the changes, or the refusal. `tryApply` (arrivals)
 * leaves the replay untouched on a refusal; `apply` (rebuilds from stored links) is the core's.
 */
function applyOne(
  replay: MembershipReplay,
  judged: string,
  how: "apply" | "tryApply" = "tryApply"
): { changes: MembershipChange[] } | { reason: string } {
  try {
    return { changes: JSON.parse(how === "apply" ? replay.apply(judged) : replay.tryApply(judged)) as MembershipChange[] };
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
    for (const row of this.db.getMembershipJudged(roomId)) applyOne(replay, row, "apply");
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
   * request sees a half-applied batch). A link already stored is answered `repeated` (stored,
   * not counted now) without change. `now`: the server's clock, for stale self-joins.
   */
  apply(roomId: string, batch: Judged[], now: number): ApplyOutcome {
    const replay = this.replay(roomId)!;
    const results: LinkResult[] = [];
    const counted: Array<{ link: WireMembershipLink; hash: string; judged: string }> = [];
    const changes: MembershipChange[] = [];
    const stored: Array<{ seq: number; link: WireMembershipLink }> = [];
    const inBatch = new Set<string>();
    for (const item of batch) {
      if (inBatch.has(item.hash) || (item.hash !== "" && this.db.hasMembershipLink(roomId, item.hash))) {
        results.push({ hash: item.hash, counted: false, stored: true, reason: REPEATED });
        continue;
      }
      if (item.refused || !item.judged) {
        results.push({ hash: item.hash, counted: false, reason: item.refused ?? "malformed" });
        continue;
      }
      const early = refusedOnArrival(item.judged, now);
      const outcome = early ? { reason: early } : applyOne(replay, item.judged);
      if ("reason" in outcome) {
        // Nothing of a refused link stays in the replay (`tryApply`): no rebuild.
        results.push({ hash: item.hash, counted: false, reason: outcome.reason });
        continue;
      }
      inBatch.add(item.hash);
      counted.push({ link: item.link, hash: item.hash, judged: item.judged });
      changes.push(...outcome.changes);
      results.push({ hash: item.hash, counted: true, stored: true });
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
  genesis(roomId: string, creator: string, batch: Judged[], now: number): { created: boolean; results: LinkResult[] } {
    const replay = new MembershipReplay(creator);
    const results: LinkResult[] = [];
    const counted: Array<{ hash: string; link: string; judged: string }> = [];
    for (const item of batch) {
      if (item.refused || !item.judged) {
        results.push({ hash: item.hash, counted: false, reason: item.refused ?? "malformed" });
        continue;
      }
      const early = refusedOnArrival(item.judged, now);
      const outcome = early ? { reason: early } : applyOne(replay, item.judged);
      if ("reason" in outcome) {
        results.push({ hash: item.hash, counted: false, reason: outcome.reason });
        continue;
      }
      counted.push({ hash: item.hash, link: JSON.stringify(item.link), judged: item.judged });
      results.push({ hash: item.hash, counted: true, stored: true });
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
