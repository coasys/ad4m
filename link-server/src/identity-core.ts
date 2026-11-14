import { createRequire } from "node:module";
import type * as Core from "../identity-core/ad4m_identity_core.js";

/**
 * The identity core's WASM exports for Node (`ad4m-identity-core`, feature `wasm`): key logs,
 * the link judgement and the membership replay. `ad4m-identity-core/build-node.sh` writes the
 * CommonJS binding to `link-server/identity-core/` (generated, gitignored); run it before
 * building or testing the server. The path is the same from `src/` and `dist/`.
 */
const core = createRequire(import.meta.url)("../identity-core/ad4m_identity_core.js") as typeof Core;

export const { KelChain, MembershipReplay, judgeLinkWithoutLog, roomKeySigningBytes, verifyRoomKey, wireLinkHash } =
  core;
export type KelHandle = Core.KelHandle;
export type MembershipReplay = Core.MembershipReplay;

/** The prefix of every membership predicate: such links go to `/membership`, never `/commit`. */
export const MEMBERSHIP_PREFIX = "coasys://membership/";

/** A membership link as it travels: plain, signed by a device key (`did:key`). */
export interface WireMembershipLink {
  author: string;
  timestamp: string;
  data: { source: string; predicate: string; target: string };
  proof: { key: string; signature: string; anchor?: string | number };
}

/** A store-signed head of a key log (`GET /v1/heads/{scid}`). */
export interface SignedHead {
  scid: string;
  seq: number;
  hash: string;
  time: number;
  store: string;
  sig: string;
}

/** `JudgedLink` as the core serializes it: what the replay takes. */
export interface JudgedLink {
  hash: string;
  source: string;
  predicate: string;
  target: string;
  authorKey: string;
  timestamp: number;
  verdict: unknown;
  person: string | null;
  byBot: boolean;
}

/** One membership in the replay: current while `until` is null. */
export interface Member {
  person: string;
  role: "admin" | "member" | "bot";
  botOf: string | null;
  since: number;
  until: number | null;
}

/** What one counted link changed (`MembershipChange`). */
export type MembershipChange =
  | ({ kind: "joined" } & Member)
  | { kind: "removed"; person: string; by: string; at: number }
  | ({ kind: "roleChanged" } & Member)
  | { kind: "joinChanged"; join: "open" | "invite" };

/** What the core throws: `malformed` input, or a `refused` link with the replay's reason. */
export interface CoreError {
  code: "malformed" | "refused";
  reason: string;
}

export function isCoreError(e: unknown): e is CoreError {
  return !!e && typeof e === "object" && typeof (e as CoreError).code === "string" && "reason" in e;
}

/** Whether `value` has the shape of a wire membership link (strings where strings go). */
export function isWireMembershipLink(value: unknown): value is WireMembershipLink {
  const l = value as Partial<WireMembershipLink> | null;
  return (
    !!l &&
    typeof l === "object" &&
    typeof l.author === "string" &&
    l.author.startsWith("did:key:") &&
    typeof l.timestamp === "string" &&
    !!l.data &&
    typeof l.data === "object" &&
    typeof l.data.source === "string" &&
    typeof l.data.predicate === "string" &&
    l.data.predicate.startsWith(MEMBERSHIP_PREFIX) &&
    typeof l.data.target === "string" &&
    !!l.proof &&
    typeof l.proof === "object" &&
    typeof l.proof.key === "string" &&
    typeof l.proof.signature === "string" &&
    (l.proof.anchor === undefined || typeof l.proof.anchor === "string" || typeof l.proof.anchor === "number")
  );
}
