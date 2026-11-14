import { createHash, randomBytes } from "node:crypto";
import { readFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import * as ed from "@noble/ed25519";
import { ed25519 } from "@noble/curves/ed25519.js";
import { base58 } from "@scure/base";
import { roomKeySigningBytes, type SignedHead, type WireMembershipLink } from "../src/identity-core.js";

/**
 * The identities the tests sign with, and a fake identity store that serves them as the
 * identity store does (`/v1/keys`, `/v1/logs`, `/v1/heads`). The identities come from
 * `ad4m-identity-core/vectors/link-server-identities.json` (written by the core's
 * `tests/link_server_fixture.rs`): real key logs, heads signed by the store key.
 *
 * Persons: alice (d1), bob (d1), carol (d1, bot key d2), dave (d1, d2; seq 1 revokes d2),
 * frank (d1). eve is a device-only key.
 */

const { bytesToHex, hexToBytes } = ed.etc;

interface FixtureKey {
  secret: string;
  multikey: string;
  did: string;
  keyId: string;
  added: number;
  revoked: number | null;
  bot: unknown;
  expires: number | null;
}

interface FixturePerson {
  did: string;
  scid: string;
  events: unknown[];
  heads: SignedHead[];
  keys: Record<string, FixtureKey>;
}

interface Fixture {
  store: { secret: string; multikey: string; did: string };
  persons: Record<PersonName, FixturePerson>;
  deviceOnly: Record<"eve", { secret: string; multikey: string; did: string }>;
}

export type PersonName = "alice" | "bob" | "carol" | "dave" | "frank";

export const IDENTITIES = JSON.parse(
  readFileSync(new URL("../../ad4m-identity-core/vectors/link-server-identities.json", import.meta.url), "utf8")
) as Fixture;

/** The store key the fake store signs heads with: what `storeHeadKeys` trusts. */
export const STORE_HEAD_KEY = IDENTITIES.store.multikey;

/** A key that signs: its `did:key`, secret, public key, and the person it speaks for (if any). */
export interface TestAgent {
  did: string;
  privateKey: Uint8Array;
  publicKey: Uint8Array;
  /** The person's did:scid for a device key of a fixture log; undefined for a device-only key. */
  person?: string;
}

function agentFrom(secretHex: string, did: string, person?: string): TestAgent {
  const privateKey = hexToBytes(secretHex);
  return { did, privateKey, publicKey: ed25519.getPublicKey(privateKey), person };
}

/** The device key `key` of the fixture person `name`. */
export function personAgent(name: PersonName, key = "d1"): TestAgent {
  const person = IDENTITIES.persons[name];
  const k = person.keys[key];
  if (!k) throw new Error(`${name} has no key ${key}`);
  return agentFrom(k.secret, k.did, person.did);
}

/** eve: a key no log lists. */
export function deviceOnlyAgent(): TestAgent {
  const eve = IDENTITIES.deviceOnly.eve;
  return agentFrom(eve.secret, eve.did);
}

export function personDid(name: PersonName): string {
  return IDENTITIES.persons[name].did;
}

/**
 * A fake identity store: the fixture's logs, keys and heads over HTTP, with a switch for how
 * far each log has grown (`showUpTo`) and one for an outage (`down`).
 */
export class FakeStore {
  url = "";
  down = false;
  /** Requests served, by path prefix: `keys`, `logs`, `heads`. */
  readonly hits = { keys: 0, logs: 0, heads: 0 };
  private seqs = new Map<PersonName, number>();
  private server: Server;

  private constructor() {
    this.server = createServer((req, res) => this.serve(req.url ?? "/", res));
  }

  static async start(): Promise<FakeStore> {
    const store = new FakeStore();
    await new Promise<void>((resolve) => store.server.listen(0, "127.0.0.1", resolve));
    store.url = `http://127.0.0.1:${(store.server.address() as AddressInfo).port}`;
    return store;
  }

  /** The store shows `name`'s log up to `seq` from now on (default: the whole log). */
  showUpTo(name: PersonName, seq: number): void {
    this.seqs.set(name, seq);
  }

  private seqOf(name: PersonName): number {
    return this.seqs.get(name) ?? IDENTITIES.persons[name].heads.length - 1;
  }

  private serve(rawUrl: string, res: import("node:http").ServerResponse): void {
    const send = (status: number, body?: unknown) => {
      res.writeHead(status, { "content-type": "application/json" });
      res.end(body === undefined ? "" : JSON.stringify(body));
    };
    if (this.down) return send(503, { error: "down" });
    const url = new URL(rawUrl, "http://store");
    const [, v1, kind, id] = url.pathname.split("/");
    if (v1 !== "v1") return send(404);
    const entries = Object.entries(IDENTITIES.persons) as Array<[PersonName, FixturePerson]>;
    if (kind === "keys") {
      this.hits.keys++;
      for (const [name, person] of entries) {
        const key = Object.values(person.keys).find((k) => k.multikey === id);
        if (!key) continue;
        const seq = this.seqOf(name);
        if (key.added > seq) return send(404);
        return send(200, {
          did: person.did,
          keyId: key.keyId,
          added: key.added,
          revoked: key.revoked !== null && key.revoked <= seq ? key.revoked : null,
          bot: key.bot,
          expires: key.expires,
          paused: false,
          head: person.heads[seq],
        });
      }
      return send(404);
    }
    const found = entries.find(([, p]) => p.scid === id);
    if (!found) return send(404);
    const [name, person] = found;
    const seq = this.seqOf(name);
    if (kind === "heads") {
      this.hits.heads++;
      return send(200, person.heads[seq]);
    }
    if (kind === "logs") {
      this.hits.logs++;
      const from = Number(url.searchParams.get("from") ?? "0");
      const events = person.events.slice(from, seq + 1);
      return send(200, {
        events,
        appendedAt: events.map(() => 1_790_000_000_000),
        head: person.heads[seq],
        vouched: false,
        pauses: [],
      });
    }
    return send(404);
  }

  close(): Promise<void> {
    return new Promise((resolve) => {
      this.server.closeAllConnections();
      this.server.close(() => resolve());
    });
  }
}

// ---- protocol 2 auth ----

/**
 * What a key signs to authenticate to the server at `origin` (`agentSignStringHex` signs SHA-256
 * of it). Written out here apart from `src/auth.ts`, so a change to either shows.
 */
export function authMessage(origin: string, roomId: string, timestamp: number): string {
  return `coasys-link-server:auth:2:${origin}:${roomId}:${timestamp}`;
}

/** The agent's X25519 key for room keys: here its Ed25519 key in Montgomery form, as `sealToDid` seals. */
export function x25519Of(agent: TestAgent): string {
  return bytesToHex(ed25519.utils.toMontgomery(agent.publicKey));
}

export interface AuthBody {
  did: string;
  origin: string;
  signature: string;
  timestamp: number;
  x25519PublicKey: string;
  x25519Signature: string;
}

/**
 * The body of `POST /rooms/:roomId/auth` for `agent`, signed for the server at `origin` (a server
 * URL: its origin is taken).
 */
export function authBody(agent: TestAgent, origin: string, roomId: string, timestamp = Date.now()): AuthBody {
  origin = new URL(origin).origin;
  const digest = createHash("sha256").update(authMessage(origin, roomId, timestamp)).digest();
  const signature = bytesToHex(ed25519.sign(digest, agent.privateKey));
  const x25519PublicKey = x25519Of(agent);
  const x25519Signature = bytesToHex(ed25519.sign(roomKeySigningBytes(agent.did, x25519PublicKey), agent.privateKey));
  return { did: agent.did, origin, signature, timestamp, x25519PublicKey, x25519Signature };
}

// ---- membership links ----

export const SPACE = "coasys://membership/space";
export const P = {
  rules: "coasys://membership/rules",
  join: "coasys://membership/join",
  admin: "coasys://membership/admin",
  member: "coasys://membership/member",
  invite: "coasys://membership/invite",
  removed: "coasys://membership/removed",
  bot: "coasys://membership/bot",
  approvedBy: "coasys://membership/approved-by",
} as const;

export function jsonLiteral(value: unknown): string {
  return `literal:json:${encodeURIComponent(JSON.stringify(value))}`;
}

export function stringLiteral(value: string): string {
  return `literal:string:${encodeURIComponent(value)}`;
}

/** A membership link signed by `agent`, as the language signs one (no anchor). */
export function signLink(
  agent: TestAgent,
  data: { source: string; predicate: string; target: string },
  timestamp: string = new Date().toISOString()
): WireMembershipLink {
  const signed = JSON.stringify({ predicate: data.predicate, source: data.source, target: data.target });
  const digest = createHash("sha256").update(signed + timestamp).digest();
  return {
    author: agent.did,
    timestamp,
    data: { source: data.source, predicate: data.predicate, target: data.target },
    proof: { key: agent.did, signature: bytesToHex(ed25519.sign(digest, agent.privateKey)) },
  };
}

export const links = {
  rules: (rules: { join?: "open" | "invite"; add?: "admins" | "members" } = { join: "open" }) =>
    ({ source: SPACE, predicate: P.rules, target: jsonLiteral({ add: rules.add ?? "admins", join: rules.join ?? "open" }) }),
  joinMode: (join: "open" | "invite") => ({ source: SPACE, predicate: P.join, target: stringLiteral(join) }),
  member: (person: string, invite?: string) => ({ source: person, predicate: P.member, target: invite ?? SPACE }),
  admin: (person: string) => ({ source: person, predicate: P.admin, target: SPACE }),
  removed: (person: string) => ({ source: person, predicate: P.removed, target: SPACE }),
  invite: (invite: string, expiresAt: number) => ({ source: invite, predicate: P.invite, target: jsonLiteral({ expiresAt }) }),
  bot: (botDid: string) => ({ source: botDid, predicate: P.bot, target: SPACE }),
  approvedBy: (botDid: string, owner: string) => ({ source: botDid, predicate: P.approvedBy, target: owner }),
};

/** The room id of the space `creator` made with `salt`: base58(sha256(creator ‖ 0x00 ‖ salt)). */
export function roomIdOf(creator: string, salt: string): string {
  return base58.encode(
    createHash("sha256").update(creator).update(new Uint8Array([0])).update(salt).digest()
  );
}

export function newSalt(): string {
  return randomBytes(16).toString("hex");
}
