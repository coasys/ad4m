import Database from "better-sqlite3";
import { mkdirSync } from "node:fs";
import { dirname } from "node:path";
import {
  computeRevision,
  isEncryptedLinkData,
  linkHash,
  type LinkExpression,
  type PerspectiveDiff,
} from "./types.js";

const SCHEMA = `
CREATE TABLE IF NOT EXISTS rooms (
  id TEXT PRIMARY KEY,
  admin_did TEXT NOT NULL,
  created_at TEXT NOT NULL,
  e2e_enabled INTEGER DEFAULT 0,
  revision TEXT
);
CREATE TABLE IF NOT EXISTS acl (
  room_id TEXT,
  did TEXT,
  added_at TEXT,
  x25519_public_key TEXT,
  PRIMARY KEY (room_id, did)
);
CREATE TABLE IF NOT EXISTS links (
  room_id TEXT,
  link_hash TEXT,
  link_data TEXT,
  encrypted INTEGER DEFAULT 0,
  sequence INTEGER,
  PRIMARY KEY (room_id, link_hash)
);
CREATE TABLE IF NOT EXISTS diffs (
  room_id TEXT,
  sequence INTEGER,
  diff_data TEXT,
  timestamp TEXT,
  author_did TEXT
);
CREATE TABLE IF NOT EXISTS sessions (
  token TEXT PRIMARY KEY,
  room_id TEXT,
  did TEXT,
  expires_at TEXT
);
CREATE TABLE IF NOT EXISTS room_keys (
  room_id TEXT,
  did TEXT,
  encrypted_key TEXT,
  version INTEGER,
  PRIMARY KEY (room_id, did, version)
);
CREATE TABLE IF NOT EXISTS federation_peers (
  room_id TEXT,
  peer_url TEXT,
  added_at TEXT,
  peer_public_key TEXT,
  PRIMARY KEY (room_id, peer_url)
);
CREATE TABLE IF NOT EXISTS server_identity (
  key_type TEXT PRIMARY KEY,
  public_key TEXT,
  private_key TEXT
);
CREATE INDEX IF NOT EXISTS idx_links_room ON links(room_id);
CREATE INDEX IF NOT EXISTS idx_diffs_room_seq ON diffs(room_id, sequence);
CREATE INDEX IF NOT EXISTS idx_acl_room ON acl(room_id);
`;

export interface RoomRow {
  id: string;
  admin_did: string;
  created_at: string;
  e2e_enabled: number;
  revision: string | null;
}

export interface AclRow {
  room_id: string;
  did: string;
  added_at: string;
  x25519_public_key: string | null;
}

export interface LinkRow {
  room_id: string;
  link_hash: string;
  link_data: string;
  encrypted: number;
  sequence: number;
}

export interface DiffRow {
  room_id: string;
  sequence: number;
  diff_data: string;
  timestamp: string;
  author_did: string;
}

export interface SessionRow {
  token: string;
  room_id: string;
  did: string;
  expires_at: string;
}

export interface RoomKeyRow {
  room_id: string;
  did: string;
  encrypted_key: string;
  version: number;
}

export interface FederationPeerRow {
  room_id: string;
  peer_url: string;
  added_at: string;
  peer_public_key: string | null;
}

export interface ServerIdentityRow {
  key_type: string;
  public_key: string;
  private_key: string;
}

/**
 * Thin synchronous wrapper around better-sqlite3 exposing all the query
 * shapes the rest of the server needs. Kept dependency-free of any other
 * module in this package so it can be imported everywhere without cycles.
 */
export class LinkServerDB {
  readonly raw: Database.Database;

  constructor(filePath: string) {
    if (filePath !== ":memory:") {
      mkdirSync(dirname(filePath), { recursive: true });
    }
    this.raw = new Database(filePath);
    this.raw.pragma("journal_mode = WAL");
    this.raw.pragma("foreign_keys = ON");
    this.raw.exec(SCHEMA);
  }

  close(): void {
    this.raw.close();
  }

  // ---- rooms ----

  getRoom(roomId: string): RoomRow | undefined {
    return this.raw
      .prepare("SELECT * FROM rooms WHERE id = ?")
      .get(roomId) as RoomRow | undefined;
  }

  createRoom(roomId: string, adminDid: string): RoomRow {
    const createdAt = new Date().toISOString();
    this.raw
      .prepare(
        "INSERT INTO rooms (id, admin_did, created_at, e2e_enabled) VALUES (?, ?, ?, 0)"
      )
      .run(roomId, adminDid, createdAt);
    return { id: roomId, admin_did: adminDid, created_at: createdAt, e2e_enabled: 0, revision: null };
  }

  /** Returns the cached revision for a room, or recomputes it if not yet cached. */
  getRoomRevision(roomId: string): string {
    const room = this.getRoom(roomId);
    if (room?.revision) return room.revision;
    // First call or legacy room without a cached revision — compute and cache it.
    const revision = computeRevision(this.getActiveHashes(roomId));
    if (room) {
      this.raw
        .prepare("UPDATE rooms SET revision = ? WHERE id = ?")
        .run(revision, roomId);
    }
    return revision;
  }

  setE2eEnabled(roomId: string, enabled: boolean): void {
    this.raw
      .prepare("UPDATE rooms SET e2e_enabled = ? WHERE id = ?")
      .run(enabled ? 1 : 0, roomId);
  }

  // ---- acl ----

  getAcl(roomId: string): AclRow[] {
    return this.raw
      .prepare("SELECT * FROM acl WHERE room_id = ? ORDER BY added_at ASC")
      .all(roomId) as AclRow[];
  }

  isMember(roomId: string, did: string): boolean {
    const row = this.raw
      .prepare("SELECT 1 FROM acl WHERE room_id = ? AND did = ?")
      .get(roomId, did);
    return row !== undefined;
  }

  addAcl(roomId: string, did: string): void {
    this.raw
      .prepare(
        "INSERT OR IGNORE INTO acl (room_id, did, added_at) VALUES (?, ?, ?)"
      )
      .run(roomId, did, new Date().toISOString());
  }

  removeAcl(roomId: string, did: string): void {
    this.raw
      .prepare("DELETE FROM acl WHERE room_id = ? AND did = ?")
      .run(roomId, did);
  }

  setX25519PublicKey(roomId: string, did: string, x25519PublicKey: string): void {
    this.raw
      .prepare("UPDATE acl SET x25519_public_key = ? WHERE room_id = ? AND did = ?")
      .run(x25519PublicKey, roomId, did);
  }

  getX25519PublicKey(roomId: string, did: string): string | null {
    const row = this.raw
      .prepare("SELECT x25519_public_key FROM acl WHERE room_id = ? AND did = ?")
      .get(roomId, did) as { x25519_public_key: string | null } | undefined;
    return row?.x25519_public_key ?? null;
  }

  // ---- links (active OR-Set) ----

  insertLink(
    roomId: string,
    linkHash: string,
    linkData: string,
    encrypted: boolean,
    sequence: number
  ): void {
    this.raw
      .prepare(
        `INSERT OR IGNORE INTO links (room_id, link_hash, link_data, encrypted, sequence)
         VALUES (?, ?, ?, ?, ?)`
      )
      .run(roomId, linkHash, linkData, encrypted ? 1 : 0, sequence);
  }

  removeLink(roomId: string, linkHash: string): void {
    this.raw
      .prepare("DELETE FROM links WHERE room_id = ? AND link_hash = ?")
      .run(roomId, linkHash);
  }

  hasLink(roomId: string, linkHash: string): boolean {
    const row = this.raw
      .prepare("SELECT 1 FROM links WHERE room_id = ? AND link_hash = ?")
      .get(roomId, linkHash);
    return row !== undefined;
  }

  getActiveLinkRows(roomId: string): LinkRow[] {
    return this.raw
      .prepare("SELECT * FROM links WHERE room_id = ?")
      .all(roomId) as LinkRow[];
  }

  getActiveLinks(roomId: string): LinkExpression[] {
    return this.getActiveLinkRows(roomId).map(
      (row) => JSON.parse(row.link_data) as LinkExpression
    );
  }

  getActiveHashes(roomId: string): string[] {
    const rows = this.raw
      .prepare("SELECT link_hash FROM links WHERE room_id = ?")
      .all(roomId) as { link_hash: string }[];
    return rows.map((r) => r.link_hash);
  }

  // ---- diffs (append-only log) ----

  getNextSequence(roomId: string): number {
    const row = this.raw
      .prepare(
        "SELECT COALESCE(MAX(sequence), 0) + 1 AS next FROM diffs WHERE room_id = ?"
      )
      .get(roomId) as { next: number };
    return row.next;
  }

  getMaxSequence(roomId: string): number {
    const row = this.raw
      .prepare("SELECT COALESCE(MAX(sequence), 0) AS max FROM diffs WHERE room_id = ?")
      .get(roomId) as { max: number };
    return row.max;
  }

  appendDiff(
    roomId: string,
    sequence: number,
    diffData: string,
    authorDid: string
  ): void {
    this.raw
      .prepare(
        `INSERT INTO diffs (room_id, sequence, diff_data, timestamp, author_did)
         VALUES (?, ?, ?, ?, ?)`
      )
      .run(roomId, sequence, diffData, new Date().toISOString(), authorDid);
  }

  getDiffsSince(roomId: string, since: number): DiffRow[] {
    return this.raw
      .prepare(
        "SELECT * FROM diffs WHERE room_id = ? AND sequence > ? ORDER BY sequence ASC"
      )
      .all(roomId, since) as DiffRow[];
  }

  getDiffsSinceParsed(
    roomId: string,
    since: number
  ): { sequence: number; diff: PerspectiveDiff; timestamp: string; authorDid: string }[] {
    return this.getDiffsSince(roomId, since).map((row) => ({
      sequence: row.sequence,
      diff: JSON.parse(row.diff_data) as PerspectiveDiff,
      timestamp: row.timestamp,
      authorDid: row.author_did,
    }));
  }

  /**
   * Applies a PerspectiveDiff's additions/removals to the active link set
   * (OR-Set: add-wins insert, remove-by-hash) and appends it to the
   * append-only diff log, all in one transaction. Shared by local commits
   * (routes.ts) and federation (federation.ts) so both paths guarantee
   * identical merge + revision semantics.
   */
  applyDiffAndAppend(
    roomId: string,
    diff: PerspectiveDiff,
    authorDid: string
  ): { sequence: number; revision: string } {
    const run = this.raw.transaction(() => {
      const sequence = this.getNextSequence(roomId);
      for (const link of diff.additions) {
        const hash = linkHash(link);
        this.insertLink(
          roomId,
          hash,
          JSON.stringify(link),
          isEncryptedLinkData(link.data),
          sequence
        );
      }
      for (const link of diff.removals) {
        const hash = linkHash(link);
        this.removeLink(roomId, hash);
      }
      this.appendDiff(roomId, sequence, JSON.stringify(diff), authorDid);
      const revision = computeRevision(this.getActiveHashes(roomId));
      this.raw
        .prepare("UPDATE rooms SET revision = ? WHERE id = ?")
        .run(revision, roomId);
      return { sequence, revision };
    });
    return run();
  }

  // ---- sessions ----

  createSession(
    token: string,
    roomId: string,
    did: string,
    expiresAt: string
  ): void {
    this.raw
      .prepare(
        "INSERT OR REPLACE INTO sessions (token, room_id, did, expires_at) VALUES (?, ?, ?, ?)"
      )
      .run(token, roomId, did, expiresAt);
  }

  getSession(token: string): SessionRow | undefined {
    return this.raw
      .prepare("SELECT * FROM sessions WHERE token = ?")
      .get(token) as SessionRow | undefined;
  }

  deleteSession(token: string): void {
    this.raw.prepare("DELETE FROM sessions WHERE token = ?").run(token);
  }

  deleteSessionsForDid(roomId: string, did: string): void {
    this.raw
      .prepare("DELETE FROM sessions WHERE room_id = ? AND did = ?")
      .run(roomId, did);
  }

  // ---- room keys (E2E) ----

  addRoomKey(
    roomId: string,
    did: string,
    version: number,
    encryptedKey: string
  ): void {
    this.raw
      .prepare(
        `INSERT OR REPLACE INTO room_keys (room_id, did, encrypted_key, version)
         VALUES (?, ?, ?, ?)`
      )
      .run(roomId, did, encryptedKey, version);
  }

  getLatestRoomKey(roomId: string, did: string): RoomKeyRow | undefined {
    return this.raw
      .prepare(
        `SELECT * FROM room_keys WHERE room_id = ? AND did = ? ORDER BY version DESC LIMIT 1`
      )
      .get(roomId, did) as RoomKeyRow | undefined;
  }

  getLatestKeyVersion(roomId: string): number {
    const row = this.raw
      .prepare(
        "SELECT COALESCE(MAX(version), 0) AS max FROM room_keys WHERE room_id = ?"
      )
      .get(roomId) as { max: number };
    return row.max;
  }

  // ---- federation peers ----

  addFederationPeer(roomId: string, peerUrl: string, peerPublicKey?: string): void {
    this.raw
      .prepare(
        `INSERT INTO federation_peers (room_id, peer_url, added_at, peer_public_key)
         VALUES (?, ?, ?, ?)
         ON CONFLICT(room_id, peer_url) DO UPDATE SET peer_public_key = COALESCE(excluded.peer_public_key, federation_peers.peer_public_key)`
      )
      .run(roomId, peerUrl, new Date().toISOString(), peerPublicKey ?? null);
  }

  removeFederationPeer(roomId: string, peerUrl: string): void {
    this.raw
      .prepare("DELETE FROM federation_peers WHERE room_id = ? AND peer_url = ?")
      .run(roomId, peerUrl);
  }

  getFederationPeers(roomId: string): FederationPeerRow[] {
    return this.raw
      .prepare("SELECT * FROM federation_peers WHERE room_id = ?")
      .all(roomId) as FederationPeerRow[];
  }

  setFederationPeerPublicKey(roomId: string, peerUrl: string, pubkey: string): void {
    this.raw
      .prepare(
        "UPDATE federation_peers SET peer_public_key = ? WHERE room_id = ? AND peer_url = ?"
      )
      .run(pubkey, roomId, peerUrl);
  }

  findFederationPeerByPublicKey(
    roomId: string,
    publicKey: string
  ): FederationPeerRow | undefined {
    return this.raw
      .prepare(
        "SELECT * FROM federation_peers WHERE room_id = ? AND peer_public_key = ?"
      )
      .get(roomId, publicKey) as FederationPeerRow | undefined;
  }

  findFederationPeerByUrl(
    roomId: string,
    peerUrl: string
  ): FederationPeerRow | undefined {
    return this.raw
      .prepare("SELECT * FROM federation_peers WHERE room_id = ? AND peer_url = ?")
      .get(roomId, peerUrl) as FederationPeerRow | undefined;
  }

  listAllRoomsWithPeers(): string[] {
    const rows = this.raw
      .prepare("SELECT DISTINCT room_id FROM federation_peers")
      .all() as { room_id: string }[];
    return rows.map((r) => r.room_id);
  }

  // ---- server identity ----

  getIdentity(keyType: string): ServerIdentityRow | undefined {
    return this.raw
      .prepare("SELECT * FROM server_identity WHERE key_type = ?")
      .get(keyType) as ServerIdentityRow | undefined;
  }

  setIdentity(keyType: string, publicKey: string, privateKey: string): void {
    this.raw
      .prepare(
        `INSERT OR REPLACE INTO server_identity (key_type, public_key, private_key)
         VALUES (?, ?, ?)`
      )
      .run(keyType, publicKey, privateKey);
  }
}
