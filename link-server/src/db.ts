import Database from "better-sqlite3";
import { mkdirSync } from "node:fs";
import { dirname } from "node:path";
import {
  EMPTY_REVISION,
  linkHash,
  xorHex,
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
CREATE TABLE IF NOT EXISTS server_identity (
  key_type TEXT PRIMARY KEY,
  public_key TEXT,
  private_key TEXT
);
CREATE INDEX IF NOT EXISTS idx_links_room ON links(room_id);
CREATE INDEX IF NOT EXISTS idx_diffs_room_seq ON diffs(room_id, sequence);
CREATE INDEX IF NOT EXISTS idx_acl_room ON acl(room_id);
CREATE INDEX IF NOT EXISTS idx_sessions_room_did ON sessions(room_id, did);
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
export interface LinkServerDBOptions {
  /** Maximum diffs to retain per room. Older diffs get pruned. Default 10000. 0 disables pruning. */
  maxDiffsPerRoom?: number;
}

export class LinkServerDB {
  readonly raw: Database.Database;
  readonly maxDiffsPerRoom: number;

  // Cached prepared statements — prepared once, reused for the lifetime of
  // the DB connection. Avoids re-parsing SQL on every call (particularly
  // inside applyDiffAndAppend where 1500 links per transaction would
  // otherwise prepare the same INSERT 1500 times).
  private readonly stmts: ReturnType<LinkServerDB["prepareStatements"]>;

  constructor(filePath: string, opts?: LinkServerDBOptions) {
    if (filePath !== ":memory:") {
      mkdirSync(dirname(filePath), { recursive: true });
    }
    this.raw = new Database(filePath);
    this.raw.pragma("journal_mode = WAL");
    this.raw.pragma("foreign_keys = ON");
    this.raw.exec(SCHEMA);
    this.maxDiffsPerRoom = opts?.maxDiffsPerRoom ?? 10_000;
    this.stmts = this.prepareStatements();
  }

  private prepareStatements() {
    return {
      getRoom: this.raw.prepare("SELECT * FROM rooms WHERE id = ?"),
      createRoom: this.raw.prepare(
        "INSERT INTO rooms (id, admin_did, created_at, e2e_enabled, revision) VALUES (?, ?, ?, 0, ?)"
      ),
      setE2e: this.raw.prepare("UPDATE rooms SET e2e_enabled = ? WHERE id = ?"),
      updateRevision: this.raw.prepare("UPDATE rooms SET revision = ? WHERE id = ?"),

      getAcl: this.raw.prepare("SELECT * FROM acl WHERE room_id = ? ORDER BY added_at ASC"),
      isMember: this.raw.prepare("SELECT 1 FROM acl WHERE room_id = ? AND did = ?"),
      addAcl: this.raw.prepare("INSERT OR IGNORE INTO acl (room_id, did, added_at) VALUES (?, ?, ?)"),
      removeAcl: this.raw.prepare("DELETE FROM acl WHERE room_id = ? AND did = ?"),
      setX25519: this.raw.prepare("UPDATE acl SET x25519_public_key = ? WHERE room_id = ? AND did = ?"),
      getX25519: this.raw.prepare("SELECT x25519_public_key FROM acl WHERE room_id = ? AND did = ?"),

      insertLink: this.raw.prepare(
        `INSERT OR IGNORE INTO links (room_id, link_hash, link_data, sequence)
         VALUES (?, ?, ?, ?)`
      ),
      removeLink: this.raw.prepare("DELETE FROM links WHERE room_id = ? AND link_hash = ?"),
      getActiveLinkRows: this.raw.prepare("SELECT * FROM links WHERE room_id = ?"),

      getNextSequence: this.raw.prepare(
        "SELECT COALESCE(MAX(sequence), 0) + 1 AS next FROM diffs WHERE room_id = ?"
      ),
      getMaxSequence: this.raw.prepare(
        "SELECT COALESCE(MAX(sequence), 0) AS max FROM diffs WHERE room_id = ?"
      ),
      appendDiff: this.raw.prepare(
        `INSERT INTO diffs (room_id, sequence, diff_data, timestamp, author_did)
         VALUES (?, ?, ?, ?, ?)`
      ),
      getDiffsSince: this.raw.prepare(
        "SELECT * FROM diffs WHERE room_id = ? AND sequence > ? ORDER BY sequence ASC"
      ),

      createSession: this.raw.prepare(
        "INSERT OR REPLACE INTO sessions (token, room_id, did, expires_at) VALUES (?, ?, ?, ?)"
      ),
      getSession: this.raw.prepare("SELECT * FROM sessions WHERE token = ?"),
      deleteSessionsForDid: this.raw.prepare("DELETE FROM sessions WHERE room_id = ? AND did = ?"),

      addRoomKey: this.raw.prepare(
        `INSERT OR REPLACE INTO room_keys (room_id, did, encrypted_key, version)
         VALUES (?, ?, ?, ?)`
      ),
      getAllRoomKeys: this.raw.prepare(
        `SELECT * FROM room_keys WHERE room_id = ? AND did = ? ORDER BY version ASC`
      ),
      getLatestKeyVersion: this.raw.prepare(
        "SELECT COALESCE(MAX(version), 0) AS max FROM room_keys WHERE room_id = ?"
      ),
      getAllMemberKeyVersions: this.raw.prepare(
        `SELECT did, version FROM room_keys WHERE room_id = ? ORDER BY did, version ASC`
      ),
      addRoomKeyIfMissing: this.raw.prepare(
        `INSERT OR IGNORE INTO room_keys (room_id, did, encrypted_key, version)
         VALUES (?, ?, ?, ?)`
      ),

      getIdentity: this.raw.prepare("SELECT * FROM server_identity WHERE key_type = ?"),
      setIdentity: this.raw.prepare(
        `INSERT OR REPLACE INTO server_identity (key_type, public_key, private_key)
         VALUES (?, ?, ?)`
      ),

      // diff retention
      countDiffs: this.raw.prepare("SELECT COUNT(*) AS cnt FROM diffs WHERE room_id = ?"),
      getOldestRetainedSeq: this.raw.prepare(
        "SELECT sequence FROM diffs WHERE room_id = ? ORDER BY sequence ASC LIMIT 1 OFFSET ?"
      ),
      deleteDiffsBefore: this.raw.prepare(
        "DELETE FROM diffs WHERE room_id = ? AND sequence < ?"
      ),

      // session sweep
      deleteExpiredSessions: this.raw.prepare(
        "DELETE FROM sessions WHERE expires_at < ?"
      ),

    };
  }

  close(): void {
    this.raw.close();
  }

  // ---- rooms ----

  getRoom(roomId: string): RoomRow | undefined {
    return this.stmts.getRoom.get(roomId) as RoomRow | undefined;
  }

  createRoom(roomId: string, adminDid: string): RoomRow {
    const createdAt = new Date().toISOString();
    this.stmts.createRoom.run(roomId, adminDid, createdAt, EMPTY_REVISION);
    return { id: roomId, admin_did: adminDid, created_at: createdAt, e2e_enabled: 0, revision: EMPTY_REVISION };
  }

  /** Returns the revision for a room. Always present — set at creation and updated on every commit. */
  getRoomRevision(roomId: string): string {
    const room = this.getRoom(roomId);
    return room?.revision ?? EMPTY_REVISION;
  }

  setE2eEnabled(roomId: string, enabled: boolean): void {
    this.stmts.setE2e.run(enabled ? 1 : 0, roomId);
  }

  // ---- acl ----

  getAcl(roomId: string): AclRow[] {
    return this.stmts.getAcl.all(roomId) as AclRow[];
  }

  isMember(roomId: string, did: string): boolean {
    return this.stmts.isMember.get(roomId, did) !== undefined;
  }

  addAcl(roomId: string, did: string): void {
    this.stmts.addAcl.run(roomId, did, new Date().toISOString());
  }

  removeAcl(roomId: string, did: string): void {
    this.stmts.removeAcl.run(roomId, did);
  }

  setX25519PublicKey(roomId: string, did: string, x25519PublicKey: string): void {
    this.stmts.setX25519.run(x25519PublicKey, roomId, did);
  }

  getX25519PublicKey(roomId: string, did: string): string | null {
    const row = this.stmts.getX25519.get(roomId, did) as { x25519_public_key: string | null } | undefined;
    return row?.x25519_public_key ?? null;
  }

  // ---- links (active OR-Set) ----

  /** Returns true if the link was actually inserted (false if it already existed). */
  insertLink(
    roomId: string,
    linkHash: string,
    linkData: string,
    sequence: number
  ): boolean {
    const info = this.stmts.insertLink.run(roomId, linkHash, linkData, sequence);
    return info.changes > 0;
  }

  /** Returns true if a link was actually removed (false if it did not exist). */
  removeLink(roomId: string, linkHash: string): boolean {
    const info = this.stmts.removeLink.run(roomId, linkHash);
    return info.changes > 0;
  }

  getActiveLinkRows(roomId: string): LinkRow[] {
    return this.stmts.getActiveLinkRows.all(roomId) as LinkRow[];
  }

  getActiveLinks(roomId: string): LinkExpression[] {
    return this.getActiveLinkRows(roomId).map(
      (row) => JSON.parse(row.link_data) as LinkExpression
    );
  }

  // ---- diffs (append-only log) ----

  getNextSequence(roomId: string): number {
    const row = this.stmts.getNextSequence.get(roomId) as { next: number };
    return row.next;
  }

  getMaxSequence(roomId: string): number {
    const row = this.stmts.getMaxSequence.get(roomId) as { max: number };
    return row.max;
  }

  appendDiff(
    roomId: string,
    sequence: number,
    diffData: string,
    authorDid: string
  ): void {
    this.stmts.appendDiff.run(roomId, sequence, diffData, new Date().toISOString(), authorDid);
  }

  getDiffsSince(roomId: string, since: number): DiffRow[] {
    return this.stmts.getDiffsSince.all(roomId, since) as DiffRow[];
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
   * append-only diff log, all in one transaction.
   *
   * Revision is maintained incrementally via XOR — O(1) per link, regardless
   * of room size. XOR is commutative and self-inverse: adding a hash XORs
   * it in, removing it XORs it back out.
   */
  applyDiffAndAppend(
    roomId: string,
    diff: PerspectiveDiff,
    authorDid: string
  ): { sequence: number; revision: string } {
    const run = this.raw.transaction(() => {
      const sequence = this.getNextSequence(roomId);
      let revision = this.getRoomRevision(roomId);
      for (const link of diff.additions) {
        const hash = linkHash(link);
        const inserted = this.insertLink(roomId, hash, JSON.stringify(link), sequence);
        if (inserted) revision = xorHex(revision, hash);
      }
      for (const link of diff.removals) {
        const hash = linkHash(link);
        const removed = this.removeLink(roomId, hash);
        if (removed) revision = xorHex(revision, hash);
      }
      this.appendDiff(roomId, sequence, JSON.stringify(diff), authorDid);
      this.stmts.updateRevision.run(revision, roomId);
      if (this.maxDiffsPerRoom > 0) {
        this.pruneOldDiffs(roomId, this.maxDiffsPerRoom);
      }
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
    this.stmts.createSession.run(token, roomId, did, expiresAt);
  }

  getSession(token: string): SessionRow | undefined {
    return this.stmts.getSession.get(token) as SessionRow | undefined;
  }

  deleteSessionsForDid(roomId: string, did: string): void {
    this.stmts.deleteSessionsForDid.run(roomId, did);
  }

  // ---- room keys (E2E) ----

  addRoomKey(
    roomId: string,
    did: string,
    version: number,
    encryptedKey: string
  ): void {
    this.stmts.addRoomKey.run(roomId, did, encryptedKey, version);
  }

  getAllRoomKeys(roomId: string, did: string): RoomKeyRow[] {
    return this.stmts.getAllRoomKeys.all(roomId, did) as RoomKeyRow[];
  }

  getLatestKeyVersion(roomId: string): number {
    const row = this.stmts.getLatestKeyVersion.get(roomId) as { max: number };
    return row.max;
  }

  /** Returns a map of DID → version numbers for all members who have any keys in this room. */
  getAllMemberKeyVersions(roomId: string): Map<string, number[]> {
    const rows = this.stmts.getAllMemberKeyVersions.all(roomId) as { did: string; version: number }[];
    const result = new Map<string, number[]>();
    for (const row of rows) {
      const versions = result.get(row.did);
      if (versions) {
        versions.push(row.version);
      } else {
        result.set(row.did, [row.version]);
      }
    }
    return result;
  }

  /** INSERT OR IGNORE — stores a sealed key only if that (room, did, version) doesn't exist yet. */
  addRoomKeyIfMissing(roomId: string, did: string, version: number, encryptedKey: string): boolean {
    const info = this.stmts.addRoomKeyIfMissing.run(roomId, did, encryptedKey, version);
    return info.changes > 0;
  }

  // ---- server identity ----

  getIdentity(keyType: string): ServerIdentityRow | undefined {
    return this.stmts.getIdentity.get(keyType) as ServerIdentityRow | undefined;
  }

  setIdentity(keyType: string, publicKey: string, privateKey: string): void {
    this.stmts.setIdentity.run(keyType, publicKey, privateKey);
  }

  // ---- diff retention ----

  /** Prune the oldest diffs for a room, keeping at most `maxDiffs` entries. */
  pruneOldDiffs(roomId: string, maxDiffs: number): void {
    const row = this.stmts.countDiffs.get(roomId) as { cnt: number };
    if (row.cnt <= maxDiffs) return;
    const cutoff = this.stmts.getOldestRetainedSeq.get(roomId, maxDiffs) as { sequence: number } | undefined;
    if (cutoff) {
      this.stmts.deleteDiffsBefore.run(roomId, cutoff.sequence);
    }
  }

  // ---- session sweep ----

  /** Delete all sessions whose expiry has passed. Returns the count removed. */
  sweepExpiredSessions(): number {
    const info = this.stmts.deleteExpiredSessions.run(new Date().toISOString());
    return info.changes;
  }

}
