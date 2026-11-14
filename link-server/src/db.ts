import Database from "better-sqlite3";
import { mkdirSync } from "node:fs";
import { dirname } from "node:path";
import {
  EMPTY_REVISION,
  linkHash,
  xorHex,
  type PerspectiveDiff,
} from "./types.js";

const SCHEMA = `
CREATE TABLE IF NOT EXISTS rooms (
  id TEXT PRIMARY KEY,
  admin_did TEXT NOT NULL,
  created_at TEXT NOT NULL,
  revision TEXT
);
-- Protocol 1's ACL (acl, acl_changes): no longer read or written. Kept so a database from
-- before protocol 2 opens unchanged; its rooms are legacy and answer 410.
CREATE TABLE IF NOT EXISTS acl (
  room_id TEXT,
  did TEXT,
  added_at TEXT,
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
-- The admin's commitment to each room key version: a member keeps an opened key only if it
-- matches, so a member cannot grant a wrong key that decrypts.
CREATE TABLE IF NOT EXISTS room_key_commitments (
  room_id TEXT,
  version INTEGER,
  commitment TEXT,
  PRIMARY KEY (room_id, version)
);
CREATE TABLE IF NOT EXISTS server_identity (
  key_type TEXT PRIMARY KEY,
  public_key TEXT,
  private_key TEXT
);
CREATE INDEX IF NOT EXISTS idx_links_room ON links(room_id);
-- The keyset of a paged /render: (sequence, link_hash) within a room.
CREATE INDEX IF NOT EXISTS idx_links_room_seq_hash ON links(room_id, sequence, link_hash);
CREATE INDEX IF NOT EXISTS idx_diffs_room_seq ON diffs(room_id, sequence);
CREATE INDEX IF NOT EXISTS idx_acl_room ON acl(room_id);
CREATE INDEX IF NOT EXISTS idx_sessions_room_did ON sessions(room_id, did);
CREATE TABLE IF NOT EXISTS acl_changes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  room_id TEXT NOT NULL,
  did TEXT NOT NULL,
  action TEXT NOT NULL,
  actor TEXT NOT NULL,
  source TEXT NOT NULL,
  at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_acl_changes_room ON acl_changes(room_id, id);
-- Protocol 2: the counted membership links of a room, in the order they counted. judged is the
-- identity core's JudgedLink JSON the replay takes; link the signed link as it arrived.
CREATE TABLE IF NOT EXISTS membership_links (
  room_id TEXT NOT NULL,
  seq INTEGER NOT NULL,
  hash TEXT NOT NULL,
  link TEXT NOT NULL,
  judged TEXT NOT NULL,
  PRIMARY KEY (room_id, seq),
  UNIQUE (room_id, hash)
);
-- Protocol 2: every key that authenticated to a room, whom it speaks for (from the identity
-- store, at its last auth or refresh) and its signed X25519 key for room keys.
CREATE TABLE IF NOT EXISTS room_member_keys (
  room_id TEXT NOT NULL,
  did TEXT NOT NULL,
  person TEXT,
  by_bot INTEGER NOT NULL DEFAULT 0,
  x25519 TEXT NOT NULL,
  x25519_sig TEXT NOT NULL,
  revoked INTEGER NOT NULL DEFAULT 0,
  updated_at TEXT NOT NULL,
  PRIMARY KEY (room_id, did)
);
CREATE INDEX IF NOT EXISTS idx_room_member_keys_did ON room_member_keys(did);
`;

export interface RoomRow {
  id: string;
  /** Protocol 2: the creator's person DID (the first admin). Protocol 1: the admin's did:key. */
  admin_did: string;
  created_at: string;
  revision: string | null;
  /** 1 for a room made under protocol 1 (an ACL room): refused with 410. */
  legacy: number;
  /** 1 while the operator has suspended the room. */
  suspended: number;
}

/** A key that authenticated to a room (protocol 2). */
export interface MemberKeyRow {
  room_id: string;
  did: string;
  /** The person the key speaks for; null for a device-only key. */
  person: string | null;
  by_bot: number;
  x25519: string;
  x25519_sig: string;
  revoked: number;
  updated_at: string;
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
  /** The person the key spoke for at auth; null for a device-only key. */
  person: string | null;
  by_bot: number;
}

export interface RoomKeyRow {
  room_id: string;
  did: string;
  encrypted_key: string;
  version: number;
  /** The version's commitment; null for a version rotated before commitments. */
  commitment: string | null;
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

  // Cached prepared statements — prepared once, reused for the lifetime of
  // the DB connection. Avoids re-parsing SQL on every call (particularly
  // inside applyDiffAndAppend where 1500 links per transaction would
  // otherwise prepare the same INSERT 1500 times).
  private readonly stmts: ReturnType<LinkServerDB["prepareStatements"]>;

  constructor(filePath: string) {
    if (filePath !== ":memory:") {
      mkdirSync(dirname(filePath), { recursive: true });
    }
    this.raw = new Database(filePath);
    this.raw.pragma("journal_mode = WAL");
    this.raw.pragma("foreign_keys = ON");
    this.raw.exec(SCHEMA);
    this.migrate();
    this.stmts = this.prepareStatements();
  }

  /**
   * Brings a database from before protocol 2 up to date. Every room it already holds was made
   * under protocol 1 (an ACL room): it is marked legacy and answers 410 from now on.
   */
  private migrate(): void {
    const columns = (table: string) =>
      new Set((this.raw.prepare(`PRAGMA table_info(${table})`).all() as Array<{ name: string }>).map((c) => c.name));
    const rooms = columns("rooms");
    const run = this.raw.transaction(() => {
      if (!rooms.has("legacy")) {
        this.raw.exec("ALTER TABLE rooms ADD COLUMN legacy INTEGER NOT NULL DEFAULT 0");
        this.raw.exec("UPDATE rooms SET legacy = 1");
      }
      if (!rooms.has("suspended")) {
        this.raw.exec("ALTER TABLE rooms ADD COLUMN suspended INTEGER NOT NULL DEFAULT 0");
      }
      const sessions = columns("sessions");
      if (!sessions.has("person")) this.raw.exec("ALTER TABLE sessions ADD COLUMN person TEXT");
      if (!sessions.has("by_bot")) {
        this.raw.exec("ALTER TABLE sessions ADD COLUMN by_bot INTEGER NOT NULL DEFAULT 0");
      }
    });
    run();
  }

  private prepareStatements() {
    return {
      getRoom: this.raw.prepare("SELECT * FROM rooms WHERE id = ?"),
      listRooms: this.raw.prepare("SELECT * FROM rooms ORDER BY created_at DESC"),
      createRoom: this.raw.prepare(
        "INSERT OR IGNORE INTO rooms (id, admin_did, created_at, revision, legacy, suspended) VALUES (?, ?, ?, ?, 0, 0)"
      ),
      updateRevision: this.raw.prepare("UPDATE rooms SET revision = ? WHERE id = ?"),
      setSuspended: this.raw.prepare("UPDATE rooms SET suspended = ? WHERE id = ?"),

      insertMembershipLink: this.raw.prepare(
        "INSERT INTO membership_links (room_id, seq, hash, link, judged) VALUES (?, ?, ?, ?, ?)"
      ),
      getMembershipSeq: this.raw.prepare(
        "SELECT COALESCE(MAX(seq), 0) AS max FROM membership_links WHERE room_id = ?"
      ),
      hasMembershipLink: this.raw.prepare("SELECT 1 FROM membership_links WHERE room_id = ? AND hash = ?"),
      getMembershipJudged: this.raw.prepare(
        "SELECT judged FROM membership_links WHERE room_id = ? ORDER BY seq ASC"
      ),
      getMembershipSince: this.raw.prepare(
        "SELECT seq, link FROM membership_links WHERE room_id = ? AND seq > ? ORDER BY seq ASC LIMIT ?"
      ),

      upsertMemberKey: this.raw.prepare(
        `INSERT INTO room_member_keys (room_id, did, person, by_bot, x25519, x25519_sig, revoked, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, 0, ?)
         ON CONFLICT (room_id, did) DO UPDATE SET person = excluded.person, by_bot = excluded.by_bot,
           x25519 = excluded.x25519, x25519_sig = excluded.x25519_sig, revoked = 0, updated_at = excluded.updated_at`
      ),
      getMemberKeys: this.raw.prepare("SELECT * FROM room_member_keys WHERE room_id = ? ORDER BY did"),
      getMemberKey: this.raw.prepare("SELECT * FROM room_member_keys WHERE room_id = ? AND did = ?"),
      getKeysOfPerson: this.raw.prepare(
        "SELECT * FROM room_member_keys WHERE room_id = ? AND ((person = ? AND by_bot = 0) OR did = ?)"
      ),
      getRoomsOfKey: this.raw.prepare("SELECT room_id FROM room_member_keys WHERE did = ? AND revoked = 0"),
      markKeyRevoked: this.raw.prepare(
        "UPDATE room_member_keys SET revoked = 1, updated_at = ? WHERE did = ? AND revoked = 0"
      ),
      markKeyRevokedInRoom: this.raw.prepare(
        "UPDATE room_member_keys SET revoked = 1, updated_at = ? WHERE room_id = ? AND did = ?"
      ),

      insertLink: this.raw.prepare(
        `INSERT OR IGNORE INTO links (room_id, link_hash, link_data, sequence)
         VALUES (?, ?, ?, ?)`
      ),
      removeLink: this.raw.prepare("DELETE FROM links WHERE room_id = ? AND link_hash = ?"),
      getActiveLinkPage: this.raw.prepare(
        `SELECT * FROM links
         WHERE room_id = ? AND sequence <= ? AND (sequence > ? OR (sequence = ? AND link_hash > ?))
         ORDER BY sequence ASC, link_hash ASC LIMIT ?`
      ),

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
      getDiffPageSince: this.raw.prepare(
        "SELECT * FROM diffs WHERE room_id = ? AND sequence > ? ORDER BY sequence ASC LIMIT ?"
      ),

      createSession: this.raw.prepare(
        `INSERT OR REPLACE INTO sessions (token, room_id, did, expires_at, person, by_bot)
         VALUES (?, ?, ?, ?, ?, ?)`
      ),
      getSession: this.raw.prepare("SELECT * FROM sessions WHERE token = ?"),
      deleteSessionsForDid: this.raw.prepare("DELETE FROM sessions WHERE room_id = ? AND did = ?"),
      deleteSessionsForKey: this.raw.prepare("DELETE FROM sessions WHERE did = ?"),
      deleteSessionsForRoom: this.raw.prepare("DELETE FROM sessions WHERE room_id = ?"),

      addRoomKey: this.raw.prepare(
        `INSERT OR REPLACE INTO room_keys (room_id, did, encrypted_key, version)
         VALUES (?, ?, ?, ?)`
      ),
      getAllRoomKeys: this.raw.prepare(
        `SELECT k.*, c.commitment FROM room_keys k
         LEFT JOIN room_key_commitments c ON c.room_id = k.room_id AND c.version = k.version
         WHERE k.room_id = ? AND k.did = ? ORDER BY k.version ASC`
      ),
      getLatestKeyVersion: this.raw.prepare(
        `SELECT COALESCE(MAX(version), 0) AS max FROM (
           SELECT version FROM room_keys WHERE room_id = ?
           UNION ALL SELECT version FROM room_key_commitments WHERE room_id = ?)`
      ),
      addKeyCommitment: this.raw.prepare(
        `INSERT INTO room_key_commitments (room_id, version, commitment) VALUES (?, ?, ?)`
      ),
      deleteRoomKey: this.raw.prepare(
        `DELETE FROM room_keys WHERE room_id = ? AND did = ? AND version = ?`
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

  /** Every room, newest first. Operator listing only. */
  listRooms(): RoomRow[] {
    return this.stmts.listRooms.all() as RoomRow[];
  }

  /**
   * Creates a protocol 2 room with its first counted membership links, in one transaction.
   * `creator` is the creator's person DID. False when the room already exists (nothing
   * changes). Every room is end-to-end encrypted: the commit route refuses plaintext links.
   */
  createRoomWithMembership(
    roomId: string,
    creator: string,
    links: Array<{ hash: string; link: string; judged: string }>
  ): boolean {
    const run = this.raw.transaction(() => {
      const created = this.stmts.createRoom.run(roomId, creator, new Date().toISOString(), EMPTY_REVISION).changes > 0;
      if (created) this.insertMembershipLinks(roomId, links);
      return created;
    });
    return run();
  }

  /** Returns the revision for a room. Always present — set at creation and updated on every commit. */
  getRoomRevision(roomId: string): string {
    const room = this.getRoom(roomId);
    return room?.revision ?? EMPTY_REVISION;
  }

  /** Suspends or resumes a room; false when there is no such room. */
  setSuspended(roomId: string, suspended: boolean): boolean {
    return this.stmts.setSuspended.run(suspended ? 1 : 0, roomId).changes > 0;
  }

  // ---- membership links (protocol 2) ----

  /** Appends counted links after the room's last seq, in one transaction: their seqs. */
  insertMembershipLinks(roomId: string, links: Array<{ hash: string; link: string; judged: string }>): number[] {
    const run = this.raw.transaction(() => {
      let seq = this.getMembershipSeq(roomId);
      return links.map((l) => {
        seq += 1;
        this.stmts.insertMembershipLink.run(roomId, seq, l.hash, l.link, l.judged);
        return seq;
      });
    });
    return run();
  }

  /** The seq of the room's last counted membership link (0 for none). */
  getMembershipSeq(roomId: string): number {
    return (this.stmts.getMembershipSeq.get(roomId) as { max: number }).max;
  }

  hasMembershipLink(roomId: string, hash: string): boolean {
    return this.stmts.hasMembershipLink.get(roomId, hash) !== undefined;
  }

  /** The JudgedLink JSON of every counted link, in the order they counted. */
  getMembershipJudged(roomId: string): string[] {
    return (this.stmts.getMembershipJudged.all(roomId) as Array<{ judged: string }>).map((r) => r.judged);
  }

  /** At most `limit` counted links after `since`, in seq order. */
  getMembershipSince(roomId: string, since: number, limit: number): Array<{ seq: number; link: string }> {
    return this.stmts.getMembershipSince.all(roomId, since, limit) as Array<{ seq: number; link: string }>;
  }

  // ---- keys of a room (protocol 2) ----

  /** Records (or refreshes) a key that authenticated to a room: whom it speaks for and its X25519 key. */
  upsertMemberKey(
    roomId: string,
    did: string,
    person: string | null,
    byBot: boolean,
    x25519: string,
    x25519Sig: string
  ): void {
    this.stmts.upsertMemberKey.run(roomId, did, person, byBot ? 1 : 0, x25519, x25519Sig, new Date().toISOString());
  }

  getMemberKeys(roomId: string): MemberKeyRow[] {
    return this.stmts.getMemberKeys.all(roomId) as MemberKeyRow[];
  }

  getMemberKey(roomId: string, did: string): MemberKeyRow | undefined {
    return this.stmts.getMemberKey.get(roomId, did) as MemberKeyRow | undefined;
  }

  /** The keys in a room that speak for `person` (not as a bot), or the bot key `person` itself. */
  getKeysOfPerson(roomId: string, person: string): MemberKeyRow[] {
    return this.stmts.getKeysOfPerson.all(roomId, person, person) as MemberKeyRow[];
  }

  /** The rooms a key (not revoked) authenticated to. */
  getRoomsOfKey(did: string): string[] {
    return (this.stmts.getRoomsOfKey.all(did) as Array<{ room_id: string }>).map((r) => r.room_id);
  }

  /** Marks a key revoked in every room (`roomId` absent) or one room. */
  markKeyRevoked(did: string, roomId?: string): void {
    const at = new Date().toISOString();
    if (roomId === undefined) this.stmts.markKeyRevoked.run(at, did);
    else this.stmts.markKeyRevokedInRoom.run(at, roomId, did);
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

  /**
   * One page of the active links that were added at or before `upto`, in
   * `(sequence, link_hash)` order, strictly after the cursor `after`
   * (`{sequence: 0, hash: ""}` for the first page). A link added after
   * `upto` gets a sequence above it, so the pages of one `upto` never shift:
   * links removed meanwhile drop out, and the diff log after `upto` carries
   * every change the pages miss.
   */
  getActiveLinkPage(
    roomId: string,
    upto: number,
    after: { sequence: number; hash: string },
    limit: number
  ): LinkRow[] {
    return this.stmts.getActiveLinkPage.all(
      roomId, upto, after.sequence, after.sequence, after.hash, limit
    ) as LinkRow[];
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

  /** At most `limit` diffs after `since`, in sequence order. */
  getDiffPageSince(roomId: string, since: number, limit: number): DiffRow[] {
    return this.stmts.getDiffPageSince.all(roomId, since, limit) as DiffRow[];
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
      return { sequence, revision };
    });
    return run();
  }

  // ---- sessions ----

  createSession(
    token: string,
    roomId: string,
    did: string,
    expiresAt: string,
    person: string | null = null,
    byBot = false
  ): void {
    this.stmts.createSession.run(token, roomId, did, expiresAt, person, byBot ? 1 : 0);
  }

  getSession(token: string): SessionRow | undefined {
    return this.stmts.getSession.get(token) as SessionRow | undefined;
  }

  deleteSessionsForDid(roomId: string, did: string): void {
    this.stmts.deleteSessionsForDid.run(roomId, did);
  }

  /** Ends a key's sessions in every room. */
  deleteSessionsForKey(did: string): void {
    this.stmts.deleteSessionsForKey.run(did);
  }

  deleteSessionsForRoom(roomId: string): void {
    this.stmts.deleteSessionsForRoom.run(roomId);
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
    const row = this.stmts.getLatestKeyVersion.get(roomId, roomId) as { max: number };
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

  addKeyCommitment(roomId: string, version: number, commitment: string): void {
    this.stmts.addKeyCommitment.run(roomId, version, commitment);
  }

  /** Deletes one member's box for one version; true when there was one. */
  deleteRoomKey(roomId: string, did: string, version: number): boolean {
    return this.stmts.deleteRoomKey.run(roomId, did, version).changes > 0;
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

  // ---- session sweep ----

  /** Delete all sessions whose expiry has passed. Returns the count removed. */
  sweepExpiredSessions(): number {
    const info = this.stmts.deleteExpiredSessions.run(new Date().toISOString());
    return info.changes;
  }

}
