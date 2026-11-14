import { randomBytes } from "node:crypto";
import { mkdtempSync, rmSync } from "node:fs";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import * as ed from "@noble/ed25519";
import { gcm } from "@noble/ciphers/aes.js";
import WebSocket from "ws";
import { publicKeyToDid, signHex } from "../src/auth.js";
import { generateRoomKey, roomKeyCommitment, sealToDid } from "./sealed-box.js";
import { buildServer, type ServerOptions } from "../src/server.js";
import {
  authBody,
  FakeStore,
  links as membershipLinks,
  newSalt,
  personAgent,
  roomIdOf,
  signLink,
  STORE_HEAD_KEY,
  type TestAgent,
} from "./identity.js";
import {
  canonicalLinkPayload,
  isEncryptedLinkData,
  linkHash as computeLinkHash,
  sha256Hex,
  type LinkData,
  type LinkExpression,
} from "../src/types.js";


export type { TestAgent } from "./identity.js";

/**
 * A fresh device-only key: no log lists it, so it speaks for no person. On an auto-admit
 * server (the `startTestServer` default) it is admitted to every room it authenticates to.
 */
export async function createTestAgent(): Promise<TestAgent> {
  const privateKey = ed.utils.randomPrivateKey();
  const publicKey = await ed.getPublicKeyAsync(privateKey);
  const did = publicKeyToDid(publicKey);
  return { did, privateKey, publicKey };
}

/** The room key the helpers encrypt links with, for tests that do not rotate their own. The server never sees it. */
export const TEST_ROOM_KEY = generateRoomKey();

/** A LinkExpression as an agent holds it, before encryption or after `openLink`. */
export type PlainLink = LinkExpression & { data: LinkData };

/**
 * Builds a signed LinkExpression and encrypts it for the wire, as the server link language does
 * (`encryptLinkForWire`): the server receives only `{data: {ciphertext, nonce}, link_hash, key_version}`,
 * where `link_hash` is the SHA-256 of the canonical plaintext fields. Open it with `openLink`.
 */
export async function createSignedLink(
  agent: TestAgent,
  data: LinkData,
  timestamp: string = new Date().toISOString(),
  roomKey: Uint8Array = TEST_ROOM_KEY,
  keyVersion = 1
): Promise<LinkExpression> {
  const unsigned: PlainLink = { author: agent.did, timestamp, data };
  const payload = canonicalLinkPayload(unsigned);
  const signature = await signHex(agent.privateKey, payload);
  const signed: PlainLink = { ...unsigned, proof: { signature, key: `${agent.did}#primary` } };
  const nonce = new Uint8Array(randomBytes(12));
  const ciphertext = gcm(roomKey, nonce).encrypt(new TextEncoder().encode(JSON.stringify(signed)));
  return {
    data: { ciphertext: Buffer.from(ciphertext).toString("base64"), nonce: Buffer.from(nonce).toString("base64") },
    link_hash: sha256Hex(payload),
    key_version: keyVersion,
  };
}

/** Decrypts a wire link `createSignedLink` built back to the signed LinkExpression. */
export function openLink(link: LinkExpression, roomKey: Uint8Array = TEST_ROOM_KEY): PlainLink {
  if (!isEncryptedLinkData(link.data)) throw new Error("openLink: not an encrypted link");
  const nonce = new Uint8Array(Buffer.from(link.data.nonce, "base64"));
  const plaintext = gcm(roomKey, nonce).decrypt(new Uint8Array(Buffer.from(link.data.ciphertext, "base64")));
  return JSON.parse(new TextDecoder().decode(plaintext)) as PlainLink;
}

export function linkHashOf(link: LinkExpression): string {
  return computeLinkHash(link);
}

type Built = Awaited<ReturnType<typeof buildServer>>;

export interface TestServerHandle {
  url: string;
  wsUrl: string;
  dataDir: string;
  built: Built;
  /** The fake identity store the server reads key logs from. */
  store: FakeStore;
  close: () => Promise<void>;
}

/**
 * Boots a fresh server on a random port with a temp data directory and a fake identity store
 * (`tests/identity.ts`). Always call `close()`.
 *
 * Auto-admit is on unless `opts.autoAdmit` says otherwise: the suites about sync, paging,
 * encryption and sockets authenticate fresh device-only keys, which only auto-admit lets in.
 * The membership suites (`membership`, `rotate`, `revocation`, `admission`, `operator`) turn
 * it off and sign with the fixture's persons.
 *
 * The server takes auth signed for the origin a request names in its Host header
 * (`hostOrigin`) unless `opts` says otherwise: the port is random, so `url` is known only once
 * it listens. The origin suites set `publicOrigins` and turn it off.
 */
export async function startTestServer(opts: Partial<ServerOptions> = {}): Promise<TestServerHandle> {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-test-"));
  const store = await FakeStore.start();
  const built = await buildServer({
    dataDir,
    logger: false,
    telepresenceGraceMs: 300,
    autoAdmit: true,
    hostOrigin: true,
    storeUrl: store.url,
    storeHeadKeys: [STORE_HEAD_KEY],
    ...opts,
  });
  await built.app.listen({ port: 0, host: "127.0.0.1" });
  const address = built.app.server.address() as AddressInfo;
  const url = `http://127.0.0.1:${address.port}`;
  const wsUrl = `ws://127.0.0.1:${address.port}`;
  return {
    url,
    wsUrl,
    dataDir,
    built,
    store,
    close: async () => {
      await built.close();
      await store.close();
      rmSync(dataDir, { recursive: true, force: true });
    },
  };
}

export interface JsonResponse<T> {
  status: number;
  body: T;
}

export async function postJson<T = unknown>(
  url: string,
  body: unknown,
  token?: string
): Promise<JsonResponse<T>> {
  const res = await fetch(url, {
    method: "POST",
    headers: {
      "content-type": "application/json",
      ...(token ? { authorization: `Bearer ${token}` } : {}),
    },
    body: JSON.stringify(body),
  });
  const text = await res.text();
  return { status: res.status, body: (text ? JSON.parse(text) : undefined) as T };
}

export async function getJson<T = unknown>(url: string, token?: string): Promise<JsonResponse<T>> {
  const res = await fetch(url, {
    headers: token ? { authorization: `Bearer ${token}` } : {},
  });
  const text = await res.text();
  return { status: res.status, body: (text ? JSON.parse(text) : undefined) as T };
}

export interface AuthAnswer {
  token?: string;
  expiresAt?: string;
  admitted?: boolean;
  error?: string;
}

/** `POST /rooms/:roomId/auth` (protocol 2) for `agent`: the raw answer. */
export async function authAs(serverUrl: string, roomId: string, agent: TestAgent): Promise<JsonResponse<AuthAnswer>> {
  return postJson<AuthAnswer>(`${serverUrl}/rooms/${roomId}/auth`, authBody(agent, serverUrl, roomId));
}

/**
 * Authenticates `agent` to an existing room (protocol 2) and returns the token. Throws when the
 * server refuses, or when `requireAdmitted` and the key is not admitted.
 */
export async function authenticateAgent(
  serverUrl: string,
  roomId: string,
  agent: TestAgent,
  requireAdmitted = true
): Promise<string> {
  const res = await authAs(serverUrl, roomId, agent);
  if (res.status !== 200 || !res.body.token) {
    throw new Error(`auth failed: ${res.status} ${JSON.stringify(res.body)}`);
  }
  if (requireAdmitted && !res.body.admitted) {
    throw new Error(`auth: ${agent.did} is not admitted to ${roomId}`);
  }
  return res.body.token;
}

export interface CreateRoomOptions {
  /** The creator's device key (default alice's d1). */
  creator?: TestAgent;
  join?: "open" | "invite";
  add?: "admins" | "members";
  salt?: string;
}

/**
 * Creates a room as protocol 2 does: the first `POST /membership` with `genesis` and the
 * creator's `rules` link. Returns the room id, `base58(sha256(creator ‖ 0x00 ‖ salt))`.
 */
export async function createRoom(serverUrl: string, opts: CreateRoomOptions = {}): Promise<string> {
  const creator = opts.creator ?? personAgent("alice");
  if (!creator.person) throw new Error("createRoom: the creator must be a person's key");
  const salt = opts.salt ?? newSalt();
  const roomId = roomIdOf(creator.person, salt);
  const res = await postJson(`${serverUrl}/rooms/${roomId}/membership`, {
    genesis: { creator: creator.person, salt },
    links: [signLink(creator, membershipLinks.rules({ join: opts.join ?? "open", add: opts.add }))],
  });
  if (res.status !== 200) throw new Error(`genesis failed: ${res.status} ${JSON.stringify(res.body)}`);
  return roomId;
}

/**
 * Opens a WebSocket to the server without sending the auth message.
 * Use `openAuthenticatedWs` for the full handshake.
 */
export function openWs(wsUrl: string, roomId: string): WebSocket {
  return new WebSocket(`${wsUrl}/rooms/${roomId}/ws`);
}

export function waitForOpen(socket: WebSocket): Promise<void> {
  return new Promise((resolve, reject) => {
    socket.once("open", () => resolve());
    socket.once("error", reject);
  });
}

/**
 * Opens a WebSocket, completes first-message auth, and waits for the
 * `online-agents` acknowledgement. Returns the connected + authenticated socket.
 */
export async function openAuthenticatedWs(wsUrl: string, roomId: string, token: string): Promise<WebSocket> {
  const ws = openWs(wsUrl, roomId);
  await waitForOpen(ws);
  ws.send(JSON.stringify({ type: "auth", token }));
  // Wait for the server to confirm auth with an online-agents message.
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error("WS auth timed out")), 5000);
    const onMsg = (raw: WebSocket.RawData) => {
      try {
        const msg = JSON.parse(raw.toString());
        if (msg.type === "online-agents") {
          clearTimeout(timeout);
          ws.removeListener("message", onMsg);
          resolve();
        } else if (msg.type === "auth-error") {
          clearTimeout(timeout);
          ws.removeListener("message", onMsg);
          reject(new Error(`WS auth error: ${msg.error}`));
        }
      } catch { /* ignore parse errors */ }
    };
    ws.on("message", onMsg);
    ws.once("close", () => {
      clearTimeout(timeout);
      reject(new Error("WS closed during auth"));
    });
  });
  return ws;
}

export async function waitFor(
  predicate: () => boolean | Promise<boolean>,
  timeoutMs = 3000,
  intervalMs = 20
): Promise<void> {
  const start = Date.now();
  while (!(await predicate())) {
    if (Date.now() - start > timeoutMs) {
      throw new Error("waitFor: condition not met within timeout");
    }
    await new Promise((resolve) => setTimeout(resolve, intervalMs));
  }
}

export interface MessageCollector {
  messages: Record<string, unknown>[];
  waitForCount: (n: number, timeoutMs?: number) => Promise<Record<string, unknown>[]>;
  waitForType: (type: string, timeoutMs?: number) => Promise<Record<string, unknown>>;
}

export function collectMessages(socket: WebSocket): MessageCollector {
  const messages: Record<string, unknown>[] = [];
  socket.on("message", (raw) => {
    try {
      messages.push(JSON.parse(raw.toString()));
    } catch {
      // ignore malformed frames
    }
  });
  return {
    messages,
    waitForCount: async (n, timeoutMs = 3000) => {
      await waitFor(() => messages.length >= n, timeoutMs);
      return messages;
    },
    waitForType: async (type, timeoutMs = 3000) => {
      await waitFor(() => messages.some((m) => m.type === type), timeoutMs);
      return messages.find((m) => m.type === type)!;
    },
  };
}

// ---------------------------------------------------------------------------
// Client-side E2E key rotation — mirrors production performRotation() flow
// ---------------------------------------------------------------------------

export interface ClientRotateResult {
  version: number;
  recipients: string[];
  roomKey: Uint8Array;
  membersNeedingHistoricalKeys?: Array<{ did: string; missingVersions: number[] }>;
}

/** The admitted keys of a room (`GET /members`): what a rotation seals to. */
export async function admittedKeys(serverUrl: string, roomId: string, token: string): Promise<string[]> {
  const res = await getJson<{ keys: Array<{ did: string }> }>(`${serverUrl}/rooms/${roomId}/members`, token);
  if (res.status !== 200) {
    throw new Error(`members fetch failed: ${res.status} ${JSON.stringify(res.body)}`);
  }
  return res.body.keys.map((k) => k.did);
}

/**
 * Performs a client-side key rotation: generates the room key locally,
 * seals it to every admitted key of the room (whether or not it is
 * connected), and POSTs only the sealed boxes. The plaintext room key never
 * leaves this function's scope (returned for test assertions only).
 */
export async function clientSideRotate(
  serverUrl: string,
  roomId: string,
  adminToken: string,
): Promise<ClientRotateResult> {
  const roomKey = generateRoomKey();

  const members = await getJson<{ keys: Array<{ did: string }>; latestKeyVersion: number }>(
    `${serverUrl}/rooms/${roomId}/members`,
    adminToken,
  );
  if (members.status !== 200) {
    throw new Error(`members fetch failed: ${members.status} ${JSON.stringify(members.body)}`);
  }
  const dids = members.body.keys.map((k) => k.did);
  const version = members.body.latestKeyVersion + 1;
  const keys = dids.map((did) => ({ did, encryptedKey: sealToDid(did, roomKey) }));

  const rotateRes = await postJson<{
    version: number;
    recipients: string[];
    membersNeedingHistoricalKeys?: Array<{ did: string; missingVersions: number[] }>;
  }>(`${serverUrl}/rooms/${roomId}/keys/rotate`, { keys, commitment: roomKeyCommitment(roomKey), version }, adminToken);

  if (rotateRes.status !== 200) {
    throw new Error(`rotate failed: ${rotateRes.status} ${JSON.stringify(rotateRes.body)}`);
  }

  return {
    version: rotateRes.body.version,
    recipients: rotateRes.body.recipients,
    roomKey,
    membersNeedingHistoricalKeys: rotateRes.body.membersNeedingHistoricalKeys,
  };
}
