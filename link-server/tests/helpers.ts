import { mkdtempSync, rmSync } from "node:fs";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import * as ed from "@noble/ed25519";
import WebSocket from "ws";
import { edwardsToMontgomeryPriv, edwardsToMontgomeryPub } from "@noble/curves/ed25519";
import { hashMessageForVerify, publicKeyToDid, signHex } from "../src/auth.js";
import { buildServer, type ServerOptions } from "../src/server.js";
import {
  canonicalLinkPayload,
  linkHash as computeLinkHash,
  type EncryptedLinkData,
  type LinkData,
  type LinkExpression,
} from "../src/types.js";

/** Everything needed to act as an AD4M agent in tests: identity + signing key. */
export interface TestAgent {
  did: string;
  privateKey: Uint8Array;
  publicKey: Uint8Array;
}

export async function createTestAgent(): Promise<TestAgent> {
  const privateKey = ed.utils.randomPrivateKey();
  const publicKey = await ed.getPublicKeyAsync(privateKey);
  const did = publicKeyToDid(publicKey);
  return { did, privateKey, publicKey };
}

export async function signChallenge(agent: TestAgent, challenge: string): Promise<string> {
  // The AD4M executor's agentSignStringHex signs SHA-256(message), not
  // the raw message bytes. Match that convention so the server can verify.
  const hashed = hashMessageForVerify(challenge);
  return signHex(agent.privateKey, hashed);
}

/** Derives the X25519 public key from a test agent's Ed25519 public key
 *  via the Edwards-to-Montgomery conversion. Used to register the agent's
 *  E2E public key with the server during auth step 2. */
export function testAgentX25519PublicKey(agent: TestAgent): string {
  const x25519Pub = edwardsToMontgomeryPub(agent.publicKey);
  return Buffer.from(x25519Pub).toString("hex");
}

/** Derives the X25519 private key from a test agent's Ed25519 private key.
 *  Matches the public key registered by `testAgentX25519PublicKey`. */
export function testAgentX25519PrivateKey(agent: TestAgent): Uint8Array {
  return edwardsToMontgomeryPriv(agent.privateKey);
}

/** Builds a fully-signed LinkExpression as a real AD4M client would. */
export async function createSignedLink(
  agent: TestAgent,
  data: LinkData | EncryptedLinkData,
  timestamp: string = new Date().toISOString()
): Promise<LinkExpression> {
  const unsigned = { author: agent.did, timestamp, data } as LinkExpression;
  const payload = canonicalLinkPayload(unsigned);
  const signature = await signHex(agent.privateKey, payload);
  return {
    ...unsigned,
    proof: { signature, key: `${agent.did}#primary` },
  };
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
  close: () => Promise<void>;
}

/** Boots a fresh server on a random port with a temp data directory. Always call `close()`. */
export async function startTestServer(opts: Partial<ServerOptions> = {}): Promise<TestServerHandle> {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-test-"));
  const built = await buildServer({
    dataDir,
    logger: false,
    telepresenceGraceMs: 300,
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
    close: async () => {
      await built.close();
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

/** Full DID challenge-response flow. Returns the issued JWT. Room is auto-created if this is the first agent. */
export async function authenticateAgent(
  serverUrl: string,
  roomId: string,
  agent: TestAgent
): Promise<string> {
  const step1 = await postJson<{ challenge?: string; error?: string }>(
    `${serverUrl}/rooms/${roomId}/auth`,
    { did: agent.did }
  );
  if (step1.status !== 200 || !step1.body.challenge) {
    throw new Error(`challenge request failed: ${step1.status} ${JSON.stringify(step1.body)}`);
  }
  const signature = await signChallenge(agent, step1.body.challenge);
  const x25519PublicKey = testAgentX25519PublicKey(agent);
  const step2 = await postJson<{ token?: string; error?: string }>(
    `${serverUrl}/rooms/${roomId}/auth`,
    { did: agent.did, challenge: step1.body.challenge, signature, x25519PublicKey }
  );
  if (step2.status !== 200 || !step2.body.token) {
    throw new Error(`auth verify failed: ${step2.status} ${JSON.stringify(step2.body)}`);
  }
  return step2.body.token;
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
