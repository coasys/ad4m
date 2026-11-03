import assert from "node:assert/strict";
import { randomBytes, randomUUID } from "node:crypto";
import net from "node:net";
import { test } from "node:test";
import {
  authenticateAgent,
  createTestAgent,
  collectMessages,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}


/**
 * A minimal WebSocket client on a raw TCP socket. Unlike the `ws` client it
 * never answers a close frame, so the server's side of the socket stays
 * CLOSING: the state a removed member can hold for up to the close timeout.
 */
async function openRawWs(httpUrl: string, path: string) {
  const { hostname, port } = new URL(httpUrl);
  const socket = net.connect(Number(port), hostname);
  await new Promise<void>((resolve, reject) => socket.once("connect", resolve).once("error", reject));
  const key = randomBytes(16).toString("base64");
  socket.write(
    `GET ${path} HTTP/1.1\r\nHost: ${hostname}:${port}\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n` +
      `Sec-WebSocket-Key: ${key}\r\nSec-WebSocket-Version: 13\r\n\r\n`
  );
  let buffer = Buffer.alloc(0);
  let upgraded = false;
  const frames: Array<{ opcode: number; payload: Buffer }> = [];
  socket.on("data", (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
    if (!upgraded) {
      const end = buffer.indexOf("\r\n\r\n");
      if (end < 0) return;
      upgraded = true;
      buffer = buffer.subarray(end + 4);
    }
    // Server frames are unmasked; payloads here are small (< 64 KiB).
    while (buffer.length >= 2) {
      let len = buffer[1] & 0x7f;
      let offset = 2;
      if (len === 126) {
        if (buffer.length < 4) return;
        len = buffer.readUInt16BE(2);
        offset = 4;
      }
      if (buffer.length < offset + len) return;
      frames.push({ opcode: buffer[0] & 0x0f, payload: buffer.subarray(offset, offset + len) });
      buffer = buffer.subarray(offset + len);
    }
  });
  const sendText = (text: string) => {
    const payload = Buffer.from(text);
    assert.ok(payload.length < 65536);
    const header =
      payload.length < 126
        ? Buffer.from([0x81, 0x80 | payload.length])
        : Buffer.from([0x81, 0x80 | 126, payload.length >> 8, payload.length & 0xff]);
    const mask = randomBytes(4);
    const masked = Buffer.from(payload.map((b, i) => b ^ mask[i % 4]));
    socket.write(Buffer.concat([header, mask, masked]));
  };
  return { socket, frames, sendText };
}

test("GET acl lists the admin and all members", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const res = await getJson<{ admin: string; members: Array<{ did: string }> }>(
      `${server.url}/rooms/${roomId}/acl`,
      adminToken
    );
    assert.equal(res.status, 200);
    assert.equal(res.body.admin, admin.did);
    const memberDids = new Set(res.body.members.map((m) => m.did));
    assert.deepEqual(memberDids, new Set([admin.did, member.did]));
  });
});

test("non-admin members cannot modify the ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const outsider = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: outsider.did },
      memberToken
    );
    assert.equal(res.status, 403);

    const acl = await getJson<{ members: Array<{ did: string }> }>(`${server.url}/rooms/${roomId}/acl`, adminToken);
    assert.ok(!acl.body.members.some((m) => m.did === outsider.did));
  });
});

test("acl rejects a missing/invalid action", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "not-a-real-action", did: "did:key:zXYZ" },
      adminToken
    );
    assert.equal(res.status, 400);
  });
});

test("admin cannot remove themselves from the ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "remove", did: admin.did },
      adminToken
    );
    assert.equal(res.status, 400);

    const acl = await getJson<{ members: Array<{ did: string }> }>(`${server.url}/rooms/${roomId}/acl`, adminToken);
    assert.ok(acl.body.members.some((m) => m.did === admin.did));
  });
});

test("removed member can no longer re-authenticate without being re-added", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);

    await assert.rejects(() => authenticateAgent(server.url, roomId, member));
  });
});

test("removing a member closes their open WebSocket, so live diffs stop at once", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, memberToken);
    const closed = new Promise<number>((resolve) => socket.once("close", (code) => resolve(code)));

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);

    const code = await Promise.race([
      closed,
      new Promise<"open">((resolve) => setTimeout(() => resolve("open"), 2000)),
    ]);
    if (code === "open") socket.terminate();
    assert.equal(code, 4005, "the removed member's socket must be closed with 4005");
  });
});

test("a removed member whose socket is still closing cannot send telepresence into the room", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const observer = await openAuthenticatedWs(server.wsUrl, roomId, adminToken);
    const seen = collectMessages(observer);
    const raw = await openRawWs(server.url, `/rooms/${roomId}/ws`);
    try {
      raw.sendText(JSON.stringify({ type: "auth", token: memberToken }));
      await waitUntil(() => raw.frames.some((f) => f.opcode === 1 && f.payload.toString().includes("online-agents")));

      await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);
      // The server's close frame has arrived; its side is CLOSING, and this client never answers.
      await waitUntil(() => raw.frames.some((f) => f.opcode === 8));
      raw.sendText(JSON.stringify({ type: "telepresence-broadcast", payload: "after-removal" }));
      raw.sendText(JSON.stringify({ type: "set-online-status", status: "after-removal" }));
      await new Promise((resolve) => setTimeout(resolve, 300));

      const leaked = seen.messages.filter((m: any) =>
        (m.type === "telepresence-broadcast" && m.payload === "after-removal") ||
        (m.type === "status-changed" && m.status === "after-removal")
      );
      assert.deepEqual(leaked, [], "messages from a closing socket must not reach the room");
    } finally {
      raw.socket.destroy();
      observer.terminate();
    }
  });
});

async function waitUntil(predicate: () => boolean, timeoutMs = 3000): Promise<void> {
  const deadline = Date.now() + timeoutMs;
  while (!predicate()) {
    if (Date.now() > deadline) throw new Error("condition not met in time");
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
}

test("member added back to the ACL can authenticate again", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const token = await authenticateAgent(server.url, roomId, member);
    assert.equal(typeof token, "string");
  });
});

test("acl endpoints require authentication", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/acl`);
    assert.equal(res.status, 401);
  });
});
