import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { readdirSync } from "node:fs";
import { test } from "node:test";
import {
  authenticateAgent,
  createTestAgent,
  openAuthenticatedWs,
  startTestServer,
  waitFor,
} from "./helpers.js";

// The server runs in this process, so /proc/self/fd counts its descriptors
// (and the test clients'). Linux only.
const fdCount = () => readdirSync("/proc/self/fd").length;

test("descriptors stay flat across many WebSocket connects and disconnects", { skip: process.platform !== "linux" }, async () => {
  const server = await startTestServer();
  try {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, admin);

    // Warm up: the first connections open what the server keeps for good.
    for (let i = 0; i < 5; i++) {
      const ws = await openAuthenticatedWs(server.wsUrl, roomId, token);
      ws.close();
    }
    await new Promise((r) => setTimeout(r, 200));
    const before = fdCount();

    for (let round = 0; round < 10; round++) {
      const sockets = await Promise.all(
        Array.from({ length: 20 }, () => openAuthenticatedWs(server.wsUrl, roomId, token))
      );
      sockets.forEach((ws) => ws.close());
    }
    // Closed sockets release their descriptors once both ends see the close.
    await waitFor(async () => fdCount() <= before + 2, 5000);
    assert.ok(fdCount() <= before + 2, `fds before ${before}, after ${fdCount()}`);
  } finally {
    await server.close();
  }
});
