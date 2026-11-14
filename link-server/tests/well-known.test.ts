import assert from "node:assert/strict";
import { test } from "node:test";
import { getJson, startTestServer } from "./helpers.js";

test("GET /.well-known/coasys-link-server names protocol 2 and membership", async () => {
  const server = await startTestServer({ name: "test-community" });
  try {
    const res = await getJson(`${server.url}/.well-known/coasys-link-server`);
    assert.equal(res.status, 200);
    assert.deepEqual(res.body, { protocol: 2, name: "test-community", membership: true });
  } finally {
    await server.close();
  }
});

test("the well-known name defaults to link-server", async () => {
  const server = await startTestServer();
  try {
    const res = await getJson<{ name: string }>(`${server.url}/.well-known/coasys-link-server`);
    assert.equal(res.body.name, "link-server");
  } finally {
    await server.close();
  }
});
