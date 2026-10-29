import assert from "node:assert/strict";
import { randomBytes } from "node:crypto";
import type { AddressInfo } from "node:net";
import { test } from "node:test";
import type { ServerOptions } from "../src/server.js";
import { startTestServer, type TestServerHandle } from "./helpers.js";

const TOKEN = randomBytes(32).toString("hex");
const ORIGIN = "https://link-admin.example.org";
const SIGNED_IN = { authorization: `Bearer ${TOKEN}`, "x-forwarded-user": "lucksus" };

async function withServer(
  opts: Partial<ServerOptions>,
  fn: (h: { server: TestServerHandle; opUrl: string }) => Promise<void>
): Promise<void> {
  const server = await startTestServer({ ...opts, operator: { token: TOKEN, allowedOrigins: [ORIGIN] } });
  try {
    const opApp = server.built.operatorApp!;
    await opApp.listen({ port: 0, host: "127.0.0.1" });
    const opUrl = `http://127.0.0.1:${(opApp.server.address() as AddressInfo).port}`;
    await fn({ server, opUrl });
  } finally {
    await server.close();
  }
}

async function get<T = any>(url: string): Promise<T> {
  const res = await fetch(url, { headers: SIGNED_IN });
  assert.equal(res.status, 200, `${url}: ${res.status}`);
  return (await res.json()) as T;
}

test("whoami reports the admission mode: allowlist by default, auto with autoAdmit", async () => {
  await withServer({}, async ({ opUrl }) => {
    assert.equal((await get(`${opUrl}/api/whoami`)).admission, "allowlist");
  });
  await withServer({ autoAdmit: true }, async ({ opUrl }) => {
    assert.equal((await get(`${opUrl}/api/whoami`)).admission, "auto");
  });
});

test("the server logs its admission mode at startup", async () => {
  const lines: string[] = [];
  await withServer({ logger: true, logDestination: { write: (l: string) => lines.push(l) } }, async ({ server }) => {
    const entry = server.built.logTail!.lines().find((e) => e.msg?.startsWith("admission mode"));
    assert.ok(entry, "an admission mode line");
    assert.equal(entry.admission, "allowlist");
  });
});
