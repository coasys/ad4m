import assert from "node:assert/strict";
import { after, before, describe, it } from "node:test";
import { startTestServer, type TestServerHandle } from "./helpers.js";

// A language in a browser Worker calls the server from the page's origin.
describe("CORS", () => {
  let server: TestServerHandle;
  before(async () => {
    server = await startTestServer();
  });
  after(async () => {
    await server.close();
  });

  it("answers a preflight for an authenticated POST from any origin", async () => {
    const res = await fetch(`${server.url}/rooms/r1/commit`, {
      method: "OPTIONS",
      headers: {
        origin: "https://app.example.org",
        "access-control-request-method": "POST",
        "access-control-request-headers": "content-type,authorization",
      },
    });
    assert.equal(res.status, 204);
    assert.equal(res.headers.get("access-control-allow-origin"), "*");
    assert.match(res.headers.get("access-control-allow-methods") ?? "", /POST/);
    assert.match(res.headers.get("access-control-allow-headers") ?? "", /authorization/);
  });

  it("puts the allow-origin header on a real answer and exposes Retry-After", async () => {
    const res = await fetch(`${server.url}/health`, { headers: { origin: "https://app.example.org" } });
    assert.equal(res.status, 200);
    assert.equal(res.headers.get("access-control-allow-origin"), "*");
    assert.match(res.headers.get("access-control-expose-headers") ?? "", /Retry-After/);
  });
});
