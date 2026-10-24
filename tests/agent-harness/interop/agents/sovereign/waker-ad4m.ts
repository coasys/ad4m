/**
 * A4/A5 (Sovereign) ad4m helper — the node-side link operations for the
 * Sovereign native-waker tests, over MCP with a user JWT.
 *
 * Exposes reusable link-writing helpers (imported by the A5 mock-A/V fixture)
 * AND a thin CLI (used by the A4/A5 verify scripts):
 *
 *   mint      <base> <jwt> <name>                  -> prints UUID=<uuid>
 *   mention   <base> <jwt> <uuid> <chan> <msg> <b> -> inject a mention message
 *                                                     (has_child + message_body)
 *   child-has <base> <jwt> <uuid> <chan> <marker>  -> prints REPLY=found|missing
 *                                                     (a channel child whose body
 *                                                      contains <marker>)
 */
import { fileURLToPath, pathToFileURL } from "url";
import { dirname, resolve } from "path";
import { existsSync } from "fs";

let HARNESS = dirname(fileURLToPath(import.meta.url));
while (HARNESS !== "/" && !existsSync(resolve(HARNESS, "src/mcp-client.ts"))) {
  HARNESS = resolve(HARNESS, "..");
}

/** Locate + construct the shared MCP client, initialised for a session. */
export async function connectMcp(base: string, token: string, label = "a4sv"): Promise<any> {
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);
  const c = new McpClient(base, token);
  await c.initialize(label);
  return c;
}

/** Create a fresh perspective; returns its uuid. */
export async function mintPerspective(c: any, name: string): Promise<string> {
  const p = await c.callToolJson("add_perspective", { name });
  return p.uuid || "";
}

/**
 * Seed a Message into a channel: a `has_child` link to the message node plus a
 * `message_body` link to an AD4M string literal. The waker's mention query
 * decodes the body via ad4m://fn/parse_literal, so a body naming the agent in
 * free text (e.g. "hey Aria …") triggers the spoken-name wake.
 */
export async function seedMentionMessage(
  c: any,
  uuid: string,
  chan: string,
  msg: string,
  body: string,
): Promise<void> {
  const literal = `literal://string:${encodeURIComponent(body)}`;
  await c.callToolJson("add_link", { perspective_id: uuid, source: chan, predicate: "ad4m://has_child", target: msg });
  await c.callToolJson("add_link", { perspective_id: uuid, source: msg, predicate: "ad4m://message_body", target: literal });
}

/**
 * True when a channel child carries `marker` anywhere in its own outgoing links
 * (e.g. a reply whose message_body literal contains the marker).
 */
export async function findChildWithMarker(c: any, uuid: string, chan: string, marker: string): Promise<boolean> {
  const kids = await c.callToolJson("query_links", { perspective_id: uuid, source: chan });
  const childTargets = [...new Set([...JSON.stringify(kids).matchAll(/"target":"([^"]+)"/g)].map((m) => m[1]))];
  for (const ct of childTargets) {
    const cl = await c.callToolJson("query_links", { perspective_id: uuid, source: ct });
    if (JSON.stringify(cl).includes(marker)) return true;
  }
  return false;
}

async function cli(): Promise<void> {
  const [, , action, base, token, ...rest] = process.argv;
  const c = await connectMcp(base, token, "a4sv");

  if (action === "mint") {
    console.log("UUID=" + (await mintPerspective(c, rest[0])));
  } else if (action === "mention") {
    const [uuid, chan, msg, body] = rest;
    await seedMentionMessage(c, uuid, chan, msg, body);
    console.log("INJECTED");
  } else if (action === "child-has") {
    const [uuid, chan, marker] = rest;
    console.log("REPLY=" + ((await findChildWithMarker(c, uuid, chan, marker)) ? "found" : "missing"));
  } else {
    console.error("usage: waker-ad4m.ts mint|mention|child-has ...");
    process.exit(2);
  }
}

// Run the CLI only when executed directly (not when imported as a helper module).
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  cli().catch((e) => {
    console.error("[a4sv] FATAL", e?.stack || e);
    process.exit(2);
  });
}
