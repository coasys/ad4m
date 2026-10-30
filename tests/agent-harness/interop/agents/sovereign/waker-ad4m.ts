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
 *                                                     (a has_child child whose
 *                                                      message_body contains <marker>)
 */
import { pathToFileURL } from "url";
import { McpClient } from "../../../src/mcp-client.ts";

/** Locate + construct the shared MCP client, initialised for a session. */
export async function connectMcp(base: string, token: string, label = "a4sv"): Promise<any> {
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
  const literal = `literal:string:${encodeURIComponent(body)}`;
  await c.callToolJson("add_link", { perspective_id: uuid, source: chan, predicate: "ad4m://has_child", target: msg });
  await c.callToolJson("add_link", { perspective_id: uuid, source: msg, predicate: "ad4m://message_body", target: literal });
}

export const HAS_CHILD = "ad4m://has_child";
export const MESSAGE_BODY = "ad4m://message_body";

/** A `query_links` row: the MCP tool returns a JSON array of these. */
type LinkRow = { source?: string; predicate?: string | null; target?: string };

/** The targets of the rows whose predicate is exactly `predicate`, each once. */
export function targetsOf(result: unknown, predicate: string): string[] {
  const rows: LinkRow[] = Array.isArray(result) ? result : [];
  return [...new Set(rows.filter((r) => r.predicate === predicate && r.target).map((r) => r.target!))];
}

/**
 * True when one of the channel's `has_child` children has a `message_body`
 * whose literal contains `marker`. A node the channel reaches by any other
 * predicate (e.g. `ad4m://call_presence`), or a marker under any other
 * predicate of a child, is not a reply. `bodiesOf` reads a child's links.
 */
export async function hasChildWithMarker(
  channelLinks: unknown,
  bodiesOf: (child: string) => Promise<unknown>,
  marker: string,
): Promise<boolean> {
  for (const child of targetsOf(channelLinks, HAS_CHILD)) {
    if (targetsOf(await bodiesOf(child), MESSAGE_BODY).some((body) => body.includes(marker))) return true;
  }
  return false;
}

/** True when a reply carrying `marker` landed in the channel, see `hasChildWithMarker`. */
export async function findChildWithMarker(c: any, uuid: string, chan: string, marker: string): Promise<boolean> {
  const kids = await c.callToolJson("query_links", { perspective_id: uuid, source: chan, predicate: HAS_CHILD });
  return hasChildWithMarker(
    kids,
    (child) => c.callToolJson("query_links", { perspective_id: uuid, source: child, predicate: MESSAGE_BODY }),
    marker,
  );
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
