/**
 * A5 mock A/V fixture — seeds a mocked audio/video call surface into a channel
 * perspective as ORDINARY AD4M perspective links. The media transport is mocked;
 * the presence entry and the transcript are REAL channel writes, so the agent
 * perceives and acts over the true MCP surface (see the wind-tunnel plan,
 * "Mocked A/V — why this stays honest").
 *
 * Exposes reusable helpers (imported by the Hermes/OpenClaw A5 drivers) AND a
 * thin CLI (used by the Sovereign A5 verify script). Reuses the shared MCP client
 * + the waker-ad4m link-writing helpers rather than duplicating link plumbing.
 *
 *   seed          <base> <jwt> <uuid> <chan> <transcript>   — (a) then (b)
 *   seed-presence <base> <jwt> <uuid> <chan>                — (a) only
 *   seed-transcript <base> <jwt> <uuid> <chan> <transcript> — (b) only
 *   call-active   <base> <jwt> <uuid> <chan>                — read (a) back
 *
 *     (a) a call-presence entry — an ordinary link marking a live call on the
 *         channel (a participant joined); NOT a message, and its targets never
 *         name the agent, so it never trips the mention waker:
 *             chan         --ad4m://call_presence--> <presenceNode>
 *             presenceNode --ad4m://call_status--> literal://string:active
 *             presenceNode --ad4m://participant--> literal://string:mock-caller
 *     (b) a transcript Message whose body names the agent in FREE, natural speech
 *         (e.g. "… hey Aria, can you summarise the last point?"), seeded as an
 *         ordinary has_child + message_body pair. That free-text name is exactly
 *         the spoken-name wake the AD4M mention waker detects — NOT a structured
 *         @mention.
 *
 *   Splitting (a) and (b) lets a scenario run a negative control: seed the
 *   call-presence entry alone and confirm it produces no wake, then seed the
 *   transcript and confirm exactly one wake — proving the wake rides the
 *   transcript's spoken-name content, not mere channel/call activity.
 */
import { fileURLToPath, pathToFileURL } from "url";
import { dirname, resolve } from "path";
import { existsSync } from "fs";

let HARNESS = dirname(fileURLToPath(import.meta.url));
while (HARNESS !== "/" && !existsSync(resolve(HARNESS, "src/mcp-client.ts"))) {
  HARNESS = resolve(HARNESS, "..");
}

/** Deterministic addresses for the mocked call surface on a channel. */
export const presenceNodeFor = (chan: string) => `${chan}/call/1`;
export const transcriptAddrFor = (chan: string) => `${chan}/transcript/1`;

/**
 * (a) A call-presence entry — ordinary links, distinct predicate (not has_child)
 * and no agent name in any target, so the mention waker never fires on it.
 */
export async function seedCallPresence(c: any, uuid: string, chan: string): Promise<string> {
  const presenceNode = presenceNodeFor(chan);
  await c.callToolJson("add_link", { perspective_id: uuid, source: chan, predicate: "ad4m://call_presence", target: presenceNode });
  await c.callToolJson("add_link", { perspective_id: uuid, source: presenceNode, predicate: "ad4m://call_status", target: "literal://string:active" });
  await c.callToolJson("add_link", { perspective_id: uuid, source: presenceNode, predicate: "ad4m://participant", target: "literal://string:mock-caller" });
  return presenceNode;
}

/** (b) A transcript Message — free-text spoken name in the body (the wake). */
export async function seedTranscript(c: any, uuid: string, chan: string, transcript: string): Promise<string> {
  const { seedMentionMessage } = await import("./sovereign/waker-ad4m.ts");
  const msg = transcriptAddrFor(chan);
  await seedMentionMessage(c, uuid, chan, msg, transcript);
  return msg;
}

/** True when the call-presence entry landed + reads back as active over MCP. */
export async function isCallActive(c: any, uuid: string, chan: string): Promise<boolean> {
  const links = await c.callToolJson("query_links", { perspective_id: uuid, source: chan });
  const presenceTargets = [...JSON.stringify(links).matchAll(/"predicate":"ad4m:\/\/call_presence","target":"([^"]+)"/g)].map((m) => m[1]);
  for (const pt of presenceTargets) {
    const pl = await c.callToolJson("query_links", { perspective_id: uuid, source: pt });
    if (JSON.stringify(pl).includes("active")) return true;
  }
  return false;
}

async function main(): Promise<void> {
  const { connectMcp } = await import("./sovereign/waker-ad4m.ts");
  const [, , action, base, token, uuid, chan, transcript] = process.argv;
  const c = await connectMcp(base, token, "a5av");

  if (action === "seed") {
    console.log("PRESENCE=" + (await seedCallPresence(c, uuid, chan)));
    console.log("MSG=" + (await seedTranscript(c, uuid, chan, transcript)));
  } else if (action === "seed-presence") {
    console.log("PRESENCE=" + (await seedCallPresence(c, uuid, chan)));
  } else if (action === "seed-transcript") {
    console.log("MSG=" + (await seedTranscript(c, uuid, chan, transcript)));
  } else if (action === "call-active") {
    console.log("PRESENCE=" + ((await isCallActive(c, uuid, chan)) ? "active" : "absent"));
  } else {
    console.error("usage: mock-av.ts seed|seed-presence|seed-transcript|call-active <base> <jwt> <uuid> <chan> [transcript]");
    process.exit(2);
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((e) => {
    console.error("[a5av] FATAL", e?.stack || e);
    process.exit(2);
  });
}
