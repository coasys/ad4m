/**
 * A5 mock A/V fixture — seeds a mocked audio/video call surface into a channel
 * perspective as ORDINARY AD4M perspective links. The media transport is mocked;
 * the presence entry and the transcript are REAL channel writes, so the agent
 * perceives and acts over the true MCP surface.
 *
 * Exposes reusable helpers (imported by the A5 driver) AND a thin CLI (used by
 * the Sovereign A5 verify script).
 *
 *   register        <mcp> <ws> <jwt> <uuid> <chan>               — the Message class
 *   seed-presence   <mcp> <ws> <jwt> <uuid> <chan>               — (a)
 *   seed-transcript <mcp> <ws> <jwt> <uuid> <chan> <transcript>  — (b)
 *   call-active     <mcp> <ws> <jwt> <uuid> <chan>               — read (a) back
 *
 *     (a) a call-presence entry — ordinary links marking a live call on the
 *         channel; NOT a message, and its targets never name the agent, so it
 *         never trips the mention waker:
 *             chan         --ad4m://call_presence--> <presenceNode>
 *             presenceNode --ad4m://call_status--> literal:string:active
 *             presenceNode --ad4m://participant--> literal:string:mock-caller
 *     (b) a transcript Message whose body names the agent in FREE, natural speech
 *         (e.g. "… hey Aria, can you summarise the last point?"): a has_child +
 *         message_body pair, an instance of the Message class `register` adds,
 *         whose body is its TextContent `text`. The caller — another agent with
 *         its own did:key — writes and signs both links, as a peer's links
 *         arrive through a neighbourhood: the executor's mention queries read
 *         only verified links another agent wrote (an agent's own words do not
 *         wake it).
 *
 *   Splitting (a) and (b) lets a scenario run a negative control: seed the
 *   call-presence entry alone and confirm it produces no wake, then seed the
 *   transcript and confirm exactly one wake — proving the wake rides the
 *   transcript's spoken-name content, not mere channel/call activity.
 */
import { createHash, generateKeyPairSync, sign } from "crypto";
import { pathToFileURL } from "url";
import { Ad4mClient, Literal } from "@coasys/ad4m";
import { connectMcp } from "./sovereign/waker-ad4m.ts";

/** Deterministic addresses for the mocked call surface on a channel. */
export const presenceNodeFor = (chan: string) => `${chan}/call/1`;
export const transcriptAddrFor = (chan: string) => `${chan}/transcript/1`;

/** A channel message: its body (`ad4m://message_body`) is its TextContent text. */
export const MESSAGE_SHACL = JSON.stringify({
  target_class: "ad4m://harness/Message",
  properties: [
    {
      path: "ad4m://message_body",
      name: "body",
      datatype: "xsd:string",
      min_count: 1,
      max_count: 1,
      writable: true,
      resolve_language: "literal",
      alias: { TextContent: "text" },
      setter: [{ action: "setSingleTarget", source: "this", predicate: "ad4m://message_body", target: "value", local: false }],
    },
  ],
  constructor_actions: [],
  destructor_actions: [],
});

/** Register the Message class, so the mention waker reads a message's body as text. */
export async function registerMessageClass(mcp: any, uuid: string): Promise<void> {
  const r = await mcp.callToolJson("add_model", { perspective_id: uuid, class_name: "Message", shacl_json: MESSAGE_SHACL });
  if (r && typeof r === "object" && r.error) throw new Error(`add_model failed: ${JSON.stringify(r)}`);
}

const B58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
function base58btc(bytes: Buffer): string {
  let n = BigInt("0x" + bytes.toString("hex"));
  let out = "";
  while (n > 0n) {
    out = B58[Number(n % 58n)] + out;
    n = n / 58n;
  }
  for (const byte of bytes) {
    if (byte !== 0) break;
    out = "1" + out;
  }
  return out;
}

/**
 * The other party on the call: a did:key agent that signs its links like the
 * executor does (Ed25519 over sha256 of the link's JSON, then the timestamp).
 */
export function makeCaller() {
  const { publicKey, privateKey } = generateKeyPairSync("ed25519");
  const raw = publicKey.export({ format: "der", type: "spki" }).subarray(-32);
  const did = "did:key:z" + base58btc(Buffer.concat([Buffer.from([0xed, 0x01]), raw]));
  return {
    did,
    link(source: string, predicate: string, target: string) {
      const timestamp = new Date().toISOString();
      // Field order of the executor's `Link`: predicate, source, target.
      const data = { predicate, source, target };
      const hash = createHash("sha256").update(JSON.stringify(data)).update(timestamp).digest();
      const signature = sign(null, hash, privateKey).toString("hex");
      return { author: did, timestamp, data, proof: { key: `${did}#${did.slice(8)}`, signature } };
    },
  };
}

/**
 * (a) A call-presence entry — ordinary links, distinct predicate (not has_child)
 * and no agent name in any target, so the mention waker never fires on it.
 */
export async function seedCallPresence(mcp: any, uuid: string, chan: string): Promise<string> {
  const presenceNode = presenceNodeFor(chan);
  await mcp.callToolJson("add_link", { perspective_id: uuid, source: chan, predicate: "ad4m://call_presence", target: presenceNode });
  await mcp.callToolJson("add_link", { perspective_id: uuid, source: presenceNode, predicate: "ad4m://call_status", target: "literal:string:active" });
  await mcp.callToolJson("add_link", { perspective_id: uuid, source: presenceNode, predicate: "ad4m://participant", target: "literal:string:mock-caller" });
  return presenceNode;
}

/** (b) The caller's transcript Message — free-text spoken name in the body (the wake). */
export async function seedTranscript(ad4m: Ad4mClient, uuid: string, chan: string, transcript: string): Promise<string> {
  const caller = makeCaller();
  const msg = transcriptAddrFor(chan);
  const body = Literal.from(transcript).toUrl();
  await ad4m.perspective.addLinkExpression(uuid, caller.link(chan, "ad4m://has_child", msg) as any);
  await ad4m.perspective.addLinkExpression(uuid, caller.link(msg, "ad4m://message_body", body) as any);
  return msg;
}

/** True when the call-presence entry landed + reads back as active over MCP. */
export async function isCallActive(mcp: any, uuid: string, chan: string): Promise<boolean> {
  const links = await mcp.callToolJson("query_links", { perspective_id: uuid, source: chan });
  const presenceTargets = [...JSON.stringify(links).matchAll(/"predicate":"ad4m:\/\/call_presence","target":"([^"]+)"/g)].map((m) => m[1]);
  for (const pt of presenceTargets) {
    const pl = await mcp.callToolJson("query_links", { perspective_id: uuid, source: pt });
    if (JSON.stringify(pl).includes("active")) return true;
  }
  return false;
}

async function main(): Promise<void> {
  const [, , action, mcpUrl, wsUrl, token, uuid, chan, transcript] = process.argv;
  const mcp = await connectMcp(mcpUrl, token, "a5av");

  if (action === "register") {
    await registerMessageClass(mcp, uuid);
    console.log("REGISTERED");
  } else if (action === "seed-presence") {
    console.log("PRESENCE=" + (await seedCallPresence(mcp, uuid, chan)));
  } else if (action === "seed-transcript") {
    console.log("MSG=" + (await seedTranscript(new Ad4mClient(wsUrl, token), uuid, chan, transcript)));
  } else if (action === "call-active") {
    console.log("PRESENCE=" + ((await isCallActive(mcp, uuid, chan)) ? "active" : "absent"));
  } else {
    console.error("usage: mock-av.ts register|seed-presence|seed-transcript|call-active <mcp> <ws> <jwt> <uuid> <chan> [transcript]");
    process.exit(2);
  }
  process.exit(0);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((e) => {
    console.error("[a5av] FATAL", e?.stack || e);
    process.exit(2);
  });
}
