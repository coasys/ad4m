/**
 * The reply oracle the A4/A5 routes assert on (`findChildWithMarker`): a reply
 * counts only as a `has_child` child of the channel with a `message_body`
 * carrying the marker. Rows have the shape the MCP `query_links` tool returns.
 *
 *   tsx --test interop/agents/sovereign/waker-ad4m.test.ts
 */
import { test } from "node:test";
import assert from "node:assert/strict";
import { HAS_CHILD, MESSAGE_BODY, hasChildWithMarker } from "./waker-ad4m.ts";

const CHAN = "a5h://channel";
const REPLY = "a5h://reply/1";
const MARKER = "A5H_REPLY_OK";

const link = (source: string, predicate: string, target: string) => ({
  source,
  predicate,
  target,
  timestamp: "2026-10-30T00:00:00.000Z",
  author: "did:key:z6MkReplier",
});

/** `bodiesOf` over a fixed set of rows, the way `query_links` filters by source. */
const store = (rows: ReturnType<typeof link>[]) => async (source: string) => rows.filter((r) => r.source === source);

const markedBody = link(REPLY, MESSAGE_BODY, `literal:string:${MARKER}`);

test("a has_child child whose message_body carries the marker is a reply", async () => {
  const channel = [link(CHAN, HAS_CHILD, REPLY)];
  assert.equal(await hasChildWithMarker(channel, store([markedBody]), MARKER), true);
});

test("the same marked node reached only by another predicate is not a reply", async () => {
  // Lal's counterexample on #1094: the has_child row changed to call_presence.
  const channel = [link(CHAN, "ad4m://call_presence", REPLY)];
  assert.equal(await hasChildWithMarker(channel, store([markedBody]), MARKER), false);
});

test("a child whose marker sits under another predicate than message_body is not a reply", async () => {
  const channel = [link(CHAN, HAS_CHILD, REPLY)];
  const elsewhere = [link(REPLY, "ad4m://call_presence", `literal:string:${MARKER}`)];
  assert.equal(await hasChildWithMarker(channel, store(elsewhere), MARKER), false);
});

test("a child whose message_body lacks the marker is not a reply", async () => {
  const channel = [link(CHAN, HAS_CHILD, REPLY)];
  const other = [link(REPLY, MESSAGE_BODY, "literal:string:Hey%20Aria")];
  assert.equal(await hasChildWithMarker(channel, store(other), MARKER), false);
});

test("an empty or non-array query result finds nothing", async () => {
  assert.equal(await hasChildWithMarker([], store([markedBody]), MARKER), false);
  assert.equal(await hasChildWithMarker("Error querying links", store([markedBody]), MARKER), false);
});
