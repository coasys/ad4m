/**
 * The store client holds a bounded number of key resolutions and folded logs: least recently
 * used go first, expired resolutions are deleted when the cache is full, and a dropped log's
 * handle stays usable by whoever still holds it.
 */
import assert from "node:assert/strict";
import { test } from "node:test";
import { StoreClient } from "../src/store-client.js";
import { createTestAgent } from "./helpers.js";
import { FakeStore, personAgent, STORE_HEAD_KEY } from "./identity.js";

test("the store client holds at most maxCachedKeys resolutions and maxLogs logs", async () => {
  const store = await FakeStore.start();
  const client = new StoreClient({ url: store.url, headKeys: [STORE_HEAD_KEY], maxCachedKeys: 2, maxLogs: 1 });
  try {
    const alice = await client.resolve(personAgent("alice").did);
    assert.equal(alice.kind, "listed");
    await client.resolve(personAgent("bob").did);
    await client.resolve(personAgent("carol").did);
    for (let i = 0; i < 5; i++) await client.resolve((await createTestAgent()).did);
    assert.deepEqual(client.held, { keys: 2, logs: 1 });

    // alice's log was dropped from the client, not freed: her resolution still reads it.
    if (alice.kind !== "listed") throw new Error("unreachable");
    assert.equal(alice.handle.id(), alice.scid);

    // The most recent resolutions are the ones held: served without asking the store.
    const recent = (await createTestAgent()).did;
    await client.resolve(recent);
    const hits = store.hits.keys;
    await client.resolve(recent);
    assert.equal(store.hits.keys, hits);
  } finally {
    client.close();
    await store.close();
  }
});

test("expired resolutions are deleted first when the cache is full", async () => {
  const store = await FakeStore.start();
  const client = new StoreClient({ url: store.url, headKeys: [STORE_HEAD_KEY], maxCachedKeys: 3, cacheMs: 1000 });
  try {
    const t0 = Date.now();
    const old = (await createTestAgent()).did;
    await client.resolve(old, { now: t0 });
    const fresh = [(await createTestAgent()).did, (await createTestAgent()).did, (await createTestAgent()).did];
    for (const did of fresh) await client.resolve(did, { now: t0 + 5000 });
    assert.equal(client.held.keys, 3);
    // The fresh ones stayed; the expired one went.
    const hits = store.hits.keys;
    for (const did of fresh) await client.resolve(did, { now: t0 + 5000 });
    assert.equal(store.hits.keys, hits);
  } finally {
    client.close();
    await store.close();
  }
});
