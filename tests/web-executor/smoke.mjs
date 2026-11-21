// The web executor's smoke test (S9, milestone M2): six RPC calls into the Wasm core under
// Node, each checked, one line each ending in `ok`. Exit 0 when all six pass.
//
//   bash scripts/web-executor/build.sh         # once: web-executor/pkg/node
//   node tests/web-executor/smoke.mjs [path/to/web_executor.js]
//
// SMOKE_IDLE_MS=10000 then lets the core idle that long and counts its timer wake-ups (the
// setTimeout callbacks that fire, by delay; the core's background loops sleep on
// setTimeout). SMOKE_PERSPECTIVES=n idles with n perspectives (default 1: the smoke one).
// SMOKE_LOG sets the core's console log level (default error).

import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { performance } from 'node:perf_hooks';

const here = path.dirname(fileURLToPath(import.meta.url));
const glue = path.resolve(process.argv[2] || path.join(here, '../../web-executor/pkg/node/web_executor.js'));

// Count timer wake-ups while `timers.on`: setTimeout callbacks that fire, by their delay.
const timers = { count: 0, byDelay: new Map(), on: false };
const realSetTimeout = globalThis.setTimeout;
globalThis.setTimeout = function (fn, delay, ...args) {
  const key = Math.round(+delay || 0);
  return realSetTimeout((...a) => {
    if (timers.on) {
      timers.count++;
      timers.byDelay.set(key, (timers.byDelay.get(key) || 0) + 1);
    }
    return fn(...a);
  }, delay, ...args);
};

const core = createRequire(import.meta.url)(glue);
const SRC = 'test://smoke/source', PRED = 'test://smoke/predicate', TGT = 'test://smoke/target';
const fail = (msg) => { throw new Error(msg); };
const events = [];
let nextId = 0;

async function call(type, params) {
  const id = `smoke-${++nextId}`;
  const reply = JSON.parse(await core.dispatch(JSON.stringify({ id, type, params })));
  if (reply.id !== id) fail(`reply id ${reply.id} for request ${id}`);
  if (reply.error) fail(`${reply.error.code} ${reply.error.message}`);
  return reply.result;
}

let uuid;
const steps = [
  ['agent.generate', () => ({ passphrase: 'smoke-passphrase' }), (r) => {
    if (!r.did?.startsWith('did:key:')) fail(`did ${r.did}`);
    if (!r.isUnlocked || !r.isInitialized) fail(`not unlocked/initialized: ${JSON.stringify(r)}`);
    if (r.error) fail(`agent.error: ${r.error}`);
    return r.did;
  }],
  ['agent.status', () => ({}), (r) => {
    if (!r.isUnlocked) fail('locked');
    if (!r.isInitialized) fail('not initialized');
    if (!r.did?.startsWith('did:key:')) fail(`did ${r.did}`);
    return `unlocked ${r.did.slice(0, 20)}…`;
  }],
  ['perspective.create', () => ({ name: 'smoke' }), (r) => {
    if (!r.uuid) fail(`no uuid: ${JSON.stringify(r)}`);
    uuid = r.uuid;
    return r.uuid;
  }],
  ['perspective.addLink', () => ({ uuid, link: { source: SRC, predicate: PRED, target: TGT } }), (r) => {
    const d = r.data;
    if (d?.source !== SRC || d?.predicate !== PRED || d?.target !== TGT) fail(JSON.stringify(r));
    if (!r.author?.startsWith('did:key:')) fail(`author ${r.author}`);
    return `signed by ${r.author.slice(0, 20)}…`;
  }],
  ['perspective.queryLinks', () => ({ uuid, query: { source: SRC } }), (r) => {
    if (!Array.isArray(r) || r.length !== 1) fail(`${r?.length} links: ${JSON.stringify(r)}`);
    if (r[0].data.target !== TGT) fail(JSON.stringify(r[0]));
    return '1 link';
  }],
  ['perspective.querySparql', () => ({ uuid, query: `SELECT ?p ?o WHERE { <${SRC}> ?p ?o }` }), (r) => {
    const rows = typeof r === 'string' ? JSON.parse(r) : r;
    if (!Array.isArray(rows) || rows.length !== 1) fail(`${rows?.length} rows: ${JSON.stringify(rows)}`);
    const row = JSON.stringify(rows[0]);
    if (!row.includes(PRED) || !row.includes(TGT)) fail(row);
    return '1 row';
  }],
];

const ms = (t) => `${t.toFixed(1)} ms`;
let passed = 0;
const t0 = performance.now();
core.boot('{}', process.env.SMOKE_LOG || 'error');
console.log(`boot ${ms(performance.now() - t0)}`);
// The events: one connection of the core's own loop (`connect`), watching the three kinds the
// calls cause. The calls themselves go through `dispatch`, one frame each.
const watch = { 'agent-status-changed': null, 'perspective-added': null, 'link-added': null };
await new Promise((resolve) => {
  const conn = core.connect((frame) => {
    const msg = JSON.parse(frame);
    if (msg.id === 'watch') resolve();
    else if (msg.type) events.push(msg);
  }, () => {});
  conn.send(JSON.stringify({ id: 'watch', type: 'events.watch', params: watch }));
});
for (const [type, params, check] of steps) {
  const start = performance.now();
  try {
    const result = await call(type, params());
    const took = performance.now() - start;
    console.log(`${type} (${ms(took)}): ${check(result)} ok`);
    passed++;
  } catch (e) {
    console.log(`${type} (${ms(performance.now() - start)}): FAIL ${e.message}`);
  }
}

const idle = +(process.env.SMOKE_IDLE_MS || 0);
if (idle > 0) {
  const perspectives = +(process.env.SMOKE_PERSPECTIVES || 1);
  for (let i = 1; i < perspectives; i++) await call('perspective.create', { name: `idle-${i}` });
  // Let the new perspectives' loops start before counting.
  await new Promise((r) => realSetTimeout(r, 500));
  timers.on = true;
  await new Promise((r) => realSetTimeout(r, idle));
  timers.on = false;
  const delays = [...timers.byDelay].sort((a, b) => a[0] - b[0]).map(([d, n]) => `${n}×${d}ms`).join(', ');
  console.log(`idle ${idle} ms with ${perspectives} perspective(s): ${timers.count} timer wake-ups (${delays || 'none'})`);
}
await new Promise((r) => realSetTimeout(r, 50));
console.log(`events: ${events.length} (${[...new Set(events.map((e) => e.type))].join(', ')})`);
console.log(`${passed}/6 ok`);
process.exit(passed === 6 ? 0 : 1);
