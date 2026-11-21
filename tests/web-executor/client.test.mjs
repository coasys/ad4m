// The web executor's transport test (S10): the unchanged `Ad4mClient` talks to the Wasm core in
// a Worker (`worker_threads`), through `WorkerSocket` and the core's own frame loop
// (`api/rpc_session.rs`). One line per case; exit 0 when all pass.
//
//   bash scripts/web-executor/build.sh         # once: web-executor/pkg/node
//   (cd core && pnpm run build)                # once: core/lib
//   node tests/web-executor/client.test.mjs
//
// The cases follow the S10 acceptance: `agent.status()`, `perspective.add()`, `addLink()` and a
// `link-added` subscription that fires exactly once; then two sockets on one Worker, each with
// its own `events.watch` filter (the filter is per session, not per Worker).

import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const fail = (msg) => { throw new Error(msg); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Both imports are inside the run, so a missing piece fails every case instead of the file.
let host, Ad4mClient;
async function setUp() {
  ({ Ad4mClient } = await import(path.join(root, 'core/lib/index.js')));
  const { startWebExecutor } = await import(path.join(root, 'web-executor/js/node.ts'));
  host = await startWebExecutor({ logLevel: process.env.CLIENT_TEST_LOG || 'error' });
}

/** A WorkerSocket that also keeps every frame the core sent it. */
function recording(WorkerSocket) {
  const frames = [];
  class Recording extends WorkerSocket {
    constructor(url) {
      super(url);
      this.addEventListener('message', (ev) => frames.push(JSON.parse(ev.data)));
    }
  }
  return { Recording, frames };
}

const URL_ = 'http://web-executor.invalid';
const LINK = { source: 'test://s10/source', predicate: 'test://s10/predicate', target: 'test://s10/target' };
let ad4m, perspective, linkAdded = [];

const cases = [
  ['agent.status()', async () => {
    ad4m = new Ad4mClient(URL_, '', { webSocketImpl: host.WorkerSocket });
    const generated = await ad4m.agent.generate('s10-passphrase');
    if (!generated.isInitialized) fail(`generate: ${JSON.stringify(generated)}`);
    const status = await ad4m.agent.status();
    if (!status.isUnlocked) fail('locked');
    if (!status.did?.startsWith('did:key:')) fail(`did ${status.did}`);
    return `unlocked ${status.did.slice(0, 20)}…`;
  }],
  ['perspective.add()', async () => {
    perspective = await ad4m.perspective.add('s10');
    if (!perspective.uuid) fail('no uuid');
    if (perspective.name !== 's10') fail(`name ${perspective.name}`);
    const all = await ad4m.perspective.all();
    if (!all.some((p) => p.uuid === perspective.uuid)) fail('not in perspective.all()');
    return perspective.uuid;
  }],
  ['addLink()', async () => {
    // The listener first: the client sends its `events.watch` ahead of the next call.
    perspective.on('link-added', (event) => linkAdded.push(event));
    const added = await perspective.add(LINK);
    if (added.data.target !== LINK.target) fail(JSON.stringify(added));
    if (!added.author?.startsWith('did:key:')) fail(`author ${added.author}`);
    const links = await perspective.get({ source: LINK.source });
    if (links.length !== 1) fail(`${links.length} links`);
    return `signed by ${added.author.slice(0, 20)}…`;
  }],
  ['link-added fires once', async () => {
    await sleep(300);
    if (linkAdded.length !== 1) fail(`${linkAdded.length} events`);
    const link = linkAdded[0].link;
    if (link?.data?.target !== LINK.target) fail(JSON.stringify(linkAdded[0]));
    return '1 event';
  }],
  ['the filter is per socket', async () => {
    // Two more clients, each on its own WorkerSocket of the same Worker: A watches
    // `link-added` on the perspective, B watches only `perspective-added`. Each must get its
    // own kind and never the other's.
    const a = recording(host.WorkerSocket), b = recording(host.WorkerSocket);
    const clientA = new Ad4mClient(URL_, '', { webSocketImpl: a.Recording });
    const clientB = new Ad4mClient(URL_, '', { webSocketImpl: b.Recording });
    const seenA = [], seenB = [];
    (await clientA.perspective.byUUID(perspective.uuid)).on('link-added', (e) => seenA.push(e));
    clientB.on('perspective-added', (e) => seenB.push(e));
    // A call sends the client's `events.watch` ahead of itself.
    await clientA.agent.status();
    await clientB.agent.status();
    await clientB.perspective.addLink(perspective.uuid, { ...LINK, target: 'test://s10/second' });
    await clientB.perspective.add('s10-second');
    await sleep(300);
    const types = (frames) => frames.filter((f) => f.type && f.type !== 'pong').map((f) => f.type);
    if (seenA.length !== 1) fail(`A saw ${seenA.length} link-added`);
    if (seenB.length !== 1) fail(`B saw ${seenB.length} perspective-added`);
    if (types(b.frames).includes('link-added')) fail(`B got link-added: ${types(b.frames)}`);
    if (types(a.frames).includes('perspective-added')) fail(`A got perspective-added: ${types(a.frames)}`);
    return `A: ${types(a.frames).join(',')}; B: ${types(b.frames).join(',')}`;
  }],
];

let passed = 0;
let setUpError = null;
try { await setUp(); } catch (e) { setUpError = e; }
for (const [name, run] of cases) {
  if (setUpError) {
    console.log(`${name}: FAIL set-up: ${setUpError.message.split('\n')[0]}`);
    continue;
  }
  try {
    console.log(`${name}: ${await run()} ok`);
    passed++;
  } catch (e) {
    console.log(`${name}: FAIL ${e.message}`);
  }
}
console.log(`${passed} passed${passed === cases.length ? '' : `, ${cases.length - passed} failed`}`);
await host?.terminate();
process.exit(passed === cases.length ? 0 : 1);
