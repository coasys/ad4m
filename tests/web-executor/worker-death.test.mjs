// A panic in the Wasm core aborts the module (`unreachable`): the Worker cannot go on. The
// Worker entry catches the RuntimeError, closes every socket with a reason, and the host learns
// that the Worker is dead (S10, S9 report point 5). One case; exit 0 when it passes.
//
//   bash scripts/web-executor/build.sh         # once: web-executor/pkg/node
//   (cd core && pnpm run build)                # once: core/lib
//   node tests/web-executor/worker-death.test.mjs

import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const fail = (msg) => { throw new Error(msg); };
const within = (ms, promise, what) => Promise.race([
  promise,
  new Promise((_, reject) => setTimeout(() => reject(new Error(`no ${what} within ${ms} ms`)), ms)),
]);

/** The close event of `socket`, a WorkerSocket. */
const closeOf = (socket) => new Promise((resolve) => socket.addEventListener('close', resolve));

let host;
try {
  const { Ad4mClient } = await import(path.join(root, 'core/lib/index.js'));
  const { startWebExecutor } = await import(path.join(root, 'web-executor/js/node.ts'));
  // The core logs the panic to the console; keep the test's output to its one line.
  host = await startWebExecutor({ logLevel: 'off', quiet: true });

  const ad4m = new Ad4mClient('http://web-executor.invalid', '', { webSocketImpl: host.WorkerSocket });
  await ad4m.agent.generate('s10-passphrase');
  const raw = new host.WorkerSocket('ws://web-executor.invalid/api/v1/ws');
  await new Promise((resolve) => raw.addEventListener('open', resolve));
  const closed = closeOf(raw);

  host.panicForTest('s10 test panic');

  const reason = await within(5000, host.dead, 'dead notice');
  if (!/s10 test panic/.test(reason)) fail(`host.dead: ${reason}`);
  const close = await within(5000, closed, 'close of the open socket');
  if (close.code !== 1011) fail(`close code ${close.code}`);
  if (!/s10 test panic/.test(close.reason)) fail(`close reason ${close.reason}`);
  // A dead Worker answers no call (the client rejects it or keeps it waiting), and a socket
  // opened after the death closes at once with the same reason.
  const call = await Promise.race([
    ad4m.agent.status().then(() => 'answered', (e) => `failed: ${e.message}`),
    new Promise((resolve) => setTimeout(() => resolve('no answer'), 2000)),
  ]);
  if (call === 'answered') fail('a dead Worker answered a call');
  const late = new host.WorkerSocket('ws://web-executor.invalid/api/v1/ws');
  const lateClose = await within(5000, closeOf(late), 'close of a socket opened after the death');
  if (lateClose.code !== 1011 || lateClose.reason !== close.reason) {
    fail(`late socket closed ${lateClose.code} ${lateClose.reason}`);
  }
  console.log(`a panic kills the Worker: closed ${close.code} "${close.reason}", host told ok`);
  console.log('1 passed');
  await host.terminate();
  process.exit(0);
} catch (e) {
  console.log(`a panic kills the Worker: FAIL ${e.message.split('\n')[0]}`);
  console.log('0 passed, 1 failed');
  await host?.terminate();
  process.exit(1);
}
