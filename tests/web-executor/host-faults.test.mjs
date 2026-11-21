// Faults at the host side of the web executor (S10 review): a socket listener that throws
// while the Worker dies, and a Worker whose boot fails. Two cases; exit 0 when both pass.
//
//   bash scripts/web-executor/build.sh         # once: web-executor/pkg/node
//   node tests/web-executor/host-faults.test.mjs

import { spawn } from 'node:child_process';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const within = (ms, promise, what) => Promise.race([
  promise,
  new Promise((_, reject) => setTimeout(() => reject(new Error(`no ${what} within ${ms} ms`)), ms)),
]);
const { WorkerHost } = await import(path.join(root, 'web-executor/js/WorkerSocket.ts'));

const cases = [
  ['a throwing listener does not stop the others at a death', async () => {
    const reported = [];
    const onUncaught = (error) => reported.push(error);
    process.on('uncaughtException', onUncaught);
    try {
      const host = new WorkerHost(() => {});
      const a = new host.WorkerSocket('ws://a');
      const b = new host.WorkerSocket('ws://b');
      await Promise.all([a, b].map((s) => new Promise((resolve) => s.addEventListener('open', resolve))));
      a.onclose = () => { throw new Error('listener fault'); };
      const bClosed = new Promise((resolve) => b.addEventListener('close', resolve));
      host.died('test death');
      const reason = await within(2000, host.dead, 'host.dead');
      const close = await within(2000, bClosed, 'close of the other socket');
      await new Promise((resolve) => setTimeout(resolve, 10));
      if (reason !== 'test death') throw new Error(`host.dead: ${reason}`);
      if (close.code !== 1011) throw new Error(`other socket closed ${close.code}`);
      if (a.readyState !== 3 || b.readyState !== 3) throw new Error(`readyState ${a.readyState} ${b.readyState}`);
      if (reported.length !== 1 || !/listener fault/.test(reported[0]?.message)) {
        throw new Error(`reported ${reported.length} errors`);
      }
      return 'both closed, dead resolved, 1 error reported';
    } finally {
      process.off('uncaughtException', onUncaught);
    }
  }],
  ['a Worker whose boot fails is stopped', async () => {
    // In a child process: with the Worker stopped, it ends on its own once the start rejects.
    const script = `
      const { startWebExecutor } = await import(${JSON.stringify(path.join(root, 'web-executor/js/node.ts'))});
      try { await startWebExecutor({ config: { notAKey: true }, logLevel: 'off', quiet: true }); console.log('booted'); }
      catch (e) { console.log('rejected: ' + e.message.slice(0, 60)); }`;
    // A file, not -e: the Worker inherits the process flags, and --input-type is refused there.
    const file = path.join(mkdtempSync(path.join(tmpdir(), 's10-host-')), 'start.mjs');
    writeFileSync(file, script);
    const child = spawn(process.execPath, [file], { stdio: ['ignore', 'pipe', 'pipe'] });
    let out = '';
    child.stdout.on('data', (d) => { out += d; });
    const code = await within(10000, new Promise((resolve) => child.on('exit', resolve)), 'exit of the host process')
      .catch((e) => { child.kill('SIGKILL'); throw e; });
    if (!/^rejected: web executor did not boot/.test(out.trim())) throw new Error(`start: ${out.trim()}`);
    if (code !== 0) throw new Error(`host process exit ${code}`);
    return 'start rejected, Worker stopped, host process ended';
  }],
];

let passed = 0;
for (const [name, run] of cases) {
  try { console.log(`${name}: ${await run()} ok`); passed++; }
  catch (e) { console.log(`${name}: FAIL ${e.message.split('\n')[0]}`); }
}
console.log(passed === cases.length ? `${passed} passed` : `${passed} passed, ${cases.length - passed} failed`);
process.exit(passed === cases.length ? 0 : 1);
