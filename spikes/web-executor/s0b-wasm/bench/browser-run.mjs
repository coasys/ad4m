// Serves the s0b-wasm dir (needs pkg-web/ from wasm-pack) on 127.0.0.1, runs bench/browser.html in headless
// Chrome via CDP, prints S0B lines. Usage: node bench/browser-run.mjs [s0b-wasm dir]. Env CHROME = Chrome binary.
import http from 'node:http'; import fs from 'node:fs'; import path from 'node:path'; import { spawn } from 'node:child_process'; import { fileURLToPath } from 'node:url';
const root = path.resolve(process.argv[2] || path.join(path.dirname(fileURLToPath(import.meta.url)), '..'));
const types = { '.html': 'text/html', '.js': 'text/javascript', '.wasm': 'application/wasm' };
const srv = http.createServer((q, r) => {
  const f = path.join(root, decodeURIComponent(new URL(q.url, 'http://x').pathname));
  if (!f.startsWith(root) || !fs.existsSync(f)) { r.writeHead(404); return r.end(); }
  r.writeHead(200, { 'content-type': types[path.extname(f)] || 'application/octet-stream' }); fs.createReadStream(f).pipe(r);
}).listen(0, '127.0.0.1');
await new Promise((ok) => srv.on('listening', ok));
const port = srv.address().port;
const prof = fs.mkdtempSync('/tmp/s0b-chrome-');
const ch = spawn(process.env.CHROME || '/usr/bin/google-chrome', ['--headless=new', '--no-first-run', '--disable-gpu', `--user-data-dir=${prof}`, '--remote-debugging-port=0', 'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'] });
const wsUrl = await new Promise((ok) => { let b = ''; ch.stderr.on('data', (d) => { b += d; const m = b.match(/DevTools listening on (ws:\/\/\S+)/); if (m) ok(m[1]); }); });
const targets = await (await fetch(wsUrl.replace('ws://', 'http://').replace(/\/devtools\/browser\/.*/, '/json/list'))).json();
const ws = new WebSocket(targets.find((t) => t.type === 'page').webSocketDebuggerUrl);
await new Promise((ok) => ws.addEventListener('open', ok));
let id = 0; const send = (method, params = {}) => ws.send(JSON.stringify({ id: ++id, method, params }));
const done = new Promise((ok) => ws.addEventListener('message', (ev) => {
  const msg = JSON.parse(ev.data);
  if (msg.method === 'Runtime.consoleAPICalled') {
    const s = msg.params.args.map((a) => a.value).join(' ');
    if (s.startsWith('S0B ')) { console.log(s.slice(4)); if (s === 'S0B END') ok(); }
  }
}));
send('Runtime.enable'); send('Page.navigate', { url: `http://127.0.0.1:${port}/bench/browser.html` });
await Promise.race([done, new Promise((ok) => setTimeout(ok, 180000))]);
ch.kill(); srv.close(); try { fs.rmSync(prof, { recursive: true, force: true, maxRetries: 5 }); } catch {} process.exit(0);
