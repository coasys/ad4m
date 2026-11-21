// Egress deny proxy: forwards only 127.0.0.1/localhost, refuses and logs every other host.
import http from "node:http";
import net from "node:net";
import fs from "node:fs";
const PORT = Number(process.argv[2] || 47459);
const LOG = process.argv[3] || "egress.log";
const okHost = (h) => ["127.0.0.1", "localhost", "::1", "[::1]"].includes(String(h).toLowerCase());
const log = (s) => fs.appendFileSync(LOG, `${new Date().toISOString()} ${s}\n`);
const srv = http.createServer((req, res) => {
  let u; try { u = new URL(req.url); } catch { res.writeHead(400); return res.end(); }
  if (!okHost(u.hostname)) { log(`REFUSED HTTP ${req.method} ${u.host}${u.pathname}`); res.writeHead(403); return res.end("egress refused by s0a proxy"); }
  log(`ALLOWED HTTP ${req.method} ${u.host}${u.pathname}`);
  const p = http.request({ host: u.hostname, port: u.port || 80, path: u.pathname + u.search, method: req.method, headers: req.headers }, (r) => { res.writeHead(r.statusCode, r.headers); r.pipe(res); });
  p.on("error", (e) => { res.writeHead(502); res.end(String(e)); });
  req.pipe(p);
});
srv.on("connect", (req, sock, head) => {
  const [host, port] = req.url.split(/:(?=\d+$)/);
  if (!okHost(host)) { log(`REFUSED CONNECT ${req.url}`); sock.end("HTTP/1.1 403 Forbidden\r\n\r\n"); return; }
  log(`ALLOWED CONNECT ${req.url}`);
  const up = net.connect(Number(port), host, () => { sock.write("HTTP/1.1 200 OK\r\n\r\n"); up.write(head); up.pipe(sock); sock.pipe(up); });
  up.on("error", () => sock.destroy()); sock.on("error", () => up.destroy());
});
srv.listen(PORT, "127.0.0.1", () => log(`egress proxy listening on ${PORT}`));
