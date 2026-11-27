// Dev CORS proxy in front of link-server (test only). Adds CORS headers,
// answers preflights, forwards WebSocket upgrades. Logs Origin + preflight asks.
import http from "node:http";
import net from "node:net";
import fs from "node:fs";
const [PORT, TARGET_PORT, LOG] = [Number(process.argv[2]), Number(process.argv[3]), process.argv[4] || "cors.log"];
const log = (s) => fs.appendFileSync(LOG, `${new Date().toISOString()} ${s}\n`);
const cors = (req) => ({
  "access-control-allow-origin": req.headers.origin || "*",
  "access-control-allow-methods": "GET,POST,PUT,DELETE,OPTIONS",
  "access-control-allow-headers": req.headers["access-control-request-headers"] || "authorization,content-type",
  "access-control-max-age": "600",
  "vary": "Origin",
});
http.createServer((req, res) => {
  if (req.method === "OPTIONS") {
    log(`PREFLIGHT ${req.headers["access-control-request-method"]} ${req.url} origin=${req.headers.origin} headers=${req.headers["access-control-request-headers"]}`);
    res.writeHead(204, cors(req)); return res.end();
  }
  log(`${req.method} ${req.url} origin=${req.headers.origin ?? "-"}`);
  const p = http.request({ host: "127.0.0.1", port: TARGET_PORT, path: req.url, method: req.method, headers: req.headers }, (r) => {
    res.writeHead(r.statusCode, { ...r.headers, ...cors(req) }); r.pipe(res);
  });
  p.on("error", (e) => { res.writeHead(502, cors(req)); res.end(String(e)); });
  req.pipe(p);
}).on("upgrade", (req, sock, head) => {
  log(`WS-UPGRADE ${req.url} origin=${req.headers.origin ?? "-"}`);
  const up = net.connect(TARGET_PORT, "127.0.0.1", () => {
    up.write(`${req.method} ${req.url} HTTP/1.1\r\n` + Object.entries(req.headers).map(([k, v]) => `${k}: ${v}`).join("\r\n") + "\r\n\r\n");
    up.write(head); up.pipe(sock); sock.pipe(up);
  });
  up.on("error", () => sock.destroy()); sock.on("error", () => up.destroy());
}).listen(PORT, "127.0.0.1", () => log(`cors proxy ${PORT} -> ${TARGET_PORT}`));
