import http from "node:http"; import fs from "node:fs"; import path from "node:path";
const [PORT, ROOT] = [Number(process.argv[2]), process.argv[3]];
const types = { ".js": "text/javascript", ".html": "text/html", ".json": "application/json" };
http.createServer((req, res) => {
  const p = path.join(ROOT, decodeURIComponent(new URL(req.url, "http://x").pathname));
  if (!p.startsWith(ROOT) || !fs.existsSync(p) || fs.statSync(p).isDirectory()) { res.writeHead(404); return res.end(); }
  res.writeHead(200, { "content-type": types[path.extname(p)] || "application/octet-stream", "cache-control": "no-store" });
  fs.createReadStream(p).pipe(res);
}).listen(PORT, "127.0.0.1");
