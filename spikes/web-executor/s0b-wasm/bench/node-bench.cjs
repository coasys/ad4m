// Usage: node node-bench.cjs <pkgdir> [links] [per_link]
const path = require('path');
const zlib = require('zlib');
const fs = require('fs');
const pkg = path.resolve(process.argv[2]);
const n = parseInt(process.argv[3] || '50000');
const perLink = process.argv[4] === 'per_link';
const wasmFile = path.join(pkg, 's0b_oxigraph_bg.wasm');
const buf = fs.readFileSync(wasmFile);
const br = zlib.brotliCompressSync(buf, { params: { [zlib.constants.BROTLI_PARAM_QUALITY]: 11, [zlib.constants.BROTLI_PARAM_SIZE_HINT]: buf.length } });
const gz = zlib.gzipSync(buf, { level: 9 });
console.log(`wasm raw=${buf.length} gzip9=${gz.length} brotli11=${br.length}`);
let t = performance.now();
const m = require(path.join(pkg, 's0b_oxigraph.js'));
console.log(`instantiate_ms=${(performance.now() - t).toFixed(1)}`);
m.reset();
const mem0 = m.memory_bytes();
t = performance.now();
const quads = m.load_links(n, perLink);
const loadMs = performance.now() - t;
const mem1 = m.memory_bytes();
console.log(`links=${n} quads=${quads} per_link=${perLink} load_ms=${loadMs.toFixed(1)} mem_before=${mem0} mem_after=${mem1} delta=${mem1 - mem0} bytes_per_link=${((mem1 - mem0) / n).toFixed(0)}`);
const nq = m.prepare_queries();
const reps = 10;
for (let i = 0; i < nq; i++) {
  const rows = m.run_query(i);
  const ts = [];
  for (let r = 0; r < reps; r++) { const s = performance.now(); m.run_query(i); ts.push(performance.now() - s); }
  ts.sort((a, b) => a - b);
  console.log(`${m.query_name(i)}\trows=${rows}\tmedian_ms=${ts[reps >> 1].toFixed(3)}\tmin_ms=${ts[0].toFixed(3)}`);
}
console.log(`mem_end=${m.memory_bytes()}`);
