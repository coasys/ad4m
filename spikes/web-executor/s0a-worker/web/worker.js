// Module Worker: loads the published server-link-language bundle with the
// ad4m:host import rewritten to ./shim.js, then calls exports by name.
const shimUrl = new URL("./shim.js", self.location.href).href;
let shim, lang;
// Count non-contract globals the language bundle (blob: URL) touches at runtime.
// The caller is told apart by the stack: "blob:" frames are the bundle, shim.js is the host.
const globalUse = {};
function who() {
  const f = (new Error().stack || "").split("\n").slice(1).find((l) => !l.includes("worker.js")) || "";
  return f.includes("blob:") ? "bundle" : f.includes("shim.js") ? "shim" : "other";
}
function count(name) { const k = who(); globalUse[name] ??= {}; globalUse[name][k] = (globalUse[name][k] || 0) + 1; }
for (const name of ["fetch", "atob", "btoa", "setTimeout", "clearTimeout", "setInterval", "clearInterval", "queueMicrotask"]) {
  const orig = self[name]; self[name] = function (...a) { count(name); return orig.apply(this, a); };
}
{ const gv = crypto.getRandomValues.bind(crypto); crypto.getRandomValues = (a) => { count("crypto.getRandomValues"); return gv(a); }; }
if (crypto.subtle) for (const m of ["digest", "importKey", "encrypt", "decrypt", "sign", "verify"]) { const o = crypto.subtle[m].bind(crypto.subtle); crypto.subtle[m] = (...a) => { count("crypto.subtle." + m); return o(...a); }; }
for (const C of ["WebSocket", "TextEncoder", "TextDecoder", "AbortController"]) {
  const Orig = self[C]; self[C] = new Proxy(Orig, { construct(t, a) { count("new " + C); return Reflect.construct(t, a); } });
}
{ const Orig = self.AbortSignal; const t = Orig.timeout.bind(Orig); Orig.timeout = (ms) => { count("AbortSignal.timeout"); return t(ms); }; }
for (const g of ["Deno", "Buffer", "process", "window", "document", "HOLOCHAIN_SERVICE", "RUNTIME_SERVICE", "SIGNATURE", "AGENT", "LANGUAGE_CONTROLLER", "UTILS"]) {
  Object.defineProperty(self, g, { configurable: true, get() { count("read " + g); return undefined; } });
}

const post = (type, data) => self.postMessage({ type, data, t: Date.now() });

self.onmessage = async (ev) => {
  const { id, cmd, arg } = ev.data;
  try {
    let result;
    if (cmd === "load") result = await load(arg);
    else if (cmd === "call") result = await lang[arg.fn](...(arg.args || []));
    else if (cmd === "commitLink") {
      const expr = shim.hostSignLink(arg);
      await lang.perspectiveCommit({ additions: [expr], removals: [] });
      result = expr;
    } else if (cmd === "removeLink") {
      await lang.perspectiveCommit({ additions: [], removals: [arg] });
      result = arg;
    } else if (cmd === "stats") result = { ...shim.shimStats(), globalUse };
    else if (cmd === "hash") result = shim.shimHash(arg);
    else if (cmd === "kv") result = null;
    post("reply", { id, ok: true, result: JSON.parse(JSON.stringify(result ?? null)) });
  } catch (e) {
    post("reply", { id, ok: false, error: String(e && e.stack || e) });
  }
};

async function load({ bundleUrl, settings, address }) {
  shim = await import(shimUrl);
  const { did } = shim.configure({
    address, settings: JSON.stringify(settings || {}),
    onEvent: (kind, payload) => post("event", { kind, payload: JSON.parse(JSON.stringify(payload ?? null)) }),
    log: (line) => post("log", line),
  });
  const src = await (await fetch(bundleUrl)).text();
  const n = src.split('from "ad4m:host"').length - 1;
  const rewritten = src.replaceAll('from "ad4m:host"', `from ${JSON.stringify(shimUrl)}`);
  const url = URL.createObjectURL(new Blob([rewritten], { type: "text/javascript" }));
  lang = await import(url);
  const exports = Object.keys(lang);
  // Globals beyond the contract: snapshot of what the worker's realm offers (not proof of use).
  return { did, importSitesRewritten: n, bundleBytes: src.length, exports };
}
