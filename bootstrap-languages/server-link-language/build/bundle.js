// ../../ad4m-ldk/js/lib/imports.js
import {
  agentDid,
  agentSigningKeyId,
  agentSign,
  agentSignStringHex,
  agentCreateSignedExpression,
  agentGetAllLocalUserDids,
  agentCreateSignedExpressionForUser,
  agentDidForUser,
  holochainRegisterDnas,
  holochainCall,
  holochainCallAsync,
  httpFetch,
  hash,
  languageStorageDirectory,
  languageAddress,
  languageSettings,
  emitPerspectiveDiff,
  emitSyncStateChange,
  emitTelepresenceSignal,
  emitSignal,
  storageGet,
  storagePut,
  storageDelete,
  storageListKeys,
  readStorageFile,
  writeStorageFile
} from "ad4m:host";

// ../../ad4m-ldk/js/lib/defineLanguage.js
function defineLanguage(spec) {
  const out = {
    name: spec.name,
    version: spec.version,
    init: spec.init.bind(spec)
  };
  if (typeof spec.isPublic === "boolean") {
    const v = spec.isPublic;
    out.isPublic = () => v;
  }
  if (spec.teardown)
    out.teardown = spec.teardown.bind(spec);
  if (spec.interactions)
    out.interactions = spec.interactions.bind(spec);
  if (spec.expression) {
    const e = spec.expression;
    if (e.get)
      out.expressionGet = e.get.bind(e);
    if (e.create)
      out.expressionCreate = e.create.bind(e);
    if (e.addressOf)
      out.expressionAddressOf = e.addressOf.bind(e);
    if (e.isImmutable)
      out.isImmutableExpression = e.isImmutable.bind(e);
    if (e.icon)
      out.expressionIcon = e.icon.bind(e);
    if (e.constructorIcon)
      out.expressionConstructorIcon = e.constructorIcon.bind(e);
  }
  if (spec.languageSource) {
    out.languageGetSource = spec.languageSource.getSource.bind(spec.languageSource);
  }
  if (spec.commit) {
    out.perspectiveCommit = spec.commit.commit.bind(spec.commit);
  }
  if (spec.sync) {
    const s = spec.sync;
    out.perspectiveSyncSync = s.sync.bind(s);
    out.perspectiveSyncRender = s.render.bind(s);
    out.perspectiveSyncCurrentRevision = s.currentRevision.bind(s);
  }
  if (spec.query) {
    out.perspectiveQuerySupportedKinds = spec.query.supportedKinds.bind(spec.query);
    out.perspectiveQueryRun = spec.query.run.bind(spec.query);
  }
  if (spec.peers) {
    out.peersSetLocal = spec.peers.setLocal.bind(spec.peers);
    out.peersRemote = spec.peers.remote.bind(spec.peers);
  }
  if (spec.telepresence) {
    const t = spec.telepresence;
    if (t.setOnlineStatus)
      out.telepresenceSetOnlineStatus = t.setOnlineStatus.bind(t);
    if (t.getOnlineAgents)
      out.telepresenceGetOnlineAgents = t.getOnlineAgents.bind(t);
    if (t.sendSignal)
      out.telepresenceSendSignal = t.sendSignal.bind(t);
    if (t.sendBroadcast)
      out.telepresenceSendBroadcast = t.sendBroadcast.bind(t);
    if (t.registerSignalCallback)
      out.telepresenceRegisterSignalCallback = t.registerSignalCallback.bind(t);
  }
  if (spec.handleHolochainSignal) {
    out.handleHolochainSignal = spec.handleHolochainSignal.bind(spec);
  }
  return out;
}

// src/adapters.ts
var _registry = {};
function initAdapters(adapters) {
  if (adapters.config) {
    adapters = {
      ...adapters,
      config: {
        serverUrl: adapters.config.serverUrl.replace(/\/+$/, ""),
        roomId: adapters.config.roomId
      }
    };
  }
  Object.assign(_registry, adapters);
}
function getTransport() {
  if (!_registry.transport) {
    throw new Error("Transport not initialized. Call initAdapters() during language init().");
  }
  return _registry.transport;
}
function getStorage() {
  if (!_registry.storage) {
    throw new Error("StorageAdapter not initialized. Call initAdapters() during language init().");
  }
  return _registry.storage;
}
function getAgent() {
  if (!_registry.agent) {
    throw new Error("AgentAdapter not initialized. Call initAdapters() during language init().");
  }
  return _registry.agent;
}
function getRuntime() {
  if (!_registry.runtime) {
    throw new Error("RuntimeAdapter not initialized. Call initAdapters() during language init().");
  }
  return _registry.runtime;
}
function getWebSocketFactory() {
  if (!_registry.wsFactory) {
    throw new Error("WebSocketFactory not initialized. Call initAdapters() during language init().");
  }
  return _registry.wsFactory;
}
function getConfig() {
  if (!_registry.config) {
    throw new Error("RoomConfig not initialized. Call initAdapters() during language init().");
  }
  return _registry.config;
}
function resetAdapters() {
  _registry = {};
}

// src/store.ts
var _hashFn = null;
function initStore(hashFn) {
  _hashFn = hashFn ?? null;
}
function getHashFn() {
  if (!_hashFn) {
    throw new Error(
      "Store not initialized with a hash function. Call initStore(hashFn) during language init()."
    );
  }
  return _hashFn;
}
function linkKey(linkHash) {
  return `links/${linkHash}`;
}
function sourceIndexKey(source, linkHash) {
  return `links-by-source/${source}/${linkHash}`;
}
function targetIndexKey(target, linkHash) {
  return `links-by-target/${target}/${linkHash}`;
}
function predIndexKey(predicate, linkHash) {
  return `links-by-pred/${predicate}/${linkHash}`;
}
function hashLink(link) {
  const content = JSON.stringify({
    source: link.data.source,
    predicate: link.data.predicate ?? null,
    target: link.data.target,
    author: link.author,
    timestamp: link.timestamp
  });
  return getHashFn()(content);
}
function putLink(link) {
  const h = hashLink(link);
  const storage = getStorage();
  storage.put(linkKey(h), JSON.stringify(link));
  const source = link.data.source || "";
  const target = link.data.target || "";
  const predicate = link.data.predicate || "";
  if (source)
    storage.put(sourceIndexKey(source, h), h);
  if (target)
    storage.put(targetIndexKey(target, h), h);
  if (predicate)
    storage.put(predIndexKey(predicate, h), h);
  return h;
}
function removeLink(link) {
  const h = hashLink(link);
  const storage = getStorage();
  storage.delete(linkKey(h));
  const source = link.data.source || "";
  const target = link.data.target || "";
  const predicate = link.data.predicate || "";
  if (source)
    storage.delete(sourceIndexKey(source, h));
  if (target)
    storage.delete(targetIndexKey(target, h));
  if (predicate)
    storage.delete(predIndexKey(predicate, h));
}
function getLink(linkHash) {
  const raw = getStorage().get(linkKey(linkHash));
  if (!raw)
    return null;
  return JSON.parse(raw);
}
function applyDiff(diff) {
  for (const addition of diff.additions) {
    putLink(addition);
  }
  for (const removal of diff.removals) {
    removeLink(removal);
  }
}
function queryLinks(query) {
  const { source, target, predicate } = query;
  const storage = getStorage();
  let candidateHashes;
  if (source) {
    const keys = storage.listKeys(`links-by-source/${source}/`);
    candidateHashes = keys.map((k) => storage.get(k) || "").filter(Boolean);
  } else if (target) {
    const keys = storage.listKeys(`links-by-target/${target}/`);
    candidateHashes = keys.map((k) => storage.get(k) || "").filter(Boolean);
  } else if (predicate) {
    const keys = storage.listKeys(`links-by-pred/${predicate}/`);
    candidateHashes = keys.map((k) => storage.get(k) || "").filter(Boolean);
  } else {
    const keys = storage.listKeys("links/");
    candidateHashes = keys.map((k) => k.replace("links/", ""));
  }
  const results = [];
  const seen = /* @__PURE__ */ new Set();
  for (const h of candidateHashes) {
    if (seen.has(h))
      continue;
    seen.add(h);
    const link = getLink(h);
    if (!link)
      continue;
    if (source && link.data.source !== source)
      continue;
    if (target && link.data.target !== target)
      continue;
    if (predicate && link.data.predicate !== predicate)
      continue;
    results.push(link);
  }
  return results;
}
function allLinks() {
  const keys = getStorage().listKeys("links/");
  const links = [];
  for (const key of keys) {
    const raw = getStorage().get(key);
    if (raw) {
      links.push(JSON.parse(raw));
    }
  }
  return { links };
}
var REVISION_KEY = "revision";
var SEQUENCE_KEY = "sequence";
function getRevision() {
  return getStorage().get(REVISION_KEY);
}
function setRevision(rev) {
  if (!rev)
    return;
  getStorage().put(REVISION_KEY, rev);
}
function getSequence() {
  const raw = getStorage().get(SEQUENCE_KEY);
  if (!raw)
    return 0;
  const n = parseInt(raw, 10);
  return Number.isFinite(n) ? n : 0;
}
function setSequence(seq) {
  if (!Number.isFinite(seq))
    return;
  getStorage().put(SEQUENCE_KEY, String(seq));
}

// src/api.ts
var ApiError = class extends Error {
  constructor(status, message) {
    super(message);
    this.name = "ApiError";
    this.status = status;
  }
};
function roomUrl(config, path) {
  const base = config.serverUrl.replace(/\/+$/, "");
  return `${base}/rooms/${encodeURIComponent(config.roomId)}${path}`;
}
function jsonHeaders(token) {
  const headers = { "Content-Type": "application/json" };
  if (token)
    headers["Authorization"] = `Bearer ${token}`;
  return headers;
}
async function request(url, method, headers, body) {
  const res = await getTransport().fetch(url, method, headers, body ?? "");
  if (res.status < 200 || res.status >= 300) {
    throw new ApiError(res.status, res.body || `HTTP ${res.status} from ${method} ${url}`);
  }
  if (!res.body || res.body.length === 0) {
    return void 0;
  }
  try {
    return JSON.parse(res.body);
  } catch (err) {
    throw new ApiError(
      res.status,
      `Invalid JSON response from ${method} ${url}: ${err.message}`
    );
  }
}
async function requestChallenge(config, did) {
  const res = await request(
    roomUrl(config, "/auth"),
    "POST",
    jsonHeaders(),
    JSON.stringify({ did })
  );
  return res.challenge;
}
async function verifyChallenge(config, did, challenge, signature, x25519PublicKeyHex, x25519Signature) {
  const payload = { did, challenge, signature };
  if (x25519PublicKeyHex)
    payload.x25519PublicKey = x25519PublicKeyHex;
  if (x25519Signature)
    payload.x25519Signature = x25519Signature;
  const res = await request(
    roomUrl(config, "/auth"),
    "POST",
    jsonHeaders(),
    JSON.stringify(payload)
  );
  return res.token;
}
async function commitDiff(config, token, diff) {
  await request(roomUrl(config, "/commit"), "POST", jsonHeaders(token), JSON.stringify(diff));
}
async function fetchSync(config, token, since) {
  const url = roomUrl(config, `/sync?since=${encodeURIComponent(String(since))}`);
  const res = await request(url, "GET", jsonHeaders(token));
  return {
    diffs: res.diffs ?? [],
    revision: res.revision ?? "",
    sequence: typeof res.sequence === "number" ? res.sequence : since
  };
}
async function fetchRender(config, token) {
  const res = await request(roomUrl(config, "/render"), "GET", jsonHeaders(token));
  return {
    links: res.links ?? [],
    revision: res.revision ?? "",
    sequence: typeof res.sequence === "number" ? res.sequence : 0
  };
}
async function fetchPeers(config, token) {
  const res = await request(roomUrl(config, "/peers"), "GET", jsonHeaders(token));
  return res.peers ?? [];
}
async function rotateKeys(config, token, keys) {
  return request(
    roomUrl(config, "/keys/rotate"),
    "POST",
    jsonHeaders(token),
    JSON.stringify({ keys })
  );
}
async function fetchRoomKeys(config, token) {
  try {
    const res = await request(roomUrl(config, "/keys"), "GET", jsonHeaders(token));
    if (!res)
      return null;
    return {
      keys: res.keys ?? [],
      e2e_enabled: res.e2e_enabled ?? (!!res.keys && res.keys.length > 0)
    };
  } catch (err) {
    if (err instanceof ApiError && (err.status === 404 || err.status === 204)) {
      return null;
    }
    throw err;
  }
}
async function grantKeys(config, token, targetDid, keys) {
  const res = await request(
    roomUrl(config, "/keys/grant"),
    "POST",
    jsonHeaders(token),
    JSON.stringify({ targetDid, keys })
  );
  return res.granted ?? [];
}
async function fetchMissingKeys(config, token) {
  const res = await request(
    roomUrl(config, "/keys/missing"),
    "GET",
    jsonHeaders(token)
  );
  return { membersNeedingHistoricalKeys: res.membersNeedingHistoricalKeys ?? [] };
}
async function fetchAclInfo(config, token) {
  const res = await request(roomUrl(config, "/acl"), "GET", jsonHeaders(token));
  return { admin: res.admin ?? "", members: res.members ?? [] };
}
function wsUrl(config) {
  const httpUrl = roomUrl(config, "/ws");
  return httpUrl.replace(/^http/, "ws");
}

// node_modules/@noble/hashes/esm/crypto.js
var crypto = typeof globalThis === "object" && "crypto" in globalThis ? globalThis.crypto : void 0;

// node_modules/@noble/hashes/esm/utils.js
/*! noble-hashes - MIT License (c) 2022 Paul Miller (paulmillr.com) */
function isBytes(a) {
  return a instanceof Uint8Array || ArrayBuffer.isView(a) && a.constructor.name === "Uint8Array";
}
function anumber(n) {
  if (!Number.isSafeInteger(n) || n < 0)
    throw new Error("positive integer expected, got " + n);
}
function abytes(b, ...lengths) {
  if (!isBytes(b))
    throw new Error("Uint8Array expected");
  if (lengths.length > 0 && !lengths.includes(b.length))
    throw new Error("Uint8Array expected of length " + lengths + ", got length=" + b.length);
}
function ahash(h) {
  if (typeof h !== "function" || typeof h.create !== "function")
    throw new Error("Hash should be wrapped by utils.createHasher");
  anumber(h.outputLen);
  anumber(h.blockLen);
}
function aexists(instance, checkFinished = true) {
  if (instance.destroyed)
    throw new Error("Hash instance has been destroyed");
  if (checkFinished && instance.finished)
    throw new Error("Hash#digest() has already been called");
}
function aoutput(out, instance) {
  abytes(out);
  const min = instance.outputLen;
  if (out.length < min) {
    throw new Error("digestInto() expects output buffer of length at least " + min);
  }
}
function clean(...arrays) {
  for (let i = 0; i < arrays.length; i++) {
    arrays[i].fill(0);
  }
}
function createView(arr) {
  return new DataView(arr.buffer, arr.byteOffset, arr.byteLength);
}
function rotr(word, shift) {
  return word << 32 - shift | word >>> shift;
}
var hasHexBuiltin = /* @__PURE__ */ (() => (
  // @ts-ignore
  typeof Uint8Array.from([]).toHex === "function" && typeof Uint8Array.fromHex === "function"
))();
var hexes = /* @__PURE__ */ Array.from({ length: 256 }, (_, i) => i.toString(16).padStart(2, "0"));
function bytesToHex(bytes) {
  abytes(bytes);
  if (hasHexBuiltin)
    return bytes.toHex();
  let hex = "";
  for (let i = 0; i < bytes.length; i++) {
    hex += hexes[bytes[i]];
  }
  return hex;
}
var asciis = { _0: 48, _9: 57, A: 65, F: 70, a: 97, f: 102 };
function asciiToBase16(ch) {
  if (ch >= asciis._0 && ch <= asciis._9)
    return ch - asciis._0;
  if (ch >= asciis.A && ch <= asciis.F)
    return ch - (asciis.A - 10);
  if (ch >= asciis.a && ch <= asciis.f)
    return ch - (asciis.a - 10);
  return;
}
function hexToBytes(hex) {
  if (typeof hex !== "string")
    throw new Error("hex string expected, got " + typeof hex);
  if (hasHexBuiltin)
    return Uint8Array.fromHex(hex);
  const hl = hex.length;
  const al = hl / 2;
  if (hl % 2)
    throw new Error("hex string expected, got unpadded hex of length " + hl);
  const array = new Uint8Array(al);
  for (let ai = 0, hi = 0; ai < al; ai++, hi += 2) {
    const n1 = asciiToBase16(hex.charCodeAt(hi));
    const n2 = asciiToBase16(hex.charCodeAt(hi + 1));
    if (n1 === void 0 || n2 === void 0) {
      const char = hex[hi] + hex[hi + 1];
      throw new Error('hex string expected, got non-hex character "' + char + '" at index ' + hi);
    }
    array[ai] = n1 * 16 + n2;
  }
  return array;
}
function utf8ToBytes(str) {
  if (typeof str !== "string")
    throw new Error("string expected");
  return new Uint8Array(new TextEncoder().encode(str));
}
function toBytes(data) {
  if (typeof data === "string")
    data = utf8ToBytes(data);
  abytes(data);
  return data;
}
function concatBytes(...arrays) {
  let sum = 0;
  for (let i = 0; i < arrays.length; i++) {
    const a = arrays[i];
    abytes(a);
    sum += a.length;
  }
  const res = new Uint8Array(sum);
  for (let i = 0, pad = 0; i < arrays.length; i++) {
    const a = arrays[i];
    res.set(a, pad);
    pad += a.length;
  }
  return res;
}
var Hash = class {
};
function createHasher(hashCons) {
  const hashC = (msg) => hashCons().update(toBytes(msg)).digest();
  const tmp = hashCons();
  hashC.outputLen = tmp.outputLen;
  hashC.blockLen = tmp.blockLen;
  hashC.create = () => hashCons();
  return hashC;
}
function randomBytes(bytesLength = 32) {
  if (crypto && typeof crypto.getRandomValues === "function") {
    return crypto.getRandomValues(new Uint8Array(bytesLength));
  }
  if (crypto && typeof crypto.randomBytes === "function") {
    return Uint8Array.from(crypto.randomBytes(bytesLength));
  }
  throw new Error("crypto.getRandomValues must be defined");
}

// node_modules/@noble/hashes/esm/_md.js
function setBigUint64(view, byteOffset, value, isLE2) {
  if (typeof view.setBigUint64 === "function")
    return view.setBigUint64(byteOffset, value, isLE2);
  const _32n2 = BigInt(32);
  const _u32_max = BigInt(4294967295);
  const wh = Number(value >> _32n2 & _u32_max);
  const wl = Number(value & _u32_max);
  const h = isLE2 ? 4 : 0;
  const l = isLE2 ? 0 : 4;
  view.setUint32(byteOffset + h, wh, isLE2);
  view.setUint32(byteOffset + l, wl, isLE2);
}
function Chi(a, b, c) {
  return a & b ^ ~a & c;
}
function Maj(a, b, c) {
  return a & b ^ a & c ^ b & c;
}
var HashMD = class extends Hash {
  constructor(blockLen, outputLen, padOffset, isLE2) {
    super();
    this.finished = false;
    this.length = 0;
    this.pos = 0;
    this.destroyed = false;
    this.blockLen = blockLen;
    this.outputLen = outputLen;
    this.padOffset = padOffset;
    this.isLE = isLE2;
    this.buffer = new Uint8Array(blockLen);
    this.view = createView(this.buffer);
  }
  update(data) {
    aexists(this);
    data = toBytes(data);
    abytes(data);
    const { view, buffer, blockLen } = this;
    const len = data.length;
    for (let pos = 0; pos < len; ) {
      const take = Math.min(blockLen - this.pos, len - pos);
      if (take === blockLen) {
        const dataView = createView(data);
        for (; blockLen <= len - pos; pos += blockLen)
          this.process(dataView, pos);
        continue;
      }
      buffer.set(data.subarray(pos, pos + take), this.pos);
      this.pos += take;
      pos += take;
      if (this.pos === blockLen) {
        this.process(view, 0);
        this.pos = 0;
      }
    }
    this.length += data.length;
    this.roundClean();
    return this;
  }
  digestInto(out) {
    aexists(this);
    aoutput(out, this);
    this.finished = true;
    const { buffer, view, blockLen, isLE: isLE2 } = this;
    let { pos } = this;
    buffer[pos++] = 128;
    clean(this.buffer.subarray(pos));
    if (this.padOffset > blockLen - pos) {
      this.process(view, 0);
      pos = 0;
    }
    for (let i = pos; i < blockLen; i++)
      buffer[i] = 0;
    setBigUint64(view, blockLen - 8, BigInt(this.length * 8), isLE2);
    this.process(view, 0);
    const oview = createView(out);
    const len = this.outputLen;
    if (len % 4)
      throw new Error("_sha2: outputLen should be aligned to 32bit");
    const outLen = len / 4;
    const state = this.get();
    if (outLen > state.length)
      throw new Error("_sha2: outputLen bigger than state");
    for (let i = 0; i < outLen; i++)
      oview.setUint32(4 * i, state[i], isLE2);
  }
  digest() {
    const { buffer, outputLen } = this;
    this.digestInto(buffer);
    const res = buffer.slice(0, outputLen);
    this.destroy();
    return res;
  }
  _cloneInto(to) {
    to || (to = new this.constructor());
    to.set(...this.get());
    const { blockLen, buffer, length, finished, destroyed, pos } = this;
    to.destroyed = destroyed;
    to.finished = finished;
    to.length = length;
    to.pos = pos;
    if (length % blockLen)
      to.buffer.set(buffer);
    return to;
  }
  clone() {
    return this._cloneInto();
  }
};
var SHA256_IV = /* @__PURE__ */ Uint32Array.from([
  1779033703,
  3144134277,
  1013904242,
  2773480762,
  1359893119,
  2600822924,
  528734635,
  1541459225
]);
var SHA512_IV = /* @__PURE__ */ Uint32Array.from([
  1779033703,
  4089235720,
  3144134277,
  2227873595,
  1013904242,
  4271175723,
  2773480762,
  1595750129,
  1359893119,
  2917565137,
  2600822924,
  725511199,
  528734635,
  4215389547,
  1541459225,
  327033209
]);

// node_modules/@noble/hashes/esm/_u64.js
var U32_MASK64 = /* @__PURE__ */ BigInt(2 ** 32 - 1);
var _32n = /* @__PURE__ */ BigInt(32);
function fromBig(n, le = false) {
  if (le)
    return { h: Number(n & U32_MASK64), l: Number(n >> _32n & U32_MASK64) };
  return { h: Number(n >> _32n & U32_MASK64) | 0, l: Number(n & U32_MASK64) | 0 };
}
function split(lst, le = false) {
  const len = lst.length;
  let Ah = new Uint32Array(len);
  let Al = new Uint32Array(len);
  for (let i = 0; i < len; i++) {
    const { h, l } = fromBig(lst[i], le);
    [Ah[i], Al[i]] = [h, l];
  }
  return [Ah, Al];
}
var shrSH = (h, _l, s) => h >>> s;
var shrSL = (h, l, s) => h << 32 - s | l >>> s;
var rotrSH = (h, l, s) => h >>> s | l << 32 - s;
var rotrSL = (h, l, s) => h << 32 - s | l >>> s;
var rotrBH = (h, l, s) => h << 64 - s | l >>> s - 32;
var rotrBL = (h, l, s) => h >>> s - 32 | l << 64 - s;
function add(Ah, Al, Bh, Bl) {
  const l = (Al >>> 0) + (Bl >>> 0);
  return { h: Ah + Bh + (l / 2 ** 32 | 0) | 0, l: l | 0 };
}
var add3L = (Al, Bl, Cl) => (Al >>> 0) + (Bl >>> 0) + (Cl >>> 0);
var add3H = (low, Ah, Bh, Ch) => Ah + Bh + Ch + (low / 2 ** 32 | 0) | 0;
var add4L = (Al, Bl, Cl, Dl) => (Al >>> 0) + (Bl >>> 0) + (Cl >>> 0) + (Dl >>> 0);
var add4H = (low, Ah, Bh, Ch, Dh) => Ah + Bh + Ch + Dh + (low / 2 ** 32 | 0) | 0;
var add5L = (Al, Bl, Cl, Dl, El) => (Al >>> 0) + (Bl >>> 0) + (Cl >>> 0) + (Dl >>> 0) + (El >>> 0);
var add5H = (low, Ah, Bh, Ch, Dh, Eh) => Ah + Bh + Ch + Dh + Eh + (low / 2 ** 32 | 0) | 0;

// node_modules/@noble/hashes/esm/sha2.js
var SHA256_K = /* @__PURE__ */ Uint32Array.from([
  1116352408,
  1899447441,
  3049323471,
  3921009573,
  961987163,
  1508970993,
  2453635748,
  2870763221,
  3624381080,
  310598401,
  607225278,
  1426881987,
  1925078388,
  2162078206,
  2614888103,
  3248222580,
  3835390401,
  4022224774,
  264347078,
  604807628,
  770255983,
  1249150122,
  1555081692,
  1996064986,
  2554220882,
  2821834349,
  2952996808,
  3210313671,
  3336571891,
  3584528711,
  113926993,
  338241895,
  666307205,
  773529912,
  1294757372,
  1396182291,
  1695183700,
  1986661051,
  2177026350,
  2456956037,
  2730485921,
  2820302411,
  3259730800,
  3345764771,
  3516065817,
  3600352804,
  4094571909,
  275423344,
  430227734,
  506948616,
  659060556,
  883997877,
  958139571,
  1322822218,
  1537002063,
  1747873779,
  1955562222,
  2024104815,
  2227730452,
  2361852424,
  2428436474,
  2756734187,
  3204031479,
  3329325298
]);
var SHA256_W = /* @__PURE__ */ new Uint32Array(64);
var SHA256 = class extends HashMD {
  constructor(outputLen = 32) {
    super(64, outputLen, 8, false);
    this.A = SHA256_IV[0] | 0;
    this.B = SHA256_IV[1] | 0;
    this.C = SHA256_IV[2] | 0;
    this.D = SHA256_IV[3] | 0;
    this.E = SHA256_IV[4] | 0;
    this.F = SHA256_IV[5] | 0;
    this.G = SHA256_IV[6] | 0;
    this.H = SHA256_IV[7] | 0;
  }
  get() {
    const { A, B, C, D, E, F, G, H } = this;
    return [A, B, C, D, E, F, G, H];
  }
  // prettier-ignore
  set(A, B, C, D, E, F, G, H) {
    this.A = A | 0;
    this.B = B | 0;
    this.C = C | 0;
    this.D = D | 0;
    this.E = E | 0;
    this.F = F | 0;
    this.G = G | 0;
    this.H = H | 0;
  }
  process(view, offset) {
    for (let i = 0; i < 16; i++, offset += 4)
      SHA256_W[i] = view.getUint32(offset, false);
    for (let i = 16; i < 64; i++) {
      const W15 = SHA256_W[i - 15];
      const W2 = SHA256_W[i - 2];
      const s0 = rotr(W15, 7) ^ rotr(W15, 18) ^ W15 >>> 3;
      const s1 = rotr(W2, 17) ^ rotr(W2, 19) ^ W2 >>> 10;
      SHA256_W[i] = s1 + SHA256_W[i - 7] + s0 + SHA256_W[i - 16] | 0;
    }
    let { A, B, C, D, E, F, G, H } = this;
    for (let i = 0; i < 64; i++) {
      const sigma1 = rotr(E, 6) ^ rotr(E, 11) ^ rotr(E, 25);
      const T1 = H + sigma1 + Chi(E, F, G) + SHA256_K[i] + SHA256_W[i] | 0;
      const sigma0 = rotr(A, 2) ^ rotr(A, 13) ^ rotr(A, 22);
      const T2 = sigma0 + Maj(A, B, C) | 0;
      H = G;
      G = F;
      F = E;
      E = D + T1 | 0;
      D = C;
      C = B;
      B = A;
      A = T1 + T2 | 0;
    }
    A = A + this.A | 0;
    B = B + this.B | 0;
    C = C + this.C | 0;
    D = D + this.D | 0;
    E = E + this.E | 0;
    F = F + this.F | 0;
    G = G + this.G | 0;
    H = H + this.H | 0;
    this.set(A, B, C, D, E, F, G, H);
  }
  roundClean() {
    clean(SHA256_W);
  }
  destroy() {
    this.set(0, 0, 0, 0, 0, 0, 0, 0);
    clean(this.buffer);
  }
};
var K512 = /* @__PURE__ */ (() => split([
  "0x428a2f98d728ae22",
  "0x7137449123ef65cd",
  "0xb5c0fbcfec4d3b2f",
  "0xe9b5dba58189dbbc",
  "0x3956c25bf348b538",
  "0x59f111f1b605d019",
  "0x923f82a4af194f9b",
  "0xab1c5ed5da6d8118",
  "0xd807aa98a3030242",
  "0x12835b0145706fbe",
  "0x243185be4ee4b28c",
  "0x550c7dc3d5ffb4e2",
  "0x72be5d74f27b896f",
  "0x80deb1fe3b1696b1",
  "0x9bdc06a725c71235",
  "0xc19bf174cf692694",
  "0xe49b69c19ef14ad2",
  "0xefbe4786384f25e3",
  "0x0fc19dc68b8cd5b5",
  "0x240ca1cc77ac9c65",
  "0x2de92c6f592b0275",
  "0x4a7484aa6ea6e483",
  "0x5cb0a9dcbd41fbd4",
  "0x76f988da831153b5",
  "0x983e5152ee66dfab",
  "0xa831c66d2db43210",
  "0xb00327c898fb213f",
  "0xbf597fc7beef0ee4",
  "0xc6e00bf33da88fc2",
  "0xd5a79147930aa725",
  "0x06ca6351e003826f",
  "0x142929670a0e6e70",
  "0x27b70a8546d22ffc",
  "0x2e1b21385c26c926",
  "0x4d2c6dfc5ac42aed",
  "0x53380d139d95b3df",
  "0x650a73548baf63de",
  "0x766a0abb3c77b2a8",
  "0x81c2c92e47edaee6",
  "0x92722c851482353b",
  "0xa2bfe8a14cf10364",
  "0xa81a664bbc423001",
  "0xc24b8b70d0f89791",
  "0xc76c51a30654be30",
  "0xd192e819d6ef5218",
  "0xd69906245565a910",
  "0xf40e35855771202a",
  "0x106aa07032bbd1b8",
  "0x19a4c116b8d2d0c8",
  "0x1e376c085141ab53",
  "0x2748774cdf8eeb99",
  "0x34b0bcb5e19b48a8",
  "0x391c0cb3c5c95a63",
  "0x4ed8aa4ae3418acb",
  "0x5b9cca4f7763e373",
  "0x682e6ff3d6b2b8a3",
  "0x748f82ee5defb2fc",
  "0x78a5636f43172f60",
  "0x84c87814a1f0ab72",
  "0x8cc702081a6439ec",
  "0x90befffa23631e28",
  "0xa4506cebde82bde9",
  "0xbef9a3f7b2c67915",
  "0xc67178f2e372532b",
  "0xca273eceea26619c",
  "0xd186b8c721c0c207",
  "0xeada7dd6cde0eb1e",
  "0xf57d4f7fee6ed178",
  "0x06f067aa72176fba",
  "0x0a637dc5a2c898a6",
  "0x113f9804bef90dae",
  "0x1b710b35131c471b",
  "0x28db77f523047d84",
  "0x32caab7b40c72493",
  "0x3c9ebe0a15c9bebc",
  "0x431d67c49c100d4c",
  "0x4cc5d4becb3e42b6",
  "0x597f299cfc657e2a",
  "0x5fcb6fab3ad6faec",
  "0x6c44198c4a475817"
].map((n) => BigInt(n))))();
var SHA512_Kh = /* @__PURE__ */ (() => K512[0])();
var SHA512_Kl = /* @__PURE__ */ (() => K512[1])();
var SHA512_W_H = /* @__PURE__ */ new Uint32Array(80);
var SHA512_W_L = /* @__PURE__ */ new Uint32Array(80);
var SHA512 = class extends HashMD {
  constructor(outputLen = 64) {
    super(128, outputLen, 16, false);
    this.Ah = SHA512_IV[0] | 0;
    this.Al = SHA512_IV[1] | 0;
    this.Bh = SHA512_IV[2] | 0;
    this.Bl = SHA512_IV[3] | 0;
    this.Ch = SHA512_IV[4] | 0;
    this.Cl = SHA512_IV[5] | 0;
    this.Dh = SHA512_IV[6] | 0;
    this.Dl = SHA512_IV[7] | 0;
    this.Eh = SHA512_IV[8] | 0;
    this.El = SHA512_IV[9] | 0;
    this.Fh = SHA512_IV[10] | 0;
    this.Fl = SHA512_IV[11] | 0;
    this.Gh = SHA512_IV[12] | 0;
    this.Gl = SHA512_IV[13] | 0;
    this.Hh = SHA512_IV[14] | 0;
    this.Hl = SHA512_IV[15] | 0;
  }
  // prettier-ignore
  get() {
    const { Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl, Eh, El, Fh, Fl, Gh, Gl, Hh, Hl } = this;
    return [Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl, Eh, El, Fh, Fl, Gh, Gl, Hh, Hl];
  }
  // prettier-ignore
  set(Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl, Eh, El, Fh, Fl, Gh, Gl, Hh, Hl) {
    this.Ah = Ah | 0;
    this.Al = Al | 0;
    this.Bh = Bh | 0;
    this.Bl = Bl | 0;
    this.Ch = Ch | 0;
    this.Cl = Cl | 0;
    this.Dh = Dh | 0;
    this.Dl = Dl | 0;
    this.Eh = Eh | 0;
    this.El = El | 0;
    this.Fh = Fh | 0;
    this.Fl = Fl | 0;
    this.Gh = Gh | 0;
    this.Gl = Gl | 0;
    this.Hh = Hh | 0;
    this.Hl = Hl | 0;
  }
  process(view, offset) {
    for (let i = 0; i < 16; i++, offset += 4) {
      SHA512_W_H[i] = view.getUint32(offset);
      SHA512_W_L[i] = view.getUint32(offset += 4);
    }
    for (let i = 16; i < 80; i++) {
      const W15h = SHA512_W_H[i - 15] | 0;
      const W15l = SHA512_W_L[i - 15] | 0;
      const s0h = rotrSH(W15h, W15l, 1) ^ rotrSH(W15h, W15l, 8) ^ shrSH(W15h, W15l, 7);
      const s0l = rotrSL(W15h, W15l, 1) ^ rotrSL(W15h, W15l, 8) ^ shrSL(W15h, W15l, 7);
      const W2h = SHA512_W_H[i - 2] | 0;
      const W2l = SHA512_W_L[i - 2] | 0;
      const s1h = rotrSH(W2h, W2l, 19) ^ rotrBH(W2h, W2l, 61) ^ shrSH(W2h, W2l, 6);
      const s1l = rotrSL(W2h, W2l, 19) ^ rotrBL(W2h, W2l, 61) ^ shrSL(W2h, W2l, 6);
      const SUMl = add4L(s0l, s1l, SHA512_W_L[i - 7], SHA512_W_L[i - 16]);
      const SUMh = add4H(SUMl, s0h, s1h, SHA512_W_H[i - 7], SHA512_W_H[i - 16]);
      SHA512_W_H[i] = SUMh | 0;
      SHA512_W_L[i] = SUMl | 0;
    }
    let { Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl, Eh, El, Fh, Fl, Gh, Gl, Hh, Hl } = this;
    for (let i = 0; i < 80; i++) {
      const sigma1h = rotrSH(Eh, El, 14) ^ rotrSH(Eh, El, 18) ^ rotrBH(Eh, El, 41);
      const sigma1l = rotrSL(Eh, El, 14) ^ rotrSL(Eh, El, 18) ^ rotrBL(Eh, El, 41);
      const CHIh = Eh & Fh ^ ~Eh & Gh;
      const CHIl = El & Fl ^ ~El & Gl;
      const T1ll = add5L(Hl, sigma1l, CHIl, SHA512_Kl[i], SHA512_W_L[i]);
      const T1h = add5H(T1ll, Hh, sigma1h, CHIh, SHA512_Kh[i], SHA512_W_H[i]);
      const T1l = T1ll | 0;
      const sigma0h = rotrSH(Ah, Al, 28) ^ rotrBH(Ah, Al, 34) ^ rotrBH(Ah, Al, 39);
      const sigma0l = rotrSL(Ah, Al, 28) ^ rotrBL(Ah, Al, 34) ^ rotrBL(Ah, Al, 39);
      const MAJh = Ah & Bh ^ Ah & Ch ^ Bh & Ch;
      const MAJl = Al & Bl ^ Al & Cl ^ Bl & Cl;
      Hh = Gh | 0;
      Hl = Gl | 0;
      Gh = Fh | 0;
      Gl = Fl | 0;
      Fh = Eh | 0;
      Fl = El | 0;
      ({ h: Eh, l: El } = add(Dh | 0, Dl | 0, T1h | 0, T1l | 0));
      Dh = Ch | 0;
      Dl = Cl | 0;
      Ch = Bh | 0;
      Cl = Bl | 0;
      Bh = Ah | 0;
      Bl = Al | 0;
      const All = add3L(T1l, sigma0l, MAJl);
      Ah = add3H(All, T1h, sigma0h, MAJh);
      Al = All | 0;
    }
    ({ h: Ah, l: Al } = add(this.Ah | 0, this.Al | 0, Ah | 0, Al | 0));
    ({ h: Bh, l: Bl } = add(this.Bh | 0, this.Bl | 0, Bh | 0, Bl | 0));
    ({ h: Ch, l: Cl } = add(this.Ch | 0, this.Cl | 0, Ch | 0, Cl | 0));
    ({ h: Dh, l: Dl } = add(this.Dh | 0, this.Dl | 0, Dh | 0, Dl | 0));
    ({ h: Eh, l: El } = add(this.Eh | 0, this.El | 0, Eh | 0, El | 0));
    ({ h: Fh, l: Fl } = add(this.Fh | 0, this.Fl | 0, Fh | 0, Fl | 0));
    ({ h: Gh, l: Gl } = add(this.Gh | 0, this.Gl | 0, Gh | 0, Gl | 0));
    ({ h: Hh, l: Hl } = add(this.Hh | 0, this.Hl | 0, Hh | 0, Hl | 0));
    this.set(Ah, Al, Bh, Bl, Ch, Cl, Dh, Dl, Eh, El, Fh, Fl, Gh, Gl, Hh, Hl);
  }
  roundClean() {
    clean(SHA512_W_H, SHA512_W_L);
  }
  destroy() {
    clean(this.buffer);
    this.set(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  }
};
var sha256 = /* @__PURE__ */ createHasher(() => new SHA256());
var sha512 = /* @__PURE__ */ createHasher(() => new SHA512());

// node_modules/@noble/curves/esm/utils.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n = /* @__PURE__ */ BigInt(0);
var _1n = /* @__PURE__ */ BigInt(1);
function _abool2(value, title = "") {
  if (typeof value !== "boolean") {
    const prefix = title && `"${title}"`;
    throw new Error(prefix + "expected boolean, got type=" + typeof value);
  }
  return value;
}
function _abytes2(value, length, title = "") {
  const bytes = isBytes(value);
  const len = value?.length;
  const needsLen = length !== void 0;
  if (!bytes || needsLen && len !== length) {
    const prefix = title && `"${title}" `;
    const ofLen = needsLen ? ` of length ${length}` : "";
    const got = bytes ? `length=${len}` : `type=${typeof value}`;
    throw new Error(prefix + "expected Uint8Array" + ofLen + ", got " + got);
  }
  return value;
}
function hexToNumber(hex) {
  if (typeof hex !== "string")
    throw new Error("hex string expected, got " + typeof hex);
  return hex === "" ? _0n : BigInt("0x" + hex);
}
function bytesToNumberBE(bytes) {
  return hexToNumber(bytesToHex(bytes));
}
function bytesToNumberLE(bytes) {
  abytes(bytes);
  return hexToNumber(bytesToHex(Uint8Array.from(bytes).reverse()));
}
function numberToBytesBE(n, len) {
  return hexToBytes(n.toString(16).padStart(len * 2, "0"));
}
function numberToBytesLE(n, len) {
  return numberToBytesBE(n, len).reverse();
}
function ensureBytes(title, hex, expectedLength) {
  let res;
  if (typeof hex === "string") {
    try {
      res = hexToBytes(hex);
    } catch (e) {
      throw new Error(title + " must be hex string or Uint8Array, cause: " + e);
    }
  } else if (isBytes(hex)) {
    res = Uint8Array.from(hex);
  } else {
    throw new Error(title + " must be hex string or Uint8Array");
  }
  const len = res.length;
  if (typeof expectedLength === "number" && len !== expectedLength)
    throw new Error(title + " of length " + expectedLength + " expected, got " + len);
  return res;
}
function equalBytes(a, b) {
  if (a.length !== b.length)
    return false;
  let diff = 0;
  for (let i = 0; i < a.length; i++)
    diff |= a[i] ^ b[i];
  return diff === 0;
}
function copyBytes(bytes) {
  return Uint8Array.from(bytes);
}
var isPosBig = (n) => typeof n === "bigint" && _0n <= n;
function inRange(n, min, max) {
  return isPosBig(n) && isPosBig(min) && isPosBig(max) && min <= n && n < max;
}
function aInRange(title, n, min, max) {
  if (!inRange(n, min, max))
    throw new Error("expected valid " + title + ": " + min + " <= n < " + max + ", got " + n);
}
function bitLen(n) {
  let len;
  for (len = 0; n > _0n; n >>= _1n, len += 1)
    ;
  return len;
}
var bitMask = (n) => (_1n << BigInt(n)) - _1n;
function _validateObject(object, fields, optFields = {}) {
  if (!object || typeof object !== "object")
    throw new Error("expected valid options object");
  function checkField(fieldName, expectedType, isOpt) {
    const val = object[fieldName];
    if (isOpt && val === void 0)
      return;
    const current = typeof val;
    if (current !== expectedType || val === null)
      throw new Error(`param "${fieldName}" is invalid: expected ${expectedType}, got ${current}`);
  }
  Object.entries(fields).forEach(([k, v]) => checkField(k, v, false));
  Object.entries(optFields).forEach(([k, v]) => checkField(k, v, true));
}
var notImplemented = () => {
  throw new Error("not implemented");
};
function memoized(fn) {
  const map = /* @__PURE__ */ new WeakMap();
  return (arg, ...args) => {
    const val = map.get(arg);
    if (val !== void 0)
      return val;
    const computed = fn(arg, ...args);
    map.set(arg, computed);
    return computed;
  };
}

// node_modules/@noble/curves/esm/abstract/modular.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n2 = BigInt(0);
var _1n2 = BigInt(1);
var _2n = /* @__PURE__ */ BigInt(2);
var _3n = /* @__PURE__ */ BigInt(3);
var _4n = /* @__PURE__ */ BigInt(4);
var _5n = /* @__PURE__ */ BigInt(5);
var _7n = /* @__PURE__ */ BigInt(7);
var _8n = /* @__PURE__ */ BigInt(8);
var _9n = /* @__PURE__ */ BigInt(9);
var _16n = /* @__PURE__ */ BigInt(16);
function mod(a, b) {
  const result = a % b;
  return result >= _0n2 ? result : b + result;
}
function pow2(x, power, modulo) {
  let res = x;
  while (power-- > _0n2) {
    res *= res;
    res %= modulo;
  }
  return res;
}
function invert(number, modulo) {
  if (number === _0n2)
    throw new Error("invert: expected non-zero number");
  if (modulo <= _0n2)
    throw new Error("invert: expected positive modulus, got " + modulo);
  let a = mod(number, modulo);
  let b = modulo;
  let x = _0n2, y = _1n2, u = _1n2, v = _0n2;
  while (a !== _0n2) {
    const q = b / a;
    const r = b % a;
    const m = x - u * q;
    const n = y - v * q;
    b = a, a = r, x = u, y = v, u = m, v = n;
  }
  const gcd = b;
  if (gcd !== _1n2)
    throw new Error("invert: does not exist");
  return mod(x, modulo);
}
function assertIsSquare(Fp2, root, n) {
  if (!Fp2.eql(Fp2.sqr(root), n))
    throw new Error("Cannot find square root");
}
function sqrt3mod4(Fp2, n) {
  const p1div4 = (Fp2.ORDER + _1n2) / _4n;
  const root = Fp2.pow(n, p1div4);
  assertIsSquare(Fp2, root, n);
  return root;
}
function sqrt5mod8(Fp2, n) {
  const p5div8 = (Fp2.ORDER - _5n) / _8n;
  const n2 = Fp2.mul(n, _2n);
  const v = Fp2.pow(n2, p5div8);
  const nv = Fp2.mul(n, v);
  const i = Fp2.mul(Fp2.mul(nv, _2n), v);
  const root = Fp2.mul(nv, Fp2.sub(i, Fp2.ONE));
  assertIsSquare(Fp2, root, n);
  return root;
}
function sqrt9mod16(P) {
  const Fp_ = Field(P);
  const tn = tonelliShanks(P);
  const c1 = tn(Fp_, Fp_.neg(Fp_.ONE));
  const c2 = tn(Fp_, c1);
  const c3 = tn(Fp_, Fp_.neg(c1));
  const c4 = (P + _7n) / _16n;
  return (Fp2, n) => {
    let tv1 = Fp2.pow(n, c4);
    let tv2 = Fp2.mul(tv1, c1);
    const tv3 = Fp2.mul(tv1, c2);
    const tv4 = Fp2.mul(tv1, c3);
    const e1 = Fp2.eql(Fp2.sqr(tv2), n);
    const e2 = Fp2.eql(Fp2.sqr(tv3), n);
    tv1 = Fp2.cmov(tv1, tv2, e1);
    tv2 = Fp2.cmov(tv4, tv3, e2);
    const e3 = Fp2.eql(Fp2.sqr(tv2), n);
    const root = Fp2.cmov(tv1, tv2, e3);
    assertIsSquare(Fp2, root, n);
    return root;
  };
}
function tonelliShanks(P) {
  if (P < _3n)
    throw new Error("sqrt is not defined for small field");
  let Q = P - _1n2;
  let S = 0;
  while (Q % _2n === _0n2) {
    Q /= _2n;
    S++;
  }
  let Z = _2n;
  const _Fp = Field(P);
  while (FpLegendre(_Fp, Z) === 1) {
    if (Z++ > 1e3)
      throw new Error("Cannot find square root: probably non-prime P");
  }
  if (S === 1)
    return sqrt3mod4;
  let cc = _Fp.pow(Z, Q);
  const Q1div2 = (Q + _1n2) / _2n;
  return function tonelliSlow(Fp2, n) {
    if (Fp2.is0(n))
      return n;
    if (FpLegendre(Fp2, n) !== 1)
      throw new Error("Cannot find square root");
    let M = S;
    let c = Fp2.mul(Fp2.ONE, cc);
    let t = Fp2.pow(n, Q);
    let R = Fp2.pow(n, Q1div2);
    while (!Fp2.eql(t, Fp2.ONE)) {
      if (Fp2.is0(t))
        return Fp2.ZERO;
      let i = 1;
      let t_tmp = Fp2.sqr(t);
      while (!Fp2.eql(t_tmp, Fp2.ONE)) {
        i++;
        t_tmp = Fp2.sqr(t_tmp);
        if (i === M)
          throw new Error("Cannot find square root");
      }
      const exponent = _1n2 << BigInt(M - i - 1);
      const b = Fp2.pow(c, exponent);
      M = i;
      c = Fp2.sqr(b);
      t = Fp2.mul(t, c);
      R = Fp2.mul(R, b);
    }
    return R;
  };
}
function FpSqrt(P) {
  if (P % _4n === _3n)
    return sqrt3mod4;
  if (P % _8n === _5n)
    return sqrt5mod8;
  if (P % _16n === _9n)
    return sqrt9mod16(P);
  return tonelliShanks(P);
}
var isNegativeLE = (num, modulo) => (mod(num, modulo) & _1n2) === _1n2;
var FIELD_FIELDS = [
  "create",
  "isValid",
  "is0",
  "neg",
  "inv",
  "sqrt",
  "sqr",
  "eql",
  "add",
  "sub",
  "mul",
  "pow",
  "div",
  "addN",
  "subN",
  "mulN",
  "sqrN"
];
function validateField(field) {
  const initial = {
    ORDER: "bigint",
    MASK: "bigint",
    BYTES: "number",
    BITS: "number"
  };
  const opts = FIELD_FIELDS.reduce((map, val) => {
    map[val] = "function";
    return map;
  }, initial);
  _validateObject(field, opts);
  return field;
}
function FpPow(Fp2, num, power) {
  if (power < _0n2)
    throw new Error("invalid exponent, negatives unsupported");
  if (power === _0n2)
    return Fp2.ONE;
  if (power === _1n2)
    return num;
  let p = Fp2.ONE;
  let d = num;
  while (power > _0n2) {
    if (power & _1n2)
      p = Fp2.mul(p, d);
    d = Fp2.sqr(d);
    power >>= _1n2;
  }
  return p;
}
function FpInvertBatch(Fp2, nums, passZero = false) {
  const inverted = new Array(nums.length).fill(passZero ? Fp2.ZERO : void 0);
  const multipliedAcc = nums.reduce((acc, num, i) => {
    if (Fp2.is0(num))
      return acc;
    inverted[i] = acc;
    return Fp2.mul(acc, num);
  }, Fp2.ONE);
  const invertedAcc = Fp2.inv(multipliedAcc);
  nums.reduceRight((acc, num, i) => {
    if (Fp2.is0(num))
      return acc;
    inverted[i] = Fp2.mul(acc, inverted[i]);
    return Fp2.mul(acc, num);
  }, invertedAcc);
  return inverted;
}
function FpLegendre(Fp2, n) {
  const p1mod2 = (Fp2.ORDER - _1n2) / _2n;
  const powered = Fp2.pow(n, p1mod2);
  const yes = Fp2.eql(powered, Fp2.ONE);
  const zero = Fp2.eql(powered, Fp2.ZERO);
  const no = Fp2.eql(powered, Fp2.neg(Fp2.ONE));
  if (!yes && !zero && !no)
    throw new Error("invalid Legendre symbol result");
  return yes ? 1 : zero ? 0 : -1;
}
function nLength(n, nBitLength) {
  if (nBitLength !== void 0)
    anumber(nBitLength);
  const _nBitLength = nBitLength !== void 0 ? nBitLength : n.toString(2).length;
  const nByteLength = Math.ceil(_nBitLength / 8);
  return { nBitLength: _nBitLength, nByteLength };
}
function Field(ORDER, bitLenOrOpts, isLE2 = false, opts = {}) {
  if (ORDER <= _0n2)
    throw new Error("invalid field: expected ORDER > 0, got " + ORDER);
  let _nbitLength = void 0;
  let _sqrt = void 0;
  let modFromBytes = false;
  let allowedLengths = void 0;
  if (typeof bitLenOrOpts === "object" && bitLenOrOpts != null) {
    if (opts.sqrt || isLE2)
      throw new Error("cannot specify opts in two arguments");
    const _opts = bitLenOrOpts;
    if (_opts.BITS)
      _nbitLength = _opts.BITS;
    if (_opts.sqrt)
      _sqrt = _opts.sqrt;
    if (typeof _opts.isLE === "boolean")
      isLE2 = _opts.isLE;
    if (typeof _opts.modFromBytes === "boolean")
      modFromBytes = _opts.modFromBytes;
    allowedLengths = _opts.allowedLengths;
  } else {
    if (typeof bitLenOrOpts === "number")
      _nbitLength = bitLenOrOpts;
    if (opts.sqrt)
      _sqrt = opts.sqrt;
  }
  const { nBitLength: BITS, nByteLength: BYTES } = nLength(ORDER, _nbitLength);
  if (BYTES > 2048)
    throw new Error("invalid field: expected ORDER of <= 2048 bytes");
  let sqrtP;
  const f = Object.freeze({
    ORDER,
    isLE: isLE2,
    BITS,
    BYTES,
    MASK: bitMask(BITS),
    ZERO: _0n2,
    ONE: _1n2,
    allowedLengths,
    create: (num) => mod(num, ORDER),
    isValid: (num) => {
      if (typeof num !== "bigint")
        throw new Error("invalid field element: expected bigint, got " + typeof num);
      return _0n2 <= num && num < ORDER;
    },
    is0: (num) => num === _0n2,
    // is valid and invertible
    isValidNot0: (num) => !f.is0(num) && f.isValid(num),
    isOdd: (num) => (num & _1n2) === _1n2,
    neg: (num) => mod(-num, ORDER),
    eql: (lhs, rhs) => lhs === rhs,
    sqr: (num) => mod(num * num, ORDER),
    add: (lhs, rhs) => mod(lhs + rhs, ORDER),
    sub: (lhs, rhs) => mod(lhs - rhs, ORDER),
    mul: (lhs, rhs) => mod(lhs * rhs, ORDER),
    pow: (num, power) => FpPow(f, num, power),
    div: (lhs, rhs) => mod(lhs * invert(rhs, ORDER), ORDER),
    // Same as above, but doesn't normalize
    sqrN: (num) => num * num,
    addN: (lhs, rhs) => lhs + rhs,
    subN: (lhs, rhs) => lhs - rhs,
    mulN: (lhs, rhs) => lhs * rhs,
    inv: (num) => invert(num, ORDER),
    sqrt: _sqrt || ((n) => {
      if (!sqrtP)
        sqrtP = FpSqrt(ORDER);
      return sqrtP(f, n);
    }),
    toBytes: (num) => isLE2 ? numberToBytesLE(num, BYTES) : numberToBytesBE(num, BYTES),
    fromBytes: (bytes, skipValidation = true) => {
      if (allowedLengths) {
        if (!allowedLengths.includes(bytes.length) || bytes.length > BYTES) {
          throw new Error("Field.fromBytes: expected " + allowedLengths + " bytes, got " + bytes.length);
        }
        const padded = new Uint8Array(BYTES);
        padded.set(bytes, isLE2 ? 0 : padded.length - bytes.length);
        bytes = padded;
      }
      if (bytes.length !== BYTES)
        throw new Error("Field.fromBytes: expected " + BYTES + " bytes, got " + bytes.length);
      let scalar = isLE2 ? bytesToNumberLE(bytes) : bytesToNumberBE(bytes);
      if (modFromBytes)
        scalar = mod(scalar, ORDER);
      if (!skipValidation) {
        if (!f.isValid(scalar))
          throw new Error("invalid field element: outside of range 0..ORDER");
      }
      return scalar;
    },
    // TODO: we don't need it here, move out to separate fn
    invertBatch: (lst) => FpInvertBatch(f, lst),
    // We can't move this out because Fp6, Fp12 implement it
    // and it's unclear what to return in there.
    cmov: (a, b, c) => c ? b : a
  });
  return Object.freeze(f);
}

// node_modules/@noble/curves/esm/abstract/curve.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n3 = BigInt(0);
var _1n3 = BigInt(1);
function negateCt(condition, item) {
  const neg = item.negate();
  return condition ? neg : item;
}
function normalizeZ(c, points) {
  const invertedZs = FpInvertBatch(c.Fp, points.map((p) => p.Z));
  return points.map((p, i) => c.fromAffine(p.toAffine(invertedZs[i])));
}
function validateW(W, bits) {
  if (!Number.isSafeInteger(W) || W <= 0 || W > bits)
    throw new Error("invalid window size, expected [1.." + bits + "], got W=" + W);
}
function calcWOpts(W, scalarBits) {
  validateW(W, scalarBits);
  const windows = Math.ceil(scalarBits / W) + 1;
  const windowSize = 2 ** (W - 1);
  const maxNumber = 2 ** W;
  const mask = bitMask(W);
  const shiftBy = BigInt(W);
  return { windows, windowSize, mask, maxNumber, shiftBy };
}
function calcOffsets(n, window, wOpts) {
  const { windowSize, mask, maxNumber, shiftBy } = wOpts;
  let wbits = Number(n & mask);
  let nextN = n >> shiftBy;
  if (wbits > windowSize) {
    wbits -= maxNumber;
    nextN += _1n3;
  }
  const offsetStart = window * windowSize;
  const offset = offsetStart + Math.abs(wbits) - 1;
  const isZero = wbits === 0;
  const isNeg = wbits < 0;
  const isNegF = window % 2 !== 0;
  const offsetF = offsetStart;
  return { nextN, offset, isZero, isNeg, isNegF, offsetF };
}
function validateMSMPoints(points, c) {
  if (!Array.isArray(points))
    throw new Error("array expected");
  points.forEach((p, i) => {
    if (!(p instanceof c))
      throw new Error("invalid point at index " + i);
  });
}
function validateMSMScalars(scalars, field) {
  if (!Array.isArray(scalars))
    throw new Error("array of scalars expected");
  scalars.forEach((s, i) => {
    if (!field.isValid(s))
      throw new Error("invalid scalar at index " + i);
  });
}
var pointPrecomputes = /* @__PURE__ */ new WeakMap();
var pointWindowSizes = /* @__PURE__ */ new WeakMap();
function getW(P) {
  return pointWindowSizes.get(P) || 1;
}
function assert0(n) {
  if (n !== _0n3)
    throw new Error("invalid wNAF");
}
var wNAF = class {
  // Parametrized with a given Point class (not individual point)
  constructor(Point, bits) {
    this.BASE = Point.BASE;
    this.ZERO = Point.ZERO;
    this.Fn = Point.Fn;
    this.bits = bits;
  }
  // non-const time multiplication ladder
  _unsafeLadder(elm, n, p = this.ZERO) {
    let d = elm;
    while (n > _0n3) {
      if (n & _1n3)
        p = p.add(d);
      d = d.double();
      n >>= _1n3;
    }
    return p;
  }
  /**
   * Creates a wNAF precomputation window. Used for caching.
   * Default window size is set by `utils.precompute()` and is equal to 8.
   * Number of precomputed points depends on the curve size:
   * 2^(𝑊−1) * (Math.ceil(𝑛 / 𝑊) + 1), where:
   * - 𝑊 is the window size
   * - 𝑛 is the bitlength of the curve order.
   * For a 256-bit curve and window size 8, the number of precomputed points is 128 * 33 = 4224.
   * @param point Point instance
   * @param W window size
   * @returns precomputed point tables flattened to a single array
   */
  precomputeWindow(point, W) {
    const { windows, windowSize } = calcWOpts(W, this.bits);
    const points = [];
    let p = point;
    let base = p;
    for (let window = 0; window < windows; window++) {
      base = p;
      points.push(base);
      for (let i = 1; i < windowSize; i++) {
        base = base.add(p);
        points.push(base);
      }
      p = base.double();
    }
    return points;
  }
  /**
   * Implements ec multiplication using precomputed tables and w-ary non-adjacent form.
   * More compact implementation:
   * https://github.com/paulmillr/noble-secp256k1/blob/47cb1669b6e506ad66b35fe7d76132ae97465da2/index.ts#L502-L541
   * @returns real and fake (for const-time) points
   */
  wNAF(W, precomputes, n) {
    if (!this.Fn.isValid(n))
      throw new Error("invalid scalar");
    let p = this.ZERO;
    let f = this.BASE;
    const wo = calcWOpts(W, this.bits);
    for (let window = 0; window < wo.windows; window++) {
      const { nextN, offset, isZero, isNeg, isNegF, offsetF } = calcOffsets(n, window, wo);
      n = nextN;
      if (isZero) {
        f = f.add(negateCt(isNegF, precomputes[offsetF]));
      } else {
        p = p.add(negateCt(isNeg, precomputes[offset]));
      }
    }
    assert0(n);
    return { p, f };
  }
  /**
   * Implements ec unsafe (non const-time) multiplication using precomputed tables and w-ary non-adjacent form.
   * @param acc accumulator point to add result of multiplication
   * @returns point
   */
  wNAFUnsafe(W, precomputes, n, acc = this.ZERO) {
    const wo = calcWOpts(W, this.bits);
    for (let window = 0; window < wo.windows; window++) {
      if (n === _0n3)
        break;
      const { nextN, offset, isZero, isNeg } = calcOffsets(n, window, wo);
      n = nextN;
      if (isZero) {
        continue;
      } else {
        const item = precomputes[offset];
        acc = acc.add(isNeg ? item.negate() : item);
      }
    }
    assert0(n);
    return acc;
  }
  getPrecomputes(W, point, transform) {
    let comp = pointPrecomputes.get(point);
    if (!comp) {
      comp = this.precomputeWindow(point, W);
      if (W !== 1) {
        if (typeof transform === "function")
          comp = transform(comp);
        pointPrecomputes.set(point, comp);
      }
    }
    return comp;
  }
  cached(point, scalar, transform) {
    const W = getW(point);
    return this.wNAF(W, this.getPrecomputes(W, point, transform), scalar);
  }
  unsafe(point, scalar, transform, prev) {
    const W = getW(point);
    if (W === 1)
      return this._unsafeLadder(point, scalar, prev);
    return this.wNAFUnsafe(W, this.getPrecomputes(W, point, transform), scalar, prev);
  }
  // We calculate precomputes for elliptic curve point multiplication
  // using windowed method. This specifies window size and
  // stores precomputed values. Usually only base point would be precomputed.
  createCache(P, W) {
    validateW(W, this.bits);
    pointWindowSizes.set(P, W);
    pointPrecomputes.delete(P);
  }
  hasCache(elm) {
    return getW(elm) !== 1;
  }
};
function pippenger(c, fieldN, points, scalars) {
  validateMSMPoints(points, c);
  validateMSMScalars(scalars, fieldN);
  const plength = points.length;
  const slength = scalars.length;
  if (plength !== slength)
    throw new Error("arrays of points and scalars must have equal length");
  const zero = c.ZERO;
  const wbits = bitLen(BigInt(plength));
  let windowSize = 1;
  if (wbits > 12)
    windowSize = wbits - 3;
  else if (wbits > 4)
    windowSize = wbits - 2;
  else if (wbits > 0)
    windowSize = 2;
  const MASK = bitMask(windowSize);
  const buckets = new Array(Number(MASK) + 1).fill(zero);
  const lastBits = Math.floor((fieldN.BITS - 1) / windowSize) * windowSize;
  let sum = zero;
  for (let i = lastBits; i >= 0; i -= windowSize) {
    buckets.fill(zero);
    for (let j = 0; j < slength; j++) {
      const scalar = scalars[j];
      const wbits2 = Number(scalar >> BigInt(i) & MASK);
      buckets[wbits2] = buckets[wbits2].add(points[j]);
    }
    let resI = zero;
    for (let j = buckets.length - 1, sumI = zero; j > 0; j--) {
      sumI = sumI.add(buckets[j]);
      resI = resI.add(sumI);
    }
    sum = sum.add(resI);
    if (i !== 0)
      for (let j = 0; j < windowSize; j++)
        sum = sum.double();
  }
  return sum;
}
function createField(order, field, isLE2) {
  if (field) {
    if (field.ORDER !== order)
      throw new Error("Field.ORDER must match order: Fp == p, Fn == n");
    validateField(field);
    return field;
  } else {
    return Field(order, { isLE: isLE2 });
  }
}
function _createCurveFields(type, CURVE, curveOpts = {}, FpFnLE) {
  if (FpFnLE === void 0)
    FpFnLE = type === "edwards";
  if (!CURVE || typeof CURVE !== "object")
    throw new Error(`expected valid ${type} CURVE object`);
  for (const p of ["p", "n", "h"]) {
    const val = CURVE[p];
    if (!(typeof val === "bigint" && val > _0n3))
      throw new Error(`CURVE.${p} must be positive bigint`);
  }
  const Fp2 = createField(CURVE.p, curveOpts.Fp, FpFnLE);
  const Fn2 = createField(CURVE.n, curveOpts.Fn, FpFnLE);
  const _b = type === "weierstrass" ? "b" : "d";
  const params = ["Gx", "Gy", "a", _b];
  for (const p of params) {
    if (!Fp2.isValid(CURVE[p]))
      throw new Error(`CURVE.${p} must be valid field element of CURVE.Fp`);
  }
  CURVE = Object.freeze(Object.assign({}, CURVE));
  return { CURVE, Fp: Fp2, Fn: Fn2 };
}

// node_modules/@noble/curves/esm/abstract/edwards.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n4 = BigInt(0);
var _1n4 = BigInt(1);
var _2n2 = BigInt(2);
var _8n2 = BigInt(8);
function isEdValidXY(Fp2, CURVE, x, y) {
  const x2 = Fp2.sqr(x);
  const y2 = Fp2.sqr(y);
  const left = Fp2.add(Fp2.mul(CURVE.a, x2), y2);
  const right = Fp2.add(Fp2.ONE, Fp2.mul(CURVE.d, Fp2.mul(x2, y2)));
  return Fp2.eql(left, right);
}
function edwards(params, extraOpts = {}) {
  const validated = _createCurveFields("edwards", params, extraOpts, extraOpts.FpFnLE);
  const { Fp: Fp2, Fn: Fn2 } = validated;
  let CURVE = validated.CURVE;
  const { h: cofactor } = CURVE;
  _validateObject(extraOpts, {}, { uvRatio: "function" });
  const MASK = _2n2 << BigInt(Fn2.BYTES * 8) - _1n4;
  const modP = (n) => Fp2.create(n);
  const uvRatio2 = extraOpts.uvRatio || ((u, v) => {
    try {
      return { isValid: true, value: Fp2.sqrt(Fp2.div(u, v)) };
    } catch (e) {
      return { isValid: false, value: _0n4 };
    }
  });
  if (!isEdValidXY(Fp2, CURVE, CURVE.Gx, CURVE.Gy))
    throw new Error("bad curve params: generator point");
  function acoord(title, n, banZero = false) {
    const min = banZero ? _1n4 : _0n4;
    aInRange("coordinate " + title, n, min, MASK);
    return n;
  }
  function aextpoint(other) {
    if (!(other instanceof Point))
      throw new Error("ExtendedPoint expected");
  }
  const toAffineMemo = memoized((p, iz) => {
    const { X, Y, Z } = p;
    const is0 = p.is0();
    if (iz == null)
      iz = is0 ? _8n2 : Fp2.inv(Z);
    const x = modP(X * iz);
    const y = modP(Y * iz);
    const zz = Fp2.mul(Z, iz);
    if (is0)
      return { x: _0n4, y: _1n4 };
    if (zz !== _1n4)
      throw new Error("invZ was invalid");
    return { x, y };
  });
  const assertValidMemo = memoized((p) => {
    const { a, d } = CURVE;
    if (p.is0())
      throw new Error("bad point: ZERO");
    const { X, Y, Z, T } = p;
    const X2 = modP(X * X);
    const Y2 = modP(Y * Y);
    const Z2 = modP(Z * Z);
    const Z4 = modP(Z2 * Z2);
    const aX2 = modP(X2 * a);
    const left = modP(Z2 * modP(aX2 + Y2));
    const right = modP(Z4 + modP(d * modP(X2 * Y2)));
    if (left !== right)
      throw new Error("bad point: equation left != right (1)");
    const XY = modP(X * Y);
    const ZT = modP(Z * T);
    if (XY !== ZT)
      throw new Error("bad point: equation left != right (2)");
    return true;
  });
  class Point {
    constructor(X, Y, Z, T) {
      this.X = acoord("x", X);
      this.Y = acoord("y", Y);
      this.Z = acoord("z", Z, true);
      this.T = acoord("t", T);
      Object.freeze(this);
    }
    static CURVE() {
      return CURVE;
    }
    static fromAffine(p) {
      if (p instanceof Point)
        throw new Error("extended point not allowed");
      const { x, y } = p || {};
      acoord("x", x);
      acoord("y", y);
      return new Point(x, y, _1n4, modP(x * y));
    }
    // Uses algo from RFC8032 5.1.3.
    static fromBytes(bytes, zip215 = false) {
      const len = Fp2.BYTES;
      const { a, d } = CURVE;
      bytes = copyBytes(_abytes2(bytes, len, "point"));
      _abool2(zip215, "zip215");
      const normed = copyBytes(bytes);
      const lastByte = bytes[len - 1];
      normed[len - 1] = lastByte & ~128;
      const y = bytesToNumberLE(normed);
      const max = zip215 ? MASK : Fp2.ORDER;
      aInRange("point.y", y, _0n4, max);
      const y2 = modP(y * y);
      const u = modP(y2 - _1n4);
      const v = modP(d * y2 - a);
      let { isValid, value: x } = uvRatio2(u, v);
      if (!isValid)
        throw new Error("bad point: invalid y coordinate");
      const isXOdd = (x & _1n4) === _1n4;
      const isLastByteOdd = (lastByte & 128) !== 0;
      if (!zip215 && x === _0n4 && isLastByteOdd)
        throw new Error("bad point: x=0 and x_0=1");
      if (isLastByteOdd !== isXOdd)
        x = modP(-x);
      return Point.fromAffine({ x, y });
    }
    static fromHex(bytes, zip215 = false) {
      return Point.fromBytes(ensureBytes("point", bytes), zip215);
    }
    get x() {
      return this.toAffine().x;
    }
    get y() {
      return this.toAffine().y;
    }
    precompute(windowSize = 8, isLazy = true) {
      wnaf.createCache(this, windowSize);
      if (!isLazy)
        this.multiply(_2n2);
      return this;
    }
    // Useful in fromAffine() - not for fromBytes(), which always created valid points.
    assertValidity() {
      assertValidMemo(this);
    }
    // Compare one point to another.
    equals(other) {
      aextpoint(other);
      const { X: X1, Y: Y1, Z: Z1 } = this;
      const { X: X2, Y: Y2, Z: Z2 } = other;
      const X1Z2 = modP(X1 * Z2);
      const X2Z1 = modP(X2 * Z1);
      const Y1Z2 = modP(Y1 * Z2);
      const Y2Z1 = modP(Y2 * Z1);
      return X1Z2 === X2Z1 && Y1Z2 === Y2Z1;
    }
    is0() {
      return this.equals(Point.ZERO);
    }
    negate() {
      return new Point(modP(-this.X), this.Y, this.Z, modP(-this.T));
    }
    // Fast algo for doubling Extended Point.
    // https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html#doubling-dbl-2008-hwcd
    // Cost: 4M + 4S + 1*a + 6add + 1*2.
    double() {
      const { a } = CURVE;
      const { X: X1, Y: Y1, Z: Z1 } = this;
      const A = modP(X1 * X1);
      const B = modP(Y1 * Y1);
      const C = modP(_2n2 * modP(Z1 * Z1));
      const D = modP(a * A);
      const x1y1 = X1 + Y1;
      const E = modP(modP(x1y1 * x1y1) - A - B);
      const G = D + B;
      const F = G - C;
      const H = D - B;
      const X3 = modP(E * F);
      const Y3 = modP(G * H);
      const T3 = modP(E * H);
      const Z3 = modP(F * G);
      return new Point(X3, Y3, Z3, T3);
    }
    // Fast algo for adding 2 Extended Points.
    // https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html#addition-add-2008-hwcd
    // Cost: 9M + 1*a + 1*d + 7add.
    add(other) {
      aextpoint(other);
      const { a, d } = CURVE;
      const { X: X1, Y: Y1, Z: Z1, T: T1 } = this;
      const { X: X2, Y: Y2, Z: Z2, T: T2 } = other;
      const A = modP(X1 * X2);
      const B = modP(Y1 * Y2);
      const C = modP(T1 * d * T2);
      const D = modP(Z1 * Z2);
      const E = modP((X1 + Y1) * (X2 + Y2) - A - B);
      const F = D - C;
      const G = D + C;
      const H = modP(B - a * A);
      const X3 = modP(E * F);
      const Y3 = modP(G * H);
      const T3 = modP(E * H);
      const Z3 = modP(F * G);
      return new Point(X3, Y3, Z3, T3);
    }
    subtract(other) {
      return this.add(other.negate());
    }
    // Constant-time multiplication.
    multiply(scalar) {
      if (!Fn2.isValidNot0(scalar))
        throw new Error("invalid scalar: expected 1 <= sc < curve.n");
      const { p, f } = wnaf.cached(this, scalar, (p2) => normalizeZ(Point, p2));
      return normalizeZ(Point, [p, f])[0];
    }
    // Non-constant-time multiplication. Uses double-and-add algorithm.
    // It's faster, but should only be used when you don't care about
    // an exposed private key e.g. sig verification.
    // Does NOT allow scalars higher than CURVE.n.
    // Accepts optional accumulator to merge with multiply (important for sparse scalars)
    multiplyUnsafe(scalar, acc = Point.ZERO) {
      if (!Fn2.isValid(scalar))
        throw new Error("invalid scalar: expected 0 <= sc < curve.n");
      if (scalar === _0n4)
        return Point.ZERO;
      if (this.is0() || scalar === _1n4)
        return this;
      return wnaf.unsafe(this, scalar, (p) => normalizeZ(Point, p), acc);
    }
    // Checks if point is of small order.
    // If you add something to small order point, you will have "dirty"
    // point with torsion component.
    // Multiplies point by cofactor and checks if the result is 0.
    isSmallOrder() {
      return this.multiplyUnsafe(cofactor).is0();
    }
    // Multiplies point by curve order and checks if the result is 0.
    // Returns `false` is the point is dirty.
    isTorsionFree() {
      return wnaf.unsafe(this, CURVE.n).is0();
    }
    // Converts Extended point to default (x, y) coordinates.
    // Can accept precomputed Z^-1 - for example, from invertBatch.
    toAffine(invertedZ) {
      return toAffineMemo(this, invertedZ);
    }
    clearCofactor() {
      if (cofactor === _1n4)
        return this;
      return this.multiplyUnsafe(cofactor);
    }
    toBytes() {
      const { x, y } = this.toAffine();
      const bytes = Fp2.toBytes(y);
      bytes[bytes.length - 1] |= x & _1n4 ? 128 : 0;
      return bytes;
    }
    toHex() {
      return bytesToHex(this.toBytes());
    }
    toString() {
      return `<Point ${this.is0() ? "ZERO" : this.toHex()}>`;
    }
    // TODO: remove
    get ex() {
      return this.X;
    }
    get ey() {
      return this.Y;
    }
    get ez() {
      return this.Z;
    }
    get et() {
      return this.T;
    }
    static normalizeZ(points) {
      return normalizeZ(Point, points);
    }
    static msm(points, scalars) {
      return pippenger(Point, Fn2, points, scalars);
    }
    _setWindowSize(windowSize) {
      this.precompute(windowSize);
    }
    toRawBytes() {
      return this.toBytes();
    }
  }
  Point.BASE = new Point(CURVE.Gx, CURVE.Gy, _1n4, modP(CURVE.Gx * CURVE.Gy));
  Point.ZERO = new Point(_0n4, _1n4, _1n4, _0n4);
  Point.Fp = Fp2;
  Point.Fn = Fn2;
  const wnaf = new wNAF(Point, Fn2.BITS);
  Point.BASE.precompute(8);
  return Point;
}
var PrimeEdwardsPoint = class {
  constructor(ep) {
    this.ep = ep;
  }
  // Static methods that must be implemented by subclasses
  static fromBytes(_bytes) {
    notImplemented();
  }
  static fromHex(_hex) {
    notImplemented();
  }
  get x() {
    return this.toAffine().x;
  }
  get y() {
    return this.toAffine().y;
  }
  // Common implementations
  clearCofactor() {
    return this;
  }
  assertValidity() {
    this.ep.assertValidity();
  }
  toAffine(invertedZ) {
    return this.ep.toAffine(invertedZ);
  }
  toHex() {
    return bytesToHex(this.toBytes());
  }
  toString() {
    return this.toHex();
  }
  isTorsionFree() {
    return true;
  }
  isSmallOrder() {
    return false;
  }
  add(other) {
    this.assertSame(other);
    return this.init(this.ep.add(other.ep));
  }
  subtract(other) {
    this.assertSame(other);
    return this.init(this.ep.subtract(other.ep));
  }
  multiply(scalar) {
    return this.init(this.ep.multiply(scalar));
  }
  multiplyUnsafe(scalar) {
    return this.init(this.ep.multiplyUnsafe(scalar));
  }
  double() {
    return this.init(this.ep.double());
  }
  negate() {
    return this.init(this.ep.negate());
  }
  precompute(windowSize, isLazy) {
    return this.init(this.ep.precompute(windowSize, isLazy));
  }
  /** @deprecated use `toBytes` */
  toRawBytes() {
    return this.toBytes();
  }
};
function eddsa(Point, cHash, eddsaOpts = {}) {
  if (typeof cHash !== "function")
    throw new Error('"hash" function param is required');
  _validateObject(eddsaOpts, {}, {
    adjustScalarBytes: "function",
    randomBytes: "function",
    domain: "function",
    prehash: "function",
    mapToCurve: "function"
  });
  const { prehash } = eddsaOpts;
  const { BASE, Fp: Fp2, Fn: Fn2 } = Point;
  const randomBytes3 = eddsaOpts.randomBytes || randomBytes;
  const adjustScalarBytes2 = eddsaOpts.adjustScalarBytes || ((bytes) => bytes);
  const domain = eddsaOpts.domain || ((data, ctx, phflag) => {
    _abool2(phflag, "phflag");
    if (ctx.length || phflag)
      throw new Error("Contexts/pre-hash are not supported");
    return data;
  });
  function modN_LE(hash2) {
    return Fn2.create(bytesToNumberLE(hash2));
  }
  function getPrivateScalar(key) {
    const len = lengths.secretKey;
    key = ensureBytes("private key", key, len);
    const hashed = ensureBytes("hashed private key", cHash(key), 2 * len);
    const head = adjustScalarBytes2(hashed.slice(0, len));
    const prefix = hashed.slice(len, 2 * len);
    const scalar = modN_LE(head);
    return { head, prefix, scalar };
  }
  function getExtendedPublicKey(secretKey) {
    const { head, prefix, scalar } = getPrivateScalar(secretKey);
    const point = BASE.multiply(scalar);
    const pointBytes = point.toBytes();
    return { head, prefix, scalar, point, pointBytes };
  }
  function getPublicKey(secretKey) {
    return getExtendedPublicKey(secretKey).pointBytes;
  }
  function hashDomainToScalar(context = Uint8Array.of(), ...msgs) {
    const msg = concatBytes(...msgs);
    return modN_LE(cHash(domain(msg, ensureBytes("context", context), !!prehash)));
  }
  function sign(msg, secretKey, options = {}) {
    msg = ensureBytes("message", msg);
    if (prehash)
      msg = prehash(msg);
    const { prefix, scalar, pointBytes } = getExtendedPublicKey(secretKey);
    const r = hashDomainToScalar(options.context, prefix, msg);
    const R = BASE.multiply(r).toBytes();
    const k = hashDomainToScalar(options.context, R, pointBytes, msg);
    const s = Fn2.create(r + k * scalar);
    if (!Fn2.isValid(s))
      throw new Error("sign failed: invalid s");
    const rs = concatBytes(R, Fn2.toBytes(s));
    return _abytes2(rs, lengths.signature, "result");
  }
  const verifyOpts = { zip215: true };
  function verify(sig, msg, publicKey, options = verifyOpts) {
    const { context, zip215 } = options;
    const len = lengths.signature;
    sig = ensureBytes("signature", sig, len);
    msg = ensureBytes("message", msg);
    publicKey = ensureBytes("publicKey", publicKey, lengths.publicKey);
    if (zip215 !== void 0)
      _abool2(zip215, "zip215");
    if (prehash)
      msg = prehash(msg);
    const mid = len / 2;
    const r = sig.subarray(0, mid);
    const s = bytesToNumberLE(sig.subarray(mid, len));
    let A, R, SB;
    try {
      A = Point.fromBytes(publicKey, zip215);
      R = Point.fromBytes(r, zip215);
      SB = BASE.multiplyUnsafe(s);
    } catch (error) {
      return false;
    }
    if (!zip215 && A.isSmallOrder())
      return false;
    const k = hashDomainToScalar(context, R.toBytes(), A.toBytes(), msg);
    const RkA = R.add(A.multiplyUnsafe(k));
    return RkA.subtract(SB).clearCofactor().is0();
  }
  const _size = Fp2.BYTES;
  const lengths = {
    secretKey: _size,
    publicKey: _size,
    signature: 2 * _size,
    seed: _size
  };
  function randomSecretKey(seed = randomBytes3(lengths.seed)) {
    return _abytes2(seed, lengths.seed, "seed");
  }
  function keygen(seed) {
    const secretKey = utils.randomSecretKey(seed);
    return { secretKey, publicKey: getPublicKey(secretKey) };
  }
  function isValidSecretKey(key) {
    return isBytes(key) && key.length === Fn2.BYTES;
  }
  function isValidPublicKey(key, zip215) {
    try {
      return !!Point.fromBytes(key, zip215);
    } catch (error) {
      return false;
    }
  }
  const utils = {
    getExtendedPublicKey,
    randomSecretKey,
    isValidSecretKey,
    isValidPublicKey,
    /**
     * Converts ed public key to x public key. Uses formula:
     * - ed25519:
     *   - `(u, v) = ((1+y)/(1-y), sqrt(-486664)*u/x)`
     *   - `(x, y) = (sqrt(-486664)*u/v, (u-1)/(u+1))`
     * - ed448:
     *   - `(u, v) = ((y-1)/(y+1), sqrt(156324)*u/x)`
     *   - `(x, y) = (sqrt(156324)*u/v, (1+u)/(1-u))`
     */
    toMontgomery(publicKey) {
      const { y } = Point.fromBytes(publicKey);
      const size = lengths.publicKey;
      const is25519 = size === 32;
      if (!is25519 && size !== 57)
        throw new Error("only defined for 25519 and 448");
      const u = is25519 ? Fp2.div(_1n4 + y, _1n4 - y) : Fp2.div(y - _1n4, y + _1n4);
      return Fp2.toBytes(u);
    },
    toMontgomerySecret(secretKey) {
      const size = lengths.secretKey;
      _abytes2(secretKey, size);
      const hashed = cHash(secretKey.subarray(0, size));
      return adjustScalarBytes2(hashed).subarray(0, size);
    },
    /** @deprecated */
    randomPrivateKey: randomSecretKey,
    /** @deprecated */
    precompute(windowSize = 8, point = Point.BASE) {
      return point.precompute(windowSize, false);
    }
  };
  return Object.freeze({
    keygen,
    getPublicKey,
    sign,
    verify,
    utils,
    Point,
    lengths
  });
}
function _eddsa_legacy_opts_to_new(c) {
  const CURVE = {
    a: c.a,
    d: c.d,
    p: c.Fp.ORDER,
    n: c.n,
    h: c.h,
    Gx: c.Gx,
    Gy: c.Gy
  };
  const Fp2 = c.Fp;
  const Fn2 = Field(CURVE.n, c.nBitLength, true);
  const curveOpts = { Fp: Fp2, Fn: Fn2, uvRatio: c.uvRatio };
  const eddsaOpts = {
    randomBytes: c.randomBytes,
    adjustScalarBytes: c.adjustScalarBytes,
    domain: c.domain,
    prehash: c.prehash,
    mapToCurve: c.mapToCurve
  };
  return { CURVE, curveOpts, hash: c.hash, eddsaOpts };
}
function _eddsa_new_output_to_legacy(c, eddsa2) {
  const Point = eddsa2.Point;
  const legacy = Object.assign({}, eddsa2, {
    ExtendedPoint: Point,
    CURVE: c,
    nBitLength: Point.Fn.BITS,
    nByteLength: Point.Fn.BYTES
  });
  return legacy;
}
function twistedEdwards(c) {
  const { CURVE, curveOpts, hash: hash2, eddsaOpts } = _eddsa_legacy_opts_to_new(c);
  const Point = edwards(CURVE, curveOpts);
  const EDDSA = eddsa(Point, hash2, eddsaOpts);
  return _eddsa_new_output_to_legacy(c, EDDSA);
}

// node_modules/@noble/curves/esm/abstract/hash-to-curve.js
var _DST_scalar = utf8ToBytes("HashToScalar-");

// node_modules/@noble/curves/esm/abstract/montgomery.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n5 = BigInt(0);
var _1n5 = BigInt(1);
var _2n3 = BigInt(2);
function validateOpts(curve) {
  _validateObject(curve, {
    adjustScalarBytes: "function",
    powPminus2: "function"
  });
  return Object.freeze({ ...curve });
}
function montgomery(curveDef) {
  const CURVE = validateOpts(curveDef);
  const { P, type, adjustScalarBytes: adjustScalarBytes2, powPminus2, randomBytes: rand } = CURVE;
  const is25519 = type === "x25519";
  if (!is25519 && type !== "x448")
    throw new Error("invalid type");
  const randomBytes_ = rand || randomBytes;
  const montgomeryBits = is25519 ? 255 : 448;
  const fieldLen = is25519 ? 32 : 56;
  const Gu = is25519 ? BigInt(9) : BigInt(5);
  const a24 = is25519 ? BigInt(121665) : BigInt(39081);
  const minScalar = is25519 ? _2n3 ** BigInt(254) : _2n3 ** BigInt(447);
  const maxAdded = is25519 ? BigInt(8) * _2n3 ** BigInt(251) - _1n5 : BigInt(4) * _2n3 ** BigInt(445) - _1n5;
  const maxScalar = minScalar + maxAdded + _1n5;
  const modP = (n) => mod(n, P);
  const GuBytes = encodeU(Gu);
  function encodeU(u) {
    return numberToBytesLE(modP(u), fieldLen);
  }
  function decodeU(u) {
    const _u = ensureBytes("u coordinate", u, fieldLen);
    if (is25519)
      _u[31] &= 127;
    return modP(bytesToNumberLE(_u));
  }
  function decodeScalar(scalar) {
    return bytesToNumberLE(adjustScalarBytes2(ensureBytes("scalar", scalar, fieldLen)));
  }
  function scalarMult(scalar, u) {
    const pu = montgomeryLadder(decodeU(u), decodeScalar(scalar));
    if (pu === _0n5)
      throw new Error("invalid private or public key received");
    return encodeU(pu);
  }
  function scalarMultBase(scalar) {
    return scalarMult(scalar, GuBytes);
  }
  function cswap(swap, x_2, x_3) {
    const dummy = modP(swap * (x_2 - x_3));
    x_2 = modP(x_2 - dummy);
    x_3 = modP(x_3 + dummy);
    return { x_2, x_3 };
  }
  function montgomeryLadder(u, scalar) {
    aInRange("u", u, _0n5, P);
    aInRange("scalar", scalar, minScalar, maxScalar);
    const k = scalar;
    const x_1 = u;
    let x_2 = _1n5;
    let z_2 = _0n5;
    let x_3 = u;
    let z_3 = _1n5;
    let swap = _0n5;
    for (let t = BigInt(montgomeryBits - 1); t >= _0n5; t--) {
      const k_t = k >> t & _1n5;
      swap ^= k_t;
      ({ x_2, x_3 } = cswap(swap, x_2, x_3));
      ({ x_2: z_2, x_3: z_3 } = cswap(swap, z_2, z_3));
      swap = k_t;
      const A = x_2 + z_2;
      const AA = modP(A * A);
      const B = x_2 - z_2;
      const BB = modP(B * B);
      const E = AA - BB;
      const C = x_3 + z_3;
      const D = x_3 - z_3;
      const DA = modP(D * A);
      const CB = modP(C * B);
      const dacb = DA + CB;
      const da_cb = DA - CB;
      x_3 = modP(dacb * dacb);
      z_3 = modP(x_1 * modP(da_cb * da_cb));
      x_2 = modP(AA * BB);
      z_2 = modP(E * (AA + modP(a24 * E)));
    }
    ({ x_2, x_3 } = cswap(swap, x_2, x_3));
    ({ x_2: z_2, x_3: z_3 } = cswap(swap, z_2, z_3));
    const z2 = powPminus2(z_2);
    return modP(x_2 * z2);
  }
  const lengths = {
    secretKey: fieldLen,
    publicKey: fieldLen,
    seed: fieldLen
  };
  const randomSecretKey = (seed = randomBytes_(fieldLen)) => {
    abytes(seed, lengths.seed);
    return seed;
  };
  function keygen(seed) {
    const secretKey = randomSecretKey(seed);
    return { secretKey, publicKey: scalarMultBase(secretKey) };
  }
  const utils = {
    randomSecretKey,
    randomPrivateKey: randomSecretKey
  };
  return {
    keygen,
    getSharedSecret: (secretKey, publicKey) => scalarMult(secretKey, publicKey),
    getPublicKey: (secretKey) => scalarMultBase(secretKey),
    scalarMult,
    scalarMultBase,
    utils,
    GuBytes: GuBytes.slice(),
    lengths
  };
}

// node_modules/@noble/curves/esm/ed25519.js
/*! noble-curves - MIT License (c) 2022 Paul Miller (paulmillr.com) */
var _0n6 = /* @__PURE__ */ BigInt(0);
var _1n6 = BigInt(1);
var _2n4 = BigInt(2);
var _3n2 = BigInt(3);
var _5n2 = BigInt(5);
var _8n3 = BigInt(8);
var ed25519_CURVE_p = BigInt("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed");
var ed25519_CURVE = /* @__PURE__ */ (() => ({
  p: ed25519_CURVE_p,
  n: BigInt("0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed"),
  h: _8n3,
  a: BigInt("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec"),
  d: BigInt("0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3"),
  Gx: BigInt("0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a"),
  Gy: BigInt("0x6666666666666666666666666666666666666666666666666666666666666658")
}))();
function ed25519_pow_2_252_3(x) {
  const _10n = BigInt(10), _20n = BigInt(20), _40n = BigInt(40), _80n = BigInt(80);
  const P = ed25519_CURVE_p;
  const x2 = x * x % P;
  const b2 = x2 * x % P;
  const b4 = pow2(b2, _2n4, P) * b2 % P;
  const b5 = pow2(b4, _1n6, P) * x % P;
  const b10 = pow2(b5, _5n2, P) * b5 % P;
  const b20 = pow2(b10, _10n, P) * b10 % P;
  const b40 = pow2(b20, _20n, P) * b20 % P;
  const b80 = pow2(b40, _40n, P) * b40 % P;
  const b160 = pow2(b80, _80n, P) * b80 % P;
  const b240 = pow2(b160, _80n, P) * b80 % P;
  const b250 = pow2(b240, _10n, P) * b10 % P;
  const pow_p_5_8 = pow2(b250, _2n4, P) * x % P;
  return { pow_p_5_8, b2 };
}
function adjustScalarBytes(bytes) {
  bytes[0] &= 248;
  bytes[31] &= 127;
  bytes[31] |= 64;
  return bytes;
}
var ED25519_SQRT_M1 = /* @__PURE__ */ BigInt("19681161376707505956807079304988542015446066515923890162744021073123829784752");
function uvRatio(u, v) {
  const P = ed25519_CURVE_p;
  const v3 = mod(v * v * v, P);
  const v7 = mod(v3 * v3 * v, P);
  const pow = ed25519_pow_2_252_3(u * v7).pow_p_5_8;
  let x = mod(u * v3 * pow, P);
  const vx2 = mod(v * x * x, P);
  const root1 = x;
  const root2 = mod(x * ED25519_SQRT_M1, P);
  const useRoot1 = vx2 === u;
  const useRoot2 = vx2 === mod(-u, P);
  const noRoot = vx2 === mod(-u * ED25519_SQRT_M1, P);
  if (useRoot1)
    x = root1;
  if (useRoot2 || noRoot)
    x = root2;
  if (isNegativeLE(x, P))
    x = mod(-x, P);
  return { isValid: useRoot1 || useRoot2, value: x };
}
var Fp = /* @__PURE__ */ (() => Field(ed25519_CURVE.p, { isLE: true }))();
var Fn = /* @__PURE__ */ (() => Field(ed25519_CURVE.n, { isLE: true }))();
var ed25519Defaults = /* @__PURE__ */ (() => ({
  ...ed25519_CURVE,
  Fp,
  hash: sha512,
  adjustScalarBytes,
  // dom2
  // Ratio of u to v. Allows us to combine inversion and square root. Uses algo from RFC8032 5.1.3.
  // Constant-time, u/√v
  uvRatio
}))();
var ed25519 = /* @__PURE__ */ (() => twistedEdwards(ed25519Defaults))();
var x25519 = /* @__PURE__ */ (() => {
  const P = Fp.ORDER;
  return montgomery({
    P,
    type: "x25519",
    powPminus2: (x) => {
      const { pow_p_5_8, b2 } = ed25519_pow_2_252_3(x);
      return mod(pow2(pow_p_5_8, _3n2, P) * b2, P);
    },
    adjustScalarBytes
  });
})();
var SQRT_M1 = ED25519_SQRT_M1;
var SQRT_AD_MINUS_ONE = /* @__PURE__ */ BigInt("25063068953384623474111414158702152701244531502492656460079210482610430750235");
var INVSQRT_A_MINUS_D = /* @__PURE__ */ BigInt("54469307008909316920995813868745141605393597292927456921205312896311721017578");
var ONE_MINUS_D_SQ = /* @__PURE__ */ BigInt("1159843021668779879193775521855586647937357759715417654439879720876111806838");
var D_MINUS_ONE_SQ = /* @__PURE__ */ BigInt("40440834346308536858101042469323190826248399146238708352240133220865137265952");
var invertSqrt = (number) => uvRatio(_1n6, number);
var MAX_255B = /* @__PURE__ */ BigInt("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
var bytes255ToNumberLE = (bytes) => ed25519.Point.Fp.create(bytesToNumberLE(bytes) & MAX_255B);
function calcElligatorRistrettoMap(r0) {
  const { d } = ed25519_CURVE;
  const P = ed25519_CURVE_p;
  const mod2 = (n) => Fp.create(n);
  const r = mod2(SQRT_M1 * r0 * r0);
  const Ns = mod2((r + _1n6) * ONE_MINUS_D_SQ);
  let c = BigInt(-1);
  const D = mod2((c - d * r) * mod2(r + d));
  let { isValid: Ns_D_is_sq, value: s } = uvRatio(Ns, D);
  let s_ = mod2(s * r0);
  if (!isNegativeLE(s_, P))
    s_ = mod2(-s_);
  if (!Ns_D_is_sq)
    s = s_;
  if (!Ns_D_is_sq)
    c = r;
  const Nt = mod2(c * (r - _1n6) * D_MINUS_ONE_SQ - D);
  const s2 = s * s;
  const W0 = mod2((s + s) * D);
  const W1 = mod2(Nt * SQRT_AD_MINUS_ONE);
  const W2 = mod2(_1n6 - s2);
  const W3 = mod2(_1n6 + s2);
  return new ed25519.Point(mod2(W0 * W3), mod2(W2 * W1), mod2(W1 * W3), mod2(W0 * W2));
}
function ristretto255_map(bytes) {
  abytes(bytes, 64);
  const r1 = bytes255ToNumberLE(bytes.subarray(0, 32));
  const R1 = calcElligatorRistrettoMap(r1);
  const r2 = bytes255ToNumberLE(bytes.subarray(32, 64));
  const R2 = calcElligatorRistrettoMap(r2);
  return new _RistrettoPoint(R1.add(R2));
}
var _RistrettoPoint = class extends PrimeEdwardsPoint {
  constructor(ep) {
    super(ep);
  }
  static fromAffine(ap) {
    return new _RistrettoPoint(ed25519.Point.fromAffine(ap));
  }
  assertSame(other) {
    if (!(other instanceof _RistrettoPoint))
      throw new Error("RistrettoPoint expected");
  }
  init(ep) {
    return new _RistrettoPoint(ep);
  }
  /** @deprecated use `import { ristretto255_hasher } from '@noble/curves/ed25519.js';` */
  static hashToCurve(hex) {
    return ristretto255_map(ensureBytes("ristrettoHash", hex, 64));
  }
  static fromBytes(bytes) {
    abytes(bytes, 32);
    const { a, d } = ed25519_CURVE;
    const P = ed25519_CURVE_p;
    const mod2 = (n) => Fp.create(n);
    const s = bytes255ToNumberLE(bytes);
    if (!equalBytes(Fp.toBytes(s), bytes) || isNegativeLE(s, P))
      throw new Error("invalid ristretto255 encoding 1");
    const s2 = mod2(s * s);
    const u1 = mod2(_1n6 + a * s2);
    const u2 = mod2(_1n6 - a * s2);
    const u1_2 = mod2(u1 * u1);
    const u2_2 = mod2(u2 * u2);
    const v = mod2(a * d * u1_2 - u2_2);
    const { isValid, value: I } = invertSqrt(mod2(v * u2_2));
    const Dx = mod2(I * u2);
    const Dy = mod2(I * Dx * v);
    let x = mod2((s + s) * Dx);
    if (isNegativeLE(x, P))
      x = mod2(-x);
    const y = mod2(u1 * Dy);
    const t = mod2(x * y);
    if (!isValid || isNegativeLE(t, P) || y === _0n6)
      throw new Error("invalid ristretto255 encoding 2");
    return new _RistrettoPoint(new ed25519.Point(x, y, _1n6, t));
  }
  /**
   * Converts ristretto-encoded string to ristretto point.
   * Described in [RFC9496](https://www.rfc-editor.org/rfc/rfc9496#name-decode).
   * @param hex Ristretto-encoded 32 bytes. Not every 32-byte string is valid ristretto encoding
   */
  static fromHex(hex) {
    return _RistrettoPoint.fromBytes(ensureBytes("ristrettoHex", hex, 32));
  }
  static msm(points, scalars) {
    return pippenger(_RistrettoPoint, ed25519.Point.Fn, points, scalars);
  }
  /**
   * Encodes ristretto point to Uint8Array.
   * Described in [RFC9496](https://www.rfc-editor.org/rfc/rfc9496#name-encode).
   */
  toBytes() {
    let { X, Y, Z, T } = this.ep;
    const P = ed25519_CURVE_p;
    const mod2 = (n) => Fp.create(n);
    const u1 = mod2(mod2(Z + Y) * mod2(Z - Y));
    const u2 = mod2(X * Y);
    const u2sq = mod2(u2 * u2);
    const { value: invsqrt } = invertSqrt(mod2(u1 * u2sq));
    const D1 = mod2(invsqrt * u1);
    const D2 = mod2(invsqrt * u2);
    const zInv = mod2(D1 * D2 * T);
    let D;
    if (isNegativeLE(T * zInv, P)) {
      let _x = mod2(Y * SQRT_M1);
      let _y = mod2(X * SQRT_M1);
      X = _x;
      Y = _y;
      D = mod2(D1 * INVSQRT_A_MINUS_D);
    } else {
      D = D2;
    }
    if (isNegativeLE(X * zInv, P))
      Y = mod2(-Y);
    let s = mod2((Z - Y) * D);
    if (isNegativeLE(s, P))
      s = mod2(-s);
    return Fp.toBytes(s);
  }
  /**
   * Compares two Ristretto points.
   * Described in [RFC9496](https://www.rfc-editor.org/rfc/rfc9496#name-equals).
   */
  equals(other) {
    this.assertSame(other);
    const { X: X1, Y: Y1 } = this.ep;
    const { X: X2, Y: Y2 } = other.ep;
    const mod2 = (n) => Fp.create(n);
    const one = mod2(X1 * Y2) === mod2(Y1 * X2);
    const two = mod2(Y1 * Y2) === mod2(X1 * X2);
    return one || two;
  }
  is0() {
    return this.equals(_RistrettoPoint.ZERO);
  }
};
_RistrettoPoint.BASE = /* @__PURE__ */ (() => new _RistrettoPoint(ed25519.Point.BASE))();
_RistrettoPoint.ZERO = /* @__PURE__ */ (() => new _RistrettoPoint(ed25519.Point.ZERO))();
_RistrettoPoint.Fp = /* @__PURE__ */ (() => Fp)();
_RistrettoPoint.Fn = /* @__PURE__ */ (() => Fn)();

// node_modules/@noble/ciphers/esm/utils.js
/*! noble-ciphers - MIT License (c) 2023 Paul Miller (paulmillr.com) */
function isBytes2(a) {
  return a instanceof Uint8Array || ArrayBuffer.isView(a) && a.constructor.name === "Uint8Array";
}
function abool(b) {
  if (typeof b !== "boolean")
    throw new Error(`boolean expected, not ${b}`);
}
function abytes2(b, ...lengths) {
  if (!isBytes2(b))
    throw new Error("Uint8Array expected");
  if (lengths.length > 0 && !lengths.includes(b.length))
    throw new Error("Uint8Array expected of length " + lengths + ", got length=" + b.length);
}
function aexists2(instance, checkFinished = true) {
  if (instance.destroyed)
    throw new Error("Hash instance has been destroyed");
  if (checkFinished && instance.finished)
    throw new Error("Hash#digest() has already been called");
}
function aoutput2(out, instance) {
  abytes2(out);
  const min = instance.outputLen;
  if (out.length < min) {
    throw new Error("digestInto() expects output buffer of length at least " + min);
  }
}
function u8(arr) {
  return new Uint8Array(arr.buffer, arr.byteOffset, arr.byteLength);
}
function u32(arr) {
  return new Uint32Array(arr.buffer, arr.byteOffset, Math.floor(arr.byteLength / 4));
}
function clean2(...arrays) {
  for (let i = 0; i < arrays.length; i++) {
    arrays[i].fill(0);
  }
}
function createView2(arr) {
  return new DataView(arr.buffer, arr.byteOffset, arr.byteLength);
}
var isLE = /* @__PURE__ */ (() => new Uint8Array(new Uint32Array([287454020]).buffer)[0] === 68)();
function utf8ToBytes2(str) {
  if (typeof str !== "string")
    throw new Error("string expected");
  return new Uint8Array(new TextEncoder().encode(str));
}
function toBytes2(data) {
  if (typeof data === "string")
    data = utf8ToBytes2(data);
  else if (isBytes2(data))
    data = copyBytes2(data);
  else
    throw new Error("Uint8Array expected, got " + typeof data);
  return data;
}
function equalBytes2(a, b) {
  if (a.length !== b.length)
    return false;
  let diff = 0;
  for (let i = 0; i < a.length; i++)
    diff |= a[i] ^ b[i];
  return diff === 0;
}
var wrapCipher = (params, constructor) => {
  function wrappedCipher(key, ...args) {
    abytes2(key);
    if (!isLE)
      throw new Error("Non little-endian hardware is not yet supported");
    if (params.nonceLength !== void 0) {
      const nonce = args[0];
      if (!nonce)
        throw new Error("nonce / iv required");
      if (params.varSizeNonce)
        abytes2(nonce);
      else
        abytes2(nonce, params.nonceLength);
    }
    const tagl = params.tagLength;
    if (tagl && args[1] !== void 0) {
      abytes2(args[1]);
    }
    const cipher = constructor(key, ...args);
    const checkOutput = (fnLength, output) => {
      if (output !== void 0) {
        if (fnLength !== 2)
          throw new Error("cipher output not supported");
        abytes2(output);
      }
    };
    let called = false;
    const wrCipher = {
      encrypt(data, output) {
        if (called)
          throw new Error("cannot encrypt() twice with same key + nonce");
        called = true;
        abytes2(data);
        checkOutput(cipher.encrypt.length, output);
        return cipher.encrypt(data, output);
      },
      decrypt(data, output) {
        abytes2(data);
        if (tagl && data.length < tagl)
          throw new Error("invalid ciphertext length: smaller than tagLength=" + tagl);
        checkOutput(cipher.decrypt.length, output);
        return cipher.decrypt(data, output);
      }
    };
    return wrCipher;
  }
  Object.assign(wrappedCipher, params);
  return wrappedCipher;
};
function getOutput(expectedLength, out, onlyAligned = true) {
  if (out === void 0)
    return new Uint8Array(expectedLength);
  if (out.length !== expectedLength)
    throw new Error("invalid output length, expected " + expectedLength + ", got: " + out.length);
  if (onlyAligned && !isAligned32(out))
    throw new Error("invalid output, must be aligned");
  return out;
}
function setBigUint642(view, byteOffset, value, isLE2) {
  if (typeof view.setBigUint64 === "function")
    return view.setBigUint64(byteOffset, value, isLE2);
  const _32n2 = BigInt(32);
  const _u32_max = BigInt(4294967295);
  const wh = Number(value >> _32n2 & _u32_max);
  const wl = Number(value & _u32_max);
  const h = isLE2 ? 4 : 0;
  const l = isLE2 ? 0 : 4;
  view.setUint32(byteOffset + h, wh, isLE2);
  view.setUint32(byteOffset + l, wl, isLE2);
}
function u64Lengths(dataLength, aadLength, isLE2) {
  abool(isLE2);
  const num = new Uint8Array(16);
  const view = createView2(num);
  setBigUint642(view, 0, BigInt(aadLength), isLE2);
  setBigUint642(view, 8, BigInt(dataLength), isLE2);
  return num;
}
function isAligned32(bytes) {
  return bytes.byteOffset % 4 === 0;
}
function copyBytes2(bytes) {
  return Uint8Array.from(bytes);
}

// node_modules/@noble/ciphers/esm/_polyval.js
var BLOCK_SIZE = 16;
var ZEROS16 = /* @__PURE__ */ new Uint8Array(16);
var ZEROS32 = u32(ZEROS16);
var POLY = 225;
var mul2 = (s0, s1, s2, s3) => {
  const hiBit = s3 & 1;
  return {
    s3: s2 << 31 | s3 >>> 1,
    s2: s1 << 31 | s2 >>> 1,
    s1: s0 << 31 | s1 >>> 1,
    s0: s0 >>> 1 ^ POLY << 24 & -(hiBit & 1)
    // reduce % poly
  };
};
var swapLE = (n) => (n >>> 0 & 255) << 24 | (n >>> 8 & 255) << 16 | (n >>> 16 & 255) << 8 | n >>> 24 & 255 | 0;
function _toGHASHKey(k) {
  k.reverse();
  const hiBit = k[15] & 1;
  let carry = 0;
  for (let i = 0; i < k.length; i++) {
    const t = k[i];
    k[i] = t >>> 1 | carry;
    carry = (t & 1) << 7;
  }
  k[0] ^= -hiBit & 225;
  return k;
}
var estimateWindow = (bytes) => {
  if (bytes > 64 * 1024)
    return 8;
  if (bytes > 1024)
    return 4;
  return 2;
};
var GHASH = class {
  // We select bits per window adaptively based on expectedLength
  constructor(key, expectedLength) {
    this.blockLen = BLOCK_SIZE;
    this.outputLen = BLOCK_SIZE;
    this.s0 = 0;
    this.s1 = 0;
    this.s2 = 0;
    this.s3 = 0;
    this.finished = false;
    key = toBytes2(key);
    abytes2(key, 16);
    const kView = createView2(key);
    let k0 = kView.getUint32(0, false);
    let k1 = kView.getUint32(4, false);
    let k2 = kView.getUint32(8, false);
    let k3 = kView.getUint32(12, false);
    const doubles = [];
    for (let i = 0; i < 128; i++) {
      doubles.push({ s0: swapLE(k0), s1: swapLE(k1), s2: swapLE(k2), s3: swapLE(k3) });
      ({ s0: k0, s1: k1, s2: k2, s3: k3 } = mul2(k0, k1, k2, k3));
    }
    const W = estimateWindow(expectedLength || 1024);
    if (![1, 2, 4, 8].includes(W))
      throw new Error("ghash: invalid window size, expected 2, 4 or 8");
    this.W = W;
    const bits = 128;
    const windows = bits / W;
    const windowSize = this.windowSize = 2 ** W;
    const items = [];
    for (let w = 0; w < windows; w++) {
      for (let byte = 0; byte < windowSize; byte++) {
        let s0 = 0, s1 = 0, s2 = 0, s3 = 0;
        for (let j = 0; j < W; j++) {
          const bit = byte >>> W - j - 1 & 1;
          if (!bit)
            continue;
          const { s0: d0, s1: d1, s2: d2, s3: d3 } = doubles[W * w + j];
          s0 ^= d0, s1 ^= d1, s2 ^= d2, s3 ^= d3;
        }
        items.push({ s0, s1, s2, s3 });
      }
    }
    this.t = items;
  }
  _updateBlock(s0, s1, s2, s3) {
    s0 ^= this.s0, s1 ^= this.s1, s2 ^= this.s2, s3 ^= this.s3;
    const { W, t, windowSize } = this;
    let o0 = 0, o1 = 0, o2 = 0, o3 = 0;
    const mask = (1 << W) - 1;
    let w = 0;
    for (const num of [s0, s1, s2, s3]) {
      for (let bytePos = 0; bytePos < 4; bytePos++) {
        const byte = num >>> 8 * bytePos & 255;
        for (let bitPos = 8 / W - 1; bitPos >= 0; bitPos--) {
          const bit = byte >>> W * bitPos & mask;
          const { s0: e0, s1: e1, s2: e2, s3: e3 } = t[w * windowSize + bit];
          o0 ^= e0, o1 ^= e1, o2 ^= e2, o3 ^= e3;
          w += 1;
        }
      }
    }
    this.s0 = o0;
    this.s1 = o1;
    this.s2 = o2;
    this.s3 = o3;
  }
  update(data) {
    aexists2(this);
    data = toBytes2(data);
    abytes2(data);
    const b32 = u32(data);
    const blocks = Math.floor(data.length / BLOCK_SIZE);
    const left = data.length % BLOCK_SIZE;
    for (let i = 0; i < blocks; i++) {
      this._updateBlock(b32[i * 4 + 0], b32[i * 4 + 1], b32[i * 4 + 2], b32[i * 4 + 3]);
    }
    if (left) {
      ZEROS16.set(data.subarray(blocks * BLOCK_SIZE));
      this._updateBlock(ZEROS32[0], ZEROS32[1], ZEROS32[2], ZEROS32[3]);
      clean2(ZEROS32);
    }
    return this;
  }
  destroy() {
    const { t } = this;
    for (const elm of t) {
      elm.s0 = 0, elm.s1 = 0, elm.s2 = 0, elm.s3 = 0;
    }
  }
  digestInto(out) {
    aexists2(this);
    aoutput2(out, this);
    this.finished = true;
    const { s0, s1, s2, s3 } = this;
    const o32 = u32(out);
    o32[0] = s0;
    o32[1] = s1;
    o32[2] = s2;
    o32[3] = s3;
    return out;
  }
  digest() {
    const res = new Uint8Array(BLOCK_SIZE);
    this.digestInto(res);
    this.destroy();
    return res;
  }
};
var Polyval = class extends GHASH {
  constructor(key, expectedLength) {
    key = toBytes2(key);
    abytes2(key);
    const ghKey = _toGHASHKey(copyBytes2(key));
    super(ghKey, expectedLength);
    clean2(ghKey);
  }
  update(data) {
    data = toBytes2(data);
    aexists2(this);
    const b32 = u32(data);
    const left = data.length % BLOCK_SIZE;
    const blocks = Math.floor(data.length / BLOCK_SIZE);
    for (let i = 0; i < blocks; i++) {
      this._updateBlock(swapLE(b32[i * 4 + 3]), swapLE(b32[i * 4 + 2]), swapLE(b32[i * 4 + 1]), swapLE(b32[i * 4 + 0]));
    }
    if (left) {
      ZEROS16.set(data.subarray(blocks * BLOCK_SIZE));
      this._updateBlock(swapLE(ZEROS32[3]), swapLE(ZEROS32[2]), swapLE(ZEROS32[1]), swapLE(ZEROS32[0]));
      clean2(ZEROS32);
    }
    return this;
  }
  digestInto(out) {
    aexists2(this);
    aoutput2(out, this);
    this.finished = true;
    const { s0, s1, s2, s3 } = this;
    const o32 = u32(out);
    o32[0] = s0;
    o32[1] = s1;
    o32[2] = s2;
    o32[3] = s3;
    return out.reverse();
  }
};
function wrapConstructorWithKey(hashCons) {
  const hashC = (msg, key) => hashCons(key, msg.length).update(toBytes2(msg)).digest();
  const tmp = hashCons(new Uint8Array(16), 0);
  hashC.outputLen = tmp.outputLen;
  hashC.blockLen = tmp.blockLen;
  hashC.create = (key, expectedLength) => hashCons(key, expectedLength);
  return hashC;
}
var ghash = wrapConstructorWithKey((key, expectedLength) => new GHASH(key, expectedLength));
var polyval = wrapConstructorWithKey((key, expectedLength) => new Polyval(key, expectedLength));

// node_modules/@noble/ciphers/esm/aes.js
var BLOCK_SIZE2 = 16;
var BLOCK_SIZE32 = 4;
var EMPTY_BLOCK = /* @__PURE__ */ new Uint8Array(BLOCK_SIZE2);
var POLY2 = 283;
function mul22(n) {
  return n << 1 ^ POLY2 & -(n >> 7);
}
function mul(a, b) {
  let res = 0;
  for (; b > 0; b >>= 1) {
    res ^= a & -(b & 1);
    a = mul22(a);
  }
  return res;
}
var sbox = /* @__PURE__ */ (() => {
  const t = new Uint8Array(256);
  for (let i = 0, x = 1; i < 256; i++, x ^= mul22(x))
    t[i] = x;
  const box = new Uint8Array(256);
  box[0] = 99;
  for (let i = 0; i < 255; i++) {
    let x = t[255 - i];
    x |= x << 8;
    box[t[i]] = (x ^ x >> 4 ^ x >> 5 ^ x >> 6 ^ x >> 7 ^ 99) & 255;
  }
  clean2(t);
  return box;
})();
var rotr32_8 = (n) => n << 24 | n >>> 8;
var rotl32_8 = (n) => n << 8 | n >>> 24;
function genTtable(sbox2, fn) {
  if (sbox2.length !== 256)
    throw new Error("Wrong sbox length");
  const T0 = new Uint32Array(256).map((_, j) => fn(sbox2[j]));
  const T1 = T0.map(rotl32_8);
  const T2 = T1.map(rotl32_8);
  const T3 = T2.map(rotl32_8);
  const T01 = new Uint32Array(256 * 256);
  const T23 = new Uint32Array(256 * 256);
  const sbox22 = new Uint16Array(256 * 256);
  for (let i = 0; i < 256; i++) {
    for (let j = 0; j < 256; j++) {
      const idx = i * 256 + j;
      T01[idx] = T0[i] ^ T1[j];
      T23[idx] = T2[i] ^ T3[j];
      sbox22[idx] = sbox2[i] << 8 | sbox2[j];
    }
  }
  return { sbox: sbox2, sbox2: sbox22, T0, T1, T2, T3, T01, T23 };
}
var tableEncoding = /* @__PURE__ */ genTtable(sbox, (s) => mul(s, 3) << 24 | s << 16 | s << 8 | mul(s, 2));
var xPowers = /* @__PURE__ */ (() => {
  const p = new Uint8Array(16);
  for (let i = 0, x = 1; i < 16; i++, x = mul22(x))
    p[i] = x;
  return p;
})();
function expandKeyLE(key) {
  abytes2(key);
  const len = key.length;
  if (![16, 24, 32].includes(len))
    throw new Error("aes: invalid key size, should be 16, 24 or 32, got " + len);
  const { sbox2 } = tableEncoding;
  const toClean = [];
  if (!isAligned32(key))
    toClean.push(key = copyBytes2(key));
  const k32 = u32(key);
  const Nk = k32.length;
  const subByte = (n) => applySbox(sbox2, n, n, n, n);
  const xk = new Uint32Array(len + 28);
  xk.set(k32);
  for (let i = Nk; i < xk.length; i++) {
    let t = xk[i - 1];
    if (i % Nk === 0)
      t = subByte(rotr32_8(t)) ^ xPowers[i / Nk - 1];
    else if (Nk > 6 && i % Nk === 4)
      t = subByte(t);
    xk[i] = xk[i - Nk] ^ t;
  }
  clean2(...toClean);
  return xk;
}
function apply0123(T01, T23, s0, s1, s2, s3) {
  return T01[s0 << 8 & 65280 | s1 >>> 8 & 255] ^ T23[s2 >>> 8 & 65280 | s3 >>> 24 & 255];
}
function applySbox(sbox2, s0, s1, s2, s3) {
  return sbox2[s0 & 255 | s1 & 65280] | sbox2[s2 >>> 16 & 255 | s3 >>> 16 & 65280] << 16;
}
function encrypt(xk, s0, s1, s2, s3) {
  const { sbox2, T01, T23 } = tableEncoding;
  let k = 0;
  s0 ^= xk[k++], s1 ^= xk[k++], s2 ^= xk[k++], s3 ^= xk[k++];
  const rounds = xk.length / 4 - 2;
  for (let i = 0; i < rounds; i++) {
    const t02 = xk[k++] ^ apply0123(T01, T23, s0, s1, s2, s3);
    const t12 = xk[k++] ^ apply0123(T01, T23, s1, s2, s3, s0);
    const t22 = xk[k++] ^ apply0123(T01, T23, s2, s3, s0, s1);
    const t32 = xk[k++] ^ apply0123(T01, T23, s3, s0, s1, s2);
    s0 = t02, s1 = t12, s2 = t22, s3 = t32;
  }
  const t0 = xk[k++] ^ applySbox(sbox2, s0, s1, s2, s3);
  const t1 = xk[k++] ^ applySbox(sbox2, s1, s2, s3, s0);
  const t2 = xk[k++] ^ applySbox(sbox2, s2, s3, s0, s1);
  const t3 = xk[k++] ^ applySbox(sbox2, s3, s0, s1, s2);
  return { s0: t0, s1: t1, s2: t2, s3: t3 };
}
function ctr32(xk, isLE2, nonce, src, dst) {
  abytes2(nonce, BLOCK_SIZE2);
  abytes2(src);
  dst = getOutput(src.length, dst);
  const ctr = nonce;
  const c32 = u32(ctr);
  const view = createView2(ctr);
  const src32 = u32(src);
  const dst32 = u32(dst);
  const ctrPos = isLE2 ? 0 : 12;
  const srcLen = src.length;
  let ctrNum = view.getUint32(ctrPos, isLE2);
  let { s0, s1, s2, s3 } = encrypt(xk, c32[0], c32[1], c32[2], c32[3]);
  for (let i = 0; i + 4 <= src32.length; i += 4) {
    dst32[i + 0] = src32[i + 0] ^ s0;
    dst32[i + 1] = src32[i + 1] ^ s1;
    dst32[i + 2] = src32[i + 2] ^ s2;
    dst32[i + 3] = src32[i + 3] ^ s3;
    ctrNum = ctrNum + 1 >>> 0;
    view.setUint32(ctrPos, ctrNum, isLE2);
    ({ s0, s1, s2, s3 } = encrypt(xk, c32[0], c32[1], c32[2], c32[3]));
  }
  const start = BLOCK_SIZE2 * Math.floor(src32.length / BLOCK_SIZE32);
  if (start < srcLen) {
    const b32 = new Uint32Array([s0, s1, s2, s3]);
    const buf = u8(b32);
    for (let i = start, pos = 0; i < srcLen; i++, pos++)
      dst[i] = src[i] ^ buf[pos];
    clean2(b32);
  }
  return dst;
}
function computeTag(fn, isLE2, key, data, AAD) {
  const aadLength = AAD ? AAD.length : 0;
  const h = fn.create(key, data.length + aadLength);
  if (AAD)
    h.update(AAD);
  const num = u64Lengths(8 * data.length, 8 * aadLength, isLE2);
  h.update(data);
  h.update(num);
  const res = h.digest();
  clean2(num);
  return res;
}
var gcm = /* @__PURE__ */ wrapCipher({ blockSize: 16, nonceLength: 12, tagLength: 16, varSizeNonce: true }, function aesgcm(key, nonce, AAD) {
  if (nonce.length < 8)
    throw new Error("aes/gcm: invalid nonce length");
  const tagLength = 16;
  function _computeTag(authKey, tagMask, data) {
    const tag = computeTag(ghash, false, authKey, data, AAD);
    for (let i = 0; i < tagMask.length; i++)
      tag[i] ^= tagMask[i];
    return tag;
  }
  function deriveKeys() {
    const xk = expandKeyLE(key);
    const authKey = EMPTY_BLOCK.slice();
    const counter = EMPTY_BLOCK.slice();
    ctr32(xk, false, counter, counter, authKey);
    if (nonce.length === 12) {
      counter.set(nonce);
    } else {
      const nonceLen = EMPTY_BLOCK.slice();
      const view = createView2(nonceLen);
      setBigUint642(view, 8, BigInt(nonce.length * 8), false);
      const g = ghash.create(authKey).update(nonce).update(nonceLen);
      g.digestInto(counter);
      g.destroy();
    }
    const tagMask = ctr32(xk, false, counter, EMPTY_BLOCK);
    return { xk, authKey, counter, tagMask };
  }
  return {
    encrypt(plaintext) {
      const { xk, authKey, counter, tagMask } = deriveKeys();
      const out = new Uint8Array(plaintext.length + tagLength);
      const toClean = [xk, authKey, counter, tagMask];
      if (!isAligned32(plaintext))
        toClean.push(plaintext = copyBytes2(plaintext));
      ctr32(xk, false, counter, plaintext, out.subarray(0, plaintext.length));
      const tag = _computeTag(authKey, tagMask, out.subarray(0, out.length - tagLength));
      toClean.push(tag);
      out.set(tag, plaintext.length);
      clean2(...toClean);
      return out;
    },
    decrypt(ciphertext) {
      const { xk, authKey, counter, tagMask } = deriveKeys();
      const toClean = [xk, authKey, tagMask, counter];
      if (!isAligned32(ciphertext))
        toClean.push(ciphertext = copyBytes2(ciphertext));
      const data = ciphertext.subarray(0, -tagLength);
      const passedTag = ciphertext.subarray(-tagLength);
      const tag = _computeTag(authKey, tagMask, data);
      toClean.push(tag);
      if (!equalBytes2(tag, passedTag))
        throw new Error("aes/gcm: invalid ghash tag");
      const out = ctr32(xk, false, counter, data);
      clean2(...toClean);
      return out;
    }
  };
});

// node_modules/@noble/hashes/esm/hmac.js
var HMAC = class extends Hash {
  constructor(hash2, _key) {
    super();
    this.finished = false;
    this.destroyed = false;
    ahash(hash2);
    const key = toBytes(_key);
    this.iHash = hash2.create();
    if (typeof this.iHash.update !== "function")
      throw new Error("Expected instance of class which extends utils.Hash");
    this.blockLen = this.iHash.blockLen;
    this.outputLen = this.iHash.outputLen;
    const blockLen = this.blockLen;
    const pad = new Uint8Array(blockLen);
    pad.set(key.length > blockLen ? hash2.create().update(key).digest() : key);
    for (let i = 0; i < pad.length; i++)
      pad[i] ^= 54;
    this.iHash.update(pad);
    this.oHash = hash2.create();
    for (let i = 0; i < pad.length; i++)
      pad[i] ^= 54 ^ 92;
    this.oHash.update(pad);
    clean(pad);
  }
  update(buf) {
    aexists(this);
    this.iHash.update(buf);
    return this;
  }
  digestInto(out) {
    aexists(this);
    abytes(out, this.outputLen);
    this.finished = true;
    this.iHash.digestInto(out);
    this.oHash.update(out);
    this.oHash.digestInto(out);
    this.destroy();
  }
  digest() {
    const out = new Uint8Array(this.oHash.outputLen);
    this.digestInto(out);
    return out;
  }
  _cloneInto(to) {
    to || (to = Object.create(Object.getPrototypeOf(this), {}));
    const { oHash, iHash, finished, destroyed, blockLen, outputLen } = this;
    to = to;
    to.finished = finished;
    to.destroyed = destroyed;
    to.blockLen = blockLen;
    to.outputLen = outputLen;
    to.oHash = oHash._cloneInto(to.oHash);
    to.iHash = iHash._cloneInto(to.iHash);
    return to;
  }
  clone() {
    return this._cloneInto();
  }
  destroy() {
    this.destroyed = true;
    this.oHash.destroy();
    this.iHash.destroy();
  }
};
var hmac = (hash2, key, message) => new HMAC(hash2, key).update(message).digest();
hmac.create = (hash2, key) => new HMAC(hash2, key);

// node_modules/@noble/hashes/esm/hkdf.js
function extract(hash2, ikm, salt) {
  ahash(hash2);
  if (salt === void 0)
    salt = new Uint8Array(hash2.outputLen);
  return hmac(hash2, toBytes(salt), toBytes(ikm));
}
var HKDF_COUNTER = /* @__PURE__ */ Uint8Array.from([0]);
var EMPTY_BUFFER = /* @__PURE__ */ Uint8Array.of();
function expand(hash2, prk, info, length = 32) {
  ahash(hash2);
  anumber(length);
  const olen = hash2.outputLen;
  if (length > 255 * olen)
    throw new Error("Length should be <= 255*HashLen");
  const blocks = Math.ceil(length / olen);
  if (info === void 0)
    info = EMPTY_BUFFER;
  const okm = new Uint8Array(blocks * olen);
  const HMAC2 = hmac.create(hash2, prk);
  const HMACTmp = HMAC2._cloneInto();
  const T = new Uint8Array(HMAC2.outputLen);
  for (let counter = 0; counter < blocks; counter++) {
    HKDF_COUNTER[0] = counter + 1;
    HMACTmp.update(counter === 0 ? EMPTY_BUFFER : T).update(info).update(HKDF_COUNTER).digestInto(T);
    okm.set(T, olen * counter);
    HMAC2._cloneInto(HMACTmp);
  }
  HMAC2.destroy();
  HMACTmp.destroy();
  clean(T, HKDF_COUNTER);
  return okm.slice(0, length);
}
var hkdf = (hash2, ikm, salt, info, length) => expand(hash2, extract(hash2, ikm, salt), info, length);

// src/types.ts
function isEncryptedLinkData(data) {
  return !!data && typeof data === "object" && typeof data.ciphertext === "string" && typeof data.nonce === "string";
}

// src/encryption.ts
var X25519_SEED_MESSAGE = "adam-server-link-language:x25519-seed:v1";
var ROOM_KEY_SEAL_INFO = "adam-server-link-language:room-key-seal:v1";
var AES_KEY_BYTES = 32;
var AES_NONCE_BYTES = 12;
var X25519_KEY_BYTES = 32;
function bytesToHex2(bytes) {
  let out = "";
  for (let i = 0; i < bytes.length; i++) {
    out += bytes[i].toString(16).padStart(2, "0");
  }
  return out;
}
var HEX_PAIR = /^[0-9a-fA-F]{2}$/;
function hexToBytes2(hex) {
  const clean3 = hex.trim();
  if (clean3.length % 2 !== 0) {
    throw new Error(`hexToBytes: odd-length hex string (${clean3.length} chars)`);
  }
  const out = new Uint8Array(clean3.length / 2);
  for (let i = 0; i < out.length; i++) {
    const byte = clean3.substring(i * 2, i * 2 + 2);
    if (!HEX_PAIR.test(byte)) {
      throw new Error(`hexToBytes: invalid hex byte "${byte}" at offset ${i * 2}`);
    }
    out[i] = parseInt(byte, 16);
  }
  return out;
}
function utf8ToBytes3(str) {
  return new TextEncoder().encode(str);
}
function bytesToUtf82(bytes) {
  return new TextDecoder().decode(bytes);
}
function concatBytes3(...chunks) {
  const total = chunks.reduce((sum, c) => sum + c.length, 0);
  const out = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    out.set(chunk, offset);
    offset += chunk.length;
  }
  return out;
}
function randomBytes2(length) {
  const out = new Uint8Array(length);
  globalThis.crypto.getRandomValues(out);
  return out;
}
var BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
var BASE58_MAP = /* @__PURE__ */ new Map();
for (let i = 0; i < BASE58_ALPHABET.length; i++)
  BASE58_MAP.set(BASE58_ALPHABET[i], i);
function base58Decode(input) {
  if (input.length === 0)
    return new Uint8Array(0);
  const bytes = [0];
  for (const ch of input) {
    const val = BASE58_MAP.get(ch);
    if (val === void 0)
      throw new Error(`base58: invalid character '${ch}'`);
    let carry = val;
    for (let j = 0; j < bytes.length; j++) {
      carry += bytes[j] * 58;
      bytes[j] = carry & 255;
      carry >>= 8;
    }
    while (carry > 0) {
      bytes.push(carry & 255);
      carry >>= 8;
    }
  }
  let leadingZeros = 0;
  for (const ch of input) {
    if (ch === "1")
      leadingZeros++;
    else
      break;
  }
  const result = new Uint8Array(leadingZeros + bytes.length);
  bytes.reverse();
  result.set(bytes, leadingZeros);
  return result;
}
var ED25519_MULTICODEC = new Uint8Array([237, 1]);
function didToEd25519PublicKey(did) {
  const base = did.includes("#") ? did.slice(0, did.indexOf("#")) : did;
  const prefix = "did:key:";
  if (!base.startsWith(prefix))
    throw new Error(`not a did:key DID: ${did}`);
  const multibase = base.slice(prefix.length);
  if (!multibase.startsWith("z"))
    throw new Error(`unsupported multibase: ${did}`);
  const decoded = base58Decode(multibase.slice(1));
  if (decoded.length !== 34 || decoded[0] !== ED25519_MULTICODEC[0] || decoded[1] !== ED25519_MULTICODEC[1]) {
    throw new Error(`unsupported did:key type: ${did}`);
  }
  return decoded.slice(2);
}
function verifyX25519Ownership(did, x25519PublicKeyHex, signatureHex) {
  try {
    const ed25519PubKey = didToEd25519PublicKey(did);
    const messageHash = sha256(utf8ToBytes3(x25519PublicKeyHex));
    const signatureBytes = hexToBytes2(signatureHex);
    return ed25519.verify(signatureBytes, messageHash, ed25519PubKey);
  } catch {
    return false;
  }
}
function deriveX25519KeyPair(signStringHex) {
  const signatureHex = signStringHex(X25519_SEED_MESSAGE);
  const signatureBytes = hexToBytes2(signatureHex);
  const privateKey = sha256(signatureBytes);
  const publicKey = x25519.getPublicKey(privateKey);
  return { privateKey, publicKey };
}
function generateRoomKey() {
  return randomBytes2(AES_KEY_BYTES);
}
function deriveSealKey(sharedSecret, ephemeralPublicKey, recipientPublicKey) {
  const salt = concatBytes3(ephemeralPublicKey, recipientPublicKey);
  return hkdf(sha256, sharedSecret, salt, utf8ToBytes3(ROOM_KEY_SEAL_INFO), AES_KEY_BYTES);
}
function sealRoomKeyForRecipient(roomKey, recipientPublicKey) {
  const ephemeralPrivateKey = randomBytes2(X25519_KEY_BYTES);
  const ephemeralPublicKey = x25519.getPublicKey(ephemeralPrivateKey);
  const sharedSecret = x25519.getSharedSecret(ephemeralPrivateKey, recipientPublicKey);
  const sealKey = deriveSealKey(sharedSecret, ephemeralPublicKey, recipientPublicKey);
  const nonce = randomBytes2(AES_NONCE_BYTES);
  const ciphertext = gcm(sealKey, nonce).encrypt(roomKey);
  return {
    ephemeralPublicKey: bytesToHex2(ephemeralPublicKey),
    nonce: bytesToHex2(nonce),
    ciphertext: bytesToHex2(ciphertext)
  };
}
function openRoomKeyEnvelope(envelope, recipientPrivateKey) {
  const ephemeralPublicKey = hexToBytes2(envelope.ephemeralPublicKey);
  const nonce = hexToBytes2(envelope.nonce);
  const ciphertext = hexToBytes2(envelope.ciphertext);
  const recipientPublicKey = x25519.getPublicKey(recipientPrivateKey);
  const sharedSecret = x25519.getSharedSecret(recipientPrivateKey, ephemeralPublicKey);
  const sealKey = deriveSealKey(sharedSecret, ephemeralPublicKey, recipientPublicKey);
  return gcm(sealKey, nonce).decrypt(ciphertext);
}
function buildKeyRing(entries, recipientPrivateKey) {
  const ring = /* @__PURE__ */ new Map();
  for (const entry of entries) {
    const plainKey = openRoomKeyEnvelope(entry.encryptedKey, recipientPrivateKey);
    ring.set(entry.version, plainKey);
  }
  return ring;
}
function latestKeyVersion(ring) {
  let max = 0;
  for (const v of ring.keys()) {
    if (v > max)
      max = v;
  }
  return max;
}
function statusField(status) {
  return status !== void 0 ? { status } : {};
}
function encryptLinkForWire(link, roomKey, keyVersion) {
  const canonical = JSON.stringify({
    source: link.data.source,
    predicate: link.data.predicate ?? null,
    target: link.data.target,
    author: link.author,
    timestamp: link.timestamp
  });
  const linkHashHex = bytesToHex2(sha256(utf8ToBytes3(canonical)));
  const fullPayload = {
    author: link.author,
    timestamp: link.timestamp,
    proof: link.proof,
    data: link.data
  };
  if (link.status !== void 0)
    fullPayload.status = link.status;
  const plaintext = utf8ToBytes3(JSON.stringify(fullPayload));
  const nonce = randomBytes2(AES_NONCE_BYTES);
  const ciphertext = gcm(roomKey, nonce).encrypt(plaintext);
  return {
    data: { ciphertext: bytesToHex2(ciphertext), nonce: bytesToHex2(nonce) },
    link_hash: linkHashHex,
    ...keyVersion !== void 0 ? { key_version: keyVersion } : {}
  };
}
function decryptLinkFromWire(wireLink, roomKeyOrRing) {
  if (!isEncryptedLinkData(wireLink.data)) {
    throw new Error("decryptLinkFromWire: wire link has no encrypted data (expected {ciphertext, nonce} in data)");
  }
  let roomKey;
  if (roomKeyOrRing instanceof Map) {
    const version2 = wireLink.key_version ?? 1;
    const key = roomKeyOrRing.get(version2);
    if (!key) {
      throw new Error(
        `decryptLinkFromWire: no key for version ${version2} in ring (have versions: ${[...roomKeyOrRing.keys()].join(", ")})`
      );
    }
    roomKey = key;
  } else {
    roomKey = roomKeyOrRing;
  }
  const nonce = hexToBytes2(wireLink.data.nonce);
  const ciphertext = hexToBytes2(wireLink.data.ciphertext);
  const plaintext = gcm(roomKey, nonce).decrypt(ciphertext);
  const parsed = JSON.parse(bytesToUtf82(plaintext));
  if (parsed.author && parsed.data && typeof parsed.data === "object") {
    return {
      author: parsed.author,
      timestamp: parsed.timestamp,
      proof: parsed.proof,
      ...statusField(parsed.status),
      data: parsed.data
    };
  }
  return {
    author: wireLink.author ?? "",
    timestamp: wireLink.timestamp ?? "",
    proof: wireLink.proof ?? { signature: "", key: "" },
    ...statusField(wireLink.status),
    data: parsed
  };
}

// src/auth.ts
var _session = null;
var _x25519PublicKeyHex = null;
var _authInFlight = null;
function parseJwtExpiryMs(token) {
  const parts = token.split(".");
  if (parts.length !== 3)
    return null;
  try {
    const b64 = parts[1].replace(/-/g, "+").replace(/_/g, "/");
    const padded = b64 + "=".repeat((4 - b64.length % 4) % 4);
    const binary = atob(padded);
    const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
    const payload = JSON.parse(new TextDecoder().decode(bytes));
    return typeof payload.exp === "number" ? payload.exp * 1e3 : null;
  } catch {
    return null;
  }
}
function myX25519PublicKeyHex() {
  if (_x25519PublicKeyHex)
    return _x25519PublicKeyHex;
  const agent = getAgent();
  const { publicKey } = deriveX25519KeyPair((payload) => agent.signStringHex(payload));
  _x25519PublicKeyHex = bytesToHex2(publicKey);
  return _x25519PublicKeyHex;
}
async function authenticate() {
  const config = getConfig();
  const agent = getAgent();
  const did = agent.did();
  const challenge = await requestChallenge(config, did);
  const signature = agent.signStringHex(challenge);
  const x25519Hex = myX25519PublicKeyHex();
  const x25519Signature = agent.signStringHex(x25519Hex);
  const token = await verifyChallenge(config, did, challenge, signature, x25519Hex, x25519Signature);
  const session = { token, expiresAt: parseJwtExpiryMs(token) };
  _session = session;
  return session;
}
async function getValidToken(skewMs = 3e4) {
  if (_session && (_session.expiresAt === null || _session.expiresAt - Date.now() > skewMs)) {
    return _session.token;
  }
  if (_authInFlight) {
    const session2 = await _authInFlight;
    return session2.token;
  }
  _authInFlight = authenticate().finally(() => {
    _authInFlight = null;
  });
  const session = await _authInFlight;
  return session.token;
}
function resetAuth() {
  _session = null;
  _x25519PublicKeyHex = null;
  _authInFlight = null;
}

// src/sync.ts
var _deps = null;
var _pendingMissingVersions = /* @__PURE__ */ new Set();
function initSync(deps3) {
  _deps = deps3;
  _pendingMissingVersions = /* @__PURE__ */ new Set();
}
function trackMissingKeyVersions(versions) {
  for (const v of versions)
    _pendingMissingVersions.add(v);
}
function deps() {
  if (!_deps) {
    throw new Error("sync module not initialized. Call initSync() during language init().");
  }
  return _deps;
}
function toWireLink(link) {
  const keyRing2 = deps().getKeyRing();
  if (!keyRing2) {
    return {
      author: link.author,
      timestamp: link.timestamp,
      proof: link.proof,
      ...statusField(link.status),
      data: link.data
    };
  }
  const version2 = latestKeyVersion(keyRing2);
  const key = keyRing2.get(version2);
  if (!key) {
    throw new Error("toWireLink: key ring has no keys");
  }
  return encryptLinkForWire(link, key, version2);
}
function fromWireLink(wireLink) {
  const keyRing2 = deps().getKeyRing();
  if (isEncryptedLinkData(wireLink.data) && keyRing2) {
    return decryptLinkFromWire(wireLink, keyRing2);
  }
  if (isEncryptedLinkData(wireLink.data) && !keyRing2) {
    const v = wireLink.key_version ?? 1;
    throw new Error(`no key for version ${v} (key ring not yet available)`);
  }
  return {
    author: wireLink.author ?? "",
    timestamp: wireLink.timestamp ?? "",
    proof: wireLink.proof ?? { signature: "", key: "" },
    ...statusField(wireLink.status),
    data: wireLink.data ?? { source: "", target: "" }
  };
}
function toWireDiff(diff) {
  return {
    additions: diff.additions.map(toWireLink),
    removals: diff.removals.map(toWireLink)
  };
}
function fromWireDiff(wire) {
  const missingVersions = /* @__PURE__ */ new Set();
  const additions = [];
  const removals = [];
  for (const wireLink of wire.additions ?? []) {
    try {
      additions.push(fromWireLink(wireLink));
    } catch (err) {
      if (isMissingVersionError(err)) {
        missingVersions.add(extractMissingVersion(wireLink));
      } else {
        throw err;
      }
    }
  }
  for (const wireLink of wire.removals ?? []) {
    try {
      removals.push(fromWireLink(wireLink));
    } catch (err) {
      if (isMissingVersionError(err)) {
        missingVersions.add(extractMissingVersion(wireLink));
      } else {
        throw err;
      }
    }
  }
  return { diff: { additions, removals }, missingVersions };
}
function isMissingVersionError(err) {
  return err instanceof Error && /no key for version \d+/.test(err.message);
}
function extractMissingVersion(wireLink) {
  return wireLink.key_version ?? 1;
}
function normalizeSyncEntry(entry, fallbackRevision, fallbackSequence) {
  const diff = entry.diff ?? {
    additions: entry.additions ?? [],
    removals: entry.removals ?? []
  };
  return {
    diff,
    revision: entry.revision ?? fallbackRevision,
    sequence: typeof entry.sequence === "number" ? entry.sequence : fallbackSequence
  };
}
function applyInboundWireDiff(wireDiff, sequence, revision) {
  const { diff, missingVersions } = fromWireDiff(wireDiff);
  if (missingVersions.size > 0) {
    console.warn(
      `[server-link-language] skipped ${missingVersions.size} undecryptable key version(s): ${[...missingVersions].join(", ")} \u2014 will request key grant`
    );
  }
  applyDiff(diff);
  if (revision)
    setRevision(revision);
  if (Number.isFinite(sequence))
    setSequence(sequence);
  deps().emitDiff(diff);
  return { diff, missingVersions };
}
async function commit(diff) {
  const { config, getToken } = deps();
  const token = await getToken();
  console.log(
    `[server-link-language] POST /commit: ${diff.additions.length} adds, ${diff.removals.length} removes \u2192 room=${config.roomId}`
  );
  await commitDiff(config, token, toWireDiff(diff));
  emitSyncStateSafe("Synced");
}
var MAX_COMMIT_ATTEMPTS = 3;
var RETRY_BASE_DELAY_MS = 200;
var MAX_RETRY_DELAY_MS = 5 * 60 * 1e3;
var _pendingQueue = [];
var _flushScheduled = false;
var _inflight = Promise.resolve();
var _retryTimer = null;
var _retryDelay = 2500;
async function flushBatch() {
  const queue = _pendingQueue;
  _pendingQueue = [];
  _flushScheduled = false;
  if (queue.length === 0)
    return;
  const segments = coalesceDiffs(queue);
  for (let i = 0; i < segments.length; i++) {
    const segment = segments[i];
    if (segment.additions.length === 0 && segment.removals.length === 0)
      continue;
    const ok = await commitSegmentWithRetries(segment);
    if (!ok) {
      const requeue = segments.slice(i);
      const requeueSummary = requeue.map((s, idx) => `  segment ${i + 1 + idx}: ${s.additions.length} adds, ${s.removals.length} removes`).join("\n");
      console.warn(
        `[server-link-language] segment ${i + 1}/${segments.length} failed after ${MAX_COMMIT_ATTEMPTS} attempts; re-enqueueing ${requeue.length} segment(s) for next flush cycle:
${requeueSummary}`
      );
      _pendingQueue.unshift(...requeue);
      if (!_retryTimer) {
        _retryDelay = Math.min(_retryDelay * 2, MAX_RETRY_DELAY_MS);
        _retryTimer = setTimeout(() => {
          _retryTimer = null;
          if (_pendingQueue.length > 0 && !_flushScheduled) {
            _flushScheduled = true;
            _inflight = _inflight.then(() => flushBatch()).catch((err) => {
              console.error(
                "[server-link-language] retry flush crashed; keeping the flush chain live:",
                err
              );
            });
          }
        }, _retryDelay);
      }
      emitSyncStateSafe("LinkLanguageInstalledButNotSynced");
      return;
    }
  }
  _retryDelay = 2500;
}
function emitSyncStateSafe(state) {
  try {
    _deps?.emitSyncState?.(state);
  } catch (err) {
    console.error("[server-link-language] emitSyncState failed (likely post-teardown):", err);
  }
}
async function commitSegmentWithRetries(segment) {
  let delay = RETRY_BASE_DELAY_MS;
  for (let attempt = 1; attempt <= MAX_COMMIT_ATTEMPTS; attempt++) {
    try {
      await commit(segment);
      return true;
    } catch (err) {
      const isLast = attempt === MAX_COMMIT_ATTEMPTS;
      console.error(
        `[server-link-language] batched commit attempt ${attempt}/${MAX_COMMIT_ATTEMPTS} failed (${segment.additions.length} adds, ${segment.removals.length} removes)` + (isLast ? " \u2014 giving up." : `; retrying in ${delay}ms.`),
        err
      );
      if (isLast)
        return false;
      await new Promise((resolve) => setTimeout(resolve, delay));
      delay *= 2;
    }
  }
  return false;
}
function coalesceDiffs(queue) {
  const segments = [];
  for (const next of queue) {
    const current = segments[segments.length - 1];
    if (!current) {
      segments.push({ additions: [...next.additions], removals: [...next.removals] });
      continue;
    }
    const currentAddIds = new Set(current.additions.map(linkIdentity));
    const currentRemIds = new Set(current.removals.map(linkIdentity));
    const conflict = next.additions.some((l) => currentRemIds.has(linkIdentity(l))) || next.removals.some((l) => currentAddIds.has(linkIdentity(l)));
    if (conflict) {
      segments.push({ additions: [...next.additions], removals: [...next.removals] });
    } else {
      current.additions.push(...next.additions);
      current.removals.push(...next.removals);
    }
  }
  return segments;
}
function linkIdentity(link) {
  return JSON.stringify({
    source: link.data?.source ?? "",
    predicate: link.data?.predicate ?? null,
    target: link.data?.target ?? "",
    author: link.author,
    timestamp: link.timestamp
  });
}
async function drainCommitBatch() {
  const MAX_LAPS = 10;
  for (let lap = 0; lap < MAX_LAPS; lap++) {
    await _inflight;
    if (_pendingQueue.length === 0)
      return;
    if (!_flushScheduled) {
      _flushScheduled = true;
      _inflight = _inflight.then(() => flushBatch()).catch((err) => {
        console.error(
          "[server-link-language] batch flush crashed during drain:",
          err
        );
      });
    }
  }
  if (_pendingQueue.length > 0) {
    console.warn(
      `[server-link-language] drainCommitBatch: bailed after ${MAX_LAPS} laps with ${_pendingQueue.length} segment(s) still pending \u2014 they will flush on the next enqueueCommitBatched call.`
    );
  }
}
async function bootstrap() {
  const { config, getToken } = deps();
  const token = await getToken();
  const rendered = await fetchRender(config, token);
  _pendingMissingVersions.clear();
  const renderDiff = { additions: rendered.links, removals: [] };
  const { diff, missingVersions } = fromWireDiff(renderDiff);
  if (missingVersions.size > 0) {
    console.warn(
      `[server-link-language] bootstrap: skipped ${missingVersions.size} undecryptable key version(s): ${[...missingVersions].join(", ")} \u2014 will recover after key grant`
    );
    trackMissingKeyVersions(missingVersions);
  }
  const existing = allLinks();
  applyDiff({ additions: [], removals: existing.links });
  applyDiff({ additions: diff.additions, removals: [] });
  if (rendered.revision)
    setRevision(rendered.revision);
  if (typeof rendered.sequence === "number")
    setSequence(rendered.sequence);
}
async function catchUp() {
  const { config, getToken } = deps();
  const token = await getToken();
  const since = getSequence();
  const res = await fetchSync(config, token, since);
  console.log(
    `[server-link-language] catchUp: since=${since}, received ${res.diffs.length} diff(s), revision=${res.revision}, sequence=${res.sequence}`
  );
  let last = { additions: [], removals: [] };
  const allMissingVersions = /* @__PURE__ */ new Set();
  for (const rawEntry of res.diffs) {
    const entry = normalizeSyncEntry(rawEntry, res.revision, res.sequence);
    const result = applyInboundWireDiff(entry.diff, entry.sequence, entry.revision);
    last = result.diff;
    for (const v of result.missingVersions)
      allMissingVersions.add(v);
  }
  if (res.diffs.length === 0 && res.revision) {
    setRevision(res.revision);
    setSequence(res.sequence);
  }
  for (const v of allMissingVersions)
    _pendingMissingVersions.add(v);
  if (_pendingMissingVersions.size > 0 && _deps?.refreshKeyRing) {
    const isRetry = allMissingVersions.size === 0;
    console.log(
      `[server-link-language] ${_pendingMissingVersions.size} pending missing key version(s)${isRetry ? " (retry from previous cycle)" : ""} \u2014 refreshing key ring\u2026`
    );
    try {
      const gotNew = await _deps.refreshKeyRing();
      if (gotNew === null) {
        console.log(
          "[server-link-language] key ring refresh skipped (cooldown) \u2014 will retry next cycle"
        );
      } else if (gotNew) {
        console.log("[server-link-language] key ring refreshed with new versions \u2014 re-bootstrapping");
        await bootstrap();
        const recovered = allLinks();
        if (recovered.links.length > 0) {
          deps().emitDiff({ additions: recovered.links, removals: [] });
        }
      } else {
        console.warn(
          "[server-link-language] key ring refresh returned no new versions \u2014 admin has not yet granted historical keys"
        );
      }
    } catch (err) {
      console.error("[server-link-language] key ring refresh failed:", err);
    }
  }
  emitSyncStateSafe("Synced");
  return last;
}
async function performSync() {
  try {
    const result = await catchUp();
    if (_deps?.onPostSync) {
      void _deps.onPostSync().catch((err) => {
        console.error("[server-link-language] post-sync callback failed:", err);
      });
    }
    return result;
  } catch (err) {
    console.error("[server-link-language] sync failed:", err);
    emitSyncStateSafe("LinkLanguageInstalledButNotSynced");
    return { additions: [], removals: [] };
  }
}
function currentRevision() {
  return getRevision() || "";
}
function render() {
  return allLinks();
}

// src/telepresence.ts
var _deps2 = null;
var onlineAgents = /* @__PURE__ */ new Map();
function initTelepresence(deps3) {
  _deps2 = deps3;
  onlineAgents = /* @__PURE__ */ new Map();
}
function deps2() {
  if (!_deps2) {
    throw new Error("telepresence module not initialized. Call initTelepresence() during language init().");
  }
  return _deps2;
}
function handleOnlineAgentsMessage(msg) {
  onlineAgents = new Map(msg.agents.map((a) => [a.did, a]));
}
function handlePeerJoined(msg) {
  if (!onlineAgents.has(msg.did)) {
    onlineAgents.set(msg.did, { did: msg.did });
  }
}
function handlePeerLeft(msg) {
  onlineAgents.delete(msg.did);
}
function handleStatusChanged(msg) {
  const existing = onlineAgents.get(msg.did);
  if (existing) {
    existing.status = msg.status;
  } else {
    onlineAgents.set(msg.did, { did: msg.did, status: msg.status });
  }
}
function clearOnlineAgents() {
  onlineAgents = /* @__PURE__ */ new Map();
}
async function setOnlineStatus(status) {
  deps2().send({ type: "set-online-status", status });
}
async function getOnlineAgents() {
  const myDid2 = deps2().getMyDid();
  return Array.from(onlineAgents.values()).filter((a) => a.did !== myDid2).map((a) => a.status !== void 0 ? a : { did: a.did, status: emptyPerspectiveExpression() });
}
function emptyPerspectiveExpression() {
  return {
    author: "",
    data: { links: [] },
    proof: { key: "", signature: "" },
    timestamp: ""
  };
}
async function sendSignal(remoteAgentDid, payload) {
  deps2().send({ type: "telepresence-signal", toDid: remoteAgentDid, payload });
  return {};
}
async function sendBroadcast(payload) {
  deps2().send({ type: "telepresence-broadcast", payload });
  return {};
}

// src/ws-client.ts
var DEFAULT_MIN_BACKOFF_MS = 500;
var DEFAULT_MAX_BACKOFF_MS = 3e4;
var WsClient = class {
  constructor(opts) {
    this.factory = getWebSocketFactory();
    this.conn = null;
    this.connected = false;
    /** True after the socket opens but before the server acknowledges auth. */
    this.authenticating = false;
    this.closedByUser = true;
    this.reconnectAttempt = 0;
    this.reconnectTimer = null;
    this.sendQueue = [];
    this.opts = opts;
    this.minBackoffMs = opts.minBackoffMs ?? DEFAULT_MIN_BACKOFF_MS;
    this.maxBackoffMs = opts.maxBackoffMs ?? DEFAULT_MAX_BACKOFF_MS;
  }
  async connect() {
    this.closedByUser = false;
    this.reconnectAttempt = 0;
    await this.openConnection();
  }
  close() {
    this.closedByUser = true;
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    if (this.sendQueue.length > 0) {
      console.warn(
        `[server-link-language] WsClient.close(): dropping ${this.sendQueue.length} queued outbound message(s) \u2014 they were enqueued while the socket was disconnected and will not be delivered.`
      );
    }
    this.sendQueue = [];
    this.connected = false;
    if (this.conn) {
      this.conn.close(1e3, "teardown");
      this.conn = null;
    }
  }
  send(msg) {
    const data = JSON.stringify(msg);
    if (this.conn && this.connected) {
      this.conn.send(data);
    } else if (this.sendQueue.length < 1e3) {
      this.sendQueue.push(data);
    } else {
      console.warn("[ws-client] send queue full (1000 msgs), dropping message");
    }
  }
  isOpen() {
    return this.connected;
  }
  // -------------------------------------------------------------------
  // Internals
  // -------------------------------------------------------------------
  async openConnection() {
    if (this.closedByUser)
      return;
    let url;
    try {
      url = await this.opts.getUrl();
    } catch (err) {
      console.error("[server-link-language] failed to resolve websocket URL:", err);
      this.scheduleReconnect();
      return;
    }
    if (this.closedByUser)
      return;
    const conn = this.factory.connect(url);
    this.conn = conn;
    conn.onOpen(async () => {
      this.authenticating = true;
      try {
        const token = await this.opts.getToken();
        if (this.closedByUser || this.conn !== conn)
          return;
        conn.send(JSON.stringify({ type: "auth", token }));
      } catch (err) {
        console.error("[server-link-language] failed to get token for WS auth:", err);
        conn.close(4010, "token error");
      }
    });
    conn.onMessage((data) => this.handleMessage(data));
    conn.onClose(() => {
      const wasConnected = this.connected;
      this.connected = false;
      this.authenticating = false;
      this.conn = null;
      if (wasConnected)
        this.opts.handlers.onClose?.();
      if (!this.closedByUser)
        this.scheduleReconnect();
    });
    conn.onError((err) => {
      console.error("[server-link-language] websocket error:", err);
    });
  }
  handleMessage(data) {
    let msg;
    try {
      msg = JSON.parse(data);
    } catch (err) {
      console.error("[server-link-language] failed to parse websocket message:", err);
      return;
    }
    if (msg.type === "auth-error") {
      console.error("[server-link-language] WS auth rejected:", msg.error);
      this.authenticating = false;
      this.conn?.close(4004, "auth rejected");
      return;
    }
    if (this.authenticating && msg.type === "online-agents") {
      this.authenticating = false;
      this.connected = true;
      this.reconnectAttempt = 0;
      this.flushQueue();
      this.opts.handlers.onOpen?.();
      this.opts.handlers.onOnlineAgents(msg);
      return;
    }
    switch (msg.type) {
      case "diff":
        this.opts.handlers.onDiff(msg);
        break;
      case "telepresence-signal":
        this.opts.handlers.onTelepresenceSignal(msg);
        break;
      case "telepresence-broadcast":
        this.opts.handlers.onTelepresenceBroadcast(msg);
        break;
      case "online-agents":
        this.opts.handlers.onOnlineAgents(msg);
        break;
      case "peer-joined":
        this.opts.handlers.onPeerJoined(msg);
        break;
      case "peer-left":
        this.opts.handlers.onPeerLeft(msg);
        break;
      case "status-changed":
        this.opts.handlers.onStatusChanged?.(msg);
        break;
      default:
        console.warn(
          "[server-link-language] unknown websocket message type:",
          msg.type
        );
    }
  }
  scheduleReconnect() {
    if (this.closedByUser || this.reconnectTimer)
      return;
    const attempt = this.reconnectAttempt++;
    const backoff = Math.min(this.minBackoffMs * 2 ** attempt, this.maxBackoffMs);
    const jitter = Math.random() * Math.min(250, backoff);
    const delay = backoff + jitter;
    this.opts.handlers.onReconnecting?.(attempt + 1, delay);
    this.reconnectTimer = setTimeout(() => {
      this.reconnectTimer = null;
      void this.openConnection();
    }, delay);
  }
  flushQueue() {
    if (!this.conn)
      return;
    for (const data of this.sendQueue.splice(0)) {
      this.conn.send(data);
    }
  }
};

// src/adapters-deno.ts
var DenoTransport = class {
  async fetch(url, method, headers, body) {
    try {
      const res = await httpFetch(
        url,
        method,
        JSON.stringify(headers),
        body
      );
      if (typeof res === "string") {
        return { status: 200, headers: {}, body: res };
      }
      return {
        status: res.status,
        headers: {},
        body: res.body || ""
      };
    } catch (err) {
      const errMsg = err instanceof Error ? err.message : String(err);
      const statusMatch = errMsg.match(/-> (\d+):/);
      const status = statusMatch ? parseInt(statusMatch[1], 10) : 0;
      const bodyIdx = statusMatch ? errMsg.indexOf(": ", errMsg.indexOf("-> ")) + 2 : -1;
      const responseBody = bodyIdx > 1 ? errMsg.substring(bodyIdx) : errMsg;
      if (status === 0) {
        console.error(`[transport] httpFetch network error: ${errMsg}`);
      }
      return { status, headers: {}, body: responseBody };
    }
  }
};
var DenoStorageAdapter = class {
  get(key) {
    return storageGet(key);
  }
  put(key, value) {
    storagePut(key, value);
  }
  delete(key) {
    storageDelete(key);
  }
  listKeys(prefix) {
    return storageListKeys(prefix);
  }
};
var DenoAgentAdapter = class {
  did() {
    return agentDid();
  }
  signStringHex(payload) {
    return agentSignStringHex(payload);
  }
};
var DenoRuntimeAdapter = class {
  emitPerspectiveDiff(diff) {
    emitPerspectiveDiff(diff);
  }
  emitSyncStateChange(state) {
    emitSyncStateChange(state);
  }
  emitTelepresenceSignal(payload, recipientDid) {
    emitTelepresenceSignal(payload, recipientDid);
  }
};
var DenoWebSocketFactory = class {
  connect(url) {
    const ws = new WebSocket(url);
    return {
      send(data) {
        ws.send(data);
      },
      close(code, reason) {
        try {
          ws.close(code, reason);
        } catch {
        }
      },
      onOpen(cb) {
        ws.addEventListener("open", () => cb());
      },
      onMessage(cb) {
        ws.addEventListener("message", (event) => {
          cb(typeof event.data === "string" ? event.data : String(event.data));
        });
      },
      onClose(cb) {
        ws.addEventListener("close", (event) => cb(event.code, event.reason));
      },
      onError(cb) {
        ws.addEventListener("error", (event) => cb(event));
      }
    };
  }
};

// index.ts
//!@ad4m-template-variable
var SERVER_URL = "<to-be-filled>";
//!@ad4m-template-variable
var ROOM_ID = "<to-be-filled>";
//!@ad4m-template-variable
var UID = "<to-be-filled>";
var myDid = "";
var configured = false;
var localAgents = [];
var wsClient = null;
var keyRing = null;
var keyRingStatus = "none";
var isRoomAdmin = false;
var lastKeyRingRetry = 0;
var KEY_RING_RETRY_COOLDOWN_MS = 1e4;
var keyRingInflight = null;
var lifecycleGen = 0;
function isPlaceholder(value) {
  return !value || value === "<to-be-filled>";
}
function setupKeyRingCoalesced() {
  if (!keyRingInflight) {
    const p = setupKeyRing().finally(() => {
      if (keyRingInflight === p)
        keyRingInflight = null;
    });
    keyRingInflight = p;
  }
  return keyRingInflight;
}
async function setupKeyRing() {
  const gen = lifecycleGen;
  const config = getConfig();
  try {
    const token = await getValidToken();
    const keysRes = await fetchRoomKeys(config, token);
    if (gen !== lifecycleGen)
      return;
    if (!keysRes) {
      keyRingStatus = "none";
      keyRing = null;
      return;
    }
    if (keysRes.keys.length === 0 && keysRes.e2e_enabled) {
      keyRingStatus = "pending";
      keyRing = null;
      console.log(
        "[server-link-language] room has E2E enabled but this agent has no keys yet \u2014 commits blocked until the room admin grants keys"
      );
      return;
    }
    if (keysRes.keys.length === 0) {
      keyRingStatus = "none";
      keyRing = null;
      return;
    }
    const { privateKey } = deriveX25519KeyPair((payload) => getAgent().signStringHex(payload));
    keyRing = buildKeyRing(keysRes.keys, privateKey);
    keyRingStatus = "ready";
    const versions = [...keyRing.keys()].sort((a, b) => a - b);
    console.log(`[server-link-language] E2E key ring acquired (${versions.length} version(s): ${versions.join(", ")})`);
  } catch (err) {
    if (gen !== lifecycleGen)
      return;
    keyRingStatus = "error";
    keyRing = null;
    console.error(
      "[server-link-language] failed to acquire/decrypt E2E key ring \u2014 refusing to commit plaintext until this resolves:",
      err
    );
  }
}
async function refreshKeyRingIfNeeded() {
  const now = Date.now();
  if (now - lastKeyRingRetry < KEY_RING_RETRY_COOLDOWN_MS) {
    return false;
  }
  lastKeyRingRetry = now;
  const prevSize = keyRing?.size ?? 0;
  await setupKeyRingCoalesced();
  const newSize = keyRing?.size ?? 0;
  if (newSize > prevSize) {
    console.log("[server-link-language] key ring refreshed \u2014 re-bootstrapping");
    await bootstrap();
    const recovered = render();
    if (recovered.links.length > 0) {
      getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
    }
    return true;
  }
  return false;
}
async function performAdminKeyGrants() {
  if (!isRoomAdmin || !keyRing || keyRing.size === 0)
    return;
  const config = getConfig();
  try {
    const token = await getValidToken();
    const missingRes = await fetchMissingKeys(config, token);
    const members = missingRes.membersNeedingHistoricalKeys;
    if (members.length === 0)
      return;
    for (const member of members) {
      if (member.x25519Signature) {
        if (!verifyX25519Ownership(member.did, member.x25519PublicKey, member.x25519Signature)) {
          console.error(
            `[server-link-language] X25519 signature verification failed for ${member.did} \u2014 skipping key grant`
          );
          continue;
        }
      }
      const recipientPub = hexToBytes2(member.x25519PublicKey);
      const sealedKeys = [];
      for (const version2 of member.missingVersions) {
        const roomKey = keyRing.get(version2);
        if (!roomKey)
          continue;
        sealedKeys.push({
          version: version2,
          encryptedKey: sealRoomKeyForRecipient(roomKey, recipientPub)
        });
      }
      if (sealedKeys.length === 0)
        continue;
      const granted = await grantKeys(config, token, member.did, sealedKeys);
      if (granted.length > 0) {
        console.log(
          `[server-link-language] admin auto-granted key versions [${granted.join(", ")}] to ${member.did}`
        );
      }
    }
  } catch (err) {
    console.error("[server-link-language] admin auto-grant failed (non-fatal):", err);
  }
}
async function performRotation() {
  if (!isRoomAdmin)
    return;
  const config = getConfig();
  try {
    const token = await getValidToken();
    const aclRes = await fetchAclInfo(config, token);
    const roomKey = generateRoomKey();
    const sealedKeys = [];
    for (const member of aclRes.members) {
      if (!member.x25519PublicKey)
        continue;
      if (member.x25519Signature) {
        if (!verifyX25519Ownership(member.did, member.x25519PublicKey, member.x25519Signature)) {
          console.error(
            `[server-link-language] X25519 signature verification failed for ${member.did} \u2014 skipping`
          );
          continue;
        }
      }
      const recipientPub = hexToBytes2(member.x25519PublicKey);
      sealedKeys.push({
        did: member.did,
        encryptedKey: sealRoomKeyForRecipient(roomKey, recipientPub)
      });
    }
    if (sealedKeys.length === 0) {
      console.log("[server-link-language] no members with X25519 keys \u2014 skipping rotation");
      return;
    }
    const result = await rotateKeys(config, token, sealedKeys);
    console.log(
      `[server-link-language] client-side key rotation complete: version ${result.version}, ${result.recipients.length} recipient(s)`
    );
    await setupKeyRing();
  } catch (err) {
    console.error("[server-link-language] key rotation failed:", err);
    throw err;
  }
}
var language = defineLanguage({
  name: "server-link-language",
  version: "0.1.0",
  isPublic: true,
  async init() {
    initAdapters({
      storage: new DenoStorageAdapter(),
      transport: new DenoTransport(),
      agent: new DenoAgentAdapter(),
      runtime: new DenoRuntimeAdapter(),
      wsFactory: new DenoWebSocketFactory()
    });
    myDid = getAgent().did();
    initStore(hash);
    configured = !isPlaceholder(SERVER_URL) && !isPlaceholder(ROOM_ID) && !isPlaceholder(UID);
    if (!configured) {
      console.log(
        `[server-link-language] init: did=${myDid}, template variables not filled in \u2014 running inert until published with SERVER_URL/ROOM_ID/UID.`
      );
      return;
    }
    initAdapters({ config: { serverUrl: SERVER_URL, roomId: UID } });
    const config = getConfig();
    initSync({
      config,
      getToken: () => getValidToken(),
      emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
      emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
      getKeyRing: () => keyRing,
      refreshKeyRing: async () => {
        const now = Date.now();
        if (now - lastKeyRingRetry < KEY_RING_RETRY_COOLDOWN_MS) {
          return null;
        }
        lastKeyRingRetry = now;
        const prevSize = keyRing?.size ?? 0;
        await setupKeyRingCoalesced();
        return (keyRing?.size ?? 0) > prevSize;
      },
      // Periodic admin key grants — fallback for when the WS
      // `peer-joined` event never fires (WS down, CI, firewalls).
      // Each successful HTTP sync triggers a check so the admin
      // discovers new members who need historical keys.
      onPostSync: () => performAdminKeyGrants()
    });
    wsClient = new WsClient({
      getUrl: async () => wsUrl(config),
      getToken: () => getValidToken(),
      handlers: {
        onDiff(msg) {
          const result = applyInboundWireDiff(msg.payload, msg.sequence, msg.revision);
          if (result.missingVersions.size > 0) {
            trackMissingKeyVersions(result.missingVersions);
            void refreshKeyRingIfNeeded().catch((err) => {
              console.error("[server-link-language] WS diff key ring refresh failed:", err);
            });
          }
        },
        onTelepresenceSignal(msg) {
          getRuntime().emitTelepresenceSignal(msg.payload, myDid);
        },
        onTelepresenceBroadcast(msg) {
          getRuntime().emitTelepresenceSignal(msg.payload);
        },
        onOnlineAgents(msg) {
          handleOnlineAgentsMessage(msg);
        },
        onPeerJoined(msg) {
          handlePeerJoined(msg);
          void performAdminKeyGrants().catch((err) => {
            console.error("[server-link-language] peer-joined admin grant failed:", err);
          });
        },
        onPeerLeft(msg) {
          handlePeerLeft(msg);
        },
        onStatusChanged(msg) {
          handleStatusChanged(msg);
        },
        onOpen() {
          getRuntime().emitSyncStateChange("Synced");
          void catchUp().catch((err) => {
            console.error("[server-link-language] post-connect catch-up failed:", err);
          });
        },
        onClose() {
          clearOnlineAgents();
          getRuntime().emitSyncStateChange("LinkLanguageInstalledButNotSynced");
        },
        onReconnecting(attempt, delayMs) {
          console.log(
            `[server-link-language] websocket reconnecting (attempt ${attempt}) in ${Math.round(delayMs)}ms`
          );
        }
      }
    });
    initTelepresence({
      send: (msg) => wsClient.send(msg),
      getMyDid: () => myDid
    });
    try {
      await authenticate();
      try {
        const aclInfo = await fetchAclInfo(config, await getValidToken());
        isRoomAdmin = aclInfo.admin === myDid;
      } catch {
        isRoomAdmin = false;
      }
      await setupKeyRing();
      if (isRoomAdmin && keyRingStatus === "none") {
        console.log(
          "[server-link-language] admin, no E2E yet \u2014 generating initial room key"
        );
        await performRotation();
      }
      await bootstrap();
      if (keyRingStatus === "pending") {
        await setupKeyRing();
        if (keyRingStatus === "ready") {
          console.log(
            "[server-link-language] keys acquired after init grant \u2014 re-bootstrapping"
          );
          await bootstrap();
        }
      }
      void performAdminKeyGrants().catch((err) => {
        console.error("[server-link-language] initial admin grant check failed:", err);
      });
    } catch (err) {
      console.error(
        "[server-link-language] initial startup sequence failed \u2014 will keep retrying via websocket reconnect backoff and the runtime's periodic sync():",
        err
      );
    }
    void wsClient.connect().catch((err) => {
      console.error("[server-link-language] initial websocket connect failed:", err);
    });
    console.log(`[server-link-language] init complete: did=${myDid}, room=${ROOM_ID}, uid=${UID}`);
  },
  async teardown() {
    lifecycleGen++;
    try {
      await drainCommitBatch();
    } catch (err) {
      console.error(
        "[server-link-language] teardown: draining batched commits failed; some writes may not have reached the server:",
        err
      );
    }
    if (wsClient) {
      wsClient.close();
      wsClient = null;
    }
    myDid = "";
    configured = false;
    localAgents = [];
    keyRing = null;
    keyRingStatus = "none";
    isRoomAdmin = false;
    lastKeyRingRetry = 0;
    keyRingInflight = null;
    resetAuth();
    resetAdapters();
    console.log("[server-link-language] teardown");
  },
  interactions() {
    return [];
  },
  // -----------------------------------------------------------------------
  // perspective-commit
  // -----------------------------------------------------------------------
  commit: {
    async commit(diff) {
      if (!configured) {
        throw new Error(
          "server-link-language: not configured (SERVER_URL/ROOM_ID/UID template variables unfilled)"
        );
      }
      if (keyRingStatus === "error" || keyRingStatus === "pending") {
        const prevStatus = keyRingStatus;
        console.log(
          `[server-link-language] retrying E2E key ring acquisition before commit (status: ${keyRingStatus})...`
        );
        await setupKeyRingCoalesced();
        if (prevStatus !== "ready" && keyRingStatus === "ready") {
          console.log(
            "[server-link-language] key ring acquired \u2014 re-bootstrapping to recover skipped links"
          );
          await bootstrap();
          const recovered = render();
          if (recovered.links.length > 0) {
            getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
          }
        }
      }
      if (keyRingStatus === "error") {
        throw new Error(
          "server-link-language: refusing to commit \u2014 this room's E2E key ring could not be acquired/decrypted, and sending plaintext to a possibly-encrypted room would be unsafe. Retry once connectivity/auth recovers."
        );
      }
      if (keyRingStatus === "pending") {
        throw new Error(
          "server-link-language: refusing to commit \u2014 this room has E2E encryption enabled but this agent has not received room keys yet. The room admin must grant keys before this agent can write."
        );
      }
      if (isRoomAdmin && keyRingStatus === "none") {
        console.log(
          "[server-link-language] admin with keyRingStatus=none \u2014 retrying rotation before commit"
        );
        await performRotation();
      }
      console.log(
        `[server-link-language] commit: ${diff.additions.length} adds, ${diff.removals.length} removes (keyRingStatus=${keyRingStatus})`
      );
      applyDiff(diff);
      await commit(diff);
      getRuntime().emitPerspectiveDiff(diff);
      return "";
    }
  },
  // -----------------------------------------------------------------------
  // perspective-sync
  // -----------------------------------------------------------------------
  sync: {
    async sync() {
      if (!configured)
        return { additions: [], removals: [] };
      return performSync();
    },
    async render() {
      return render();
    },
    async currentRevision() {
      return currentRevision();
    }
  },
  // -----------------------------------------------------------------------
  // perspective-query
  // -----------------------------------------------------------------------
  query: {
    supportedKinds() {
      return ["link-pattern"];
    },
    async run(req) {
      if (req.kind !== "link-pattern") {
        return { kind: "error", payload: `Unsupported query kind: ${req.kind}` };
      }
      const pattern = req.payload ?? {};
      const links = queryLinks(pattern);
      return { kind: "links", payload: links };
    }
  },
  // -----------------------------------------------------------------------
  // peers
  // -----------------------------------------------------------------------
  peers: {
    async setLocal(agents) {
      localAgents = agents;
    },
    async remote() {
      if (!configured)
        return [];
      const config = getConfig();
      const token = await getValidToken();
      const allPeers = await fetchPeers(config, token);
      return allPeers.filter((did) => did !== myDid && !localAgents.includes(did));
    }
  },
  // -----------------------------------------------------------------------
  // telepresence
  // -----------------------------------------------------------------------
  telepresence: {
    async setOnlineStatus(status) {
      return setOnlineStatus(status);
    },
    async getOnlineAgents() {
      return getOnlineAgents();
    },
    async sendSignal(remoteAgentDid, payload) {
      return sendSignal(remoteAgentDid, payload);
    },
    async sendBroadcast(payload) {
      return sendBroadcast(payload);
    }
  }
});
var {
  name,
  version,
  isPublic,
  init,
  teardown,
  interactions,
  perspectiveCommit,
  perspectiveSyncSync,
  perspectiveSyncRender,
  perspectiveSyncCurrentRevision,
  perspectiveQuerySupportedKinds,
  perspectiveQueryRun,
  peersSetLocal,
  peersRemote,
  telepresenceSetOnlineStatus,
  telepresenceGetOnlineAgents,
  telepresenceSendSignal,
  telepresenceSendBroadcast
} = language;
var server_link_language_default = language;
var possibleTemplateParams = ["ROOM_ID", "SERVER_URL", "UID"];
export {
  server_link_language_default as default,
  init,
  interactions,
  isPublic,
  name,
  peersRemote,
  peersSetLocal,
  perspectiveCommit,
  perspectiveQueryRun,
  perspectiveQuerySupportedKinds,
  perspectiveSyncCurrentRevision,
  perspectiveSyncRender,
  perspectiveSyncSync,
  possibleTemplateParams,
  teardown,
  telepresenceGetOnlineAgents,
  telepresenceSendBroadcast,
  telepresenceSendSignal,
  telepresenceSetOnlineStatus,
  version
};
