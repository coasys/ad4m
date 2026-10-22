// Runs once in a fresh tool-bundle isolate, before the bundle. It installs the few globals a bundle
// may use beyond the language itself, removes what deno_core leaves on globalThis, and evaluates to
// the function Rust calls the bundle through. Pure ASCII: it is passed to V8 as a script.
//
// What a bundle can reach is exactly what is defined here. The isolate has no extension that
// touches files, the network, the agent or any service, so there is nothing else to remove.
(() => {
  "use strict";

  const core = globalThis.Deno.core;
  const ops = core.ops;
  const encodeUtf8 = core.encode;
  const decodeUtf8 = core.decode;
  const clone = core.structuredClone;
  const randomValues = ops.op_tool_bundle_random_values;
  const log = ops.op_tool_bundle_log;
  const stringify = JSON.stringify;
  const parse = JSON.parse;
  const resolved = Promise.resolve.bind(Promise);

  const define = (name, value) =>
    Object.defineProperty(globalThis, name, {
      value,
      writable: true,
      enumerable: false,
      configurable: true,
    });

  const bytesOf = (input) => {
    if (input === undefined) return new Uint8Array(0);
    if (ArrayBuffer.isView(input)) {
      return new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
    }
    if (input instanceof ArrayBuffer) return new Uint8Array(input);
    throw new TypeError("Expected an ArrayBuffer or a view of one");
  };

  class TextEncoder {
    get encoding() {
      return "utf-8";
    }
    encode(input = "") {
      // deno_core's encode answers with memory allocated in Rust, which the memory cap does not
      // count; what the bundle keeps is a copy in memory the cap does.
      const encoded = encodeUtf8(String(input));
      const bytes = new Uint8Array(encoded.length);
      bytes.set(encoded);
      return bytes;
    }
  }

  class TextDecoder {
    constructor(label = "utf-8") {
      if (!/^utf-?8$/i.test(String(label).trim())) {
        throw new RangeError("Only utf-8 is supported here: " + label);
      }
    }
    get encoding() {
      return "utf-8";
    }
    decode(input) {
      return decodeUtf8(bytesOf(input));
    }
  }

  const BASE64 =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  const invalid = (message) => {
    const error = new Error(message);
    error.name = "InvalidCharacterError";
    return error;
  };

  // Each character of a binary string is one byte, as the web's btoa takes it.
  const btoa = (data) => {
    const text = String(data);
    let out = "";
    for (let i = 0; i < text.length; i += 3) {
      const a = text.charCodeAt(i);
      const b = i + 1 < text.length ? text.charCodeAt(i + 1) : -1;
      const c = i + 2 < text.length ? text.charCodeAt(i + 2) : -1;
      if (a > 255 || b > 255 || c > 255) {
        throw invalid("btoa: the string has characters outside Latin-1");
      }
      out += BASE64[a >> 2];
      out += BASE64[((a & 3) << 4) | (b < 0 ? 0 : b >> 4)];
      out += b < 0 ? "=" : BASE64[((b & 15) << 2) | (c < 0 ? 0 : c >> 6)];
      out += c < 0 ? "=" : BASE64[c & 63];
    }
    return out;
  };

  const atob = (data) => {
    let text = String(data).replace(/[\t\n\f\r ]/g, "");
    if (text.length % 4 === 0) text = text.replace(/==?$/, "");
    if (text.length % 4 === 1 || /[^A-Za-z0-9+/]/.test(text)) {
      throw invalid("atob: the string is not valid base64");
    }
    let out = "";
    let buffer = 0;
    let bits = 0;
    for (let i = 0; i < text.length; i++) {
      buffer = (buffer << 6) | BASE64.indexOf(text[i]);
      bits += 6;
      if (bits >= 8) {
        bits -= 8;
        out += String.fromCharCode((buffer >> bits) & 255);
      }
    }
    return out;
  };

  const crypto = Object.freeze({
    getRandomValues(array) {
      if (
        !ArrayBuffer.isView(array) ||
        array instanceof Float32Array ||
        array instanceof Float64Array ||
        array instanceof DataView
      ) {
        throw new TypeError("getRandomValues takes an integer typed array");
      }
      if (array.byteLength > 65536) {
        const error = new Error("getRandomValues: at most 65536 bytes at a time");
        error.name = "QuotaExceededError";
        throw error;
      }
      randomValues(bytesOf(array));
      return array;
    },
  });

  const show = (value) => {
    if (typeof value === "string") return value;
    if (value instanceof Error) return value.stack || String(value);
    try {
      const text = stringify(value);
      return text === undefined ? String(value) : text;
    } catch {
      return String(value);
    }
  };
  const write = (level) => (...args) => log(level, args.map(show).join(" "));
  const console = Object.freeze({
    debug: write(0),
    log: write(1),
    info: write(1),
    warn: write(2),
    error: write(3),
  });

  // A resizable ArrayBuffer or growable SharedArrayBuffer is reserved by V8 itself, outside the
  // allocator the isolate's memory cap counts. So the constructors a bundle can reach make only
  // fixed-length buffers: each replaces V8's on the global and as its prototype's constructor,
  // which with species (a getter answering `this`) are the only ways to reach V8's.
  const fixedLength = (Original) => {
    const kind = Original.name;
    const construct = Reflect.construct;
    function Fixed(length, options) {
      if (new.target === undefined) {
        throw new TypeError("Constructor " + kind + " requires 'new'");
      }
      if (options !== undefined && options !== null && options.maxByteLength !== undefined) {
        throw new TypeError("A tool bundle cannot make a resizable " + kind);
      }
      return construct(Original, [length], new.target);
    }
    for (const key of Reflect.ownKeys(Original)) {
      if (key === "prototype") continue;
      Object.defineProperty(Fixed, key, Object.getOwnPropertyDescriptor(Original, key));
    }
    Object.defineProperty(Fixed, "prototype", {
      value: Original.prototype,
      writable: false,
      enumerable: false,
      configurable: false,
    });
    Object.defineProperty(Original.prototype, "constructor", {
      value: Fixed,
      writable: true,
      enumerable: false,
      configurable: true,
    });
    define(kind, Fixed);
  };
  fixedLength(ArrayBuffer);
  fixedLength(SharedArrayBuffer);

  define("TextEncoder", TextEncoder);
  define("TextDecoder", TextDecoder);
  define("atob", atob);
  define("btoa", btoa);
  define("structuredClone", (value) => clone(value));
  define("crypto", crypto);
  define("console", console);

  // deno_core's own: its ops, its bootstrap object, and queueMicrotask, which is harmless but
  // not on the list a bundle is tested against, so a bundle relying on it fails there first.
  delete globalThis.Deno;
  delete globalThis.__bootstrap;
  delete globalThis.queueMicrotask;
  // V8's own, but a WebAssembly.Memory is reserved outside the allocator the isolate's memory
  // cap counts, so a bundle could hold any amount of it. A bundle is plain JavaScript.
  delete globalThis.WebAssembly;

  // How Rust reaches the bundle: walk an export path, then read what is there (args absent) or
  // call it with the JSON-encoded arguments, with the object it was found on as `this`. Both
  // answer with JSON text, so nothing but strings crosses into Rust.
  return (namespace, path, argsJson) => {
    let owner;
    let value = namespace;
    for (const key of path) {
      if (value === null || value === undefined) {
        throw new TypeError("The bundle exports nothing at " + path.join("."));
      }
      owner = value;
      value = value[key];
    }
    if (argsJson === undefined) return stringify(value === undefined ? null : value);
    if (typeof value !== "function") {
      throw new TypeError(path.join(".") + " is not a function");
    }
    return resolved(value.apply(owner, parse(argsJson))).then((result) =>
      stringify(result === undefined ? null : result)
    );
  };
})();
