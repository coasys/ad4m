/**
 * SPARQL binding helpers — typed shapes and parsers for `querySparql<T>()`.
 *
 * AD4M stores non-string property values as URL-encoded `Literal` payloads
 * (see {@link Literal}). When raw SPARQL queries pull these values out of the
 * RDF store the bindings are still in encoded form; consumers must decode
 * before use. The helpers here centralise that decode step so each call site
 * can pick the variant matching its declared binding type.
 *
 * @example
 * ```ts
 * interface MessageBinding {
 *   id: string;
 *   timestamp: string;  // raw literal — decode with parseLitNumber
 *   body?: string;      // raw literal — decode with parseLit
 * }
 * const rows = await perspective.querySparql<MessageBinding[]>(query);
 * const ts = parseLitNumber(rows[0].timestamp);   // number
 * const body = rows[0].body ? parseLit(rows[0].body) : '';
 * ```
 */

import { Literal } from "../Literal";

/** Common AD4M link binding — matches the `?source/?predicate/?target/?author/?timestamp` pattern. */
export interface LinkBinding {
  source: string;
  predicate: string;
  target: string;
  author?: string;
  timestamp?: string;
}

/** COUNT(?x) result binding — SPARQL returns counts as string literals. */
export interface CountBinding {
  count: string;
}

/** Sum a SPARQL COUNT(?x) result down to a plain number (returns 0 on empty). */
export function parseSparqlCount(result: CountBinding[] | undefined | null): number {
  if (!result || result.length === 0) return 0;
  const raw = result[0].count;
  const n = parseInt(raw, 10);
  return Number.isNaN(n) ? 0 : n;
}

/**
 * Decode a `Literal`-encoded SPARQL binding back to a plain string.
 *
 * Returns `''` for `undefined`/empty input. Falls through to the raw value
 * if decoding fails — bindings against properties resolved through a custom
 * language are already raw URIs and should pass through unchanged.
 *
 * Signed-envelope literals
 * ------------------------
 * Properties declared with `resolveLanguage: 'literal'` are written through
 * the built-in `literal` language, which wraps the value in a signed
 * expression envelope (`{ author, timestamp, data, proof }`) and encodes
 * the whole envelope as a `literal:json:` URL — see
 * `rust-executor/src/languages/mod.rs::expression_create` (literal branch).
 * On read, `parseLit` unwraps `.data` from that envelope so callers get the
 * original scalar value instead of the envelope JSON.
 *
 * The Channel V refactor moved most scalar properties to deterministic
 * typed XSD literals which no longer round-trip through this envelope — for
 * those properties the SPARQL binding is already the lexical form and this
 * helper decodes to the primitive.  Signed-envelope literals still exist
 * for any property that opts back into `resolveLanguage: 'literal'` (e.g.
 * per-message provenance for chat bodies), which is why the `.data` unwrap
 * is preserved here.
 *
 * Non-envelope JSON objects (`literal:json:` payloads that are not signed
 * envelopes) are JSON-stringified for display.
 */
export function parseLit(val: string | undefined | null): string {
  if (val === undefined || val === null || val === '') return '';

  // Fast-path 1: `literal:*` URLs from properties written through the built-in
  // `literal` language.  On Channel V typed-XSD-literal properties, these
  // decode straight to a primitive; on `resolveLanguage: 'literal'` properties
  // (per-message provenance in chat/synergy) they decode to a signed-envelope
  // object we unwrap via `.data`.
  try {
    const result = Literal.fromUrl(val).get();
    if (typeof result === 'object' && result !== null) {
      const data = (result as { data?: unknown }).data;
      if (typeof data === 'string') return data;
      return JSON.stringify(result);
    }
    return String(result);
  } catch {
    // fall through to plain-JSON envelope detection
  }

  // Fast-path 2: envelope stored/returned as a bare JSON string rather than a
  // `literal:json:*` URL.  Depending on the storage backend and query path,
  // SPARQL bindings for `resolveLanguage: 'literal'` properties can surface
  // the envelope object directly as its JSON serialisation (Oxigraph typed
  // xsd:string of the JSON, or a Channel V pass-through) instead of the URL
  // form.  Detect that shape by parsing the string and unwrapping `.data`
  // when it looks like a signed envelope; anything else passes through
  // unchanged so plain text (e.g. "just a string") is not corrupted.
  if (val.length >= 2 && val.charCodeAt(0) === 0x7b /* '{' */) {
    try {
      const parsed = JSON.parse(val);
      if (parsed && typeof parsed === 'object') {
        const data = (parsed as { data?: unknown }).data;
        if (typeof data === 'string') return data;
      }
    } catch {
      // not JSON — fall through and return the raw value
    }
  }

  return val;
}

/** Decode a `Literal`-encoded binding to a number. Returns 0 on empty/invalid input. */
export function parseLitNumber(val: string | undefined | null): number {
  if (val === undefined || val === null || val === '') return 0;
  const decoded = parseLit(val);
  const n = Number(decoded);
  return Number.isNaN(n) ? 0 : n;
}

/** Decode a `Literal`-encoded binding to a boolean. Returns false on empty input. */
export function parseLitBoolean(val: string | undefined | null): boolean {
  if (val === undefined || val === null || val === '') return false;
  return parseLit(val) === 'true';
}
