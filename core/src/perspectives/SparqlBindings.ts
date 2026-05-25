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
 * Mirrors Flux's local `parseLit` helper. Returns `''` for `undefined`/empty
 * input. Falls through to the raw value if decoding fails (defensive — the
 * binding may already be a plain string for unwrapped properties).
 */
export function parseLit(val: string | undefined | null): string {
  if (val === undefined || val === null || val === '') return '';
  try {
    const result = Literal.fromUrl(val).get();
    if (result && typeof result === 'object') {
      return (result as { data?: string }).data ?? JSON.stringify(result);
    }
    return String(result);
  } catch {
    return val;
  }
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
