/**
 * Shared instance hydration utilities for Ad4mModel.
 *
 * Both `getData()` (single-instance path) and `instancesFromSurrealResult()`
 * (bulk path) delegate to `hydrateInstanceFromLinks()` and
 * `evaluateCustomGetters()` here, guaranteeing identical semantics.
 *
 * Previously the two implementations diverged — most notably, `getData()` used
 * "latest-wins" for properties while `instancesFromSurrealResult()` used
 * "first-wins". Both now use latest-wins (last ASC-ordered link per predicate).
 */

import { Literal } from "../../Literal";
import { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import type { ModelMetadata } from "../types";
import { formatSurrealValue } from "./surrealCompiler";

// ─────────────────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────────────────

/** Raw link row as returned by SurrealDB queries. */
export interface RawLink {
  predicate: string;
  target: string;
  author?: string;
  timestamp?: string | number;
}

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Normalise a SurrealDB timestamp to epoch-milliseconds.
 *
 * - ISO strings (`"2024-01-01T00:00:00Z"`) → `Date.getTime()`
 * - Numeric strings (`"1700000000000"`) → `parseInt`
 * - Numbers → returned as-is
 * - Anything else → returned as-is (no data loss)
 */
export function normalizeTimestamp(ts: any): number | string {
  if (typeof ts === "number") return ts;
  if (typeof ts === "string") {
    if (ts.includes("T")) {
      const ms = new Date(ts).getTime();
      return isNaN(ms) ? ts : ms;
    }
    const parsed = parseInt(ts, 10);
    return isNaN(parsed) ? ts : parsed;
  }
  return ts;
}

/**
 * Resolve a raw SurrealDB link target to a typed JavaScript value.
 *
 * Resolution order:
 * 1. Non-literal `resolveLanguage` → `perspective.getExpression(target)`
 * 2. `literal://` URL → `Literal.fromUrl(target).get().data`
 * 3. Otherwise → raw string unchanged
 */
async function resolveValue(
  raw: string,
  resolveLanguage: string | undefined,
  perspective: PerspectiveProxy,
  propName: string,
): Promise<any> {
  // Non-literal language: fetch the expression via the perspective
  if (
    resolveLanguage &&
    resolveLanguage !== "literal" &&
    typeof raw === "string" &&
    !raw.startsWith("literal://")
  ) {
    try {
      const expression = await perspective.getExpression(raw);
      if (expression) {
        try {
          return JSON.parse(expression.data);
        } catch {
          return expression.data;
        }
      }
    } catch (e) {
      console.warn(`Failed to resolve expression for ${propName}:`, e);
    }
    return raw;
  }

  // Literal URL: parse inline — only when resolveLanguage is 'literal' or unset.
  // A property with a non-literal resolveLanguage whose stored target happens to
  // start with 'literal://' (e.g. a model baseExpression URI stored as a string
  // property value) must NOT be unwrapped here, or the URI itself is destroyed.
  // Note: unlike the old monolithic Ad4mModel.ts, ALL scalar values in our
  // mutation layer are stored as literal:// (when they have no URI scheme), so
  // resolveLanguage === undefined is the normal case for plain string/number props.
  if (
    (resolveLanguage === "literal" || resolveLanguage === undefined) &&
    typeof raw === "string" &&
    raw.startsWith("literal://")
  ) {
    try {
      const parsed = Literal.fromUrl(raw).get();
      return parsed.data !== undefined ? parsed.data : parsed;
    } catch {
      // fall through to raw
    }
  }

  return raw;
}

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Hydrates a model instance from a flat array of raw SurrealDB link rows.
 *
 * **Guarantees:**
 * - Properties use *"latest-wins"* semantics — links must be ordered
 *   `ASC` by `timestamp`; the last matching link per predicate wins.
 * - Forward relations preserve insertion order (links already ASC).
 * - `createdAt` / `updatedAt` / `author` are derived from the global
 *   min/max timestamps across **all** links (not just property links).
 *
 * **Does NOT handle** (left to the caller):
 * - Reverse relations — require a separate `WHERE out.uri = $base` query.
 * - Custom getter evaluation — call `evaluateCustomGetters()` afterwards.
 * - `relatedModel` eager hydration — do that in a batch pass afterwards.
 */
export async function hydrateInstanceFromLinks(
  instance: any,
  links: RawLink[],
  metadata: ModelMetadata,
  perspective: PerspectiveProxy,
): Promise<void> {
  if (!links || links.length === 0) return;

  // ── Global timestamp / author tracking ─────────────────────────────────────
  // Normalise to epoch-ms so numeric and ISO timestamps compare correctly.
  let minTimestamp: number | null = null;
  let maxTimestamp: number | null = null;
  let originalAuthor: string | null = null;

  for (const link of links) {
    const ts = link.timestamp;
    if (ts == null) continue;
    const t = Number(normalizeTimestamp(ts));
    if (isNaN(t)) continue;
    if (minTimestamp === null || t < minTimestamp) {
      minTimestamp = t;
      originalAuthor = link.author ?? null;
    }
    if (maxTimestamp === null || t > maxTimestamp) {
      maxTimestamp = t;
    }
  }

  // ── Properties ──────────────────────────────────────────────────────────────
  // Links are ordered ASC by timestamp; the LAST match is the most recent value.
  for (const [propName, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.getter) continue; // handled separately by evaluateCustomGetters

    const matching = links.filter(
      (l) => l.predicate === propMeta.predicate && l.target !== "None",
    );
    if (matching.length === 0) continue;

    const link = matching[matching.length - 1]; // latest wins
    let value: any = await resolveValue(
      link.target,
      propMeta.resolveLanguage,
      perspective,
      propName,
    );

    if (propMeta.transform && typeof propMeta.transform === "function") {
      value = propMeta.transform(value);
    }

    instance[propName] = value;
  }

  // ── Forward relations ───────────────────────────────────────────────────────
  // Collect targets in their natural (ASC timestamp) order; filter None/empty.
  const forwardRelations = Object.entries(metadata.relations).filter(
    ([, m]) => !m.getter && m.direction !== "reverse",
  );
  for (const [relationName, relMeta] of forwardRelations) {
    const matching = links.filter((l) => l.predicate === relMeta.predicate);
    const values = matching
      .map((l) => l.target)
      .filter((v) => v !== undefined && v !== null && v !== "" && v !== "None");

    // maxCount === 1: take first value (oldest) — "@HasOne" has only one link
    // in the happy path; "first" vs "last" only differs in error/corrupt state.
    instance[relationName] =
      relMeta.maxCount === 1 ? (values[0] ?? null) : values;
  }

  // ── Author & timestamps ──────────────────────────────────────────────────────
  if (originalAuthor) instance.author = originalAuthor;
  if (minTimestamp !== null)
    instance.createdAt = normalizeTimestamp(minTimestamp);
  if (maxTimestamp !== null)
    instance.updatedAt = normalizeTimestamp(maxTimestamp);
}

/**
 * Evaluates custom SurrealQL getter expressions for all `getter`-decorated
 * properties and relations on a single model instance.
 *
 * Called by both `getData()` (single-instance path) and
 * `instancesFromSurrealResult()` (bulk path) — single implementation so
 * neither path can diverge.
 */
export async function evaluateCustomGetters(
  instance: any,
  perspective: PerspectiveProxy,
  metadata: ModelMetadata,
): Promise<void> {
  const safeBase = formatSurrealValue(instance.id);

  // Property getters
  for (const [propName, propMeta] of Object.entries(metadata.properties)) {
    if (!propMeta.getter) continue;
    try {
      const query = propMeta.getter.replace(/Base/g, safeBase);
      const result = await perspective.querySurrealDB(
        `SELECT (${query}) AS value FROM node WHERE uri = ${safeBase}`,
      );
      if (
        result?.length > 0 &&
        result[0].value !== undefined &&
        result[0].value !== null &&
        result[0].value !== "None" &&
        result[0].value !== ""
      ) {
        instance[propName] = result[0].value;
      }
    } catch (error) {
      console.warn(`Failed to evaluate getter for ${propName}:`, error);
    }
  }

  // Relation getters
  for (const [relationName, relMeta] of Object.entries(metadata.relations)) {
    if (!relMeta.getter) continue;
    try {
      const query = relMeta.getter.replace(/Base/g, safeBase);
      const result = await perspective.querySurrealDB(
        `SELECT (${query}) AS value FROM node WHERE uri = ${safeBase}`,
      );
      if (
        result?.length > 0 &&
        result[0].value !== undefined &&
        result[0].value !== null
      ) {
        const value = result[0].value;
        instance[relationName] = Array.isArray(value)
          ? value.filter(
              (v: any) =>
                v !== undefined && v !== null && v !== "" && v !== "None",
            )
          : value;
      }
    } catch (error) {
      console.warn(`Failed to evaluate getter for ${relationName}:`, error);
    }
  }
}
