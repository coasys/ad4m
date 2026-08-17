/**
 * Shared query compilation utilities.
 *
 * These functions are used by both the runtime query builder (`Ad4mModel`) and
 * the compile-time SHACL generator (`decorators.ts`).
 *
 * All output uses native SPARQL syntax.
 */

import type { WhereCondition, Where, ModelMetadata } from "./types";
import { escapeQueryString } from "../utils";

/**
 * Format a JavaScript value for embedding in a query expression.
 *
 * - Strings are single-quoted with backslash / quote / newline escaping
 * - Numbers and booleans are passed through as-is
 * - Arrays are recursed
 */
export function formatQueryValue(value: any): string {
    if (typeof value === 'string') {
        // Escape backslashes first, then single quotes and other special characters
        const escaped = value
            .replace(/\\/g, '\\\\')  // Backslash -> \\
            .replace(/'/g, "\\'")     // Single quote -> \'
            .replace(/"/g, '\\"')     // Double quote -> \"
            .replace(/\n/g, '\\n')    // Newline -> \n
            .replace(/\r/g, '\\r')    // Carriage return -> \r
            .replace(/\t/g, '\\t');   // Tab -> \t
        return `'${escaped}'`;
    } else if (typeof value === 'number' || typeof value === 'boolean') {
        return String(value);
    } else if (Array.isArray(value)) {
        return `[${value.map(v => formatQueryValue(v)).join(', ')}]`;
    } else {
        return String(value);
    }
}

/**
 * Compile a single Where condition for a property into a SPARQL triple pattern
 * that can be embedded in a getter's WHERE clause.
 *
 * The generated pattern operates on `?target` — e.g.:
 *   `?target <pred> <value> .`
 *   `FILTER EXISTS { ?target <pred> ?_v0 }`
 *
 * @param predicate  - The predicate URI for the property
 * @param condition  - The Where condition value
 * @param opts       - Optional settings. `isDeterministicLiteral` (default
 *                     `true`) selects the direct typed-literal WHERE path.
 *                     Set to `false` when the property stores a signed
 *                     envelope (`resolveLanguage: "literal"`) or a
 *                     custom-language expression — the envelope path unwraps
 *                     with `parse_literal` before comparing.
 * @returns A single SPARQL condition string
 */
export function buildWhereCondition(
    predicate: string,
    condition: WhereCondition,
    opts?: { isDeterministicLiteral?: boolean; varIndex?: number },
): string {
    const escapedPredicate = escapeQueryString(predicate);
    // For deterministic-literal properties, values are stored as typed
    // literals like `"X"^^xsd:string` (or the legacy `<literal:string:VALUE>`
    // IRI form). For envelope / custom-language properties the target is a
    // signed expression URI; comparisons need to unwrap via `parse_literal`.
    const isLiteral = opts?.isDeterministicLiteral !== false;

    const varSuffix = opts?.varIndex ?? 0;

    function formatValue(v: any): string {
        if (typeof v === 'string') {
            return isLiteral
                ? `<literal:string:${escapeQueryString(v)}>`
                : `<${escapeQueryString(v)}>`;
        }
        // Numbers/booleans — also stored as literal:string: in AD4M
        return `<literal:string:${v}>`;
    }

    /**
     * For literal properties, generate a FILTER-based match that handles both
     * the raw value and the literal-encoded value. This mirrors the Rust
     * `build_projection_where_patterns` approach which uses:
     *   FILTER(STR(?var) = "value" || STR(?var) = "literal:string:encodedValue")
     * This is more robust than exact IRI matching because it handles edge cases
     * where the stored format might vary.
     */
    function formatLiteralFilter(varName: string, v: any): string {
        const raw = escapeQueryString(String(v));
        const encoded = encodeURIComponent(String(v))
            .replace(/!/g, '%21')
            .replace(/'/g, '%27')
            .replace(/\(/g, '%28')
            .replace(/\)/g, '%29')
            .replace(/\*/g, '%2A');
        if (typeof v === 'string') {
            return `FILTER(STR(${varName}) = "${raw}" || STR(${varName}) = "literal:string:${encoded}")`;
        } else if (typeof v === 'number') {
            return `FILTER(STR(${varName}) = "${v}" || STR(${varName}) = "literal:number:${v}")`;
        } else if (typeof v === 'boolean') {
            return `FILTER(STR(${varName}) = "${v}" || STR(${varName}) = "literal:boolean:${v}")`;
        }
        return `FILTER(STR(${varName}) = "${raw}")`;
    }

    if (Array.isArray(condition)) {
        // Array values → FILTER IN
        if (isLiteral) {
            const varName = `?_wc${varSuffix}`;
            const inValues = (condition as any[]).flatMap(v => {
                const raw = escapeQueryString(String(v));
                const encoded = encodeURIComponent(String(v))
                    .replace(/!/g, '%21').replace(/'/g, '%27')
                    .replace(/\(/g, '%28').replace(/\)/g, '%29').replace(/\*/g, '%2A');
                return [`"${raw}"`, `"literal:string:${encoded}"`];
            }).join(', ');
            return `?target <${escapedPredicate}> ${varName} . FILTER(STR(${varName}) IN (${inValues}))`;
        }
        const formattedValues = (condition as any[]).map(formatValue).join(', ');
        return `FILTER EXISTS { ?target <${escapedPredicate}> ?_val . FILTER(?_val IN (${formattedValues})) }`;
    } else if (typeof condition === 'object' && condition !== null) {
        // Operator object
        const ops = condition as any;
        const parts: string[] = [];

        if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
                const formattedValues = ops.not.map((v: any) => formatValue(v)).join(', ');
                // FILTER NOT EXISTS here is scoped to a specific predicate, not a global scan,
                // so O(N²) behavior is bounded by the number of values for this property.
                parts.push(
                    `FILTER NOT EXISTS { ?target <${escapedPredicate}> ?_nval . FILTER(?_nval IN (${formattedValues})) }`,
                );
            } else {
                // FILTER NOT EXISTS here is scoped to a specific predicate, not a global scan,
                // so O(N²) behavior is bounded by the number of values for this property.
                parts.push(
                    `FILTER NOT EXISTS { ?target <${escapedPredicate}> ${formatValue(ops.not)} }`,
                );
            }
        }

        // Comparison operators — ensure property exists
        const hasComparisonOps =
            ops.gt !== undefined || ops.gte !== undefined ||
            ops.lt !== undefined || ops.lte !== undefined ||
            ops.between !== undefined || ops.contains !== undefined;
        if (hasComparisonOps) {
            parts.push(
                `FILTER EXISTS { ?target <${escapedPredicate}> ?_cmp }`,
            );
        }

        return parts.join(' ');
    } else {
        // Simple equality
        if (isLiteral) {
            // Use FILTER matching (mirrors Rust build_projection_where_patterns)
            // to handle both raw values and literal-encoded IRIs robustly.
            const varName = `?_wc${varSuffix}`;
            return `?target <${escapedPredicate}> ${varName} . ${formatLiteralFilter(varName, condition)}`;
        }
        return `?target <${escapedPredicate}> ${formatValue(condition)} .`;
    }
}

/**
 * Compile a full `Where` clause to an array of SPARQL condition strings.
 *
 * When `metadata` is provided, property names are resolved to predicates
 * using the model's property metadata.  When `metadata` is `null`, property
 * names are treated as raw predicate URIs.
 *
 * Only handles **regular properties** — special fields (`id`, `author`,
 * `timestamp`) and relation fields are skipped (they are only relevant at
 * runtime query time, not for compile-time getter generation).
 */
export function compileWhereClause(
    where: Where,
    metadata: ModelMetadata | null,
): string[] {
    const conditions: string[] = [];

    for (const [propertyName, condition] of Object.entries(where)) {
        // Skip special fields that don't apply at compile time
        if (['id', 'author', 'timestamp'].includes(propertyName)) continue;

        let predicate: string;
        // Storage mode is derived from `resolveLanguage` alone:
        //   - undefined      → deterministic typed literal (fast POS-index path)
        //   - "literal"      → signed envelope on the literal language
        //   - custom address → expression on that custom language
        // Envelope and custom-language properties both need the `parse_literal`
        // unwrap path, so we treat any `resolveLanguage`-carrying property as
        // non-deterministic here.
        let isDeterministicLiteral: boolean | undefined;

        if (metadata) {
            const propMeta = metadata.properties[propertyName];
            if (propMeta) {
                predicate = propMeta.predicate;
                isDeterministicLiteral = propMeta.resolveLanguage === undefined;
            } else {
                predicate = propertyName;
            }
        } else {
            predicate = propertyName;
        }

        const cond = buildWhereCondition(predicate, condition, { isDeterministicLiteral, varIndex: conditions.length });
        if (cond) {
            conditions.push(cond);
        }
    }

    return conditions;
}
