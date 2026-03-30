/**
 * Shared query compilation utilities.
 *
 * These functions are used by both the runtime query builder (`Ad4mModel`) and
 * the compile-time SHACL generator (`decorators.ts`).
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
 * Compile a single Where condition for a property into a query
 * graph-traversal sub-expression.
 *
 * Operates on the **target node** — generates `count(->link[WHERE ...]) > 0`
 * style checks that can be used inside a `[WHERE ...]` filter on nodes.
 *
 * @param predicate  - The predicate URI for the property
 * @param condition  - The Where condition value
 * @param opts       - Optional settings (e.g. `resolveLanguage`)
 * @returns A single query condition string
 */
export function buildWhereCondition(
    predicate: string,
    condition: WhereCondition,
    opts?: { resolveLanguage?: string },
): string {
    const escapedPredicate = escapeQueryString(predicate);
    // Default to fn::parse_literal for properties without resolveLanguage or with resolveLanguage="literal",
    // since the Rust executor stores their values as literal: IRIs.
    // Only use raw out.uri when resolveLanguage is explicitly set to a non-literal language.
    const useParseLiteral = !opts?.resolveLanguage || opts.resolveLanguage === 'literal';
    const targetField = useParseLiteral
        ? 'fn::parse_literal(out.uri)'
        : 'out.uri';

    if (Array.isArray(condition)) {
        // Array values → IN clause
        const formattedValues = (condition as any[]).map(v => formatQueryValue(v)).join(', ');
        return `count(->link[WHERE predicate = '${escapedPredicate}' AND ${targetField} IN [${formattedValues}]]) > 0`;
    } else if (typeof condition === 'object' && condition !== null) {
        // Operator object
        const ops = condition as any;
        const parts: string[] = [];

        if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
                const formattedValues = ops.not.map((v: any) => formatQueryValue(v)).join(', ');
                parts.push(
                    `count(->link[WHERE predicate = '${escapedPredicate}' AND ${targetField} IN [${formattedValues}]]) = 0`,
                );
            } else {
                parts.push(
                    `count(->link[WHERE predicate = '${escapedPredicate}' AND ${targetField} = ${formatQueryValue(ops.not)}]) = 0`,
                );
            }
        }

        // Comparison operators — ensure property exists (actual comparison
        // may be post-filtered in JS at runtime, but the query condition
        // is valid for compile-time getter generation too).
        const hasComparisonOps =
            ops.gt !== undefined || ops.gte !== undefined ||
            ops.lt !== undefined || ops.lte !== undefined ||
            ops.between !== undefined || ops.contains !== undefined;
        if (hasComparisonOps) {
            parts.push(
                `count(->link[WHERE predicate = '${escapedPredicate}']) > 0`,
            );
        }

        return parts.join(' AND ');
    } else {
        // Simple equality
        return `count(->link[WHERE predicate = '${escapedPredicate}' AND ${targetField} = ${formatQueryValue(condition)}]) > 0`;
    }
}

/**
 * Compile a full `Where` clause to an array of query condition strings.
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
        let resolveLanguage: string | undefined;

        if (metadata) {
            const propMeta = metadata.properties[propertyName];
            if (propMeta) {
                predicate = propMeta.predicate;
                resolveLanguage = propMeta.resolveLanguage;
            } else {
                // Property not found in metadata — treat name as raw predicate URI
                predicate = propertyName;
            }
        } else {
            // No metadata — treat name as raw predicate URI
            predicate = propertyName;
        }

        const cond = buildWhereCondition(predicate, condition, { resolveLanguage });
        if (cond) {
            conditions.push(cond);
        }
    }

    return conditions;
}
