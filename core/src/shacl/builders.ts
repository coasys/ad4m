/**
 * Builder functions for SHACL-AF Node Expressions.
 *
 * These functions construct JSON-serializable expression trees for property transforms.
 * All builders return frozen objects to prevent accidental mutation.
 */

import type { NodeExpression } from './NodeExpression';

/**
 * Identity expression: pass the focus node through unchanged.
 */
export function focus(): NodeExpression {
  return Object.freeze({ type: 'focus' });
}

/**
 * Literal constant expression.
 */
export function literal(value: string | number | boolean): NodeExpression {
  return Object.freeze({ type: 'literal', value });
}

/**
 * Path expression: navigate to a property on the resolved object.
 * The predicate can be a full IRI (e.g., "file_storage://data_base64")
 * or a simple property name. The Rust evaluator will strip the namespace.
 */
export function path(predicate: string): NodeExpression {
  return Object.freeze({ type: 'path', predicate });
}

/**
 * Existence check: returns true if the inner expression is non-null and non-empty.
 */
export function exists(expr: NodeExpression): NodeExpression {
  return Object.freeze({ type: 'exists', expr });
}

/**
 * Conditional expression with optional else branch.
 * Returns a chainable builder that supports `.then()` and `.else()`.
 */
export function ifExpr(cond: NodeExpression) {
  return {
    then: (thenExpr: NodeExpression) => ({
      else: (elseExpr?: NodeExpression) => {
        return Object.freeze({
          type: 'if',
          cond,
          then: thenExpr,
          ...(elseExpr !== undefined && { else: elseExpr }),
        } as const);
      },
      // Allow chaining without explicit else()
      toExpression: () => Object.freeze({
        type: 'if',
        cond,
        then: thenExpr,
      } as const),
    }),
  };
}

/**
 * String concatenation: joins evaluated arguments as strings.
 * Null/undefined arguments are skipped.
 */
export function concat(...args: NodeExpression[]): NodeExpression {
  return Object.freeze({ type: 'concat', args });
}

/**
 * Coalesce: returns the first non-null evaluated argument.
 */
export function coalesce(...args: NodeExpression[]): NodeExpression {
  return Object.freeze({ type: 'coalesce', args });
}

/**
 * Function call: invoke a named transform function by IRI.
 * Primarily used for built-in functions like "ad4m://fn/defaultFileDecode".
 */
export function fn(iri: string, args: NodeExpression[] = []): NodeExpression {
  return Object.freeze({ type: 'function', iri, args });
}
