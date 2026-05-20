/**
 * SHACL-AF Node Expressions for property transforms.
 *
 * A `NodeExpression` is a JSON-serializable tree that describes how to transform
 * a resolved property value. Unlike JS lambdas, these are portable, inspectable,
 * and can be executed in the Rust model query engine.
 *
 * Each expression is a tagged union that evaluates to a `Value` (JSON-compatible).
 */

/**
 * Union type representing all supported transform expressions.
 * Each variant maps to a SHACL-AF §6 expression construct.
 */
export type NodeExpression =
  | { type: 'focus' }
  | { type: 'literal'; value: string | number | boolean }
  | { type: 'path'; predicate: string }
  | { type: 'exists'; expr: NodeExpression }
  | { type: 'if'; cond: NodeExpression; then: NodeExpression; else?: NodeExpression }
  | { type: 'concat'; args: NodeExpression[] }
  | { type: 'coalesce'; args: NodeExpression[] }
  | { type: 'function'; iri: string; args: NodeExpression[] };

/**
 * Type guard to check if a value is a valid NodeExpression.
 */
export function isNodeExpression(v: unknown): v is NodeExpression {
  if (typeof v !== 'object' || v === null) return false;
  const obj = v as any;
  const type = obj.type;

  switch (type) {
    case 'focus':
      return typeof obj.type === 'string';
    case 'literal':
      return typeof obj.value === 'string' || typeof obj.value === 'number' || typeof obj.value === 'boolean';
    case 'path':
      return typeof obj.predicate === 'string';
    case 'exists':
      return isNodeExpression(obj.expr);
    case 'if':
      return isNodeExpression(obj.cond) && isNodeExpression(obj.then) &&
             (obj.else === undefined || isNodeExpression(obj.else));
    case 'concat':
      return Array.isArray(obj.args) && obj.args.every(isNodeExpression);
    case 'coalesce':
      return Array.isArray(obj.args) && obj.args.every(isNodeExpression);
    case 'function':
      return typeof obj.iri === 'string' && Array.isArray(obj.args) && obj.args.every(isNodeExpression);
    default:
      return false;
  }
}
