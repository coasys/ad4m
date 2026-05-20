/**
 * SHACL-AF Transform DSL — public API
 *
 * Re-exports builder functions and provides common built-in transforms.
 */

import {
  focus,
  literal,
  path,
  exists,
  ifExpr,
  concat,
  coalesce,
  fn,
} from './builders';

export type { NodeExpression } from './NodeExpression';
export { isNodeExpression } from './NodeExpression';

export {
  focus,
  literal,
  path,
  exists,
  ifExpr,
  concat,
  coalesce,
  fn,
};

/**
 * Built-in transform: FileData → data: URI
 *
 * Converts a file stored via FILE_STORAGE_LANGUAGE into a data: URL.
 * If the object has a `data_base64` field, returns:
 *   `data:{file_type};base64,{data_base64}`
 * Otherwise passes through the input unchanged.
 *
 * This is the Rust equivalent of the JS `defaultFileDecode` function.
 *
 * Usage:
 * ```typescript
 * @Property({
 *   through: 'we://image',
 *   resolveLanguage: FILE_STORAGE_LANGUAGE,
 *   transform: fileToDataUri,
 * })
 * avatar?: string;
 * ```
 */
export const fileToDataUri = ifExpr(exists(path('file_storage://data_base64')))
  .then(
    concat(
      literal('data:'),
      coalesce(
        path('file_storage://file_type'),
        literal('image/png'),
      ),
      literal(';base64,'),
      path('file_storage://data_base64'),
    ),
  )
  .else(focus());

// Re-export SHACL types for convenience
export {
  SHACLShape,
  SHACLPropertyShape,
  AD4MAction,
  ConformanceCondition,
} from './SHACLShape';
