/**
 * Types for the perspective model manifest — a human-readable, AI-friendly summary
 * of all Ad4mModel-compatible classes defined in a perspective's SHACL shapes.
 *
 * Used by `PerspectiveProxy.getModelManifest()` and consumed by WE's AI context
 * injection (`formatManifestForPrompt`) to describe external perspective models
 * to the schema-generation AI.
 *
 * @module
 */

/** Normalised description of a single property on a model class. */
export interface ModelManifestProperty {
  /** Property name as declared in the SHACL shape (e.g. "name", "body", "participants"). */
  name: string;
  /** The exact RDF predicate URI (e.g. "flux://has_name"). */
  predicate: string;
  /** Normalised scalar type; inferred from xsd:/sh: datatype + nodeKind. */
  type: 'string' | 'number' | 'boolean' | 'uri';
  /** `true` when maxCount is absent or > 1 (i.e. returned as an array). */
  isCollection: boolean;
  /** `true` when minCount >= 1. */
  required: boolean;
  writable: boolean;
  /** AD4M language used to resolve the value (e.g. "literal"). */
  resolveLanguage?: string;
  /**
   * Local name of the related model class when this is a typed relation property.
   * Derived from `SHACLPropertyShape.class` by stripping the URI prefix.
   * E.g. `flux://Message` → `"Message"`.
   */
  relatedModel?: string;
}

/** Normalised description of a single model class stored in a perspective. */
export interface ModelManifestEntry {
  /** Class name — usable directly as the `model` field in a `$query` token. */
  name: string;
  /** The SHACL `targetClass` URI (e.g. "flux://Channel"). */
  targetClass: string;
  /** All data properties (flag/hasValue properties are excluded). */
  properties: ModelManifestProperty[];
}
