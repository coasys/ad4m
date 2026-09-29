import { Link } from "../links/Links";
import { Literal } from "../Literal";
import type { NodeExpression } from "./NodeExpression";
import { isNodeExpression } from "./NodeExpression";

/**
 * Extract namespace from a URI
 * Examples:
 *   - "recipe://name" -> "recipe://"
 *   - "https://example.com/vocab#term" -> "https://example.com/vocab#"
 *   - "https://example.com/vocab/term" -> "https://example.com/vocab/"
 *   - "recipe:Recipe" -> "recipe:"
 */
function extractNamespace(uri: string): string {
  // Handle hash fragments first (highest priority)
  const hashIndex = uri.lastIndexOf('#');
  if (hashIndex !== -1) {
    return uri.substring(0, hashIndex + 1);
  }
  
  // Handle protocol-style URIs with paths
  const protocolMatch = uri.match(/^([a-zA-Z][a-zA-Z0-9+.-]*:\/\/)(.*)$/);
  if (protocolMatch) {
    const afterScheme = protocolMatch[2];
    const lastSlash = afterScheme.lastIndexOf('/');
    if (lastSlash !== -1) {
      // Has path segments - namespace includes up to last slash
      return protocolMatch[1] + afterScheme.substring(0, lastSlash + 1);
    }
    // Simple protocol URI without path (e.g., "recipe://name")
    return protocolMatch[1];
  }
  
  // Handle colon-separated (namespace:localName)
  const colonMatch = uri.match(/^([a-zA-Z][a-zA-Z0-9+.-]*:)/);
  if (colonMatch) {
    return colonMatch[1];
  }
  
  // Fallback: no clear namespace
  return '';
}

/**
 * Escape special characters for Turtle string literals
 * Handles backslashes, quotes, newlines, and other control characters
 */
function escapeTurtleString(value: string): string {
  return value
    .replace(/\\/g, '\\\\')     // Backslash must be first
    .replace(/"/g, '\\"')        // Double quotes
    .replace(/\n/g, '\\n')       // Newlines
    .replace(/\r/g, '\\r')       // Carriage returns
    .replace(/\t/g, '\\t')       // Tabs
    .replace(/\x08/g, '\\b')     // Backspace (not /\b/, which matches word boundaries)
    .replace(/\f/g, '\\f');      // Form feed
}

/** Parse a stored `transform`; throws when it is not a valid NodeExpression. */
function parseTransform(json: string, propShapeId: string): NodeExpression {
  let parsed: unknown;
  try { parsed = JSON.parse(json); } catch { parsed = undefined; }
  if (!isNodeExpression(parsed)) {
    throw new Error(`Failed to deserialize transform for property ${propShapeId}: not a valid NodeExpression. Payload: ${json}`);
  }
  return parsed;
}

/** Whether the executor stores a property as a collection (`ad4m://CollectionShape`). */
function isCollection(p: SHACLPropertyShape): boolean {
  return p.collection ?? (p.relationKind === 'hasMany' || p.relationKind === 'belongsToMany');
}

/**
 * Extract local name from a URI
 * Examples:
 *   - "recipe://name" -> "name"
 *   - "https://example.com/vocab#term" -> "term"
 *   - "https://example.com/vocab/term" -> "term"
 *   - "recipe:Recipe" -> "Recipe"
 */
function extractLocalName(uri: string): string {
  // Handle hash fragments
  const hashIndex = uri.lastIndexOf('#');
  if (hashIndex !== -1) {
    return uri.substring(hashIndex + 1);
  }
  
  // Handle slash-based namespaces
  const lastSlash = uri.lastIndexOf('/');
  if (lastSlash !== -1 && lastSlash < uri.length - 1) {
    return uri.substring(lastSlash + 1);
  }
  
  // Handle colon-separated
  const colonMatch = uri.match(/^[a-zA-Z][a-zA-Z0-9+.-]*:(.+)$/);
  if (colonMatch) {
    return colonMatch[1];
  }
  
  // Fallback: entire URI
  return uri;
}

/**
 * AD4M Action - represents a link operation
 */
export interface AD4MAction {
  action: string;
  source: string;
  predicate: string;
  target: string;
  local?: boolean;
}

/**
 * A single structured conformance condition for relation filtering.
 * DB-agnostic representation that can be translated to any query language.
 */
export interface ConformanceCondition {
  /** Type of check: 'flag' (predicate + value) or 'required' (predicate exists) */
  type: 'flag' | 'required';
  /** The predicate URI to check on the target node */
  predicate: string;
  /** For 'flag' conditions: the expected value */
  value?: string;
}

/**
 * SHACL Property Shape
 * Represents constraints on a single property path
 */
export interface SHACLPropertyShape {
  /** Property name (e.g., "name", "ingredients") - used for generating named URIs */
  name?: string;

  /** The property path (predicate URI) */
  path: string;

  /** Expected datatype (e.g., xsd:string, xsd:integer) */
  datatype?: string;

  /**
   * CRDT ordering strategy for this collection ("linkedList"), when it has one.
   *
   * Declared in the shape rather than passed per query so the executor applies
   * it to every writer — the ORM, MCP agents, raw WS-RPC callers, another app
   * on the same neighbourhood.
   */
  ordering?: string;

  /** Node kind constraint (IRI, Literal, BlankNode) */
  nodeKind?: 'IRI' | 'Literal' | 'BlankNode';

  /** Minimum cardinality (required if >= 1) */
  minCount?: number;

  /** Maximum cardinality (single-valued if 1, omit for collections) */
  maxCount?: number;

  /** Regex pattern for string validation */
  pattern?: string;

  /** Minimum value (inclusive) for numeric properties */
  minInclusive?: number;

  /** Maximum value (inclusive) for numeric properties */
  maxInclusive?: number;

  /** Fixed value constraint (for Flag properties) */
  hasValue?: string;

  /** AD4M-specific: Local-only property */
  local?: boolean;

  /** AD4M-specific: Writable property */
  writable?: boolean;

  /** AD4M-specific: sole selector of storage mode:
   *   - unset               → deterministic typed literal (fast POS-index
   *                            path, the default for a plain `@Property()`).
   *   - `"literal"`         → signed envelope on the built-in literal
   *                            language (per-value provenance, e.g. Flux
   *                            message bodies).
   *   - `<custom address>`  → `expression_create` on that custom language. */
  resolveLanguage?: string;

  /** AD4M-specific: Setter action for this property */
  setter?: AD4MAction[];

  /** AD4M-specific: Adder action for collection properties */
  adder?: AD4MAction[];

  /** AD4M-specific: Remover action for collection properties */
  remover?: AD4MAction[];

  /** AD4M-specific: Pre-computed SPARQL getter expression for reading this relation/property.
   *  For relations with a target model, this encodes conformance filtering
   *  so that Rust/MCP can execute the exact same query as the JS runtime. */
  getter?: string;

  /** AD4M-specific: Structured conformance conditions (DB-agnostic).
   *  Each condition describes a check on the target node (flag match or required property). */
  conformanceConditions?: ConformanceCondition[];

  /** sh:class — the target SHACL node shape URI that linked nodes must conform to.
   *  Set automatically when a relation has a `target` model. Enables typed construction
   *  on the Rust/MCP side by referencing the full target shape. */
  class?: string;

  /** sh:in — allowed values for this property (enum constraint).
   *  Each entry has a `value` (the RDF term) and an optional `label` (human-readable name).
   *  Standard SHACL `sh:in` only defines values; labels are an AD4M extension. */
  in?: Array<{ value: string; label?: string }>;

  /** AD4M-specific: multi-valued property (`ad4m://CollectionShape`).
   *  `toJSON()` defaults it to true for `hasMany` and `belongsToMany`. */
  collection?: boolean;

  /** AD4M-specific: kind of relation this property describes.
   *  Drives direction (forward/reverse), scalar-vs-collection rendering,
   *  and default max-count. */
  relationKind?: 'hasMany' | 'hasOne' | 'belongsToOne' | 'belongsToMany';

  /** AD4M-specific: target class name (local-name) for a relation property.
   *  Complements `class` (the SHACL node-shape URI) by providing the bare
   *  model class name used by the executor to look up the target shape
   *  through the perspective's shape cache. */
  targetClassName?: string;

  /** AD4M-specific: post-getter where-clause filter for a relation property. */
  whereFilter?: any;

  /** AD4M-specific: predicate-IRI lookup map for `whereFilter` keys
   *  (property name → predicate URI on the target class). */
  wherePredicates?: Record<string, string>;

  /** AD4M-specific: whether conformance/type filtering is enabled for this
   *  relation.  `false` opts out of DB-level type filtering while keeping
   *  hydration via `include`. */
  filter?: boolean;

  /** AD4M-specific: Transform expression (SHACL-AF Node Expression).
   *  Applied to a property's resolved value in the Rust model query engine —
   *  for values resolved through a `resolveLanguage` (`"literal"` for a signed
   *  envelope, or a custom language) and for any property that declares a
   *  transform. */
  transform?: NodeExpression;

  /** AD4M-specific: Natural-language hint that steers the generic LLM
   *  extractor when producing typed instances from a transcript.  Emitted
   *  as an `ad4m://interpretation_hint` link on the property shape node and
   *  surfaced on `ShapeProperty.interpretation_hint` by the Rust model query. */
  interpretationHint?: string;

  /** AD4M-specific: marks this property as the class's identity (dedup key)
   *  for the generic LLM interpreter. Emitted as an `ad4m://identity` link
   *  (`literal:string:true`) and read back on `ShapeProperty.identity`. */
  identity?: boolean;
}

/**
 * SHACL Node Shape
 * Defines constraints for instances of a class
 */
export class SHACLShape {
  /** URI of this shape (e.g., recipe:RecipeShape) */
  nodeShapeUri: string;

  /** Target class this shape applies to (e.g., recipe:Recipe) */
  targetClass?: string;

  /** Property constraints */
  properties: SHACLPropertyShape[];

  /** AD4M-specific: Constructor actions for creating instances */
  constructor_actions?: AD4MAction[];

  /** AD4M-specific: Destructor actions for removing instances */
  destructor_actions?: AD4MAction[];

  /** Parent shape URIs for model inheritance (sh:node references) */
  parentShapes: string[];

  /** AD4M-specific: Natural-language hint that steers the generic LLM
   *  extractor when producing instances of this class.  Emitted as an
   *  `ad4m://interpretation_hint` link on the shape node itself and surfaced
   *  on `ModelShape.interpretation_hint` by the Rust model query. */
  interpretationHint?: string;

  /**
   * Create a new SHACL Shape
   * @param targetClassOrShapeUri - If one argument: the target class (shape URI auto-derived as {class}Shape)
   *                                If two arguments: first is shape URI, second is target class
   * @param targetClass - Optional target class when first arg is shape URI
   */
  constructor(targetClassOrShapeUri: string, targetClass?: string) {
    if (targetClass !== undefined) {
      // Two arguments: explicit shape URI and target class
      this.nodeShapeUri = targetClassOrShapeUri;
      this.targetClass = targetClass;
    } else {
      // One argument: derive shape URI from target class
      this.targetClass = targetClassOrShapeUri;
      // Derive shape URI by appending "Shape" to the local name
      const namespace = extractNamespace(targetClassOrShapeUri);
      const localName = extractLocalName(targetClassOrShapeUri);
      this.nodeShapeUri = `${namespace}${localName}Shape`;
    }
    this.properties = [];
    this.parentShapes = [];
  }

  /**
   * Add a parent shape reference (sh:node) for model inheritance.
   * When a @Model class extends another @Model, the child shape
   * references the parent shape so SHACL validators can walk the
   * class hierarchy.
   */
  addParentShape(parentShapeUri: string): void {
    if (!this.parentShapes.includes(parentShapeUri)) {
      this.parentShapes.push(parentShapeUri);
    }
  }

  /**
   * Add a property constraint to this shape
   */
  addProperty(prop: SHACLPropertyShape): void {
    this.properties.push(prop);
  }

  /**
   * Set constructor actions for this shape
   */
  setConstructorActions(actions: AD4MAction[]): void {
    this.constructor_actions = actions;
  }

  /**
   * Set destructor actions for this shape
   */
  setDestructorActions(actions: AD4MAction[]): void {
    this.destructor_actions = actions;
  }
  
  /**
   * Serialize shape to Turtle (RDF) format
   */
  toTurtle(): string {
    const links = this.toLinks();
    const str = (v: string) => `"${escapeTurtleString(v)}"`;
    const pred = (p: string) => p === 'rdf://type' ? 'a' : p.replace(/^(sh|ad4m):\/\//, '$1:');
    const obj = ({ predicate, target: t }: Link): string => {
      if (predicate === 'ad4m://identity') return 'true';
      if (predicate === 'sh://pattern') return str(t.slice('literal:'.length));
      if (predicate === 'sh://hasValue' && t.startsWith('literal:')) {
        // A literal URL: numbers and booleans bare, anything else a string.
        let v: unknown; try { v = Literal.fromUrl(t).get(); } catch { v = t; }
        return typeof v === 'number' || typeof v === 'boolean' ? String(v) : str(typeof v === 'string' ? v : JSON.stringify(v));
      }
      if (t.startsWith('literal:string:')) return str(t.slice(15));
      if (t.startsWith('literal:')) return t.slice(8).replace(/\^\^.*$/, ''); // numbers and booleans
      if (t.startsWith('sh://')) return pred(t);
      return `<${t}>`;
    };
    // One Turtle statement per link; sh:in becomes a standard RDF list, and
    // ad4m:in keeps the JSON with labels.
    const statements = (source: string) => links
      .filter(l => l.source === source && l.predicate !== 'sh://property')
      .flatMap(l => l.predicate === 'sh://in'
        ? [`sh:in ( ${JSON.parse(l.target.slice(15)).map((v: { value: string }) => str(v.value)).join(' ')} )`, `ad4m:in ${obj(l)}`]
        : [`${pred(l.predicate!)} ${obj(l)}`]);

    const properties = links
      .filter(l => l.source === this.nodeShapeUri && l.predicate === 'sh://property')
      .map((l, i) => {
        const lines = statements(l.target);
        // A blank node cannot carry the name the way the link URI does.
        const name = this.properties[i].name;
        if (name) lines.splice(1, 0, `sh:name ${str(name)}`);
        return `sh:property [\n    ${lines.join(' ;\n    ')}\n  ]`;
      });

    return `@prefix sh: <http://www.w3.org/ns/shacl#> .\n` +
      `@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n` +
      `@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n` +
      `@prefix ad4m: <ad4m://> .\n\n` +
      `<${this.nodeShapeUri}>\n  ${[...statements(this.nodeShapeUri), ...properties].join(' ;\n  ')} .\n`;
  }

  
  /**
   * Serialize shape to AD4M Links (RDF triples)
   * Stores the shape as a graph of links in a Perspective
   */
  toLinks(): Link[] {
    const links: Link[] = [];

    // Shape type declaration
    links.push({
      source: this.nodeShapeUri,
      predicate: "rdf://type",
      target: "sh://NodeShape"
    });

    // Target class
    if (this.targetClass) {
      links.push({
        source: this.nodeShapeUri,
        predicate: "sh://targetClass",
        target: this.targetClass
      });
    }

    // Parent shapes (model inheritance)
    for (const parentUri of this.parentShapes) {
      links.push({
        source: this.nodeShapeUri,
        predicate: "sh://node",
        target: parentUri
      });
    }

    // Constructor and destructor actions, always, as the executor writes
    // them: an empty `[]` still tells `createSubject` the class exists.
    for (const [predicate, actions] of [
      ["ad4m://constructor", this.constructor_actions],
      ["ad4m://destructor", this.destructor_actions],
    ] as const) {
      links.push({
        source: this.nodeShapeUri,
        predicate,
        target: `literal:string:${JSON.stringify(actions ?? [])}`
      });
    }

    // Class-level interpretation hint — read by the Rust model query as
    // `ModelShape.interpretation_hint` and fed to the generic LLM extractor.
    if (this.interpretationHint) {
      links.push({
        source: this.nodeShapeUri,
        predicate: "ad4m://interpretation_hint",
        target: `literal:string:${this.interpretationHint}`
      });
    }

    // Property shapes (each gets a named URI: {namespace}/{ClassName}.{propertyName})
    for (let i = 0; i < this.properties.length; i++) {
      const prop = this.properties[i];
      
      // Generate named property shape URI
      let propShapeId: string;
      if (prop.name && this.targetClass) {
        // Extract namespace from targetClass
        const namespace = extractNamespace(this.targetClass);
        const className = extractLocalName(this.targetClass);
        // Use format: {namespace}{ClassName}.{propertyName}
        propShapeId = `${namespace}${className}.${prop.name}`;
      } else {
        // Fallback to blank node if name is missing
        propShapeId = `_:propShape${i}`;
      }
      
      // Link shape to property shape, typed as the executor types it
      links.push({
        source: this.nodeShapeUri,
        predicate: "sh://property",
        target: propShapeId
      });
      links.push({
        source: propShapeId,
        predicate: "rdf://type",
        target: isCollection(prop) ? "ad4m://CollectionShape" : "sh://PropertyShape"
      });
      
      // Property path
      links.push({
        source: propShapeId,
        predicate: "sh://path",
        target: prop.path
      });
      
      // Constraints
      if (prop.ordering) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://ordering",
          target: `literal:string:${prop.ordering}`
        } as any);
      }
      if (prop.datatype) {
        links.push({
          source: propShapeId,
          predicate: "sh://datatype",
          target: prop.datatype
        });
      }
      
      if (prop.nodeKind) {
        links.push({
          source: propShapeId,
          predicate: "sh://nodeKind",
          target: `sh://${prop.nodeKind}`
        });
      }
      
      if (prop.minCount !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://minCount",
          target: `literal:${prop.minCount}^^xsd:integer`
        });
      }
      
      if (prop.maxCount !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://maxCount",
          target: `literal:${prop.maxCount}^^xsd:integer`
        });
      }
      
      if (prop.pattern) {
        links.push({
          source: propShapeId,
          predicate: "sh://pattern",
          target: `literal:${prop.pattern}`
        });
      }
      
      if (prop.minInclusive !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://minInclusive",
          target: `literal:${prop.minInclusive}`
        });
      }
      
      if (prop.maxInclusive !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://maxInclusive",
          target: `literal:${prop.maxInclusive}`
        });
      }
      
      if (prop.hasValue) {
        // Same encoding as the executor: URIs and literal URLs as they are.
        const v = prop.hasValue;
        links.push({
          source: propShapeId,
          predicate: "sh://hasValue",
          target: v.includes('://') || v.startsWith('literal:') ? v : `literal:string:${encodeURIComponent(v)}`
        });
      }
      
      // AD4M-specific metadata
      if (prop.local !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://local",
          target: `literal:${prop.local}`
        });
      }
      
      if (prop.writable !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://writable",
          target: `literal:${prop.writable}`
        });
      }

      if (prop.resolveLanguage != null) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://resolveLanguage",
          target: `literal:string:${prop.resolveLanguage}`
        });
      }

      // AD4M-specific actions
      if (prop.setter && prop.setter.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://setter",
          target: `literal:string:${JSON.stringify(prop.setter)}`
        });
      }

      if (prop.adder && prop.adder.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://adder",
          target: `literal:string:${JSON.stringify(prop.adder)}`
        });
      }

      if (prop.remover && prop.remover.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://remover",
          target: `literal:string:${JSON.stringify(prop.remover)}`
        });
      }

      if (prop.getter) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://getter",
          target: `literal:string:${prop.getter}`
        });
      }

      if (prop.conformanceConditions && prop.conformanceConditions.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://conformanceConditions",
          target: `literal:string:${JSON.stringify(prop.conformanceConditions)}`
        });
      }

      if (prop.class) {
        links.push({
          source: propShapeId,
          predicate: "sh://class",
          target: prop.class
        });
      }

      if (prop.in && prop.in.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "sh://in",
          target: `literal:string:${JSON.stringify(prop.in)}`
        });
      }

      if (prop.relationKind) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://relationKind",
          target: `literal:string:${prop.relationKind}`
        });
      }

      if (prop.targetClassName) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://targetClassName",
          target: `literal:string:${prop.targetClassName}`
        });
      }

      if (prop.whereFilter !== undefined && prop.whereFilter !== null) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://whereFilter",
          target: `literal:string:${JSON.stringify(prop.whereFilter)}`
        });
      }

      if (prop.wherePredicates && Object.keys(prop.wherePredicates).length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://wherePredicates",
          target: `literal:string:${JSON.stringify(prop.wherePredicates)}`
        });
      }

      if (prop.filter !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://filter",
          target: `literal:${prop.filter}`
        });
      }

      if (prop.transform && typeof prop.transform === 'object') {
        links.push({
          source: propShapeId,
          predicate: "ad4m://transform",
          target: `literal:string:${JSON.stringify(prop.transform)}`
        });
      }

      if (prop.interpretationHint) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://interpretation_hint",
          target: `literal:string:${prop.interpretationHint}`
        });
      }

      if (prop.identity) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://identity",
          target: `literal:string:true`
        });
      }
    }

    return links;
  }
  
  /**
   * Reconstruct shape from AD4M Links
   */
  static fromLinks(links: Link[], shapeUri: string): SHACLShape {
    const target = (source: string, predicate: string) =>
      links.find(l => l.source === source && l.predicate === predicate)?.target;
    // `literal:string:<text>` (not URL-encoded) and `literal:<value>` targets.
    const str = (source: string, predicate: string) => target(source, predicate)?.replace(/^literal:string:/, '');
    const plain = (source: string, predicate: string) => target(source, predicate)?.replace(/^literal:/, '');
    // Counts and flags: the executor wrote `literal://number:N` and
    // `literal://boolean:B` from 2026-02-02 to 2026-02-17, and its storage
    // migration only rewrites them to `literal:number:N` / `literal:boolean:B`.
    // The executor's own reader (model_query/shape.rs) still accepts both.
    const bool = (source: string, predicate: string) => {
      const v = plain(source, predicate)?.replace(/^boolean:/, '');
      return v === undefined ? undefined : v === 'true';
    };
    const num = (source: string, predicate: string, parse: (v: string) => number) => {
      const v = plain(source, predicate)?.replace(/^number:/, '');
      return v === undefined ? undefined : parse(v.replace(/\^\^.*$/, ''));
    };
    // JSON payloads that fail to parse are dropped.
    const json = (source: string, predicate: string) => {
      const v = str(source, predicate);
      if (v === undefined) return undefined;
      try { return JSON.parse(v); } catch { return undefined; }
    };

    const shape = new SHACLShape(shapeUri, target(shapeUri, "sh://targetClass"));
    for (const l of links) {
      if (l.source === shapeUri && l.predicate === "sh://node") shape.addParentShape(l.target);
    }
    shape.constructor_actions = json(shapeUri, "ad4m://constructor");
    shape.destructor_actions = json(shapeUri, "ad4m://destructor");
    shape.interpretationHint = str(shapeUri, "ad4m://interpretation_hint");

    for (const propLink of links.filter(l => l.source === shapeUri && l.predicate === "sh://property")) {
      const id = propLink.target;
      const path = target(id, "sh://path");
      if (!path) continue;

      // Named property shapes are `{namespace}{ClassName}.{propertyName}`.
      const dot = id.lastIndexOf('.');
      const name = !id.startsWith('_:') && dot !== -1 ? id.substring(dot + 1) : undefined;

      const relationKind = str(id, "ad4m://relationKind");
      const hasValue = target(id, "sh://hasValue");
      const identity = str(id, "ad4m://identity");
      const transform = str(id, "ad4m://transform");

      const prop: SHACLPropertyShape = {
        name,
        path,
        collection: links.some(l => l.source === id && l.predicate === "rdf://type" && l.target === "ad4m://CollectionShape") || undefined,
        datatype: target(id, "sh://datatype"),
        ordering: str(id, "ad4m://ordering"),
        nodeKind: target(id, "sh://nodeKind")?.replace('sh://', '') as SHACLPropertyShape['nodeKind'],
        minCount: num(id, "sh://minCount", parseInt),
        maxCount: num(id, "sh://maxCount", parseInt),
        pattern: plain(id, "sh://pattern"),
        minInclusive: num(id, "sh://minInclusive", parseFloat),
        maxInclusive: num(id, "sh://maxInclusive", parseFloat),
        hasValue: hasValue?.startsWith('literal:string:') ? decodeURIComponent(hasValue.slice(15)) : hasValue,
        local: bool(id, "ad4m://local"),
        writable: bool(id, "ad4m://writable"),
        resolveLanguage: str(id, "ad4m://resolveLanguage"),
        setter: json(id, "ad4m://setter"),
        adder: json(id, "ad4m://adder"),
        remover: json(id, "ad4m://remover"),
        getter: str(id, "ad4m://getter"),
        conformanceConditions: json(id, "ad4m://conformanceConditions"),
        class: target(id, "sh://class"),
        in: json(id, "sh://in"),
        relationKind: ['hasMany', 'hasOne', 'belongsToOne', 'belongsToMany'].includes(relationKind!)
          ? relationKind as SHACLPropertyShape['relationKind'] : undefined,
        targetClassName: str(id, "ad4m://targetClassName"),
        whereFilter: json(id, "ad4m://whereFilter"),
        wherePredicates: json(id, "ad4m://wherePredicates"),
        filter: bool(id, "ad4m://filter"),
        interpretationHint: str(id, "ad4m://interpretation_hint"),
        identity: identity === undefined ? undefined : identity === 'true',
        transform: transform === undefined ? undefined : parseTransform(transform, id),
      };
      shape.addProperty(Object.fromEntries(Object.entries(prop).filter(([, v]) => v !== undefined)) as SHACLPropertyShape);
    }

    return shape;
  }

  /**
   * Convert the shape to a JSON-serializable object.
   * Useful for passing to addSdna() as shaclJson parameter.
   * 
   * @returns JSON-serializable object representing the shape
   */
  toJSON(): object {
    return {
      node_shape_uri: this.nodeShapeUri,
      target_class: this.targetClass,
      parent_shapes: this.parentShapes.length > 0 ? this.parentShapes : undefined,
      interpretation_hint: this.interpretationHint,
      properties: this.properties.map(p => ({
        path: p.path,
        name: p.name,
        datatype: p.datatype,
        node_kind: p.nodeKind,
        min_count: p.minCount,
        max_count: p.maxCount,
        min_inclusive: p.minInclusive,
        max_inclusive: p.maxInclusive,
        pattern: p.pattern,
        has_value: p.hasValue,
        local: p.local,
        writable: p.writable,
        resolve_language: p.resolveLanguage,
        setter: p.setter,
        adder: p.adder,
        remover: p.remover,
        getter: p.getter,
        conformance_conditions: p.conformanceConditions,
        class: p.class,
        in: p.in,
        relation_kind: p.relationKind,
        collection: p.collection ?? (isCollection(p) || undefined),
        target_class_name: p.targetClassName,
        where_filter: p.whereFilter,
        where_predicates: p.wherePredicates,
        filter: p.filter,
        transform: p.transform,
        interpretation_hint: p.interpretationHint,
        identity: p.identity,
        // The backend reads the ordering declaration back from the
        // `ad4m://ordering` link that `parse_shacl_to_links` writes from this
        // field. `ensureSubjectClass` ships `toJSON()`, so omitting it here
        // leaves `@HasMany({ ordering })` inert no matter what `toLinks()` does.
        ordering: p.ordering,
      })),
      constructor_actions: this.constructor_actions,
      destructor_actions: this.destructor_actions,
    };
  }

  /**
   * Create a shape from a JSON object (inverse of toJSON)
   */
  static fromJSON(json: any): SHACLShape {
    const shape = json.node_shape_uri
      ? new SHACLShape(json.node_shape_uri, json.target_class)
      : new SHACLShape(json.target_class);
    
    for (const p of json.properties || []) {
      // Validate transform if present
      if (p.transform && !isNodeExpression(p.transform)) {
        throw new Error(
          `Invalid transform for property ${p.name}: ` +
          `payload is not a valid NodeExpression. Received: ${JSON.stringify(p.transform)}`
        );
      }

      shape.addProperty({
        path: p.path,
        name: p.name,
        datatype: p.datatype,
        nodeKind: p.node_kind,
        minCount: p.min_count,
        maxCount: p.max_count,
        minInclusive: p.min_inclusive,
        maxInclusive: p.max_inclusive,
        pattern: p.pattern,
        hasValue: p.has_value,
        local: p.local,
        writable: p.writable,
        resolveLanguage: p.resolve_language,
        setter: p.setter,
        adder: p.adder,
        remover: p.remover,
        getter: p.getter,
        conformanceConditions: p.conformance_conditions,
        class: p.class,
        in: p.in,
        relationKind: p.relation_kind,
        collection: p.collection,
        targetClassName: p.target_class_name,
        whereFilter: p.where_filter,
        wherePredicates: p.where_predicates,
        filter: p.filter,
        transform: p.transform,
        interpretationHint: p.interpretation_hint,
        identity: p.identity,
        ordering: p.ordering,
      });
    }

    if (json.constructor_actions) {
      shape.constructor_actions = json.constructor_actions;
    }
    if (json.destructor_actions) {
      shape.destructor_actions = json.destructor_actions;
    }
    if (json.parent_shapes) {
      for (const ps of json.parent_shapes) {
        shape.addParentShape(ps);
      }
    }
    if (json.interpretation_hint) {
      shape.interpretationHint = json.interpretation_hint;
    }

    return shape;
  }
}
