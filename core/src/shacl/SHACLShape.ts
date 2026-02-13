import { Link } from "../links/Links";

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

  /** AD4M-specific: Language to resolve property values through */
  resolveLanguage?: string;

  /** AD4M-specific: Setter action for this property */
  setter?: AD4MAction[];

  /** AD4M-specific: Adder action for collection properties */
  adder?: AD4MAction[];

  /** AD4M-specific: Remover action for collection properties */
  remover?: AD4MAction[];
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
    let turtle = `@prefix sh: <http://www.w3.org/ns/shacl#> .\n`;
    turtle += `@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n`;
    turtle += `@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n`;
    turtle += `@prefix ad4m: <ad4m://> .\n\n`;
    
    turtle += `<${this.nodeShapeUri}>\n`;
    turtle += `  a sh:NodeShape ;\n`;
    
    if (this.targetClass) {
      turtle += `  sh:targetClass <${this.targetClass}> ;\n`;
    }
    
    // Add property shapes
    for (let i = 0; i < this.properties.length; i++) {
      const prop = this.properties[i];
      const isLast = i === this.properties.length - 1;
      
      turtle += `  sh:property [\n`;
      turtle += `    sh:path <${prop.path}> ;\n`;
      
      if (prop.datatype) {
        turtle += `    sh:datatype <${prop.datatype}> ;\n`;
      }
      
      if (prop.nodeKind) {
        turtle += `    sh:nodeKind sh:${prop.nodeKind} ;\n`;
      }
      
      if (prop.minCount !== undefined) {
        turtle += `    sh:minCount ${prop.minCount} ;\n`;
      }
      
      if (prop.maxCount !== undefined) {
        turtle += `    sh:maxCount ${prop.maxCount} ;\n`;
      }
      
      if (prop.pattern) {
        turtle += `    sh:pattern "${prop.pattern}" ;\n`;
      }
      
      if (prop.minInclusive !== undefined) {
        turtle += `    sh:minInclusive ${prop.minInclusive} ;\n`;
      }
      
      if (prop.maxInclusive !== undefined) {
        turtle += `    sh:maxInclusive ${prop.maxInclusive} ;\n`;
      }
      
      if (prop.hasValue) {
        turtle += `    sh:hasValue "${prop.hasValue}" ;\n`;
      }
      
      // AD4M-specific metadata
      if (prop.local !== undefined) {
        turtle += `    ad4m:local ${prop.local} ;\n`;
      }
      
      if (prop.writable !== undefined) {
        turtle += `    ad4m:writable ${prop.writable} ;\n`;
      }
      
      // Remove trailing semicolon and close bracket
      turtle = turtle.slice(0, -2) + '\n';
      turtle += isLast ? `  ] .\n` : `  ] ;\n`;
    }
    
    return turtle;
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

    // Constructor actions
    if (this.constructor_actions && this.constructor_actions.length > 0) {
      links.push({
        source: this.nodeShapeUri,
        predicate: "ad4m://constructor",
        target: `literal://string:${JSON.stringify(this.constructor_actions)}`
      });
    }

    // Destructor actions
    if (this.destructor_actions && this.destructor_actions.length > 0) {
      links.push({
        source: this.nodeShapeUri,
        predicate: "ad4m://destructor",
        target: `literal://string:${JSON.stringify(this.destructor_actions)}`
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
      
      // Link shape to property shape
      links.push({
        source: this.nodeShapeUri,
        predicate: "sh://property",
        target: propShapeId
      });
      
      // Property path
      links.push({
        source: propShapeId,
        predicate: "sh://path",
        target: prop.path
      });
      
      // Constraints
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
          target: `literal://${prop.minCount}^^xsd:integer`
        });
      }
      
      if (prop.maxCount !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://maxCount",
          target: `literal://${prop.maxCount}^^xsd:integer`
        });
      }
      
      if (prop.pattern) {
        links.push({
          source: propShapeId,
          predicate: "sh://pattern",
          target: `literal://${prop.pattern}`
        });
      }
      
      if (prop.minInclusive !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://minInclusive",
          target: `literal://${prop.minInclusive}`
        });
      }
      
      if (prop.maxInclusive !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "sh://maxInclusive",
          target: `literal://${prop.maxInclusive}`
        });
      }
      
      if (prop.hasValue) {
        links.push({
          source: propShapeId,
          predicate: "sh://hasValue",
          target: `literal://${prop.hasValue}`
        });
      }
      
      // AD4M-specific metadata
      if (prop.local !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://local",
          target: `literal://${prop.local}`
        });
      }
      
      if (prop.writable !== undefined) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://writable",
          target: `literal://${prop.writable}`
        });
      }

      if (prop.resolveLanguage) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://resolveLanguage",
          target: `literal://string:${prop.resolveLanguage}`
        });
      }

      // AD4M-specific actions
      if (prop.setter && prop.setter.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://setter",
          target: `literal://string:${JSON.stringify(prop.setter)}`
        });
      }

      if (prop.adder && prop.adder.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://adder",
          target: `literal://string:${JSON.stringify(prop.adder)}`
        });
      }

      if (prop.remover && prop.remover.length > 0) {
        links.push({
          source: propShapeId,
          predicate: "ad4m://remover",
          target: `literal://string:${JSON.stringify(prop.remover)}`
        });
      }
    }

    return links;
  }
  
  /**
   * Reconstruct shape from AD4M Links
   */
  static fromLinks(links: Link[], shapeUri: string): SHACLShape {
    // Find target class
    const targetClassLink = links.find(l => 
      l.source === shapeUri && l.predicate === "sh://targetClass"
    );
    
    const shape = new SHACLShape(shapeUri, targetClassLink?.target);

    // Find constructor actions
    const constructorLink = links.find(l =>
      l.source === shapeUri && l.predicate === "ad4m://constructor"
    );
    if (constructorLink) {
      try {
        const jsonStr = constructorLink.target.replace('literal://string:', '');
        shape.constructor_actions = JSON.parse(jsonStr);
      } catch (e) {
        // Ignore parse errors
      }
    }

    // Find destructor actions
    const destructorLink = links.find(l =>
      l.source === shapeUri && l.predicate === "ad4m://destructor"
    );
    if (destructorLink) {
      try {
        const jsonStr = destructorLink.target.replace('literal://string:', '');
        shape.destructor_actions = JSON.parse(jsonStr);
      } catch (e) {
        // Ignore parse errors
      }
    }

    // Find all property shapes
    const propShapeLinks = links.filter(l => 
      l.source === shapeUri && l.predicate === "sh://property"
    );
    
    for (const propLink of propShapeLinks) {
      const propShapeId = propLink.target;
      
      // Reconstruct property from its links
      const pathLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://path"
      );
      
      if (!pathLink) continue;
      
      // Extract property name from propShapeId if it's a named URI
      // Format: {namespace}{ClassName}.{propertyName}
      let propertyName: string | undefined;
      if (!propShapeId.startsWith('_:')) {
        const lastDotIndex = propShapeId.lastIndexOf('.');
        if (lastDotIndex !== -1) {
          propertyName = propShapeId.substring(lastDotIndex + 1);
        }
      }
      
      const prop: SHACLPropertyShape = {
        name: propertyName,
        path: pathLink.target
      };
      
      // Extract constraints
      const datatypeLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://datatype"
      );
      if (datatypeLink) prop.datatype = datatypeLink.target;
      
      const nodeKindLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://nodeKind"
      );
      if (nodeKindLink) {
        prop.nodeKind = nodeKindLink.target.replace('sh://', '') as any;
      }
      
      const minCountLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://minCount"
      );
      if (minCountLink) {
        // Handle both formats: literal://5^^xsd:integer and literal://number:5
        let val = minCountLink.target.replace('literal://', '').replace(/\^\^.*$/, '');
        if (val.startsWith('number:')) val = val.substring(7);
        prop.minCount = parseInt(val);
      }
      
      const maxCountLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://maxCount"
      );
      if (maxCountLink) {
        // Handle both formats: literal://5^^xsd:integer and literal://number:5
        let val = maxCountLink.target.replace('literal://', '').replace(/\^\^.*$/, '');
        if (val.startsWith('number:')) val = val.substring(7);
        prop.maxCount = parseInt(val);
      }
      
      const patternLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://pattern"
      );
      if (patternLink) {
        prop.pattern = patternLink.target.replace('literal://', '');
      }
      
      const minInclusiveLink = links.find(l =>
        l.source === propShapeId && l.predicate === "sh://minInclusive"
      );
      if (minInclusiveLink) {
        // Handle both formats: literal://5 and literal://number:5
        let val = minInclusiveLink.target.replace('literal://', '');
        if (val.startsWith('number:')) val = val.substring(7);
        prop.minInclusive = parseFloat(val);
      }

      const maxInclusiveLink = links.find(l =>
        l.source === propShapeId && l.predicate === "sh://maxInclusive"
      );
      if (maxInclusiveLink) {
        // Handle both formats: literal://5 and literal://number:5
        let val = maxInclusiveLink.target.replace('literal://', '');
        if (val.startsWith('number:')) val = val.substring(7);
        prop.maxInclusive = parseFloat(val);
      }
      
      const hasValueLink = links.find(l => 
        l.source === propShapeId && l.predicate === "sh://hasValue"
      );
      if (hasValueLink) {
        prop.hasValue = hasValueLink.target.replace('literal://', '');
      }
      
      // AD4M-specific
      const localLink = links.find(l => 
        l.source === propShapeId && l.predicate === "ad4m://local"
      );
      if (localLink) {
        // Handle both formats: literal://true and literal://boolean:true
        let val = localLink.target.replace('literal://', '');
        if (val.startsWith('boolean:')) val = val.substring(8);
        prop.local = val === 'true';
      }
      
      const writableLink = links.find(l =>
        l.source === propShapeId && l.predicate === "ad4m://writable"
      );
      if (writableLink) {
        // Handle both formats: literal://true and literal://boolean:true
        let val = writableLink.target.replace('literal://', '');
        if (val.startsWith('boolean:')) val = val.substring(8);
        prop.writable = val === 'true';
      }

      const resolveLangLink = links.find(l =>
        l.source === propShapeId && l.predicate === "ad4m://resolveLanguage"
      );
      if (resolveLangLink) {
        prop.resolveLanguage = resolveLangLink.target.replace('literal://string:', '');
      }

      // Parse action arrays
      const setterLink = links.find(l =>
        l.source === propShapeId && l.predicate === "ad4m://setter"
      );
      if (setterLink) {
        try {
          const jsonStr = setterLink.target.replace('literal://string:', '');
          prop.setter = JSON.parse(jsonStr);
        } catch (e) {
          // Ignore parse errors
        }
      }

      const adderLink = links.find(l =>
        l.source === propShapeId && l.predicate === "ad4m://adder"
      );
      if (adderLink) {
        try {
          const jsonStr = adderLink.target.replace('literal://string:', '');
          prop.adder = JSON.parse(jsonStr);
        } catch (e) {
          // Ignore parse errors
        }
      }

      const removerLink = links.find(l =>
        l.source === propShapeId && l.predicate === "ad4m://remover"
      );
      if (removerLink) {
        try {
          const jsonStr = removerLink.target.replace('literal://string:', '');
          prop.remover = JSON.parse(jsonStr);
        } catch (e) {
          // Ignore parse errors
        }
      }

      shape.addProperty(prop);
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
      });
    }
    
    if (json.constructor_actions) {
      shape.constructor_actions = json.constructor_actions;
    }
    if (json.destructor_actions) {
      shape.destructor_actions = json.destructor_actions;
    }
    
    return shape;
  }
}
