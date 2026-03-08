import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { LinkQuery } from "../perspectives/LinkQuery";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomId, PropertyOptions, Model, getPropertiesMetadata, getRelationsMetadata, setPropertyRegistryEntry, setRelationRegistryEntry, PropertyMetadataEntry, RelationMetadataEntry } from "./decorators";
import { singularToPlural, pluralToSingular, propertyNameToSetterName, collectionToAdderName, collectionToRemoverName, collectionToSetterName } from "./util";
import { escapeSurrealString } from "../utils";

// JSON Schema type definitions
interface JSONSchemaProperty {
  type: string | string[];
  items?: JSONSchemaProperty;
  properties?: { [key: string]: JSONSchemaProperty };
  required?: string[];
  "x-ad4m"?: {
    through?: string;
    resolveLanguage?: string;
    local?: boolean;
    writable?: boolean;
    initial?: string;
  };
}

interface JSONSchema {
  $schema?: string;
  title?: string;
  $id?: string;
  type?: string;
  properties?: { [key: string]: JSONSchemaProperty };
  required?: string[];
  "x-ad4m"?: {
    namespace?: string;
    className?: string;
  };
}

interface JSONSchemaToModelOptions {
  name: string;
  namespace?: string;
  predicateTemplate?: string;
  predicateGenerator?: (title: string, property: string) => string;
  propertyMapping?: Record<string, string>;
  resolveLanguage?: string;
  local?: boolean;
  propertyOptions?: Record<string, Partial<PropertyOptions>>;
}

type ValueTuple = [name: string, value: any, resolve?: boolean];
type WhereOps = {
  not: string | number | boolean | string[] | number[];
  between: [number, number];
  lt: number; // less than
  lte: number; // less than or equal to
  gt: number; // greater than
  gte: number; // greater than or equal to
  contains: string | number; // substring/element check
};
type WhereCondition = string | number | boolean | string[] | number[] | { [K in keyof WhereOps]?: WhereOps[K] };
type Where = { [propertyName: string]: WhereCondition };
type Order = { [propertyName: string]: "ASC" | "DESC" };

/**
 * Discriminated union for parent-scoped queries.
 *
 * **Model form** (preferred) — predicate auto-resolved from the parent model's
 * relation metadata. Use `field` to disambiguate when the parent has multiple
 * relations targeting the same child class.
 *
 * **Raw form** — explicit predicate string, no metadata lookup.
 */
export type ParentScope =
  | { model: typeof Ad4mModel; id: string; field?: string }
  | { id: string; predicate: string };

/**
 * Describes which relations to eager-load when querying.
 *
 * Each value is either:
 * - `true` — hydrate the relation one level deep
 * - A `RelationSubQuery` — scoped sub-query (filter / sort / paginate / nested include)
 *
 * @example
 * ```typescript
 * // One level deep
 * { comments: true }
 *
 * // Sub-query: only the 5 most-recent comments
 * { comments: { order: { createdAt: 'DESC' }, limit: 5 } }
 *
 * // Nested eager-load
 * { comments: { include: { author: true } } }
 * ```
 */
export interface IncludeMap {
  [relation: string]: boolean | RelationSubQuery;
}

export type Query = {
  /** Filter to instances that are the target of a link from a given parent. */
  parent?: ParentScope;
  properties?: string[];
  include?: IncludeMap;
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
};

/**
 * Sub-query options for a specific relation inside an `IncludeMap`.
 *
 * Equivalent to `Query` without top-level scoping (`parent`) or `count`,
 * since the result set is already constrained to the linked relation.
 *
 * @example
 * ```typescript
 * await post.get({ include: { comments: { order: { createdAt: 'DESC' }, limit: 5 } } });
 * ```
 */
export type RelationSubQuery = Omit<Query, 'parent' | 'count'>;

/**
 * Options accepted by the instance `get()` method.
 *
 * A subset of `Query` — only hydration controls apply to a single known instance.
 */
export type GetOptions = Pick<Query, 'include' | 'properties'>;

export type AllInstancesResult = { AllInstances: Ad4mModel[]; TotalCount?: number; isInit?: boolean };
export type ResultsWithTotalCount<T> = { results: T[]; totalCount?: number };
export type PaginationResult<T> = { results: T[]; totalCount?: number; pageSize: number; pageNumber: number };

/**
 * Metadata for a single property extracted from decorators.
 */
export interface PropertyMetadata {
  /** The property name */
  name: string;
  /** The predicate URI (through value) */
  predicate: string;
  /** Whether the property is required */
  required: boolean;
  /** Whether the property is read-only */
  readOnly: boolean;
  /** Initial value if specified */
  initial?: string;
  /** Language for resolution (e.g., "literal") */
  resolveLanguage?: string;
  /** Custom Prolog getter code */
  prologGetter?: string;
  /** Custom Prolog setter code */
  prologSetter?: string;
  /** Custom SurrealQL getter code */
  getter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Transform function */
  transform?: (value: any) => any;
  /** Whether this is a flag property */
  flag?: boolean;
}

/**
 * Metadata for a single relation (collection) extracted from decorators.
 */
export interface RelationMetadata {
  /** The relation name */
  name: string;
  /** The predicate URI (through value) */
  predicate: string;
  /** Custom SurrealQL getter code */
  getter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Link direction: 'forward' for HasMany/HasOne, 'reverse' for BelongsToMany/BelongsToOne */
  direction?: 'forward' | 'reverse';
}

/** @deprecated Use RelationMetadata */
export type CollectionMetadata = RelationMetadata;

/**
 * Complete model metadata extracted from decorators.
 */
export interface ModelMetadata {
  /** The model class name from @Model */
  className: string;
  /** Map of property name to metadata */
  properties: Record<string, PropertyMetadata>;
  /** Map of relation name to metadata */
  relations: Record<string, RelationMetadata>;
  /** @deprecated Use relations */
  collections: Record<string, RelationMetadata>;
}

function capitalize(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

/**
 * Resolves the predicate for a parent query.
 *
 * Uses TS discriminated union narrowing:
 * - Raw form (`{ id, predicate }`) → predicate used as-is
 * - Model form (`{ model, id, field? }`) → lookup from relation metadata
 *   - With `field`: direct key lookup
 *   - Without `field`: scan for a relation whose `target()` matches `childCtor`
 */
function resolveParentPredicate(
  parent: ParentScope,
  childCtor: Function,
): string {
  // Raw form — explicit predicate
  if ('predicate' in parent) return parent.predicate;

  // Model form — resolve from relation metadata
  const { model } = parent;
  const relMeta = getRelationsMetadata(model);

  // Direct lookup by field name when provided
  if (parent.field) {
    const entry = relMeta[parent.field];
    if (!entry) {
      throw new Error(
        `parent(): field "${parent.field}" is not a registered relation on ${model.name}`,
      );
    }
    return entry.predicate;
  }

  // Fallback: scan for a relation whose target matches the child class
  for (const [, entry] of Object.entries(relMeta)) {
    if (entry.target && entry.target() === childCtor) {
      return entry.predicate;
    }
  }
  throw new Error(
    `parent(): could not resolve predicate — no relation on ${model.name} targets ${(childCtor as any).name || 'the queried class'}`,
  );
}

function buildParentQuery(
  parent: ParentScope | undefined,
  resolvedPredicate?: string,
): string {
  if (!parent || !resolvedPredicate) return '';
  return `triple("${parent.id}", "${resolvedPredicate}", Base)`;
}

// todo: only return Timestamp & Author from query (Base, AllLinks, and SortLinks not required)
function buildAuthorAndTimestampQuery(): string {
  // Gets the author and timestamp of a Ad4mModel instance (based on the first link mentioning the base)
  return `
    findall(
      [T, A],
      link(Base, _, _, T, A),
      AllLinks
    ),
    sort(AllLinks, SortedLinks),
    SortedLinks = [[Timestamp, Author]|_]
  `;
}

function buildPropertiesQuery(properties?: string[]): string {
  // Gets the name, value, and resolve boolean for all (or some) properties on a Ad4mModel instance
  // Resolves literals (if property_resolve/2 is true) to their value - either the data field if it is
  // an Expression in JSON literal, or the direct literal value if it is a simple literal
  // If no properties are provided, all are included
  return `
    findall([PropertyName, PropertyValue, Resolve], (
      % Constrain to specified properties if provided
      ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}
      resolve_property(SubjectClass, Base, PropertyName, PropertyValue, Resolve)
    ), Properties)
  `;
}

function buildWhereQuery(where: Where = {}): string {
  // Constrains the query to instances that match the provided where conditions
  // 'id' maps to the Prolog 'Base' variable (the base expression of the instance).

  function formatValue(value) {
    // Wrap strings in quotes
    return typeof value === "string" ? `"${value}"` : value;
  }

  return (Object.entries(where) as [string, WhereCondition][])
    .map(([key, value]) => {
      const isSpecial = ["id", "author", "timestamp"].includes(key);
      const getter = `resolve_property(SubjectClass, Base, "${key}", Value${key}, _)`;
      // For 'id' the Prolog variable is always 'Base'
      const field = key === "id" ? "Base" : capitalize(key);

      // Handle direct array values (for IN conditions)
      if (Array.isArray(value)) {
        const formattedValues = value.map((v) => formatValue(v)).join(", ");
        if (isSpecial) return `member(${field}, [${formattedValues}])`;
        else return `${getter}, member(Value${key}, [${formattedValues}])`;
      }

      // Handle operation object
      if (typeof value === "object" && value !== null) {
        const { not, between, lt, lte, gt, gte } = value;

        // Handle NOT operation
        if (not !== undefined) {
          if (Array.isArray(not)) {
            // NOT IN array
            const formattedValues = not.map((v) => formatValue(v)).join(", ");
            if (isSpecial) return `\\+ member(${field}, [${formattedValues}])`;
            else return `${getter}, \\+ member(Value${key}, [${formattedValues}])`;
          } else {
            // NOT EQUAL
            if (isSpecial) return `${field} \\= ${formatValue(not)}`;
            else return `${getter}, Value${key} \\= ${formatValue(not)}`;
          }
        }

        // Handle BETWEEN
        if (between !== undefined && Array.isArray(between) && between.length === 2) {
          if (isSpecial) return `${field} >= ${between[0]}, ${field} =< ${between[1]}`;
          else return `${getter}, Value${key} >= ${between[0]}, Value${key} =< ${between[1]}`;
        }

        // Handle lt, lte, gt, & gte operations
        const operators = [
          { value: lt, symbol: "<" }, // LESS THAN
          { value: lte, symbol: "=<" }, // LESS THAN OR EQUAL TO
          { value: gt, symbol: ">" }, // GREATER THAN
          { value: gte, symbol: ">=" }, // GREATER THAN OR EQUAL TO
        ];

        for (const { value, symbol } of operators) {
          if (value !== undefined)
            return isSpecial ? `${field} ${symbol} ${value}` : `${getter}, Value${key} ${symbol} ${value}`;
        }
      }

      // Default to direct equality
      if (isSpecial) return `${field} = ${formatValue(value)}`;
      else return `${getter}, Value${key} = ${formatValue(value)}`;
    })
    .join(", ");
}

function buildCountQuery(count?: boolean): string {
  return count ? "length(UnsortedInstances, TotalCount)" : "";
}

function buildOrderQuery(order?: Order): string {
  if (!order) return "SortedInstances = UnsortedInstances";
  const entries = Object.entries(order);
  if (entries.length === 1) {
    const [propertyName, direction] = entries[0];
    return `sort_instances(UnsortedInstances, "${propertyName}", "${direction}", SortedInstances)`;
  }
  // Multi-field sort: sort from least-significant to most-significant key
  // so that the final (primary) sort preserves secondary-key ordering for equal values.
  // The merge_sort implementation is stable (equal elements keep original order).
  const clauses: string[] = [];
  for (let i = entries.length - 1; i >= 0; i--) {
    const [propertyName, direction] = entries[i];
    const inputVar = i === entries.length - 1 ? "UnsortedInstances" : `MultiSortIntermediate${i + 1}`;
    const outputVar = i === 0 ? "SortedInstances" : `MultiSortIntermediate${i}`;
    clauses.push(`sort_instances(${inputVar}, "${propertyName}", "${direction}", ${outputVar})`);
  }
  return clauses.join(",\n      ");
}

function buildOffsetQuery(offset?: number): string {
  if (!offset || offset < 0) return "InstancesWithOffset = SortedInstances";
  return `skipN(SortedInstances, ${offset}, InstancesWithOffset)`;
}

function buildLimitQuery(limit?: number): string {
  if (!limit || limit < 0) return "AllInstances = InstancesWithOffset";
  return `takeN(InstancesWithOffset, ${limit}, AllInstances)`;
}

function normalizeNamespaceString(namespace: string): string {
  if (!namespace) return '';
  if (namespace.includes('://')) {
    const [scheme, rest] = namespace.split('://');
    const path = (rest || '').replace(/\/+$/,'');
    return `${scheme}://${path}`;
  } else {
    return namespace.replace(/\/+$/,'');
  }
}

function normalizeSchemaType(type?: string | string[]): string | undefined {
  if (!type) return undefined;
  if (typeof type === "string") return type;
  if (Array.isArray(type) && type.length > 0) {
    const nonNull = type.find((t) => t !== "null");
    return nonNull || type[0];
  }
  return undefined;
}

function isSchemaType(schema: JSONSchemaProperty, expectedType: string): boolean {
  return normalizeSchemaType(schema.type) === expectedType;
}

function isArrayType(schema: JSONSchemaProperty): boolean {
  return isSchemaType(schema, "array");
}

function isObjectType(schema: JSONSchemaProperty): boolean {
  return isSchemaType(schema, "object");
}

function isNumericType(schema: JSONSchemaProperty): boolean {
  const normalized = normalizeSchemaType(schema.type);
  return normalized === "number" || normalized === "integer";
}

/**
 * Base class for defining data models in AD4M.
 * 
 * @description
 * Ad4mModel provides the foundation for creating data models that are stored in AD4M perspectives.
 * Each model instance is represented as a subgraph in the perspective, with properties and collections
 * mapped to links in that graph. The class uses Prolog-based queries to efficiently search and filter
 * instances based on their properties and relationships.
 * 
 * Key concepts:
 * - Each model instance has a unique base expression that serves as its identifier
 * - Properties are stored as links with predicates defined by the `through` option
 * - Collections represent one-to-many relationships as sets of links
 * - Queries are translated to Prolog for efficient graph pattern matching
 * - Changes are tracked through the perspective's subscription system
 * 
 * @example
 * ```typescript
 * // Define a recipe model
 * @Model({ name: "Recipe" })
 * class Recipe extends Ad4mModel {
 *   // Required property with literal value
 *   @Property({
 *     through: "recipe://name",
 *     resolveLanguage: "literal"
 *   })
 *   name: string = "";
 * 
 *   // Optional property with custom initial value
 *   @Optional({
 *     through: "recipe://status",
 *     initial: "recipe://draft"
 *   })
 *   status: string = "";
 * 
 *   // Read-only computed property
 *   @ReadOnly({
 *     through: "recipe://rating",
 *     getter: `
 *       findall(Rating, triple(Base, "recipe://user_rating", Rating), Ratings),
 *       sum_list(Ratings, Sum),
 *       length(Ratings, Count),
 *       Value is Sum / Count
 *     `
 *   })
 *   averageRating: number = 0;
 * 
 *   // Collection of ingredients
 *   @Collection({ through: "recipe://ingredient" })
 *   ingredients: string[] = [];
 * 
 *   // Collection of comments linked to another model
 *   @HasMany(() => Comment, { through: "recipe://comment" })
 *   comments: Comment[] = [];
 * }
 * 
 * // Create and save a new recipe
 * const recipe = new Recipe(perspective);
 * recipe.name = "Chocolate Cake";
 * recipe.ingredients = ["flour", "sugar", "cocoa"];
 * await recipe.save();
 * 
 * // Query recipes in different ways
 * // Get all recipes
 * const allRecipes = await Recipe.findAll(perspective);
 * 
 * // Find recipes with specific criteria
 * const desserts = await Recipe.findAll(perspective, {
 *   where: { 
 *     status: "recipe://published",
 *     averageRating: { gt: 4 }
 *   },
 *   order: { name: "ASC" },
 *   limit: 10
 * });
 * 
 * // Use the fluent query builder
 * const popularRecipes = await Recipe.query(perspective)
 *   .where({ averageRating: { gt: 4.5 } })
 *   .order({ averageRating: "DESC" })
 *   .limit(5)
 *   .get();
 * 
 * // Subscribe to real-time updates
 * await Recipe.query(perspective)
 *   .where({ status: "recipe://cooking" })
 *   .subscribe(recipes => {
 *     console.log("Currently being cooked:", recipes);
 *   });
 * 
 * // Paginate results
 * const { results, totalCount, pageNumber } = await Recipe.query(perspective)
 *   .where({ status: "recipe://published" })
 *   .paginate(10, 1);
 * ```
 */
export class Ad4mModel {
  private _baseExpression: string;
  private _subjectClassName: string;
  private _perspective: PerspectiveProxy;
  private _snapshot: Record<string, any> | null = null;
  author: string;
  createdAt: any;
  updatedAt: any;

  private static classNamesByClass = new WeakMap<typeof Ad4mModel, { [perspectiveId: string]: string }>();

  /**
   * Generates the SDNA (Subject DNA) Prolog rules for this model class.
   * Injected at class-definition time by the `@Model` decorator.
   * Returns an empty string on un-decorated base classes.
   */
  static generateSDNA(): string {
    return '';
  }

  /**
   * Generates the SHACL shape graph for this model class.
   * Injected at class-definition time by the `@Model` decorator.
   * Returns `{ shape: null, name: '' }` on un-decorated base classes —
   * the decorator's `parentSHACL?.shape?.nodeShapeUri` check handles this.
   */
  static generateSHACL(): { shape: any; name: string } {
    return { shape: null, name: '' };
  }

  static async getClassName(perspective: PerspectiveProxy) {
      // Check if this is the Ad4mModel class itself or a subclass
      const isBaseClass = this === Ad4mModel;
  
      // For the base Ad4mModel class, we can't use the cache
      if (isBaseClass) {
        return await perspective.stringOrTemplateObjectToSubjectClassName(this);
      }

    // Get or create the cache for this class
    let classCache = this.classNamesByClass.get(this);
    if (!classCache) {
      classCache = {};
      this.classNamesByClass.set(this, classCache);
    }

    // Get or create the cached name for this perspective
    const perspectiveID = perspective.uuid;
    if (!classCache[perspectiveID]) {
      classCache[perspectiveID] = await perspective.stringOrTemplateObjectToSubjectClassName(this);
    }

    return classCache[perspectiveID];
  }

  /**
   * Backwards compatibility alias for createdAt.
   * @deprecated Use createdAt instead. This will be removed in a future version.
   */
  get timestamp(): any {
    return (this as any).createdAt;
  }

  /**
   * Extracts metadata from decorators for query building.
   * 
   * @description
   * This method reads the metadata stored by decorators (@Property, @Collection, etc.)
   * and returns it in a structured format that's easier to work with for query builders
   * and other systems that need to introspect model structure.
   * 
   * The metadata includes:
   * - Class name from @Model
   * - Property metadata (predicates, types, constraints, etc.)
   * - Collection metadata (predicates, filters, etc.)
   * 
   * For models created via `fromJSONSchema()`, this method will derive metadata from
   * the WeakMap registries that were populated during the dynamic class creation.
   * If these structures are empty but a JSON schema was attached to the class,
   * it can fall back to deriving metadata from that schema.
   * 
   * @returns Structured metadata object containing className, properties, and collections
   * @throws Error if the class doesn't have @Model decorator
   * 
   * @example
   * ```typescript
   * @Model({ name: "Recipe" })
   * class Recipe extends Ad4mModel {
   *   @Property({ through: "recipe://name", resolveLanguage: "literal" })
   *   name: string = "";
   *   
   *   @Collection({ through: "recipe://ingredient" })
   *   ingredients: string[] = [];
   * }
   * 
   * const metadata = Recipe.getModelMetadata();
   * console.log(metadata.className); // "Recipe"
   * console.log(metadata.properties.name.predicate); // "recipe://name"
   * console.log(metadata.collections.ingredients.predicate); // "recipe://ingredient"
   * ```
   */
  public static getModelMetadata(): ModelMetadata {
    // Access the prototype with any type to access decorator-added properties
    const prototype = this.prototype as any;
    
    // Validate that the class has @Model decorator
    // The decorator sets prototype.className, so we check for its existence
    if (!prototype.className || prototype.className === 'Ad4mModel') {
      throw new Error("Model class must be decorated with @Model");
    }
    
    // Extract className
    const className = prototype.className;
    
    // Extract properties from WeakMap registry
    const propertiesMetadata: Record<string, PropertyMetadata> = {};
    const prototypeProperties = getPropertiesMetadata(this);
    
    for (const [propertyName, opts] of Object.entries(prototypeProperties)) {
      const options = opts as PropertyOptions & { required?: boolean; flag?: boolean; writable?: boolean };
      propertiesMetadata[propertyName] = {
        name: propertyName,
        predicate: options.through || "",
        required: options.required || false,
        readOnly: !(options.writable ?? false),
        ...(options.initial !== undefined && { initial: options.initial }),
        ...(options.resolveLanguage !== undefined && { resolveLanguage: options.resolveLanguage }),
        ...(options.prologGetter !== undefined && { prologGetter: options.prologGetter }),
        ...(options.getter !== undefined && { getter: options.getter }),
        ...(options.prologSetter !== undefined && { prologSetter: options.prologSetter }),
        ...(options.local !== undefined && { local: options.local }),
        ...(options.transform !== undefined && { transform: options.transform }),
        ...(options.flag !== undefined && { flag: options.flag })
      };
    }
    
    // Extract relations (collections) from WeakMap registry
    const relationsMetadata: Record<string, RelationMetadata> = {};
    const allRelationsMeta = getRelationsMetadata(this as any);
    const prototypeCollections = Object.fromEntries(
      Object.entries(allRelationsMeta).filter(([, r]) => r.kind === 'hasMany' || r.kind === 'belongsToMany')
    );
    
    for (const [relationName, opts] of Object.entries(prototypeCollections)) {
      const options = opts as RelationMetadataEntry;
      relationsMetadata[relationName] = {
        name: relationName,
        predicate: options.predicate || "",
        ...(options.local !== undefined && { local: options.local }),
        ...(options.getter !== undefined && { getter: options.getter }),
        direction: (options.kind === 'belongsToMany' || options.kind === 'belongsToOne') ? 'reverse' : 'forward',
      };
    }
    
    // Fallback: If both structures are empty but a JSON schema is attached, derive from it
    // This handles edge cases where fromJSONSchema() was called but metadata wasn't properly populated
    const hasProperties = Object.keys(propertiesMetadata).length > 0;
    const hasRelations = Object.keys(relationsMetadata).length > 0;
    const hasMetadata = hasProperties || hasRelations;
    
    if (!hasMetadata && prototype.__jsonSchema) {
      // Derive metadata from the attached JSON schema
      const schema = prototype.__jsonSchema;
      const options = prototype.__jsonSchemaOptions || {};
      
      if (schema.properties) {
        for (const [propertyName, propertySchema] of Object.entries(schema.properties)) {
          const isArray = isArrayType(propertySchema as JSONSchemaProperty);
          const predicate = this.determinePredicate(
            schema, 
            propertyName, 
            propertySchema as JSONSchemaProperty, 
            this.determineNamespace(schema, options),
            options
          );
          
          if (isArray) {
            relationsMetadata[propertyName] = {
              name: propertyName,
              predicate: predicate,
              ...(propertySchema["x-ad4m"]?.local !== undefined && { local: propertySchema["x-ad4m"].local })
            };
          } else {
            const isRequired = schema.required?.includes(propertyName) || false;
            propertiesMetadata[propertyName] = {
              name: propertyName,
              predicate: predicate,
              required: isRequired,
              readOnly: propertySchema["x-ad4m"]?.writable === false,
              ...(propertySchema["x-ad4m"]?.resolveLanguage && { resolveLanguage: propertySchema["x-ad4m"].resolveLanguage }),
              ...(propertySchema["x-ad4m"]?.initial && { initial: propertySchema["x-ad4m"].initial }),
              ...(propertySchema["x-ad4m"]?.local !== undefined && { local: propertySchema["x-ad4m"].local })
            };
          }
        }
      }
    }
    
    return {
      className,
      properties: propertiesMetadata,
      relations: relationsMetadata,
      collections: relationsMetadata,
    };
  }

  /**
   * Constructs a new model instance.
   * 
   * @param perspective - The perspective where this model will be stored
   * @param baseExpression - Optional expression URI for this instance.
   *             If omitted, a random Literal URL is generated.
   * @param source - Optional source expression this instance is linked to
   * 
   * @example
   * ```typescript
   * // Create a new recipe with auto-generated base expression
   * const recipe = new Recipe(perspective);
   * 
   * // Create with specific base expression
   * const recipe = new Recipe(perspective, "literal://...");
   * ```
   */
  constructor(perspective: PerspectiveProxy, baseExpression?: string) {
    this._baseExpression = baseExpression ? baseExpression : Literal.from(makeRandomId(24)).toUrl();
    this._perspective = perspective;
  }

  /**
   * The unique identifier (expression URI) of this model instance.
   */
  get id(): string {
    return this._baseExpression;
  }

  /**
   * @deprecated Use `.id` instead. Will be removed in a future version.
   */
  get baseExpression(): string {
    return this._baseExpression;
  }

  /**
   * Protected getter for the perspective.
   * Allows subclasses to access the perspective while keeping it private from external code.
   */
  protected get perspective(): PerspectiveProxy {
    return this._perspective;
  }

  /**
   * Get property metadata from decorator (Phase 1: Prolog-free refactor)
   * @private
   */
  private getPropertyMetadata(key: string): PropertyMetadataEntry | undefined {
    const ctor = this.constructor;
    const props = getPropertiesMetadata(ctor);
    return props[key];
  }

  /**
   * Get relation (collection) options from decorator
   * @private
   */
  private getRelationOptions(key: string): RelationMetadataEntry | undefined {
    const ctor = this.constructor;
    const rels = getRelationsMetadata(ctor);
    return rels[key];
  }

  /**
   * Generate property setter action from metadata (Phase 1: Prolog-free refactor)
   * Replaces Prolog query: property_setter(C, key, Setter)
   * @private
   */
  private generatePropertySetterAction(key: string, metadata: PropertyMetadataEntry): any[] {
    // Flags are always immutable — throw a clear error
    if (metadata.flag) {
      throw new Error(
        `Property "${key}" is a @Flag and cannot be written. ` +
        `Flags are immutable type markers set at creation time.`
      );
    }

    // Check if property is read-only
    if (metadata.readOnly) {
      throw new Error(`Property "${key}" is read-only and cannot be written`);
    }

    if (metadata.prologSetter) {
      // Custom Prolog setter - throw error for now (Phase 2)
      throw new Error(
        `Custom Prolog setter for property "${key}" not yet supported without Prolog. ` +
        `Use standard @Property decorator or enable Prolog for custom setters.`
      );
    }

    if (!metadata.through) {
      throw new Error(`Property "${key}" has no 'through' predicate defined`);
    }

    return [{
      action: "setSingleTarget",
      source: "this",
      predicate: metadata.through,
      target: "value",
      ...(metadata.local && { local: true })
    }];
  }

  /**
   * Generate collection action from metadata (Phase 1: Prolog-free refactor)
   * Replaces Prolog queries: collection_adder, collection_remover, collection_setter
   * @private
   */
  private generateRelationAction(key: string, actionType: 'adder' | 'remover' | 'setter'): any[] {
    const metadata = this.getRelationOptions(key);
    if (!metadata) {
      throw new Error(`Relation "${key}" has no metadata defined`);
    }

    if (!metadata.predicate) {
      throw new Error(`Relation "${key}" has no predicate defined`);
    }

    const actionMap = {
      adder: "addLink",
      remover: "removeLink",
      setter: "collectionSetter"
    };

    return [{
      action: actionMap[actionType],
      source: "this",
      predicate: metadata.predicate,
      target: "value",
      ...(metadata.local && { local: true })
    }];
  }

  // ──────────────────────────────────────────────────────────
  //  Unified hydration helpers
  // ──────────────────────────────────────────────────────────

  /**
   * Resolves a raw link target value into a hydrated property value.
   *
   * Handles, in order:
   * 1. Non-literal expression resolution (`perspective.getExpression`)
   * 2. Literal URI parsing (`Literal.fromUrl(…).get()`)
   * 3. Primitive type coercion (string → number / boolean)
   * 4. Transform function application
   *
   * @param target        - The raw target string from the link
   * @param propMeta      - Property metadata from the decorator registry
   * @param perspective   - The perspective for expression resolution
   * @param expectedType  - Optional JS typeof hint for coercion (e.g. 'number')
   * @returns The resolved value
   * @private
   */
  private static async hydratePropertyValue(
    target: string,
    propMeta: PropertyMetadata,
    perspective: PerspectiveProxy,
    expectedType?: string,
  ): Promise<any> {
    let value: any = target;

    if (target !== undefined && target !== null && target !== '') {
      // Non-literal expression resolution
      if (
        propMeta.resolveLanguage != null &&
        propMeta.resolveLanguage !== 'literal' &&
        typeof target === 'string' &&
        !target.startsWith('literal://')
      ) {
        try {
          const expression = await perspective.getExpression(target);
          if (expression) {
            try { value = JSON.parse(expression.data); } catch { value = expression.data; }
          }
        } catch (e) {
          console.warn(`hydratePropertyValue: failed to resolve expression for "${propMeta.name}":`, e);
        }
      }
      // Literal URI parsing
      else if (
        propMeta.resolveLanguage === 'literal' &&
        typeof target === 'string' &&
        target.startsWith('literal://')
      ) {
        try {
          const parsed = Literal.fromUrl(target).get();
          value = parsed.data !== undefined ? parsed.data : parsed;
        } catch {
          // Keep raw value
        }
      }
      // Primitive type coercion
      else if (typeof target === 'string' && expectedType) {
        if (expectedType === 'number') value = Number(target);
        else if (expectedType === 'boolean') value = target === 'true' || target === '1';
      }
    }

    // Transform function
    if (propMeta.transform && typeof propMeta.transform === 'function') {
      value = propMeta.transform(value);
    }

    return value;
  }

  /**
   * Link shape accepted by `hydrateFromLinks`.
   * Both `getData()` and `instancesFromSurrealResult()` produce this shape.
   */
  private static readonly _linkShape: undefined;  // type-only marker

  /**
   * Hydrates an instance from an array of raw links.
   *
   * Processes properties (latest-wins semantics), collections
   * (chronological accumulation), and timestamps/author in a single
   * pass over the links array.
   *
   * @param instance     - The blank model instance to populate
   * @param links        - Array of link objects (predicate, target, author?, timestamp?)
   * @param metadata     - Model metadata from `getModelMetadata()`
   * @param perspective  - The perspective for expression resolution
   * @param requestedProperties - Optional sparse fieldset; when provided, only these
   *                              property names are hydrated (collections are unaffected).
   *                              Omit or pass `undefined` to hydrate all properties.
   * @private
   */
  private static async hydrateFromLinks(
    instance: any,
    links: Array<{ predicate: string; target: string; author?: string; timestamp?: string | number }>,
    metadata: ModelMetadata,
    perspective: PerspectiveProxy,
    requestedProperties?: string[],
  ): Promise<void> {
    if (!links || links.length === 0) return;

    let minTimestamp: string | number | null = null;
    let maxTimestamp: string | number | null = null;
    let originalAuthor: string | null = null;
    let latestAuthor: string | null = null;

    // Build predicate→propName and predicate→collName lookup maps for O(1) matching
    // When requestedProperties is provided, only include those properties in the map
    const propFilter = requestedProperties && requestedProperties.length > 0
      ? new Set(requestedProperties)
      : null;

    const predToProperty = new Map<string, [string, PropertyMetadata]>();
    for (const [propName, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.getter) continue;  // Handled via custom getter evaluation
      if (propFilter && !propFilter.has(propName)) continue;  // Skip unrequested properties
      predToProperty.set(propMeta.predicate, [propName, propMeta]);
    }

    const predToCollection = new Map<string, [string, RelationMetadata]>();
    for (const [collName, collMeta] of Object.entries(metadata.relations)) {
      if (collMeta.getter) continue;
      predToCollection.set(collMeta.predicate, [collName, collMeta]);
    }

    // Per-property accumulator: track all matching targets so we can pick the last (latest-wins)
    const propertyLatest = new Map<string, { target: string; timestamp?: string | number }>();

    // Per-collection accumulator: ordered targets with metadata for sorting
    const collectionAccum = new Map<string, Array<{ target: string; timestamp: string | number; index: number }>>();

    // Single pass over all links
    for (let i = 0; i < links.length; i++) {
      const link = links[i];
      const { predicate, target, author, timestamp } = link;
      if (target === 'None' || target === undefined || target === null) continue;

      // Track timestamps/authors
      if (timestamp != null) {
        if (minTimestamp == null || timestamp < minTimestamp) {
          minTimestamp = timestamp;
          originalAuthor = author ?? null;
        }
        if (maxTimestamp == null || timestamp > maxTimestamp) {
          maxTimestamp = timestamp;
          latestAuthor = author ?? null;
        }
      }

      // Property match — accumulate for latest-wins
      const propEntry = predToProperty.get(predicate);
      if (propEntry) {
        const existing = propertyLatest.get(propEntry[0]);
        // Latest-wins: always overwrite (links are ordered ASC so last = latest)
        propertyLatest.set(propEntry[0], { target, timestamp });
        continue;
      }

      // Collection match — accumulate all
      const collEntry = predToCollection.get(predicate);
      if (collEntry) {
        const [collName] = collEntry;
        let arr = collectionAccum.get(collName);
        if (!arr) {
          arr = [];
          collectionAccum.set(collName, arr);
        }
        arr.push({ target, timestamp: timestamp ?? '', index: i });
      }
    }

    // Resolve properties
    for (const [propName, { target }] of propertyLatest) {
      const [, propMeta] = predToProperty.get(
        metadata.properties[propName].predicate
      )!;
      const expectedType = typeof instance[propName];
      instance[propName] = await this.hydratePropertyValue(
        target,
        propMeta,
        perspective,
        expectedType !== 'undefined' ? expectedType : undefined,
      );
    }

    // Resolve collections: sort by timestamp (stable via index tiebreaker), filter empties
    for (const [collName, items] of collectionAccum) {
      items.sort((a, b) => {
        const cmp = String(a.timestamp).localeCompare(String(b.timestamp));
        return cmp !== 0 ? cmp : a.index - b.index;
      });
      instance[collName] = items
        .map(i => i.target)
        .filter((v: any) => v !== undefined && v !== null && v !== '' && v !== 'None');
    }

    // Assign author / timestamps
    if (originalAuthor) instance.author = originalAuthor;
    if (minTimestamp != null) {
      instance.createdAt = typeof minTimestamp === 'string' && minTimestamp.includes('T')
        ? new Date(minTimestamp).getTime()
        : typeof minTimestamp === 'string'
          ? (isNaN(parseInt(minTimestamp, 10)) ? minTimestamp : parseInt(minTimestamp, 10))
          : minTimestamp;
    }
    if (maxTimestamp != null) {
      instance.updatedAt = typeof maxTimestamp === 'string' && maxTimestamp.includes('T')
        ? new Date(maxTimestamp).getTime()
        : typeof maxTimestamp === 'string'
          ? (isNaN(parseInt(maxTimestamp, 10)) ? maxTimestamp : parseInt(maxTimestamp, 10))
          : maxTimestamp;
    }
  }

  public static async assignValuesToInstance(perspective: PerspectiveProxy, instance: Ad4mModel, values: ValueTuple[]) {
    // Map properties to object
    const propsObject = Object.fromEntries(
      await Promise.all(
        values.map(async ([name, value, resolve]) => {
          let finalValue = value;

          // Handle UTF-8 byte sequences from Prolog URL decoding
          if (!resolve && typeof value === 'string') {
            // Only attempt reconstruction if the string looks like a byte string (all code points <= 0xFF)
            // and contains at least one high byte (>= 0x80). This avoids mangling valid Unicode.
            const codePoints = Array.from(value, ch => ch.codePointAt(0)!);
            const looksByteString = codePoints.every(cp => cp <= 0xFF);
            const hasHighByte = codePoints.some(cp => cp >= 0x80);
            if (looksByteString && hasHighByte) {
              try {
                const bytes = Uint8Array.from(codePoints);
                const decoded = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
                if (decoded !== value) finalValue = decoded;
              } catch (error) {
                // If UTF-8 conversion fails, keep the original value
                console.warn(`UTF-8 byte reconstruction failed for property "${name}"`, { value, error });
              }
            }
          }

          // Resolve the value if necessary
          if (resolve) {
            let resolvedExpression = await perspective.getExpression(value);
            if (resolvedExpression) {
              try {
                // Attempt to parse the data as JSON
                finalValue = JSON.parse(resolvedExpression.data);
              } catch (error) {
                // If parsing fails, keep the original data
                finalValue = resolvedExpression.data;
              }
            }
          }
          // Apply transform function if it exists
          const propsMeta = getPropertiesMetadata(instance.constructor);
          const transform = propsMeta[name]?.transform;
          if (transform && typeof transform === "function") {
            finalValue = transform(finalValue);
          }
          return [name, finalValue];
        })
      )
    );
    // Filter out properties that are read-only (getters without setters)
    const writableProps = Object.fromEntries(
      Object.entries(propsObject).filter(([key]) => {
        const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(instance), key);
        if (!descriptor) {
          // No descriptor means it's a regular property on the instance, allow it
          return true;
        }
        // Check if it's an accessor descriptor (has get/set) vs data descriptor (has value/writable)
        const isAccessor = descriptor.get !== undefined || descriptor.set !== undefined;
        if (isAccessor) {
          // Accessor descriptor: only allow if it has a setter
          return descriptor.set !== undefined;
        } else {
          // Data descriptor: only allow if writable is not explicitly false
          return descriptor.writable !== false;
        }
      })
    );
    // Assign properties to instance
    Object.assign(instance, writableProps);
  }

  // ──────────────────────────────────────────────────────────
  //  Snapshot / dirty tracking
  // ──────────────────────────────────────────────────────────

  /**
   * Captures a shallow snapshot of the instance's current property and
   * collection values.  Called automatically after hydration (`get()`,
   * query results).  The snapshot is used by `isDirty()`,
   * `changedFields()`, and by `update()` to skip unchanged fields.
   * @private
   */
  /**
   * Normalize a value for snapshot storage.
   * Arrays of model instances are reduced to their `.id` strings so that
   * dirty-tracking compares stable identifiers instead of object references.
   */
  private static normalizeValue(value: any): any {
    if (Array.isArray(value)) {
      return value.map((v: any) =>
        v && typeof v === 'object' && typeof v.id === 'string' ? v.id : v,
      );
    }
    return value;
  }

  private takeSnapshot(): void {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();
    const snap: Record<string, any> = {};

    for (const propName of Object.keys(metadata.properties)) {
      const val = (this as any)[propName];
      snap[propName] = Ad4mModel.normalizeValue(
        Array.isArray(val) ? [...val] : val,
      );
    }
    for (const collName of Object.keys(metadata.relations)) {
      const val = (this as any)[collName];
      snap[collName] = Ad4mModel.normalizeValue(
        Array.isArray(val) ? [...val] : val,
      );
    }

    this._snapshot = snap;
  }

  /**
   * Returns `true` if any tracked property or collection has changed
   * since the last hydration (or since `takeSnapshot()` was last called).
   *
   * Always returns `true` if no snapshot exists (e.g. a freshly
   * constructed instance that hasn't been fetched yet).
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.create(perspective, { name: "Pasta" });
   * recipe.isDirty();        // false — just hydrated
   * recipe.name = "Risotto";
   * recipe.isDirty();        // true
   * ```
   */
  isDirty(): boolean {
    if (!this._snapshot) return true;
    return this.changedFields().length > 0;
  }

  /**
   * Returns the names of properties/collections that differ from the
   * snapshot taken at the last hydration.
   *
   * Returns **all** field names if no snapshot exists.
   *
   * @example
   * ```typescript
   * recipe.name = "New Name";
   * recipe.changedFields(); // ["name"]
   * ```
   */
  changedFields(): string[] {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();

    if (!this._snapshot) {
      return [
        ...Object.keys(metadata.properties),
        ...Object.keys(metadata.relations),
      ];
    }

    const changed: string[] = [];
    const allFields = [
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ];

    for (const field of allFields) {
      const current = Ad4mModel.normalizeValue((this as any)[field]);
      const original = this._snapshot[field];

      if (Array.isArray(current) || Array.isArray(original)) {
        // Order-insensitive comparison (sorted) so reordering alone
        // doesn't mark a collection as dirty.
        const a = Array.isArray(current) ? [...current].sort() : [];
        const b = Array.isArray(original) ? [...original].sort() : [];
        if (a.length !== b.length || a.some((v: any, i: number) => v !== b[i])) {
          changed.push(field);
        }
      } else if (current !== original) {
        changed.push(field);
      }
    }
    return changed;
  }

  private async getData(opts?: GetOptions) {
    // Builds an object with the author, timestamp, all properties, & all collections on the Ad4mModel and saves it to the instance
    // Use SurrealDB for data queries
    try {
      const ctor = this.constructor as typeof Ad4mModel;
      const metadata = ctor.getModelMetadata();

      // Query for all links from this specific node (base expression)
      const safeBaseExpression = ctor.formatSurrealValue(this._baseExpression);
      const linksQuery = `
        SELECT id, predicate, out.uri AS target, author, timestamp
        FROM link
        WHERE in.uri = ${safeBaseExpression}
        ORDER BY timestamp ASC
      `;
      const links = await this._perspective.querySurrealDB(linksQuery);

      if (links && links.length > 0) {
        // Core hydration: properties (latest-wins), collections, timestamps/author
        const requestedProperties = opts?.properties && opts.properties.length > 0 ? opts.properties : undefined;
        await ctor.hydrateFromLinks(this, links, metadata, this._perspective, requestedProperties);
      }

      // Populate reverse relation fields (belongsToOne / belongsToMany) as string IDs.
      const allRelsMeta = getRelationsMetadata(ctor as any);
      const requestedProps = opts?.properties && opts.properties.length > 0 ? new Set(opts.properties) : null;
      for (const [relName, relMeta] of Object.entries(allRelsMeta)) {
        if (relMeta.kind !== 'belongsToOne' && relMeta.kind !== 'belongsToMany') continue;
        if (requestedProps && !requestedProps.has(relName)) continue;
        const reverseLinks = await this._perspective.get(
          new LinkQuery({ predicate: relMeta.predicate, target: this._baseExpression })
        );
        const sourceIds = reverseLinks
          .filter((l) => l.data.target === this._baseExpression)
          .map((l) => l.data.source);
        if (relMeta.kind === 'belongsToOne') {
          (this as any)[relName] = sourceIds.length > 0 ? sourceIds[sourceIds.length - 1] : null;
        } else {
          (this as any)[relName] = sourceIds;
        }
      }

      // Evaluate SurrealQL getters
      await ctor.evaluateCustomGettersForInstance(this, this._perspective, metadata);

      // Eager-load relations if requested
      if (opts?.include) {
        await ctor.hydrateRelations([this], this._perspective, opts.include);
      }
    } catch (e) {
      console.error(`SurrealDB getData also failed for ${this._baseExpression}:`, e);
    }

    this.takeSnapshot();
    return this;
  }

  // Todo: Only return AllInstances (InstancesWithOffset, SortedInstances, & UnsortedInstances not required)
  public static async queryToProlog(perspective: PerspectiveProxy, query: Query, modelClassName?: string | null) {
    const { properties, where, order, offset, limit, count } = query;
    const className = modelClassName || (await this.getClassName(perspective));

    // Resolve parent predicate from model metadata if needed
    const resolvedParentPredicate = query.parent
      ? resolveParentPredicate(query.parent, this)
      : undefined;

    const instanceQueries = [
      buildAuthorAndTimestampQuery(),
      buildParentQuery(query.parent, resolvedParentPredicate),
      buildPropertiesQuery(properties),
      buildWhereQuery(where),
    ];

    const resultSetQueries = [buildCountQuery(count), buildOrderQuery(order), buildOffsetQuery(offset), buildLimitQuery(limit)];

    const fullQuery = `
      findall([Base, Properties, Collections, Timestamp, Author], (
        subject_class("${className}", SubjectClass),
        instance(SubjectClass, Base),
        ${instanceQueries.filter((q) => q).join(", ")}
      ), UnsortedInstances),
      ${resultSetQueries.filter((q) => q).join(", ")}
    `;

    return fullQuery;
  }

  /**
   * Evaluates custom SurrealQL getters for properties and collections on a specific instance.
   * @private
   */
  private static async evaluateCustomGettersForInstance(
    instance: any,
    perspective: PerspectiveProxy,
    metadata: any
  ) {
    const safeBaseExpression = this.formatSurrealValue(instance.id);

    // Evaluate property getters
    for (const [propName, propMeta] of Object.entries(metadata.properties)) {
      if ((propMeta as any).getter) {
        try {
          // Replace 'Base' placeholder with actual base expression
          const query = (propMeta as any).getter.replace(/Base/g, safeBaseExpression);
          // Query from node table to have graph traversal context
          const result = await perspective.querySurrealDB(
            `SELECT (${query}) AS value FROM node WHERE uri = ${safeBaseExpression}`
          );
          if (result && result.length > 0 && result[0].value !== undefined && result[0].value !== null && result[0].value !== 'None' && result[0].value !== '') {
            instance[propName] = result[0].value;
          }
        } catch (error) {
          console.warn(`Failed to evaluate getter for ${propName}:`, error);
        }
      }
    }

    // Evaluate collection getters
    for (const [collName, collMeta] of Object.entries(metadata.collections)) {
      if ((collMeta as any).getter) {
        try {
          // Replace 'Base' placeholder with actual base expression
          const query = (collMeta as any).getter.replace(/Base/g, safeBaseExpression);
          // Query from node table to have graph traversal context
          const result = await perspective.querySurrealDB(
            `SELECT (${query}) AS value FROM node WHERE uri = ${safeBaseExpression}`
          );
          if (result && result.length > 0 && result[0].value !== undefined && result[0].value !== null) {
            // Filter out 'None' from collection results
            const value = result[0].value;
            instance[collName] = Array.isArray(value) 
              ? value.filter((v: any) => v !== undefined && v !== null && v !== '' && v !== 'None')
              : value;
          }
        } catch (error) {
          console.warn(`Failed to evaluate getter for ${collName}:`, error);
        }
      }
    }
  }

  /**
   * Hydrates relation fields on instances according to the provided IncludeMap.
   *
   * For each relation listed in `includeMap`, the raw expression-URI strings
   * stored on the instance are replaced with fully-hydrated model instances
   * (fetched via the relation's `target()` class).  Nested IncludeMaps are
   * supported for multi-level eager loading.
   *
   * @param instances - The instances whose relations should be hydrated
   * @param perspective - The perspective to fetch related instances from
   * @param includeMap - Describes which relations to hydrate
   * @private
   */
  private static async hydrateRelations<T extends Ad4mModel>(
    instances: T[],
    perspective: PerspectiveProxy,
    includeMap: IncludeMap | undefined,
  ): Promise<void> {
    if (!includeMap || Object.keys(includeMap).length === 0) return;

    const relMeta = getRelationsMetadata(this);

    for (const [relName, includeValue] of Object.entries(includeMap)) {
      const meta: RelationMetadataEntry | undefined = relMeta[relName];
      if (!meta) {
        console.warn(`include: relation "${relName}" not found in metadata, skipping`);
        continue;
      }

      const TargetClass = meta.target() as unknown as typeof Ad4mModel;

      // Determine if a RelationSubQuery was supplied (object) vs a plain `true`
      const subQuery: RelationSubQuery | undefined =
        typeof includeValue === 'object' && includeValue !== null
          ? (includeValue as RelationSubQuery)
          : undefined;
      const nestedInclude: IncludeMap | undefined = subQuery?.include;

      // ── Reverse relations (belongsToOne / belongsToMany) ──────────────────
      // The link goes target→instance, so we query backwards:
      //   predicate = meta.predicate, target = inst.id  →  source is the related id
      if (meta.kind === 'belongsToOne' || meta.kind === 'belongsToMany') {
        // Per-instance reverse lookup (can't batch easily across instances)
        for (const inst of instances) {
          const reverseLinks = await perspective.get(
            new LinkQuery({ predicate: meta.predicate, target: inst.id })
          );
          // Defensive filter: perspective.get may return extra results; ensure
          // we only use links that genuinely point to this instance.
          const sourceIds = reverseLinks
            .filter(l => l.data.target === inst.id)
            .map(l => l.data.source);

          if (meta.kind === 'belongsToOne') {
            if (sourceIds.length === 0) {
              (inst as any)[relName] = null;
              continue;
            }
            const sourceId = sourceIds[sourceIds.length - 1]; // latest-wins
            try {
              const related = new TargetClass(perspective, sourceId);
              await related.get();
              (inst as any)[relName] = related;
            } catch {
              (inst as any)[relName] = null;
            }
          } else {
            // belongsToMany — return array of hydrated instances
            let hydrated: Ad4mModel[] = [];

            // If there's a where/order sub-query, delegate to findAll for filtering
            if (subQuery && (subQuery.where || subQuery.order || subQuery.properties)) {
              const whereWithIds: Record<string, any> = {
                id: sourceIds,
                ...(subQuery.where ?? {}),
              };
              hydrated = await TargetClass.findAll(perspective, {
                where: whereWithIds as any,
                ...(subQuery.order && { order: subQuery.order as any }),
                ...(subQuery.properties && { properties: subQuery.properties }),
              });
            } else {
              await Promise.all(sourceIds.map(async (sid) => {
                try {
                  const related = new TargetClass(perspective, sid);
                  await related.get(
                    subQuery?.properties ? { properties: subQuery.properties } : undefined
                  );
                  hydrated.push(related);
                } catch { /* skip */ }
              }));
            }

            // Apply order (client-side, if not already handled by findAll above)
            if (subQuery?.order && !(subQuery.where || subQuery.properties)) {
              const orderEntries = Object.entries(subQuery.order);
              hydrated = hydrated.sort((a, b) => {
                for (const [field, dir] of orderEntries) {
                  const av = String((a as any)[field] ?? '');
                  const bv = String((b as any)[field] ?? '');
                  const diff = av.localeCompare(bv);
                  if (diff !== 0) return dir === 'DESC' ? -diff : diff;
                }
                return 0;
              });
            }

            // Apply offset and limit
            if (subQuery?.offset != null || subQuery?.limit != null) {
              const start = subQuery.offset ?? 0;
              const end = subQuery.limit != null ? start + subQuery.limit : undefined;
              hydrated = hydrated.slice(start, end);
            }

            (inst as any)[relName] = hydrated;

            // Recurse for nested includes
            if (nestedInclude && hydrated.length > 0) {
              await TargetClass.hydrateRelations(hydrated, perspective, nestedInclude);
            }
          }
        }
        continue; // skip the forward-relation path below
      }

      // ── Forward relations (hasMany / hasOne) ──────────────────────────────
      // Collect all unique URIs across all instances for batch-friendly lookup
      const uriSet = new Set<string>();
      for (const inst of instances) {
        const raw = (inst as any)[relName];
        if (raw == null) continue;
        if (Array.isArray(raw)) {
          for (const v of raw) if (typeof v === 'string') uriSet.add(v);
        } else if (typeof raw === 'string') {
          uriSet.add(raw);
        }
      }

      if (uriSet.size === 0) continue;

      // Hydrate related instances using findAll to ensure conformance checking.
      // findAll validates model membership via graph traversal (required predicates / flags),
      // so non-conforming linked URIs are silently dropped — matching the documented behaviour.
      const hydrated = new Map<string, Ad4mModel>();

      const whereWithIds: Record<string, any> = {
        id: Array.from(uriSet),
        ...(subQuery?.where ?? {}),
      };
      const fetchQuery: any = {
        where: whereWithIds,
        ...(subQuery?.order && { order: subQuery.order }),
        ...(subQuery?.properties && { properties: subQuery.properties }),
      };
      const results = await TargetClass.findAll(perspective, fetchQuery);
      for (const result of results) {
        hydrated.set(result.id, result);
      }

      // Replace raw URIs with hydrated instances on each parent instance
      for (const inst of instances) {
        const raw = (inst as any)[relName];
        if (raw == null) continue;
        if (Array.isArray(raw)) {
          // Map URIs → instances; drop those filtered out by where
          let resolved: Ad4mModel[] = raw
            .map((v: any) =>
              typeof v === 'string' && hydrated.has(v) ? hydrated.get(v)! : null,
            )
            .filter((v): v is Ad4mModel => v !== null);

          // Per-instance sort (client-side, after filtering)
          if (subQuery?.order) {
            const orderEntries = Object.entries(subQuery.order);
            resolved = resolved.sort((a, b) => {
              for (const [field, dir] of orderEntries) {
                const av = String((a as any)[field] ?? '');
                const bv = String((b as any)[field] ?? '');
                const diff = av.localeCompare(bv);
                if (diff !== 0) return dir === 'DESC' ? -diff : diff;
              }
              return 0;
            });
          }

          // Per-instance limit / offset
          if (subQuery?.offset != null || subQuery?.limit != null) {
            const start = subQuery.offset ?? 0;
            const end =
              subQuery.limit != null ? start + subQuery.limit : undefined;
            resolved = resolved.slice(start, end);
          }

          // Enforce maxCount guard — single-valued relations keep only the last item
          if (meta.maxCount === 1) {
            if (resolved.length > 1) {
              console.warn(
                `include: relation "${relName}" has maxCount 1 but ${resolved.length} values found; keeping the last`,
              );
            }
            (inst as any)[relName] = resolved.length > 0
              ? resolved[resolved.length - 1]
              : null;
          } else {
            (inst as any)[relName] = resolved;
          }
        } else if (typeof raw === 'string' && hydrated.has(raw)) {
          (inst as any)[relName] = hydrated.get(raw);
        }
      }

      // Recurse for nested includes
      if (nestedInclude) {
        const hydratedInstances = Array.from(hydrated.values());
        await TargetClass.hydrateRelations(hydratedInstances, perspective, nestedInclude);
      }
    }
  }

  /**
   * Generates a SurrealQL query from a Query object.
   * 
   * @description
   * This method translates high-level query parameters into a SurrealQL query string
   * that can be executed against the SurrealDB backend. Unlike Prolog queries which
   * operate on SDNA-aware predicates, SurrealQL queries operate directly on raw links
   * stored in SurrealDB.
   * 
   * The generated query uses a CTE (Common Table Expression) pattern:
   * 1. First, identify candidate base expressions by filtering links based on where conditions
   * 2. Then, for each candidate base, resolve properties and collections via subqueries
   * 3. Finally, apply ordering, pagination (LIMIT/START) at the SQL level
   * 
   * Key architectural notes:
   * - SurrealDB stores only raw links (source, predicate, target, author, timestamp)
   * - No SDNA knowledge at the database level
   * - Properties are resolved via subqueries that look for links with specific predicates
   * - Collections are similar but return multiple values instead of one
   * - Special fields (base, author, timestamp) are accessed directly, not via subqueries
   * 
   * @param perspective - The perspective to query (used for metadata extraction)
   * @param query - Query parameters (where, order, limit, offset, properties, collections)
   * @returns Complete SurrealQL query string ready for execution
   * 
   * @example
   * ```typescript
   * const query = Recipe.queryToSurrealQL(perspective, {
   *   where: { name: "Pasta", rating: { gt: 4 } },
   *   order: { timestamp: "DESC" },
   *   limit: 10
   * });
   * // Returns: SELECT source AS base, array::first(target[WHERE predicate = ...]) AS name, ...
   * //          FROM link WHERE ... GROUP BY source ORDER BY timestamp DESC LIMIT 10
   * ```
   */
  public static async queryToSurrealQL(perspective: PerspectiveProxy, query: Query): Promise<string> {
    const metadata = this.getModelMetadata();
    const { where, order, offset, limit } = query;

    // Build list of graph traversal filters for required predicates
    const graphTraversalFilters: string[] = [];

    // Add parent filter if specified (filter to nodes linked from a parent via a specific predicate)
    if (query.parent) {
      const parentPredicate = resolveParentPredicate(query.parent, this);
      graphTraversalFilters.push(
        `count(<-link[WHERE perspective = $perspective AND in.uri = ${this.formatSurrealValue(query.parent.id)} AND predicate = '${escapeSurrealString(parentPredicate)}']) > 0`
      );
    }

    // Add filters for required properties
    for (const [propName, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.required) {
        // For flag properties, also filter by the target value
        if (propMeta.flag && propMeta.initial) {
          graphTraversalFilters.push(
            `count(->link[WHERE perspective = $perspective AND predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`
          );
        } else {
          graphTraversalFilters.push(
            `count(->link[WHERE perspective = $perspective AND predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`
          );
        }
      }
    }

    // If no required properties, we need at least one property to define the model
    // Use any property with an initial value as the defining characteristic
    if (graphTraversalFilters.length === 0) {
      for (const [propName, propMeta] of Object.entries(metadata.properties)) {
        if (propMeta.initial) {
          // For flag properties, also filter by the target value
          if (propMeta.flag) {
            graphTraversalFilters.push(
              `count(->link[WHERE perspective = $perspective AND predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`
            );
          } else {
            graphTraversalFilters.push(
              `count(->link[WHERE perspective = $perspective AND predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`
            );
          }
          break; // Just need one defining property
        }
      }
    }

    // Build user WHERE clause filters using graph traversal
    const userWhereClause = this.buildGraphTraversalWhereClause(metadata, where);

    // Build complete WHERE clause using graph traversal filters
    const whereConditions: string[] = [];

    // Add all graph traversal filters for required properties
    whereConditions.push(...graphTraversalFilters);

    // Add user where conditions if any
    if (userWhereClause) {
      whereConditions.push(userWhereClause);
    }

    // Always ensure node has at least one link in this perspective
    whereConditions.push(`count(->link[WHERE perspective = $perspective]) > 0`);

    // Build the query FROM node using direct graph traversal in WHERE
    // This avoids slow subqueries and uses graph indexes for fast traversal
    const fullQuery = `
SELECT
    id AS source,
    uri AS source_uri,
    ->link[WHERE perspective = $perspective] AS links
FROM node
WHERE ${whereConditions.join(' AND ')}
    `.trim();

    return fullQuery;
  }

  /**
   * Builds the WHERE clause for SurrealQL queries using graph traversal syntax.
   *
   * @description
   * Translates where conditions into graph traversal filters: `->link[WHERE ...]`
   * This is more efficient than nested SELECTs because SurrealDB can optimize graph traversals.
   *
   * Handles several condition types:
   * - Simple equality: `{ name: "Pasta" }` → `->link[WHERE predicate = 'X' AND out.uri = 'Pasta']`
   * - Arrays (IN clause): `{ name: ["Pasta", "Pizza"] }` → `->link[WHERE predicate = 'X' AND out.uri IN [...]]`
   * - NOT operators: Use `NOT` prefix
   * - Comparison operators (gt, gte, lt, lte, etc.): Handled in post-query JavaScript filtering
   * - Special fields: base uses `uri` directly, author/timestamp handled post-query
   *
   * @param metadata - Model metadata containing property predicates
   * @param where - Where conditions from the query
   * @returns Graph traversal WHERE clause filters, or empty string if no conditions
   *
   * @private
   */
  private static buildGraphTraversalWhereClause(metadata: ModelMetadata, where?: Where): string {
    if (!where) return '';

    const conditions: string[] = [];

    for (const [propertyName, condition] of Object.entries(where)) {
      // Check if this is a special field (id, author, timestamp)
      // Note: author and timestamp filtering is done in JavaScript after query
      const isSpecial = ['id', 'author', 'timestamp'].includes(propertyName);

      if (isSpecial) {
        // Skip author and timestamp - they'll be filtered in JavaScript
        // Only handle 'id' (which maps to 'uri') here
        if (propertyName === 'author' || propertyName === 'timestamp') {
          continue; // Skip - will be filtered post-query
        }

        const columnName = 'uri'; // id maps to uri in node table

        // Handle base/uri field directly
        if (Array.isArray(condition)) {
          // Array values (IN clause)
          const formattedValues = condition.map(v => this.formatSurrealValue(v)).join(', ');
          conditions.push(`${columnName} IN [${formattedValues}]`);
        } else if (typeof condition === 'object' && condition !== null) {
          // Operator object
          const ops = condition as any;
          if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
              const formattedValues = ops.not.map(v => this.formatSurrealValue(v)).join(', ');
              conditions.push(`${columnName} NOT IN [${formattedValues}]`);
            } else {
              conditions.push(`${columnName} != ${this.formatSurrealValue(ops.not)}`);
            }
          }
          if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
            conditions.push(`${columnName} >= ${this.formatSurrealValue(ops.between[0])} AND ${columnName} <= ${this.formatSurrealValue(ops.between[1])}`);
          }
          if (ops.gt !== undefined) {
            conditions.push(`${columnName} > ${this.formatSurrealValue(ops.gt)}`);
          }
          if (ops.gte !== undefined) {
            conditions.push(`${columnName} >= ${this.formatSurrealValue(ops.gte)}`);
          }
          if (ops.lt !== undefined) {
            conditions.push(`${columnName} < ${this.formatSurrealValue(ops.lt)}`);
          }
          if (ops.lte !== undefined) {
            conditions.push(`${columnName} <= ${this.formatSurrealValue(ops.lte)}`);
          }
          if (ops.contains !== undefined) {
            conditions.push(`${columnName} CONTAINS ${this.formatSurrealValue(ops.contains)}`);
          }
        } else {
          // Simple equality
          conditions.push(`${columnName} = ${this.formatSurrealValue(condition)}`);
        }
      } else {
        // Handle regular properties via graph traversal.
        // IMPORTANT: Check relation metadata first — @BelongsToOne / @BelongsToMany
        // also write into propertyRegistry, but the link direction is inverted.
        // If we matched the property path they would get a forward ->link filter
        // which is wrong for belongs-to relations.
        const allRelations = getRelationsMetadata(this);
        const relMeta = allRelations[propertyName];
        const isBelongs = relMeta?.kind === 'belongsToOne' || relMeta?.kind === 'belongsToMany';

        if (relMeta) {
          const predicate = escapeSurrealString(relMeta.predicate);

          if (Array.isArray(condition)) {
            const formattedValues = condition.map(v => this.formatSurrealValue(v)).join(', ');
            if (isBelongs) {
              conditions.push(`count(<-link[WHERE perspective = $perspective AND predicate = '${predicate}' AND in.uri IN [${formattedValues}]]) > 0`);
            } else {
              conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND out.uri IN [${formattedValues}]]) > 0`);
            }
          } else if (typeof condition === 'object' && condition !== null) {
            const ops = condition as any;
            if (ops.not !== undefined) {
              if (Array.isArray(ops.not)) {
                const formattedValues = ops.not.map(v => this.formatSurrealValue(v)).join(', ');
                if (isBelongs) {
                  conditions.push(`count(<-link[WHERE perspective = $perspective AND predicate = '${predicate}' AND in.uri IN [${formattedValues}]]) = 0`);
                } else {
                  conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND out.uri IN [${formattedValues}]]) = 0`);
                }
              } else {
                if (isBelongs) {
                  conditions.push(`count(<-link[WHERE perspective = $perspective AND predicate = '${predicate}' AND in.uri = ${this.formatSurrealValue(ops.not)}]) = 0`);
                } else {
                  conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND out.uri = ${this.formatSurrealValue(ops.not)}]) = 0`);
                }
              }
            }
          } else {
            // Simple equality
            if (isBelongs) {
              conditions.push(`count(<-link[WHERE perspective = $perspective AND predicate = '${predicate}' AND in.uri = ${this.formatSurrealValue(condition)}]) > 0`);
            } else {
              conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND out.uri = ${this.formatSurrealValue(condition)}]) > 0`);
            }
          }
          continue;
        }

        const propMeta = metadata.properties[propertyName];
        if (!propMeta) continue; // Skip if property not found in metadata

        const predicate = escapeSurrealString(propMeta.predicate);
        // Use fn::parse_literal() for properties with resolveLanguage
        const targetField = propMeta.resolveLanguage === 'literal' ? 'fn::parse_literal(out.uri)' : 'out.uri';

        if (Array.isArray(condition)) {
          // Array values (IN clause)
          const formattedValues = condition.map(v => this.formatSurrealValue(v)).join(', ');
          conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) > 0`);
        } else if (typeof condition === 'object' && condition !== null) {
          // Operator object
          const ops = condition as any;
          if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
              // For NOT IN with array: must NOT have a link with value in the array
              const formattedValues = ops.not.map(v => this.formatSurrealValue(v)).join(', ');
              conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) = 0`);
            } else {
              // For NOT with single value: must NOT have this value
              conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND ${targetField} = ${this.formatSurrealValue(ops.not)}]) = 0`);
            }
          }
          // Note: gt, gte, lt, lte, between, contains operators are filtered in JavaScript
          // post-query because fn::parse_literal() comparisons in SurrealDB
          // don't work reliably with numeric comparisons.
          // These are handled in instancesFromSurrealResult along with author/timestamp filtering.
          // However, we still need to ensure the property exists
          const hasComparisonOps = ops.gt !== undefined || ops.gte !== undefined ||
                                   ops.lt !== undefined || ops.lte !== undefined ||
                                   ops.between !== undefined || ops.contains !== undefined;
          if (hasComparisonOps) {
            // Ensure we only get nodes that have this property
            conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}']) > 0`);
          }
        } else {
          // Simple equality
          conditions.push(`count(->link[WHERE perspective = $perspective AND predicate = '${predicate}' AND ${targetField} = ${this.formatSurrealValue(condition)}]) > 0`);
        }
      }
    }

    return conditions.join(' AND ');
  }

  /**
   * Builds the WHERE clause for SurrealQL queries.
   *
   * @description
   * Translates the where conditions from the Query object into SurrealQL WHERE clause fragments.
   * For each property filter, generates a subquery that checks for links with the appropriate
   * predicate and target value.
   *
   * Handles several condition types:
   * - Simple equality: `{ name: "Pasta" }` → subquery checking for predicate and target match
   * - Arrays (IN clause): `{ name: ["Pasta", "Pizza"] }` → target IN [...]
   * - Operators: `{ rating: { gt: 4 } }` → target > '4'
   *   - gt, gte, lt, lte: comparison operators
   *   - not: negation (single value or array)
   *   - between: range check
   *   - contains: substring/element check (uses SurrealQL CONTAINS)
   * - Special fields: base, author, timestamp are accessed directly, not via subqueries
   *
   * All conditions are joined with AND.
   *
   * @param metadata - Model metadata containing property predicates
   * @param where - Where conditions from the query
   * @returns WHERE clause string (without the "WHERE" keyword), or empty string if no conditions
   *
   * @private
   */
  private static buildSurrealWhereClause(metadata: ModelMetadata, where?: Where): string {
    if (!where) return '';
    
    const conditions: string[] = [];
    
    for (const [propertyName, condition] of Object.entries(where)) {
      // Check if this is a special field (id, author, timestamp)
      // Note: author and timestamp filtering is done in JavaScript after GROUP BY
      // because they need to be computed from the grouped links first
      const isSpecial = ['id', 'author', 'timestamp'].includes(propertyName);
      
      if (isSpecial) {
        // Skip author and timestamp - they'll be filtered in JavaScript
        // Only handle 'id' (which maps to 'source') here
        if (propertyName === 'author' || propertyName === 'timestamp') {
          continue; // Skip - will be filtered post-query
        }
        
        const columnName = 'source'; // id maps to source
        
        // Handle base/source field directly
        if (Array.isArray(condition)) {
          // Array values (IN clause)
          const formattedValues = condition.map(v => this.formatSurrealValue(v)).join(', ');
          conditions.push(`${columnName} IN [${formattedValues}]`);
        } else if (typeof condition === 'object' && condition !== null) {
          // Operator object
          const ops = condition as any;
          if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
              const formattedValues = ops.not.map(v => this.formatSurrealValue(v)).join(', ');
              conditions.push(`${columnName} NOT IN [${formattedValues}]`);
            } else {
              conditions.push(`${columnName} != ${this.formatSurrealValue(ops.not)}`);
            }
          }
          if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
            conditions.push(`${columnName} >= ${this.formatSurrealValue(ops.between[0])} AND ${columnName} <= ${this.formatSurrealValue(ops.between[1])}`);
          }
          if (ops.gt !== undefined) {
            conditions.push(`${columnName} > ${this.formatSurrealValue(ops.gt)}`);
          }
          if (ops.gte !== undefined) {
            conditions.push(`${columnName} >= ${this.formatSurrealValue(ops.gte)}`);
          }
          if (ops.lt !== undefined) {
            conditions.push(`${columnName} < ${this.formatSurrealValue(ops.lt)}`);
          }
          if (ops.lte !== undefined) {
            conditions.push(`${columnName} <= ${this.formatSurrealValue(ops.lte)}`);
          }
          if (ops.contains !== undefined) {
            conditions.push(`${columnName} CONTAINS ${this.formatSurrealValue(ops.contains)}`);
          }
        } else {
          // Simple equality
          conditions.push(`${columnName} = ${this.formatSurrealValue(condition)}`);
        }
      } else {
        // Handle regular properties via subqueries
        const propMeta = metadata.properties[propertyName];
        if (!propMeta) continue; // Skip if property not found in metadata
        
        const predicate = escapeSurrealString(propMeta.predicate);
        // Use fn::parse_literal() for properties with resolveLanguage
        const targetField = propMeta.resolveLanguage === 'literal' ? 'fn::parse_literal(target)' : 'target';
        
        if (Array.isArray(condition)) {
          // Array values (IN clause)
          const formattedValues = condition.map(v => this.formatSurrealValue(v)).join(', ');
          conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`);
        } else if (typeof condition === 'object' && condition !== null) {
          // Operator object
          const ops = condition as any;
          if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
              // For NOT IN with array: exclude sources that HAVE a value in the array
              const formattedValues = ops.not.map(v => this.formatSurrealValue(v)).join(', ');
              conditions.push(`source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`);
            } else {
              // For NOT with single value: exclude sources that HAVE this value
              conditions.push(`source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${this.formatSurrealValue(ops.not)})`);
            }
          }
          // Note: gt, gte, lt, lte, between, contains operators are filtered in JavaScript
          // post-query because fn::parse_literal() comparisons in SurrealDB subqueries
          // don't work reliably with numeric comparisons.
          // These are handled in instancesFromSurrealResult along with author/timestamp filtering.
          // However, we still need to ensure the property exists by filtering on the predicate
          const hasComparisonOps = ops.gt !== undefined || ops.gte !== undefined ||
                                   ops.lt !== undefined || ops.lte !== undefined ||
                                   ops.between !== undefined || ops.contains !== undefined;
          if (hasComparisonOps) {
            // Ensure we only get instances that have this property
            conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}')`);
          }
        } else {
          // Simple equality
          conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${this.formatSurrealValue(condition)})`);
        }
      }
    }
    
    return conditions.join(' AND ');
  }

  /**
   * Builds the SELECT fields for SurrealQL queries.
   * 
   * @description
   * Generates the field list for the SELECT clause, resolving properties and collections
   * via subqueries. Each property is fetched with a subquery that finds the link with the
   * appropriate predicate and returns its target. Collections are similar but don't use LIMIT 1.
   * 
   * Field types:
   * - Properties: `(SELECT VALUE target FROM link WHERE source = $parent.base AND predicate = 'X' LIMIT 1) AS propName`
   * - Collections: `(SELECT VALUE target FROM link WHERE source = $parent.base AND predicate = 'X') AS collName`
   * - Author/Timestamp: Always included to provide metadata about each instance
   * 
   * If properties or collections arrays are provided, only those fields are included.
   * Otherwise, all properties/collections from metadata are included.
   * 
   * @param metadata - Model metadata containing property and collection predicates
   * @param properties - Optional array of property names to include (default: all)
   * @param collections - Optional array of collection names to include (default: all)
   * @returns Comma-separated SELECT field list
   * 
   * @private
   */
  private static buildSurrealSelectFields(metadata: ModelMetadata, properties?: string[], collections?: string[]): string {
    const fields: string[] = [];
    
    // Determine properties to fetch
    const propsToFetch = properties || Object.keys(metadata.properties);
    for (const propName of propsToFetch) {
      const propMeta = metadata.properties[propName];
      if (!propMeta) continue; // Skip if not found
      
      // Reference source directly since we're selecting from link table
      const escapedPredicate = escapeSurrealString(propMeta.predicate);
      fields.push(`(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}' LIMIT 1) AS ${propName}`);
    }
    
    // Determine collections to fetch
    const collsToFetch = collections || Object.keys(metadata.collections);
    for (const collName of collsToFetch) {
      const collMeta = metadata.collections[collName];
      if (!collMeta) continue; // Skip if not found
      
      // Reference source directly since we're selecting from link table
      const escapedPredicate = escapeSurrealString(collMeta.predicate);
      fields.push(`(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}') AS ${collName}`);
    }
    
    // Always add author and timestamp fields
    fields.push(`(SELECT VALUE author FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS author`);
    fields.push(`(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS createdAt`);
    fields.push(`(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp DESC LIMIT 1) AS updatedAt`);
    
    return fields.join(',\n  ');
  }

  /**
   * Builds the SELECT fields for SurrealQL queries using aggregation functions.
   * Compatible with GROUP BY source queries.
   * 
   * @private
   */
  private static buildSurrealSelectFieldsWithAggregation(metadata: ModelMetadata, properties?: string[], collections?: string[]): string {
    const fields: string[] = [];
    
    // Determine properties to fetch
    const propsToFetch = properties || Object.keys(metadata.properties);
    for (const propName of propsToFetch) {
      const propMeta = metadata.properties[propName];
      if (!propMeta) continue; // Skip if not found
      
      // Use array::first to get the first target value for this predicate
      const escapedPredicate = escapeSurrealString(propMeta.predicate);
      fields.push(`array::first(target[WHERE predicate = '${escapedPredicate}']) AS ${propName}`);
    }
    
    // Determine collections to fetch
    const collsToFetch = collections || Object.keys(metadata.collections);
    for (const collName of collsToFetch) {
      const collMeta = metadata.collections[collName];
      if (!collMeta) continue; // Skip if not found
      
      // Use array filtering to get all target values for this predicate
      const escapedPredicate = escapeSurrealString(collMeta.predicate);
      fields.push(`target[WHERE predicate = '${escapedPredicate}'] AS ${collName}`);
    }
    
    // Always add author and timestamp fields
    fields.push(`array::first(author) AS author`);
    fields.push(`array::first(timestamp) AS createdAt`);
    fields.push(`array::last(timestamp) AS updatedAt`);
    
    return fields.join(',\n  ');
  }


  /**
   * Formats a value for use in SurrealQL queries.
   * 
   * @description
   * Handles different value types:
   * - Strings: Wrapped in single quotes with backslash-escaped special characters
   * - Numbers/booleans: Converted to string
   * - Arrays: Recursively formatted and wrapped in brackets
   * 
   * @param value - The value to format
   * @returns Formatted value string ready for SurrealQL
   * 
   * @private
   */
  private static formatSurrealValue(value: any): string {
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
      return `[${value.map(v => this.formatSurrealValue(v)).join(', ')}]`;
    } else {
      return String(value);
    }
  }

  public static async instancesFromPrologResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy,
    query: Query,
    result: AllInstancesResult
  ): Promise<ResultsWithTotalCount<T>> {
    if (!result?.[0]?.AllInstances) return { results: [], totalCount: 0 };
    // Map results to instances
    const requestedProperties = query?.properties || [];
    const allInstances = await Promise.all(
      result[0].AllInstances.map(async ([Base, Properties, Collections, Timestamp, Author]) => {
        try {
          const instance = new this(perspective, Base) as any;
          // Remove unrequested attributes from instance
          if (requestedProperties.length) {
            Object.keys(instance).forEach((key) => {
              if (!requestedProperties.includes(key) && key !== 'createdAt' && key !== 'updatedAt' && key !== 'author' && key !== 'id' && key !== 'baseExpression') delete instance[key];
            });
          }
          // Collect values to assign to instance
          const values = [...Properties, ...Collections, ["createdAt", Timestamp], ["author", Author]];
          await Ad4mModel.assignValuesToInstance(perspective, instance, values);

          return instance;
        } catch (error) {
          console.error(`Failed to process instance ${Base}:`, error);
          // Return null for failed instances - we'll filter these out below
          return null;
        }
      })
    );
    const instances = allInstances.filter((instance) => instance !== null) as T[];

    // Eager-load relations if requested (BEFORE snapshot so dirty tracking is accurate)
    if (query.include && instances.length > 0) {
      await this.hydrateRelations(instances, perspective, query.include);
    }

    // Take snapshots for dirty tracking after ALL hydration is complete
    // (including eager-loaded relations)
    for (const inst of instances) {
      (inst as Ad4mModel).takeSnapshot();
    }

    return { results: instances, totalCount: result[0].TotalCount };
  }

  /**
   * Converts SurrealDB query results to Ad4mModel instances.
   * 
   * @param perspective - The perspective context
   * @param query - The query parameters used
   * @param result - Array of result objects from SurrealDB
   * @returns Promise resolving to results with total count
   * 
   * @internal
   */
  public static async instancesFromSurrealResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy,
    query: Query,
    result: any[]
  ): Promise<ResultsWithTotalCount<T>> {
    if (!result || result.length === 0) return { results: [], totalCount: 0 };
    
    const metadata = this.getModelMetadata();
    const requestedProperties = query?.properties || [];
    
    // The query used GROUP BY with graph traversal, so each row has:
    // - source: the node ID (e.g., "node:abc123")
    // - source_uri: the actual URI (the base expression)
    // - links: array of link objects with {predicate, target, author, timestamp}

    const instances: T[] = [];
    for (const row of result) {
      let base;
      try {
        // Use source_uri as the base (the actual URI), not the node ID
        base = row.source_uri;

        // Skip rows without a source_uri field
        if (!base) {
          continue;
        }
        
        const links = row.links || [];
        
        const instance = new this(perspective, base) as any;

        // Core hydration via unified helper (pass requestedProperties for sparse fieldset)
        await this.hydrateFromLinks(instance, links, metadata, perspective, requestedProperties.length > 0 ? requestedProperties : undefined);
        
        // When specific properties are requested, delete unrequested properties
        // so they return undefined instead of their constructor defaults (e.g. 0, [])
        if (requestedProperties.length > 0) {
          const requested = new Set(requestedProperties);
          for (const propName of Object.keys(metadata.properties)) {
            if (!requested.has(propName)) {
              delete instance[propName];
            }
          }
          for (const collName of Object.keys(metadata.relations)) {
            if (!requested.has(collName) && !(query.include && collName in query.include)) {
              delete instance[collName];
            }
          }
          // Also strip metadata fields unless explicitly requested
          for (const metaField of ['author', 'createdAt', 'updatedAt'] as const) {
            if (!requested.has(metaField)) {
              delete instance[metaField];
            }
          }
        }

        instances.push(instance);
      } catch (error) {
        console.error(`Failed to process SurrealDB instance ${base}:`, error);
      }
    }

    // Populate reverse relation fields (belongsToOne / belongsToMany) as string IDs.
    // These relations point FROM other nodes TO this instance, so they cannot be resolved
    // from the node's own outgoing links fetched above. We do a reverse-link lookup here
    // so that these fields are populated as IDs even without an explicit include.
    const allRelsMeta = getRelationsMetadata(this as any);
    const reverseRelEntries = Object.entries(allRelsMeta).filter(
      ([relName, meta]) =>
        (meta.kind === 'belongsToOne' || meta.kind === 'belongsToMany') &&
        (requestedProperties.length === 0 || requestedProperties.includes(relName))
    );
    if (reverseRelEntries.length > 0 && instances.length > 0) {
      await Promise.all(
        instances.map(async (inst) => {
          for (const [relName, relMeta] of reverseRelEntries) {
            const reverseLinks = await perspective.get(
              new LinkQuery({ predicate: relMeta.predicate, target: inst.id })
            );
            const sourceIds = reverseLinks
              .filter((l) => l.data.target === inst.id)
              .map((l) => l.data.source);
            if (relMeta.kind === 'belongsToOne') {
              (inst as any)[relName] = sourceIds.length > 0 ? sourceIds[sourceIds.length - 1] : null;
            } else {
              (inst as any)[relName] = sourceIds;
            }
          }
        })
      );
    }

    // Evaluate custom getters for all instances (single pass)
    for (const instance of instances) {
      await this.evaluateCustomGettersForInstance(instance, perspective, metadata);
    }
    
    // Filter by where conditions that couldn't be filtered in SQL
    // This includes:
    // - author/timestamp (computed from grouped links)
    // - Properties with comparison operators (gt, gte, lt, lte, between, contains)
    //   because fn::parse_literal() comparisons in SurrealDB subqueries don't work reliably
    let filteredInstances = instances;
    if (query.where) {
      filteredInstances = instances.filter(instance => {
        for (const [propertyName, condition] of Object.entries(query.where!)) {
          // Skip 'base' as it's filtered in SQL
          if (propertyName === 'base') continue;

          // For author and timestamp, always filter in JS
          if (propertyName === 'author' || propertyName === 'timestamp') {
            if (!this.matchesCondition(instance[propertyName], condition)) {
              return false;
            }
            continue;
          }

          // For regular properties, only filter comparison operators in JS
          // Simple equality and NOT are handled in SQL, but gt/gte/lt/lte/between/contains need JS
          if (typeof condition === 'object' && condition !== null && !Array.isArray(condition)) {
            const ops = condition as any;
            // Check if any comparison operators are present
            const hasComparisonOps = ops.gt !== undefined || ops.gte !== undefined ||
                                     ops.lt !== undefined || ops.lte !== undefined ||
                                     ops.between !== undefined || ops.contains !== undefined;
            if (hasComparisonOps) {
              if (!this.matchesCondition(instance[propertyName], condition)) {
                return false;
              }
            }
          }
        }
        return true;
      });
    }

    // Apply ordering in JavaScript
    // If limit/offset is used but no explicit order, default to ordering by timestamp (ASC)
    // This ensures consistent pagination behavior
    const effectiveOrder = query.order ||
      (query.limit !== undefined || query.offset !== undefined ? { timestamp: 'ASC' as 'ASC' } : null);

    if (effectiveOrder) {
      const orderEntries = Object.entries(effectiveOrder);

      filteredInstances.sort((a: any, b: any) => {
        for (const [orderPropName, orderDirection] of orderEntries) {
          let aVal = a[orderPropName];
          let bVal = b[orderPropName];

          // Handle undefined values - push them to the end
          if (aVal === undefined && bVal === undefined) continue;
          if (aVal === undefined) return orderDirection === 'ASC' ? 1 : -1;
          if (bVal === undefined) return orderDirection === 'ASC' ? -1 : 1;

          // Compare values
          let comparison = 0;
          if (typeof aVal === 'number' && typeof bVal === 'number') {
            comparison = aVal - bVal;
          } else if (typeof aVal === 'string' && typeof bVal === 'string') {
            comparison = aVal.localeCompare(bVal);
          } else {
            comparison = String(aVal).localeCompare(String(bVal));
          }

          if (comparison !== 0) {
            return orderDirection === 'DESC' ? -comparison : comparison;
          }
          // comparison === 0: continue to next sort field
        }
        return 0;
      });
    }

    // Calculate totalCount BEFORE applying limit/offset
    const totalCount = filteredInstances.length;

    // Apply offset and limit in JavaScript
    let paginatedInstances = filteredInstances;
    if (query.offset !== undefined || query.limit !== undefined) {
      const start = query.offset || 0;
      const end = query.limit ? start + query.limit : undefined;
      paginatedInstances = filteredInstances.slice(start, end);
    }

    // Eager-load relations if requested (BEFORE snapshot so dirty tracking is accurate)
    if (query.include && paginatedInstances.length > 0) {
      await this.hydrateRelations(paginatedInstances, perspective, query.include);
    }

    // Take snapshots for dirty tracking after ALL hydration is complete
    // (including eager-loaded relations)
    for (const inst of paginatedInstances) {
      (inst as Ad4mModel).takeSnapshot();
    }

    return {
      results: paginatedInstances,
      totalCount
    };
  }
  
  /**
   * Checks if a value matches a condition (for post-query filtering).
   * @private
   */
  private static matchesCondition(value: any, condition: WhereCondition): boolean {
    // Handle array values (IN clause)
    if (Array.isArray(condition)) {
      return (condition as any[]).includes(value);
    }
    
    // Handle operator object
    if (typeof condition === 'object' && condition !== null) {
      const ops = condition as any;
      
      // Special case: 'not' operator (exclusive with other operators)
      if (ops.not !== undefined) {
        if (Array.isArray(ops.not)) {
          return !(ops.not as any[]).includes(value);
        } else {
          return value !== ops.not;
        }
      }
      
      // Special case: 'between' operator (inclusive range, exclusive with gt/gte/lt/lte)
      if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
        return value >= ops.between[0] && value <= ops.between[1];
      }
      
      // For all other operators (gt, gte, lt, lte, contains), we need to check ALL of them
      // and return true only if ALL conditions are satisfied
      let allConditionsMet = true;
      
      if (ops.gt !== undefined) {
        allConditionsMet = allConditionsMet && (value > ops.gt);
      }
      
      if (ops.gte !== undefined) {
        allConditionsMet = allConditionsMet && (value >= ops.gte);
      }
      
      if (ops.lt !== undefined) {
        allConditionsMet = allConditionsMet && (value < ops.lt);
      }
      
      if (ops.lte !== undefined) {
        allConditionsMet = allConditionsMet && (value <= ops.lte);
      }
      
      if (ops.contains !== undefined) {
        if (typeof value === 'string') {
          allConditionsMet = allConditionsMet && value.includes(String(ops.contains));
        } else if (Array.isArray(value)) {
          allConditionsMet = allConditionsMet && value.includes(ops.contains);
        } else {
          allConditionsMet = false;
        }
      }
      
      return allConditionsMet;
    }
    
    // Simple equality
    return value === condition;
  }

  /**
   * Gets all instances of the model in the perspective that match the query params.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSurrealDB - Whether to use SurrealDB (default: true, 10-100x faster) or Prolog (legacy)
   * @returns Array of matching models
   * 
   * @example
   * ```typescript
   * // Get all recipes (uses SurrealDB by default)
   * const allRecipes = await Recipe.findAll(perspective);
   * 
   * // Get recipes with specific criteria (uses SurrealDB)
   * const recipes = await Recipe.findAll(perspective, {
   *   where: { 
   *     name: "Pasta",
   *     rating: { gt: 4 }
   *   },
   *   order: { createdAt: "DESC" },
   *   limit: 10
   * });
   * 
   * // Explicitly use Prolog (legacy, for backward compatibility)
   * const recipesProlog = await Recipe.findAll(perspective, {}, false);
   * ```
   */
  static async findAll<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    query: Query = {},
    useSurrealDB: boolean = true
  ): Promise<T[]> {
    if (query.properties && query.properties.length === 0) {
      throw new Error("properties[] must not be empty — omit the field to return all properties, or specify at least one field name");
    }
    if (useSurrealDB) {
      const surrealQuery = await this.queryToSurrealQL(perspective, query);
      const result = await perspective.querySurrealDB(surrealQuery);
      const { results } = await this.instancesFromSurrealResult(perspective, query, result);
      return results;
    } else {
      const prologQuery = await this.queryToProlog(perspective, query);
      const result = await perspective.infer(prologQuery);
      const { results } = await this.instancesFromPrologResult(perspective, query, result);
      return results;
    }
  }

  /**
   * Finds the first instance matching the query, or `null` if none exists.
   *
   * Equivalent to `findAll` with `limit: 1` — only one instance is hydrated.
   *
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSurrealDB - Whether to use SurrealDB (default: true) or Prolog (legacy)
   * @returns The first matching instance, or `null`
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.findOne(perspective, {
   *   where: { name: "Pasta" }
   * });
   * if (recipe) {
   *   console.log(recipe.name);
   * }
   * ```
   */
  static async findOne<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
    useSurrealDB: boolean = true,
  ): Promise<T | null> {
    const limitedQuery = { ...query, limit: 1 };
    const results = await this.findAll(perspective, limitedQuery, useSurrealDB);
    return results[0] ?? null;
  }

  /**
   * Gets all instances with count of total matches without offset & limit applied.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSurrealDB - Whether to use SurrealDB (default: true, 10-100x faster) or Prolog (legacy)
   * @returns Object containing results array and total count
   * 
   * @example
   * ```typescript
   * const { results, totalCount } = await Recipe.findAllAndCount(perspective, {
   *   where: { category: "Dessert" },
   *   limit: 10
   * });
   * console.log(`Showing 10 of ${totalCount} dessert recipes`);
   * 
   * // Use Prolog explicitly (legacy)
   * const { results, totalCount } = await Recipe.findAllAndCount(perspective, {}, false);
   * ```
   */
  static async findAllAndCount<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    query: Query = {},
    useSurrealDB: boolean = true
  ): Promise<ResultsWithTotalCount<T>> {
    if (useSurrealDB) {
      const surrealQuery = await this.queryToSurrealQL(perspective, query);
      const result = await perspective.querySurrealDB(surrealQuery);
      return await this.instancesFromSurrealResult(perspective, query, result);
    } else {
      const prologQuery = await this.queryToProlog(perspective, query);
      const result = await perspective.infer(prologQuery);
      return await this.instancesFromPrologResult(perspective, query, result);
    }
  }

  /**
   * Helper function for pagination with explicit page size and number.
   * 
   * @param perspective - The perspective to search in
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param query - Optional additional query parameters
   * @param useSurrealDB - Whether to use SurrealDB (default: true, 10-100x faster) or Prolog (legacy)
   * @returns Paginated results with metadata
   * 
   * @example
   * ```typescript
   * const page = await Recipe.paginate(perspective, 10, 1, {
   *   where: { category: "Main Course" }
   * });
   * console.log(`Page ${page.pageNumber} of recipes, ${page.results.length} items`);
   * 
   * // Use Prolog explicitly (legacy)
   * const pageProlog = await Recipe.paginate(perspective, 10, 1, {}, false);
   * ```
   */
  static async paginate<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    pageSize: number, 
    pageNumber: number, 
    query?: Query,
    useSurrealDB: boolean = true
  ): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(query || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    if (useSurrealDB) {
      const surrealQuery = await this.queryToSurrealQL(perspective, paginationQuery);
      const result = await perspective.querySurrealDB(surrealQuery);
      const { results, totalCount } = await this.instancesFromSurrealResult(perspective, paginationQuery, result);
      return { results, totalCount, pageSize, pageNumber };
    } else {
      const prologQuery = await this.queryToProlog(perspective, paginationQuery);
      const result = await perspective.infer(prologQuery);
      const { results, totalCount } = await this.instancesFromPrologResult(perspective, paginationQuery, result);
      return { results, totalCount, pageSize, pageNumber };
    }
  }

  static async countQueryToProlog(perspective: PerspectiveProxy, query: Query = {}, modelClassName?: string | null) {
    const { where } = query;
    const className = modelClassName || (await this.getClassName(perspective));
    const resolvedParentPredicate = query.parent
      ? resolveParentPredicate(query.parent, this)
      : undefined;
    const instanceQueries = [buildAuthorAndTimestampQuery(), buildParentQuery(query.parent, resolvedParentPredicate), buildWhereQuery(where)];
    const resultSetQueries = [buildCountQuery(true), buildOrderQuery(), buildOffsetQuery(), buildLimitQuery()];

    const fullQuery = `
      findall([Base, Properties, Collections, Timestamp, Author], (
        subject_class("${className}", SubjectClass),
        instance(SubjectClass, Base),
        ${instanceQueries.filter((q) => q).join(", ")}
      ), UnsortedInstances),
      ${resultSetQueries.filter((q) => q).join(", ")}
    `;

    return fullQuery;
  }

  /**
   * Generates a SurrealQL COUNT query for the model.
   * 
   * @param perspective - The perspective context
   * @param query - Query parameters to filter the count
   * @returns SurrealQL COUNT query string
   * 
   * @private
   */
  public static async countQueryToSurrealQL(perspective: PerspectiveProxy, query: Query): Promise<string> {
    // Use the same query as the main query (with GROUP BY), just without LIMIT/OFFSET
    // We'll count the number of rows returned (one row per source)
    const countQuery = { ...query };
    delete countQuery.limit;
    delete countQuery.offset;
    return await this.queryToSurrealQL(perspective, countQuery);
  }

  /**
   * Gets a count of all matching instances.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSurrealDB - Whether to use SurrealDB (default: true, 10-100x faster) or Prolog (legacy)
   * @returns Total count of matching entities
   * 
   * @example
   * ```typescript
   * const totalRecipes = await Recipe.count(perspective);
   * const activeRecipes = await Recipe.count(perspective, {
   *   where: { status: "active" }
   * });
   * 
   * // Use Prolog explicitly (legacy)
   * const countProlog = await Recipe.count(perspective, {}, false);
   * ```
   */
  static async count(perspective: PerspectiveProxy, query: Query = {}, useSurrealDB: boolean = true) {
    if (useSurrealDB) {
      const surrealQuery = await this.queryToSurrealQL(perspective, query);
      const result = await perspective.querySurrealDB(surrealQuery);
      // Use instancesFromSurrealResult to apply JS-level filtering for advanced where conditions
      // (e.g., gt, gte, lt, lte, between, contains on properties and author/timestamp)
      // This ensures count() returns the same number as findAll().length
      const { totalCount } = await this.instancesFromSurrealResult(perspective, query, result);
      return totalCount;
    } else {
      const result = await perspective.infer(await this.countQueryToProlog(perspective, query));
      return result?.[0]?.TotalCount || 0;
    }
  }

  private async setProperty(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getPropertyMetadata(key);
    if (!metadata) {
      console.warn(`Property "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generatePropertySetterAction(key, metadata);

    // Get resolve language from metadata (replaces Prolog query)
    let resolveLanguage = metadata.resolveLanguage;

    // Skip storing empty/null/undefined values to avoid invalid empty literals (e.g. literal://string:)
    if (value === undefined || value === null || value === "") {
      return;
    }

    if (resolveLanguage) {
      value = await this._perspective.createExpression(value, resolveLanguage);
    }

    await this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value }], batchId);
  }

  /** Resolve a relation argument to a plain string ID. Accepts either a raw
   * string ID or an Ad4mModel instance (in which case `.id` is used). */
  private resolveRelationId(value: any): string {
    if (typeof value === 'string') return value;
    if (value && typeof value === 'object' && typeof value.id === 'string') return value.id;
    return String(value);
  }

  private async setRelationValues(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationOptions(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, 'setter');

    if (value != null) {
      if (Array.isArray(value)) {
        await this._perspective.executeAction(
          actions,
          this._baseExpression,
          value.map((v) => ({ name: "value", value: this.resolveRelationId(v) })),
          batchId
        );
      } else {
        await this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value: this.resolveRelationId(value) }], batchId);
      }
    }
  }

  private async addRelationValue(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationOptions(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, 'adder');

    if (value != null) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value: this.resolveRelationId(v) }], batchId)
          )
        );
      } else {
        await this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value: this.resolveRelationId(value) }], batchId);
      }
    }
  }

  private async removeRelationValue(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationOptions(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, 'remover');

    if (value != null) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value: this.resolveRelationId(v) }], batchId)
          )
        );
      } else {
        await this._perspective.executeAction(actions, this._baseExpression, [{ name: "value", value: this.resolveRelationId(value) }], batchId);
      }
    }
  }

  /**
   * Saves the model instance to the perspective.
   *
   * **New instances** (no snapshot yet): creates the subject via
   * `createSubject` with initial scalar values, then sets collections
   * via `innerUpdate`.
   *
   * **Existing instances** (snapshot present, i.e. fetched via `get()`
   * or a query): updates only dirty fields via `innerUpdate`, then
   * refreshes from the perspective.
   * 
   * @param batchId - Optional batch ID for batch operations
   * @throws Will throw if instance creation, linking, or updating fails
   * 
   * @example
   * ```typescript
   * // Create
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * await recipe.save();
   * 
   * // Update
   * recipe.rating = 10;
   * await recipe.save();
   * ```
   */
  async save(batchId?: string) {
    // Existing instance → update path (has been fetched / hydrated before)
    if (this._snapshot) {
      await this.innerUpdate(true, batchId);
      if (batchId) {
        // Batch hasn't been committed yet — getData() would fetch stale data
        // and overwrite in-memory values. Just refresh the snapshot so further
        // saves within the same batch diff correctly.
        this.takeSnapshot();
      } else {
        await this.getData();
      }
      return;
    }

    // New instance → create path
    let batchCreatedHere = false;
    if(!batchId) {
      batchId = await this.perspective.createBatch()
      batchCreatedHere = true;
    }
    

    // First filter out the properties that are not collections (arrays)
    const initialValues = {};
    for (const [key, value] of Object.entries(this)) {
      if (value !== undefined && value !== null && !(Array.isArray(value) && value.length > 0) && !value?.action) {
        initialValues[key] = value;
      }
    }

    // Get the class name instead of passing the instance to avoid Prolog query generation
    const className = await this.perspective.stringOrTemplateObjectToSubjectClassName(this);

    // Create the subject with the initial values
    await this.perspective.createSubject(
      className,
      this._baseExpression,
      initialValues,
      batchId
    );

    // Set collections
    await this.innerUpdate(false, batchId)

    // If we got a batchId passed in, we let the caller decide when to commit.
    // We can't call getData() since the instance won't exist in the perspective
    // until the batch is committed.
    if (batchCreatedHere) {
      await this.perspective.commitBatch(batchId)
      await this.getData();
    } else {
      // Take a snapshot so that a subsequent save() within the same batch
      // routes through the UPDATE path instead of CREATE, avoiding duplicate
      // links (e.g. two competing "title" values).
      this.takeSnapshot();
    }
  }

  private cleanCopy() {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();
    const cleanCopy: Record<string, any> = {};

    // Only include schema-declared fields (properties + relations),
    // preserving internal ORM machinery (_id, _perspective, generated
    // addX/removeX/setX methods, _snapshot, etc.)
    const schemaFields = new Set([
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ]);

    for (const key of schemaFields) {
      const value = (this as any)[key];
      if (value !== undefined && value !== null && key !== "author" && key !== "timestamp") {
        cleanCopy[key] = value;
      }
    }
    return cleanCopy;
  }

  private async innerUpdate(setProperties: boolean = true, batchId?: string) {
    const ctor = this.constructor as typeof Ad4mModel;
    this._subjectClassName = await this._perspective.stringOrTemplateObjectToSubjectClassName(this.cleanCopy());

    // Determine which fields actually changed (skip unchanged when snapshot exists)
    const dirty = this._snapshot ? new Set(this.changedFields()) : null;

    // --- DEBUG: remove after confirming fix ---
    const _className = ctor.name ?? 'unknown';
    console.log(`[Ad4mModel.innerUpdate] ${_className} id=${(this as any).id ?? '?'} dirty=${dirty ? JSON.stringify([...dirty]) : 'ALL (no snapshot)'} setProperties=${setProperties}`);
    // --- END DEBUG ---

    // Only iterate schema-declared fields, not internal ORM properties
    const metadata = ctor.getModelMetadata();
    const schemaFields = new Set([
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ]);

    const entries = Object.entries(this);
    for (const [key, value] of entries) {
      // Only process schema-declared fields
      if (!schemaFields.has(key)) continue;
      // Skip unchanged fields when a snapshot is available
      if (dirty && !dirty.has(key)) continue;

      if (value !== undefined && value !== null) {
        if (value?.action) {
          switch (value.action) {
            case "setter":
              await this.setRelationValues(key, value.value, batchId);
              break;
            case "adder":
              await this.addRelationValue(key, value.value, batchId);
              break;
            case "remover":
              await this.removeRelationValue(key, value.value, batchId);
              break;
            default:
              await this.setRelationValues(key, value.value, batchId);
              break;
          }
        } else if (Array.isArray(value)) {
          // Handle all arrays as relations, including empty ones (which clears the relation)
          await this.setRelationValues(key, value, batchId);
        } else if (value !== undefined && value !== null && value !== "") {
          if (setProperties) {
            // Check if this is a relation property (has relation metadata)
            const relationMeta = this.getRelationOptions(key);
            if (relationMeta) {
              // Skip - it's a relation, not a regular property
              continue;
            }

            // Skip flag properties — they are immutable after creation
            const propMeta = this.getPropertyMetadata(key);
            if (propMeta?.flag) {
              continue;
            }

            await this.setProperty(key, value, batchId);
          }
        }
      }
    }
  }

  /**
   * Gets the model instance with all properties and collections populated.
   *
   * @param optsOrInclude - Optional hydration options. Accepts two forms:
   *   - `GetOptions` wrapper: `{ include: { comments: true }, properties: ['title'] }`
   *   - `IncludeMap` shorthand: `{ comments: true }` (equivalent to `{ include: { comments: true } }`)
   * @returns The populated model instance
   * @throws Will throw if data retrieval fails
   *
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective, existingId);
   * await recipe.get();
   * console.log(recipe.name, recipe.ingredients);
   *
   * // Shorthand — pass IncludeMap directly:
   * await recipe.get({ ingredients: true });
   *
   * // Full options — includes sparse fieldset:
   * await recipe.get({ include: { ingredients: true }, properties: ['name'] });
   * ```
   */
  async get(optsOrInclude?: GetOptions | IncludeMap) {
    // Normalise: if the caller passed a plain IncludeMap shorthand (no `include`
    // or `properties` key at the top level) wrap it in GetOptions automatically.
    let opts: GetOptions | undefined;
    if (!optsOrInclude) {
      opts = undefined;
    } else if ('include' in optsOrInclude || 'properties' in optsOrInclude) {
      opts = optsOrInclude as GetOptions;
    } else {
      opts = { include: optsOrInclude as IncludeMap };
    }

    this._subjectClassName = await this._perspective.stringOrTemplateObjectToSubjectClassName(this.cleanCopy());

    return await this.getData(opts);
  }

  /**
   * Deletes the model instance from the perspective.
   * 
   * @param batchId - Optional batch ID for batch operations
   * @throws Will throw if removal fails
   * 
   * @example
   * ```typescript
   * const recipe = await Recipe.findAll(perspective)[0];
   * await recipe.delete();
   * 
   * // Or with batch operations:
   * const batchId = await perspective.createBatch();
   * await recipe.delete(batchId);
   * await perspective.commitBatch(batchId);
   * ```
   */
  async delete(batchId?: string) {
    // Remove the subject itself (destructor actions)
    await this._perspective.removeSubject(this, this._baseExpression, batchId);

    // Clean up incoming links — remove any links that point **to** this instance
    try {
      const incomingLinks = await this._perspective.get(new LinkQuery({ target: this._baseExpression }));
      if (incomingLinks.length > 0) {
        await this._perspective.removeLinks(incomingLinks, batchId);
      }
    } catch (e) {
      // Non-fatal: the subject was already deleted; incoming link cleanup is best-effort
      console.warn(`delete(): failed to clean up incoming links for ${this._baseExpression}:`, e);
    }
  }

  // ──────────────────────────────────────────────────────────
  //  Static convenience methods
  // ──────────────────────────────────────────────────────────

  /**
   * Options for `Ad4mModel.create()`.
   */
  static readonly CreateOptions: undefined; // type-only anchor for JSDoc

  /**
   * Creates and saves a new model instance in one step.
   *
   * @param perspective - The perspective to create the instance in
   * @param data - Property values to assign before saving
   * @param options - Optional settings:
   *   - `parent` — a `ParentScope` (model form or raw form) whose `id` will
   *     be used to create an incoming link from the parent to the new instance.
   *   - `batchId` — an existing batch id; when provided the link write and
   *     `save()` are added to the batch instead of committed immediately.
   * @returns The saved model instance
   *
   * @example
   * ```typescript
   * // Simple create
   * const recipe = await Recipe.create(perspective, {
   *   name: "Spaghetti",
   *   rating: 5,
   * });
   *
   * // Create under a parent (link auto-created)
   * const comment = await Comment.create(perspective, { text: "Great!" }, {
   *   parent: { model: Post, id: postId },
   * });
   *
   * // Create inside a transaction
   * await Ad4mModel.transaction(perspective, async (tx) => {
   *   await Recipe.create(perspective, { name: "Pasta" }, { batchId: tx.batchId });
   * });
   * ```
   */
  static async create<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    data: Record<string, any> = {},
    options?: { parent?: ParentScope; batchId?: string },
  ): Promise<T> {
    const instance = new this(perspective) as T;
    Object.assign(instance, data);

    // When a parent scope is provided without a caller-supplied batch, open a
    // new batch ourselves so that the instance creation and the parent→child
    // link are committed atomically.  If either step throws, commitBatch is
    // never reached and the batch is implicitly abandoned (rollback).
    if (options?.parent && !options?.batchId) {
      const batchId = await perspective.createBatch();
      await instance.save(batchId);
      const predicate = resolveParentPredicate(options.parent, this);
      const link = new Link({
        source: options.parent.id,
        predicate,
        target: instance.id,
      });
      await perspective.add(link, 'shared', batchId);
      await perspective.commitBatch(batchId);
      // Hydrate the instance now that the batch has been committed (mirrors the
      // behaviour of save() when it manages its own batch).
      await instance.get();
      return instance;
    }

    await instance.save(options?.batchId);

    // Create parent → child link if a parent scope was provided
    if (options?.parent) {
      const predicate = resolveParentPredicate(options.parent, this);
      const link = new Link({
        source: options.parent.id,
        predicate,
        target: instance.id,
      });
      await perspective.add(link, 'shared', options?.batchId);
    }

    return instance;
  }

  /**
   * Updates an existing model instance identified by `id`.
   *
   * Fetches the instance, applies the provided changes, calls `save()`,
   * and returns the refreshed instance.
   *
   * @param perspective - The perspective containing the instance
   * @param id - The expression URI of the instance to update
   * @param data - Property values to merge before saving
   * @returns The updated model instance
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.update(perspective, recipeId, {
   *   rating: 10,
   * });
   * ```
   */
  static async update<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    id: string,
    data: Record<string, any>,
  ): Promise<T> {
    const instance = new this(perspective, id) as T;
    await instance.get();
    Object.assign(instance, data);
    await instance.save();
    return instance;
  }

  /**
   * Deletes an existing model instance identified by `id`.
   *
   * Also cleans up any incoming links that point to this instance.
   *
   * @param perspective - The perspective containing the instance
   * @param id - The expression URI of the instance to delete
   *
   * @example
   * ```typescript
   * await Recipe.delete(perspective, recipeId);
   * ```
   *
   * @deprecated Use the name `delete` — `remove` is preserved as an alias.
   */
  static async remove(
    this: typeof Ad4mModel & (new (...args: any[]) => Ad4mModel),
    perspective: PerspectiveProxy,
    id: string,
  ): Promise<void> {
    return this.delete(perspective, id);
  }

  /**
   * Deletes an existing model instance identified by `id`.
   *
   * Also cleans up any incoming links that point to this instance.
   *
   * @param perspective - The perspective containing the instance
   * @param id - The expression URI of the instance to delete
   *
   * @example
   * ```typescript
   * await Recipe.delete(perspective, recipeId);
   * ```
   */
  static async delete(
    this: typeof Ad4mModel & (new (...args: any[]) => Ad4mModel),
    perspective: PerspectiveProxy,
    id: string,
  ): Promise<void> {
    const instance = new this(perspective, id);
    await instance.delete();
  }

  /**
   * Registers this model's SHACL schema on the given perspective.
   *
   * This ensures the perspective knows about the model's shape
   * (properties, relations, constraints) so instances can be
   * created, queried, and validated.
   *
   * @param perspective - The perspective to register the model on
   *
   * @example
   * ```typescript
   * await Recipe.register(perspective);
   * // Now you can create / query Recipe instances on this perspective
   * ```
   */
  static async register(
    this: typeof Ad4mModel,
    perspective: PerspectiveProxy,
  ): Promise<void> {
    await perspective.ensureSDNASubjectClass(this);
  }

  /**
   * Executes a set of model operations inside a single batch (transaction).
   *
   * All `save`, `update`, and `delete` calls made via the provided `batchId`
   * are buffered and flushed atomically when the callback completes.
   * If the callback throws, the batch is **not** committed, preventing
   * partial writes.
   *
   * @param perspective - The perspective to operate on
   * @param fn - Async callback that receives a `TransactionContext` object.
   *             Pass `tx.batchId` to `save(tx.batchId)`, `update(tx.batchId)`,
   *             `delete(tx.batchId)`, etc.
   * @returns The value returned by `fn`
   *
   * @example
   * ```typescript
   * await Ad4mModel.transaction(perspective, async (tx) => {
   *   const recipe = new Recipe(perspective);
   *   recipe.name = "Spaghetti";
   *   await recipe.save(tx.batchId);
   *
   *   const old = await Recipe.query(perspective).where({ name: "Stale" }).run();
   *   for (const r of old) await r.delete(tx.batchId);
   * });
   * // All changes committed atomically here
   * ```
   */
  static async transaction<R = void>(
    perspective: PerspectiveProxy,
    fn: (tx: { batchId: string }) => Promise<R>,
  ): Promise<R> {
    const batchId = await perspective.createBatch();
    const result = await fn({ batchId });
    await perspective.commitBatch(batchId);
    return result;
  }

  /**
   * Creates a query builder for fluent query construction.
   * 
   * @param perspective - The perspective to query
   * @param query - Optional initial query parameters
   * @returns A new query builder instance
   * 
   * @example
   * ```typescript
   * const recipes = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .order({ rating: "DESC" })
   *   .limit(5)
   *   .run();
   * 
   * // With real-time updates
   * await Recipe.query(perspective)
   *   .where({ status: "cooking" })
   *   .subscribe(recipes => {
   *     console.log("Currently cooking:", recipes);
   *   });
   * ```
   */
  static query<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    query?: Query
  ): ModelQueryBuilder<T> {
    return new ModelQueryBuilder<T>(perspective, this as any, query);
  }

  /**
   * Creates an Ad4mModel class from a JSON Schema definition.
   * 
   * @description
   * This method dynamically generates an Ad4mModel subclass from a JSON Schema,
   * enabling integration with systems that use JSON Schema for type definitions.
   * 
   * The method follows a cascading approach for determining predicates:
   * 1. Explicit configuration in options parameter (highest precedence)
   * 2. x-ad4m metadata in the JSON Schema
   * 3. Inference from schema title and property names
   * 4. Error if no namespace can be determined
   * 
   * @example
   * ```typescript
   * // With explicit configuration
   * const PersonClass = Ad4mModel.fromJSONSchema(schema, {
   *   name: "Person",
   *   namespace: "person://",
   *   resolveLanguage: "literal"
   * });
   * 
   * // With property mapping
   * const ContactClass = Ad4mModel.fromJSONSchema(schema, {
   *   name: "Contact",
   *   namespace: "contact://",
   *   propertyMapping: {
   *     "name": "foaf://name",
   *     "email": "foaf://mbox"
   *   }
   * });
   * 
   * // With x-ad4m metadata in schema
   * const schema = {
   *   "title": "Product",
   *   "x-ad4m": { "namespace": "product://" },
   *   "properties": {
   *     "name": { 
   *       "type": "string",
   *       "x-ad4m": { "through": "product://title" }
   *     }
   *   }
   * };
   * const ProductClass = Ad4mModel.fromJSONSchema(schema, { name: "Product" });
   * ```
   * 
   * @param schema - JSON Schema definition
   * @param options - Configuration options
   * @returns Generated Ad4mModel subclass
   * @throws Error when namespace cannot be inferred
   */
  static fromJSONSchema(
    schema: JSONSchema,
    options: JSONSchemaToModelOptions
  ): typeof Ad4mModel {
    // Disallow top-level "author" property since Ad4mModel provides it implicitly via link authorship
    if (schema?.properties && Object.prototype.hasOwnProperty.call(schema.properties, "author")) {
      throw new Error('JSON Schema must not define a top-level "author" property because Ad4mModel already exposes it. Please rename the property (e.g., "writer").');
    }
    // Determine namespace with cascading precedence
    const namespace = this.determineNamespace(schema, options);
    
    // Create the dynamic class
    const DynamicModelClass = class extends Ad4mModel {};
    
    // Set up class metadata
    if (!options.name || options.name.trim() === '') {
      throw new Error("options.name is required and cannot be empty");
    }
    (DynamicModelClass as any).className = options.name;
    (DynamicModelClass.prototype as any).className = options.name;
    
    // Generate properties and collections metadata
    const properties: any = {};
    const collections: any = {};
    
    if (schema.properties) {
      for (const [propertyName, propertySchema] of Object.entries(schema.properties)) {
        const predicate = this.determinePredicate(schema, propertyName, propertySchema, namespace, options);
        const isRequired = schema.required?.includes(propertyName) || false;
        const propertyType = normalizeSchemaType(propertySchema.type);
        const isArray = isArrayType(propertySchema);
        
        if (isArray) {
          // Handle arrays as collections
          // Store the singular form as the collection key since SDNA generation expects singular
          collections[propertyName] = {
            through: predicate,
            local: this.getPropertyOption(propertyName, propertySchema, options, 'local')
          };
          
          // Define the property on prototype
          Object.defineProperty(DynamicModelClass.prototype, propertyName, {
            configurable: true,
            writable: true,
            value: []
          });
          
          // Add collection methods
          const adderName = `add${capitalize(propertyName)}`;
          const removerName = `remove${capitalize(propertyName)}`;
          const setterName = `set${capitalize(propertyName)}`;
          
          (DynamicModelClass.prototype as any)[adderName] = function() {
            // Placeholder function for SDNA generation
          };
          (DynamicModelClass.prototype as any)[removerName] = function() {
            // Placeholder function for SDNA generation
          };
          (DynamicModelClass.prototype as any)[setterName] = function() {
            // Placeholder function for SDNA generation
          };
          
        } else {
          // Handle regular properties
          let resolveLanguage = this.getPropertyOption(propertyName, propertySchema, options, 'resolveLanguage');
          // If no specific resolveLanguage for this property, use the global one
          if (!resolveLanguage && options.resolveLanguage) {
            resolveLanguage = options.resolveLanguage;
          }
          const local = this.getPropertyOption(propertyName, propertySchema, options, 'local');
          // Determine readOnly: check PropertyOptions first, then x-ad4m.writable (inverted) for JSON Schema wire format
          let readOnly = this.getPropertyOption(propertyName, propertySchema, options, 'readOnly');
          if (readOnly === undefined) {
            const xWritable = propertySchema["x-ad4m"]?.writable;
            readOnly = xWritable !== undefined ? !xWritable : false;
          }
          const writable = !readOnly;
          let initial = this.getPropertyOption(propertyName, propertySchema, options, 'initial');
          
          // Handle nested objects by serializing to JSON
          if (isObjectType(propertySchema) && !resolveLanguage) {
            resolveLanguage = 'literal';
            console.warn(`Property "${propertyName}" is an object type. It will be stored as JSON. Consider flattening complex objects for better semantic querying.`);
          }

          // Ensure numeric properties use literal language for correct typing
          if ((resolveLanguage === undefined || resolveLanguage === null) && isNumericType(propertySchema)) {
            resolveLanguage = 'literal';
          }
          
          // If property is required, ensure it has an initial value
          if (isRequired && !initial) {
            if (isObjectType(propertySchema)) {
              initial = 'literal://json:{}';
            } else {
              initial = "ad4m://undefined";
            }
          }
          
          properties[propertyName] = {
            through: predicate,
            required: isRequired,
            writable: writable,
            ...(resolveLanguage && { resolveLanguage }),
            ...(local !== undefined && { local }),
            ...(initial && { initial })
          };
          
          // Define the property on prototype
          Object.defineProperty(DynamicModelClass.prototype, propertyName, {
            configurable: true,
            writable: true,
            value: this.getDefaultValueForType(propertyType)
          });
          
          // Add setter function if writable
          if (writable) {
            const setterName = propertyNameToSetterName(propertyName);
            (DynamicModelClass.prototype as any)[setterName] = function() {
              // This is a placeholder function that the SDNA generation looks for
              // The actual setter logic is handled by the Ad4mModel base class
            };
          }
        }
      }
    }
    
    // Validate that at least one property has an initial value (needed for valid SDNA constructor)
    // Collections don't create constructor actions, only properties with initial values do
    const hasPropertyWithInitial = Object.values(properties).some((prop: any) => prop.initial);
    
    if (!hasPropertyWithInitial) {
      // If no properties have initial values, add a type identifier automatically
      const typeProperty = `ad4m://type`;
      let typeValue: string;
      if (namespace.includes('://')) {
        const [scheme, rest] = namespace.split('://');
        const path = (rest || '').replace(/\/+$/,'');
        if (path) {
          typeValue = `${scheme}://${path}/instance`;
        } else {
          typeValue = `${scheme}://instance`;
        }
      } else {
        const path = namespace.replace(/\/+$/,'');
        typeValue = `${path}/instance`;
      }
      
      properties['__ad4m_type'] = {
        through: typeProperty,
        required: true,
        writable: false,
        initial: typeValue,
        flag: true
      };
      
      // Add the type property to the prototype
      Object.defineProperty(DynamicModelClass.prototype, '__ad4m_type', {
        configurable: true,
        writable: false,
        value: typeValue
      });
      
      console.warn(`No properties with initial values found. Added automatic type flag: ${typeProperty} = ${typeValue}`);
    }
    
    // Attach metadata to WeakMap registries
    for (const [propName, propMeta] of Object.entries(properties)) {
      setPropertyRegistryEntry(DynamicModelClass, propName, propMeta as any);
    }
    for (const [collName, collMeta] of Object.entries(collections)) {
      setRelationRegistryEntry(DynamicModelClass, collName, {
        predicate: (collMeta as any).through || "",
        kind: 'hasMany',
        ...(( collMeta as any).getter !== undefined && { getter: (collMeta as any).getter }),
        ...(( collMeta as any).local !== undefined && { local: (collMeta as any).local }),
      });
    }
    
    // Store the JSON schema and options on the prototype for potential fallback use by getModelMetadata()
    (DynamicModelClass.prototype as any).__jsonSchema = schema;
    (DynamicModelClass.prototype as any).__jsonSchemaOptions = options;
    
    // Apply the Model decorator to set up the generateSDNA method
    const ModelDecorator = Model({ name: options.name });
    ModelDecorator(DynamicModelClass);
    
    return DynamicModelClass as typeof Ad4mModel;
  }
  
  /**
   * Determines the namespace for predicates using cascading precedence
   */
  private static determineNamespace(schema: JSONSchema, options: JSONSchemaToModelOptions): string {
    // 1. Explicit namespace in options (highest precedence)
    if (options.namespace) {
      return options.namespace;
    }
    
    // 2. x-ad4m metadata in schema
    if (schema["x-ad4m"]?.namespace) {
      return schema["x-ad4m"].namespace;
    }
    
    // 3. Infer from schema title
    if (schema.title) {
      return `${schema.title.toLowerCase()}://`;
    }
    
    // 4. Try to extract from $id if it's a URL
    if (schema.$id) {
      try {
        const url = new URL(schema.$id);
        const pathParts = url.pathname.split('/').filter(p => p);
        if (pathParts.length > 0) {
          const lastPart = pathParts[pathParts.length - 1];
          const baseName = lastPart.replace(/\.schema\.json$/, '').replace(/\.json$/, '');
          return `${baseName.toLowerCase()}://`;
        }
      } catch {
        // If $id is not a valid URL, continue to error
      }
    }
    
    // 5. Error if no namespace can be determined
    throw new Error(
      `Cannot infer namespace for JSON Schema. Please provide one of:
      - options.namespace
      - schema["x-ad4m"].namespace  
      - schema.title
      - valid schema.$id`
    );
  }
  
  /**
   * Determines the predicate for a specific property using cascading precedence
   */
  private static determinePredicate(
    schema: JSONSchema,
    propertyName: string,
    propertySchema: JSONSchemaProperty,
    namespace: string,
    options: JSONSchemaToModelOptions
  ): string {
    // 1. Explicit property mapping (highest precedence)
    if (options.propertyMapping?.[propertyName]) {
      return options.propertyMapping[propertyName];
    }
    
    // 2. x-ad4m metadata in property schema
    if (propertySchema["x-ad4m"]?.through) {
      return propertySchema["x-ad4m"].through;
    }
    
    // 3. Generate from namespace + property name
    if (options.predicateTemplate) {
      const normalizedNs = normalizeNamespaceString(namespace);
      const [scheme, rest] = normalizedNs.includes('://') ? normalizedNs.split('://') : ['', normalizedNs];
      const nsNoScheme = rest || '';
      return options.predicateTemplate
        .replace('${namespace}', nsNoScheme)
        .replace('${scheme}', scheme)
        .replace('${ns}', nsNoScheme)
        .replace('${title}', schema.title || '')
        .replace('${property}', propertyName);
    }
    
    // 4. Custom predicate generator
    if (options.predicateGenerator) {
      return options.predicateGenerator(schema.title || '', propertyName);
    }
    
    // 5. Default: namespace + property name
    const normalizedNs = normalizeNamespaceString(namespace);
    if (normalizedNs.includes('://')) {
      // For namespaces like "product://", append property directly
      return `${normalizedNs}${propertyName}`;
    } else {
      return `${normalizedNs}://${propertyName}`;
    }
  }
  
  /**
   * Gets property-specific options using cascading precedence
   */
  private static getPropertyOption(
    propertyName: string,
    propertySchema: JSONSchemaProperty,
    options: JSONSchemaToModelOptions,
    optionName: keyof PropertyOptions,
    defaultValue?: any
  ): any {
    // 1. Property-specific options
    if (options.propertyOptions?.[propertyName]?.[optionName] !== undefined) {
      return options.propertyOptions[propertyName][optionName];
    }
    
    // 2. x-ad4m metadata in property
    if (propertySchema["x-ad4m"]?.[optionName as keyof JSONSchemaProperty["x-ad4m"]] !== undefined) {
      return propertySchema["x-ad4m"][optionName as keyof JSONSchemaProperty["x-ad4m"]];
    }
    
    // 3. Global option
    if (options[optionName as keyof JSONSchemaToModelOptions] !== undefined) {
      return options[optionName as keyof JSONSchemaToModelOptions];
    }
    
    // 4. Default value
    return defaultValue;
  }
  
  /**
   * Gets default value for a JSON Schema type
   */
  private static getDefaultValueForType(type?: string): any {
    switch (type) {
      case 'string': return '';
      case 'number': return 0;
      case 'integer': return 0;
      case 'boolean': return false;
      case 'array': return [];
      case 'object': return {};
      default: return '';
    }
  }
}

/** Query builder for Ad4mModel queries.
 * Allows building queries with a fluent interface and either running them once
 * or subscribing to updates.
 * 
 * @example
 * ```typescript
 * const builder = Recipe.query(perspective)
 *   .where({ category: "Dessert" })
 *   .order({ rating: "DESC" })
 *   .limit(10);
 * 
 * // Run once
 * const recipes = await builder.run();
 * 
 * // Or subscribe to updates
 * await builder.subscribe(recipes => {
 *   console.log("Updated recipes:", recipes);
 * });
 * ```
 */
export class ModelQueryBuilder<T extends Ad4mModel> {
  private perspective: PerspectiveProxy;
  private queryParams: Query = {};
  private modelClassName: string | null = null;
  private ctor: typeof Ad4mModel;
  private currentSubscription?: any;
  private useSurrealDBFlag: boolean = true;

  constructor(perspective: PerspectiveProxy, ctor: typeof Ad4mModel, query?: Query) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
  }

  /**
   * Disposes of the current subscription if one exists.
   * 
   * This method:
   * 1. Stops the keepalive signals to the subscription
   * 2. Unsubscribes from GraphQL subscription updates
   * 3. Notifies the backend to clean up subscription resources
   * 4. Clears the subscription reference
   * 
   * You should call this method when you're done with a subscription
   * to prevent memory leaks and ensure proper cleanup.
   */
  dispose() {
    if (this.currentSubscription) {
      this.currentSubscription.dispose();
      this.currentSubscription = undefined;
    }
  }

  /**
   * Adds where conditions to the query.
   * 
   * @param conditions - The conditions to filter by
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .where({
   *   category: "Dessert",
   *   rating: { gt: 4 },
   *   tags: ["vegan", "quick"],
   *   published: true
   * })
   * ```
   */
  where(conditions: Where): ModelQueryBuilder<T> {
    this.queryParams.where = conditions;
    return this;
  }

  /**
   * Sets the order for the query results.
   * 
   * @param orderBy - The ordering criteria
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .order({ createdAt: "DESC" })
   * ```
   */
  order(orderBy: Order): ModelQueryBuilder<T> {
    this.queryParams.order = orderBy;
    return this;
  }

  /**
   * Sets the maximum number of results to return.
   * 
   * @param limit - Maximum number of results
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .limit(10)
   * ```
   */
  limit(limit: number): ModelQueryBuilder<T> {
    this.queryParams.limit = limit;
    return this;
  }

  /**
   * Sets the number of results to skip.
   * 
   * @param offset - Number of results to skip
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .offset(20) // Skip first 20 results
   * ```
   */
  offset(offset: number): ModelQueryBuilder<T> {
    this.queryParams.offset = offset;
    return this;
  }

  /**
   * Scopes the query to instances linked from a parent.
   *
   * The predicate is resolved in order of precedence:
   * 1. **Instance only** — the parent's constructor is used as the model
   *    class; its relation metadata is scanned for a relation whose
   *    `target()` matches the queried model class.
   * 2. **Instance + options with `field`** — direct field-name lookup on
   *    the parent model's relation metadata (disambiguates when a parent
   *    has multiple relations targeting the same child class).
   * 3. **String id + model class** — same metadata scan (or field lookup if
   *    options include `field`).
   * 4. **String id + string predicate** — raw escape hatch, no metadata lookup.
   *
   * Passing a plain string id with no second argument throws because the
   * predicate cannot be resolved without a model class.
   *
   * @param idOrInstance - The parent's expression URI **or** an Ad4mModel instance
   * @param modelOrPredicate - A model class (predicate auto-resolved) **or** a raw predicate string
   * @param options - Optional settings: `field` for direct relation-name lookup
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * // Instance — predicate auto-resolved from Cookbook's @HasMany(() => Recipe)
   * Recipe.query(perspective).parent(cookbook).get();
   *
   * // Instance + field — disambiguate when parent has multiple relations to same child
   * Recipe.query(perspective).parent(cookbook, { field: "recipes" }).get();
   *
   * // String id + model class
   * Recipe.query(perspective).parent(cookbookId, Cookbook).get();
   *
   * // String id + model class + field
   * Recipe.query(perspective).parent(cookbookId, Cookbook, { field: "recipes" }).get();
   *
   * // String id + raw predicate (escape hatch)
   * Recipe.query(perspective).parent(cookbookId, "cookbook://recipe").get();
   * ```
   */
  parent(idOrInstance: string | Ad4mModel, modelOrPredicate?: typeof Ad4mModel | string | { field: string }, options?: { field?: string }): ModelQueryBuilder<T> {
    const id = typeof idOrInstance === 'string' ? idOrInstance : idOrInstance.id;

    // Handle options-object as second arg: parent(instance, { field: "recipes" })
    if (typeof modelOrPredicate === 'object' && modelOrPredicate !== null && !('prototype' in modelOrPredicate)) {
      if (typeof idOrInstance === 'string') {
        throw new Error(
          'parent() called with a string id and options object requires a model class as second argument',
        );
      }
      const model = idOrInstance.constructor as typeof Ad4mModel;
      this.queryParams.parent = { id, model, field: (modelOrPredicate as { field: string }).field };
      return this;
    }

    const field = options?.field;

    if (typeof modelOrPredicate === 'string') {
      // Raw predicate string → raw form of ParentScope
      this.queryParams.parent = { id, predicate: modelOrPredicate };
    } else if (typeof modelOrPredicate === 'function') {
      // Model class → model form of ParentScope
      this.queryParams.parent = { id, model: modelOrPredicate, ...(field && { field }) };
    } else if (typeof idOrInstance !== 'string') {
      // Ad4mModel instance — derive model class from constructor
      this.queryParams.parent = { id, model: idOrInstance.constructor as typeof Ad4mModel, ...(field && { field }) };
    } else {
      throw new Error(
        'parent() called with a string id requires a second argument: either a model class or a predicate string',
      );
    }
    return this;
  }

  /**
   * Specifies which properties to include in the results.
   * 
   * @param properties - Array of property names to include
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .properties(["name", "description", "rating"])
   * ```
   */
  properties(properties: string[]): ModelQueryBuilder<T> {
    this.queryParams.properties = properties;
    return this;
  }

  /**
   * Specifies which relations to eager-load (hydrate into model instances).
   *
   * Without `include`, relation fields contain raw expression URIs (strings).
   * With `include`, the URIs are resolved into fully-hydrated model instances
   * using the `target` class declared in the relation decorator.
   *
   * Supports nested includes for multi-level eager loading.
   *
   * @param map - An IncludeMap describing which relations to hydrate
   * @returns The query builder for chaining
   *
   * @example
   * ```typescript
   * // Hydrate comments one level deep
   * const recipes = await Recipe.query(perspective)
   *   .include({ comments: true })
   *   .run();
   * // recipe.comments is now Comment[] (model instances), not string[]
   *
   * // Nested: hydrate comments AND each comment's author
   * const recipes = await Recipe.query(perspective)
   *   .include({ comments: { author: true } })
   *   .run();
   * ```
   */
  include(map: IncludeMap): ModelQueryBuilder<T> {
    this.queryParams.include = map;
    return this;
  }

  overrideModelClassName(className: string): ModelQueryBuilder<T> {
    this.modelClassName = className;
    return this;
  }

  /**
   * Enables or disables SurrealDB query path.
   * 
   * @param enabled - Whether to use SurrealDB (default: true, 10-100x faster) or Prolog (legacy)
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * // Use SurrealDB (default)
   * const recipes = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .useSurrealDB(true)
   *   .get();
   * 
   * // Use Prolog (legacy)
   * const recipesProlog = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .useSurrealDB(false)
   *   .get();
   * ```
   * 
   * @remarks
   * Note: Subscriptions (subscribe(), countSubscribe(), paginateSubscribe()) default to SurrealDB live queries
   * if useSurrealDB(true) is set (default).
   */
  useSurrealDB(enabled: boolean = true): ModelQueryBuilder<T> {
    this.useSurrealDBFlag = enabled;
    return this;
  }

  /**
   * Executes the query once and returns the results.
   * 
   * @returns Array of matching entities
   * 
   * @example
   * ```typescript
   * const recipes = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .get();
   * ```
   */
  async get(): Promise<T[]> {
    let results: T[];
    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, this.queryParams);
      const result = await this.perspective.querySurrealDB(surrealQuery);
      ({ results } = await this.ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result) as { results: T[] });
    } else {
      const query = await this.ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
      const result = await this.perspective.infer(query);
      ({ results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result) as { results: T[] });
    }

    return results;
  }

  /**
   * Returns the first matching instance, or `null` if none match.
   *
   * Internally sets `limit: 1` and delegates to `get()`.
   *
   * @returns The first matching instance, or `null`
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.query(perspective)
   *   .where({ name: "Pasta" })
   *   .first();
   * ```
   */
  async first(): Promise<T | null> {
    this.queryParams.limit = 1;
    const results = await this.get();
    return results[0] ?? null;
  }

  /**
   * Subscribes to the query and receives updates when results change.
   *
   * This method:
   * 1. Creates and initializes a SurrealDB live query subscription (default)
   * 2. Sets up the callback to process future updates
   * 3. Returns the initial results immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param callback - Function to call with updated results
   * @returns Initial results array
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ status: "cooking" });
   *
   * const initialRecipes = await builder.subscribe(recipes => {
   *   console.log("Updated recipes:", recipes);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   * @remarks
   * By default, this uses SurrealDB live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSurrealDB(false)`.
   */
  async subscribe(callback: (results: T[]) => void): Promise<T[]> {
    // Clean up any existing subscription
    this.dispose();

    const ctor = this.ctor;

    if (this.useSurrealDBFlag) {
        const surrealQuery = await ctor.queryToSurrealQL(this.perspective, this.queryParams);
        this.currentSubscription = await this.perspective.subscribeSurrealDB(surrealQuery);

        const processResults = async (result: any) => {
            const { results } = await ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result);
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        
        // Process initial result
        const { results } = await ctor.instancesFromSurrealResult(
            this.perspective, 
            this.queryParams, 
            this.currentSubscription.result
        );
        return results as T[];
    } else {
        const query = await ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
        this.currentSubscription = await this.perspective.subscribeInfer(query);

        const processResults = async (result: AllInstancesResult) => {
            const { results } = await ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        const { results } = await ctor.instancesFromPrologResult(
            this.perspective,
            this.queryParams,
            this.currentSubscription.result
        );
        return results as T[];
    }
  }

  /**
   * Gets the total count of matching entities.
   * 
   * @returns Total count
   * 
   * @example
   * ```typescript
   * const totalDesserts = await Recipe.query(perspective)
   *   .where({ category: "Dessert" })
   *   .count();
   * ```
   */
  async count(): Promise<number> {
    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, this.queryParams);
      const result = await this.perspective.querySurrealDB(surrealQuery);
      // Use instancesFromSurrealResult to apply JS-level filtering for advanced where conditions
      // (e.g., gt, gte, lt, lte, between, contains on properties and author/timestamp)
      // This ensures count() returns the same number as get().length
      const { totalCount } = await this.ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result);
      return totalCount;
    } else {
      const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
      const result = await this.perspective.infer(query);
      return result?.[0]?.TotalCount || 0;
    }
  }

  /**
   * Subscribes to count updates for matching entities.
   *
   * This method:
   * 1. Creates and initializes a SurrealDB live query subscription for the count (default)
   * 2. Sets up the callback to process future count updates
   * 3. Returns the initial count immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param callback - Function to call with updated count
   * @returns Initial count
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ status: "active" });
   *
   * const initialCount = await builder.countSubscribe(count => {
   *   console.log("Active items:", count);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   * @remarks
   * By default, this uses SurrealDB live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSurrealDB(false)`.
   */
  async countSubscribe(callback: (count: number) => void): Promise<number> {
    // Clean up any existing subscription
    this.dispose();

    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, this.queryParams);
      this.currentSubscription = await this.perspective.subscribeSurrealDB(surrealQuery);

      const processResults = async (result: any) => {
        const { totalCount } = await this.ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result);
        callback(totalCount);
      };

      this.currentSubscription.onResult(processResults);
      const { totalCount } = await this.ctor.instancesFromSurrealResult(
        this.perspective, 
        this.queryParams, 
        this.currentSubscription.result
      );
      return totalCount;
    } else {
      const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
      this.currentSubscription = await this.perspective.subscribeInfer(query);

      const processResults = async (result: any) => {
        const newCount = result?.[0]?.TotalCount || 0;
        callback(newCount);
      };

      this.currentSubscription.onResult(processResults);
      return this.currentSubscription.result?.[0]?.TotalCount || 0;
    }
  }

  /**
   * Gets a page of results with pagination metadata.
   * 
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @returns Paginated results with metadata
   * 
   * @example
   * ```typescript
   * const page = await Recipe.query(perspective)
   *   .where({ category: "Main" })
   *   .paginate(10, 1);
   * console.log(`Page ${page.pageNumber}, ${page.results.length} of ${page.totalCount}`);
   * ```
   */
  async paginate(pageSize: number, pageNumber: number): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(this.queryParams || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, paginationQuery);
      const result = await this.perspective.querySurrealDB(surrealQuery);
      const { results, totalCount } = (await this.ctor.instancesFromSurrealResult(this.perspective, paginationQuery, result)) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    } else {
      const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
      const result = await this.perspective.infer(prologQuery);
      const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, result)) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    }
  }

  /**
   * Subscribes to paginated results updates.
   *
   * This method:
   * 1. Creates and initializes a SurrealDB live query subscription for the paginated results (default)
   * 2. Sets up the callback to process future page updates
   * 3. Returns the initial page immediately
   *
   * Remember to call dispose() when you're done with the subscription
   * to clean up resources.
   *
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param callback - Function to call with updated pagination results
   * @returns Initial pagination results
   *
   * @example
   * ```typescript
   * const builder = Recipe.query(perspective)
   *   .where({ category: "Main" });
   *
   * const initialPage = await builder.paginateSubscribe(10, 1, page => {
   *   console.log("Updated page:", page.results);
   * });
   *
   * // When done with subscription:
   * builder.dispose();
   * ```
   *
   * @remarks
   * By default, this uses SurrealDB live queries for real-time updates.
   * Prolog subscriptions remain available via `.useSurrealDB(false)`.
   */
  async paginateSubscribe(
    pageSize: number, 
    pageNumber: number, 
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    // Clean up any existing subscription
    this.dispose();

    const paginationQuery = { ...(this.queryParams || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };

    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, paginationQuery);
      this.currentSubscription = await this.perspective.subscribeSurrealDB(surrealQuery);

      const processResults = async (result: any) => {
        const { results, totalCount } = (await this.ctor.instancesFromSurrealResult(this.perspective, paginationQuery, result)) as ResultsWithTotalCount<T>;
        callback({ results, totalCount, pageSize, pageNumber });
      };

      this.currentSubscription.onResult(processResults);
      const { results, totalCount } = (await this.ctor.instancesFromSurrealResult(this.perspective, paginationQuery, this.currentSubscription.result)) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    } else {
      const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
      this.currentSubscription = await this.perspective.subscribeInfer(prologQuery);

      const processResults = async (r: AllInstancesResult) => {
        const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, r)) as ResultsWithTotalCount<T>;
        callback({ results, totalCount, pageSize, pageNumber });
      };

      this.currentSubscription.onResult(processResults);
      const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, this.currentSubscription.result)) as ResultsWithTotalCount<T>;
      return { results, totalCount, pageSize, pageNumber };
    }
  }
}

// ── Standalone Prolog fact generator ──────────────────────────────────────────

/**
 * Convert a camelCase or PascalCase identifier to snake_case.
 * Examples: "TestPost" -> "test_post", "createdAt" -> "created_at"
 */
function toSnakeCase(str: string): string {
  return str
    .replace(/([A-Z])/g, "_$1")
    .toLowerCase()
    .replace(/^_/, "");
}

function buildInstanceClause(predicateName: string, metadata: ModelMetadata): string | null {
  const props = metadata.properties;
  // Collect flags first — these are the strongest recognizers
  const flags = Object.values(props).filter((p) => p.flag && p.predicate && p.initial);
  if (flags.length > 0) {
    const conditions = flags
      .map((p) => `triple(X, '${p.predicate}', '${p.initial}')`)
      .join(",\n    ");
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }
  // Fallback: required non-flag properties
  const required = Object.values(props).filter((p) => p.required && p.predicate && !p.flag);
  if (required.length > 0) {
    const conditions = required
      .map((p) => `triple(X, '${p.predicate}', _)`)
      .join(",\n    ");
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }
  return null;
}

function buildPropertyClause(modelPredicateName: string, prop: PropertyMetadata): string | null {
  if (prop.flag) return null;
  if (!prop.predicate) return null;
  const clauseName = `${modelPredicateName}_${toSnakeCase(prop.name)}`;
  return `${clauseName}(X, Value) :- triple(X, '${prop.predicate}', Value).`;
}

function buildCollectionClause(modelPredicateName: string, coll: RelationMetadata): string | null {
  if (!coll.predicate) return null;
  const clauseName = `${modelPredicateName}_${toSnakeCase(coll.name)}`;
  if (coll.direction === "reverse") {
    return `${clauseName}(X, Values) :- findall(V, triple(V, '${coll.predicate}', X), Values).`;
  }
  return `${clauseName}(X, Values) :- findall(V, triple(X, '${coll.predicate}', V), Values).`;
}

/**
 * Generate Prolog predicate facts from a model class's decorator metadata.
 *
 * Given a model class decorated with `@Model` (and its `@Flag`, `@Property`,
 * `@HasMany`, `@BelongsToMany` decorators), this function emits a string of
 * Prolog clauses that can be prepended to any `perspective.infer()` call.
 *
 * The generated predicates are:
 * - **Instance recognizer** — `modelName(X)` — matches instances of the model
 * - **Property getters** — `modelName_propName(X, Value)` — one per property
 * - **Collection getters** — `modelName_collName(X, Values)` — one per collection
 *
 * @example
 * ```typescript
 * import { generatePrologFacts } from '@coasys/ad4m';
 *
 * const facts = generatePrologFacts(Poll);
 * const result = await perspective.infer(\`
 *   \${facts}
 *   recent_popular_poll(X) :-
 *     poll(X),
 *     poll_vote_count(X, N), N > 10.
 * \`);
 * ```
 *
 * @param ModelClass - A class decorated with `@Model` that extends `Ad4mModel`
 * @returns A multi-line Prolog string ready for use with `perspective.infer()`
 */
export function generatePrologFacts(ModelClass: typeof Ad4mModel): string {
  const metadata = ModelClass.getModelMetadata();
  const predicateName = toSnakeCase(metadata.className);
  const lines: string[] = [];

  lines.push(`% ${metadata.className} — generated Prolog facts`);

  // Instance recognizer
  const instanceClause = buildInstanceClause(predicateName, metadata);
  if (instanceClause) {
    lines.push("");
    lines.push(`% Instance recognizer`);
    lines.push(instanceClause);
  }

  // Property getters
  const propClauses = Object.values(metadata.properties)
    .map((p) => buildPropertyClause(predicateName, p))
    .filter((c): c is string => c !== null);
  if (propClauses.length > 0) {
    lines.push("");
    lines.push(`% Field getters`);
    lines.push(...propClauses);
  }

  // Relation getters
  const collClauses = Object.values(metadata.relations)
    .map((c) => buildCollectionClause(predicateName, c))
    .filter((c): c is string => c !== null);
  if (collClauses.length > 0) {
    lines.push("");
    lines.push(`% Relation getters`);
    lines.push(...collClauses);
  }

  return lines.join("\n");
}
