import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom, PropertyOptions, CollectionOptions, ModelOptions } from "./decorators";
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

export type Query = {
  source?: string;
  properties?: string[];
  collections?: string[]; // replace with include: Query[]
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
  count?: boolean;
};

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
  /** Whether the property is writable */
  writable: boolean;
  /** Initial value if specified */
  initial?: string;
  /** Language for resolution (e.g., "literal") */
  resolveLanguage?: string;
  /** Custom Prolog getter code */
  getter?: string;
  /** Custom Prolog setter code */
  setter?: string;
  /** Whether stored locally only */
  local?: boolean;
  /** Transform function */
  transform?: (value: any) => any;
  /** Whether this is a flag property */
  flag?: boolean;
}

/**
 * Metadata for a single collection extracted from decorators.
 */
export interface CollectionMetadata {
  /** The collection name */
  name: string;
  /** The predicate URI (through value) */
  predicate: string;
  /** Filter conditions */
  where?: { isInstance?: any; condition?: string };
  /** Whether stored locally only */
  local?: boolean;
}

/**
 * Complete model metadata extracted from decorators.
 */
export interface ModelMetadata {
  /** The model class name from @ModelOptions */
  className: string;
  /** Map of property name to metadata */
  properties: Record<string, PropertyMetadata>;
  /** Map of collection name to metadata */
  collections: Record<string, CollectionMetadata>;
}

function capitalize(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

function buildSourceQuery(source?: string): string {
  // Constrains the query to instances that have the provided source
  if (!source) return "";
  return `triple("${source}", "ad4m://has_child", Base)`;
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

function buildCollectionsQuery(collections?: string[]): string {
  // Gets the name and array of values for all (or some) collections on a Ad4mModel instance
  // If no collections are provided, all are included
  return `
    findall([CollectionName, CollectionValues], (
      % Constrain to specified collections if provided
      ${collections ? `member(CollectionName, [${collections.map((name) => `"${name}"`).join(", ")}]),` : ""}

      collection(SubjectClass, CollectionName),
      collection_getter(SubjectClass, Base, CollectionName, CollectionValues)
    ), Collections)
  `;
}

function buildWhereQuery(where: Where = {}): string {
  // Constrains the query to instances that match the provided where conditions

  function formatValue(value) {
    // Wrap strings in quotes
    return typeof value === "string" ? `"${value}"` : value;
  }

  return (Object.entries(where) as [string, WhereCondition][])
    .map(([key, value]) => {
      const isSpecial = ["base", "author", "timestamp"].includes(key);
      const getter = `resolve_property(SubjectClass, Base, "${key}", Value${key}, _)`;
      // const getter = `property_getter(SubjectClass, Base, "${key}", URI), literal_from_url(URI, V, _)`;
      const field = capitalize(key);

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
  const [propertyName, direction] = Object.entries(order)[0];
  return `sort_instances(UnsortedInstances, "${propertyName}", "${direction}", SortedInstances)`;
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
 * @ModelOptions({ name: "Recipe" })
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
 *   // Collection of comments that are instances of another model
 *   @Collection({
 *     through: "recipe://comment",
 *     where: { isInstance: Comment }
 *   })
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
  #baseExpression: string;
  #subjectClassName: string;
  #source: string;
  #perspective: PerspectiveProxy;
  author: string;
  timestamp: string;

  private static classNamesByClass = new WeakMap<typeof Ad4mModel, { [perspectiveId: string]: string }>();

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
   * Extracts metadata from decorators for query building.
   * 
   * @description
   * This method reads the metadata stored by decorators (@Property, @Collection, etc.)
   * and returns it in a structured format that's easier to work with for query builders
   * and other systems that need to introspect model structure.
   * 
   * The metadata includes:
   * - Class name from @ModelOptions
   * - Property metadata (predicates, types, constraints, etc.)
   * - Collection metadata (predicates, filters, etc.)
   * 
   * For models created via `fromJSONSchema()`, this method will derive metadata from
   * the stored `__properties` and `__collections` structures that were populated during
   * the dynamic class creation. If these structures are empty but a JSON schema was
   * attached to the class, it can fall back to deriving metadata from that schema.
   * 
   * @returns Structured metadata object containing className, properties, and collections
   * @throws Error if the class doesn't have @ModelOptions decorator
   * 
   * @example
   * ```typescript
   * @ModelOptions({ name: "Recipe" })
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
    
    // Validate that the class has @ModelOptions decorator
    // The decorator sets prototype.className, so we check for its existence
    if (!prototype.className || prototype.className === 'Ad4mModel') {
      throw new Error("Model class must be decorated with @ModelOptions");
    }
    
    // Extract className
    const className = prototype.className;
    
    // Extract properties from prototype.__properties
    const propertiesMetadata: Record<string, PropertyMetadata> = {};
    const prototypeProperties = prototype.__properties || {};
    
    for (const [propertyName, opts] of Object.entries(prototypeProperties)) {
      const options = opts as PropertyOptions & { required?: boolean; flag?: boolean };
      propertiesMetadata[propertyName] = {
        name: propertyName,
        predicate: options.through || "",
        required: options.required || false,
        writable: options.writable || false,
        ...(options.initial !== undefined && { initial: options.initial }),
        ...(options.resolveLanguage !== undefined && { resolveLanguage: options.resolveLanguage }),
        ...(options.getter !== undefined && { getter: options.getter }),
        ...(options.setter !== undefined && { setter: options.setter }),
        ...(options.local !== undefined && { local: options.local }),
        ...(options.transform !== undefined && { transform: options.transform }),
        ...(options.flag !== undefined && { flag: options.flag })
      };
    }
    
    // Extract collections from prototype.__collections
    const collectionsMetadata: Record<string, CollectionMetadata> = {};
    const prototypeCollections = prototype.__collections || {};
    
    for (const [collectionName, opts] of Object.entries(prototypeCollections)) {
      const options = opts as CollectionOptions;
      collectionsMetadata[collectionName] = {
        name: collectionName,
        predicate: options.through || "",
        ...(options.where !== undefined && { where: options.where }),
        ...(options.local !== undefined && { local: options.local })
      };
    }
    
    // Fallback: If both structures are empty but a JSON schema is attached, derive from it
    // This handles edge cases where fromJSONSchema() was called but metadata wasn't properly populated
    const hasProperties = Object.keys(propertiesMetadata).length > 0;
    const hasCollections = Object.keys(collectionsMetadata).length > 0;
    const hasMetadata = hasProperties || hasCollections;
    
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
            collectionsMetadata[propertyName] = {
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
              writable: propertySchema["x-ad4m"]?.writable !== false,
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
      collections: collectionsMetadata
    };
  }

  /**
   * Constructs a new model instance.
   * 
   * @param perspective - The perspective where this model will be stored
   * @param baseExpression - Optional unique identifier for this instance
   * @param source - Optional source expression this instance is linked to
   * 
   * @example
   * ```typescript
   * // Create a new recipe with auto-generated base expression
   * const recipe = new Recipe(perspective);
   * 
   * // Create with specific base expression
   * const recipe = new Recipe(perspective, "recipe://chocolate-cake");
   * 
   * // Create with source link
   * const recipe = new Recipe(perspective, undefined, "cookbook://desserts");
   * ```
   */
  constructor(perspective: PerspectiveProxy, baseExpression?: string, source?: string) {
    this.#baseExpression = baseExpression ? baseExpression : Literal.from(makeRandomPrologAtom(24)).toUrl();
    this.#perspective = perspective;
    this.#source = source || "ad4m://self";
  }

  /**
   * Gets the base expression of the subject.
   */
  get baseExpression() {
    return this.#baseExpression;
  }

  /**
   * Protected getter for the perspective.
   * Allows subclasses to access the perspective while keeping it private from external code.
   */
  protected get perspective(): PerspectiveProxy {
    return this.#perspective;
  }

  /**
   * Get property metadata from decorator (Phase 1: Prolog-free refactor)
   * @private
   */
  private getPropertyMetadata(key: string): PropertyOptions | undefined {
    const proto = Object.getPrototypeOf(this);
    return proto.__properties?.[key];
  }

  /**
   * Get collection metadata from decorator (Phase 1: Prolog-free refactor)
   * @private
   */
  private getCollectionMetadata(key: string): CollectionOptions | undefined {
    const proto = Object.getPrototypeOf(this);
    return proto.__collections?.[key];
  }

  /**
   * Generate property setter action from metadata (Phase 1: Prolog-free refactor)
   * Replaces Prolog query: property_setter(C, key, Setter)
   * @private
   */
  private generatePropertySetterAction(key: string, metadata: PropertyOptions): any[] {
    if (metadata.setter) {
      // Custom setter - throw error for now (Phase 2)
      throw new Error(
        `Custom setter for property "${key}" not yet supported without Prolog. ` +
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
  private generateCollectionAction(key: string, actionType: 'adder' | 'remover' | 'setter'): any[] {
    const metadata = this.getCollectionMetadata(key);
    if (!metadata) {
      throw new Error(`Collection "${key}" has no metadata defined`);
    }

    if (!metadata.through) {
      throw new Error(`Collection "${key}" has no 'through' predicate defined`);
    }

    const actionMap = {
      adder: "addLink",
      remover: "removeLink",
      setter: "collectionSetter"
    };

    return [{
      action: actionMap[actionType],
      source: "this",
      predicate: metadata.through,
      target: "value",
      ...(metadata.local && { local: true })
    }];
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
          const transform = instance["__properties"]?.[name]?.transform;
          if (transform && typeof transform === "function") {
            finalValue = transform(finalValue);
          }
          return [name, finalValue];
        })
      )
    );
    // Assign properties to instance
    Object.assign(instance, propsObject);
  }

  private async getData() {
    // Builds an object with the author, timestamp, all properties, & all collections on the Ad4mModel and saves it to the instance
    const subQueries = [buildAuthorAndTimestampQuery(), buildPropertiesQuery(), buildCollectionsQuery()];
    const fullQuery = `
      Base = "${this.#baseExpression}",
      subject_class("${this.#subjectClassName}", SubjectClass),
      ${subQueries.join(", ")}
    `;

    // Try Prolog first
    try {
      const result = await this.#perspective.infer(fullQuery);
      if (result?.[0] && result[0].Properties && result[0].Properties.length > 0) {
        const { Properties, Collections, Timestamp, Author } = result[0];
        const values = [...Properties, ...Collections, ["timestamp", Timestamp], ["author", Author]];
        await Ad4mModel.assignValuesToInstance(this.#perspective, this, values);
        return this;
      }
    } catch (e) {
      console.log(`Prolog getData failed for ${this.#baseExpression}, falling back to SurrealDB`);
    }

    // Fallback to SurrealDB (SdnaOnly mode)
    try {
      const ctor = this.constructor as typeof Ad4mModel;
      const metadata = ctor.getModelMetadata();

      // Query for all links from this specific node (base expression)
      // Using formatSurrealValue to prevent SQL injection by properly escaping the value
      const safeBaseExpression = ctor.formatSurrealValue(this.#baseExpression);
      const linksQuery = `
        SELECT id, predicate, out.uri AS target, author, timestamp
        FROM link
        WHERE in.uri = ${safeBaseExpression}
        ORDER BY timestamp ASC
      `;
      const links = await this.#perspective.querySurrealDB(linksQuery);

      if (links && links.length > 0) {
        let maxTimestamp = null;
        let latestAuthor = null;

        // Process properties
        for (const [propName, propMeta] of Object.entries(metadata.properties)) {
          const matching = links.filter((l: any) => l.predicate === propMeta.predicate);
          if (matching.length > 0) {
            const link = matching[0]; // Take first/latest
            let value = link.target;

            // Track timestamp/author
            if (link.timestamp && (!maxTimestamp || link.timestamp > maxTimestamp)) {
              maxTimestamp = link.timestamp;
              latestAuthor = link.author;
            }

            // Handle resolveLanguage
            if (propMeta.resolveLanguage && propMeta.resolveLanguage !== 'literal') {
              try {
                const expression = await this.#perspective.getExpression(value);
                if (expression) {
                  try {
                    value = JSON.parse(expression.data);
                  } catch {
                    value = expression.data;
                  }
                }
              } catch (e) {
                console.warn(`Failed to resolve expression for ${propName}:`, e);
              }
            } else if (typeof value === 'string' && value.startsWith('literal://')) {
              // Parse literal URL
              try {
                const parsed = Literal.fromUrl(value).get();
                value = parsed.data !== undefined ? parsed.data : parsed;
              } catch (e) {
                // Keep original value
              }
            }

            // Apply transform if exists
            if (propMeta.transform && typeof propMeta.transform === 'function') {
              value = propMeta.transform(value);
            }

            (this as any)[propName] = value;
          }
        }

        // Process collections
        for (const [collName, collMeta] of Object.entries(metadata.collections)) {
          const matching = links.filter((l: any) => l.predicate === collMeta.predicate);
          // Links are already sorted by timestamp ASC from the query, so map preserves order
          (this as any)[collName] = matching.map((l: any) => l.target);
        }

        // Set author and timestamp
        if (latestAuthor) {
          (this as any).author = latestAuthor;
        }
        if (maxTimestamp) {
          (this as any).timestamp = maxTimestamp;
        }
      }
    } catch (e) {
      console.error(`SurrealDB getData also failed for ${this.#baseExpression}:`, e);
    }

    return this;
  }

  // Todo: Only return AllInstances (InstancesWithOffset, SortedInstances, & UnsortedInstances not required)
  public static async queryToProlog(perspective: PerspectiveProxy, query: Query, modelClassName?: string | null) {
    const { source, properties, collections, where, order, offset, limit, count } = query;
    const className = modelClassName || (await this.getClassName(perspective));

    const instanceQueries = [
      buildAuthorAndTimestampQuery(),
      buildSourceQuery(source),
      buildPropertiesQuery(properties),
      buildCollectionsQuery(collections),
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
    const { source, where, order, offset, limit } = query;

    // Build list of graph traversal filters for required predicates
    const graphTraversalFilters: string[] = [];

    // Add source filter if specified (filter to nodes that are children of this source)
    // Source filter means: find targets of 'ad4m://has_child' links from the specified source
    if (source) {
      // Use graph traversal: node must be target of has_child link from source
      graphTraversalFilters.push(
        `count(<-link[WHERE perspective = $perspective AND in.uri = ${this.formatSurrealValue(source)} AND predicate = 'ad4m://has_child']) > 0`
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
      // Check if this is a special field (base, author, timestamp)
      // Note: author and timestamp filtering is done in JavaScript after query
      const isSpecial = ['base', 'author', 'timestamp'].includes(propertyName);

      if (isSpecial) {
        // Skip author and timestamp - they'll be filtered in JavaScript
        // Only handle 'base' (which maps to 'uri') here
        if (propertyName === 'author' || propertyName === 'timestamp') {
          continue; // Skip - will be filtered post-query
        }

        const columnName = 'uri'; // base maps to uri in node table

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
        // Handle regular properties via graph traversal
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
      // Check if this is a special field (base, author, timestamp)
      // Note: author and timestamp filtering is done in JavaScript after GROUP BY
      // because they need to be computed from the grouped links first
      const isSpecial = ['base', 'author', 'timestamp'].includes(propertyName);
      
      if (isSpecial) {
        // Skip author and timestamp - they'll be filtered in JavaScript
        // Only handle 'base' (which maps to 'source') here
        if (propertyName === 'author' || propertyName === 'timestamp') {
          continue; // Skip - will be filtered post-query
        }
        
        const columnName = 'source'; // base maps to source
        
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
    fields.push(`(SELECT VALUE author FROM link WHERE source = source LIMIT 1) AS author`);
    fields.push(`(SELECT VALUE timestamp FROM link WHERE source = source LIMIT 1) AS timestamp`);
    
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
    
    // Always add author and timestamp fields using array::first
    fields.push(`array::first(author) AS author`);
    fields.push(`array::first(timestamp) AS timestamp`);
    
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
    const requestedAttribtes = [...(query?.properties || []), ...(query?.collections || [])];
    const allInstances = await Promise.all(
      result[0].AllInstances.map(async ([Base, Properties, Collections, Timestamp, Author]) => {
        try {
          const instance = new this(perspective, Base) as any;
          // Remove unrequested attributes from instance
          if (requestedAttribtes.length) {
            Object.keys(instance).forEach((key) => {
              if (!requestedAttribtes.includes(key)) delete instance[key];
            });
          }
          // Collect values to assign to instance
          const values = [...Properties, ...Collections, ["timestamp", Timestamp], ["author", Author]];
          await Ad4mModel.assignValuesToInstance(perspective, instance, values);

          return instance;
        } catch (error) {
          console.error(`Failed to process instance ${Base}:`, error);
          // Return null for failed instances - we'll filter these out below
          return null;
        }
      })
    );
    return { results: allInstances.filter((instance) => instance !== null), totalCount: result[0].TotalCount };
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
    const requestedCollections = query?.collections || [];
    
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
        
        // Track the most recent timestamp and corresponding author
        let maxTimestamp = null;
        let latestAuthor = null;
        
        // Process each link (track index for collection ordering)
        for (let linkIndex = 0; linkIndex < links.length; linkIndex++) {
          const link = links[linkIndex];
          const predicate = link.predicate;
          const target = link.target;
          
          // Skip 'None' values
          if (target === 'None') continue;
          
          // Track the most recent timestamp and its author
          if (link.timestamp && (!maxTimestamp || link.timestamp > maxTimestamp)) {
            maxTimestamp = link.timestamp;
            latestAuthor = link.author;
          }
          
          // Find matching property
          let foundProperty = false;
          for (const [propName, propMeta] of Object.entries(metadata.properties)) {
            if (propMeta.predicate === predicate) {
              // For properties, take the first value (or we could use timestamp to get latest)
              // Note: Empty objects {} are truthy, so we need to check for them explicitly
              const currentValue = instance[propName];
              const isEmptyObject = typeof currentValue === 'object' && currentValue !== null && !Array.isArray(currentValue) && Object.keys(currentValue).length === 0;
              if (!currentValue || currentValue === "" || currentValue === 0 || isEmptyObject) {
                let convertedValue = target;
                
                // Only process if target has a value
                if (target !== undefined && target !== null && target !== '') {
                  // Check if we need to resolve a non-literal language expression
                  if (propMeta.resolveLanguage != undefined && propMeta.resolveLanguage !== 'literal' && typeof target === 'string') {
                    // For non-literal languages, resolve the expression via perspective.getExpression()
                    // Note: Literals are already parsed by SurrealDB's fn::parse_literal()
                    try {
                      const expression = await perspective.getExpression(target);
                      if (expression) {
                        // Parse the expression data if it's a JSON string
                        try {
                          convertedValue = JSON.parse(expression.data);
                        } catch {
                          // If parsing fails, use the data as-is
                          convertedValue = expression.data;
                        }
                      }
                    } catch (e) {
                      console.warn(`Failed to resolve expression for ${propName} with target "${target}":`, e);
                      console.warn("Falling back to raw value");
                      convertedValue = target; // Fall back to raw value
                    }
                  } else if (typeof target === 'string' && target.startsWith('literal://')) {
                    // Fallback: If we somehow got a literal URL that wasn't parsed by SurrealDB, parse it now
                    try {
                      const parsed = Literal.fromUrl(target).get();
                      if(parsed.data !== undefined) {
                        convertedValue = parsed.data;
                      } else {
                        convertedValue = parsed;
                      }
                    } catch (e) {
                      // If literal parsing fails, just use the value as-is (don't bail)
                      convertedValue = target;
                    }
                  } else if (typeof target === 'string') {
                    // Type conversion: check the instance property's current type
                    const expectedType = typeof instance[propName];
                    if (expectedType === 'number') {
                      convertedValue = Number(target);
                    } else if (expectedType === 'boolean') {
                      convertedValue = target === 'true' || target === '1';
                    }
                  }
                }

                // Apply transform function if it exists
                if (propMeta.transform && typeof propMeta.transform === 'function') {
                  convertedValue = propMeta.transform(convertedValue);
                }

                instance[propName] = convertedValue;
              }
              foundProperty = true;
              break;
            }
          }
          
          // If not a property, check if it's a collection
          if (!foundProperty) {
            for (const [collName, collMeta] of Object.entries(metadata.collections)) {
              if (collMeta.predicate === predicate) {
                // For collections, accumulate all values with their timestamps and indices for sorting
                if (!instance[collName]) {
                  instance[collName] = [];
                }
                // Initialize timestamp tracking array if not already done
                const timestampsKey = `__${collName}_timestamps`;
                const indicesKey = `__${collName}_indices`;
                if (!instance[timestampsKey]) {
                  instance[timestampsKey] = [];
                }
                if (!instance[indicesKey]) {
                  instance[indicesKey] = [];
                }
                if (!instance[collName].includes(target)) {
                  instance[collName].push(target);
                  instance[timestampsKey].push(link.timestamp || '');
                  // Track original position in the links array for stable sorting
                  instance[indicesKey].push(linkIndex);
                }
                break;
              }
            }
          }
        }
        
        // Set author and timestamp from the most recent link
        if (latestAuthor && maxTimestamp) {
          instance.author = latestAuthor;
          // Convert timestamp to number (milliseconds) if it's an ISO string
          if (typeof maxTimestamp === 'string' && maxTimestamp.includes('T')) {
            instance.timestamp = new Date(maxTimestamp).getTime();
          } else if (typeof maxTimestamp === 'string') {
            // Try to parse as number string
            const parsed = parseInt(maxTimestamp, 10);
            instance.timestamp = isNaN(parsed) ? maxTimestamp : parsed;
          } else {
            instance.timestamp = maxTimestamp;
          }
        }
        
        // Sort collections by timestamp to maintain insertion order
        for (const [collName, collMeta] of Object.entries(metadata.collections)) {
          const timestampsKey = `__${collName}_timestamps`;
          const indicesKey = `__${collName}_indices`;
          if (instance[collName] && instance[timestampsKey]) {
            // Create array of [value, timestamp, index] tuples
            const pairs = instance[collName].map((value: any, index: number) => ({
              value,
              timestamp: instance[timestampsKey][index] || '',
              originalIndex: instance[indicesKey]?.[index] ?? index
            }));
            // Sort by timestamp first, then by original index for stable sorting
            pairs.sort((a, b) => {
              const tsA = String(a.timestamp || '');
              const tsB = String(b.timestamp || '');
              const tsCompare = tsA.localeCompare(tsB);
              if (tsCompare !== 0) return tsCompare;
              // Use original index as tiebreaker for stable sorting
              return a.originalIndex - b.originalIndex;
            });
            // Replace collection with sorted values
            instance[collName] = pairs.map(p => p.value);
            // Clean up temporary arrays
            delete instance[timestampsKey];
            delete instance[indicesKey];
          }
        }
        
        // Filter by requested attributes if specified
        if (requestedProperties.length > 0 || requestedCollections.length > 0) {
          const requestedAttributes = [...requestedProperties, ...requestedCollections];
          Object.keys(instance).forEach((key) => {
            // Keep only requested attributes, plus always keep timestamp and author
            if (!requestedAttributes.includes(key) && key !== 'timestamp' && key !== 'author' && key !== 'baseExpression') {
              delete instance[key];
            }
          });
        }
        
        instances.push(instance);
      } catch (error) {
        console.error(`Failed to process SurrealDB instance ${base}:`, error);
      }
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
      const orderPropName = Object.keys(effectiveOrder)[0];
      const orderDirection = Object.values(effectiveOrder)[0];

      filteredInstances.sort((a: any, b: any) => {
        let aVal = a[orderPropName];
        let bVal = b[orderPropName];

        // Handle undefined values - push them to the end
        if (aVal === undefined && bVal === undefined) return 0;
        if (aVal === undefined) return orderDirection === 'ASC' ? 1 : -1;
        if (bVal === undefined) return orderDirection === 'ASC' ? -1 : 1;

        // Compare values
        let comparison = 0;
        if (typeof aVal === 'number' && typeof bVal === 'number') {
          comparison = aVal - bVal;
        } else if (typeof aVal === 'string' && typeof bVal === 'string') {
          comparison = aVal.localeCompare(bVal);
        } else {
          // Convert to strings for comparison
          comparison = String(aVal).localeCompare(String(bVal));
        }

        return orderDirection === 'DESC' ? -comparison : comparison;
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
    const { source, where } = query;
    const className = modelClassName || (await this.getClassName(perspective));
    const instanceQueries = [buildAuthorAndTimestampQuery(), buildSourceQuery(source), buildWhereQuery(where)];
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

    if (resolveLanguage) {
      value = await this.#perspective.createExpression(value, resolveLanguage);
    }

    await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }], batchId);
  }

  private async setCollectionSetter(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getCollectionMetadata(key);
    if (!metadata) {
      console.warn(`Collection "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateCollectionAction(key, 'setter');

    if (value) {
      if (Array.isArray(value)) {
        await this.#perspective.executeAction(
          actions,
          this.#baseExpression,
          value.map((v) => ({ name: "value", value: v })),
          batchId
        );
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }], batchId);
      }
    }
  }

  private async setCollectionAdder(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getCollectionMetadata(key);
    if (!metadata) {
      console.warn(`Collection "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateCollectionAction(key, 'adder');

    if (value) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }], batchId)
          )
        );
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }], batchId);
      }
    }
  }

  private async setCollectionRemover(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getCollectionMetadata(key);
    if (!metadata) {
      console.warn(`Collection "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateCollectionAction(key, 'remover');

    if (value) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }], batchId)
          )
        );
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }], batchId);
      }
    }
  }

  /**
   * Saves the model instance to the perspective.
   * Creates a new instance with the base expression and links it to the source.
   * 
   * @param batchId - Optional batch ID for batch operations
   * @throws Will throw if instance creation, linking, or updating fails
   * 
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * recipe.ingredients = ["pasta", "tomato sauce"];
   * await recipe.save();
   * 
   * // Or with batch operations:
   * const batchId = await perspective.createBatch();
   * await recipe.save(batchId);
   * await perspective.commitBatch(batchId);
   * ```
   */
  async save(batchId?: string) {
    // We use createSubject's initialValues to set properties (but not collections)
    // We then later use innerUpdate to set collections

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

    // Create the subject with the initial values
    await this.perspective.createSubject(
      this,
      this.#baseExpression,
      initialValues,
      batchId
    );

    // Link the subject to the source
    await this.#perspective.add(
      new Link({ source: this.#source, predicate: "ad4m://has_child", target: this.baseExpression }),
      'shared',
      batchId
    );

    // Set collections
    await this.innerUpdate(false, batchId)

    // If we got a batchId passed in, we let the caller decide when to commit.
    // But then we can call getData() since the instance won't exist in the perspective
    // until the bacht is committedl
    if (batchCreatedHere) {
      await this.perspective.commitBatch(batchId)
      await this.getData();
    }
  }

  private cleanCopy() {
    const cleanCopy = {};
    const props = Object.entries(this);
    for (const [key, value] of props) {
      if (value !== undefined && value !== null && key !== "author" && key !== "timestamp") {
        cleanCopy[key] = value;
      }
    }
    return cleanCopy;
  }

  private async innerUpdate(setProperties: boolean = true, batchId?: string) {
    this.#subjectClassName = await this.#perspective.stringOrTemplateObjectToSubjectClassName(this.cleanCopy());

    const entries = Object.entries(this);
    for (const [key, value] of entries) {
      if (value !== undefined && value !== null) {
        if (value?.action) {
          switch (value.action) {
            case "setter":
              await this.setCollectionSetter(key, value.value, batchId);
              break;
            case "adder":
              await this.setCollectionAdder(key, value.value, batchId);
              break;
            case "remover":
              await this.setCollectionRemover(key, value.value, batchId);
              break;
            default:
              await this.setCollectionSetter(key, value.value, batchId);
              break;
          }
        } else if (Array.isArray(value)) {
          // Handle all arrays as collections, even empty ones
          if (value.length > 0) {
            await this.setCollectionSetter(key, value, batchId);
          }
          // Skip empty arrays - don't try to set them as properties
        } else if (value !== undefined && value !== null && value !== "") {
          if (setProperties) {
            // Check if this is a collection property (has collection metadata)
            const collMetadata = this.getCollectionMetadata(key);
            if (collMetadata) {
              // Skip - it's a collection, not a regular property
              continue;
            }
            await this.setProperty(key, value, batchId);
          }
        }
      }
    }
  }

  /**
   * Updates the model instance's properties and collections.
   * 
   * @param batchId - Optional batch ID for batch operations
   * @throws Will throw if property setting or collection updates fail
   * 
   * @example
   * ```typescript
   * const recipe = await Recipe.findAll(perspective)[0];
   * recipe.rating = 5;
   * recipe.ingredients.push("garlic");
   * await recipe.update();
   * 
   * // Or with batch operations:
   * const batchId = await perspective.createBatch();
   * await recipe.update(batchId);
   * await perspective.commitBatch(batchId);
   * ```
   */
  async update(batchId?: string) {
    await this.innerUpdate(true, batchId);
    await this.getData();
  }

  /**
   * Gets the model instance with all properties and collections populated.
   * 
   * @returns The populated model instance
   * @throws Will throw if data retrieval fails
   * 
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective, existingId);
   * await recipe.get();
   * console.log(recipe.name, recipe.ingredients);
   * ```
   */
  async get() {
    this.#subjectClassName = await this.#perspective.stringOrTemplateObjectToSubjectClassName(this.cleanCopy());

    return await this.getData();
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
    await this.#perspective.removeSubject(this, this.#baseExpression, batchId);
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
          const setterName = `setCollection${capitalize(propertyName)}`;
          
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
          const writable = this.getPropertyOption(propertyName, propertySchema, options, 'writable', true);
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
    
    // Attach metadata to prototype
    (DynamicModelClass.prototype as any).__properties = properties;
    (DynamicModelClass.prototype as any).__collections = collections;
    
    // Store the JSON schema and options on the prototype for potential fallback use by getModelMetadata()
    (DynamicModelClass.prototype as any).__jsonSchema = schema;
    (DynamicModelClass.prototype as any).__jsonSchemaOptions = options;
    
    // Apply the ModelOptions decorator to set up the generateSDNA method
    const ModelOptionsDecorator = ModelOptions({ name: options.name });
    ModelOptionsDecorator(DynamicModelClass);
    
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
   * Sets the source filter for the query.
   * 
   * @param source - The source to filter by
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .source("ad4m://self")
   * ```
   */
  source(source: string): ModelQueryBuilder<T> {
    this.queryParams.source = source;
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
   * Specifies which collections to include in the results.
   * 
   * @param collections - Array of collection names to include
   * @returns The query builder for chaining
   * 
   * @example
   * ```typescript
   * .collections(["ingredients", "steps"])
   * ```
   */
  collections(collections: string[]): ModelQueryBuilder<T> {
    this.queryParams.collections = collections;
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
    if (this.useSurrealDBFlag) {
      const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, this.queryParams);
      const result = await this.perspective.querySurrealDB(surrealQuery);
      const { results } = await this.ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result);
      return results as T[];
    } else {
      const query = await this.ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
      const result = await this.perspective.infer(query);
      const { results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
      return results as T[];
    }
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

    if (this.useSurrealDBFlag) {
        const surrealQuery = await this.ctor.queryToSurrealQL(this.perspective, this.queryParams);
        this.currentSubscription = await this.perspective.subscribeSurrealDB(surrealQuery);

        const processResults = async (result: any) => {
            // The result from live query subscription update (handled in PerspectiveInstance listener)
            // is the new full set of results (because we re-query in Rust).
            // So we just need to map it to instances.
            const { results } = await this.ctor.instancesFromSurrealResult(this.perspective, this.queryParams, result);
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        
        // Process initial result
        const { results } = await this.ctor.instancesFromSurrealResult(
            this.perspective, 
            this.queryParams, 
            this.currentSubscription.result
        );
        return results as T[];
    } else {
        // Note: Subscriptions currently only work with Prolog
        const query = await this.ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
        this.currentSubscription = await this.perspective.subscribeInfer(query);

        const processResults = async (result: AllInstancesResult) => {
            const { results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
            callback(results as T[]);
        };

        this.currentSubscription.onResult(processResults);
        const { results } = await this.ctor.instancesFromPrologResult(
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
