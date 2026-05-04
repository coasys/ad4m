import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { LinkQuery } from "../perspectives/LinkQuery";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomId } from "./util";
import { getPropertiesMetadata, getRelationsMetadata, buildConformanceFilter, setPropertyRegistryEntry, setRelationRegistryEntry, Model } from "./decorators";
import type { PropertyOptions, PropertyMetadataEntry, RelationMetadataEntry } from "./decorators";
import { formatQueryValue, compileWhereClause } from "./query-utils";
import { resolveParentPredicate } from "./query-common";
import { isArrayType, determinePredicate, determineNamespace, buildModelFromJSONSchema } from "./json-schema";
import type { SHACLShape } from "../shacl/SHACLShape";
import type { JSONSchemaProperty, JSONSchema, JSONSchemaToModelOptions } from "./json-schema";

import { buildSPARQLQuery } from "./query-sparql";
import { ModelQueryBuilder } from "./ModelQueryBuilder";
import {
  normalizeValue,
  evaluateCustomGettersForInstance,
} from "./hydration";
import type {
  ParentScope, IncludeMap, Query,
  GetOptions, AllInstancesResult, ResultsWithTotalCount,
  PaginationResult, PropertyMetadata, RelationMetadata, ModelMetadata,
} from "./types";

// ---------------------------------------------------------------------------
// Helpers for Rust-side include resolution
// ---------------------------------------------------------------------------

/**
 * Recursively enrich relation metadata in the shape with target class shapes
 * so the Rust endpoint can resolve includes in-process (no extra GraphQL
 * round-trips per relation).
 */
function enrichShapeForIncludes(
  metadata: ModelMetadata,
  include: IncludeMap,
  allRelMeta: Record<string, RelationMetadataEntry>,
): void {
  for (const [relName, includeVal] of Object.entries(include)) {
    if (!includeVal) continue;
    const meta = allRelMeta[relName];
    if (!meta?.target) continue;

    const TargetClass = meta.target() as typeof Ad4mModel;
    // Deep-copy so we don't mutate the cached metadata
    const targetMeta = JSON.parse(JSON.stringify(TargetClass.getModelMetadata()));

    // Ensure the relation is in metadata.relations (fallback for edge cases)
    if (!metadata.relations[relName]) {
      metadata.relations[relName] = {
        name: relName,
        predicate: meta.predicate,
        direction:
          meta.kind === 'belongsToMany' || meta.kind === 'belongsToOne'
            ? 'reverse'
            : 'forward',
      };
    }
    const rel = metadata.relations[relName] as any;
    rel.kind = meta.kind;
    rel.maxCount = meta.maxCount;
    rel.targetShape = targetMeta;
    rel.targetClassName = targetMeta.className;

    // Recurse for nested includes
    const nested =
      typeof includeVal === 'object' && includeVal !== null
        ? (includeVal as any).include
        : undefined;
    if (nested) {
      const targetRelMeta = getRelationsMetadata(TargetClass as any);
      enrichShapeForIncludes(targetMeta, nested, targetRelMeta);
    }
  }
}

/**
 * Construct a model class instance from a plain JSON object returned by the
 * Rust endpoint.  Recursively converts included relation values (which come
 * back as plain JSON objects) into proper model class instances.
 */
function jsonToModelInstance<T extends Ad4mModel>(
  ModelClass: typeof Ad4mModel & (new (...args: any[]) => T),
  perspective: PerspectiveProxy,
  json: any,
  include?: IncludeMap,
  properties?: string[],
): T {
  const instance = new ModelClass(perspective, json.id || json.baseExpression) as any;

  // When a properties projection is active, remove own properties set by the
  // constructor that are not in the projected JSON.  This ensures assertions
  // like `expect(r).to.not.have.own.property("body")` pass.
  if (properties) {
    const jsonKeys = new Set(Object.keys(json));
    for (const key of Object.getOwnPropertyNames(instance)) {
      // Keep internal fields (backing store for getters), id, baseExpression, perspective
      if (key === 'id' || key === 'baseExpression' || key === 'perspective' || key.startsWith('_')) continue;
      if (!jsonKeys.has(key)) {
        delete instance[key];
      }
    }
  }

  for (const [key, value] of Object.entries(json)) {
    if (key === 'baseExpression' || key === 'id') continue;

    // Skip getter-only properties anywhere in the prototype chain
    let isReadonly = false;
    let proto = Object.getPrototypeOf(instance);
    while (proto) {
      const desc = Object.getOwnPropertyDescriptor(proto, key);
      if (desc) { isReadonly = !!(desc.get && !desc.set); break; }
      proto = Object.getPrototypeOf(proto);
    }
    if (isReadonly) continue;

    // Parse ISO timestamps to epoch milliseconds (matches hydrateFromLinks behaviour)
    if ((key === 'createdAt' || key === 'updatedAt') && typeof value === 'string' && value.includes('T')) {
      instance[key] = new Date(value).getTime();
      continue;
    }

    instance[key] = value;
  }

  // Apply property transform functions from decorators (only for literal-resolved
  // or no-resolveLanguage properties; non-literal properties are resolved + transformed
  // asynchronously in executeModelQuery).
  try {
    const propsMeta = getPropertiesMetadata(ModelClass as any);
    for (const [propName, opts] of Object.entries(propsMeta)) {
      const o = opts as any;
      if (typeof o.transform !== 'function' || !(propName in json)) continue;
      // Skip non-literal resolveLanguage props — they need async expression resolution first
      if (o.resolveLanguage != null && o.resolveLanguage !== 'literal') continue;
      instance[propName] = o.transform(instance[propName]);
    }
  } catch (_) { /* no metadata available */ }

  // Recursively convert included relation values to class instances
  if (include) {
    const relMeta = getRelationsMetadata(ModelClass as any);
    for (const [relName, includeVal] of Object.entries(include)) {
      if (!includeVal) continue;
      const meta = relMeta[relName];
      if (!meta?.target) continue;
      const TargetClass = meta.target() as any;
      const nestedInclude =
        typeof includeVal === 'object' && includeVal !== null
          ? (includeVal as any).include
          : undefined;
      const nestedProperties =
        typeof includeVal === 'object' && includeVal !== null
          ? (includeVal as any).properties
          : undefined;

      const raw = instance[relName];
      if (Array.isArray(raw)) {
        instance[relName] = raw.map((item: any) => {
          if (typeof item === 'object' && item !== null && item.id) {
            return jsonToModelInstance(TargetClass, perspective, item, nestedInclude, nestedProperties);
          }
          return item;
        });
      } else if (typeof raw === 'object' && raw !== null && raw.id) {
        instance[relName] = jsonToModelInstance(
          TargetClass, perspective, raw, nestedInclude, nestedProperties,
        );
      }
    }
  }

  return instance;
}

/**
 * Base class for defining data models in AD4M.
 * 
 * @description
 * Ad4mModel provides the foundation for creating data models that are stored in AD4M perspectives.
 * Each model instance is represented as a subgraph in the perspective, with properties and relations
 * mapped to links in that graph. The class uses Prolog-based queries to efficiently search and filter
 * instances based on their properties and relationships.
 * 
 * Key concepts:
 * - Each model instance has a unique base expression that serves as its identifier
 * - Properties are stored as links with predicates defined by the `through` option
 * - Relations represent one-to-many relationships as sets of links
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
 *   // Relation of ingredients
   *   @HasMany({ through: "recipe://ingredient" })
   *   ingredients: string[] = [];
 * 
 *   // Relation of comments linked to another model
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
  private _perspective: PerspectiveProxy;
  private _snapshot: Record<string, any> | null = null;
  author: string;
  createdAt: any;
  updatedAt: any;

  /**
   * Backwards compatibility alias for createdAt.
   * @deprecated Use createdAt instead. This will be removed in a future version.
   */
  get timestamp(): any {
    return (this as any).createdAt;
  }

  private static classNamesByClass = new WeakMap<typeof Ad4mModel, { [perspectiveId: string]: string }>();

  /**
   * Generates the SDNA (Subject DNA) Prolog rules for this model class.
   * Injected at class-definition time by the `@Model` decorator.
   * Returns a default value on un-decorated base classes.
   */
  static generateSDNA(): { sdna: string; name: string } {
    return { sdna: '', name: '' };
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
   * Extracts metadata from decorators for query building.
   * 
   * @description
   * This method reads the metadata stored by decorators (@Property, @HasMany, etc.)
   * and returns it in a structured format that's easier to work with for query builders
   * and other systems that need to introspect model structure.
   * 
   * The metadata includes:
   * - Class name from @Model
   * - Property metadata (predicates, types, constraints, etc.)
   * - Relation metadata (predicates, filters, etc.)
   * 
   * For models created via `fromJSONSchema()`, this method will derive metadata from
   * the WeakMap registries that were populated during the dynamic class creation.
   * If these structures are empty but a JSON schema was attached to the class,
   * it can fall back to deriving metadata from that schema.
   * 
   * @returns Structured metadata object containing className, properties, and relations
   * @throws Error if the class doesn't have @Model decorator
   * 
   * @example
   * ```typescript
   * @Model({ name: "Recipe" })
   * class Recipe extends Ad4mModel {
   *   @Property({ through: "recipe://name", resolveLanguage: "literal" })
   *   name: string = "";
   *   
   *   @HasMany({ through: "recipe://ingredient" })
   *   ingredients: string[] = [];
   * }
   * 
   * const metadata = Recipe.getModelMetadata();
   * console.log(metadata.className); // "Recipe"
   * console.log(metadata.properties.name.predicate); // "recipe://name"
   * console.log(metadata.relations.ingredients.predicate); // "recipe://ingredient"
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
    
    // Extract relations (relations) from WeakMap registry
    const relationsMetadata: Record<string, RelationMetadata> = {};
    const allRelationsMeta = getRelationsMetadata(this as any);
    const prototypeRelations = allRelationsMeta;
    
    for (const [relationName, opts] of Object.entries(prototypeRelations)) {
      const options = opts as RelationMetadataEntry;
      relationsMetadata[relationName] = {
        name: relationName,
        predicate: options.predicate || "",
        ...(options.local !== undefined && { local: options.local }),
        ...(options.getter !== undefined && { getter: options.getter }),
        direction: (options.kind === 'belongsToMany' || options.kind === 'belongsToOne') ? 'reverse' : 'forward',
        ...(options.kind !== undefined && { kind: options.kind }),
        ...(options.maxCount !== undefined && { maxCount: options.maxCount }),
        ...(options.target !== undefined && { target: options.target }),
        ...(options.filter !== undefined && { filter: options.filter }),
        ...(options.where !== undefined && { where: options.where }),
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
          const predicate = determinePredicate(
            schema, 
            propertyName, 
            propertySchema as JSONSchemaProperty, 
            determineNamespace(schema, options),
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
   * const recipe = new Recipe(perspective, "literal:...");
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
   * Protected getter for the perspective.
   * Allows subclasses to access the perspective while keeping it private from external code.
   */
  protected get perspective(): PerspectiveProxy {
    return this._perspective;
  }

  /**
   * Get property metadata from decorator.
   * @private
   */
  private getPropertyMetadata(key: string): PropertyMetadataEntry | undefined {
    const ctor = this.constructor;
    const props = getPropertiesMetadata(ctor);
    return props[key];
  }

  /**
   * Get relation options from decorator
   * @private
   */
  private getRelationOptions(key: string): RelationMetadataEntry | undefined {
    const ctor = this.constructor;
    const rels = getRelationsMetadata(ctor);
    return rels[key];
  }

  /**
   * Generate property setter action from metadata.
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
   * Generate relation action from metadata.
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
  //  Snapshot / dirty tracking
  // ──────────────────────────────────────────────────────────

  /**
   * @param includedRelations  Controls which relation fields are recorded
   *   in the snapshot for dirty-tracking:
   *     • `undefined` (default) — snapshot ALL relations (used by `.get()`,
   *       `.create()`, `.save()` etc. where full hydration has occurred).
   *     • `IncludeMap` object (e.g. `{ views: true }`) — only snapshot the
   *       relations named in the map.  Fields not listed are omitted from
   *       the snapshot so that `changedFields()` ignores them.
   *     • `null` / empty object — skip ALL relations (used by bare
   *       subscriptions that don't eagerly load relations).
   */
  private takeSnapshot(includedRelations?: Record<string, any> | null): void {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();
    const snap: Record<string, any> = {};

    // Always snapshot properties
    for (const propName of Object.keys(metadata.properties)) {
      const val = (this as any)[propName];
      snap[propName] = normalizeValue(
        Array.isArray(val) ? [...val] : val,
      );
    }

    // Snapshot relations only when appropriate:
    //   undefined → all relations (backward compat)
    //   IncludeMap → only the keys present in the map
    //   null / {} → none
    if (includedRelations === undefined) {
      // Full snapshot — e.g. after getData() / create()
      for (const relName of Object.keys(metadata.relations)) {
        const val = (this as any)[relName];
        snap[relName] = normalizeValue(
          Array.isArray(val) ? [...val] : val,
        );
      }
    } else if (includedRelations && Object.keys(includedRelations).length > 0) {
      // Partial snapshot — only the explicitly included relations
      for (const relName of Object.keys(includedRelations)) {
        if (relName in metadata.relations) {
          const val = (this as any)[relName];
          snap[relName] = normalizeValue(
            Array.isArray(val) ? [...val] : val,
          );
        }
      }
    }
    // else: null or empty object → skip all relations

    this._snapshot = snap;
  }

  /**
   * Returns `true` if any tracked property or relation has changed
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
   * Returns the names of properties/relations that differ from the
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
      // Skip fields that were not recorded in the snapshot (e.g. relation
      // fields omitted because the originating query had no `include`).
      // Without this guard, a relation populated by hydrateFromLinks with
      // raw string IDs would appear "dirty" against a missing snapshot
      // entry and trigger an unnecessary (and potentially destructive)
      // setRelationValues call during innerUpdate.
      if (!(field in this._snapshot)) continue;

      const current = normalizeValue((this as any)[field]);
      const original = this._snapshot[field];

      if (Array.isArray(current) || Array.isArray(original)) {
        // Order-insensitive comparison (sorted) so reordering alone
        // doesn't mark a relation as dirty.
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
    // Route through the Rust model query endpoint — same pipeline as executeModelQuery
    // but for a single instance by ID.
    try {
      const ctor = this.constructor as typeof Ad4mModel;
      const query: Query = {
        where: { id: this._baseExpression },
        limit: 1,
      };
      if (opts?.properties) query.properties = opts.properties;
      if (opts?.include) query.include = opts.include;

      const { results } = await (ctor as any).executeModelQuery(
        this._perspective, query, null
      );

      if (results.length > 0) {
        const hydrated = results[0];
        // Copy all hydrated values from the Rust-side result onto this instance
        for (const [key, value] of Object.entries(hydrated as any)) {
          if (key === '_baseExpression' || key === '_perspective' || key === '_snapshot') continue;
          if (key.startsWith('_')) continue;
          // Check for readonly getters
          let isReadonly = false;
          let proto = Object.getPrototypeOf(this);
          while (proto) {
            const desc = Object.getOwnPropertyDescriptor(proto, key);
            if (desc) { isReadonly = !!(desc.get && !desc.set); break; }
            proto = Object.getPrototypeOf(proto);
          }
          if (isReadonly) continue;
          (this as any)[key] = value;
        }
      }

      // Property getters and relation conformance getters are now evaluated
      // Rust-side via evaluate_getters() in model_query.rs.
    } catch (e) {
      console.error(`getData via Rust model query failed for ${this._baseExpression}:`, e);
    }

    this.takeSnapshot(opts?.include);
    return this;
  }

  /**
   * Generates a SPARQL query from a Query object.
   * 
   * @description
   * This method translates high-level query parameters into a SPARQL query string
   * that can be executed against the SPARQL backend. Unlike Prolog queries which
   * operate on SDNA-aware predicates, SPARQL queries operate directly on raw links
   * stored in SPARQL.
   * 
   * The generated query uses a CTE (Common Table Expression) pattern:
   * 1. First, identify candidate base expressions by filtering links based on where conditions
   * 2. Then, for each candidate base, resolve properties and relations via subqueries
   * 3. Finally, apply ordering, pagination (LIMIT/START) at the SQL level
   * 
   * Key architectural notes:
   * - SPARQL stores only raw links (source, predicate, target, author, timestamp)
   * - No SDNA knowledge at the database level
   * - Properties are resolved via subqueries that look for links with specific predicates
   * - Relations are similar but return multiple values instead of one
   * - Special fields (base, author, timestamp) are accessed directly, not via subqueries
   * 
   * @param perspective - The perspective to query (used for metadata extraction)
   * @param query - Query parameters (where, order, limit, offset, properties, relations)
   * @returns Complete SPARQL query string ready for execution
   * 
   * @example
   * ```typescript
   * const query = Recipe.queryToSPARQL(perspective, {
   *   where: { name: "Pasta", rating: { gt: 4 } },
   *   order: { timestamp: "DESC" },
   *   limit: 10
   * });
   * // Returns: SELECT source AS base, array::first(target[WHERE predicate = ...]) AS name, ...
   * //          FROM link WHERE ... GROUP BY source ORDER BY timestamp DESC LIMIT 10
   * ```
   */
  public static async queryToSPARQL(perspective: PerspectiveProxy, query: Query): Promise<string> {
    const metadata = this.getModelMetadata();
    const allRelMeta = getRelationsMetadata(this as any);
    return buildSPARQLQuery(metadata, allRelMeta, query, this);
  }

  /**
   * Build the JSON parameters needed for a model query/subscription endpoint.
   * Returns the className, queryJson, and shapeJson that the Rust executor expects.
   * @internal
   */
  static prepareModelQueryParams(
    query: Query = {},
    classNameOverride?: string | null,
  ): { className: string; queryJson: string; shapeJson: string } {
    const metadata = this.getModelMetadata();
    const className = classNameOverride || metadata.className;

    const queryInput: any = {};
    if (query.parent) {
      const parentPredicate = resolveParentPredicate(query.parent, this);
      queryInput.parent = { id: query.parent.id, predicate: parentPredicate };
    }
    if (query.properties) queryInput.properties = query.properties;
    if (query.include) queryInput.include = query.include;
    if (query.where) queryInput.where = query.where;
    if (query.order) {
      queryInput.order = Object.entries(query.order).map(([k, v]) => [k, v]);
    }
    if (query.offset !== undefined) queryInput.offset = query.offset;
    if (query.limit !== undefined) queryInput.limit = query.limit;
    if (query.count !== undefined) queryInput.count = query.count;

    if (query.include) {
      const allRelMeta = getRelationsMetadata(this as any);
      enrichShapeForIncludes(metadata, query.include, allRelMeta);
    }

    // Pre-compute conformance getters (with where-clause support)
    {
      const allRelMeta = getRelationsMetadata(this as any);
      for (const [relName, relMeta] of Object.entries(metadata.relations)) {
        const rel = relMeta as any;
        if (rel.getter || rel.direction === 'reverse' || rel.filter === false) continue;
        const meta = allRelMeta[relName];
        if (!meta?.target) continue;
        try {
          const TargetClass = meta.target();
          const filter = buildConformanceFilter(meta.predicate, TargetClass);

          // Only compile where clauses to SPARQL when the target properties
          // are stored as plain IRIs (not signed expressions). Properties
          // without resolveLanguage or with resolveLanguage='literal' store
          // values as signed JSON envelopes, so SPARQL exact-match fails.
          let whereConditions: string[] = [];
          if (rel.where) {
            try {
              const targetMetadata = (TargetClass as any).getModelMetadata?.() ?? null;
              let allSparqlFilterable = true;
              if (targetMetadata) {
                for (const propName of Object.keys(rel.where)) {
                  if (['id', 'author', 'timestamp'].includes(propName)) continue;
                  const propMeta = targetMetadata.properties[propName];
                  if (propMeta && (!propMeta.resolveLanguage || propMeta.resolveLanguage === 'literal')) {
                    allSparqlFilterable = false;
                    break;
                  }
                }
              } else {
                allSparqlFilterable = false;
              }
              if (allSparqlFilterable) {
                whereConditions = compileWhereClause(rel.where, targetMetadata);
              }
            } catch (_) {}
          }

          if (filter) {
            let getter = filter.getter;
            if (whereConditions.length > 0) {
              getter = getter.replace(/ \}$/, ` ${whereConditions.join(' ')} }`);
            }
            rel.getter = getter;
          } else if (whereConditions.length > 0) {
            const escapedPred = meta.predicate.replace(/[<>"{}|\\^`\u0000-\u0020]/g, '');
            rel.getter = `SELECT ?target WHERE { <Base> <${escapedPred}> ?target . ${whereConditions.join(' ')} }`;
          }
        } catch (_) {}
      }
    }

    return {
      className,
      queryJson: JSON.stringify(queryInput),
      shapeJson: JSON.stringify(metadata),
    };
  }

  /**
   * Parse raw model query/subscription result JSON into typed model instances.
   * Used by ModelQueryBuilder to convert subscription updates without circular imports.
   * @internal
   */
  static parseModelResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    raw: any,
    include?: IncludeMap,
    properties?: string[],
  ): T[] {
    const data = typeof raw === 'string' ? JSON.parse(raw) : raw;
    const arr = data.instances || data;
    if (!Array.isArray(arr)) return [];
    return arr.map((json: any) => jsonToModelInstance(this, perspective, json, include, properties));
  }

  // instancesFromQueryResult — removed (superseded by Rust executeModelQuery pipeline)

  /**
   * Execute a model query via the executor-side endpoint.
   * 
   * This replaces the old SPARQL-build → hydrate → JS-filter → JS-sort → JS-paginate pipeline
   * with a single RPC to the executor that handles everything in Rust.
   * 
   * @internal
   */
  private static async executeModelQuery<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
    classNameOverride?: string | null,
  ): Promise<ResultsWithTotalCount<T>> {
    const metadata = this.getModelMetadata();
    const className = classNameOverride || metadata.className;

    // Build the query input that the Rust endpoint expects.
    // Convert the TS Query type to the executor's ModelQueryInput format.
    const queryInput: any = {};
    if (query.parent) {
      const parentPredicate = resolveParentPredicate(query.parent, this);
      queryInput.parent = { id: query.parent.id, predicate: parentPredicate };
    }
    if (query.properties) queryInput.properties = query.properties;
    if (query.include) queryInput.include = query.include;
    if (query.where) queryInput.where = query.where;
    if (query.order) {
      // Convert { propName: 'ASC' | 'DESC' } to [[propName, 'ASC'|'DESC'], ...]
      queryInput.order = Object.entries(query.order).map(([k, v]) => [k, v]);
    }
    if (query.offset !== undefined) queryInput.offset = query.offset;
    if (query.limit !== undefined) queryInput.limit = query.limit;
    if (query.count !== undefined) queryInput.count = query.count;

    // Enrich relation metadata with target class shapes for Rust-side
    // include resolution. This eliminates per-relation GraphQL round-trips
    // by letting the Rust endpoint resolve includes in-process.
    if (query.include) {
      const allRelMeta = getRelationsMetadata(this as any);
      enrichShapeForIncludes(metadata, query.include, allRelMeta);
    }

    // ── Pre-compute conformance getters for Rust-side evaluation ──────
    // For each relation with a target class and filter !== false, compute
    // the conformance getter SPARQL so Rust can evaluate it in-process
    // (eliminating N × querySparql round-trips per instance).
    // When a relation has a `where` clause, compile the where conditions
    // into the getter SPARQL so Rust applies them during evaluation.
    {
      const allRelMeta = getRelationsMetadata(this as any);
      for (const [relName, relMeta] of Object.entries(metadata.relations)) {
        const rel = relMeta as any;
        if (rel.getter) continue; // explicit getter already set
        if (rel.direction === 'reverse') continue; // reverse relations use reverse link lookup
        if (rel.filter === false) continue; // opt-out of conformance filtering

        const meta = allRelMeta[relName];
        if (!meta?.target) continue;

        try {
          const TargetClass = meta.target();
          const filter = buildConformanceFilter(meta.predicate, TargetClass);

          // Only compile where clauses to SPARQL when the target properties
          // are stored as plain IRIs (not signed expressions). Properties
          // without resolveLanguage or with resolveLanguage='literal' store
          // values as signed JSON envelopes, so SPARQL exact-match fails.
          let whereConditions: string[] = [];
          if (rel.where) {
            try {
              const targetMetadata = (TargetClass as any).getModelMetadata?.() ?? null;
              let allSparqlFilterable = true;
              if (targetMetadata) {
                for (const propName of Object.keys(rel.where)) {
                  if (['id', 'author', 'timestamp'].includes(propName)) continue;
                  const propMeta = targetMetadata.properties[propName];
                  if (propMeta && (!propMeta.resolveLanguage || propMeta.resolveLanguage === 'literal')) {
                    allSparqlFilterable = false;
                    break;
                  }
                }
              } else {
                allSparqlFilterable = false;
              }
              if (allSparqlFilterable) {
                whereConditions = compileWhereClause(rel.where, targetMetadata);
              }
            } catch (_) { /* target metadata unavailable */ }
          }

          if (filter) {
            let getter = filter.getter;
            if (whereConditions.length > 0) {
              // Append where conditions before the closing }
              getter = getter.replace(/ \}$/, ` ${whereConditions.join(' ')} }`);
            }
            rel.getter = getter;
          } else if (whereConditions.length > 0) {
            // No conformance filter but where clause exists — build getter from where alone
            const escapedPred = meta.predicate.replace(/[<>"{}|\\^`\u0000-\u0020]/g, '');
            rel.getter = `SELECT ?target WHERE { <Base> <${escapedPred}> ?target . ${whereConditions.join(' ')} }`;
          }
        } catch (e) {
          // Target class may not be available; skip silently
        }
      }
    }

    // Send shape metadata to the executor so it knows the model structure.
    // This is more reliable than reading SHACL from the store because
    // the TS decorators are the definitive source of truth.
    const shapeJson = JSON.stringify(metadata);
    const queryJson = JSON.stringify(queryInput);

    const result = await perspective.modelQuery(className, queryJson, shapeJson);

    // Convert JSON instances to model class instances, recursively constructing
    // class instances for any included relations resolved by Rust.
    const instances: T[] = result.instances.map((json: any) => {
      return jsonToModelInstance(this, perspective, json, query.include, query.properties);
    });

    // Resolve non-literal expressions (e.g. file languages where the stored
    // value is a content-addressed hash that must be fetched from the language
    // runtime). The Rust endpoint returns raw target URIs; we resolve them here.
    const propsMeta = getPropertiesMetadata(this as any);
    const resolveProps = Object.entries(propsMeta).filter(
      ([, opts]: [string, any]) =>
        opts.resolveLanguage != null &&
        opts.resolveLanguage !== 'literal',
    );
    if (resolveProps.length > 0) {
      await Promise.all(
        instances.map(async (inst: any) => {
          for (const [propName, opts] of resolveProps) {
            const val = inst[propName];
            if (typeof val !== 'string' || !val || val.startsWith('literal:')) continue;
            try {
              const expression = await perspective.getExpression(val);
              if (expression) {
                let resolved: any;
                try { resolved = JSON.parse(expression.data); } catch { resolved = expression.data; }
                const transform = (opts as any).transform;
                inst[propName] = typeof transform === 'function' ? transform(resolved) : resolved;
              }
            } catch (_) { /* resolution failed — keep raw value */ }
          }
        }),
      );
    }

    // Relation conformance getters are evaluated Rust-side via evaluate_getters()
    // in model_query.rs. However, for relations with non-SPARQL-filterable where
    // clauses (e.g. properties stored as signed expressions), we still need
    // TS-side evaluation as a fallback. Running evaluateCustomGettersForInstance
    // with skipPropertyGetters ensures conformance + JS post-filtering for
    // where clauses that couldn't be compiled to SPARQL.
    const includedRelNames = query.include ? new Set(Object.keys(query.include)) : new Set<string>();
    for (const inst of instances) {
      // Save included relation values that came from Rust hydration
      const savedIncluded: Record<string, any> = {};
      for (const relName of includedRelNames) {
        if ((inst as any)[relName] !== undefined) {
          savedIncluded[relName] = (inst as any)[relName];
        }
      }

      await evaluateCustomGettersForInstance(inst, perspective, metadata, {
        skipPropertyGetters: true,
      });

      // Restore hydrated include objects that were overwritten by conformance getters
      for (const [relName, value] of Object.entries(savedIncluded)) {
        (inst as any)[relName] = value;
      }

      // Fix cardinality for non-included HasOne/BelongsToOne relations:
      // The conformance getter always produces arrays; unwrap to scalar.
      for (const [relName, relMeta] of Object.entries(metadata.relations)) {
        if (includedRelNames.has(relName)) continue;
        const kind = (relMeta as any).kind;
        if ((kind === 'hasOne' || kind === 'belongsToOne') && Array.isArray((inst as any)[relName])) {
          const arr = (inst as any)[relName] as any[];
          (inst as any)[relName] = arr.length > 0 ? arr[0] : null;
        }
      }
    }

    // Take snapshots for dirty tracking
    const snapshotRelations = query.include;
    for (const inst of instances) {
      (inst as Ad4mModel).takeSnapshot(snapshotRelations);
    }

    return {
      results: instances,
      totalCount: result.totalCount,
    };
  }

  /**
   * Gets all instances of the model in the perspective that match the query params.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSPARQL - Whether to use SPARQL (default: true, 10-100x faster) or Prolog (legacy)
   * @returns Array of matching models
   * 
   * @example
   * ```typescript
   * // Get all recipes (uses SPARQL by default)
   * const allRecipes = await Recipe.findAll(perspective);
   * 
   * // Get recipes with specific criteria (uses SPARQL)
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
    _engine?: 'sparql' | 'prolog' | boolean
  ): Promise<T[]> {
    if (query.properties && query.properties.length === 0) {
      throw new Error("properties[] must not be empty — omit the field to return all properties, or specify at least one field name");
    }

    const { results } = await this.executeModelQuery(perspective, query);
    return results;
  }

  /**
   * Finds the first instance matching the query, or `null` if none exists.
   *
   * Equivalent to `findAll` with `limit: 1` — only one instance is hydrated.
   *
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSPARQL - Whether to use SPARQL (default: true) or Prolog (legacy)
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
    _engine?: 'sparql' | 'prolog' | boolean,
  ): Promise<T | null> {
    const limitedQuery = { ...query, limit: 1 };
    const results = await this.findAll(perspective, limitedQuery);
    return results[0] ?? null;
  }

  /**
   * Gets all instances with count of total matches without offset & limit applied.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param useSPARQL - Whether to use SPARQL (default: true, 10-100x faster) or Prolog (legacy)
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
    _engine?: 'sparql' | 'prolog' | boolean
  ): Promise<ResultsWithTotalCount<T>> {
    return await this.executeModelQuery(perspective, query);
  }

  /**
   * Helper function for pagination with explicit page size and number.
   * 
   * @param perspective - The perspective to search in
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param query - Optional additional query parameters
   * @returns Paginated results with metadata
   * 
   * @example
   * ```typescript
   * const page = await Recipe.paginate(perspective, 10, 1, {
   *   where: { category: "Main Course" }
   * });
   * console.log(`Page ${page.pageNumber} of recipes, ${page.results.length} items`);
   * ```
   */
  static async paginate<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    pageSize: number, 
    pageNumber: number, 
    query?: Query,
    _engine?: 'sparql' | 'prolog' | boolean
  ): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(query || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const { results, totalCount } = await this.executeModelQuery(perspective, paginationQuery);
    return { results, totalCount, pageSize, pageNumber };
  }

  /**
   * Generates a SPARQL COUNT query for the model.
   * 
   * @param perspective - The perspective context
   * @param query - Query parameters to filter the count
   * @returns SPARQL COUNT query string
   * 
   * @private
   */
  // countQueryToSPARQL — removed (zero callers; Rust COUNT fast-path supersedes)

  /**
   * Gets a count of all matching instances.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @returns Total count of matching entities
   * 
   * @example
   * ```typescript
   * const totalRecipes = await Recipe.count(perspective);
   * const activeRecipes = await Recipe.count(perspective, {
   *   where: { status: "active" }
   * });
   * ```
   */
  static async count(perspective: PerspectiveProxy, query: Query = {}): Promise<number> {
    const { totalCount } = await this.executeModelQuery(perspective, { ...query, limit: 0 });
    return totalCount;
  }

  private async setProperty(key: string, value: any, batchId?: string) {
    const metadata = this.getPropertyMetadata(key);
    if (!metadata) {
      console.warn(`Property "${key}" has no metadata, skipping`);
      return;
    }

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generatePropertySetterAction(key, metadata);

    // Get resolve language from metadata (replaces Prolog query)
    let resolveLanguage = metadata.resolveLanguage;

    // Skip storing empty/null/undefined values to avoid invalid empty literals (e.g. literal:string:)
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
   * `createSubject` with initial scalar values, then sets relations
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
    

    // Check if the model has any constructor actions (required properties,
    // flags, or properties with initial values).  Models whose properties are
    // all optional, have no @Flag, and have no initial values produce an empty
    // SHACL constructor, so calling createSubject would fail on the Rust side
    // ("No SHACL constructor found").  In that case we skip createSubject
    // entirely and let innerUpdate write the links directly.
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    const hasConstructor = Object.values(metadata.properties).some(
      (p) => p.required || p.flag || p.initial !== undefined
    );

    // Track properties that have resolveLanguage (non-literal) so they can be
    // set via setProperty after createSubject (which doesn't resolve languages).
    const deferredResolveLanguageProps: string[] = [];

    if (hasConstructor) {
      // First filter out the properties that are not relations (arrays)
      const initialValues = {};
      for (const [key, value] of Object.entries(this)) {
        if (value !== undefined && value !== null && !(Array.isArray(value) && value.length > 0) && !value?.action) {
          // Check if this property requires language resolution (e.g. file storage).
          // If so, resolve the expression *before* passing to createSubject so
          // the constructor receives a valid URI instead of raw data.
          const propMeta = metadata.properties[key];
          if (propMeta?.resolveLanguage && propMeta.resolveLanguage !== 'literal' && typeof value === 'object') {
            // Defer these properties — they need createExpression which may
            // fail inside a batch context on some languages.  We'll set them
            // via setProperty after createSubject.
            deferredResolveLanguageProps.push(key);
            continue;
          }
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
    }

    // Set properties and relations via innerUpdate.
    // When createSubject was skipped (no constructor actions), we must enable
    // property writing so that scalar values are persisted as links.
    await this.innerUpdate(!hasConstructor, batchId)

    // Now handle any deferred resolveLanguage properties that were excluded
    // from initialValues.  setProperty will call createExpression to upload
    // the data to the appropriate language and store the resulting URI.
    for (const key of deferredResolveLanguageProps) {
      const value = (this as any)[key];
      if (value !== undefined && value !== null) {
        await this.setProperty(key, value, batchId);
      }
    }

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

    // Determine which fields actually changed (skip unchanged when snapshot exists)
    const dirty = this._snapshot ? new Set(this.changedFields()) : null;

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

      // Skip read-only computed relations — explicit getters never write links.
      // For target+filter relations, skip only when another relation on this
      // model claims the same predicate (i.e., this is a filtered *view* of a
      // base relation and writing would collide).
      const relMeta = metadata.relations[key];
      if (relMeta) {
        if (relMeta.getter) continue;
        if (relMeta.target && relMeta.filter !== false) {
          // Check for predicate collision with a sibling relation
          const hasCollision = Object.entries(metadata.relations).some(
            ([otherName, otherMeta]) =>
              otherName !== key &&
              (otherMeta as RelationMetadata).predicate === relMeta.predicate
          );
          if (hasCollision) continue;
        }
      }

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
   * Evaluate SPARQL getters for a batch of instances on demand.
   *
   * Use after a shallow collection query (the default) to resolve
   * getter-backed properties for visible items only.  This avoids the
   * O(N × getters) cost of evaluating every getter on every instance in
   * a large collection query.
   *
   * @param instances - Array of model instances to evaluate getters on
   * @param perspective - The perspective to query against
   * @param propertyNames - Optional list of getter-backed property names to evaluate.
   *                        If omitted, all getters are evaluated.
   *
   * @example
   * ```typescript
   * const messages = await Message.findAll(perspective, { parent: channel, limit: 30 });
   * // messages[i].replyingTo is undefined (getter skipped by default)
   *
   * // Evaluate getters for the visible subset
   * await Message.evaluateGetters(messages.slice(0, 10), perspective, ['replyingTo']);
   * // messages[0..9].replyingTo is now populated
   * ```
   */
  static async evaluateGetters<T extends Ad4mModel>(
    instances: T[],
    perspective: PerspectiveProxy,
    propertyNames?: string[],
  ): Promise<void> {
    if (instances.length === 0) return;
    const metadata = this.getModelMetadata();
    const opts = propertyNames && propertyNames.length > 0
      ? { requestedProperties: propertyNames }
      : undefined;

    // Determine which property names will actually be evaluated
    const evaluatedPropertyNames: string[] = [];
    for (const [propName, propMeta] of Object.entries(metadata.properties)) {
      if ((propMeta as any).getter) {
        if (!propertyNames || propertyNames.length === 0 || propertyNames.includes(propName)) {
          evaluatedPropertyNames.push(propName);
        }
      }
    }

    for (const instance of instances) {
      await evaluateCustomGettersForInstance(instance, perspective, metadata, opts);
      // Sync snapshot so isDirty() doesn't flag getter-only changes.
      // evaluateGetters() mutates instances in place, but their snapshots
      // were already taken during hydration. Without this sync, the computed
      // getter values would make isDirty() return true and save() would try
      // to write read-only computed properties.
      const modelInstance = instance as Ad4mModel;
      if (modelInstance._snapshot) {
        for (const propName of evaluatedPropertyNames) {
          const val = (instance as any)[propName];
          modelInstance._snapshot[propName] = normalizeValue(Array.isArray(val) ? [...val] : val);
        }
      }
    }
  }

  /**
   * Gets the model instance with all properties and relations populated.
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
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    const hasDestructor = Object.values(metadata.properties).some(
      (p) => p.required || p.flag || p.initial !== undefined
    );

    if (hasDestructor) {
      // Remove the subject itself (destructor actions)
      await this._perspective.removeSubject(this, this._baseExpression, batchId);
    } else {
      // No destructor (all-optional model).
      // SAFETY: We must NOT remove all outgoing links because another model
      // class may be instantiated on the same base expression.  Removing
      // everything would silently destroy that other instance's data — and
      // if that instance has a destructor, it can no longer be called
      // cleanly, leaving link debris.
      //
      // Instead we only remove outgoing links whose predicate is declared
      // by THIS model's schema (properties + relations).
      try {
        const knownPredicates = new Set<string>();
        for (const p of Object.values(metadata.properties)) {
          if (p.predicate) knownPredicates.add(p.predicate);
        }
        for (const r of Object.values(metadata.relations)) {
          if (r.predicate) knownPredicates.add(r.predicate);
        }

        const outgoingLinks = await this._perspective.get(new LinkQuery({ source: this._baseExpression }));
        const ownLinks = outgoingLinks.filter(
          (link) => link.data.predicate && knownPredicates.has(link.data.predicate)
        );
        if (ownLinks.length > 0) {
          await this._perspective.removeLinks(ownLinks, batchId);
        }
      } catch (e) {
        console.warn(`delete(): failed to remove outgoing links for ${this._baseExpression}:`, e);
      }
    }

    // Clean up incoming links — remove any links that point **to** this instance.
    // Unlike outgoing links (scoped above), incoming links originate from OTHER
    // nodes.  Removing them doesn't damage sibling models on this node; it
    // prevents dangling references elsewhere (e.g. a parent's has_child link).
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
    return buildModelFromJSONSchema(this, schema, options);
  }

  /**
   * Creates a fully functional Ad4mModel subclass from a SHACL shape.
   *
   * Unlike `fromJSONSchema()`, no predicate inference is required —
   * `SHACLShape` already contains the exact predicate URI in `path` for
   * every property. The method reads each property's `path`, `maxCount`,
   * `writable`, and `resolveLanguage` directly and writes them to the
   * WeakMap metadata registries.
   *
   * Properties with `hasValue` (flag / type-discrimination markers) are
   * registered as hidden flag entries so that the SPARQL query builder emits
   * the fixed triple `?source <predicate> <value>` needed for type discrimination.
   * Properties without a `name` field are skipped.
   *
   * **Backward-compat:** Old Flux SHACL shapes (created before `sh:hasValue`
   * was persisted on property shapes) carry the flag value only in
   * `shape.constructor_actions` as the `target` of an `addLink` action.
   * This method recovers those values so that old perspectives are queried
   * with the correct type-discriminator triple.
   *
   * @param shape - SHACL node shape (as returned by `PerspectiveProxy.getAllShacl()`)
   * @param name  - Class name to assign (e.g. "Channel")
   * @returns Generated Ad4mModel subclass, ready for querying
   */
  static fromSHACL(shape: SHACLShape, name: string): typeof Ad4mModel {
    const DynamicModelClass = class extends (this as any) {} as unknown as typeof Ad4mModel;
    (DynamicModelClass as any).className = name;
    (DynamicModelClass.prototype as any).className = name;

    // Build a backward-compat fallback map: predicate → fixed-IRI-value from
    // constructor actions. Old SHACL stores (created before sh:hasValue was
    // persisted on the property shape) don't carry sh:hasValue on properties,
    // but the shape-level constructor_actions always contain an addLink action
    // with the fixed flag value as the target.
    const flagValueFromConstructor = new Map<string, string>();
    for (const action of shape.constructor_actions ?? []) {
      if (
        action.action === 'addLink' &&
        typeof action.predicate === 'string' &&
        typeof action.target === 'string' &&
        action.target !== 'value' &&
        !action.target.startsWith('literal:')
      ) {
        flagValueFromConstructor.set(action.predicate, action.target);
      }
    }

    for (const prop of shape.properties) {
      const resolvedHasValue = prop.hasValue ?? flagValueFromConstructor.get(prop.path);

      if (resolvedHasValue !== undefined) {
        const flagKey = prop.name ?? `_flag_${prop.path.replace(/[^a-zA-Z0-9_]/g, '_')}`;
        setPropertyRegistryEntry(DynamicModelClass, flagKey, {
          through: prop.path,
          required: true,
          initial: resolvedHasValue,
          flag: true,
          writable: false,
          readOnly: true,
        });
        continue;
      }

      // Skip properties without a declared name
      if (!prop.name) continue;

      const isCollection = prop.maxCount === undefined || prop.maxCount > 1;

      if (isCollection) {
        setRelationRegistryEntry(DynamicModelClass, prop.name, {
          predicate: prop.path,
          kind: 'hasMany',
          ...(prop.local !== undefined && { local: prop.local }),
          ...(prop.getter !== undefined && { getter: prop.getter }),
        });
      } else {
        setPropertyRegistryEntry(DynamicModelClass, prop.name, {
          through: prop.path,
          writable: prop.writable ?? true,
          ...(prop.resolveLanguage !== undefined && { resolveLanguage: prop.resolveLanguage }),
          ...(prop.local !== undefined && { local: prop.local }),
        });
      }
    }

    const ModelDecorator = Model({ name });
    ModelDecorator(DynamicModelClass);

    return DynamicModelClass as typeof Ad4mModel;
  }
}

