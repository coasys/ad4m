import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { LinkQuery } from "../perspectives/LinkQuery";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomId } from "./util";
import { getPropertiesMetadata, getRelationsMetadata } from "./decorators";
import type { PropertyOptions, PropertyMetadataEntry, RelationMetadataEntry } from "./decorators";
import { formatSurrealValue } from "./surreal-utils";
import { resolveParentPredicate } from "./query-common";
import { buildParentQuery, buildAuthorAndTimestampQuery, buildPropertiesQuery, buildWhereQuery, buildCountQuery, buildOrderQuery, buildOffsetQuery, buildLimitQuery } from "./query-prolog";
import { isArrayType, determinePredicate, determineNamespace, buildModelFromJSONSchema } from "./json-schema";
import type { JSONSchemaProperty, JSONSchema, JSONSchemaToModelOptions } from "./json-schema";
import { buildSurrealQLQuery } from "./query-surreal";
import { buildSPARQLQuery, groupSPARQLResults } from "./query-sparql";
import { buildBatchSPARQLQuery } from "./query-sparql-batch";
import { hydrateBatchResult } from "./hydration-batch";
import { ModelQueryBuilder } from "./ModelQueryBuilder";
import {
  normalizeValue, matchesCondition, hydrateFromLinks,
  assignValuesToInstance as _assignValuesToInstance,
  evaluateCustomGettersForInstance,
  hydrateRelations,
} from "./hydration";
import type {
  ParentScope, IncludeMap, Query,
  GetOptions, AllInstancesResult, ResultsWithTotalCount,
  PaginationResult, PropertyMetadata, RelationMetadata, ModelMetadata, ValueTuple,
} from "./types";

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
    const prototypeRelations = Object.fromEntries(
      Object.entries(allRelationsMeta).filter(([, r]) => r.kind === 'hasMany' || r.kind === 'belongsToMany')
    );
    
    for (const [relationName, opts] of Object.entries(prototypeRelations)) {
      const options = opts as RelationMetadataEntry;
      relationsMetadata[relationName] = {
        name: relationName,
        predicate: options.predicate || "",
        ...(options.local !== undefined && { local: options.local }),
        ...(options.getter !== undefined && { getter: options.getter }),
        direction: (options.kind === 'belongsToMany' || options.kind === 'belongsToOne') ? 'reverse' : 'forward',
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

  /**
   * Assigns decoded Prolog property values to an instance.
   * Delegates to the standalone function in hydration.ts.
   */
  public static async assignValuesToInstance(perspective: PerspectiveProxy, instance: Ad4mModel, values: ValueTuple[]) {
    return _assignValuesToInstance(perspective, instance, values);
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
    // Builds an object with the author, timestamp, all properties, & all relations on the Ad4mModel and saves it to the instance
    // Use SurrealDB for data queries
    try {
      const ctor = this.constructor as typeof Ad4mModel;
      const metadata = ctor.getModelMetadata();

      // Query for all links from this specific node (base expression)
      const safeBaseExpression = formatSurrealValue(this._baseExpression);
      const linksQuery = `
        SELECT id, predicate, out.uri AS target, author, timestamp
        FROM link
        WHERE in.uri = ${safeBaseExpression}
        ORDER BY timestamp ASC
      `;
      const links = await this._perspective.querySurrealDB(linksQuery);

      if (links && links.length > 0) {
        // Core hydration: properties (latest-wins), relations, timestamps/author
        const requestedProperties = opts?.properties && opts.properties.length > 0 ? opts.properties : undefined;
        await hydrateFromLinks(this, links, metadata, this._perspective, requestedProperties);
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
      const getterOpts = opts?.properties || opts?.include
        ? { requestedProperties: opts?.properties, include: opts?.include }
        : undefined;
      await evaluateCustomGettersForInstance(this, this._perspective, metadata, getterOpts);

      // Eager-load relations if requested
      if (opts?.include) {
        await hydrateRelations(ctor, [this], this._perspective, opts.include);
      }
    } catch (e) {
      console.error(`SurrealDB getData also failed for ${this._baseExpression}:`, e);
    }

    this.takeSnapshot();
    return this;
  }

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
   * 2. Then, for each candidate base, resolve properties and relations via subqueries
   * 3. Finally, apply ordering, pagination (LIMIT/START) at the SQL level
   * 
   * Key architectural notes:
   * - SurrealDB stores only raw links (source, predicate, target, author, timestamp)
   * - No SDNA knowledge at the database level
   * - Properties are resolved via subqueries that look for links with specific predicates
   * - Relations are similar but return multiple values instead of one
   * - Special fields (base, author, timestamp) are accessed directly, not via subqueries
   * 
   * @param perspective - The perspective to query (used for metadata extraction)
   * @param query - Query parameters (where, order, limit, offset, properties, relations)
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
    const allRelMeta = getRelationsMetadata(this as any);
    return buildSurrealQLQuery(metadata, allRelMeta, query, this);
  }

  /**
   * Generates a SPARQL query from a Query object.
   *
   * @param perspective - The perspective to query (used for metadata extraction)
   * @param query - Query parameters (where, order, limit, offset, properties, relations)
   * @returns Complete SPARQL query string ready for execution
   */
  public static async queryToSPARQL(perspective: PerspectiveProxy, query: Query): Promise<string> {
    const metadata = this.getModelMetadata();
    const allRelMeta = getRelationsMetadata(this as any);
    return buildSPARQLQuery(metadata, allRelMeta, query, this);
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
      await hydrateRelations(this, instances, perspective, query.include);
    }

    // Take snapshots for dirty tracking after ALL hydration is complete
    // (including eager-loaded relations).
    // When `include` is specified, only snapshot those relations.
    // Otherwise snapshot ALL fields (properties + relations) since
    // hydrateFromLinks populates relations with stable raw IDs.
    const snapshotRelations = query.include;
    for (const inst of instances) {
      (inst as Ad4mModel).takeSnapshot(snapshotRelations);
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
        await hydrateFromLinks(instance, links, metadata, perspective, requestedProperties.length > 0 ? requestedProperties : undefined);
        
        // When specific properties are requested, delete unrequested properties
        // so they return undefined instead of their constructor defaults (e.g. 0, [])
        if (requestedProperties.length > 0) {
          const requested = new Set(requestedProperties);
          for (const propName of Object.keys(metadata.properties)) {
            if (!requested.has(propName)) {
              delete instance[propName];
            }
          }
          for (const relName of Object.keys(metadata.relations)) {
            if (!requested.has(relName) && !(query.include && relName in query.include)) {
              delete instance[relName];
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
    const getterOpts = requestedProperties.length > 0 || query.include
      ? { requestedProperties, include: query.include }
      : undefined;
    for (const instance of instances) {
      await evaluateCustomGettersForInstance(instance, perspective, metadata, getterOpts);
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
            if (!matchesCondition(instance[propertyName], condition)) {
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
              if (!matchesCondition(instance[propertyName], condition)) {
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
      await hydrateRelations(this, paginatedInstances, perspective, query.include);
    }

    // Take snapshots for dirty tracking after ALL hydration is complete
    // (including eager-loaded relations).
    // When `include` is specified, only snapshot those relations.
    // Otherwise snapshot ALL fields (properties + relations) since
    // hydrateFromLinks populates relations with stable raw IDs —
    // this ensures push-to-array + save() correctly detects dirty relations.
    const snapshotRelations = query.include;
    for (const inst of paginatedInstances) {
      (inst as Ad4mModel).takeSnapshot(snapshotRelations);
    }

    return {
      results: paginatedInstances,
      totalCount
    };
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
    engine: 'sparql' | 'surreal' | 'prolog' | boolean = 'sparql'
  ): Promise<T[]> {
    if (query.properties && query.properties.length === 0) {
      throw new Error("properties[] must not be empty — omit the field to return all properties, or specify at least one field name");
    }

    // Backward compatibility: boolean maps to surreal (true) or prolog (false)
    const resolvedEngine = typeof engine === 'boolean'
      ? (engine ? 'surreal' : 'prolog')
      : engine;

    if (resolvedEngine === 'sparql') {
      // Use batch SPARQL for queries with includes (eager-loading)
      if (query.include && Object.keys(query.include).length > 0) {
        const sparqlQuery = buildBatchSPARQLQuery(this.getModelMetadata(), query, this);
        const rawResult = await perspective.querySparql(sparqlQuery);
        return hydrateBatchResult<T>(rawResult, this, query.include, perspective);
      }

      const sparqlQuery = await this.queryToSPARQL(perspective, query);
      const rawResult = await perspective.querySparql(sparqlQuery);
      const grouped = groupSPARQLResults(rawResult);
      const { results } = await this.instancesFromSurrealResult(perspective, query, grouped);
      return results;
    } else if (resolvedEngine === 'surreal') {
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
    engine: 'sparql' | 'surreal' | 'prolog' | boolean = 'sparql',
  ): Promise<T | null> {
    const limitedQuery = { ...query, limit: 1 };
    const results = await this.findAll(perspective, limitedQuery, engine);
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
    engine: 'sparql' | 'surreal' | 'prolog' | boolean = 'sparql'
  ): Promise<ResultsWithTotalCount<T>> {
    const resolvedEngine = typeof engine === 'boolean'
      ? (engine ? 'surreal' : 'prolog')
      : engine;

    if (resolvedEngine === 'sparql') {
      const sparqlQuery = await this.queryToSPARQL(perspective, query);
      const rawResult = await perspective.querySparql(sparqlQuery);
      const grouped = groupSPARQLResults(rawResult);
      return await this.instancesFromSurrealResult(perspective, query, grouped);
    } else if (resolvedEngine === 'surreal') {
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
    engine: 'sparql' | 'surreal' | 'prolog' | boolean = 'sparql'
  ): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(query || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const resolvedEngine = typeof engine === 'boolean'
      ? (engine ? 'surreal' : 'prolog')
      : engine;

    if (resolvedEngine === 'sparql') {
      const sparqlQuery = await this.queryToSPARQL(perspective, paginationQuery);
      const rawResult = await perspective.querySparql(sparqlQuery);
      const grouped = groupSPARQLResults(rawResult);
      const { results, totalCount } = await this.instancesFromSurrealResult(perspective, paginationQuery, grouped);
      return { results, totalCount, pageSize, pageNumber };
    } else if (resolvedEngine === 'surreal') {
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
  static async count(perspective: PerspectiveProxy, query: Query = {}, engine: 'sparql' | 'surreal' | 'prolog' | boolean = 'sparql') {
    const resolvedEngine = typeof engine === 'boolean'
      ? (engine ? 'surreal' : 'prolog')
      : engine;

    if (resolvedEngine === 'sparql') {
      const sparqlQuery = await this.queryToSPARQL(perspective, query);
      const rawResult = await perspective.querySparql(sparqlQuery);
      const grouped = groupSPARQLResults(rawResult);
      const { totalCount } = await this.instancesFromSurrealResult(perspective, query, grouped);
      return totalCount;
    } else if (resolvedEngine === 'surreal') {
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
}

