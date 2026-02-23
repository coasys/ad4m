import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import {
  makeRandomId,
  PropertyOptions,
  RelationOptions,
  Model,
  getPropertiesMetadata,
  getRelationsMetadata,
  propertyRegistry,
  relationRegistry,
} from "./decorators";
import { capitalize, propertyNameToSetterName } from "./util";

// ── Public types (re-exported so consumers see no change) ──────────────────
export type {
  Query,
  Where,
  Order,
  WhereCondition,
  AllInstancesResult,
  ResultsWithTotalCount,
  PaginationResult,
  PropertyMetadata,
  RelationMetadata,
  ModelMetadata,
} from "./types";
import type {
  Query,
  Where,
  Order,
  WhereCondition,
  AllInstancesResult,
  ResultsWithTotalCount,
  PaginationResult,
  PropertyMetadata,
  RelationMetadata,
  ModelMetadata,
} from "./types";

// ── JSON Schema factory ────────────────────────────────────────────────────
import { createModelFromJSONSchema } from "./schema/fromJSONSchema";
import type {
  JSONSchema,
  JSONSchemaToModelOptions,
} from "./schema/fromJSONSchema";
export type {
  JSONSchema,
  JSONSchemaToModelOptions,
  JSONSchemaProperty, // keep re-exporting for external consumers
} from "./schema/fromJSONSchema";

// ── Fluent query builder (re-exported for consumers) ──────────────────────
import { ModelQueryBuilder } from "./query/QueryBuilder";
export { ModelQueryBuilder };

// ── SurrealDB query helpers (used internally, also re-exported) ────────────
export {
  buildSurrealQuery,
  buildSurrealCountQuery,
  formatSurrealValue,
  matchesCondition,
  buildGraphTraversalWhereClause,
  buildSurrealWhereClause,
  buildSurrealSelectFields,
  buildSurrealSelectFieldsWithAggregation,
} from "./query/SurrealQueryBuilder";
import { formatSurrealValue } from "./query/SurrealQueryBuilder";

// ── Hydration utilities (re-exported for advanced consumers) ──────────────
export {
  hydrateInstanceFromLinks,
  evaluateCustomGetters,
  normalizeTimestamp,
} from "./query/hydration";
export type { RawLink } from "./query/hydration";


// ── Static query operations (each static method below delegates here) ─────────
import * as ops from "./query/operations";
import { fetchInstanceData } from "./query/fetchInstance";

// ── Metadata helpers ────────────────────────────────────────────────────────
import { getModelMetadata as _getModelMetadata } from "./schema/metadata";

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
 *   // Relation: ingredients
 *   @HasMany({ through: "recipe://ingredient" })
 *   ingredients: string[] = [];
 *
 *   // Relation: comments that are instances of another model
 *   @HasMany({
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
  createdAt: any;
  updatedAt: any;

  private static classNamesByClass = new WeakMap<
    typeof Ad4mModel,
    { [perspectiveId: string]: string }
  >();

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
      classCache[perspectiveID] =
        await perspective.stringOrTemplateObjectToSubjectClassName(this);
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
   * - Class name from @ModelOptions
   * - Property metadata (predicates, types, constraints, etc.)
   * - Relation metadata (predicates, filters, etc.)
   *
   * For models created via `fromJSONSchema()`, this method will derive metadata from
   * the stored `__properties` and `__relations` structures that were populated during
   * the dynamic class creation. If these structures are empty but a JSON schema was
   * attached to the class, it can fall back to deriving metadata from that schema.
   *
   * @returns Structured metadata object containing className, properties, and relations
   * @throws Error if the class doesn't have @ModelOptions decorator
   *
   * @example
   * ```typescript
   * @ModelOptions({ name: "Recipe" })
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
    return _getModelMetadata(this);
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
  constructor(
    perspective: PerspectiveProxy,
    baseExpression?: string,
    source?: string,
  ) {
    this.#baseExpression = baseExpression
      ? baseExpression
      : Literal.from(makeRandomId(24)).toUrl();
    this.#perspective = perspective;
    this.#source = source || "ad4m://self";

    // Wire up real relation adder/remover/setter methods for decorator-based classes.
    // The @HasMany / @HasOne decorators place empty stubs on the prototype at class-definition
    // time (e.g. `addLocations = () => {}`). Here, at instance-creation time, we replace each
    // stub with a closure that actually calls the private implementation so that callers like
    // `instance.addLocations(value)` persist the link in the perspective.
    const proto = Object.getPrototypeOf(this);
    const relations: Record<string, any> = getRelationsMetadata(
      proto.constructor,
    );
    for (const key of Object.keys(relations)) {
      // Reverse relations (@BelongsToOne / @BelongsToMany) are read-only traversals —
      // the link is owned by the other side, so no mutator methods should exist here.
      if (relations[key].direction === "reverse") continue;

      const cap = capitalize(key);
      this[`add${cap}`] = (value: any, batchId?: string) =>
        this.setRelationAdder(key, value, batchId);
      this[`remove${cap}`] = (value: any, batchId?: string) =>
        this.setRelationRemover(key, value, batchId);
      this[`set${cap}`] = (value: any, batchId?: string) =>
        this.setRelationSetter(key, value, batchId);
    }
  }

  /**
   * Gets the base expression of the subject.
   */
  get baseExpression() {
    return this.#baseExpression;
  }

  /** Alias for {@link baseExpression}. Prefer `id` in new code. */
  get id() {
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
    return getPropertiesMetadata(proto.constructor)?.[key];
  }

  /**
   * Get relation metadata from decorator (Phase 1: Prolog-free refactor)
   * @private
   */
  private getRelationMetadata(key: string): RelationOptions | undefined {
    const proto = Object.getPrototypeOf(this);
    return getRelationsMetadata(proto.constructor)?.[key];
  }

  /**
   * Generate property setter action from metadata (Phase 1: Prolog-free refactor)
   * Replaces Prolog query: property_setter(C, key, Setter)
   * @private
   */
  private generatePropertySetterAction(
    key: string,
    metadata: PropertyOptions,
  ): any[] {
    // Check if property is a flag (immutable — written once by createSubject constructor action)
    if (metadata.flag) {
      throw new Error(
        `Property "${key}" is a @Flag and cannot be set after creation`,
      );
    }

    // Check if property is read-only
    if (metadata.writable === false) {
      throw new Error(`Property "${key}" is read-only and cannot be written`);
    }

    if (!metadata.through) {
      throw new Error(`Property "${key}" has no 'through' predicate defined`);
    }

    return [
      {
        action: "setSingleTarget",
        source: "this",
        predicate: metadata.through,
        target: "value",
        ...(metadata.local && { local: true }),
      },
    ];
  }

  /**
   * Generate relation action from metadata (Phase 1: Prolog-free refactor)
   * Replaces Prolog queries: relation_adder, relation_remover, relation_setter
   * @private
   */
  private generateRelationAction(
    key: string,
    actionType: "adder" | "remover" | "setter",
  ): any[] {
    const metadata = this.getRelationMetadata(key);
    if (!metadata) {
      throw new Error(`Relation "${key}" has no metadata defined`);
    }

    if (!metadata.through) {
      throw new Error(`Relation "${key}" has no 'through' predicate defined`);
    }

    const actionMap = {
      adder: "addLink",
      remover: "removeLink",
      setter: "relationSetter",
    };

    return [
      {
        action: actionMap[actionType],
        source: "this",
        predicate: metadata.through,
        target: "value",
        ...(metadata.local && { local: true }),
      },
    ];
  }

  private async getData() {
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    return fetchInstanceData(this, this.#perspective, this.#baseExpression, metadata);
  }

  /**
   * Generates a SurrealQL query string for this model.
   *
   * @param perspective - The perspective context
   * @param query - Query parameters (where, order, limit, offset, properties, relations)
   * @returns Complete SurrealQL query string ready for execution
   */
  public static async queryToSurrealQL(
    perspective: PerspectiveProxy,
    query: Query,
  ): Promise<string> {
    return ops.queryToSurrealQL(this as any, perspective, query);
  }

  /**
   * Converts SurrealDB query results to Ad4mModel instances.
   *
   * @param perspective - The perspective context
   * @param query - The query parameters used
   * @param result - Array of result objects from SurrealDB
   * @param _hydrateRelations - Set to false to skip nested relatedModel hydration
   * @returns Promise resolving to results with total count
   *
   * @internal
   */
  public static async instancesFromSurrealResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query,
    result: any[],
    _hydrateRelations = true,
  ): Promise<ResultsWithTotalCount<T>> {
    return ops.instancesFromSurrealResult(
      this as any,
      perspective,
      query,
      result,
      _hydrateRelations,
    );
  }

  /**
   * Internal implementation used by findAll and eager relation hydration.
   * Pass `_hydrateRelations = false` to prevent recursive model hydration (depth guard).
   */
  static async _findAllInternal<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
    _hydrateRelations = true,
  ): Promise<T[]> {
    return ops._findAllInternal(
      this as any,
      perspective,
      query,
      _hydrateRelations,
    );
  }

  /**
   * Gets all instances of the model in the perspective that match the query.
   *
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @returns Array of matching model instances
   *
   * @example
   * ```typescript
   * const recipes = await Recipe.findAll(perspective, {
   *   where: { name: "Pasta", rating: { gt: 4 } },
   *   order: { createdAt: "DESC" },
   *   limit: 10
   * });
   * ```
   */
  static async findAll<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<T[]> {
    return ops.findAll(this as any, perspective, query);
  }

  /**
   * Returns the first matching instance, or `null` if none found.
   *
   * @example
   * ```typescript
   * const post = await TestPost.findOne(perspective, { where: { id: someId } });
   * ```
   */
  static async findOne<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<T | null> {
    return ops.findOne(this as any, perspective, query);
  }

  /**
   * Gets all matching instances with the total count (ignoring limit/offset).
   *
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @returns Object containing results array and total count
   *
   * @example
   * ```typescript
   * const { results, totalCount } = await Recipe.findAllAndCount(perspective, {
   *   where: { category: "Dessert" },
   *   limit: 10
   * });
   * console.log(`Showing 10 of ${totalCount} dessert recipes`);
   * ```
   */
  static async findAllAndCount<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
  ): Promise<ResultsWithTotalCount<T>> {
    return ops.findAllAndCount(this as any, perspective, query);
  }

  /**
   * Paginates results by explicit page size and 1-based page number.
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
  ): Promise<PaginationResult<T>> {
    return ops.paginate(
      this as any,
      perspective,
      pageSize,
      pageNumber,
      query ?? {},
    );
  }

  /**
   * Generates a SurrealQL COUNT query for this model.
   * @private
   */
  public static async countQueryToSurrealQL(
    perspective: PerspectiveProxy,
    query: Query,
  ): Promise<string> {
    return ops.countQueryToSurrealQL(this as any, perspective, query);
  }

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
   * const activeRecipes = await Recipe.count(perspective, { where: { status: "active" } });
   * ```
   */
  static async count(perspective: PerspectiveProxy, query: Query = {}) {
    return ops.count(this as any, perspective, query);
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
      value = await this.#perspective.createExpression(value, resolveLanguage);
    } else if (
      typeof value !== "string" ||
      !/^[a-zA-Z][a-zA-Z0-9+\-.]*:/.test(value)
    ) {
      // Encode raw values as literal:// URIs so they are valid link targets.
      // This mirrors what Rust's resolve_property_value does inside createSubject.
      // Values that already carry a URI scheme (did:, expression://, literal://, etc.)
      // are passed through unchanged.
      value = Literal.from(value).toUrl();
    }

    await this.#perspective.executeAction(
      actions,
      this.#baseExpression,
      [{ name: "value", value }],
      batchId,
    );
  }

  private async setRelationSetter(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationMetadata(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Accept either a string ID or an Ad4mModel instance (extract baseExpression)
    const toId = (v: any): any =>
      v && typeof v === "object" && typeof v.baseExpression === "string"
        ? v.baseExpression
        : v;

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, "setter");

    if (value != null) {
      if (Array.isArray(value)) {
        await this.#perspective.executeAction(
          actions,
          this.#baseExpression,
          value.map((v) => ({ name: "value", value: toId(v) })),
          batchId,
        );
      } else {
        await this.#perspective.executeAction(
          actions,
          this.#baseExpression,
          [{ name: "value", value: toId(value) }],
          batchId,
        );
      }
    }
  }

  private async setRelationAdder(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationMetadata(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Accept either a string ID or an Ad4mModel instance (extract baseExpression)
    const toId = (v: any): any =>
      v && typeof v === "object" && typeof v.baseExpression === "string"
        ? v.baseExpression
        : v;

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, "adder");

    if (value != null) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this.#perspective.executeAction(
              actions,
              this.#baseExpression,
              [{ name: "value", value: toId(v) }],
              batchId,
            ),
          ),
        );
      } else {
        await this.#perspective.executeAction(
          actions,
          this.#baseExpression,
          [{ name: "value", value: toId(value) }],
          batchId,
        );
      }
    }
  }

  private async setRelationRemover(key: string, value: any, batchId?: string) {
    // Phase 1: Use metadata instead of Prolog queries
    const metadata = this.getRelationMetadata(key);
    if (!metadata) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }

    // Accept either a string ID or an Ad4mModel instance (extract baseExpression)
    const toId = (v: any): any =>
      v && typeof v === "object" && typeof v.baseExpression === "string"
        ? v.baseExpression
        : v;

    // Generate actions from metadata (replaces Prolog query)
    const actions = this.generateRelationAction(key, "remover");

    if (value != null) {
      if (Array.isArray(value)) {
        await Promise.all(
          value.map((v) =>
            this.#perspective.executeAction(
              actions,
              this.#baseExpression,
              [{ name: "value", value: toId(v) }],
              batchId,
            ),
          ),
        );
      } else {
        await this.#perspective.executeAction(
          actions,
          this.#baseExpression,
          [{ name: "value", value: toId(value) }],
          batchId,
        );
      }
    }
  }

  /**
   * Persists the model instance to the perspective.
   *
   * Automatically determines whether to **create** or **update**:
   * - If no links exist for `baseExpression` yet (new instance): runs the full
   *   create path — `createSubject`, `ad4m://has_child` link, then relation setters.
   * - If links already exist (existing instance): runs the update path — property
   *   and relation setters only; skips `createSubject` and `has_child` to avoid
   *   duplicate links.
   *
   * This means `save()` is always correct regardless of whether the instance was
   * just constructed or was fetched from the perspective.
   *
   * @param batchId - Optional batch ID for batch operations. When provided the
   *   caller is responsible for calling `perspective.commitBatch(batchId)`.
   * @throws Will throw if instance creation, linking, or updating fails
   *
   * @example
   * ```typescript
   * // Create
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * recipe.ingredients = ["pasta", "tomato sauce"];
   * await recipe.save();
   *
   * // Update
   * recipe.name = "Spaghetti Bolognese";
   * await recipe.save();  // same call — no separate update()
   *
   * // Batch operations
   * const batchId = await perspective.createBatch();
   * await recipe.save(batchId);
   * await perspective.commitBatch(batchId);
   * ```
   */
  async save(batchId?: string) {
    const ctor = this.constructor as typeof Ad4mModel;

    // Check whether this instance already exists in the perspective so we can
    // choose the create vs update path. Query persisted links only — uncommitted
    // batch state is not visible to SurrealDB queries, which is correct: if the
    // caller passed in a batchId and the instance was written earlier in that
    // same (not-yet-committed) batch, we treat it as new here, which is safe.
    const safeBase = formatSurrealValue(this.#baseExpression);
    const existingLinks = await this.#perspective.querySurrealDB(
      `SELECT 1 FROM link WHERE in.uri = ${safeBase} LIMIT 1`,
    );
    const isNew = !existingLinks || existingLinks.length === 0;

    let batchCreatedHere = false;
    if (!batchId) {
      batchId = await this.perspective.createBatch();
      batchCreatedHere = true;
    }

    if (isNew) {
      // ── CREATE PATH ───────────────────────────────────────────────────────
      // Use createSubject's initialValues to set scalar properties (not relations),
      // then innerUpdate(false) for relations only.

      // Filter to scalar (non-relation, non-action) values for createSubject
      const initialValues = {};
      for (const [key, value] of Object.entries(this)) {
        if (
          value !== undefined &&
          value !== null &&
          !(Array.isArray(value) && value.length > 0) &&
          !value?.action
        ) {
          initialValues[key] = value;
        }
      }

      const className =
        await this.perspective.stringOrTemplateObjectToSubjectClassName(this);

      await this.perspective.createSubject(
        className,
        this.#baseExpression,
        initialValues,
        batchId,
      );

      // Attach instance to its parent source node
      await this.#perspective.add(
        new Link({
          source: this.#source,
          predicate: "ad4m://has_child",
          target: this.baseExpression,
        }),
        "shared",
        batchId,
      );

      // Set relations
      await this.innerUpdate(false, batchId);
    } else {
      // ── UPDATE PATH ───────────────────────────────────────────────────────
      // Instance already exists — update properties and relations only.
      // Skipping createSubject and has_child prevents duplicate links.
      await this.innerUpdate(true, batchId);
    }

    if (batchCreatedHere) {
      await this.perspective.commitBatch(batchId);
      await this.getData();
    }
  }

  private cleanCopy() {
    const cleanCopy = {};
    const props = Object.entries(this);
    for (const [key, value] of props) {
      if (
        value !== undefined &&
        value !== null &&
        key !== "author" &&
        key !== "timestamp"
      ) {
        cleanCopy[key] = value;
      }
    }
    return cleanCopy;
  }

  private async innerUpdate(setProperties: boolean = true, batchId?: string) {
    this.#subjectClassName =
      await this.#perspective.stringOrTemplateObjectToSubjectClassName(
        this.cleanCopy(),
      );

    const entries = Object.entries(this);
    for (const [key, value] of entries) {
      if (value !== undefined && value !== null) {
        if (value?.action) {
          switch (value.action) {
            case "setter":
              await this.setRelationSetter(key, value.value, batchId);
              break;
            case "adder":
              await this.setRelationAdder(key, value.value, batchId);
              break;
            case "remover":
              await this.setRelationRemover(key, value.value, batchId);
              break;
            default:
              await this.setRelationSetter(key, value.value, batchId);
              break;
          }
        } else if (Array.isArray(value)) {
          // Handle all arrays as relations, including empty ones (which clears the relation)
          await this.setRelationSetter(key, value, batchId);
        } else if (value !== undefined && value !== null && value !== "") {
          if (setProperties) {
            // Check if this is a relation (has relation metadata)
            const relationMetadata = this.getRelationMetadata(key);
            if (relationMetadata) {
              // Skip - it's a relation, not a regular property
              continue;
            }
            // Skip flag fields — flags are immutable, written once by the
            // createSubject constructor action. Re-writing them would corrupt
            // the flag link via setSingleTarget on every re-save.
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
   * @deprecated Use `save()` instead. `save()` now automatically detects whether
   * to create or update based on whether the instance already exists in the
   * perspective. `update()` is kept for backwards compatibility and simply
   * delegates to `save()`.
   *
   * @param batchId - Optional batch ID for batch operations
   */
  async update(batchId?: string) {
    return this.save(batchId);
  }

  /**
   * Gets the model instance with all properties and relations populated.
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
    this.#subjectClassName =
      await this.#perspective.stringOrTemplateObjectToSubjectClassName(
        this.cleanCopy(),
      );

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
    query?: Query,
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
    options: JSONSchemaToModelOptions,
  ): typeof Ad4mModel {
    return createModelFromJSONSchema(this, schema, options) as typeof Ad4mModel;
  }
}
