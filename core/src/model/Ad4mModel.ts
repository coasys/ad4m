import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom } from "./decorators";
import { singularToPlural } from "./util";

type ValueTuple = [name: string, value: any, resolve?: boolean];
type WhereOps = {
  not: string | number | boolean | string[] | number[];
  between: [number, number];
  lt: number; // less than
  lte: number; // less than or equal to
  gt: number; // greater than
  gte: number; // greater than or equal to
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

export type AllInstancesResult = { AllInstances: Ad4mModel[]; TotalCount?: number };
export type ResultsWithTotalCount<T> = { results: T[]; totalCount?: number };
export type PaginationResult<T> = { results: T[]; totalCount?: number; pageSize: number; pageNumber: number };

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

  public static async assignValuesToInstance(
    perspective: PerspectiveProxy,
    instance: Ad4mModel,
    values: ValueTuple[]
  ) {
    // Map properties to object
    const propsObject = Object.fromEntries(
      await Promise.all(
        values.map(async ([name, value, resolve]) => {
          let finalValue = value;
          // Resolve the value if necessary
          if (resolve) {
            let resolvedExpression = await perspective.getExpression(value);
            if (resolvedExpression) {
              finalValue = resolvedExpression.data;
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

    const result = await this.#perspective.infer(fullQuery);
    if (result?.[0]) {
      const { Properties, Collections, Timestamp, Author } = result?.[0];
      const values = [...Properties, ...Collections, ["timestamp", Timestamp], ["author", Author]];
      await Ad4mModel.assignValuesToInstance(this.#perspective, this, values);
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
   * Gets all instances of the model in the perspective that match the query params.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @returns Array of matching models
   * 
   * @example
   * ```typescript
   * // Get all recipes
   * const allRecipes = await Recipe.findAll(perspective);
   * 
   * // Get recipes with specific criteria
   * const recipes = await Recipe.findAll(perspective, {
   *   where: { 
   *     name: "Pasta",
   *     rating: { gt: 4 }
   *   },
   *   order: { createdAt: "DESC" },
   *   limit: 10
   * });
   * ```
   */
  static async findAll<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T), 
    perspective: PerspectiveProxy, 
    query: Query = {}
  ): Promise<T[]> {
    const prologQuery = await this.queryToProlog(perspective, query);
    const result = await perspective.infer(prologQuery);
    const { results } = await this.instancesFromPrologResult(perspective, query, result);
    return results;
  }

  /**
   * Gets all instances with count of total matches without offset & limit applied.
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
    query: Query = {}
  ): Promise<ResultsWithTotalCount<T>> {
    const prologQuery = await this.queryToProlog(perspective, query);
    const result = await perspective.infer(prologQuery);
    return await this.instancesFromPrologResult(perspective, query, result);
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
    query?: Query
  ): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(query || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const prologQuery = await this.queryToProlog(perspective, paginationQuery);
    const result = await perspective.infer(prologQuery);
    const { results, totalCount } = await this.instancesFromPrologResult(perspective, paginationQuery, result);
    return { results, totalCount, pageSize, pageNumber };
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
  static async count(perspective: PerspectiveProxy, query: Query = {}) {
    const result = await perspective.infer(await this.countQueryToProlog(perspective, query));

    return result?.[0]?.TotalCount || 0;
  }

  private async setProperty(key: string, value: any) {
    const setters = await this.#perspective.infer(
      `subject_class("${this.#subjectClassName}", C), property_setter(C, "${key}", Setter)`
    );
    if (setters && setters.length > 0) {
      const actions = eval(setters[0].Setter);
      const resolveLanguageResults = await this.#perspective.infer(
        `subject_class("${this.#subjectClassName}", C), property_resolve_language(C, "${key}", Language)`
      );
      let resolveLanguage;
      if (resolveLanguageResults && resolveLanguageResults.length > 0) {
        resolveLanguage = resolveLanguageResults[0].Language;
      }

      if (resolveLanguage) {
        value = await this.#perspective.createExpression(value, resolveLanguage);
      }
      await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }]);
    }
  }

  private async setCollectionSetter(key: string, value: any) {
    let collectionSetters = await this.#perspective.infer(
      `subject_class("${this.#subjectClassName}", C), collection_setter(C, "${singularToPlural(key)}", Setter)`
    );
    if (!collectionSetters) collectionSetters = [];

    if (collectionSetters.length > 0) {
      const actions = eval(collectionSetters[0].Setter);

      if (value) {
        if (Array.isArray(value)) {
          await this.#perspective.executeAction(
            actions,
            this.#baseExpression,
            value.map((v) => ({ name: "value", value: v }))
          );
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }]);
        }
      }
    }
  }

  private async setCollectionAdder(key: string, value: any) {
    let adders = await this.#perspective.infer(
      `subject_class("${this.#subjectClassName}", C), collection_adder(C, "${singularToPlural(key)}", Adder)`
    );
    if (!adders) adders = [];

    if (adders.length > 0) {
      const actions = eval(adders[0].Adder);
      if (value) {
        if (Array.isArray(value)) {
          await Promise.all(
            value.map((v) =>
              this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])
            )
          );
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }]);
        }
      }
    }
  }

  private async setCollectionRemover(key: string, value: any) {
    let removers = await this.#perspective.infer(
      `subject_class("${this.#subjectClassName}", C), collection_remover(C, "${singularToPlural(key)}", Remover)`
    );
    if (!removers) removers = [];

    if (removers.length > 0) {
      const actions = eval(removers[0].Remover);
      if (value) {
        if (Array.isArray(value)) {
          await Promise.all(
            value.map((v) =>
              this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])
            )
          );
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }]);
        }
      }
    }
  }

  /**
   * Saves the model instance to the perspective.
   * Creates a new instance with the base expression and links it to the source.
   * 
   * @throws Will throw if instance creation, linking, or updating fails
   * 
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * recipe.ingredients = ["pasta", "tomato sauce"];
   * await recipe.save();
   * ```
   */
  async save() {
    // We use createSubject's initialValues to set properties (but not collections)
    // We then later use innerUpdate to set collections

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
      initialValues
    );

    // Link the subject to the source
    await this.#perspective.add(
      new Link({ source: this.#source, predicate: "ad4m://has_child", target: this.baseExpression })
    );

    // Set collections
    await this.innerUpdate(false)
  }

  private async innerUpdate(setProperties: boolean = true) {
    this.#subjectClassName = await this.#perspective.stringOrTemplateObjectToSubjectClassName(this);

    const entries = Object.entries(this);
    for (const [key, value] of entries) {
      if (value !== undefined && value !== null) {
        if (value?.action) {
          switch (value.action) {
            case "setter":
              await this.setCollectionSetter(key, value.value);
              break;
            case "adder":
              await this.setCollectionAdder(key, value.value);
              break;
            case "remover":
              await this.setCollectionRemover(key, value.value);
            default:
              await this.setCollectionSetter(key, value.value);
              break;
          }
        } else if (Array.isArray(value) && value.length > 0) {
          await this.setCollectionSetter(key, value);
        } else if (value !== undefined && value !== null && value !== "") {
          if (setProperties) {
            await this.setProperty(key, value);
          }
        }
      }
    }
  }

  /**
   * Updates the model instance's properties and collections.
   * 
   * @throws Will throw if property setting or collection updates fail
   * 
   * @example
   * ```typescript
   * const recipe = await Recipe.findAll(perspective)[0];
   * recipe.rating = 5;
   * recipe.ingredients.push("garlic");
   * await recipe.update();
   * ```
   */
  async update() {
    await this.innerUpdate(true);
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
    this.#subjectClassName = await this.#perspective.stringOrTemplateObjectToSubjectClassName(this);

    return await this.getData();
  }

  /**
   * Deletes the model instance from the perspective.
   * 
   * @throws Will throw if removal fails
   * 
   * @example
   * ```typescript
   * const recipe = await Recipe.findAll(perspective)[0];
   * await recipe.delete();
   * ```
   */
  async delete() {
    await this.#perspective.removeSubject(this, this.#baseExpression);
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

  constructor(perspective: PerspectiveProxy, ctor: typeof Ad4mModel, query?: Query) {
    this.perspective = perspective;
    this.ctor = ctor;
    if (query) this.queryParams = query;
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
    const query = await this.ctor.queryToProlog(this.perspective, this.queryParams);
    const result = await this.perspective.infer(query);
    const { results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
    return results as T[];
  }

  /**
   * Subscribes to the query and receives updates when results change.
   * Also returns initial results immediately.
   * 
   * @param callback - Function to call with updated results
   * @returns Initial results array
   * 
   * @example
   * ```typescript
   * await Recipe.query(perspective)
   *   .where({ status: "cooking" })
   *   .subscribe(recipes => {
   *     console.log("Currently cooking:", recipes);
   *   });
   * ```
   */
  async subscribe(callback: (results: T[]) => void): Promise<T[]> {
    const query = await this.ctor.queryToProlog(this.perspective, this.queryParams, this.modelClassName);
    const subscription = await this.perspective.subscribeInfer(query);

    const processResults = async (result: AllInstancesResult) => {
      const { results } = await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, result);
      callback(results as T[]);
    };

    subscription.onResult(processResults);
    const { results } = await this.ctor.instancesFromPrologResult(
      this.perspective,
      this.queryParams,
      subscription.result
    );
    return results as T[];
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
    const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
    const result = await this.perspective.infer(query);
    return result?.[0]?.TotalCount || 0;
  }

  /**
   * Subscribes to count updates for matching entities.
   * 
   * @param callback - Function to call with updated count
   * @returns Initial count
   * 
   * @example
   * ```typescript
   * await Recipe.query(perspective)
   *   .where({ status: "active" })
   *   .countSubscribe(count => {
   *     console.log("Active items:", count);
   *   });
   * ```
   */
  async countSubscribe(callback: (count: number) => void): Promise<number> {
    const query = await this.ctor.countQueryToProlog(this.perspective, this.queryParams, this.modelClassName);
    const subscription = await this.perspective.subscribeInfer(query);

    const processResults = async (result: any) => {
      const newCount = result?.[0]?.TotalCount || 0;
      callback(newCount);
    };

    subscription.onResult(processResults);
    return  subscription.result?.[0]?.TotalCount || 0;
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
    const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
    const result = await this.perspective.infer(prologQuery);
    const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, result)) as ResultsWithTotalCount<T>;
    return { results, totalCount, pageSize, pageNumber };
  }

  /**
   * Subscribes to paginated results updates.
   * 
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param callback - Function to call with updated pagination results
   * @returns Initial pagination results
   * 
   * @example
   * ```typescript
   * await Recipe.query(perspective)
   *   .where({ category: "Main" })
   *   .paginateSubscribe(10, 1, page => {
   *     console.log("Updated page:", page.results);
   *   });
   * ```
   */
  async paginateSubscribe(
    pageSize: number, 
    pageNumber: number, 
    callback: (results: PaginationResult<T>) => void
  ): Promise<PaginationResult<T>> {
    const paginationQuery = { ...(this.queryParams || {}), limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const prologQuery = await this.ctor.queryToProlog(this.perspective, paginationQuery, this.modelClassName);
    const subscription = await this.perspective.subscribeInfer(prologQuery);
    

    const processResults = async (r: AllInstancesResult) => {
      const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, this.queryParams, r)) as ResultsWithTotalCount<T>;
      callback({ results, totalCount, pageSize, pageNumber });
    };

    subscription.onResult(processResults);
    const { results, totalCount } = (await this.ctor.instancesFromPrologResult(this.perspective, paginationQuery, subscription.result)) as ResultsWithTotalCount<T>;
    return { results, totalCount, pageSize, pageNumber };
  }
}
