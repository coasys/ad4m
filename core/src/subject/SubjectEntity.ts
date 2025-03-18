import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom } from "./SDNADecorators";
import { singularToPlural } from "./util";

export type QueryPartialEntity<T> = {
  [P in keyof T]?: T[P] | (() => string);
};

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
type Query = {
  source?: string;
  properties?: string[];
  collections?: string[];
  where?: Where;
  order?: Order;
  offset?: number;
  limit?: number;
};
export type AllInstancesResult = { AllInstances: SubjectEntity[] }

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
  // Gets the author and timestamp of a SubjectEntity instance (based on the first link mentioning the base)
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

// Binds:
// - SubjectClass
// - Base
// - PropertyName
// - PropertyUri
// - PropertyValue
// - Resolve
// Not need externally (only for debugging):
// - LiteralValue
// - Scheme
//
// `PropertyUri is what we get from the property_getter/4.
// If the property_resolve/2 is true, then we check if the property is a literal
// and try to resolve it in prolog, binding the resolved value to PropertyValue.
// `Resolve` is only true if the JS code still needs to resolve it!
const PROPERTY_RESOLVE_QUERY = `
  % Get the property name and resolve boolean
  property(SubjectClass, PropertyName),
  property_getter(SubjectClass, Base, PropertyName, PropertyUri),
  ( property_resolve(SubjectClass, PropertyName)
    % If the property is resolvable, try to resolve it
    -> (
      append("literal://", _, PropertyUri)
      % If the property is a literal, we can resolve it here
      -> (
        % so tell JS to not resolve it
        Resolve = false,
        literal_from_url(PropertyUri, LiteralValue, Scheme),
        (
          json_property(LiteralValue, "data", Data)
          % If it is a JSON literal, and it has a 'data' field, use that
          -> PropertyValue = Data
          % Otherwise, just use the literal value
          ; PropertyValue = LiteralValue 
        )
      )
      ;
      % else (it should be resolved but is not a literal),
      % pass through URI to JS and tell JS to resolve it
      (Resolve = true, PropertyValue = PropertyUri)
    )
    ;
    % else (no property resolve), just return the URI as the value
    (Resolve = false, PropertyValue = PropertyUri)
  ) 
`;

function buildPropertiesQuery(properties?: string[]): string {
  // Gets the name, value, and resolve boolean for all (or some) properties on a SubjectEntity instance
  // Resolves literals (if property_resolve/2 is true) to their value - either the data field if it is
  // an Expression in JSON literal, or the direct literal value if it is a simple literal
  // If no properties are provided, all are included
  return `
    findall([PropertyName, PropertyValue, Resolve], (
      % Constrain to specified properties if provided
      ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}
      ${PROPERTY_RESOLVE_QUERY}
    ), Properties)
  `;
}

function buildCollectionsQuery(collections?: string[]): string {
  // Gets the name and array of values for all (or some) collections on a SubjectEntity instance
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
      const isSpecial = ["author", "timestamp"].includes(key);
      const getter = `PropertyName = "${key}", ${PROPERTY_RESOLVE_QUERY}, V = PropertyValue`;
      // const getter = `property_getter(SubjectClass, Base, "${key}", URI), literal_from_url(URI, V, _)`;
      const field = capitalize(key);

      // Handle direct array values (for IN conditions)
      if (Array.isArray(value)) {
        const formattedValues = value.map((v) => formatValue(v)).join(", ");
        if (isSpecial) return `member(${field}, [${formattedValues}])`;
        else return `${getter}, member(V, [${formattedValues}])`;
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
            else return `${getter}, \\+ member(V, [${formattedValues}])`;
          } else {
            // NOT EQUAL
            if (isSpecial) return `${field} \\= ${formatValue(not)}`;
            else return `\\+ (${getter}, V = ${formatValue(not)})`;
          }
        }

        // Handle BETWEEN
        if (between !== undefined && Array.isArray(between) && between.length === 2) {
          if (isSpecial) return `${field} >= ${between[0]}, ${field} =< ${between[1]}`;
          else return `${getter}, V >= ${between[0]}, V =< ${between[1]}`;
        }

        // Handle lt, lte, gt, & gte operations
        const operators = [
          { value: lt, symbol: "<" }, // LESS THAN
          { value: lte, symbol: "=<" }, // LESS THAN OR EQUAL TO
          { value: gt, symbol: ">" }, // GREATER THAN
          { value: gte, symbol: ">=" }, // GREATER THAN OR EQUAL TO
        ];

        for (const { value, symbol } of operators) {
          if (value !== undefined) return isSpecial ? `${field} ${symbol} ${value}` : `${getter}, V ${symbol} ${value}`;
        }
      }

      // Default to direct equality
      if (isSpecial) return `${field} = ${formatValue(value)}`;
      else return `${getter}, V = ${formatValue(value)}`;
    })
    .join(", ");
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
 * Class representing a subject entity.
 * Can extend this class to create a new subject entity to add methods interact with SDNA and much better experience then using the bare bone methods.
 */
export class SubjectEntity {
  #baseExpression: string;
  #subjectClassName: string;
  #source: string;
  #perspective: PerspectiveProxy;
  author: string;
  timestamp: string;

  static classNamesByPerspectiveID: { [key: string]: string } = {};

  static async getClassName(perspective: PerspectiveProxy) {
    // cache the class name for the perspective
    const perspectiveID = perspective.uuid;
    if (!this.classNamesByPerspectiveID[perspectiveID]) {
      this.classNamesByPerspectiveID[perspectiveID] = await perspective.stringOrTemplateObjectToSubjectClassName(this);
    }

    return this.classNamesByPerspectiveID[perspectiveID];
  }

  /**
   * Constructs a new subject.
   * @param perspective - The perspective that the subject belongs to.
   * @param baseExpression - The base expression of the subject.
   * @param source - The source of the subject, the expression this instance is linked too.
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
    instance: SubjectEntity,
    values: ValueTuple[]
  ) {
    // Map properties to object
    const propsObject = Object.fromEntries(
      await Promise.all(
        values.map(async ([name, value, resolve]) => {
          let finalValue = value;
          // Resolve the value if necessary
          if(resolve) {
            let resolvedExpression = await perspective.getExpression(value)
            if(resolvedExpression) {
              finalValue = resolvedExpression.data;
            }
          }
          return [name, finalValue];
        })
      )
    );
    // Assign properties to instance
    Object.assign(instance, propsObject);
  }

  private async getData() {
    // Builds an object with the author, timestamp, all properties, & all collections on the SubjectEntity and saves it to the instance
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
      await SubjectEntity.assignValuesToInstance(this.#perspective, this, values);
    }

    return this;
  }

  // Todo: Only return AllInstances (InstancesWithOffset, SortedInstances, & UnsortedInstances not required)
  public static async queryToProlog(perspective: PerspectiveProxy, query: Query) {
    const { source, properties, collections, where, order, offset, limit } = query;

    const instanceQueries = [
      buildAuthorAndTimestampQuery(),
      buildSourceQuery(source),
      buildPropertiesQuery(properties),
      buildCollectionsQuery(collections),
      buildWhereQuery(where),
    ];

    const resultSetQueries = [buildOrderQuery(order), buildOffsetQuery(offset), buildLimitQuery(limit)];

    const fullQuery = `
      findall([Base, Properties, Collections, Timestamp, Author], (
        subject_class("${await this.getClassName(perspective)}", SubjectClass),
        instance(SubjectClass, Base),
        ${instanceQueries.filter((q) => q).join(", ")}
      ), UnsortedInstances),
      ${resultSetQueries.filter((q) => q).join(", ")}
    `;

    return fullQuery;
  }

  public static async instancesFromPrologResult(perspective: PerspectiveProxy, query: Query, result: AllInstancesResult) {
    if (!result?.[0]?.AllInstances) return [];
    // Map results to instances
    const requestedAttribtes = [...(query?.properties || []), ...(query?.collections || [])];
    const allInstances = await Promise.all(
      result[0].AllInstances.map(async ([Base, Properties, Collections, Timestamp, Author]) => {
        const instance = new this(perspective, Base) as any;
        // Remove unrequested attributes from instance
        if (requestedAttribtes.length) {
          Object.keys(instance).forEach((key) => {
            if (!requestedAttribtes.includes(key)) delete instance[key];
          });
        }
        // Collect values to assign to instance
        const values = [...Properties, ...Collections, ["timestamp", Timestamp], ["author", Author]];
        await SubjectEntity.assignValuesToInstance(perspective, instance, values);

        return instance;
      })
    );
    return allInstances;
  }


  /**
   * Gets all instances of the subject entity in the perspective that match the query params .
   * @param perspective - The perspective that the subject entities belongs to.
   * @param query - The query object used to define search contraints.
   *
   * @returns An array of all subject entity matches.
   *
   */
  static async findAll(perspective: PerspectiveProxy, query: Query = {}) {
    // todo: set up includes
    let prologQuery = await this.queryToProlog(perspective, query);
    const result = await perspective.infer(prologQuery);
    const allInstances = await this.instancesFromPrologResult(perspective, query, result);
    return allInstances;
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
   * Save the subject entity.
   * This method will create a new subject with the base expression and add a new link from the source to the base expression with the predicate "ad4m://has_child".
   *
   * If a property has an action, it will perform the action (Only for collections).
   * If a property is an array and is not empty, it will set the collection.
   * If a property is not undefined, not null, and not an empty string, it will set the property.
   *
   *
   * @throws Will throw an error if the subject entity cannot be converted to a subject class, or if the subject cannot be created, or if the link cannot be added, or if the subject entity cannot be updated.
   */
  async save() {
    await this.#perspective.createSubject(this, this.#baseExpression);

    await this.#perspective.add(
      new Link({ source: this.#source, predicate: "ad4m://has_child", target: this.baseExpression })
    );

    await this.update();
  }

  /**
   * Update the subject entity.
   *
   * It will iterate over the properties of the subject entity.
   *
   * If a property has an action, it will perform the action (Only for collections).
   * If a property is an array and is not empty, it will set the collection.
   * If a property is not undefined, not null, and not an empty string, it will set the property.
   *
   * @throws Will throw an error if the subject entity cannot be converted to a subject class, or if a property cannot be set, or if a collection cannot be set, or if the data of the subject entity cannot be gotten.
   */
  async update() {
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
          await this.setProperty(key, value);
        }
      }
    }

    await this.getData();
  }

  /**
   * Get the subject entity with all the properties & collection populated.
   *
   * @returns The subject entity.
   *
   * @throws Will throw an error if the subject entity cannot be converted to a subject class, or if the data of the subject entity cannot be gotten.
   */
  async get() {
    this.#subjectClassName = await this.#perspective.stringOrTemplateObjectToSubjectClassName(this);

    return await this.getData();
  }

  /**
   * Delete the subject entity.
   * This method will remove the subject from the perspective.
   *
   * @throws Will throw an error if the subject entity cannot be removed.
   */
  async delete() {
    await this.#perspective.removeSubject(this, this.#baseExpression);
  }

  /** Query builder for SubjectEntity queries.
   * Allows building queries with a fluent interface and either running them once
   * or subscribing to updates.
   */
  static query<T extends SubjectEntity>(perspective: PerspectiveProxy): SubjectQueryBuilder<T> {
    return new SubjectQueryBuilder<T>(perspective, this.name);
  }
}

/** Query builder for SubjectEntity queries.
 * Allows building queries with a fluent interface and either running them once
 * or subscribing to updates.
 */
export class SubjectQueryBuilder<T extends SubjectEntity> {
    private perspective: PerspectiveProxy;
    private subjectClass: string;
    private queryParams: Query = {};

    constructor(perspective: PerspectiveProxy, subjectClass: string) {
        this.perspective = perspective;
        this.subjectClass = subjectClass;
    }

    where(conditions: Where): SubjectQueryBuilder<T> {
        this.queryParams.where = conditions;
        return this;
    }

    order(orderBy: Order): SubjectQueryBuilder<T> {
        this.queryParams.order = orderBy;
        return this;
    }

    limit(limit: number): SubjectQueryBuilder<T> {
        this.queryParams.limit = limit;
        return this;
    }

    offset(offset: number): SubjectQueryBuilder<T> {
        this.queryParams.offset = offset;
        return this;
    }

    source(source: string): SubjectQueryBuilder<T> {
        this.queryParams.source = source;
        return this;
    }

    properties(properties: string[]): SubjectQueryBuilder<T> {
        this.queryParams.properties = properties;
        return this;
    }

    collections(collections: string[]): SubjectQueryBuilder<T> {
        this.queryParams.collections = collections;
        return this;
    }

    /** Execute the query once and return the results */
    async run(): Promise<T[]> {
        const query = await SubjectEntity.queryToProlog(this.perspective, this.queryParams);
        const result = await this.perspective.infer(query);
        const allInstances = await SubjectEntity.instancesFromPrologResult(this.perspective, this.queryParams, result);
        return allInstances;
    }

    /** Subscribe to the query and receive updates when results change
     * @param callback Function that will be called with the updated results whenever they change
     * @returns A function that can be called to unsubscribe
     */
    async subscribeAndRun(callback: (results: T[]) => void): Promise<T[]> {
        const query = await SubjectEntity.queryToProlog(this.perspective, this.queryParams);
        const subscription = await this.perspective.subscribeInfer(query);

        const processResults = async (result: AllInstancesResult) => {
          let newInstances = await SubjectEntity.instancesFromPrologResult(this.perspective, this.queryParams, result);
          callback(newInstances);
        };

        subscription.onResult(processResults);
        let instances = await SubjectEntity.instancesFromPrologResult(this.perspective, this.queryParams, subscription.result);
        return instances;
    }
}
