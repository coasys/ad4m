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
  lessThan: number;
  greaterThan: number;
  between: [number, number];
};
type WhereCondition = string | number | boolean | string[] | number[] | { [K in keyof WhereOps]?: WhereOps[K] };

type Query = {
  source?: string;
  properties?: string[];
  collections?: string[];
  where?: { [propertyName: string]: WhereCondition };
  order?: [propertyName: string, direction: "asc" | "desc"];
  // limit?: number;
  // offset?: number;
};

function capitalize(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1);
}

function buildSourceQuery(source?: string): string {
  // Constrains the query to instances that have the provided source
  if (!source) return "";
  return `triple("${source}", "ad4m://has_child", Base)`;
}

// todo: only return Timestamp & Author from query (Base, AllLinks, and SortLinks not required)
function buildAuthorAndTimestampQuery(options?: string[]): string {
  // Gets the author and/or timestamp of a SubjectEntity instance (based on the first link mentioning the base)

  // If no options are provided, both author and timestamp are included
  const includeAuthor = !options || options.includes("author");
  const includeTimestamp = !options || options.includes("timestamp");

  if (!includeAuthor && !includeTimestamp) return "";

  const variables = [];
  if (includeTimestamp) variables.push("Timestamp");
  if (includeAuthor) variables.push("Author");

  return `
    findall(
      [${variables.map((v) => v[0]).join(", ")}],
      link(Base, _, _, ${includeTimestamp ? "T" : "_"}, ${includeAuthor ? "A" : "_"}),
      AllLinks
    ),
    sort(AllLinks, SortedLinks),
    SortedLinks = [[${variables.join(", ")}]|_]
  `;
}

function buildPropertiesQuery(properties?: string[]): string {
  // Gets the name, value, and resolve boolean for all (or some) properties on a SubjectEntity instance
  // If no properties are provided, all are included
  return `
    findall([PropertyName, PropertyValue, Resolve], (
      % Constrain to specified properties if provided
      ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}

      % Get the property name and resolve boolean
      property(SubjectClass, PropertyName),
      property_getter(SubjectClass, Base, PropertyName, PropertyValue),
      (property_resolve(SubjectClass, PropertyName) -> Resolve = true ; Resolve = false)
    ), Properties)
  `;

  // Works with literal_from_url
  // return `
  //   findall([PropertyName, PropertyValue, Resolve], (
  //     % Constrain to specified properties if provided
  //     ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}

  //     % Get the property name, URI, and resolve boolean
  //     property(SubjectClass, PropertyName),
  //     property_getter(SubjectClass, Base, PropertyName, PropertyURI),
  //     (property_resolve(SubjectClass, PropertyName) -> Resolve = true ; Resolve = false),

  //     % Decode the property value from the URI
  //     literal_from_url(PropertyURI, PropertyValue, _)
  //   ), Properties)
  // `;
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

// Todo: greaterThanOrEqualTo, lessThanOrEqualTo
function buildWhereQuery(where: WhereCondition = {}): string {
  // Constrains the query to instances that match the provided where conditions

  function formatValue(key, value) {
    // Base format on type of value by default
    let isString = typeof value === "string";
    // Override for special fields
    if (key === "author") isString = true;
    if (key === "timestamp") isString = false;
    // Wrap strings in quotes
    return isString ? `"${value}"` : value;
  }

  return (Object.entries(where) as [string, WhereCondition][])
    .map(([key, value]) => {
      const isSpecial = ["author", "timestamp"].includes(key);
      const getter = `property_getter(SubjectClass, Base, "${key}", URI), literal_from_url(URI, V, _)`;
      const field = capitalize(key);

      // Handle direct array values (for IN conditions)
      if (Array.isArray(value)) {
        const formattedValues = value.map((v) => formatValue(key, v)).join(", ");
        if (isSpecial) return `member(${field}, [${formattedValues}])`;
        else return `${getter}, member(V, [${formattedValues}])`;
      }

      // Handle operation object
      if (typeof value === "object" && value !== null) {
        const { not, lessThan, greaterThan, between } = value;

        // Handle NOT operation
        if (not !== undefined) {
          if (Array.isArray(not)) {
            // NOT IN array
            const formattedValues = not.map((v) => formatValue(key, v)).join(", ");
            if (isSpecial) return `\\+ member(${field}, [${formattedValues}])`;
            else return `${getter}, \\+ member(V, [${formattedValues}])`;
          } else {
            // NOT EQUAL
            if (isSpecial) return `${field} \\= ${formatValue(key, not)}`;
            else return `\\+ (${getter}, V = ${formatValue(key, not)})`;
          }
        }

        // Handle LESS THAN
        if (lessThan !== undefined) {
          if (isSpecial) return `${field} < ${formatValue(key, lessThan)}`;
          else return `${getter}, V < ${formatValue(key, lessThan)}`;
        }

        // Handle GREATER THAN
        if (greaterThan !== undefined) {
          if (isSpecial) return `${field} > ${formatValue(key, greaterThan)}`;
          else return `${getter}, V > ${formatValue(key, greaterThan)}`;
        }

        // Handle BETWEEN
        if (between !== undefined && Array.isArray(between) && between.length === 2) {
          if (isSpecial)
            return `${field} >= ${formatValue(key, between[0])}, ${field} =< ${formatValue(key, between[1])}`;
          else return `${getter}, V >= ${formatValue(key, between[0])}, V =< ${formatValue(key, between[1])}`;
        }
      }

      // Default to direct equality
      if (isSpecial) return `${field} = ${formatValue(key, value)}`;
      else return `${getter}, V = ${formatValue(key, value)}`;
    })
    .join(", ");
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
   * @param soruce - The source of the subject, the expression this instance is linked too.
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

  private static async assignValuesToInstance(
    perspective: PerspectiveProxy,
    instance: SubjectEntity,
    values: ValueTuple[]
  ) {
    // Map properties to object
    const propsObject = Object.fromEntries(
      await Promise.all(
        values.map(async ([name, value, resolve]) => {
          // Resolve the value if necessary
          const finalValue = resolve ? (await perspective.getExpression(value)).data : value;
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

  // static async findOne(perspective: PerspectiveProxy, query?: any) {
  //   let prologQuery = await this.getPropertiesQuery(perspective);
  //   const result = (await perspective.infer(prologQuery))[0];
  // }

  /**
   * Gets all instances of the subject entity in the perspective that match the query params .
   * @param perspective - The perspective that the subject entities belongs to.
   * @param query - The query object used to define search contraints.
   *
   * @returns An array of all subject entity matches.
   *
   */
  static async findAll(perspective: PerspectiveProxy, query?: Query) {
    // todo:
    // + order
    // + limit
    // + offset
    // + include

    const subQueries = [
      buildSourceQuery(query?.source),
      buildAuthorAndTimestampQuery(query?.properties),
      buildPropertiesQuery(query?.properties),
      buildCollectionsQuery(query?.collections),
      buildWhereQuery(query?.where),
    ];

    const fullQuery = `
      findall([Base, Properties, Collections, Timestamp, Author], (
        subject_class("${await this.getClassName(perspective)}", SubjectClass),
        instance(SubjectClass, Base),
        ${subQueries.filter((q) => q).join(", ")}
      ), AllInstances)
    `;

    if (query?.where) console.log("whereQuery!!!", buildWhereQuery(query?.where));

    const result = await perspective.infer(fullQuery);
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
        const values = [...Properties, ...Collections];
        if (!query?.properties || query.properties.includes("timestamp")) values.push(["timestamp", Timestamp]);
        if (!query?.properties || query.properties.includes("author")) values.push(["author", Author]);
        await SubjectEntity.assignValuesToInstance(perspective, instance, values);

        return instance;
      })
    );

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
}

export type SubjectArray<T> =
  | T[]
  | {
      action: "setter" | "adder" | "remover";
      value: T[];
    };

export type SubjectEntityQueryParam = {
  // The source of the query.
  source?: string;

  // The size of the query.
  size?: number;

  // The page of the query.
  page?: number;

  // conditions on properties
  where?: { condition?: string } | object;
};
