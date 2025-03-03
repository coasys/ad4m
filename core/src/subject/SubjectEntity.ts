import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom } from "./SDNADecorators";
import { singularToPlural } from "./util";

export type QueryPartialEntity<T> = {
  [P in keyof T]?: T[P] | (() => string);
};

type ValueTuple = [name: string, value: any, resolve?: boolean];

const AUTHOR = `
  % Gets the author and timestamp of a SubjectEntity instance (based on the first link mentioning the base)
  findall([A], link(Base, _, _, _, A), AllLinks),
  sort(AllLinks, SortedLinks),
  SortedLinks = [[Author]|_]
`;

const TIMESTAMP = `
  % Gets the author and timestamp of a SubjectEntity instance (based on the first link mentioning the base)
  findall([T], link(Base, _, _, T, _), AllLinks),
  sort(AllLinks, SortedLinks),
  SortedLinks = [[Timestamp]|_]
`;

const AUTHOR_AND_TIMESTAMP = `
  % Gets the author and timestamp of a SubjectEntity instance (based on the first link mentioning the base)
  findall([T, A], link(Base, _, _, T, A), AllLinks),
  sort(AllLinks, SortedLinks),
  SortedLinks = [[Timestamp, Author]|_]
`;

function buildPropertiesQuery(properties?: string[]) {
  return `
    % Gets the name, value, and resolve boolean for all (or some) properties on a SubjectEntity instance
    findall([PropertyName, PropertyValue, Resolve], (
      % Constrain to specified properties if provided
      ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}
      property(SubjectClass, PropertyName),
      property_getter(SubjectClass, Base, PropertyName, PropertyValue),
      (property_resolve(SubjectClass, PropertyName) -> Resolve = true ; Resolve = false)
    ), Properties)
  `;
}

function buildCollectionsQuery(collections?: string[]) {
  return `
    % Gets the name and array of values for all (or some) collections on a SubjectEntity instance
    findall([CollectionName, CollectionValues], (
      % Constrain to specified collections if provided
      ${collections ? `member(CollectionName, [${collections.map((name) => `"${name}"`).join(", ")}]),` : ""}
      collection(SubjectClass, CollectionName),
      collection_getter(SubjectClass, Base, CollectionName, CollectionValues)
    ), Collections)
  `;
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

  // todo: only return Properties, Timestamp, & Author from prolog query (Base, AllLinks, and SortLinks not required)
  private async getData() {
    // Builds an object with all the author, timestamp, properties, & collections on the SubjectEntity and saves it to the instance
    const prologQuery = `
      Base = "${this.#baseExpression}",
      subject_class("${this.#subjectClassName}", SubjectClass),
      ${AUTHOR_AND_TIMESTAMP},
      ${buildPropertiesQuery()},
      ${buildCollectionsQuery()}
    `;

    const result = await this.#perspective.infer(prologQuery);

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

  // todo:
  // + add source prop
  // + get queries prop working with:
  //    + collections
  //    + where
  //    + order
  //    + limit
  //    + offset
  //    + include
  static async findAll(perspective: PerspectiveProxy, query?: any) {
    // Find all instances of the subject entity that match the query
    const subjectClassName = await this.getClassName(perspective);

    // Set default queries
    let propertiesQuery = buildPropertiesQuery();
    let collectionsQuery = buildCollectionsQuery();
    let authorAndTimestampQuery = AUTHOR_AND_TIMESTAMP;

    // Update properties query
    const requestedProperties = Array.isArray(query?.properties) ? query.properties : null;
    if (requestedProperties) {
      propertiesQuery = buildPropertiesQuery(requestedProperties);
      // Update author and timestamp query
      const includeAuthor = requestedProperties.includes("author");
      const includeTimestamp = requestedProperties.includes("timestamp");
      if (includeAuthor && !includeTimestamp) authorAndTimestampQuery = AUTHOR;
      else if (!includeAuthor && includeTimestamp) authorAndTimestampQuery = TIMESTAMP;
      else if (!includeAuthor && !includeTimestamp) authorAndTimestampQuery = "";
    }

    // Update collections query
    const requestedCollections = Array.isArray(query?.collections) ? query.collections : null;
    if (requestedCollections) collectionsQuery = buildCollectionsQuery(requestedCollections);

    // Build full query
    const subQueries = [propertiesQuery, collectionsQuery, authorAndTimestampQuery].filter((q) => q);
    const fullQuery = `
      findall([Base, Properties, Collections, Timestamp, Author], (
        subject_class("${subjectClassName}", SubjectClass),
        instance(SubjectClass, Base),
        ${subQueries.join(", ")}
      ), AllInstances)
    `;

    const result = await perspective.infer(fullQuery);

    if (!result?.[0]?.AllInstances) return [];

    // Map results to instances
    const requestedAttribtes = [...(requestedProperties || []), ...(requestedCollections || [])];
    const allInstances = await Promise.all(
      result[0].AllInstances.map(async ([Base, Properties, Collections, Timestamp, Author]) => {
        const instance = new this(perspective, Base) as any;
        if (requestedAttribtes.length) {
          // remove unrequested attributes from instance
          Object.keys(instance).forEach((key) => {
            if (!requestedAttribtes.includes(key)) delete instance[key];
          });
        }
        const values = [...Properties, ...Collections];
        if (!requestedProperties || requestedProperties.includes("timestamp")) values.push(["timestamp", Timestamp]);
        if (!requestedProperties || requestedProperties.includes("author")) values.push(["author", Author]);
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

  /**
   * Get all the subject entities of the subject class.
   *
   * NOTE: this is a static method and should be called on the class itself.
   *
   * @param perspective - The perspective that the subject belongs to.
   *
   * @returns The subject entities.
   *
   * @throws Will throw an error if the subject entity cannot be converted to a subject class, or if the subject proxies cannot be gotten.
   */
  // static async all(perspective: PerspectiveProxy) {
  //   let subjectClassName = await perspective.stringOrTemplateObjectToSubjectClassName(this)
  //   const proxies = await perspective.getAllSubjectProxies(subjectClassName)

  //   const instances = []

  //   if (proxies) {
  //     for (const proxy of proxies) {
  //       // @ts-ignore
  //       const instance = new this(perspective, proxy.X)
  //       instances.push(await instance.get())
  //     }

  //     return instances;
  //   }

  //   return []
  // }

  // {
  //   attributes: ['id', 'type', 'title', 'author', 'timestamp'],
  //   where: { id: [1, 2, 3, 4, 5], type: 'poll' },
  //   order: ['id', 'descending'],
  //   limit: 10,
  //   offset: 30,
  // }

  static async all(perspective: PerspectiveProxy, query?: any) {
    console.log("SubjectEntity.all() query", query);
    let subjectClassName = await this.getClassName(perspective);

    // Build the base Prolog query
    let baseQuery = `subject_class("${subjectClassName}", C), instance(C, Base)`;
    let selectProperties = [];
    let whereConditions = [];

    // Process the query parameters
    if (query) {
      // Handle property selection (attributes)
      if (query.attributes && Array.isArray(query.attributes)) {
        for (const attr of query.attributes) {
          const varName = attr.charAt(0).toUpperCase() + attr.slice(1);
          selectProperties.push(`property_getter(C, Base, "${attr}", ${varName})`);
        }
      } else {
        // If no attributes specified, get all properties of the subject class
        const propertiesQuery = `findall(Prop, property(C, Prop), Props)`;
        const properties = await perspective.infer(`subject_class("${subjectClassName}", C), ${propertiesQuery}`);

        if (properties && properties.length > 0 && properties[0].Props) {
          for (const prop of properties[0].Props) {
            const varName = prop.charAt(0).toUpperCase() + prop.slice(1);
            selectProperties.push(`property_getter(C, Base, "${prop}", ${varName})`);
          }
        }
      }

      // Handle where conditions
      if (query.where) {
        for (const [key, value] of Object.entries(query.where)) {
          if (Array.isArray(value)) {
            // Handle array of values (IN condition)
            const valueList = value.map((v) => (typeof v === "string" ? `"${v}"` : v)).join(", ");
            whereConditions.push(`property_getter(C, Base, "${key}", TempVal), member(TempVal, [${valueList}])`);
          } else {
            // Handle simple equality
            const formattedValue = typeof value === "string" ? `"${value}"` : value;
            whereConditions.push(`property_getter(C, Base, "${key}", ${formattedValue})`);
          }
        }
      }
    } else {
      // If no query specified, get all properties
      const propertiesQuery = `findall(Prop, property(C, Prop), Props)`;
      const properties = await perspective.infer(`subject_class("${subjectClassName}", C), ${propertiesQuery}`);

      if (properties && properties.length > 0 && properties[0].Props) {
        for (const prop of properties[0].Props) {
          const varName = prop.charAt(0).toUpperCase() + prop.slice(1);
          selectProperties.push(`property_getter(C, Base, "${prop}", ${varName})`);
        }
      }
    }

    // Create the complete query
    let prologQuery = `findall([Base`;

    // Create a mapping of variable names to property names
    let varToPropertyMap = {};
    if (query && query.attributes && Array.isArray(query.attributes)) {
      query.attributes.forEach((attr) => {
        const varName = attr.charAt(0).toUpperCase() + attr.slice(1);
        prologQuery += `, ${varName}`;
        varToPropertyMap[varName] = attr;
      });
    } else {
      const properties = await perspective.infer(
        `subject_class("${subjectClassName}", C), findall(Prop, property(C, Prop), Props)`
      );
      if (properties && properties.length > 0 && properties[0].Props) {
        for (const prop of properties[0].Props) {
          const varName = prop.charAt(0).toUpperCase() + prop.slice(1);
          prologQuery += `, ${varName}`;
          varToPropertyMap[varName] = prop;
        }
      }
    }

    prologQuery += `], (${baseQuery}`;

    // Add property getter conditions
    if (selectProperties.length > 0) {
      prologQuery += `, ${selectProperties.join(", ")}`;
    }

    // Add where conditions
    if (whereConditions.length > 0) {
      prologQuery += `, ${whereConditions.join(", ")}`;
    }

    prologQuery += `), Results)`;

    // Handle sorting
    if (query?.order && Array.isArray(query.order)) {
      const [orderField, direction] = query.order;
      const needsReverse = direction === "descending";

      prologQuery += `, sort_with_key(Results, ${orderField}, SortedResults)`;

      if (needsReverse) {
        prologQuery += `, reverse(SortedResults, OrderedResults)`;
      } else {
        prologQuery += `, OrderedResults = SortedResults`;
      }
    } else {
      prologQuery += `, OrderedResults = Results`;
    }

    // Handle pagination (limit and offset)
    if (query && query.limit !== undefined) {
      const offset = query.offset || 0;

      if (offset > 0) {
        prologQuery += `, length(OrderedResults, TotalLength), 
                       (TotalLength > ${offset} -> 
                          append(_, Skip, OrderedResults), 
                          length(Skip, ${offset}), 
                          (Skip = [] -> FinalResults = [] ; 
                            append(PageResults, _, Skip), 
                            length(PageResults, ${query.limit}), 
                            FinalResults = PageResults) ; 
                          FinalResults = [])`;
      } else {
        prologQuery += `, append(PageResults, _, OrderedResults), 
                       length(PageResults, ${query.limit}), 
                       FinalResults = PageResults`;
      }
    } else {
      prologQuery += `, FinalResults = OrderedResults`;
    }

    // Execute the Prolog query
    const prologResults = await perspective.infer(prologQuery);

    console.log("Prolog results", prologResults);

    if (!prologResults || prologResults.length === 0 || !prologResults[0].FinalResults) {
      return [];
    }

    // Convert the Prolog results to objects
    const instances = [];
    for (const result of prologResults[0].FinalResults) {
      const baseExpr = result[0];

      // @ts-ignore
      const instance = new this(perspective, baseExpr);

      // Instead of calling instance.get(), construct the object directly
      const data = {};

      let idx = 1;
      for (const varName in varToPropertyMap) {
        const propName = varToPropertyMap[varName];
        data[propName] = result[idx++];
      }

      // Assign properties directly to instance
      Object.assign(instance, data);

      instances.push(instance);
    }

    return instances;
  }

  /**
   * Query the subject entities of the subject class.
   *
   * NOTE: this is a static method and should be called on the class itself.
   *
   * @param perspective - The perspective that the subject belongs to.
   * @param query - The query of the subject entities.
   *
   * @returns The subject entities.
   *
   * @throws Will throw an error if the subject entity cannot be converted to a subject class, or if the query cannot be inferred, or if the data of the subject entities cannot be gotten.
   */
  static async query(perspective: PerspectiveProxy, query?: SubjectEntityQueryParam) {
    const source = query?.source || "ad4m://self";
    let subjectClassName = await perspective.stringOrTemplateObjectToSubjectClassName(this);

    let res = [];
    let instanceConditions = `subject_class("${subjectClassName}", C), instance(C, Base), link("${source}", Predicate, Base, Timestamp, Author)`;

    if (query) {
      if (query.where) {
        if (query.where["condition"]) {
          instanceConditions += ", " + query.where["condition"];
        } else {
          const pairs = Object.entries(query.where);
          for (let p of pairs) {
            const propertyName = p[0];
            const propertyValue = p[1];
            instanceConditions += `, property_getter(C, Base, "${propertyName}", "${propertyValue}")`;
          }
        }
      }

      try {
        const queryResponse = (
          await perspective.infer(
            `findall([Timestamp, Base], (${instanceConditions}), AllData), sort(AllData, SortedData), length(SortedData, DataLength).`
          )
        )[0];

        if (queryResponse.DataLength >= query.size) {
          const mainQuery = `findall([Timestamp, Base], (${instanceConditions}), AllData), sort(AllData, SortedData), reverse(SortedData, ReverseSortedData), paginate(ReverseSortedData, ${query.page}, ${query.size}, PageData).`;

          res = await perspective.infer(mainQuery);

          res = res[0].PageData.map((r) => ({
            Base: r[1],
            Timestamp: r[0],
          }));
        } else {
          res = await perspective.infer(instanceConditions);
        }
      } catch (e) {
        console.log("Query failed", e);
      }
    } else {
      res = await perspective.infer(instanceConditions);
    }

    if (!res) return [];

    const data = await Promise.all(
      res.map(async (result) => {
        const instance = new this(perspective, result.Base);

        return await instance.get();
      })
    );

    return data;
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
