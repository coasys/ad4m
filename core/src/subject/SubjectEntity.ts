import { Literal } from "../Literal";
import { Link } from "../links/Links";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom } from "./SDNADecorators";
import { singularToPlural } from "./util";

export type QueryPartialEntity<T> = {
  [P in keyof T]?: T[P] | (() => string);
};


/**
 * Class representing a subject entity.
 * Can extend this class to create a new subject entity to add methods interact with SDNA and much better experience then using the bare bone methods.
 */
export class SubjectEntity {
  #baseExpression: string;
  #subjectClass: string;
  #source: string;
  #perspective: PerspectiveProxy
  author: string;
  timestamp: string;


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
    return this.#baseExpression
  }

  /**
   * Protected getter for the perspective.
   * Allows subclasses to access the perspective while keeping it private from external code.
   */
  protected get perspective(): PerspectiveProxy {
    return this.#perspective;
  }

  private async getData(id?: string) {
    const tempId = id ?? this.#baseExpression;
    let data = await this.#perspective.getSubjectData(this.#subjectClass, tempId)
    Object.assign(this, data);
    this.#baseExpression = tempId;
    return this
  }

  private async setProperty(key: string, value: any) {
    const setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_setter(C, "${key}", Setter)`)
    if (setters && setters.length > 0) {
      const actions = eval(setters[0].Setter)
      const resolveLanguageResults = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve_language(C, "${key}", Language)`)
      let resolveLanguage
      if (resolveLanguageResults && resolveLanguageResults.length > 0) {
        resolveLanguage = resolveLanguageResults[0].Language
      }

      if (resolveLanguage) {
        value = await this.#perspective.createExpression(value, resolveLanguage)
      }
      await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
    }
  }

  private async setCollectionSetter(key: string, value: any) {
    let collectionSetters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_setter(C, "${singularToPlural(key)}", Setter)`)
    if (!collectionSetters) collectionSetters = []

    if (collectionSetters.length > 0) {
      const actions = eval(collectionSetters[0].Setter)

      if (value) {
        if (Array.isArray(value)) {
          await this.#perspective.executeAction(actions, this.#baseExpression, value.map(v => ({ name: "value", value: v })))
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
        }
      }
    }
  }

  private async setCollectionAdder(key: string, value: any) {
    let adders = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_adder(C, "${singularToPlural(key)}", Adder)`)
    if (!adders) adders = []

    if (adders.length > 0) {
      const actions = eval(adders[0].Adder)
      if (value) {
        if (Array.isArray(value)) {
          await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])))
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
        }
      }
    }
  }

  private async setCollectionRemover(key: string, value: any) {
    let removers = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_remover(C, "${singularToPlural(key)}", Remover)`)
    if (!removers) removers = []

    if (removers.length > 0) {
      const actions = eval(removers[0].Remover)
      if (value) {
        if (Array.isArray(value)) {
          await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])))
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
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
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)

    await this.#perspective.createSubject(this, this.#baseExpression);

    await this.#perspective.add(
      new Link({
        source: this.#source,
        predicate: "ad4m://has_child",
        target: this.baseExpression,
      })
    );

    await this.update()
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
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)

    const entries = Object.entries(this);

    for (const [key, value] of entries) {
      if (value !== undefined && value !== null) {
        if (value?.action) {
          switch (value.action) {
            case 'setter':
              await this.setCollectionSetter(key, value.value)
              break;
            case "adder":
              await this.setCollectionAdder(key, value.value)
              break;
            case 'remover':
              await this.setCollectionRemover(key, value.value)
            default:
              await this.setCollectionSetter(key, value.value)
              break;
          }
        } else if (Array.isArray(value) && value.length > 0) {
          await this.setCollectionSetter(key, value)
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
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)

    return await this.getData()
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
  static async all(perspective: PerspectiveProxy) {
    let subjectClass = await perspective.stringOrTemplateObjectToSubjectClass(this)
    const proxies = await perspective.getAllSubjectProxies(subjectClass)

    const instances = []

    if (proxies) {
      for (const proxy of proxies) {
        // @ts-ignore
        const instance = new this(perspective, proxy.X)
        instances.push(await instance.get())
      }

      return instances;
    }

    return []
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
    let subjectClass = await perspective.stringOrTemplateObjectToSubjectClass(this)

    let res = [];
    let instanceConditions = `subject_class("${subjectClass}", C), instance(C, Base), link("${source}", Predicate, Base, Timestamp, Author)`

    if (query) {  
      if(query.where) {
        if(query.where["condition"]) {
          instanceConditions += ", " + query.where["condition"]
        } else {
          const pairs = Object.entries(query.where);
          for(let p of pairs) {
            const propertyName = p[0];
            const propertyValue = p[1];
            instanceConditions += `, property_getter(C, Base, "${propertyName}", "${propertyValue}")`
          }
        }
      }

      try {
        const queryResponse = (await perspective.infer(`findall([Timestamp, Base], (${instanceConditions}), AllData), sort(AllData, SortedData), length(SortedData, DataLength).`))[0]

        if (queryResponse.DataLength >= query.size) {
          const mainQuery = `findall([Timestamp, Base], (${instanceConditions}), AllData), sort(AllData, SortedData), reverse(SortedData, ReverseSortedData), paginate(ReverseSortedData, ${query.page}, ${query.size}, PageData).`

          res = await perspective.infer(mainQuery);

          res = res[0].PageData.map(r => ({
            Base: r[1],
            Timestamp: r[0]
          }))
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
        const instance = new this(perspective, result.Base)

        return await instance.get();
      })
    );

    return data;
  }
}

export type SubjectArray<T> = T[] | {
  action: 'setter' | 'adder' | 'remover',
  value: T[]
}

export type SubjectEntityQueryParam = {
  // The source of the query.
  source?: string;

  // The size of the query.
  size?: number;

  // The page of the query.
  page?: number;

  // conditions on properties
  where?: { condition?: string } | object;
}