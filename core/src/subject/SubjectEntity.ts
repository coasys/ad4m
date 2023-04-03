import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { makeRandomPrologAtom } from "./SDNADecorators";
import { singularToPlural } from "./util";

const flattenPrologList = (list: object): any[] => {
  let result = []
  while (list && list["head"]) {
    result.push(list["head"])
    list = list["tail"]
  }
  return result
}

export type QueryPartialEntity<T> = {
  [P in keyof T]?: T[P] | (() => string);
};


export class SubjectEntity {
  #baseExpression: string;
  #subjectClass: string;
  #perspective: PerspectiveProxy

  constructor(perspective: PerspectiveProxy, baseExpression?: string) {
    this.#baseExpression = baseExpression ? baseExpression : `entry://${makeRandomPrologAtom(24)}`;
    this.#perspective = perspective
  }

  get baseExpression() {
    return this.#baseExpression
  }

  private async getData(id?: string) {
    const tempId = id || this.baseExpression;
    let isInstance = await this.#perspective.isSubjectInstance(tempId, this.#subjectClass)
    if (!isInstance) {
      throw `Not a valid subject instance of ${this.#subjectClass} for ${tempId}`
    }
    await this.#perspective.createSubject(this.#subjectClass, tempId);

    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property(C, Property)`)
    let properties = results.map(result => result.Property)

    const obj = {}

    for (let p of properties) {
      const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve(C, "${p}")`)
      const getProperty = async () => {
        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_getter(C, "${tempId}", "${p}", Value)`)
        if (results && results.length > 0) {
          let expressionURI = results[0].Value

          if (resolveExpressionURI) {
            const expression = await this.#perspective.getExpression(expressionURI)
            obj['author'] = expression.author;
            obj['timestamp'] = expression.timestamp;
            try {
              return JSON.parse(expression.data)
            } catch (e) {
              return expression.data
            }
          } else {
            return expressionURI
          }
        } else if (results != undefined) {
          if (results) {
            return results
          } else {
            return false
          }
        } else {
          return undefined
        }
      };

      obj[p] = await getProperty()
    }

    let results2 = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection(C, Collection)`)
    if (!results2) results2 = []
    let collections = results2.map(result => result.Collection)

    for (let c of collections) {
      const getProperty = async () => {
        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_getter(C, "${tempId}", "${c}", Value)`)
        if (results && results.length > 0 && results[0].Value) {
          return flattenPrologList(eval(results[0].Value))
        } else {
          return []
        }
      }

      obj[c] = await getProperty()
    }

    obj['id'] = tempId;

    return obj;
  }

  private async setProperties(key: string, value: any) {
    const setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_setter(C, Property, Setter)`)
    if (setters.length > 0) {
      const setter = setters.find(e => e.Property === key)

      if (setter) {
        const property = setter.Property
        const actions = eval(setter.Setter)
        const resolveLanguageResults = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve_language(C, "${property}", Language)`)
        let resolveLanguage
        if (resolveLanguageResults && resolveLanguageResults.length > 0) {
          resolveLanguage = resolveLanguageResults[0].Language
        }

        if (resolveLanguage) {
          value = await this.#perspective.createExpression(value, resolveLanguage)
        }
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
      }
    } else {
      throw Error("No setters found of the subject class")
    }
  }

  private async setCollectionSetter(key: string, value: any) {
    let collectionSetters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_setter(C, Collection, Setter)`)
    if (!collectionSetters) collectionSetters = []

    if (collectionSetters.length > 0) {
      const collectionSetter = collectionSetters.find(e => e.Collection === singularToPlural(key))

      if (collectionSetter) {
        const collection = collectionSetter.Collection
        const actions = eval(collectionSetter.Setter)

        if (Array.isArray(value)) {
          await this.#perspective.executeAction(actions, this.#baseExpression, value.map(v => ({ name: "value", value: v })))
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
        }
      }
    }
  }

  private async setCollectionAdder(key: string, value: any) {
    let adders = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_adder(C, Collection, Adder)`)
    if (!adders) adders = []

    if (adders.length > 0) {
      const adder = adders.find(e => e.Collection === singularToPlural(key))

      if (adder) {
        const collection = adder.Collection
        const actions = eval(adder.Adder)
        if (Array.isArray(value)) {
          await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])))
        } else {
          await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
        }
      }
    }
  }

  async save() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)
    
    const subject = await this.#perspective.createSubject(this, this.#baseExpression);

    for (const [key, value] of Object.entries(this)) {
      await this.setProperties(key, value);

      if (value?.action) {
        switch (value?.action) {
          case 'setter':
            await this.setCollectionSetter(key, value.value)
            break;
          case "adder":
            await this.setCollectionAdder(key, value.value)
            break;
          default:
            await this.setCollectionSetter(key, value.value)
            break;
        }
      } else {
        await this.setCollectionSetter(key, value)
      }
    }
  }

  async update() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)

    for (const [key, value] of Object.entries(this)) {
      await this.setProperties(key, value);

      if (value.action) {
        switch (value.action) {
          case 'setter':
            await this.setCollectionSetter(key, value.value)
            break;
          case "adder":
            await this.setCollectionAdder(key, value.value)
            break;
          default:
            await this.setCollectionSetter(key, value.value)
            break;
        }
      } else {
        await this.setCollectionSetter(key, value)
      }
    }
  }

  async get() {
    return await this.getData()
  }

  // TODO: implement simple quering like limit, skip etc.
  async find() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)
    const proxies = await this.#perspective.getAllSubjectProxies(this.#subjectClass)

    const instances = []

    if (proxies) {
      for (const proxy of proxies) {
        // @ts-ignore
        instances.push(await this.getData(proxy.X))
      }

      return instances;
    }

    return []

  }
}

export type SubjectArray<T> = T[] | {
  action: 'setter' | 'adder',
  value: T[]
}

