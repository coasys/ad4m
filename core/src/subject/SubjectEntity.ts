import { Literal } from "../Literal";
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
  author: string;
  timestamp: string;

  constructor(perspective: PerspectiveProxy, baseExpression?: string) {
    this.#baseExpression = baseExpression ? baseExpression : Literal.from(makeRandomPrologAtom(24)).toUrl();
    this.#perspective = perspective
  }

  get baseExpression() {
    return this.#baseExpression
  }

  private async getData(id?: string) {
    const tempId = id ?? this.#baseExpression;
    let isInstance = await this.#perspective.isSubjectInstance(tempId, this.#subjectClass)
    if (!isInstance) {
      throw `Not a valid subject instance of ${this.#subjectClass} for ${tempId}`
    }

    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property(C, Property)`)
    let properties = results.map(result => result.Property)

    for (let p of properties) {
      const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve(C, "${p}")`)
      const getProperty = async () => {
        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_getter(C, "${tempId}", "${p}", Value)`)
        if (results && results.length > 0) {
          let expressionURI = results[0].Value

          if (resolveExpressionURI) {
            const expression = await this.#perspective.getExpression(expressionURI)
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

      this[p] = await getProperty()
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

      this[c] = await getProperty()
    }

    this.#baseExpression = tempId;

    return this
  }

  private async setProperty(key: string, value: any) {
    const setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_setter(C, "${key}", Setter)`)
    if (setters && setters.length > 0) {
      const property = setters[0].Property
      const actions = eval(setters[0].Setter)
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
  }

  private async setCollectionSetter(key: string, value: any) {
    let collectionSetters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_setter(C, "${singularToPlural(key)}", Setter)`)
    if (!collectionSetters) collectionSetters = []

    if (collectionSetters.length > 0) {
      const actions = eval(collectionSetters[0].Setter)

      if (Array.isArray(value)) {
        await this.#perspective.executeAction(actions, this.#baseExpression, value.map(v => ({ name: "value", value: v })))
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
      }
    }
  }

  private async setCollectionAdder(key: string, value: any) {
    let adders = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_adder(C, "${singularToPlural(key)}", Adder)`)
    if (!adders) adders = []

    if (adders.length > 0) {
      const actions = eval(adders[0].Adder)
      if (Array.isArray(value)) {
        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])))
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
      }
    }
  }

  private async setCollectionRemover(key: string, value: any) {
    let removers = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_remover(C, "${singularToPlural(key)}", Remover)`)
    if (!removers) removers = []

    if (removers.length > 0) {
      const actions = eval(removers[0].Remover)
      if (Array.isArray(value)) {
        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value: v }])))
      } else {
        await this.#perspective.executeAction(actions, this.#baseExpression, [{ name: "value", value }])
      }
    }
  }

  async save() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)
    
    await this.#perspective.createSubject(this, this.#baseExpression);

    await this.update()
  }

  async update() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)

    const entries = Object.entries(this);

    for (const [key, value] of entries) {
      if (value) {
        await this.setProperty(key, value);
    
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
        } else {
          await this.setCollectionSetter(key, value)
        }
      }
    }
  }

  async get() {
    this.#subjectClass = await this.#perspective.stringOrTemplateObjectToSubjectClass(this)
    
    return await this.getData()
  }

  // TODO: implement simple quering like limit, skip etc.
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
}

export type SubjectArray<T> = T[] | {
  action: 'setter' | 'adder' | 'remover',
  value: T[]
}

