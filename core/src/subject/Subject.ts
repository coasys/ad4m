import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { collectionSetterToName, collectionToAdderName, collectionToRemoverName, collectionToSetterName, propertyNameToSetterName } from "./util";

export class Subject {
    #baseExpression: string;
    #subjectClass: string;
    #perspective: PerspectiveProxy

    constructor(perspective: PerspectiveProxy, baseExpression: string, subjectClass: string) {
        this.#baseExpression = baseExpression
        this.#subjectClass = subjectClass
        this.#perspective = perspective
    }

    get baseExpression() {
        return this.#baseExpression
    }

    async init() {
        let isInstance = await this.#perspective.isSubjectInstance(this.#baseExpression, this.#subjectClass)
        if(!isInstance) {
            throw `Not a valid subject instance of ${this.#subjectClass} for ${this.#baseExpression}`
        }

        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property(C, Property)`)
        let properties = results.map(result => result.Property)
        //console.log("Subject properties: " + properties)
        

        for(let p of properties) {
            const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve(C, "${p}")`)
            Object.defineProperty(this, p, {
                configurable: true,
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_getter(C, "${this.#baseExpression}", "${p}", Value)`)
                    if(results && results.length > 0) {
                        let expressionURI = results[0].Value
                        if(resolveExpressionURI) {
                            try {
                                const expression = await this.#perspective.getExpression(expressionURI)
                                try {
                                    return JSON.parse(expression.data)
                                } catch(e) {
                                    return expression.data
                                }
                            } catch (err) {
                                return expressionURI
                            }
                        } else {
                            return expressionURI
                        }
                    } else if(results) {
                        return results
                    } else {
                        return undefined
                    }
                }
            })
        }


        const setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_setter(C, Property, Setter)`)
        
        //console.log("Subject setters: " + setters.map(setter => setter.Property))
        for(let setter of (setters ? setters : [])) {
            if(setter) {
                const property = setter.Property
                const actions = eval(setter.Setter)
                const resolveLanguageResults = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), property_resolve_language(C, "${property}", Language)`)
                let resolveLanguage
                if(resolveLanguageResults && resolveLanguageResults.length > 0) {
                    resolveLanguage = resolveLanguageResults[0].Language
                }
                this[propertyNameToSetterName(property)] = async (value: any) => {
                    if(resolveLanguage) {
                        value = await this.#perspective.createExpression(value, resolveLanguage)
                    }
                    await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                }
            }
        }
        
        let results2 = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection(C, Collection)`)
        if(!results2) results2 = []
        let collections = results2.map(result => result.Collection)

        for(let c of collections) {
            Object.defineProperty(this, c, {
                configurable: true,
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_getter(C, "${this.#baseExpression}", "${c}", Value)`)
                    if(results && results.length > 0 && results[0].Value) {
                        let collectionContent = results[0].Value.filter((v: any) => v !== "" && v !== '')
                        return collectionContent
                    } else {
                        return []
                    }
                }
            })
        }

        let adders = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_adder(C, Collection, Adder)`)
        if(!adders) adders = []

        for(let adder of adders) {
            if(adder) {
                const collection = adder.Collection
                const actions = eval(adder.Adder)
                this[collectionToAdderName(collection)] = async (value: any) => {
                    if (Array.isArray(value)) {
                        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value: v}])))
                    } else {
                        await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                    }
                }
            }
        }

        let removers = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_remover(C, Collection, Remover)`)
        if(!removers) removers = []

        for(let remover of removers) {
            if(remover) {
                const collection = remover.Collection
                const actions = eval(remover.Remover)
                this[collectionToRemoverName(collection)] = async (value: any) => {
                    if (Array.isArray(value)) {
                        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value: v}])))
                    } else {
                        await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                    }
                }
            }
        }

        let collectionSetters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", C), collection_setter(C, Collection, Setter)`)
        if(!collectionSetters) collectionSetters = []

        for(let collectionSetter of collectionSetters) {
            if(collectionSetter) {
                const collection = collectionSetter.Collection
                const actions = eval(collectionSetter.Setter)
                this[collectionToSetterName(collection)] = async (value: any) => {
                    if (Array.isArray(value)) {
                        await this.#perspective.executeAction(actions, this.#baseExpression, value.map(v => ({name: "value", value: v})))
                    } else {
                        await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                    }
                }
            }
        }
    }
}