import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { collectionSetterToName, collectionToAdderName, collectionToRemoverName, collectionToSetterName, propertyNameToSetterName } from "./util";

/**
 * Represents a subject in the perspective.
 * A subject is an entity that has properties and collections.
 */
export class Subject {
    #baseExpression: string;
    #subjectClassName: string;
    #perspective: PerspectiveProxy

    /**
     * Constructs a new subject.
     * @param perspective - The perspective that the subject belongs to.
     * @param baseExpression - The base expression of the subject.
     * @param subjectClassName - The class name of the subject.
     */
    constructor(perspective: PerspectiveProxy, baseExpression: string, subjectClassName: string) {
        this.#baseExpression = baseExpression
        this.#subjectClassName = subjectClassName
        this.#perspective = perspective
    }

    /**
     * Gets the base expression of the subject.
     */
    get baseExpression() {
        return this.#baseExpression
    }

    /**
     * Initializes the subject by validating it and defining its properties and collections dynamically.
     * 
     * NOTE: This method should be called before using the subject. All the properties and collections of the subject defined are not type-checked.
     */
    async init() {
        // Check if the subject is a valid instance of the subject class
        let isInstance = await this.#perspective.isSubjectInstance(this.#baseExpression, this.#subjectClassName)
        if(!isInstance) {
            throw `Not a valid subject instance of ${this.#subjectClassName} for ${this.#baseExpression}`
        }

        // Define properties and collections dynamically
        let results = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property(C, Property)`)
        let properties = results.map(result => result.Property)
        
        for(let p of properties) {
            const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property_resolve(C, "${p}")`)
            Object.defineProperty(this, p, {
                configurable: true,
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property_getter(C, "${this.#baseExpression}", "${p}", Value)`)
                    if(results && results.length > 0) {
                        let expressionURI = results[0].Value
                        if(resolveExpressionURI) {
                            try {
                                if (expressionURI) {
                                    const expression = await this.#perspective.getExpression(expressionURI)
                                    try {
                                        return JSON.parse(expression.data)
                                    } catch(e) {
                                        return expression.data
                                    }
                                } else {
                                    return expressionURI
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

        // Define setters
        const setters = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property_setter(C, Property, Setter)`)

        for(let setter of (setters ? setters : [])) {
            if(setter) {
                const property = setter.Property
                const actions = eval(setter.Setter)
                const resolveLanguageResults = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property_resolve_language(C, "${property}", Language)`)
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
        
        // Define collections
        let results2 = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection(C, Collection)`)
        if(!results2) results2 = []
        let collections = results2.map(result => result.Collection)

        for(let c of collections) {
            Object.defineProperty(this, c, {
                configurable: true,
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_getter(C, "${this.#baseExpression}", "${c}", Value)`)
                    if(results && results.length > 0 && results[0].Value) {
                        let collectionContent = results[0].Value.filter((v: any) => v !== "" && v !== '')
                        return collectionContent
                    } else {
                        return []
                    }
                }
            })
        }

        // Define collection adders
        let adders = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_adder(C, Collection, Adder)`)
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

        // Define collection removers
        let removers = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_remover(C, Collection, Remover)`)
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

        // Define collection setters
        let collectionSetters = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_setter(C, Collection, Setter)`)
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