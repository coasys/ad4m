import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { relationToAdderName, relationToRemoverName, relationToSetterName, propertyNameToSetterName } from "./util";

/**
 * Represents a subject in the perspective.
 * A subject is an entity that has properties and relations.
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
     * Initializes the subject by validating it and defining its properties and relations dynamically.
     * 
     * NOTE: This method should be called before using the subject. All the properties and relations of the subject defined are not type-checked.
     */
    async init() {
        // Check if the subject is a valid instance of the subject class
        let isInstance = await this.#perspective.isSubjectInstance(this.#baseExpression, this.#subjectClassName)
        if(!isInstance) {
            throw `Not a valid subject instance of ${this.#subjectClassName} for ${this.#baseExpression}`
        }

        // Define properties and relations dynamically
        let results = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property(C, Property)`)
        let properties = results.map(result => result.Property)
        
        for(let p of properties) {
            const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), property_resolve(C, "${p}")`)
            Object.defineProperty(this, p, {
                configurable: true,
                get: async () => {
                    // Use SurrealDB for data queries
                    try {
                        return await this.#perspective.getPropertyValueViaSurreal(this.#baseExpression, this.#subjectClassName, p);
                    } catch (err) {
                        console.warn(`Failed to get property ${p} via SurrealDB:`, err);
                        return undefined;
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
        
        // Define relations
        let results2 = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection(C, Collection)`)
        if(!results2) results2 = []
        let relations = results2.map(result => result.Collection)

        for(let c of relations) {
            Object.defineProperty(this, c, {
                configurable: true,
                get: async () => {
                    // Use SurrealDB for data queries
                    try {
                        return await this.#perspective.getRelationValuesViaSurreal(this.#baseExpression, this.#subjectClassName, c);
                    } catch (err) {
                        console.warn(`Failed to get relation ${c} via SurrealDB:`, err);
                        return [];
                    }
                }
            })
        }

        // Define relation adders
        let adders = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_adder(C, Collection, Adder)`)
        if(!adders) adders = []

        for(let adder of adders) {
            if(adder) {
                const relation = adder.Collection
                const actions = eval(adder.Adder)
                this[relationToAdderName(relation)] = async (value: any) => {
                    if (Array.isArray(value)) {
                        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value: v}])))
                    } else {
                        await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                    }
                }
            }
        }

        // Define relation removers
        let removers = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_remover(C, Collection, Remover)`)
        if(!removers) removers = []

        for(let remover of removers) {
            if(remover) {
                const relation = remover.Collection
                const actions = eval(remover.Remover)
                this[relationToRemoverName(relation)] = async (value: any) => {
                    if (Array.isArray(value)) {
                        await Promise.all(value.map(v => this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value: v}])))
                    } else {
                        await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                    }
                }
            }
        }

        // Define relation setters
        let relationSetters = await this.#perspective.infer(`subject_class("${this.#subjectClassName}", C), collection_setter(C, Collection, Setter)`)
        if(!relationSetters) relationSetters = []

        for(let relationSetter of relationSetters) {
            if(relationSetter) {
                const relation = relationSetter.Collection
                const actions = eval(relationSetter.Setter)
                this[relationToSetterName(relation)] = async (value: any) => {
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