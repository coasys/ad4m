import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { collectionToAdderName, propertyNameToSetterName } from "./util";

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

        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property(c, Property)`)
        let properties = results.map(result => result.Property)
        //console.log("Subject properties: " + properties)
        

        for(let p of properties) {
            const resolveExpressionURI = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_resolve(c, "${p}")`)
            Object.defineProperty(this, p, {
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_getter(c, "${this.#baseExpression}", "${p}", Value)`)
                    if(results && results.length > 0) {
                        let expressionURI = results[0].Value
                        if(resolveExpressionURI) {
                            return await this.#perspective.getExpression(expressionURI)
                        } else {
                            return expressionURI
                        }
                    } else {
                        return undefined
                    }
                }
            })
        }


        const setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_setter(c, Property, Setter)`)
        
        //console.log("Subject setters: " + setters.map(setter => setter.Property))
        for(let setter of setters) {
            if(setter) {
                const property = setter.Property
                const actions = eval(setter.Setter)
                const resolveLanguageResults = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_resolve_language(c, "${property}", Language)`)
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
        

        const flattenPrologList = (list: object): any[] =>{
            let result = []
            while(list && list["head"]) {
                result.push(list["head"])
                list = list["tail"]
            }
            return result
        }

        let results2 = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), collection(c, Collection)`)
        let collections = results2.map(result => result.Collection)

        for(let c of collections) {
            Object.defineProperty(this, c, {
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), collection_getter(c, "${this.#baseExpression}", "${c}", Value)`)
                    if(results && results.length > 0 && results[0].Value) {
                        return flattenPrologList(eval(results[0].Value))
                    } else {
                        return []
                    }
                }
            })
        }

        let adders = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), collection_adder(c, Collection, Adder)`)

        for(let adder of adders) {
            if(adder) {
                const collection = adder.Collection
                const actions = eval(adder.Adder)
                this[collectionToAdderName(collection)] = async (value: any) => {
                    //console.log("Setting property: " + property + " to " + value)
                    //console.log("Actions: " + JSON.stringify(actions))
                    await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                }
            }
        }
    }
}