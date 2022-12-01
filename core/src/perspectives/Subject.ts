import { PerspectiveProxy } from "./PerspectiveProxy";

export class Subject {
    #baseExpression: string;
    #subjectClass: string;
    #perspective: PerspectiveProxy

    constructor(perspective: PerspectiveProxy, baseExpression: string, subjectClass: string) {
        this.#baseExpression = baseExpression
        this.#subjectClass = subjectClass
        this.#perspective = perspective
    }

    async init() {
        let isInstance = await this.#perspective.isSubjectInstance(this.#baseExpression, this.#subjectClass)
        if(!isInstance) {
            throw `Not a valid subject instance of ${this.#subjectClass} for ${this.#baseExpression}`
        }

        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property(c, Property)`)
        let properties = results.map(result => result.Property)
        console.log("Subject properties: " + properties)
        

        for(let p of properties) {
            Object.defineProperty(this, p, {
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_getter(c, "${this.#baseExpression}", "${p}", Value)`)
                    if(results && results.length > 0) {
                        return results[0].Value
                    } else {
                        return undefined
                    }
                }
            })
        }


        let setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), property_setter(c, Property, Setter)`)
        console.log("Subject setters: " + setters.map(setter => setter.Property))
        for(let setter of setters) {
            if(setter) {
                const property = setter.Property
                const actions = eval(setter.Setter)
                const capitalized = property.charAt(0).toUpperCase() + property.slice(1)
                this[`set${capitalized}`] = async (value: any) => {
                    console.log("Setting property: " + property + " to " + value)
                    console.log("Actions: " + JSON.stringify(actions))
                    await this.#perspective.executeAction(actions, this.#baseExpression, [{name: "value", value}])
                }
            }
        }
        


        
    }
}