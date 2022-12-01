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

        let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), instance_property(c, _, Property, _)`)
        let properties = results.map(result => result.Property)
        let setters = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), instance_property_setter(c, Property, Setter)`)
        for(let p of properties) {
            let propObject = {
                get: async () => {
                    let results = await this.#perspective.infer(`subject_class("${this.#subjectClass}", c), instance_property(c, "${this.#baseExpression}", "${p}", Value)`)
                    if(results && results.length > 0) {
                        return results[0].Value
                    } else {
                        return undefined
                    }
                }
            }

            let setter = setters.find(s => s.Property === p)
            if(setter) {
                propObject['set'] = async (value: any) => {
                    await this.#perspective.executeAction(setter, this.#baseExpression, [{name: "value", value}])
                }
            }

            Object.defineProperty(this, p, propObject)
        }
    }
}