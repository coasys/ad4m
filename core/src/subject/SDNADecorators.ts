import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { Subject } from "./Subject";
import { collectionToAdderName, propertyNameToSetterName, stringifyObjectLiteral } from "./util";

export class PerspectiveAction {
    action: string
    source: string
    predicate: string
    target: string
}

export function addLink(source: string, predicate: string, target: string): PerspectiveAction {
    return {
        action: "addLink",
        source,
        predicate,
        target,
    };
}

export function hasLink(predicate: string): string {
    return `triple(this, "${predicate}", _)`
}

interface InstanceQueryParams {
    where?: string;
}

export function instanceQuery(options?: InstanceQueryParams) {
    return function <T>(target: T, key: keyof T, descriptor: PropertyDescriptor) {
        const originalMethod = descriptor.value;
        if(typeof originalMethod !== "function") {
            throw new Error("instanceQuery decorator can only be applied to methods");
        }

        descriptor.value = async function(perspective: PerspectiveProxy): Promise<T[]> {
            let instances: T[] = []
            //@ts-ignore
            let subjectClassName = target.name
            let query = `subject_class("${subjectClassName}", c), instance(c, Instance)`
            if(options && options.where) {
                query += `, ${options.where}`
            }

            let results = await perspective.infer(query)
            for(let result of results) {
                let instance = result.Instance
                console.log("Instance: " + instance)
                let subject = new Subject(perspective, instance, subjectClassName)
                await subject.init()
                instances.push(subject as T)
            }

            return instances
        }
    };
}

interface PropertyOptions {
    through: string;
    initial?: string,
    required?: boolean,
}
export function subjectProperty(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = opts;
        Object.defineProperty(target, key, {});
        //return descriptor;
    };
}

export function subjectCollection(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__collections"] = target["__collections"] || {};
        target["__collections"][key] = opts;
        Object.defineProperty(target, key, {});
        //return descriptor;
    };
}

export function sdnaOutput(target: any, key: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;
    if(typeof originalMethod !== "function") {
        throw new Error("sdnaOutput decorator can only be applied to methods");
    }

    descriptor.value = () => {
        let sdna = ""
        let subjectName = target.name
        let obj = new target

        sdna += `subject_class("${subjectName}", c).\n`

        let constructorActions = []
        if(obj.subjectConstructor && obj.subjectConstructor.length) {
            constructorActions = constructorActions.concat(obj.subjectConstructor)
        }

        let instanceConditions = []
        if(obj.isSubjectInstance && obj.isSubjectInstance.length) {
            instanceConditions = instanceConditions.concat(obj.isSubjectInstance)
        }

        let propertiesCode = []
        let properties = obj.__properties || {}
        for(let property in properties) {
            let propertyCode = `property(c, "${property}").\n`

            let { through, initial, required } = properties[property]
            
            if(through) {
                propertyCode += `property_getter(c, Base, "${property}", Value) :- triple(Base, "${through}", Value).\n`

                if(required) {
                    instanceConditions.push(`triple(Base, "${through}", _)`)
                }    

                let setter = obj[propertyNameToSetterName(property)]
                if(typeof setter === "function") {
                    let action = [{
                        action: "setSingleTarget",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]
                    propertyCode += `property_setter(c, "${property}", '${stringifyObjectLiteral(action)}').\n`
                }
            }

            propertiesCode.push(propertyCode)

            if(initial) {
                constructorActions.push({
                    action: "addLink",
                    source: "this",
                    predicate: through,
                    target: initial,
                })
            }
        }

        let collectionsCode = []
        let collections = obj.__collections || {}
        for(let collection in collections) {
            let collectionCode = `collection(c, "${collection}").\n`

            let { through } = collections[collection]

            if(through) {
                collectionCode += `collection_getter(c, Base, "${collection}", List) :- findall(C, triple(Base, "${through}", C), List).\n`
                let adder = obj[collectionToAdderName(collection)]
                if(typeof adder === "function") {
                    let action = [{
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]
                    collectionCode += `collection_adder(c, "${collection}", '${stringifyObjectLiteral(action)}').\n`
                }
            }

            collectionsCode.push(collectionCode)
        }

        let subjectContructorJSONString = stringifyObjectLiteral(constructorActions)
        sdna += `constructor(c, '${subjectContructorJSONString}').\n`
        let instanceConditionProlog = instanceConditions.join(", ")
        sdna += `instance(c, Base) :- ${instanceConditionProlog}.\n`
        sdna += "\n"
        sdna += propertiesCode.join("\n")
        sdna += "\n"
        sdna += collectionsCode.join("\n")

        return sdna
    }

    return descriptor
}