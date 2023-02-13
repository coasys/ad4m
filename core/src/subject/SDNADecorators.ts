import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { Subject } from "./Subject";
import { capitalize, propertyNameToSetterName, singularToPlural, stringifyObjectLiteral } from "./util";

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
    where?: object;
    condition?: string;
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
            let query = `subject_class("${subjectClassName}", C), instance(C, Instance)`
            if(options && options.where) {
                for(let prop in options.where) {
                    let value = options.where[prop]
                    query += `, property_getter(C, Instance, "${prop}", "${value}")`
                }
            }

            if(options && options.condition) {
                query += ', ' + options.condition
            }

            let results = await perspective.infer(query)
            if(results == false) {
                return instances
            }
            if(typeof results == "string") {
                throw results
            }
            for(let result of results) {
                let instance = result.Instance
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
    writable?: boolean,
    resolveLanguage?: string;
}
export function subjectProperty(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = target["__properties"][key] || {};
        target["__properties"][key] = { ...target["__properties"][key], ...opts }

        if (opts.writable) {
            const value = key as string
            target[`set${capitalize(value)}`] = () => {}
        }

        Object.defineProperty(target, key, {});
    };
}

interface FlagOptions {
    through: string;
    value: string;
}
export function subjectFlag(opts: FlagOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = target["__properties"][key] || {};
        target["__properties"][key] = { 
            ...target["__properties"][key],
            through: opts.through,
            required: true,
            initial: opts.value,
            flag: true
        }

        // @ts-ignore
        target[key] = opts.value;

        Object.defineProperty(target, key, {configurable: true});
    };
}

interface WhereOptions {
    isInstance: any
}
interface CollectionOptions {
    through: string,
    where?: WhereOptions,
}

export function subjectCollection(opts: CollectionOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__collections"] = target["__collections"] || {};
        target["__collections"][key] = opts;
        
        const value = key as string
        target[`add${capitalize(value)}`] = () => {}
        target[`setCollection${capitalize(value)}`] = () => {}

        Object.defineProperty(target, key, {configurable: true});
    };
}

function makeRandomPrologAtom(length: number): string {
    let result = '';
    let characters = 'abcdefghijklmnopqrstuvwxyz';
    let charactersLength = characters.length;
    for (let i = 0; i < length; i++) {
       result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
 }

 interface SDNAClassOptions {
    name: string;
}

export function SDNAClass(opts: SDNAClassOptions) {
    return function (target: any) {
        target.prototype.className = opts.name;
        target.className = opts.name;
        
        target.generateSDNA = function() {
            let sdna = ""
            let subjectName = opts.name
            let obj = target.prototype;
    
            let uuid = makeRandomPrologAtom(8)
    
            sdna += `subject_class("${subjectName}", ${uuid}).\n`
    
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
                let propertyCode = `property(${uuid}, "${property}").\n`
    
                let { through, initial, required, resolveLanguage, writable, flag } = properties[property]
    
                if(resolveLanguage) {
                    propertyCode += `property_resolve(${uuid}, "${property}").\n`
                    propertyCode += `property_resolve_language(${uuid}, "${property}", "${resolveLanguage}").\n`
                }
                
                if(through) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- triple(Base, "${through}", Value).\n`
    
                    if(required) {
                        if(flag) {
                            instanceConditions.push(`triple(Base, "${through}", "${initial}")`)
                        } else {
                            instanceConditions.push(`triple(Base, "${through}", _)`)    
                        }
                    }    
                }
                
                if (writable) {
                    let setter = obj[propertyNameToSetterName(property)]
                    if(typeof setter === "function") {
                        let action = [{
                            action: "setSingleTarget",
                            source: "this",
                            predicate: through,
                            target: "value",
                        }]
                        propertyCode += `property_setter(${uuid}, "${property}", '${stringifyObjectLiteral(action)}').\n`
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
                let collectionCode = `collection(${uuid}, "${collection}").\n`
    
                let { through, where } = collections[collection]
    
                if(through) {
                    if(where) {
                        if(!where.isInstance) {
                            throw "'where' currently only supports 'isInstance'"
                        }
                        let otherClass
                        if(where.isInstance.name) {
                            otherClass = where.isInstance.name
                        } else {
                            otherClass = where.isInstance
                        }
                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- setof(C, (triple(Base, "${through}", C), instance(OtherClass, C), subject_class("${otherClass}", OtherClass)), List).\n`    
                    } else {
                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- findall(C, triple(Base, "${through}", C), List).\n`
                    }
                    
                    let action = [{
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]
                    
                    let collectionSetterAction = [{
                        action: "collectionSetter",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]
                    collectionCode += `collection_adder(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(action)}').\n`
                    collectionCode += `collection_setter(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionSetterAction)}').\n`
                }
    
                collectionsCode.push(collectionCode)
            }
    
            let subjectContructorJSONString = stringifyObjectLiteral(constructorActions)
            sdna += `constructor(${uuid}, '${subjectContructorJSONString}').\n`
            let instanceConditionProlog = instanceConditions.join(", ")
            sdna += `instance(${uuid}, Base) :- ${instanceConditionProlog}.\n`
            sdna += "\n"
            sdna += propertiesCode.join("\n")
            sdna += "\n"
            sdna += collectionsCode.join("\n")

            return sdna
        }

        Object.defineProperty(target, 'type', {configurable: true});
    }
}