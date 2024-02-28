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

export interface InstanceQueryParams {
/**
 * An object representing the WHERE clause of the query.
 */
where?: object;

/**
 * A string representing the condition clause of the query.
 */
condition?: string;
}

/**
 * Decorator for querying instances of a subject class.
 * 
 * @category Decorators
 * 
 * @description
 * NOTE: Only works on methods that return a promise and will throw an error if not used on a method that returns a promise.
 * This will allow you to query for instances of a subject class with custom clauses within the instance clauses.
 * 
 * @example
 * // Usage with where clause
 * InstanceQuery({ where: { name: "John" }}) // this will return all the properties of the subject class with the name "John"
 * 
 * @example
 * // Usage with condition clause
 * InstanceQuery({ condition: "triple(Instance, 'age', Age), Age > 18" }) // this will return all the properties of the subject class with the age greater than 18
 * 
 * @param {Object} [options] - Query options.
 */
export function InstanceQuery(options?: InstanceQueryParams) {
    return function <T>(target: T, key: keyof T, descriptor: PropertyDescriptor) {
        const originalMethod = descriptor.value;
        if(typeof originalMethod !== "function") {
            throw new Error("InstanceQuery decorator can only be applied to methods");
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


export interface PropertyOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through?: string;

    /**
     * The initial value of the property. Required if the property is marked as required.
     */
    initial?: string;

    /**
     * Indicates whether the property is required. If true, an initial value must be provided.
     */
    required?: boolean;

    /**
     * Indicates whether the property is writable. If true, a setter will be available in the prolog engine.
     */
    writable?: boolean;

    /**
     * The language used to store the property. Can be the default `Literal` Language or a custom language address.
     */
    resolveLanguage?: string;

    /**
     * Custom getter to get the value of the property in the prolog engine. If not provided, the default getter will be used.
     */
    getter?: string;

    /**
     * Custom setter to set the value of the property in the prolog engine. Only available if the property is writable.
     */
    setter?: string;

    /**
     * Indicates whether the property is stored locally in the perspective and not in the network. Useful for properties that are not meant to be shared with the network.
     */
    local?: boolean;
}

/**
 * Decorator for defining properties of a subject class.
 * 
 * @category Decorators
 * 
 * @description
 * This will allow you to define properties with different conditions and how they would be defined in proflog engine.
 * 
 * - All properties must have a `through` option which is the predicate of the property.
 * -e If the property is required, it must have an `initial` option which is the initial value of the property.
 * - If the property is writable, it will have a setter in prolog engine. A custom setter can be defined with the `setter` option.
 * - If resolveLanguage is defined, you can use the default `Literal` Language or use your custom language address that can be used to store the property.
 * - If a custom getter is defined, it will be used to get the value of the property in prolog engine. If not, the default getter will be used.
 * - If local is defined, the property will be stored locally in the perspective and not in the network. This is useful for properties that are not meant to be shared with the network
 * 
 * @example
 * // Usage
 * SubjectProperty({ through: "ad4m://name", initial: "John", required: true }) // this will define a property with the name "ad4m://name" and the initial value "John"
 * 
 * @param {PropertyOptions} [opts] - Property options.
 */
export function SubjectProperty(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        if (opts.required && !opts.initial) {
            throw new Error("SubjectProperty requires an 'initial' option if 'required' is true");
        }

        if (!opts.through && !opts.getter) {
            throw new Error("SubjectProperty requires either 'through' or 'getter' option")
        }

        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = target["__properties"][key] || {};
        target["__properties"][key] = { ...target["__properties"][key], ...opts }

        if (opts.writable) {
            const value = key as string
            target[`set${capitalize(value)}`] = () => {}
        }

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

export interface FlagOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through: string;

    /**
     * The value of the property.
     */
    value: string;
}

/**
 * Decorator for defining flags of a subject class
 * 
 * @category Decorators
 * 
 * @description
 * The idea behind flag decorator is to define a property that is required and has an initial value. 
 * This will allow you to define a strict instance query. This behaviour can also be achieved with the `SubjectProperty` decorator but the `SubjectFlag` decorator is a shorthand for that.
 * 
 * NOTE: Use of Flag is discouraged and should be used only when necessary.
 * 
 * - All properties must have a `through` & `initial` option which is the predicate of the property.
 * 
 * @example
 * // Usage
 * SubjectFlag({ through: "ad4m://name", value: "John" }) // this will define a flag with the name "ad4m://name" and the initial value "John"   
 * 
 * @param {FlagOptions} [opts] Flag options.
 */
export function SubjectFlag(opts: FlagOptions) {
    return function <T>(target: T, key: keyof T) {
        if (!opts.through && !opts.value) {
            throw new Error("SubjectFlag requires a 'through' and 'value' option")
        }

        if (!opts.through) {
            throw new Error("SubjectFlag requires a 'through' option")
        }

        if (!opts.value) {
            throw new Error("SubjectFlag requires a 'value' option")
        }

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

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

interface WhereOptions {
    isInstance?: any
    condition?: string
}

export interface CollectionOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through: string;

    /**
     * An object representing the WHERE clause of the query.
     */
    where?: WhereOptions;

    /**
     * Indicates whether the property is stored locally in the perspective and not in the network. Useful for properties that are not meant to be shared with the network.
     */
    local?: boolean;
}

/**
 * Decorator for defining collections of a subject class.
 * 
 * @category Decorators
 * 
 * @description
 * This will allow you to define collections with different conditions and how they would be defined in proflog engine.
 * 
 * NOTE: The property needs to be an array for it to picked up during the initialization phase.
 * 
 * - All collections must have a `through` option which is the predicate of the collection.
 * - If the collection has a `where` option, it can be used to define a custom condition for the collection.
 * - If the collection has a `local` option, the collection will be stored locally in the perspective and not in the network. This is useful for collections that are not meant to be shared with the network.
 * 
 * @example
 * // Usage
 * SubjectCollection({ through: "ad4m://friends" }) // this will define a collection with the name "ad4m://friends"
 * 
 * @param opts Collection options.
 */
export function SubjectCollection(opts: CollectionOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__collections"] = target["__collections"] || {};
        target["__collections"][key] = opts;

        const value = key as string
        target[`add${capitalize(value)}`] = () => {}
        target[`remove${capitalize(value)}`] = () => {}
        target[`setCollection${capitalize(value)}`] = () => {}

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

export function makeRandomPrologAtom(length: number): string {
    let result = '';
    let characters = 'abcdefghijklmnopqrstuvwxyz';
    let charactersLength = characters.length;
    for (let i = 0; i < length; i++) {
       result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
 }

export interface SDNAClassOptions {
    /**
     * The name of the entity.
     */
    name: string;
}

/**
 * Decorator for defining an SDNA class.
 * 
 * @category Decorators
 * 
 * @description
 * This will create a new SDNA class with the given name and add custom methods to generate the SDNA for the class, for this to work the class need to have the properties defined using the decorators like `SubjectProperty`.
 * 
 * Note: This decorator is required for the class to be picked up during the initialization phase.
 * 
 * @example
 * // Usage
 *  SDNAClass({ name: "Person" }) // this will create a new SDNA class with the name "Person"
 * 
 * @param opts SDNA class options.
 */
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


            let classRemoverActions = []

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

                let { through, initial, required, resolveLanguage, writable, flag, getter, setter, local } = properties[property]

                if(resolveLanguage) {
                    propertyCode += `property_resolve(${uuid}, "${property}").\n`
                    propertyCode += `property_resolve_language(${uuid}, "${property}", "${resolveLanguage}").\n`
                }

                if(getter) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- ${getter}.\n`
                } else if(through) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- triple(Base, "${through}", Value).\n`

                    if(required) {
                        if(flag) {
                            instanceConditions.push(`triple(Base, "${through}", "${initial}")`)
                        } else {
                            instanceConditions.push(`triple(Base, "${through}", _)`)
                        }
                    }
                }

                if(setter) {
                    propertyCode += `property_setter(${uuid}, "${property}", Actions) :- ${setter}.\n`
                } else if (writable) {
                    let setter = obj[propertyNameToSetterName(property)]
                    if(typeof setter === "function") {
                        let action = [{
                            action: "setSingleTarget",
                            source: "this",
                            predicate: through,
                            target: "value",
                            ...(local && { local: true })
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

                    classRemoverActions.push({
                        action: "removeLink",
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

                let { through, where, local} = collections[collection]

                if(through) {
                    if(where) {
                        if(!where.isInstance && !where.condition) {
                            throw "'where' needs one of 'isInstance' or 'condition'"
                        }

                        let conditions = []

                        if(where.isInstance) {
                            let otherClass
                            if(where.isInstance.name) {
                                otherClass = where.isInstance.name
                            } else {
                                otherClass = where.isInstance
                            }
                            conditions.push(`instance(OtherClass, Target), subject_class("${otherClass}", OtherClass)`)
                        }

                        if(where.condition) {
                            conditions.push(where.condition)
                        }

                        const conditionString = conditions.join(", ")

                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- setof(Target, (triple(Base, "${through}", Target), ${conditionString}), List).\n`
                    } else {
                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- findall(C, triple(Base, "${through}", C), List).\n`
                    }

                    let collectionAdderAction = [{
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                        ...(local && { local: true })
                    }]

                    let collectionRemoverAction = [{
                        action: "removeLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]

                    let collectionSetterAction = [{
                        action: "collectionSetter",
                        source: "this",
                        predicate: through,
                        target: "value",
                        ...(local && { local: true })
                    }]
                    collectionCode += `collection_adder(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionAdderAction)}').\n`
                    collectionCode += `collection_remover(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionRemoverAction)}').\n`
                    collectionCode += `collection_setter(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionSetterAction)}').\n`
                }

                collectionsCode.push(collectionCode)
            }

            let subjectContructorJSONString = stringifyObjectLiteral(constructorActions)
            sdna += `constructor(${uuid}, '${subjectContructorJSONString}').\n`
            if(instanceConditions.length > 0) {
                let instanceConditionProlog = instanceConditions.join(", ")
                sdna += `instance(${uuid}, Base) :- ${instanceConditionProlog}.\n`
                sdna += "\n"
            }
            sdna += `destructor(${uuid}, '${stringifyObjectLiteral(classRemoverActions)}').\n`
            sdna += "\n"
            sdna += propertiesCode.join("\n")
            sdna += "\n"
            sdna += collectionsCode.join("\n")

            return {
                sdna,
                name: subjectName
            }
        }

        Object.defineProperty(target, 'type', {configurable: true});
    }
}