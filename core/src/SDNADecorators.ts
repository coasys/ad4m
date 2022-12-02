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
    return `triple(this, ${predicate}, _)`
}

interface PropertyOptions {
    through: string;
    initial?: string,
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
        let subjectName = target.constructor.name

        sdna += `subject_class("${subjectName}", c).\n`
        sdna += `constructor(c, '${target.prototype.subjectConstructor}').`
        let instanceCondition = target.prototype.isSubjectInstance.join(", ")
        sdna += `instance :- ${instanceCondition}.\n`


    }

    return descriptor
}