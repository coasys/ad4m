/**
 * SDNA (Social DNA) Prolog code generation.
 *
 * Builds subject-class Prolog facts from a model's property and relation
 * metadata.  The heavy lifting used to live inside the `@Model` decorator
 * closure — it is extracted here so decorators.ts stays focused on
 * decorator registration logic.
 */
import { makeRandomId, propertyNameToSetterName, stringifyObjectLiteral } from "./util";
import type { PropertyMetadataEntry, RelationMetadataEntry } from "./decorators";

/**
 * Build SDNA (Social DNA) Prolog code for a model class.
 *
 * @param subjectName  - The model's registered name (from `@Model({ name })`)
 * @param obj          - `target.prototype` — used to inspect prototype methods
 *                       (e.g. setter stubs, `subjectConstructor`, `isSubjectInstance`)
 * @param properties   - Merged property metadata (from `getPropertiesMetadata`)
 * @param allRelationsMeta - Merged relation metadata (from `getRelationsMetadata`)
 */
export function buildSDNA(
    subjectName: string,
    obj: any,
    properties: Record<string, PropertyMetadataEntry>,
    allRelationsMeta: Record<string, RelationMetadataEntry>,
): { sdna: string; name: string } {
    let sdna = "";
    let uuid = makeRandomId(8);

    sdna += `subject_class("${subjectName}", ${uuid}).\n`;

    let classRemoverActions: any[] = [];

    let constructorActions: any[] = [];
    if (obj.subjectConstructor && obj.subjectConstructor.length) {
        constructorActions = constructorActions.concat(obj.subjectConstructor);
    }

    let instanceConditions: string[] = [];
    if (obj.isSubjectInstance && obj.isSubjectInstance.length) {
        instanceConditions = instanceConditions.concat(obj.isSubjectInstance);
    }

    let propertiesCode: string[] = [];
    for (let property in properties) {
        let propertyCode = `property(${uuid}, "${property}").\n`;

        let { through, initial, required, resolveLanguage, writable, flag, prologGetter, prologSetter, local } = properties[property];

        if (resolveLanguage) {
            propertyCode += `property_resolve(${uuid}, "${property}").\n`;
            propertyCode += `property_resolve_language(${uuid}, "${property}", "${resolveLanguage}").\n`;
        }

        if (prologGetter) {
            propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- ${prologGetter}.\n`;
        } else if (through) {
            propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- triple(Base, "${through}", Value).\n`;

            if (required) {
                if (flag) {
                    instanceConditions.push(`triple(Base, "${through}", "${initial}")`);
                } else {
                    instanceConditions.push(`triple(Base, "${through}", _)`);
                }
            }
        }

        if (prologSetter) {
            propertyCode += `property_setter(${uuid}, "${property}", Actions) :- ${prologSetter}.\n`;
        } else if (writable && through) {
            let setter = obj[propertyNameToSetterName(property)];
            if (typeof setter === "function") {
                let action = [{
                    action: "setSingleTarget",
                    source: "this",
                    predicate: through,
                    target: "value",
                    ...(local && { local: true })
                }];
                propertyCode += `property_setter(${uuid}, "${property}", '${stringifyObjectLiteral(action)}').\n`;
            }
        }

        propertiesCode.push(propertyCode);

        // Auto-derive effectiveInitial for required, writable, non-flag properties
        // so constructor/destructor always handle the property link.
        // Optional properties (required: false) are excluded — they should
        // remain unset (undefined) until explicitly assigned.
        const effectiveInitial = initial
            ?? (required && writable && !flag && through ? "literal://string:" : undefined);

        if (effectiveInitial) {
            constructorActions.push({
                action: "addLink",
                source: "this",
                predicate: through,
                target: effectiveInitial,
            });

            classRemoverActions.push({
                action: "removeLink",
                source: "this",
                predicate: through,
                target: "*",
            });
        }
    }

    let relationsCode: string[] = [];
    const relations = Object.fromEntries(
        Object.entries(allRelationsMeta).filter(([, r]) => r.kind === 'hasMany' || r.kind === 'belongsToMany')
    );
    for (let relation in relations) {
        let relationCode = `collection(${uuid}, "${relation}").\n`;

        let { predicate: through, local } = relations[relation];

        if (through) {
            relationCode += `collection_getter(${uuid}, Base, "${relation}", List) :- findall(C, triple(Base, "${through}", C), List).\n`;

            let relationAdderAction = [{
                action: "addLink",
                source: "this",
                predicate: through,
                target: "value",
                ...(local && { local: true })
            }];

            let relationRemoverAction = [{
                action: "removeLink",
                source: "this",
                predicate: through,
                target: "value",
            }];

            let relationSetterAction = [{
                action: "collectionSetter",
                source: "this",
                predicate: through,
                target: "value",
                ...(local && { local: true })
            }];
            relationCode += `collection_adder(${uuid}, "${relation}", '${stringifyObjectLiteral(relationAdderAction)}').\n`;
            relationCode += `collection_remover(${uuid}, "${relation}", '${stringifyObjectLiteral(relationRemoverAction)}').\n`;
            relationCode += `collection_setter(${uuid}, "${relation}", '${stringifyObjectLiteral(relationSetterAction)}').\n`;
        }

        relationsCode.push(relationCode);
    }

    let subjectContructorJSONString = stringifyObjectLiteral(constructorActions);
    sdna += `constructor(${uuid}, '${subjectContructorJSONString}').\n`;
    if (instanceConditions.length > 0) {
        let instanceConditionProlog = instanceConditions.join(", ");
        sdna += `instance(${uuid}, Base) :- ${instanceConditionProlog}.\n`;
        sdna += "\n";
    }
    sdna += `destructor(${uuid}, '${stringifyObjectLiteral(classRemoverActions)}').\n`;
    sdna += "\n";
    sdna += propertiesCode.join("\n");
    sdna += "\n";
    sdna += relationsCode.join("\n");

    return {
        sdna,
        name: subjectName
    };
}
