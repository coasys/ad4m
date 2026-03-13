/**
 * SHACL shape generation for AD4M model classes.
 *
 * Builds a W3C SHACL `SHACLShape` (with AD4M-specific action extensions)
 * from a model's property and relation metadata.  The heavy lifting used
 * to live inside the `@Model` decorator closure — it is extracted here so
 * decorators.ts stays focused on decorator registration logic.
 */
import { SHACLShape } from "../shacl/SHACLShape";
import type { SHACLPropertyShape, ConformanceCondition } from "../shacl/SHACLShape";
import { escapeSurrealString } from "../utils";
import { compileWhereClause } from "./surreal-utils";
import { propertyNameToSetterName } from "./util";
import type { PropertyMetadataEntry, RelationMetadataEntry, Ad4mModelLike } from "./decorators";

/** Signature for the conformance-filter builder injected by the caller. */
export type ConformanceFilterFn = (
    relationPredicate: string,
    targetClass: Ad4mModelLike,
) => { getter: string; conformanceConditions: ConformanceCondition[] } | undefined;

/**
 * Build a SHACL shape for a model class.
 *
 * @param subjectName          - The model's registered name (from `@Model({ name })`)
 * @param target               - The class constructor (used for prototype access,
 *                               inheritance chain, and relation target resolution)
 * @param properties           - Merged property metadata (from `getPropertiesMetadata`)
 * @param allRelationsMeta     - Merged relation metadata (from `getRelationsMetadata`)
 * @param conformanceFilterFn  - Injected conformance-filter builder
 *                               (avoids circular dependency back into decorators.ts)
 */
export function buildSHACL(
    subjectName: string,
    target: any,
    properties: Record<string, PropertyMetadataEntry>,
    allRelationsMeta: Record<string, RelationMetadataEntry>,
    conformanceFilterFn: ConformanceFilterFn,
): { shape: SHACLShape; name: string } {
    const obj = target.prototype;

    // ── Determine namespace from first property or relation ─────────────
    let namespace = "ad4m://";
    const relations = Object.fromEntries(
        Object.entries(allRelationsMeta).filter(([, r]) => r.kind === 'hasMany' || r.kind === 'belongsToMany')
    );

    // Try properties first
    if (Object.keys(properties).length > 0) {
        const firstProp = properties[Object.keys(properties)[0]];
        if (firstProp.through) {
            const match = firstProp.through.match(/^([^:]+:\/\/)/);
            if (match) {
                namespace = match[1];
            }
        }
    }
    // Fall back to relations if no properties
    else if (Object.keys(relations).length > 0) {
        const firstRel = relations[Object.keys(relations)[0]];
        if (firstRel.predicate) {
            const match = firstRel.predicate.match(/^([^:]+:\/\/)/);
            if (match) {
                namespace = match[1];
            }
        }
    }

    // ── Create SHACL shape ──────────────────────────────────────────────
    const shapeUri = `${namespace}${subjectName}Shape`;
    const targetClass = `${namespace}${subjectName}`;
    const shape = new SHACLShape(shapeUri, targetClass);

    // Detect @Model inheritance — if the parent class also has
    // generateSHACL it is itself a @Model and we reference its shape
    // via sh:node so SHACL validators can walk the hierarchy.
    const parentCtor = Object.getPrototypeOf(target);
    if (parentCtor && typeof parentCtor.generateSHACL === 'function') {
        const parentSHACL = parentCtor.generateSHACL();
        if (parentSHACL?.shape?.nodeShapeUri) {
            shape.addParentShape(parentSHACL.shape.nodeShapeUri);
        }
    }

    // ── Constructor / Destructor actions ────────────────────────────────
    let constructorActions: any[] = [];
    if (obj.subjectConstructor && obj.subjectConstructor.length) {
        constructorActions = constructorActions.concat(obj.subjectConstructor);
    }

    let destructorActions: any[] = [];

    // ── Convert properties to SHACL property shapes ────────────────────
    for (const propName in properties) {
        const propMeta = properties[propName];

        if (!propMeta.through) continue; // Skip properties without predicates

        const propShape: SHACLPropertyShape = {
            name: propName,
            path: propMeta.through,
        };

        // Determine datatype from initial value or resolveLanguage
        if (propMeta.resolveLanguage === "literal") {
            propShape.datatype = "xsd://string";
        } else if (propMeta.initial) {
            const initialType = typeof obj[propName];
            if (initialType === "number") {
                propShape.datatype = "xsd://integer";
            } else if (initialType === "boolean") {
                propShape.datatype = "xsd://boolean";
            } else if (initialType === "string") {
                propShape.datatype = "xsd://string";
            }
        }

        // Cardinality constraints
        if (propMeta.required) {
            propShape.minCount = 1;
        }

        // Single-valued properties get maxCount 1
        propShape.maxCount = 1;

        // Flag properties have fixed value
        if (propMeta.flag && propMeta.initial) {
            propShape.hasValue = propMeta.initial;
        }

        // AD4M-specific metadata
        if (propMeta.local !== undefined) {
            propShape.local = propMeta.local;
        }

        if (propMeta.writable !== undefined) {
            propShape.writable = propMeta.writable;
        }

        if (propMeta.resolveLanguage) {
            propShape.resolveLanguage = propMeta.resolveLanguage;
        }

        // ── Setter actions ──────────────────────────────────────────────
        if (propMeta.prologSetter) {
            console.warn(
                `[SHACL Generation] Custom Prolog setter for property '${propName}' in class '${subjectName}' is not yet supported. ` +
                `The property will be created without setter actions. Consider using standard writable properties or provide explicit SHACL JSON.`
            );
        } else if (propMeta.writable && propMeta.through) {
            let setter = obj[propertyNameToSetterName(propName)];
            if (typeof setter === "function") {
                propShape.setter = [{
                    action: "setSingleTarget",
                    source: "this",
                    predicate: propMeta.through,
                    target: "value",
                    ...(propMeta.local && { local: true })
                }];
            }
        }

        // ── Constructor / Destructor entries ────────────────────────────
        const effectiveInitial = propMeta.initial
            ?? (propMeta.required && propMeta.writable && !propMeta.flag && propMeta.through
                ? "literal://string:" : undefined);

        if (effectiveInitial) {
            constructorActions.push({
                action: "addLink",
                source: "this",
                predicate: propMeta.through,
                target: effectiveInitial,
            });

            destructorActions.push({
                action: "removeLink",
                source: "this",
                predicate: propMeta.through,
                target: "*",
            });
        }

        shape.addProperty(propShape);
    }

    // ── Convert relations to SHACL property shapes ─────────────────────
    for (const relName in relations) {
        const relMeta = relations[relName];

        if (!relMeta.predicate) continue;

        const relShape: SHACLPropertyShape = {
            name: relName,
            path: relMeta.predicate,
        };

        // Relations typically contain IRIs
        relShape.nodeKind = 'IRI';

        // AD4M-specific metadata
        if (relMeta.local !== undefined) {
            relShape.local = relMeta.local;
        }

        // Adder action
        relShape.adder = [{
            action: "addLink",
            source: "this",
            predicate: relMeta.predicate,
            target: "value",
            ...(relMeta.local && { local: true })
        }];

        // Remover action
        relShape.remover = [{
            action: "removeLink",
            source: "this",
            predicate: relMeta.predicate,
            target: "value",
            ...(relMeta.local && { local: true })
        }];

        // ── Build Getter (conformance filter) ───────────────────────────
        // Priority chain:
        // 1. Explicit getter string → use verbatim
        // 2. `where` clause → compile DSL to SurrealQL getter
        // 3. target + filter !== false → auto-derive from shape
        if (relMeta.getter) {
            relShape.getter = relMeta.getter;
        } else if (relMeta.where) {
            try {
                const TargetClass = relMeta.target?.();
                const targetMetadata = TargetClass
                    ? (TargetClass as any).getModelMetadata?.() ?? null
                    : null;

                const conditions = compileWhereClause(
                    relMeta.where,
                    targetMetadata,
                );

                if (conditions.length > 0) {
                    const escapedPredicate = escapeSurrealString(relMeta.predicate);
                    relShape.getter = `(->link[WHERE predicate = '${escapedPredicate}'].out[WHERE ${conditions.join(' AND ')}].uri)`;
                }
            } catch (e) {
                // Target metadata may not be available yet
            }
        } else if (relMeta.target && relMeta.filter !== false) {
            try {
                const TargetClass = relMeta.target();
                const filter = conformanceFilterFn(relMeta.predicate, TargetClass);
                if (filter) {
                    relShape.getter = filter.getter;
                    relShape.conformanceConditions = filter.conformanceConditions;
                }
            } catch (e) {
                // Target class may not be available yet — getter will be
                // absent and runtime will fall back to unfiltered get_links
            }
        }

        // ── sh:class — target shape reference ───────────────────────────
        if (relMeta.target) {
            try {
                const TargetClass = relMeta.target();
                const targetSHACL = (TargetClass as any).generateSHACL?.();
                if (targetSHACL?.shape?.nodeShapeUri) {
                    relShape.class = targetSHACL.shape.nodeShapeUri;
                }
            } catch (e) {
                // Target class may not be available yet
            }
        }

        shape.addProperty(relShape);
    }

    // Always set constructor and destructor actions on the shape, even
    // when empty.  An empty array serialises to `literal://string:[]`
    // which the Rust executor parses as a valid (no-op) command list,
    // avoiding "No SHACL constructor found" errors for models whose
    // properties are all optional and have no @Flag.
    shape.setConstructorActions(constructorActions);
    shape.setDestructorActions(destructorActions);

    return {
        shape,
        name: subjectName
    };
}
