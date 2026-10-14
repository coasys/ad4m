import type { ImplementsInput } from "../generated/api/ImplementsInput";

/**
 * What a model query reads (`PerspectiveProxy.modelQuery` / `modelSubscribe`):
 *
 * - a class name, or several (one union query, #1238);
 * - `{ interface: 'Name' }`: every installed class that implements a named
 *   interface, registered with {@link PerspectiveProxy.addInterface} (#1435);
 * - `{ implements: { properties, relations } }`: the same for an interface
 *   given inline.
 *
 * An interface query is a union query over the classes that implement it:
 * every row carries `__subjectClass` and `__subjectClasses`, and `where`,
 * `order`, paging and `include` behave as they do for a list of class names.
 * Like any union it refuses a parent scope with `limitPerAnchor` / `levels`
 * and dotted (relation) order keys. No implementing class is an empty
 * result, not an error. A subscription over an interface picks up classes
 * installed after it was opened.
 *
 * A class implements an interface when its shape has:
 * - a property of each name asked for (`'text'`), stored under any predicate;
 *   `{ name, through }` also pins the predicate and `{ name, datatype }` the
 *   datatype. A getter-backed property counts by its name;
 * - a relation through each predicate asked for (`{ through }`), in the given
 *   `direction` (`'forward'`, the default, or `'reverse'`). Relation names do
 *   not matter.
 *
 * @example
 * // Anything with text, whatever the app calls its classes or predicates:
 * await perspective.modelQuery({ implements: { properties: ['text'] } }, JSON.stringify({ where: { id: base } }))
 */
export type ModelQueryClasses =
    | string
    | string[]
    | { interface: string }
    | { implements: ImplementsInput };

/** A named interface's definition: the same shape as an inline `implements`. */
export type InterfaceDefinition = ImplementsInput;

/** The wire form of a model query's classes (`PerspectiveModelQueryParams`). */
export function modelClassParams(
    classes: ModelQueryClasses,
): { class_name: string } | { class_names: string[] } | { interface: string } | { implements: ImplementsInput } {
    if (typeof classes === "string") return { class_name: classes };
    if (Array.isArray(classes)) return { class_names: classes };
    if ("interface" in classes) return { interface: classes.interface };
    return { implements: classes.implements };
}

function localName(uri: string): string {
    const parts = uri.split(/[/#:]/).filter((p) => p.length > 0);
    return parts[parts.length - 1] ?? uri;
}

/**
 * The SHACL JSON the executor's writer turns into a named interface: a node
 * shape typed `ad4m://Interface`, with no constructor, flags or actions. A
 * property without `through` is written without a path, which makes it match
 * by name alone.
 */
export function interfaceShaclJson(
    name: string,
    definition: InterfaceDefinition,
    namespace = "ad4m://interface/",
): string {
    const properties = (definition.properties ?? []).map((p) =>
        typeof p === "string"
            ? { path: "", name: p }
            : { path: p.through ?? "", name: p.name, ...(p.datatype && { datatype: p.datatype }) },
    );
    const relations = (definition.relations ?? []).map((r) => ({
        path: r.through,
        name: r.name ?? localName(r.through),
        relation_kind: r.direction === "reverse" ? "belongsToMany" : "hasMany",
    }));
    return JSON.stringify({
        target_class: `${namespace}${name}`,
        properties: [...properties, ...relations],
    });
}
