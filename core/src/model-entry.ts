/**
 * `@coasys/ad4m/model` — the model/SHACL surface of the SDK without the
 * client.
 *
 * Everything exported here is free of `Ad4mClient`, `PerspectiveProxy` and
 * the WS-RPC machinery, so a Language bundle can import the decorators and
 * `generateSHACL()` without pulling the client runtime into its Deno
 * isolate. That is what `mount.classes` in `@coasys/ad4m-ldk`'s
 * `defineLanguage` is for: a mountable Language describes the classes it
 * exposes with the same `@Model`/`@Property`/`@HasMany` decorators app
 * developers use, and the LDK reads each class's `generateSHACL()`.
 *
 * Classes declared for a mount do not need to extend `Ad4mModel` — the
 * `@Model` decorator attaches `generateSHACL()` to the constructor and the
 * shape is all a mount needs (`Ad4mModel` itself stays on the main entry;
 * it needs `PerspectiveProxy` for `save()`/`query()`).
 *
 * Named `model-entry.ts`, not `model.ts`: the root barrel's
 * `export * from "./model"` must keep resolving to the `model/` directory
 * (Ad4mModel, the decorators, hydration, query helpers) — a `model.ts`
 * file next to it shadows the directory and takes the root entry's model
 * exports with it.
 */

export * from "./model/types";
export * from "./model/decorators";
export * from "./model/sdna";
export * from "./model/shacl-gen";
export * from "./shacl/SHACLShape";
export * from "./shacl/NodeExpression";
export * from "./shacl/index";
export * from "./Literal";
export * from "./links/Links";

export type { ShapeJson } from "./generated/api/ShapeJson";
export type { PropertyShapeJson } from "./generated/api/PropertyShapeJson";
export type { ShapeAction } from "./generated/api/ShapeAction";
export type { ShapeConformanceCondition } from "./generated/api/ShapeConformanceCondition";
