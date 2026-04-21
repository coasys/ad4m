/**
 * Ad4mModel — barrel re-exports.
 *
 * All public symbols from the model subsystem are re-exported here so that
 * consumers can import from `"./model"` (or `"@coasys/ad4m"` via the
 * top-level barrel) without knowing the internal file layout.
 */

export * from "./types";
export * from "./Ad4mModel";
export * from "./decorators";
export * from "./Subject";
export * from "./query-common";
export * from "./query-prolog";
export * from "./prolog-facts";
export * from "./json-schema";
export * from "./query-utils";
export * from "./query-sparql";
export * from "./hydration";
export * from "./ModelQueryBuilder";
export * from "./sdna";
export * from "./shacl-gen";
export * from "./query-cache";
export * from "./subscription-pool";
