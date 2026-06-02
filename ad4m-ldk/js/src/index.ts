/**
 * @coasys/ad4m-ldk — AD4M Language Development Kit (JS)
 *
 * Authoring layer for flat-export AD4M Languages. Wraps the runtime
 * imports the AD4M executor installs on globalThis and provides
 * `defineLanguage()` as a grouped → flat transform.
 *
 * Usage:
 * ```ts
 * import { defineLanguage, agentDid, holochainCall } from "@coasys/ad4m-ldk";
 *
 * const lang = defineLanguage({
 *     name: "@coasys/my-language",
 *     version: "1.0.0",
 *     isPublic: false,
 *     async init() { ... },
 *     commit: { async commit(diff) { ... } },
 *     sync:   { async sync() { ... }, async render() { ... }, currentRevision() { ... } },
 *     peers:  { setLocal(agents) { ... }, async remote() { ... } },
 * });
 *
 * export default lang;
 * export const { name, version, isPublic, init, perspectiveCommit,
 *     perspectiveSyncSync, perspectiveSyncRender, perspectiveSyncCurrentRevision,
 *     peersSetLocal, peersRemote } = lang;
 * ```
 */

export * from "./types.js";
export * from "./errors.js";
export * from "./imports.js";
export * from "./defineLanguage.js";
