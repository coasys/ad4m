/**
 * WE Identity Module — PR9.
 *
 * The identity plane's face. Deployable inside WE now and standalone
 * later — the hard boundary (agent language only, no perspective DB)
 * preserves the later split.
 *
 * ## Hard boundary (non-negotiable)
 *
 * This module talks ONLY to the agent language. It never touches the
 * executor's perspective DB. Key generation happens on-device. The
 * password never leaves the device. Mnemonic entry occurs only in this
 * module's own origin.
 *
 * ## Three ceremonies
 *
 * C1 — Create: name + password + mnemonic (or defer) → working identity.
 * C2 — Connect: QR / mnemonic / hosted → new device enrols.
 * C3 — Revoke: revoke + propagation proof + lost-everything recovery.
 */

export { createIdentityModule } from "./module";
export { IDENTITY_SCREENS } from "./screens";
export type { IdentityModuleConfig } from "./module";
