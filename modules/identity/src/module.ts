/**
 * Identity module definition.
 *
 * Uses `defineModule` from the WE module system. The module carries no
 * store of its own — identity state comes from the agent language
 * (the KEL), not from a local perspective. This keeps the hard boundary
 * intact: no perspective DB access.
 */

/** Configuration for the identity module. */
export interface IdentityModuleConfig {
  /**
   * Whether mnemonic backup can be deferred.
   *
   * When true, a persistent banner nags until backup completes, and the
   * pre-backup risk appears plainly: "Before backup, a lost password
   * means a lost account."
   *
   * Default: true (deferred allowed, with nag).
   */
  allowDeferredBackup?: boolean;

  /**
   * Default timelock for guardian recovery (seconds).
   * Default: 604800 (7 days).
   */
  recoveryTimelockSecs?: number;
}

/**
 * Build the identity module definition.
 *
 * Takes the UI component map from the host — the module never imports
 * Solid or the design system directly, preserving the single-instance
 * guarantee.
 *
 * @param components - Map of component names to their implementations
 * @param config - Module configuration
 */
export function createIdentityModule(
  components: Record<string, unknown>,
  config?: IdentityModuleConfig
) {
  // The defineModule import comes from the host's @we/module-shared.
  // Since this package declares it as a peerDependency, the host
  // provides the actual implementation.
  //
  // For now, return a plain object that matches the ModuleDefinition
  // shape. The defineModule wrapper gets added when the WE framework
  // integrates this module.
  return {
    id: "identity",
    name: "Identity",
    description:
      "Create, manage, and recover your did:scid identity — devices, assistants, guardians.",
    icon: "fingerprint",

    // Hard boundary: no backend datasets, no perspective access.
    // The module talks ONLY to the agent language.
    capabilities: [],

    // No owned entities — identity state lives in the KEL, not in
    // perspective links.
    frameworks: ["solid"],

    components,

    // Module-level config (accessible by screens via module context).
    config: {
      allowDeferredBackup: config?.allowDeferredBackup ?? true,
      recoveryTimelockSecs: config?.recoveryTimelockSecs ?? 604800,
    },
  };
}
