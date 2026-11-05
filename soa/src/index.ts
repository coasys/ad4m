export { default as StateOfAffair } from './StateOfAffair';
export type { SoAModality } from './StateOfAffair';

/**
 * SoA Relationship Predicates
 * 
 * Relationships between StateOfAffair nodes are expressed as
 * standard AD4M links using these soa language expressions
 * as predicates. No separate Relationship model needed —
 * the link expression itself carries author and timestamp.
 * 
 * Usage:
 *   perspective.addLink({ source: soaA, predicate: SoA.REL_SUPPORTS, target: soaB })
 */
export const SoA = {
  /** Evidence or argument for */
  REL_SUPPORTS: 'soa://rel_supports',
  /** Evidence or argument against */
  REL_CONTRADICTS: 'soa://rel_contradicts',
  /** Related but not identical */
  REL_SIMILAR: 'soa://rel_similar',
  /** Equivalent propositions */
  REL_SAME: 'soa://rel_same',
  /** Dependency: target requires source */
  REL_REQUIRES: 'soa://rel_requires',
  /** Capability: source enables target */
  REL_ENABLES: 'soa://rel_enables',
  /** Tree structure: source is parent of target */
  REL_PARENT: 'soa://rel_parent',
  /** Source is a more specific version of target */
  REL_REFINES: 'soa://rel_refines',
  /** Source prevents/blocks target */
  REL_BLOCKS: 'soa://rel_blocks',
} as const;

export type SoARelationshipType = typeof SoA[keyof typeof SoA];
