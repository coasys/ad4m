import { ModelOptions, Ad4mModel, Property, Flag } from '@coasys/ad4m';

/**
 * Types of relationships between States of Affairs.
 * 
 * Epistemic:
 * - supports: evidence or argument for
 * - contradicts: evidence or argument against
 * 
 * Equivalence:
 * - similar: related but not identical
 * - same: equivalent propositions
 * 
 * Structural:
 * - requires: dependency (target requires source)
 * - enables: capability (source enables target)
 * - parent: source is parent of target (tree structure)
 * - refines: source is a more specific version of target
 * - blocks: source prevents target
 */
export type SoARelationshipType = 
  | 'supports' 
  | 'contradicts' 
  | 'similar' 
  | 'same' 
  | 'requires' 
  | 'enables' 
  | 'parent'
  | 'refines'
  | 'blocks';

/**
 * SoARelationship — a typed edge between two States of Affairs.
 * 
 * Relationships give SoA trees their graph-like cross-linking
 * capabilities. While parent/child links form the primary tree
 * structure, other relationship types allow rich semantic connections
 * across branches and even across different perspectives/agents.
 */
@ModelOptions({ name: 'SoARelationship' })
export default class SoARelationship extends Ad4mModel {
  @Flag({
    through: 'soa://entry_type',
    value: 'soa://relationship',
  })
  type: string;

  /**
   * The relationship type.
   */
  @Property({
    through: 'soa://relationship_type',
    resolveLanguage: 'literal',
    writable: true,
  })
  relationshipType: string;

  /**
   * Reference to the source StateOfAffair (its base expression / URI).
   */
  @Property({
    through: 'soa://relationship_source',
    resolveLanguage: 'literal',
    writable: true,
  })
  sourceRef: string;

  /**
   * Reference to the target StateOfAffair (its base expression / URI).
   */
  @Property({
    through: 'soa://relationship_target',
    resolveLanguage: 'literal',
    writable: true,
  })
  targetRef: string;

  /**
   * Strength or weight of the relationship (0.0 to 1.0).
   * How strongly does source support/contradict/etc. target?
   */
  @Property({
    through: 'soa://relationship_strength',
    resolveLanguage: 'literal',
    writable: true,
  })
  strength: number;

  /**
   * Optional annotation explaining the relationship.
   */
  @Property({
    through: 'soa://relationship_note',
    resolveLanguage: 'literal',
    writable: true,
  })
  note: string;

  /**
   * Who asserted this relationship.
   */
  @Property({
    through: 'soa://relationship_author',
    resolveLanguage: 'literal',
    writable: true,
  })
  author: string;

  /**
   * ISO 8601 timestamp of creation.
   */
  @Property({
    through: 'soa://relationship_created_at',
    resolveLanguage: 'literal',
    writable: true,
  })
  createdAt: string;
}
