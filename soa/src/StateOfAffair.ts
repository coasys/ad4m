import { ModelOptions, Ad4mModel, Property } from '@coasys/ad4m';

/**
 * Modality of a State of Affairs — its epistemic status.
 * 
 * - belief: SoA we hold as true
 * - observation: SoA we have directly verified  
 * - intention: SoA we want to make true (goal)
 * - vision: High-level desired future state
 * - plan: Steps connecting current SoA to intended SoA
 * - skill: SoA about a capability an agent possesses
 */
export type SoAModality = 'belief' | 'observation' | 'intention' | 'vision' | 'plan' | 'skill';

/**
 * StateOfAffair — the fundamental unit of knowledge representation.
 * 
 * A proposition about how things are, could be, or should be.
 * Forms the nodes of State of Affairs trees, which represent
 * an agent's worldview, goals, plans, and capabilities.
 * 
 * Used by both humans and AI agents in the Eve architecture.
 * Same schema works for personal memory, shared task boards,
 * and collective intelligence networks.
 * 
 * Note: author and timestamp come from the link expressions
 * themselves — every AD4M link carries this metadata natively.
 * A SoA can have multiple authors (when created independently
 * by different agents).
 * 
 * Relationships between SoA nodes are expressed as AD4M links
 * using the soa language for predicates:
 *   link(soaA, "soa://rel_supports", soaB)
 *   link(soaA, "soa://rel_contradicts", soaB)
 *   link(soaA, "soa://rel_similar", soaB)
 *   link(soaA, "soa://rel_same", soaB)
 *   link(soaA, "soa://rel_requires", soaB)
 *   link(soaA, "soa://rel_enables", soaB)
 *   link(soaA, "soa://rel_parent", soaB)     // soaA is parent of soaB
 *   link(soaA, "soa://rel_refines", soaB)
 *   link(soaA, "soa://rel_blocks", soaB)
 */
@ModelOptions({ name: 'StateOfAffair' })
export default class StateOfAffair extends Ad4mModel {
  /**
   * Short summary of this state of affairs.
   */
  @Property({
    through: 'soa://title',
    resolveLanguage: 'literal',
    writable: true,
    required: true,
  })
  title: string;

  /**
   * Epistemic modality: belief, observation, intention, vision, plan, skill.
   */
  @Property({
    through: 'soa://modality',
    resolveLanguage: 'literal',
    writable: true,
    required: true,
  })
  modality: SoAModality;

  /**
   * Longer description or body text.
   */
  @Property({
    through: 'soa://description',
    resolveLanguage: 'literal',
    writable: true,
  })
  description?: string;

  /**
   * Confidence level (0.0 to 1.0).
   * For beliefs: how certain we are.
   * For intentions: how committed we are.
   * For observations: reliability of the observation.
   */
  @Property({
    through: 'soa://confidence',
    resolveLanguage: 'literal',
    writable: true,
  })
  confidence?: number;

  /**
   * Current status of this SoA.
   * For intentions/plans: 'active' | 'completed' | 'abandoned' | 'blocked'
   * For beliefs: 'held' | 'revised' | 'retracted'
   * For observations: 'current' | 'outdated'
   */
  @Property({
    through: 'soa://status',
    resolveLanguage: 'literal',
    writable: true,
  })
  status?: string;

  /**
   * Optional tags for categorization and search.
   * Stored as comma-separated values.
   */
  @Property({
    through: 'soa://tags',
    resolveLanguage: 'literal',
    writable: true,
  })
  tags?: string;

  /**
   * Optional priority level (1-5, where 1 is highest).
   * Primarily useful for intentions and plans.
   */
  @Property({
    through: 'soa://priority',
    resolveLanguage: 'literal',
    writable: true,
  })
  priority?: number;

  /**
   * Source or evidence for this SoA.
   * Could be a URL, file path, or description.
   */
  @Property({
    through: 'soa://source',
    resolveLanguage: 'literal',
    writable: true,
  })
  source?: string;
}
