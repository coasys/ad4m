import { ModelOptions, Ad4mModel, Property, Collection, Flag } from '@coasys/ad4m';

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
 */
@ModelOptions({ name: 'StateOfAffair' })
export default class StateOfAffair extends Ad4mModel {
  @Flag({
    through: 'soa://entry_type',
    value: 'soa://state_of_affair',
  })
  type: string;

  /**
   * Short summary of this state of affairs.
   */
  @Property({
    through: 'soa://title',
    resolveLanguage: 'literal',
    writable: true,
  })
  title: string;

  /**
   * Epistemic modality: belief, observation, intention, vision, plan, skill.
   */
  @Property({
    through: 'soa://modality',
    resolveLanguage: 'literal',
    writable: true,
  })
  modality: string;

  /**
   * Longer description or body text.
   */
  @Property({
    through: 'soa://description',
    resolveLanguage: 'literal',
    writable: true,
  })
  description: string;

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
  confidence: number;

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
  status: string;

  /**
   * Who authored/asserted this SoA.
   * Could be a DID, agent name, or identifier.
   */
  @Property({
    through: 'soa://author',
    resolveLanguage: 'literal',
    writable: true,
  })
  author: string;

  /**
   * ISO 8601 timestamp of when this SoA was created.
   */
  @Property({
    through: 'soa://created_at',
    resolveLanguage: 'literal',
    writable: true,
  })
  createdAt: string;

  /**
   * ISO 8601 timestamp of last modification.
   */
  @Property({
    through: 'soa://updated_at',
    resolveLanguage: 'literal',
    writable: true,
  })
  updatedAt: string;

  /**
   * Optional tags for categorization and search.
   * Stored as comma-separated values.
   */
  @Property({
    through: 'soa://tags',
    resolveLanguage: 'literal',
    writable: true,
  })
  tags: string;

  /**
   * Optional priority level (1-5, where 1 is highest).
   * Primarily useful for intentions and plans.
   */
  @Property({
    through: 'soa://priority',
    resolveLanguage: 'literal',
    writable: true,
  })
  priority: number;

  /**
   * Source or evidence for this SoA.
   * Could be a URL, file path, or description.
   */
  @Property({
    through: 'soa://source',
    resolveLanguage: 'literal',
    writable: true,
  })
  source: string;
}
