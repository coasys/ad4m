/**
 * SoA Language Expression Registry
 * 
 * Static documentation for every soa:// expression.
 * When an agent resolves a soa:// URL, they get back structured
 * documentation that helps them understand what it means and
 * how to use it in context.
 * 
 * This is the canonical reference for the SoA ontology —
 * a pattern for how AD4M languages can serve as
 * self-documenting semantic vocabularies.
 */

export interface SoADocumentation {
  /** Human-readable name */
  name: string;
  /** What this expression represents */
  description: string;
  /** Whether this is a property (on a node) or a relationship (between nodes) */
  kind: 'property' | 'relationship';
  /** Expected data type for properties, or role description for relationships */
  valueType?: string;
  /** Usage example as an AD4M link triple */
  example: string;
  /** Additional notes for AI agents working with this expression */
  agentNotes?: string;
}

/**
 * All known soa:// expressions and their documentation.
 * 
 * Properties are used as predicates in links from a StateOfAffair
 * base expression to literal values:
 *   link(soa_instance, "soa://title", "literal://string:My Belief")
 * 
 * Relationships are used as predicates in links between two
 * StateOfAffair instances:
 *   link(soa_A, "soa://rel_supports", soa_B)
 */
export const SOA_EXPRESSIONS: Record<string, SoADocumentation> = {
  // ── Properties ──────────────────────────────────────────────

  'title': {
    name: 'Title',
    kind: 'property',
    description: 'Short summary of a State of Affairs — the core proposition or claim.',
    valueType: 'string (via literal:// language)',
    example: 'link(soa, "soa://title", "literal://string:Distributed systems are more resilient")',
    agentNotes: 'Every StateOfAffair must have exactly one title. Keep it concise — use description for details.',
  },

  'modality': {
    name: 'Modality',
    kind: 'property',
    description: 'Epistemic status of this State of Affairs — what kind of knowledge it represents.',
    valueType: 'enum: belief | observation | intention | vision | plan | skill',
    example: 'link(soa, "soa://modality", "literal://string:belief")',
    agentNotes: 'Modality determines how to interpret confidence and status. A "belief" with 0.3 confidence is uncertain; an "intention" with 0.3 confidence is weakly committed. "vision" and "plan" are related: visions are high-level desired futures, plans are concrete steps toward them.',
  },

  'description': {
    name: 'Description',
    kind: 'property',
    description: 'Extended description, evidence, reasoning, or body text for a State of Affairs.',
    valueType: 'string (via literal:// language)',
    example: 'link(soa, "soa://description", "literal://string:Based on 20 years of distributed systems research...")',
    agentNotes: 'Optional. Use for context that doesn\'t fit in the title. Can be long-form.',
  },

  'confidence': {
    name: 'Confidence',
    kind: 'property',
    description: 'Confidence level from 0.0 to 1.0. Meaning depends on modality: certainty (beliefs), reliability (observations), commitment (intentions).',
    valueType: 'number 0.0-1.0 (via literal:// language)',
    example: 'link(soa, "soa://confidence", "literal://number:0.85")',
    agentNotes: 'Optional. When absent, no confidence judgment is implied. Useful for epistemic humility — an agent that tracks confidence can reason about what it knows vs. what it assumes.',
  },

  'status': {
    name: 'Status',
    kind: 'property',
    description: 'Current lifecycle status. Values depend on modality: intentions/plans use active|completed|abandoned|blocked; beliefs use held|revised|retracted; observations use current|outdated.',
    valueType: 'string (via literal:// language)',
    example: 'link(soa, "soa://status", "literal://string:active")',
    agentNotes: 'Optional. Tracks the evolution of knowledge over time. "revised" means the belief has been updated (look for a newer version via rel_refines). "retracted" means it was wrong.',
  },

  'tags': {
    name: 'Tags',
    kind: 'property',
    description: 'Comma-separated tags for categorization, filtering, and search.',
    valueType: 'string, comma-separated (via literal:// language)',
    example: 'link(soa, "soa://tags", "literal://string:architecture,distributed,resilience")',
    agentNotes: 'Optional. Use for cross-cutting concerns that don\'t fit the tree structure. Enables flat queries across the SoA graph.',
  },

  'priority': {
    name: 'Priority',
    kind: 'property',
    description: 'Priority level from 1 (highest) to 5 (lowest). Most useful for intentions and plans.',
    valueType: 'number 1-5 (via literal:// language)',
    example: 'link(soa, "soa://priority", "literal://number:1")',
    agentNotes: 'Optional. Only meaningful for actionable SoAs (intentions, plans). A priority-1 intention should be worked on before priority-3.',
  },

  'source': {
    name: 'Source',
    kind: 'property',
    description: 'Source of evidence or origin of this State of Affairs. Could be a URL, document reference, person, or description of how it was learned.',
    valueType: 'string (via literal:// language)',
    example: 'link(soa, "soa://source", "literal://string:https://arxiv.org/abs/...")',
    agentNotes: 'Optional. Crucial for provenance tracking. When an AI agent creates a SoA from external information, always set the source so humans can verify.',
  },

  // ── Relationships ───────────────────────────────────────────

  'rel_supports': {
    name: 'Supports',
    kind: 'relationship',
    description: 'Source provides evidence, argument, or backing for target. The source SoA makes the target SoA more likely or justified.',
    example: 'link(evidence_soa, "soa://rel_supports", claim_soa)',
    agentNotes: 'Directional: A supports B means A is evidence FOR B. Multiple SoAs can support the same target, building a body of evidence. Combined with rel_contradicts, enables argument mapping.',
  },

  'rel_contradicts': {
    name: 'Contradicts',
    kind: 'relationship',
    description: 'Source provides evidence or argument against target. The source SoA undermines, challenges, or refutes the target SoA.',
    example: 'link(counter_evidence, "soa://rel_contradicts", claim_soa)',
    agentNotes: 'Directional: A contradicts B means A is evidence AGAINST B. Use with confidence scores to build nuanced worldviews — a belief with high-confidence contradictions should lower its own confidence.',
  },

  'rel_similar': {
    name: 'Similar',
    kind: 'relationship',
    description: 'Source and target are related but not identical propositions. They share themes, domains, or implications.',
    example: 'link(soa_A, "soa://rel_similar", soa_B)',
    agentNotes: 'Symmetric in meaning (if A is similar to B, B is similar to A), though the link itself is directional. Use for clustering related ideas. NOT the same as rel_same.',
  },

  'rel_same': {
    name: 'Same',
    kind: 'relationship',
    description: 'Source and target express the same proposition, possibly worded differently or from different perspectives. Semantic equivalence.',
    example: 'link(soa_A, "soa://rel_same", soa_B)',
    agentNotes: 'Use for deduplication and cross-referencing when the same insight appears in different contexts. If two agents independently arrive at the same SoA, linking them with rel_same enables collective intelligence without merging.',
  },

  'rel_requires': {
    name: 'Requires',
    kind: 'relationship',
    description: 'Target depends on source — target cannot be true/achieved without source. Logical or practical prerequisite.',
    example: 'link(prerequisite_soa, "soa://rel_requires", dependent_soa)',
    agentNotes: 'Directional: A requires B means B is needed for A. Useful for planning — find all unmet requirements by traversing rel_requires to SoAs with status != "completed". Forms a dependency graph.',
  },

  'rel_enables': {
    name: 'Enables',
    kind: 'relationship',
    description: 'Source creates conditions or capability for target. Softer than requires — source helps but isn\'t strictly necessary.',
    example: 'link(capability_soa, "soa://rel_enables", goal_soa)',
    agentNotes: 'Directional: A enables B means A makes B possible or easier. The inverse of blocks. Use for mapping how capabilities connect to goals.',
  },

  'rel_parent': {
    name: 'Parent',
    kind: 'relationship',
    description: 'Tree structure: source is the parent of target. Creates hierarchical organization of States of Affairs into trees.',
    example: 'link(parent_soa, "soa://rel_parent", child_soa)',
    agentNotes: 'This is the primary structural relationship for SoA trees. A vision breaks down into intentions, which break down into plans. Each SoA should have at most one parent (tree, not DAG). Root SoAs with no parent are top-level worldview anchors.',
  },

  'rel_refines': {
    name: 'Refines',
    kind: 'relationship',
    description: 'Source is a more specific, detailed, or updated version of target. Tracks epistemic evolution.',
    example: 'link(refined_soa, "soa://rel_refines", original_soa)',
    agentNotes: 'Directional: A refines B means A is a better version of B. Use when updating beliefs — create a new SoA, link it with rel_refines to the old one, and set the old one\'s status to "revised". This preserves history while showing current thinking.',
  },

  'rel_blocks': {
    name: 'Blocks',
    kind: 'relationship',
    description: 'Source prevents, inhibits, or blocks target. The source SoA is an obstacle to the target SoA.',
    example: 'link(blocker_soa, "soa://rel_blocks", blocked_soa)',
    agentNotes: 'Directional: A blocks B means A is an obstacle to B. The inverse of enables. Particularly useful for intentions with status "blocked" — the blocking SoA explains why. Resolving the blocker should unblock the target.',
  },
};
