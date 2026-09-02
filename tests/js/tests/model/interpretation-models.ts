/**
 * SoA subject classes for the `perspective.runInterpretation` integration test.
 *
 * Each class carries an `interpretationHint` (class-level + per-property) — the
 * natural-language guidance the generic LLM extractor uses to turn a transcript
 * into typed instances. The field named `title` is the instance identity used
 * for dedup.
 */

import { Ad4mModel, Flag, HasMany, Model, Property } from "@coasys/ad4m";

@Model({
  name: "ExtTask",
  interpretationHint:
    "A concrete, actionable unit of work someone will do, ideally with an owner. Not a belief or a question.",
})
export class ExtTask extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://task" })
  type = "soa://task";

  @Property({ through: "soa://title", required: true, identity: true, interpretationHint: "Imperative summary of the task." })
  title: string = "";

  @Property({ through: "soa://owner", interpretationHint: "Person responsible for the task, if stated." })
  owner?: string;
}

@Model({
  name: "ExtBelief",
  interpretationHint: "A claim a participant holds to be true. Not a task or a question.",
})
export class ExtBelief extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://belief" })
  type = "soa://belief";

  @Property({ through: "soa://title", required: true, identity: true, interpretationHint: "One-sentence statement of the claim." })
  title: string = "";
}

@Model({
  name: "ExtQuestion",
  interpretationHint: "An open question raised in the conversation that still needs an answer.",
})
export class ExtQuestion extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://question" })
  type = "soa://question";

  @Property({ through: "soa://title", required: true, identity: true, interpretationHint: "The question, phrased as a question." })
  title: string = "";
}

@Model({
  name: "ExtIntention",
  interpretationHint:
    "An intention someone forms that follows from — and possibly stands against — existing beliefs. Not a raw task; not a bare belief. If the transcript expresses a course of action justified by (or reacting to) beliefs already in the perspective, extract the intention and link it to those beliefs via the correct relation.",
})
export class ExtIntention extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://intention" })
  type = "soa://intention";

  @Property({ through: "soa://title", required: true, identity: true, interpretationHint: "One-sentence statement of the intention (what someone intends to do or bring about)." })
  title: string = "";

  /**
   * Beliefs the intention DERIVES from — the positive grounding case:
   * "we intend to do X BECAUSE we believe Y." The per-relation
   * `interpretationHint` is what the Rust harness surfaces to the LLM in
   * the `ExtIntention_propose_link_child` `predicate` field description,
   * so the model knows to pick THIS predicate when a belief supports
   * the intention.
   */
  @HasMany(() => ExtBelief, {
    through: "soa://basedOn",
    interpretationHint:
      "The prior beliefs this intention derives from — beliefs that JUSTIFY the intention. Use this predicate when a belief supports or motivates the intended course of action.",
  })
  basedOn: ExtBelief[] = [];

  /**
   * Beliefs the intention STANDS AGAINST — the negative case: "we
   * intend to do X to counter belief Z." Separate relation, separate
   * predicate URI, separate interpretation hint. The relation-hint
   * test scenario seeds a mix of supporting and opposing beliefs and
   * asserts the LLM picks the right predicate per belief.
   */
  @HasMany(() => ExtBelief, {
    through: "soa://contradicts",
    interpretationHint:
      "Beliefs this intention REJECTS or seeks to counter — beliefs the intended course of action is a response AGAINST. Use this predicate when a belief opposes or is opposed by the intention.",
  })
  contradicts: ExtBelief[] = [];
}
