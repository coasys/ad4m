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
    "An intention someone forms that follows from — and is grounded in — one or more existing beliefs. Not a raw task; not a bare belief. If the transcript expresses a course of action justified by beliefs already in the perspective, extract the intention and link it to the beliefs it rests on.",
})
export class ExtIntention extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://intention" })
  type = "soa://intention";

  @Property({ through: "soa://title", required: true, identity: true, interpretationHint: "One-sentence statement of the intention (what someone intends to do or bring about)." })
  title: string = "";

  // TODO: relation-level `interpretationHint` is not on `RelationOptions` yet
  // (only Property + Model carry it). The class-level hint above therefore
  // has to name the `basedOn` relation explicitly so the LLM knows to link
  // existing beliefs it discovered via `ExtBelief_query`.
  @HasMany(() => ExtBelief, { through: "soa://basedOn" })
  basedOn: ExtBelief[] = [];
}
