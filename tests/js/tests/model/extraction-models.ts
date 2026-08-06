/**
 * SoA subject classes for the `perspective.runExtraction` integration test.
 *
 * Each class carries an `extractionHint` (class-level + per-property) — the
 * natural-language guidance the generic LLM extractor uses to turn a transcript
 * into typed instances. The field named `title` is the instance identity used
 * for dedup.
 */

import { Ad4mModel, Flag, Model, Property } from "@coasys/ad4m";

@Model({
  name: "ExtTask",
  extractionHint:
    "A concrete, actionable unit of work someone will do, ideally with an owner. Not a belief or a question.",
})
export class ExtTask extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://task" })
  type = "soa://task";

  @Property({ through: "soa://title", required: true, extractionHint: "Imperative summary of the task." })
  title: string = "";

  @Property({ through: "soa://owner", extractionHint: "Person responsible for the task, if stated." })
  owner?: string;
}

@Model({
  name: "ExtBelief",
  extractionHint: "A claim a participant holds to be true. Not a task or a question.",
})
export class ExtBelief extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://belief" })
  type = "soa://belief";

  @Property({ through: "soa://title", required: true, extractionHint: "One-sentence statement of the claim." })
  title: string = "";
}

@Model({
  name: "ExtQuestion",
  extractionHint: "An open question raised in the conversation that still needs an answer.",
})
export class ExtQuestion extends Ad4mModel {
  @Flag({ through: "soa://type", value: "soa://question" })
  type = "soa://question";

  @Property({ through: "soa://title", required: true, extractionHint: "The question, phrased as a question." })
  title: string = "";
}
