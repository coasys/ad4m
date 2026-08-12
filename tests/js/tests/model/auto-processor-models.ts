/**
 * ConversationSubgroup model for the auto-processor integration tests — the JS
 * (@Model) mirror of the Rust `CONVERSATION_SUBGROUP_SDNA`. The interpretation
 * hints carry the same Flux "grouping" rules: reuse an existing subgroup's id
 * only on the same topic, mint a new one on a topic shift, and extend the
 * rolling summary in place.
 */
import { Ad4mModel, Flag, Model, Property } from "@coasys/ad4m";

@Model({
  name: "ConversationSubgroup",
  interpretationHint:
    "A coherent conversational thread — a set of turns focused on the same topic. Group turns discussing the same subject under one subgroup; a clear shift in subject starts a new subgroup. When an existing subgroup already covers the topic being discussed, REUSE its id (via the `id` field on the proposed instance) instead of creating a duplicate. CRITICAL DECISION RULE: read each `existing` entry's `title` (its topic name) BEFORE deciding whether to reuse an id. Only reuse an existing subgroup's id when the current turns are clearly on the SAME topic as that subgroup's title. If the current turns are on a different topic — even if there is only one existing subgroup — leave `id` unset and mint a NEW subgroup.",
})
export class ConversationSubgroup extends Ad4mModel {
  @Flag({ through: "ns://type", value: "ns://conversationsubgroup" })
  type: string = "ns://conversationsubgroup";

  @Property({
    through: "ns://name",
    required: true,
    resolveLanguage: "literal",
    identity: true,
    interpretationHint: "Short label for the topic (2-5 words).",
  })
  name: string = "";

  @Property({
    through: "ns://summary",
    resolveLanguage: "literal",
    interpretationHint:
      "1-2 sentence rolling summary of what has been discussed in THIS subgroup specifically — its own topic only. When updating an existing subgroup, incorporate ONLY the new turns that belong to this subgroup's topic, extending the existing summary rather than replacing it. NEVER fold in turns about a different topic.",
  })
  summary: string = "";
}
