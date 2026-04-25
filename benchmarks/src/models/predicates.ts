/**
 * Flux ontology predicates — extracted from flux/packages/constants/src/communityPredicates.ts
 * and flux/packages/types/src/index.ts.
 * Zero runtime dependencies on Flux packages.
 */

// ── Entry types ──
export enum EntryType {
  Community = 'flux://has_community',
  Channel = 'flux://has_channel',
  Message = 'flux://has_message',
  Post = 'flux://has_post',
  Member = 'flux://has_member',
  App = 'flux://has_app',
  Conversation = 'flux://conversation',
  ConversationSubgroup = 'flux://conversation_subgroup',
  Task = 'flux://has_task',
  Topic = 'flux://has_topic',
  SemanticRelationship = 'flux://has_semantic_relationship',
}

// ── Predicates ──
export const ENTRY_TYPE = 'flux://entry_type';
export const BODY = 'flux://body';
export const TITLE = 'flux://title';
export const URL = 'flux://url';
export const IMAGE = 'flux://image';
export const THUMBNAIL = 'flux://thumbnail';

export const CHANNEL_NAME = 'flux://has_channel_name';
export const CHANNEL_DESCRIPTION = 'flux://has_channel_description';
export const CHANNEL_IS_CONVERSATION = 'flux://channel_is_conversation';
export const CHANNEL_IS_PINNED = 'flux://channel_is_pinned';

export const HAS_REPLY = 'flux://has_reply';
export const REACTION = 'flux://has_reaction';
export const TRANSCRIPT_STARTED_AT = 'flux://transcript_started_at';

export const FLUX_PARTICIPANT = 'flux://has_participant';
export const CHANNEL_MESSAGE = 'flux://has_message';
export const CHANNEL_CONVERSATION = 'flux://has_conversation';
export const CHANNEL_SUBCHANNEL = 'flux://has_subchannel';
export const CONVERSATION_SUBGROUP = 'flux://has_subgroup';
export const SUBGROUP_ITEM = 'flux://has_item';
export const MESSAGE_THREAD = 'flux://has_thread_message';
export const CHANNEL_TASK_BOARD = 'flux://has_task_board';
export const CHANNEL_TASK_COLUMN = 'flux://has_task_column';
export const CHANNEL_TASK = 'flux://has_task';
export const CHANNEL_POST = 'flux://has_post';
export const TASK_COMMENT = 'flux://has_task_comment';
export const POST_COMMENT = 'flux://has_post_comment';
export const CHANNEL_POLL = 'flux://has_poll';

export const NAME = 'rdf://name';
export const DESCRIPTION = 'rdf://description';
export const CREATOR = 'rdf://creator';
export const CREATED_AT = 'rdf://dateCreated';
export const CARD_HIDDEN = 'flux://is_card_hidden';
export const EDITED_TO = 'temp://edited_to';
export const START_DATE = 'flux://start_date';
export const END_DATE = 'flux://end_date';
