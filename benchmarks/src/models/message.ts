/**
 * Message model — extracted from flux/packages/api/src/message/index.ts
 * Imports only @coasys/ad4m decorators.
 */
import { Ad4mModel, HasMany, Flag, Model, Property } from '@coasys/ad4m';
import {
  ENTRY_TYPE,
  BODY,
  HAS_REPLY,
  REACTION,
  TRANSCRIPT_STARTED_AT,
  MESSAGE_THREAD,
  EntryType,
} from './predicates';

@Model({ name: 'Message' })
export class Message extends Ad4mModel {
  @Flag({ through: ENTRY_TYPE, value: EntryType.Message })
  type: string;

  @Property({ through: BODY })
  body: string;

  @Property({ through: TRANSCRIPT_STARTED_AT })
  transcriptStartedAt?: string;

  @HasMany({ through: REACTION })
  reactions: string[] = [];

  @Property({
    through: HAS_REPLY,
    getter: `SELECT ?target WHERE { ?target <${HAS_REPLY}> ?source . } LIMIT 1`,
  })
  replyingTo?: string;

  @Property({
    through: 'flux://is_popular',
    getter: `ASK WHERE { SELECT (COUNT(DISTINCT ?reactor) AS ?count) WHERE { ?reactor <${REACTION}> ?source . FILTER(?reactor = <emoji://1f44d>) } HAVING(?count > 5) }`,
    readOnly: true,
  })
  isPopular: boolean = false;

  @HasMany({ through: MESSAGE_THREAD })
  thread: string[] = [];

  @HasMany({ through: HAS_REPLY })
  replies: string[] = [];
}
