/**
 * Channel model — extracted from flux/packages/api/src/channel/index.ts
 * Imports only @coasys/ad4m decorators.
 */
import { Ad4mModel, HasMany, Flag, Model, Property } from '@coasys/ad4m';
import {
  ENTRY_TYPE,
  CHANNEL_NAME,
  CHANNEL_DESCRIPTION,
  CHANNEL_IS_CONVERSATION,
  CHANNEL_IS_PINNED,
  FLUX_PARTICIPANT,
  CHANNEL_MESSAGE,
  CHANNEL_CONVERSATION,
  CHANNEL_SUBCHANNEL,
  SUBGROUP_ITEM,
  EntryType,
} from './predicates';
import { Message } from './message';
import { Conversation } from './conversation';

@Model({ name: 'Channel' })
export class Channel extends Ad4mModel {
  @Flag({ through: ENTRY_TYPE, value: EntryType.Channel })
  type: string;

  @Property({ through: CHANNEL_NAME })
  name: string;

  @Property({ through: CHANNEL_DESCRIPTION })
  description: string;

  @Property({ through: CHANNEL_IS_CONVERSATION })
  isConversation: boolean;

  @Property({ through: CHANNEL_IS_PINNED })
  isPinned: boolean;

  @HasMany({ through: FLUX_PARTICIPANT })
  participants: string[] = [];

  @HasMany(() => Message, { through: CHANNEL_MESSAGE })
  messages: Message[] = [];

  @HasMany(() => Conversation, { through: CHANNEL_CONVERSATION })
  conversations: Conversation[] = [];

  @HasMany(() => Channel, { through: CHANNEL_SUBCHANNEL })
  childChannels: Channel[] = [];

  /**
   * Custom SPARQL: get all items (messages, posts, tasks) ordered by timestamp.
   */
  async allItems(): Promise<any[]> {
    const sparqlQuery = `
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT ?id ?author ?timestamp ?type ?body ?title ?taskName WHERE {
        <${this.id}> <${CHANNEL_MESSAGE}> ?id .
        ?_reifier rdf:reifies <<( <${this.id}> <${CHANNEL_MESSAGE}> ?id )>> .
        ?_reifier <ad4m://ontology/timestamp> ?timestamp .
        ?_reifier <ad4m://ontology/author> ?author .
        ?id <flux://entry_type> ?type .
        FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
        OPTIONAL { ?id <flux://body> ?body . }
        OPTIONAL { ?id <flux://title> ?title . }
        OPTIONAL { ?id <flux://name> ?taskName . }
      }
      ORDER BY ?timestamp
    `;
    return (await this.perspective.querySparql(sparqlQuery)) || [];
  }

  /**
   * Custom SPARQL: unprocessed items (set-difference approach).
   */
  async unprocessedItems(): Promise<any[]> {
    const allItemsQuery = `
      SELECT ?id WHERE {
        <${this.id}> <${CHANNEL_MESSAGE}> ?id .
        ?id <flux://entry_type> ?type .
        FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
      }
    `;
    const processedQuery = `
      SELECT ?id WHERE {
        ?sg <${SUBGROUP_ITEM}> ?id .
        ?sg <flux://entry_type> <flux://conversation_subgroup> .
      }
    `;

    const [allItemsResult, processedResult] = await Promise.all([
      this.perspective.querySparql(allItemsQuery),
      this.perspective.querySparql(processedQuery),
    ]);

    const processedSet = new Set((processedResult || []).map((r: any) => r.id));
    const unprocessedIds = (allItemsResult || [])
      .map((r: any) => r.id)
      .filter((id: string) => id && !processedSet.has(id));

    if (unprocessedIds.length === 0) return [];

    const valuesClause = unprocessedIds.map((id: string) => `<${id}>`).join(' ');
    const dataQuery = `
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT ?id ?author ?timestamp ?type ?body ?title ?taskName WHERE {
        VALUES ?id { ${valuesClause} }
        <${this.id}> <${CHANNEL_MESSAGE}> ?id .
        ?_reifier rdf:reifies <<( <${this.id}> <${CHANNEL_MESSAGE}> ?id )>> .
        ?_reifier <ad4m://ontology/author> ?author .
        ?_reifier <ad4m://ontology/timestamp> ?timestamp .
        ?id <flux://entry_type> ?type .
        FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
        OPTIONAL { ?id <flux://body> ?body . }
        OPTIONAL { ?id <flux://title> ?title . }
        OPTIONAL { ?id <flux://name> ?taskName . }
      }
      ORDER BY ?timestamp
    `;

    return (await this.perspective.querySparql(dataQuery)) || [];
  }

  /**
   * Custom SPARQL: COUNT DISTINCT items.
   */
  async totalItemCount(): Promise<number> {
    const sparqlQuery = `
      SELECT (COUNT(DISTINCT ?id) AS ?count) WHERE {
        <${this.id}> <${CHANNEL_MESSAGE}> ?id .
        ?id <flux://entry_type> ?type .
        FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
      }
    `;
    const result = await this.perspective.querySparql(sparqlQuery);
    const countValue = result?.[0]?.count;
    return countValue ? parseInt(countValue, 10) : 0;
  }
}
