/**
 * ConversationSubgroup model — extracted from flux/packages/api/src/conversation-subgroup/index.ts
 * Imports only @coasys/ad4m decorators.
 */
import { Ad4mModel, HasMany, Flag, Model, Property } from '@coasys/ad4m';
import { FLUX_PARTICIPANT, SUBGROUP_ITEM, EntryType } from './predicates';

@Model({ name: 'ConversationSubgroup' })
export class ConversationSubgroup extends Ad4mModel {
  @Flag({ through: 'flux://entry_type', value: EntryType.ConversationSubgroup })
  type: string;

  @Property({ through: 'flux://has_name' })
  subgroupName: string;

  @Property({ through: 'flux://has_summary' })
  summary: string;

  @HasMany({ through: FLUX_PARTICIPANT })
  participants: string[] = [];

  /**
   * Custom SPARQL: count items + get participants.
   */
  async stats(): Promise<{ totalItems: number; participants: string[] }> {
    const itemsQuery = `
      SELECT DISTINCT ?item WHERE {
        <${this.id}> <${SUBGROUP_ITEM}> ?item .
        ?item <flux://entry_type> ?type .
        FILTER(?type IN (<flux://has_message>, <flux://has_post>, <flux://has_task>))
      }
    `;
    const result = await this.perspective.querySparql(itemsQuery);
    const totalItems = result?.length || 0;
    await this.get();
    return { totalItems, participants: this.participants };
  }

  /**
   * Custom SPARQL: full item data with multi-join.
   */
  async itemsData(): Promise<any[]> {
    const sparqlQuery = `
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT ?id ?type ?author ?timestamp ?body ?title ?taskName WHERE {
        <${this.id}> <${SUBGROUP_ITEM}> ?id .
        ?_reifier rdf:reifies <<( <${this.id}> <${SUBGROUP_ITEM}> ?id )>> .
        ?_reifier <ad4m://ontology/timestamp> ?timestamp .
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
}
