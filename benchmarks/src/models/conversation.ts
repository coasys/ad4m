/**
 * Conversation model — extracted from flux/packages/api/src/conversation/index.ts
 * Imports only @coasys/ad4m decorators.
 */
import { Ad4mModel, HasMany, Flag, Model, Property } from '@coasys/ad4m';
import { FLUX_PARTICIPANT, CONVERSATION_SUBGROUP, EntryType } from './predicates';
import { ConversationSubgroup } from './conversation-subgroup';

@Model({ name: 'Conversation' })
export class Conversation extends Ad4mModel {
  @Flag({ through: 'flux://entry_type', value: EntryType.Conversation })
  type: string;

  @Property({ through: 'flux://has_name' })
  conversationName: string;

  @Property({ through: 'flux://name_is_fixed' })
  nameFixed: boolean = false;

  @Property({ through: 'flux://has_summary' })
  summary: string;

  @HasMany({ through: FLUX_PARTICIPANT })
  participants: string[] = [];

  @HasMany(() => ConversationSubgroup, { through: CONVERSATION_SUBGROUP })
  subgroupEntities: ConversationSubgroup[] = [];

  /**
   * Custom SPARQL: count subgroups + get participants.
   */
  async stats(): Promise<{ totalSubgroups: number; participants: string[] }> {
    const subgroupsQuery = `
      SELECT ?sg WHERE {
        <${this.id}> <${CONVERSATION_SUBGROUP}> ?sg .
        ?sg <flux://entry_type> <flux://conversation_subgroup> .
      }
    `;
    const result = await this.perspective.querySparql(subgroupsQuery);
    const totalSubgroups = result?.length || 0;
    await this.get();
    return { totalSubgroups, participants: this.participants };
  }
}
