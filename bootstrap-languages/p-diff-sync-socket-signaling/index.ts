import type { Address, Language, Interaction, HolochainLanguageDelegate, LanguageContext, AgentService } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { LinkAdapter } from "./linksAdapter.ts";
import { TelepresenceAdapterImplementation } from "./telepresenceAdapter.ts";
import { DNA, DNA_NICK, ZOME_NAME } from "./build/dna.js";

function interactions(expression: Address): Interaction[] {
  return [];
}

//!@ad4m-template-variable
const name = "perspective-diff-sync";

//!@ad4m-template-variable
const uid = "perspective-diff-sync-uuid";

export default async function create(context: LanguageContext): Promise<Language> {
  const Holochain = context.Holochain as HolochainLanguageDelegate;
  const agent = context.agent as AgentService;

  const linksAdapter = new LinkAdapter(context, uid);
  const telepresenceAdapter = new TelepresenceAdapterImplementation(context);

  await Holochain.registerDNAs(
    //@ts-ignore
    [{ file: DNA, nick: DNA_NICK, zomeCalls: 
      [
        [ZOME_NAME, "current_revision"],
        [ZOME_NAME, "sync"],
        [ZOME_NAME, "render"],
        [ZOME_NAME, "commit"],
        [ZOME_NAME, "fast_forward_signal"],
        [ZOME_NAME, "get_others"],
        [ZOME_NAME, "add_active_agent_link"],
        [ZOME_NAME, "create_did_pub_key_link"],
      ]
    }],
    async (signal) => {
      //@ts-ignore
      if (signal.payload.diff || (signal.payload.additions && signal.payload.removals)) {
        await linksAdapter.handleHolochainSignal(signal)
      } else {
        // For multi-user support: check if signal has recipient_did and filter accordingly
        const recipientDid = signal.payload.recipient_did;
        const actualPayload = signal.payload.payload || signal.payload;

        // If there's a recipient DID specified, only deliver to that user
        // Otherwise (broadcast), deliver to all users
        if (!recipientDid || recipientDid === agent.did) {
          for (const callback of telepresenceAdapter.signalCallbacks) {
            await callback(actualPayload);
          }
        }
      }
    }
  );

  //Setup the link between did and agent pub key
  await Holochain.call(DNA_NICK, ZOME_NAME, "create_did_pub_key_link", agent.did);

  //@ts-ignore
  return {
    name,
    linksAdapter,
    interactions,
    telepresenceAdapter
  } as Language;
}
