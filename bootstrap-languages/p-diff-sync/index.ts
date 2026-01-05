import type { Address, Language, Interaction, HolochainLanguageDelegate, LanguageContext, AgentService } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { LinkAdapter } from "./linksAdapter.ts";
import { TelepresenceAdapterImplementation } from "./telepresenceAdapter.ts";
import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

function interactions(expression: Address): Interaction[] {
  return [];
}

//!@ad4m-template-variable
const name = "perspective-diff-sync";

export default async function create(context: LanguageContext): Promise<Language> {
  const Holochain = context.Holochain as HolochainLanguageDelegate;
  const agent = context.agent as AgentService;

  console.log(`[p-diff-sync] create() called with agent DID: ${agent.did}`);

  const linksAdapter = new LinkAdapter(context);
  const telepresenceAdapter = new TelepresenceAdapterImplementation(context);

  console.log(`[p-diff-sync] Registering DNAs for agent: ${agent.did}`);

  await Holochain.registerDNAs(
    //@ts-ignore
    [{ file: BUNDLE, nick: DNA_ROLE, zomeCalls:
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
      const payload = signal.payload;


      // Link updates
      if (payload.reference || (payload.additions && payload.removals)) {
        await linksAdapter.handleHolochainSignal(signal);
        return;
      }

      // Routed telepresence signal (has recipient_did field from RoutedSignalPayload)
      if (payload.recipient_did) {
        // Check if this signal is for THIS specific user (agent.did)
        // Each language instance is created for a specific user

        const recipientDid = payload.recipient_did;
        const localUserDIDs = await agent.getAllLocalUserDIDs();
        
        if (! localUserDIDs.includes(recipientDid)) {
          console.error(`[p-diff-sync] Received Signal not for user on this node. Recipient is ${payload.recipient_did}. All local user DIDs: ${localUserDIDs.join(', ')}`);
          return; // Not for this user
        }

        // Reconstruct PerspectiveExpression from flattened RoutedSignalPayload
        const perspectiveExpression = {
          author: payload.author,
          data: payload.data,
          timestamp: payload.timestamp,
          proof: payload.proof
        };

        for (const callback of telepresenceAdapter.signalCallbacks) {
          await callback(perspectiveExpression, recipientDid);
        }
        return;
      }

      // Regular broadcast telepresence signal (no specific recipient)
      for (const callback of telepresenceAdapter.signalCallbacks) {
        await callback(payload);
      }
    }
  );

  console.log(`[p-diff-sync] Language initialized for agent: ${agent.did}. DID link will be created in LinkAdapter.sync()`);

  //@ts-ignore
  return {
    name,
    linksAdapter,
    interactions,
    telepresenceAdapter
  } as Language;
}
