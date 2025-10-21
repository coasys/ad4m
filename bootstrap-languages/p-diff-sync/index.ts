import type { Address, Language, Interaction, HolochainLanguageDelegate, LanguageContext, AgentService } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
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
      if (signal.payload.reference || (signal.payload.additions && signal.payload.removals)) {
        await linksAdapter.handleHolochainSignal(signal)
      } else {
        // For multi-user support: check if signal has recipient_did and filter accordingly
        const recipientDid = signal.payload.recipient_did;
        const actualPayload = signal.payload.payload || signal.payload;

        // Check if this signal should be delivered to ANY local user
        let shouldDeliver = !recipientDid; // If no recipient specified, it's a broadcast

        if (recipientDid) {
          // Check if recipient matches ANY local user DID
          if (typeof agent.getAllLocalUserDIDs === 'function') {
            try {
              const localDIDs = await agent.getAllLocalUserDIDs();
              shouldDeliver = localDIDs.includes(recipientDid);
            } catch (e) {
              console.error(`[p-diff-sync] Failed to get local user DIDs for signal routing:`, e);
              // Fallback: check against current agent DID only
              shouldDeliver = recipientDid === agent.did;
            }
          } else {
            // Fallback: check against current agent DID only
            shouldDeliver = recipientDid === agent.did;
          }
        }

        if (shouldDeliver) {
          for (const callback of telepresenceAdapter.signalCallbacks) {
            await callback(actualPayload);
          }
        }
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
