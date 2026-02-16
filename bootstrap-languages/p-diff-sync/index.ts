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
      try {
        //@ts-ignore
        const payload = signal.payload;

        // DIAGNOSTIC: Log raw signal shape to debug why signals are not being routed
        const payloadKeys = payload ? Object.keys(payload) : 'null/undefined';
        const payloadType = typeof payload;
        const isArray = Array.isArray(payload);
        console.log(`[p-diff-sync] 🔍 RAW SIGNAL ENTERED CALLBACK: type=${payloadType}, isArray=${isArray}, keys=${JSON.stringify(payloadKeys)}, has_reference=${!!payload?.reference}, has_additions=${!!payload?.additions}, has_removals=${!!payload?.removals}, has_recipient_did=${!!payload?.recipient_did}`);
        if (isArray) {
          console.log(`[p-diff-sync] 🔍 RAW SIGNAL ARRAY: length=${payload.length}, first_element_type=${typeof payload[0]}, first_element_keys=${payload[0] ? JSON.stringify(Object.keys(payload[0])).substring(0, 200) : 'N/A'}`);
        }
        // Log first few bytes/values of key fields for deeper inspection
        if (payload && payloadType === 'object' && !isArray) {
          const refType = typeof payload.reference;
          const refVal = payload.reference ? JSON.stringify(payload.reference).substring(0, 200) : 'falsy';
          const addType = typeof payload.additions;
          const remType = typeof payload.removals;
          console.log(`[p-diff-sync] 🔍 PAYLOAD DETAILS: reference(${refType})=${refVal}, additions(${addType})=${payload.additions ? 'length=' + (payload.additions.length || 'N/A') : 'falsy'}, removals(${remType})=${payload.removals ? 'length=' + (payload.removals.length || 'N/A') : 'falsy'}`);
        }

        // Link updates
        if (payload.reference || (payload.additions && payload.removals)) {
          const signalType = payload.reference ? 'HashBroadcast' : 'DiffSignal';
          console.log(`[p-diff-sync] 📡 Signal MATCHED link-update branch: type=${signalType}, has_additions=${!!payload.additions}, additions_count=${payload.additions?.length || 0}`);
          await linksAdapter.handleHolochainSignal(signal);
          console.log(`[p-diff-sync] 📡 Signal link-update branch COMPLETED`);
          return;
        }

        // Routed telepresence signal (has recipient_did field from RoutedSignalPayload)
        if (payload.recipient_did) {
          console.log(`[p-diff-sync] 📡 Signal MATCHED routed-telepresence branch: recipient_did=${payload.recipient_did}`);
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
        console.log(`[p-diff-sync] 📡 Signal FELL THROUGH to broadcast-telepresence (no branch matched). telepresence callbacks: ${telepresenceAdapter.signalCallbacks.length}`);
        for (const callback of telepresenceAdapter.signalCallbacks) {
          await callback(payload);
        }
      } catch (e) {
        console.error(`[p-diff-sync] ❌ SIGNAL CALLBACK ERROR:`, e);
        console.error(`[p-diff-sync] ❌ Error stack:`, e?.stack || 'no stack');
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
