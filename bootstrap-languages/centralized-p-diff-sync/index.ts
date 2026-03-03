import type { Address, Language, Interaction, HolochainLanguageDelegate, LanguageContext, AgentService } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { LinkAdapter } from "./linksAdapter.ts";
import { TelepresenceAdapterImplementation } from "./telepresenceAdapter.ts";
import { io } from "https://esm.sh/v135/socket.io-client@4.7.2";

function interactions(expression: Address): Interaction[] {
  return [];
}

//!@ad4m-template-variable
const name = "centralized-perspective-diff-sync";

//!@ad4m-template-variable
const uid = "centralized-perspective-diff-sync-uuid";

export default async function create(context: LanguageContext): Promise<Language> {
  let socketClient = io("https://socket.ad4m.dev", { 
    transports: ['websocket', 'polling'], 
    autoConnect: true, 
    query: { did: context.agent.did, linkLanguageUUID: uid } 
  });
  console.log("Created socket connection");

  const linksAdapter = new LinkAdapter(context, uid, socketClient);
  const telepresenceAdapter = new TelepresenceAdapterImplementation(context, uid, socketClient);

  //@ts-ignore
  return {
    name,
    linksAdapter,
    interactions,
    telepresenceAdapter
  } as Language;
}
