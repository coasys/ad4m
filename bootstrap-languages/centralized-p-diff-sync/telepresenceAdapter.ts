import type { TelepresenceAdapter, OnlineAgent, PerspectiveExpression, DID, TelepresenceSignalCallback, HolochainLanguageDelegate, LanguageContext } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";;
import makeHttpRequest from "./util.ts";

export class TelepresenceAdapterImplementation implements TelepresenceAdapter {
    me: DID
    uuid: string;
    hcDna: HolochainLanguageDelegate;
    signalCallbacks: TelepresenceSignalCallback[] = [];

    constructor(context: LanguageContext, uuid: string) {
        this.hcDna = context.Holochain as HolochainLanguageDelegate;
        this.me = context.agent.did;
        this.uuid = uuid;
    }

    async setOnlineStatus(status: PerspectiveExpression): Promise<void> {     
        await makeHttpRequest("https://socket.ad4m.dev/setStatus", "POST",  {}, {
            did: this.me,
            link: status,
            LinkLanguageUUID: this.uuid
        })
    }

    async getOnlineAgents(): Promise<OnlineAgent[]> {
        const result = await makeHttpRequest("https://socket.ad4m.dev/getOnlineAgents", "GET",  {}, {
            did: this.me,
            LinkLanguageUUID: this.uuid
        })

        // @ts-ignore
        return result
    }

    async sendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object> {
        //let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "send_signal", {remote_agent_did: remoteAgentDid, payload});
        return {};
    }

    async sendBroadcast(payload: PerspectiveExpression): Promise<object> {
        //let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "send_broadcast", payload);
        return {};
    }

    async registerSignalCallback(callback: TelepresenceSignalCallback): Promise<void> {
        this.signalCallbacks.push(callback);
    }
}