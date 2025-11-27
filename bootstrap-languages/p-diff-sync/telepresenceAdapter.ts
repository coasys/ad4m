import type { TelepresenceAdapter, OnlineAgent, PerspectiveExpression, TelepresenceSignalCallback, HolochainLanguageDelegate, LanguageContext } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";;
import { DNA_ROLE, ZOME_NAME } from "./build/happ.js";

export class TelepresenceAdapterImplementation implements TelepresenceAdapter {
    hcDna: HolochainLanguageDelegate;
    signalCallbacks: TelepresenceSignalCallback[] = [];

    constructor(context: LanguageContext) {
        this.hcDna = context.Holochain as HolochainLanguageDelegate;
    }

    async setOnlineStatus(status: PerspectiveExpression): Promise<void> {
        await this.hcDna.call(DNA_ROLE, ZOME_NAME, "set_online_status", status);
    }

    async getOnlineAgents(): Promise<OnlineAgent[]> {
        //@ts-ignore
        const getActiveAgents = await this.hcDna.call(DNA_ROLE, ZOME_NAME, "get_active_agents", null);
        let calls = [];
        for (const activeAgent of getActiveAgents) {
            calls.push({dnaNick: DNA_ROLE, zomeName: ZOME_NAME, fnName: "get_agents_status", params: activeAgent});
        };
        return await this.hcDna.callAsync(calls, 1000) as OnlineAgent[];
    }

    async sendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object> {
        let res = await this.hcDna.call(DNA_ROLE, ZOME_NAME, "send_signal", {remote_agent_did: remoteAgentDid, payload});
        return res;
    }

    async sendBroadcast(payload: PerspectiveExpression): Promise<object> {
        let res = await this.hcDna.call(DNA_ROLE, ZOME_NAME, "send_broadcast", payload);
        return res;
    }

    async registerSignalCallback(callback: TelepresenceSignalCallback): Promise<void> {
        this.signalCallbacks.push(callback);
    }
}