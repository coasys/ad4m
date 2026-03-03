import type { TelepresenceAdapter, OnlineAgent, PerspectiveExpression, DID, TelepresenceSignalCallback, HolochainLanguageDelegate, LanguageContext } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";;
import axiod from "https://deno.land/x/axiod/mod.ts";
import type { Socket, ServerToClientEvents, ClientToServerEvents } from "https://esm.sh/v135/socket.io-client@4.7.2";

export class TelepresenceAdapterImplementation implements TelepresenceAdapter {
    me: DID
    uuid: string;
    hcDna: HolochainLanguageDelegate;
    signalCallbacks: TelepresenceSignalCallback[] = [];
    socketClient: Socket<ServerToClientEvents, ClientToServerEvents>;

    constructor(context: LanguageContext, uuid: string, socketClient: Socket<ServerToClientEvents, ClientToServerEvents>) {
        this.hcDna = context.Holochain as HolochainLanguageDelegate;
        this.me = context.agent.did;
        this.uuid = uuid;
        this.socketClient = socketClient;

        //Add broadcast signal handler from socket to signalCallbacks
        this.socketClient.on("telepresence-signal", (payload: PerspectiveExpression) => {
            this.signalCallbacks.forEach(callback => {
                callback(payload);
            });
        });
    }

    async setOnlineStatus(status: PerspectiveExpression): Promise<void> {     
        const res = await axiod.post("https://socket.ad4m.dev/setAgentStatus", {
            did: this.me,
            status: status,
            linkLanguageUUID: this.uuid
        });
        if (res.status === 200) {
            console.log("setOnlineStatus: success");
            console.log(res.data);
        } else {
            //console.log("setOnlineStatus: failed");
            //console.log(res.data);
        }
        return null;
    }

    async getOnlineAgents(): Promise<OnlineAgent[]> {
        const result = await axiod.get("https://socket.ad4m.dev/getOnlineAgents", {
            params: {
                did: this.me,
                linkLanguageUUID: this.uuid
            }
        });
        if (result.status === 200) {
            //("getOnlineAgents: success");
            //console.dir(result.data);
            return result.data;
        } else {
            //console.log("getOnlineAgents: failed");
            //console.dir(result.data);
            return [];
        }
    }

    async sendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object> {
        this.socketClient.emit("send-signal", { remoteAgentDid, linkLanguageUUID: this.uuid, payload }, (err, signal) => {
            if (err) {
                console.log("sendSignal: failed");
                console.dir(err);
            } else {
                //console.log("sendSignal: success");
                //console.dir(signal);
            }
        });
        return {};
    }

    async sendBroadcast(payload: PerspectiveExpression): Promise<object> {
        this.socketClient.emit("send-broadcast", { linkLanguageUUID: this.uuid, payload }, (err, signal) => {
            if (err) {
                console.log("sendBroadcast: failed");
                console.dir(err);
            }// else {
            //    console.log("sendBroadcast: success");
            //    console.dir(signal);
            //}
        });

        return {};
    }

    async registerSignalCallback(callback: TelepresenceSignalCallback): Promise<void> {
        this.signalCallbacks.push(callback);
    }
}