//import type HolochainLanguageDelegate from "../core/storage-services/Holochain/HolochainLanguageDelegate";
import type AgentService from "./AgentService";
import type SignaturesService from "./SignaturesService";

export default interface LanguageContext {
    agent: AgentService;
    IPFS: IPFSNode;
    signatures: SignaturesService;
    storageDirectory: string;
    customSettings: object;
    //Holochain: HolochainLanguageDelegate | void;
}

export interface IPFSNode {
    add(data: object): Promise<object>
    cat(data: object): Promise<object>
}