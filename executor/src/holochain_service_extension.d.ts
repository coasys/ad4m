import { AgentInfoSigned, AgentPubKey, AppInfo, CallZomeResponse, InstallAppRequest, Signature } from "./holochain_types";
declare global {
    interface ConductorConfig {
        passphrase: String,
        conductorPath: String,
        dataPath: String,
        useBootstrap: bool,
        useProxy: bool,
        useLocalProxy: bool,
        useMdns: bool,
        proxyUrl: String,
        bootstrapUrl: String,
        relayUrl?: String,
        appPort: Number
    }

    interface HolochainService {
        async startHolochainConductor: (config: ConductorConfig) => void;
        async logDhtStatus: () => void;
        async installApp: (install_app_payload: InstallAppRequest) => AppInfo;
        async getAppInfo: (app_id: String) => AppInfo | null;
        async callZomeFunction: (app_id: String, cell_name: String, zome_name: String, fn_name: String, payload: any) => CallZomeResponse;
        async agentInfos: () => AgentInfoSigned[];
        async addAgentInfos: (agent_infos: AgentInfoSigned[]) => void;
        async removeApp: (app_id: String) => void;
        async signString: (string: String) => Signature;
        async shutdown: () => void;
        async getAgentKey: () => AgentPubKey;
        async packDna: (path: String) => String;
        async unPackDna: (path: String) => String;
        async packHapp: (path: String) => String;
        async unPackHapp: (path: String) => String;
    }

    const HOLOCHAIN_SERVICE: HolochainService;
}

export {};