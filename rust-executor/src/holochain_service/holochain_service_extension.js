((globalThis) => {
    const core = Deno.core;

    globalThis.HOLOCHAIN_SERVICE = {
        startHolochainConductor: async (config) => {
            return core.opAsync("start_holochain_conductor", config);
        },
        logDhtStatus: async () => {
            return core.opAsync("log_dht_status");
        },
        installApp: async (install_app_payload) => {
            return core.opAsync("install_app", install_app_payload);
        },
        getAppInfo: async (app_id) => {
            return core.opAsync("get_app_info", app_id);
        },
        callZomeFunction: async (app_id, cell_name, zome_name, fn_name, payload) => {
            return core.opAsync("call_zome_function", app_id, cell_name, zome_name, fn_name, payload);
        },
        agentInfos: async () => {
            return core.opAsync("agent_infos");
        },
        addAgentInfos: async (agent_infos) => {
            return core.opAsync("add_agent_infos", agent_infos);
        },
        removeApp: async (app_id) => {
            return core.opAsync("remove_app", app_id);
        },
        signString: async (string) => {
            return core.opAsync("sign_string", string);
        },
        shutdown: async () => {
            return core.opAsync("shutdown")
        },
        getAgentKey: async () => {
            return core.opAsync("get_agent_key")
        },
        packDna: async (path) => {
            return core.opAsync("pack_dna", path)
        },
        UnPackDna: async (path) => {
            return core.opAsync("unpack_dna", path)
        }
    };
  })(globalThis);
  