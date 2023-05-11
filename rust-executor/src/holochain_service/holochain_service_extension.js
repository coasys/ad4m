((globalThis) => {
    const core = Deno.core;

    globalThis.HOLOCHAIN_SERVICE = {
        start_holochain_conductor: async (config) => {
            return core.opAsync("start_holochain_conductor", config);
        },
        log_dht_status: async () => {
            return core.opAsync("log_dht_status");
        },
        install_app: async (install_app_payload) => {
            return core.opAsync("install_app", install_app_payload);
        },
        get_app_info: async (app_id) => {
            return core.opAsync("get_app_info", app_id);
        },
        call_zome_function: async (app_id, cell_name, zome_name, fn_name, payload) => {
            return core.opAsync("call_zome_function", app_id, cell_name, zome_name, fn_name, payload);
        },
        agent_infos: async () => {
            return core.opAsync("agent_infos");
        },
        add_agent_infos: async (agent_infos) => {
            return core.opAsync("add_agent_infos", agent_info);
        },
        remove_app: async (app_id) => {
            return core.opAsync("remove_app", app_id);
        },
        sign_string: async (string) => {
            return core.opAsync("sign_string", string);
        },
        shutdown: async () => {
            return core.opAsync("shutdown")
        },
        get_agent_key: async () => {
            return core.opAsync("get_agent_key")
        }
    };
  })(globalThis);
  