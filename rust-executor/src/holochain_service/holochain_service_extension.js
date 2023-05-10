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
        call_zome_function: async (app_id, cell_name, zome_name, fn_name, payload) => {
            return core.opAsync("call_zome_function", app_id, cell_name, zome_name, fn_name, payload);
        }
    };
  })(globalThis);
  