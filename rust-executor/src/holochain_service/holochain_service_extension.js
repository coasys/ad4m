((globalThis) => {
    const core = Deno.core;

    globalThis.HOLOCHAIN_SERVICE = {
        start_holochain_conductor: async (config) => {
            return core.opAsync("start_holochain_conductor", config);
        },
        log_dht_status: async () => {
            return core.opAsync("log_dht_status");
        }
    };
  })(globalThis);
  