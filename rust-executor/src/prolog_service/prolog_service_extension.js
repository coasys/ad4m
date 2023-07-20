((globalThis) => {
    const core = Deno.core;

    globalThis.PROLOG_SERVICE = {
        initPrologService: async () => {
            return core.opAsync("init_prolog_service");
        },
        spawnEngine: async () => {
            return core.opAsync("spawn_engine");
        },
        runQuery: async (engineName, query) => {
            return core.opAsync("run_query"), engineName, query;
        },
        loadModuleString: async (engineName, module_name, program) => {
            return core.opAsync("load_module_string", engineName, module_name, program);
        }
    };
  })(globalThis);
  