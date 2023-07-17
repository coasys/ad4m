((globalThis) => {
    const core = Deno.core;

    globalThis.PROLOG_SERVICE = {
        startPrologService: async () => {
            return core.opAsync("start_prolog_service");
        },
        runQuery: async (query) => {
            return core.opAsync("run_query"), query;
        },
        loadModuleString: async (module_name, program) => {
            return core.opAsync("load_module_string", module_name, program);
        }
    };
  })(globalThis);
  