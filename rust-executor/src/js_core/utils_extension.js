((globalThis) => {
    const core = Deno.core;

    globalThis.UTILS = {
        hash: (data) => {
            return core.ops.hash(data);
        },
        loadModule: async (path) => {
            return core.opAsync("load_module", path);
        }
    };
  })(globalThis);
  