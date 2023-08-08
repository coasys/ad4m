((globalThis) => {
    const core = Deno.core;

    globalThis.UTILS = {
        hash: (data) => {
            return core.ops.hash(data);
        },
        loadModule: async (path) => {
            return core.opAsync("load_module", path);
        },
        consoleLog: (args) => {
            return core.ops.console_log(args);
        },
        consoleDebug: (args) => {
            return core.ops.console_debug(args);
        },
        consoleError: (args) => {
            return core.ops.console_error(args);
        },
        consoleWarn: (args) => {
            return core.ops.console_warn(args);
        }
    };
  })(globalThis);
