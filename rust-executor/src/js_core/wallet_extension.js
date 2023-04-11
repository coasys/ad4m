((globalThis) => {
    const core = Deno.core;

    globalThis.WALLET = {
        getMainKey: () => {
            return core.opAsync("wallet_get_main_key");
        },
        getMainKeyDocument: () => {
            return core.opAsync("wallet_get_main_key_document");
        },
        createMainKey: () => {
            return core.opAsync("wallet_create_main_key");
        },
        isUnlocked: () => {
            return core.opAsync("wallet_is_unlocked", path);
        },
        unlock: (password) => {
            return core.opAsync("wallet_unlock", password);
        },
        lock: (password) => {
            return core.opAsync("wallet_lock", password);
        },
        export: () => {
            return core.opAsync("wallet_export");
        },
        load: (data) => {
            return core.opAsync("wallet_load", data);
        },
    };
  })(globalThis);
  