((globalThis) => {
    const core = Deno.core;

    globalThis.WALLET = {
        getMainKey: () => {
            return core.ops.wallet_get_main_key();
        },
        getMainKeyDocument: () => {
            return core.ops.wallet_get_main_key_document();
        },
        createMainKey: () => {
            return core.ops.wallet_create_main_key();
        },
        isUnlocked: () => {
            return core.ops.wallet_is_unlocked();
        },
        unlock: (password) => {
            return core.ops.wallet_unlock(password);
        },
        lock: (password) => {
            return core.ops.wallet_lock(password);
        },
        export: (password) => {
            return core.ops.wallet_export(password);
        },
        load: (data) => {
            return core.ops.wallet_load(data);
        },
        sign: (payload) => {
            return core.ops.wallet_sign(payload);
        },
    };
  })(globalThis);
  