((globalThis) => {
    const core = Deno.core;

    globalThis.LANGUAGE_CONTROLLER = {
        perspectiveDiffReceived: (diff, language_address) => {
            return core.ops.perspectiveDiffReceived(diff, language_address);
        },
        syncStateChanged: (syncState, language_address) => {
            return core.ops.syncStateChanged(syncState, language_address);
        },
        telepresenceSignalReceived: (signal, language_address) => {
            return core.ops.telepresenceSignalReceived(signal, language_address);
        },
    };
  })(globalThis);
  