((globalThis) => {
    const core = Deno.core;

    globalThis.LANGUAGE_CONTROLLER = {
        perspectiveDiffReceived: (diff, language_address) => {
            return core.ops.perspective_diff_received(diff, language_address);
        },
        syncStateChanged: (syncState, language_address) => {
            return core.ops.sync_state_changed(syncState, language_address);
        },
        telepresenceSignalReceived: (signal, language_address) => {
            return core.ops.telepresence_signal_received(signal, language_address);
        },
    };
  })(globalThis);
  