import {
    perspective_diff_received, sync_state_changed, telepresence_signal_received,
    register_holochain_signal_handler, ad4m_signal_emitted,
    language_storage_directory, language_address, language_settings
} from 'ext:core/ops';

console.log("[languages_extension] ops loaded:", {
    language_storage_directory: !!language_storage_directory,
    language_address: !!language_address,
    language_settings: !!language_settings
});

((globalThis) => {
    // Test the ops immediately
    try {
        const testDir = language_storage_directory();
        console.log("[languages_extension] Test language_storage_directory():", testDir);
    } catch (e) {
        console.log("[languages_extension] Test language_storage_directory() error (expected if not set yet):", e.message);
    }
    
    globalThis.LANGUAGE_CONTROLLER = {
        perspectiveDiffReceived: (diff, language_address) => {
            return perspective_diff_received(diff, language_address);
        },
        syncStateChanged: (syncState, language_address) => {
            return sync_state_changed(syncState, language_address);
        },
        telepresenceSignalReceived: (signal, language_address, recipientDid) => {
            return telepresence_signal_received(signal, language_address, recipientDid);
        },
        ad4mSignalEmitted: (signal, language_address) => {
            return ad4m_signal_emitted(signal, language_address);
        },
        registerHolochainSignalHandler: (cellIdKey, language_address) => {
            return register_holochain_signal_handler(cellIdKey, language_address);
        },
        // Language context globals for flat pattern languages
        languageStorageDirectory: () => {
            try {
                return language_storage_directory();
            } catch (e) {
                console.error("[LANGUAGE_CONTROLLER.languageStorageDirectory] Error:", e.message);
                throw e;
            }
        },
        languageAddress: () => {
            try {
                return language_address();
            } catch (e) {
                console.error("[LANGUAGE_CONTROLLER.languageAddress] Error:", e.message);
                throw e;
            }
        },
        languageSettings: () => {
            try {
                return language_settings();
            } catch (e) {
                console.error("[LANGUAGE_CONTROLLER.languageSettings] Error:", e.message);
                throw e;
            }
        },
    };
    console.log("[languages_extension] LANGUAGE_CONTROLLER initialized with languageStorageDirectory:", typeof globalThis.LANGUAGE_CONTROLLER.languageStorageDirectory);
})(globalThis);