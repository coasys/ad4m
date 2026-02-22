import {
    perspective_diff_received, sync_state_changed, telepresence_signal_received,
    register_holochain_signal_handler, load_system_languages, ad4m_signal_emitted
} from 'ext:core/ops';

((globalThis) => {
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
        loadSystemLanguages: (languageLanguageOnly) => {
            return load_system_languages(languageLanguageOnly);
        },
    };
  })(globalThis);
