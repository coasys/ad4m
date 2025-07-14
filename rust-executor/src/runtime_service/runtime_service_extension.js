import {
    friends, add_message_outbox, get_trusted_agents, add_debug_string, get_debug_strings
} from 'ext:core/ops';

((globalThis) => {
    globalThis.RUNTIME_SERVICE = {
        friends: async () => {
            return friends();
        },
        addMessageOutbox: async (did, message, wasSent) => {
            return add_message_outbox(did, message, wasSent);
        },
        getTrustedAgents: async () => {
            return get_trusted_agents();
        },
        addDebugString: async (languageAddress, debugString, operation) => {
            return add_debug_string(languageAddress, debugString, operation);
        },
        getDebugStrings: async (languageAddress) => {
            return get_debug_strings(languageAddress);
        },
    }
})(globalThis);