((globalThis) => {
    const core = Deno.core;

    globalThis.RUNTIME_SERVICE = {
        friends: async () => {
            return core.ops.friends();
        },
        addMessageOutbox: async (did, message, wasSent) => {
            return core.ops.add_message_outbox(did, message, wasSent);
        },
        getTrustedAgents: async () => {
            return core.ops.get_trusted_agents();
        },
    }
})(globalThis);