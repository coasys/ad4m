((globalThis) => {
    const core = Deno.core;

    globalThis.AGENT = {
        didDocument: () => {
            return core.ops.agent_did_document();
        },
        signingKeyId: () => {
            return core.ops.agent_signing_key_id();
        },
        did: () => {
            return core.ops.agent_did();
        },
        createSignedExpression: (data) => {
            return core.ops.agent_create_signed_expression(data);
        },
        sign: (payload) => {
            return core.ops.agent_sign(payload);
        },
    };
})(globalThis);