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
            try {
                return core.ops.agent_create_signed_expression(data);
            } catch (error) {
                console.error("Error calling agent_create_signed_expression:", error);
                console.error("Data was:", data)
                console.log("Falling back to stringified version")
                try {
                    let stringified = JSON.stringify(data);
                    let resultString = core.ops.agent_create_signed_expression_stringified(stringified)
                    let result = JSON.parse(resultString);
                    return result;
                } catch (error) {
                    console.error("Error calling agent_create_signed_expression_stringified:", error);
                    console.error("Data was:", JSON.stringify(data))
                }
            }
        },
        sign: (payload) => {
            return core.ops.agent_sign(payload);
        },
        signStringHex: (payload) => {
            return core.ops.agent_sign_string_hex(payload);
        },
    };
})(globalThis);