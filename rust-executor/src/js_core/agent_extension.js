import {
    agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_sign,
    agent_sign_string_hex, agent, agent_is_initialized, agent_is_unlocked,
    agent_create_signed_expression_stringified, agent_create_signed_expression_for_user,
    agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user,
    agent_open_sealed, agent_call_token
} from 'ext:core/ops';

((globalThis) => {
    const core = Deno.core;

    // The agent of the call this code runs for: a token from `agent_call_token`, kept in a
    // V8 async-context variable, so every await, promise reaction and timer of the call
    // keeps it while other calls into this language run (agent_extension.rs, "The agent a
    // call acts as"). Outside any call it is null and the ops act as the main agent.
    const callAgent = new core.AsyncVariable();
    const caller = () => callAgent.get() ?? null;

    // The runtime runs every script call as `__ad4mRunCall(id, async () => (script))`.
    Object.defineProperty(globalThis, "__ad4mRunCall", {
        value: (id, fn) => {
            const token = agent_call_token(id);
            const previous = callAgent.enter(token);
            try {
                return fn();
            } finally {
                core.setAsyncContext(previous);
            }
        },
        writable: false,
        enumerable: false,
        configurable: false,
    });

    globalThis.AGENT = {
        didDocument: () => {
            return agent_did_document();
        },
        signingKeyId: () => {
            return agent_signing_key_id(caller());
        },
        did: () => {
            return agent_did(caller());
        },
        createSignedExpression: (data) => {
            try {
                return agent_create_signed_expression(data, caller());
            } catch (error) {
                console.error("Error calling agent_create_signed_expression:", error);
                console.error("Data was:", data)
                console.log("Falling back to stringified version")
                try {
                    let stringified = JSON.stringify(data);
                    let resultString = agent_create_signed_expression_stringified(stringified, caller())
                    let result = JSON.parse(resultString);
                    return result;
                } catch (fallbackError) {
                    // Both paths failed. Previously this swallowed the
                    // error and returned undefined, which cascaded into
                    // cryptic "cannot read properties of undefined" crashes
                    // in every caller that tried to access .data or
                    // .proof on the result. Rethrow so the failure is
                    // visible at the actual signing call site.
                    console.error("Error calling agent_create_signed_expression_stringified:", fallbackError);
                    console.error("Data was:", JSON.stringify(data))
                    throw fallbackError;
                }
            }
        },
        sign: (payload) => {
            return agent_sign(payload, caller());
        },
        signStringHex: (payload) => {
            return agent_sign_string_hex(payload, caller());
        },
        openSealed: (sealed) => {
            return agent_open_sealed(sealed, caller());
        },
        agent: () => {
            return agent();
        },
        isInitialized: () => {
            return agent_is_initialized();
        },
        isUnlocked: () => {
            return agent_is_unlocked();
        },
        createSignedExpressionForUser: (userEmail, data) => {
            if (typeof userEmail !== 'string' || userEmail.trim() === '') {
                throw new TypeError('userEmail must be a non-empty string');
            }
            try {
                return agent_create_signed_expression_for_user(userEmail, data, caller());
            } catch (error) {
                console.error("Error calling agent_create_signed_expression_for_user:", error);
                console.error("userEmail was:", userEmail);
                console.error("Data was:", data);
                throw error;
            }
        },
        didForUser: (userEmail) => {
            if (typeof userEmail !== 'string' || userEmail.trim() === '') {
                throw new TypeError('userEmail must be a non-empty string');
            }
            try {
                return agent_did_for_user(userEmail);
            } catch (error) {
                console.error("Error calling agent_did_for_user:", error);
                console.error("userEmail was:", userEmail);
                throw error;
            }
        },
        agentForUser: (userEmail) => {
            if (typeof userEmail !== 'string' || userEmail.trim() === '') {
                throw new TypeError('userEmail must be a non-empty string');
            }
            try {
                return agent_agent_for_user(userEmail);
            } catch (error) {
                console.error("Error calling agent_agent_for_user:", error);
                console.error("userEmail was:", userEmail);
                throw error;
            }
        },
        listUserEmails: () => {
            try {
                return agent_list_user_emails();
            } catch (error) {
                console.error("Error calling agent_list_user_emails:", error);
                throw error;
            }
        },
        getAllLocalUserDIDs: () => {
            try {
                return agent_get_all_local_user_dids();
            } catch (error) {
                console.error("Error calling agent_get_all_local_user_dids:", error);
                throw error;
            }
        }
    };
})(globalThis);