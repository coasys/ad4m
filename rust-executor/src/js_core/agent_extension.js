import {
    agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_sign,
    agent_sign_string_hex, agent, agent_is_initialized, agent_is_unlocked,
    agent_create_signed_expression_stringified, agent_create_signed_expression_for_user,
    agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user,
    agent_open_sealed, agent_call_token
} from 'ext:core/ops';

((globalThis) => {
    const core = Deno.core;

    // The agent of the call this code runs for, kept in a V8 async-context
    // variable: agent_extension.rs, "The agent a call acts as".
    const callAgent = new core.AsyncVariable();
    const caller = () => callAgent.get() ?? null;

    // A WASM export's future reads the calling call's token once, at export
    // entry where the ambient context is still the call's own
    // (ad4m-ldk `agent::export_future`), and passes it on every agent import
    // afterwards: wasm-bindgen-futures polls futures under whichever context
    // scheduled the drain, not under the caller's. `undefined` from a caller
    // means "no pinned token" (old bundles, JS languages, code outside an
    // export's poll) and the ambient context answers instead; `null` means
    // the pinned call has no agent and the op must refuse.
    const token = (call) => (call === undefined ? caller() : call);
    Object.defineProperty(globalThis, "__ad4mCallAgentToken", {
        value: () => callAgent.get() ?? null,
        writable: false,
        enumerable: false,
        configurable: false,
    });

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

    // The host's module evaluation is driven from Rust (`JsCore::load_module_from_source`),
    // not a script call: `__ad4mEnterCallAgent` takes the call's token into the
    // async context the way `__ad4mRunCall` does, and `__ad4mLeaveCallAgent`
    // restores the context it replaced. Evaluations do not nest: one module
    // evaluates at a time, under the runtime's worker lock.
    let replaced;
    Object.defineProperty(globalThis, "__ad4mEnterCallAgent", {
        value: (id) => {
            replaced = callAgent.enter(agent_call_token(id));
        },
        writable: false,
        enumerable: false,
        configurable: false,
    });
    Object.defineProperty(globalThis, "__ad4mLeaveCallAgent", {
        value: () => {
            core.setAsyncContext(replaced);
            replaced = undefined;
        },
        writable: false,
        enumerable: false,
        configurable: false,
    });

    globalThis.AGENT = {
        didDocument: () => {
            return agent_did_document();
        },
        signingKeyId: (call) => {
            return agent_signing_key_id(token(call));
        },
        did: (call) => {
            return agent_did(token(call));
        },
        createSignedExpression: (data, call) => {
            try {
                return agent_create_signed_expression(data, token(call));
            } catch (error) {
                console.error("Error calling agent_create_signed_expression:", error);
                console.error("Data was:", data)
                console.log("Falling back to stringified version")
                try {
                    let stringified = JSON.stringify(data);
                    let resultString = agent_create_signed_expression_stringified(stringified, token(call))
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
        sign: (payload, call) => {
            return agent_sign(payload, token(call));
        },
        signStringHex: (payload, call) => {
            return agent_sign_string_hex(payload, token(call));
        },
        openSealed: (sealed, call) => {
            return agent_open_sealed(sealed, token(call));
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
        createSignedExpressionForUser: (userEmail, data, call) => {
            if (typeof userEmail !== 'string' || userEmail.trim() === '') {
                throw new TypeError('userEmail must be a non-empty string');
            }
            try {
                return agent_create_signed_expression_for_user(userEmail, data, token(call));
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