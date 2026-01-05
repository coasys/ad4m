import {
    agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_sign,
    agent_sign_string_hex, agent_load, agent, agent_is_initialized, agent_is_unlocked, agent_unlock,
    agent_lock, agent_create_signed_expression_stringified, agent_create_signed_expression_for_user,
    agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user, save_agent_profile
} from 'ext:core/ops';

((globalThis) => {
    const core = Deno.core;

    globalThis.AGENT = {
        didDocument: () => {
            return agent_did_document();
        },
        signingKeyId: () => {
            return agent_signing_key_id();
        },
        did: () => {
            return agent_did();
        },
        createSignedExpression: (data) => {
            try {
                return agent_create_signed_expression(data);
            } catch (error) {
                console.error("Error calling agent_create_signed_expression:", error);
                console.error("Data was:", data)
                console.log("Falling back to stringified version")
                try {
                    let stringified = JSON.stringify(data);
                    let resultString = agent_create_signed_expression_stringified(stringified)
                    let result = JSON.parse(resultString);
                    return result;
                } catch (error) {
                    console.error("Error calling agent_create_signed_expression_stringified:", error);
                    console.error("Data was:", JSON.stringify(data))
                }
            }
        },
        sign: (payload) => {
            return agent_sign(payload);
        },
        signStringHex: (payload) => {
            return agent_sign_string_hex(payload);
        },
        load: () => {
            return agent_load();
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
        unlock: (password) => {
            return agent_unlock(password);
        },
        lock: () => {
            return agent_lock();
        },
        save_agent_profile: (profile) => {
            return save_agent_profile(profile);
        },
        createSignedExpressionForUser: (userEmail, data) => {
            if (typeof userEmail !== 'string' || userEmail.trim() === '') {
                throw new TypeError('userEmail must be a non-empty string');
            }
            try {
                return agent_create_signed_expression_for_user(userEmail, data);
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