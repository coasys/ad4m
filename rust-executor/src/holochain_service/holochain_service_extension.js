import {
    start_holochain_conductor, log_dht_status, install_app, get_app_info,
    call_zome_function, agent_infos, add_agent_infos, remove_app,
    shutdown, get_agent_key, get_agent_key_for_language,
    pack_dna, unpack_dna,
    pack_happ, unpack_happ,
} from 'ext:core/ops';

((globalThis) => {
    const core = Deno.core;

    // Recursively convert {__binary: [...]} markers from Rust into Uint8Array
    function convertBinaryMarkers(val) {
        if (val === null || val === undefined) return val;
        if (Array.isArray(val)) {
            return val.map(convertBinaryMarkers);
        }
        if (typeof val === 'object') {
            // Check for __binary marker: { __binary: [byte, byte, ...] }
            const keys = Object.keys(val);
            if (keys.length === 1 && keys[0] === '__binary' && Array.isArray(val.__binary)) {
                return new Uint8Array(val.__binary);
            }
            // Recursively convert nested objects
            const result = {};
            for (const key of keys) {
                result[key] = convertBinaryMarkers(val[key]);
            }
            return result;
        }
        return val;
    }

    // Recursively convert Uint8Array/Buffer instances to {__binary: [...]} markers
    // so the Rust side can re-encode them as msgpack Binary
    function convertUint8Arrays(val) {
        if (val === null || val === undefined) return val;
        if (val instanceof Uint8Array || (typeof Buffer !== 'undefined' && Buffer.isBuffer(val))) {
            return { __binary: Array.from(val) };
        }
        if (Array.isArray(val)) {
            return val.map(convertUint8Arrays);
        }
        if (typeof val === 'object') {
            const result = {};
            for (const key of Object.keys(val)) {
                result[key] = convertUint8Arrays(val[key]);
            }
            return result;
        }
        return val;
    }

    globalThis.HOLOCHAIN_SERVICE = {
        startHolochainConductor: async (config) => {
            return start_holochain_conductor(config);
        },
        logDhtStatus: async () => {
            return log_dht_status();
        },
        installApp: async (install_app_payload) => {
            return install_app(install_app_payload);
        },
        getAppInfo: async (app_id) => {
            return get_app_info(app_id);
        },
        callZomeFunction: async (app_id, cell_name, zome_name, fn_name, payload) => {
            // Convert any Uint8Array/Buffer in payload to __binary markers for Rust
            const convertedPayload = convertUint8Arrays(payload);
            const response = await call_zome_function(app_id, cell_name, zome_name, fn_name, convertedPayload);
            // Unwrap DecodedZomeCallResponse: { type: "Ok", value: ... }
            if (response.type === "Ok") {
                // Convert __binary markers back to Uint8Array
                return convertBinaryMarkers(response.value);
            } else if (response.type === "NetworkError") {
                throw new Error(`Holochain NetworkError: ${response.value}`);
            } else if (response.type === "CountersigningSession") {
                throw new Error(`Holochain CountersigningSession error: ${response.value}`);
            } else {
                throw new Error(`Unexpected ZomeCallResponse: ${JSON.stringify(response)}`);
            }
        },
        agentInfos: async () => {
            return agent_infos();
        },
        addAgentInfos: async (agent_infos) => {
            return add_agent_infos(agent_infos);
        },
        removeApp: async (app_id) => {
            return remove_app(app_id);
        },
        shutdown: async () => {
            return shutdown()
        },
        // The node's first lair key, not the calling language's own agent key (that's
        // getAgentKeyForLanguage below, added for issue #1099). No in-tree caller; kept
        // for third-party languages that may already call it.
        getAgentKey: async () => {
            return get_agent_key()
        },
        // Per-language agent key (issue #1099) — distinct languages get
        // distinct cells even when they bundle the same DNA + network seed.
        // No languageAddress argument: the op reads the real one from the
        // isolate's own state, since this global is reachable from any
        // language's code and a caller-supplied address could bind another
        // language's mapping.
        getAgentKeyForLanguage: async (appId) => {
            return get_agent_key_for_language(appId)
        },
        packDna: async (path) => {
            return pack_dna(path)
        },
        unPackDna: async (path) => {
            return unpack_dna(path)
        },
        packHapp: async (path) => {
            return pack_happ(path)
        },
        unPackHapp: async (path) => {
            return unpack_happ(path)
        }
    };
  })(globalThis);
