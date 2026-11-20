// note-store: an expression Language that stores signed notes on this machine.
//
// `create` gets the agent to sign the content, stores the signed expression under
// its content hash and returns the hash as the expression's address. `get` reads
// it back. Storage is the executor's per-Language key-value store, so nothing
// leaves the machine.
import {
    defineLanguage,
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "@coasys/ad4m-ldk";

export default defineLanguage({
    name: "note-store",
    version: "1.0.0",

    async init() {},

    expression: {
        async create(content) {
            const expression = agentCreateSignedExpression(content);
            const serialized = JSON.stringify(expression);
            const address = hash(serialized);
            storagePut(address, serialized);
            return address;
        },

        async get(address) {
            const stored = storageGet(address);
            return stored ? JSON.parse(stored) : null;
        },
    },
});
