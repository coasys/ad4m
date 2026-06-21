import {
    holograph_create_neighborhood,
    holograph_commit,
    holograph_render,
    holograph_next_emitted,
    holograph_join_agent,
    holograph_current_revision,
    holograph_latest_revision,
    holograph_close_neighborhood,
} from "ext:core/ops";

((globalThis) => {
    // Mirror of HOLOCHAIN_SERVICE: thin async wrappers around the
    // op2(async) entry points exposed by holograph_service_extension.rs.
    // language_bootstrap.js builds the per-language
    // __holographDelegate__ from this surface.
    globalThis.HOLOGRAPH_SERVICE = {
        createNeighborhood: async (spaceId, storageDir) => {
            return Number(await holograph_create_neighborhood(spaceId, storageDir));
        },
        commit: async (handle, diff) => {
            // diff: { additions: any[], removals: any[] }
            return await holograph_commit(BigInt(handle), diff);
        },
        render: async (handle) => {
            return await holograph_render(BigInt(handle));
        },
        nextEmitted: async (handle) => {
            const v = await holograph_next_emitted(BigInt(handle));
            return v == null ? null : v;
        },
        joinAgent: async (handle, agentKeyB64) => {
            return await holograph_join_agent(BigInt(handle), agentKeyB64);
        },
        currentRevision: async (handle) => {
            // Rust side returns "" for None -- convert back to null so
            // the AD4M spec's `Promise<string | null>` contract holds.
            const s = await holograph_current_revision(BigInt(handle));
            return s === "" ? null : s;
        },
        latestRevision: async (handle) => {
            const s = await holograph_latest_revision(BigInt(handle));
            return s === "" ? null : s;
        },
        closeNeighborhood: async (handle) => {
            return await holograph_close_neighborhood(BigInt(handle));
        },
    };
})(globalThis);
