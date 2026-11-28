// Local link-language — local link storage with sync/commit/render.
//
// Single-instance link language for local/standalone deployments.
// Stores all links in memory (backed by ad4m:host storageGet/storagePut
// for persistence across restarts). No peer sync — the node runs
// standalone.
import {
    agentDid,
    storageGet,
    storagePut,
    emitPerspectiveDiff,
} from "ad4m:host";

export const name = "local-link-language";
export const version = "0.1.0";

// Template variable: applyTemplateAndPublish({uid}) rewrites the line below,
// so each neighbourhood gets its own copy of this language, with its own
// address and its own storage. Without it every template would hash to this
// language's address and all neighbourhoods on a node would share one link
// store.
//!@ad4m-template-variable
const uid = "local-link-language";

let links = [];
let revision = 0;
let synced = false;

function loadState() {
    try {
        const raw = storageGet("link-state");
        if (raw) {
            const state = JSON.parse(raw);
            links = state.links || [];
            revision = state.revision || 0;
        }
    } catch (e) {
        console.error("[local-link-language] Failed to parse link-state, starting empty:", e);
        links = [];
        revision = 0;
    }
}

function saveState() {
    storagePut("link-state", JSON.stringify({ links: links, revision: revision }));
}

export async function init() {
    loadState();
}

export function interactions() { return []; }
export async function teardown() { saveState(); }

// -- sync capability --

export async function perspectiveSyncSync() {
    if (synced) return null;
    synced = true;
    if (links.length > 0) {
        emitPerspectiveDiff({
            additions: links,
            removals: [],
        });
    }
    return null;
}

export async function perspectiveSyncRender() {
    return { links: links };
}

export async function perspectiveSyncCurrentRevision() {
    return revision.toString();
}

// The links of `asked` this store lacks. Never null: this store is the room.
// Compares by the capability's identity, where "" and no predicate are the
// same: the executor reads an empty predicate back as none. One Set of the
// whole store per call: a linear scan, not an index.
export async function perspectiveSyncMissing(asked) {
    const held = new Set(links.map(memberKey));
    return asked.filter(function(l) { return !held.has(memberKey(l)); });
}

function memberKey(l) {
    const data = l.data || {};
    return JSON.stringify([
        l.author || "",
        l.timestamp || "",
        data.source || "",
        data.predicate || "",
        data.target || "",
    ]);
}

// -- commit capability --

function linkKey(l) {
    return JSON.stringify({
        author: l.author || "",
        timestamp: l.timestamp || "",
        data: l.data || {},
    });
}

export async function perspectiveCommit(diff) {
    const additions = diff.additions || [];
    const removals = diff.removals || [];

    if (removals.length > 0) {
        const removalKeys = new Set(removals.map(linkKey));
        links = links.filter(function(l) {
            return !removalKeys.has(linkKey(l));
        });
    }

    const existing = new Set(links.map(linkKey));
    for (var i = 0; i < additions.length; i++) {
        var key = linkKey(additions[i]);
        if (!existing.has(key)) {
            existing.add(key);
            links.push(additions[i]);
        }
    }

    revision = revision + 1;
    saveState();
    return "";
}

// -- peers capability --

export async function peersSetLocal(_agents) {}
export async function peersRemote() { return []; }
