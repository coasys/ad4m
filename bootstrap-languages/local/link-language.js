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
    } catch (_) {
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
