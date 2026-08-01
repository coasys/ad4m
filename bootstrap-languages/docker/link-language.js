// Docker link-language — local link storage with sync/commit/render.
//
// Single-instance link language for Docker deployments. Stores all
// links in memory (backed by ad4m:host storageGet/storagePut for
// persistence across restarts). No peer sync — a Docker container
// runs as a standalone node.
import {
    agentDid,
    storageGet,
    storagePut,
    emitPerspectiveDiff,
} from "ad4m:host";

export const name = "docker-link-language";
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
    loadState();
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

export async function perspectiveCommit(diff) {
    const additions = diff.additions || [];
    const removals = diff.removals || [];

    // Apply removals
    if (removals.length > 0) {
        const removalKeys = new Set(
            removals.map(function(l) {
                return (l.data.source || "") + "|" + (l.data.predicate || "") + "|" + (l.data.target || "");
            })
        );
        links = links.filter(function(l) {
            var key = (l.data.source || "") + "|" + (l.data.predicate || "") + "|" + (l.data.target || "");
            return !removalKeys.has(key);
        });
    }

    // Apply additions
    for (var i = 0; i < additions.length; i++) {
        links.push(additions[i]);
    }

    revision = revision + 1;
    saveState();
    return "";
}

// -- peers capability --

export async function peersSetLocal(_agents) {}
export async function peersRemote() { return []; }
