// Blocking link language: the local link language
// (bootstrap-languages/local/link-language.js) plus telepresence, and calls
// that are slow on demand. Ported from the rehearsal soak
// (rehearsal/calls-1120, helpers/sfu/appload/soak-link-language.js).
//
// One instance serves every user of the executor, as a hosted space's link
// language does. Its calls take the time a networked language's take (a few
// tens of ms), and a commit that carries a link with the predicate
// `soak://slow` sleeps for the milliseconds in that link's target: the
// stand-in for the 10-30 s link-language calls seen on staging (a
// key-grant job, a large commit).
import {
    storageGet,
    storagePut,
    emitPerspectiveDiff,
} from "ad4m:host";

export const name = "blocking-link-language";
export const version = "0.1.0";

// The test writes a copy of this file per run with its own uid, so each
// run's language has its own address and storage.
//!@ad4m-template-variable
const uid = "blocking-link-language";

/** What an ordinary call costs: a round trip to a peer or a server. */
const CALL_MS = 30;
const SLOW_PREDICATE = "soak://slow";

let links = [];
let revision = 0;
let synced = false;
let onlineStatus = null;
const calls = { commit: 0, slow: 0, broadcast: 0, signal: 0, status: 0, online: 0 };

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

function loadState() {
    try {
        const raw = storageGet("link-state");
        if (raw) {
            const state = JSON.parse(raw);
            links = state.links || [];
            revision = state.revision || 0;
        }
    } catch (e) {
        console.error("[blocking-link-language] Failed to parse link-state, starting empty:", e);
        links = [];
        revision = 0;
    }
}

export async function init() {
    loadState();
}

export function interactions() { return []; }
export async function teardown() {
    storagePut("link-state", JSON.stringify({ links: links, revision: revision }));
}

// -- sync capability --

export async function perspectiveSyncSync() {
    await sleep(CALL_MS);
    if (synced) return null;
    synced = true;
    if (links.length > 0) emitPerspectiveDiff({ additions: links, removals: [] });
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
    return JSON.stringify({ author: l.author || "", timestamp: l.timestamp || "", data: l.data || {} });
}

/** The longest sleep a `soak://slow` link in `additions` asks for, in ms; 0 with none. */
function slowMs(additions) {
    let ms = 0;
    for (const l of additions) {
        if (l.data && l.data.predicate === SLOW_PREDICATE) {
            const asked = Number(String(l.data.target).replace(/^.*:/, ""));
            if (Number.isFinite(asked)) ms = Math.max(ms, asked);
        }
    }
    return ms;
}

export async function perspectiveCommit(diff) {
    const additions = diff.additions || [];
    const removals = diff.removals || [];
    calls.commit++;
    const slow = slowMs(additions);
    if (slow > 0) {
        calls.slow++;
        console.log("[blocking-link-language] slow commit: sleeping " + slow + " ms");
    }
    await sleep(slow > 0 ? slow : CALL_MS);

    if (removals.length > 0) {
        const removalKeys = new Set(removals.map(linkKey));
        links = links.filter(function (l) { return !removalKeys.has(linkKey(l)); });
    }
    const existing = new Set(links.map(linkKey));
    for (const addition of additions) {
        const key = linkKey(addition);
        if (!existing.has(key)) {
            existing.add(key);
            links.push(addition);
        }
    }
    revision = revision + 1;
    // The local link language rewrites its whole state on every commit; with
    // thousands of links that is the cost of the storage, not of the call.
    // Keep it to every 50th.
    if (revision % 50 === 0) storagePut("link-state", JSON.stringify({ links: links, revision: revision }));
    return "";
}

// -- peers capability --

export async function peersSetLocal(_agents) {}
export async function peersRemote() { return []; }

// -- telepresence capability --
// One executor serves every member, and it delivers their signals itself:
// nothing goes out from here. Each call still takes the time of a send.

export async function telepresenceSetOnlineStatus(status) {
    calls.status++;
    await sleep(CALL_MS);
    onlineStatus = status;
    return null;
}

export async function telepresenceGetOnlineAgents() {
    calls.online++;
    await sleep(CALL_MS);
    return [];
}

export async function telepresenceSendSignal(_remoteAgentDid, _payload) {
    calls.signal++;
    await sleep(CALL_MS);
    return {};
}

export async function telepresenceSendBroadcast(_payload) {
    calls.broadcast++;
    await sleep(CALL_MS);
    return {};
}

export async function telepresenceRegisterSignalCallback(_callback) {}
