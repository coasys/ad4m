// Test lifecycle helper for the link-server used by the server-link-language
// integration suites. Starts an identity-store (helpers/identity.ts) unless the
// caller passes one, then spawns `link-server` (protocol 2) from its built
// entrypoint on a free port with a temp data dir, pointed at that store
// (`STORE_URL`, `STORE_HEAD_KEYS`), and waits for /health. No AUTO_ADMIT: a
// key gets into a room only as a member, as on a production server.
//
// Executors that use the server must speak for a person in the same store:
// start them with `handle.store.executorEnv()` and give them a person with
// `givePerson` (helpers/identity.ts).
//
// Modelled on runHcLocalServices in utils.ts — same shape so it slots into
// the existing test before()/after() hooks without new conventions.

import { spawn, ChildProcess } from "child_process";
import fs from "fs-extra";
import os from "os";
import path from "path";
import { fileURLToPath } from "url";
import { getFreePort, getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { IdentityStore, requireIdentityTools } from "../helpers/identity";
import { sleep } from "./utils";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const REPO_ROOT = path.resolve(__dirname, "..", "..", "..");
const LINK_SERVER_ENTRY = path.join(REPO_ROOT, "link-server", "dist", "index.js");

export interface LinkServerHandle {
    url: string;
    port: number;
    dataDir: string;
    process: ChildProcess;
    /** The identity store the server asks whom each key speaks for. */
    store: IdentityStore;
    /** Stops the server, and the store when `startLinkServer` started it. */
    kill: () => Promise<void>;
}

export interface LinkServerOptions {
    /**
     * The identity store whose key logs say which person each key speaks for. Without one,
     * `startLinkServer` starts a store of its own in a temp directory and stops it on `kill`.
     */
    store?: IdentityStore;
    /**
     * How often the server asks the store again about every key it admitted (ms): a key its
     * log revoked loses its sockets (`4005`) at the next refresh. The server's default is 10
     * minutes; a test of revocation sets a few seconds (`REFRESH_INTERVAL_MS`).
     */
    refreshIntervalMs?: number;
    readyTimeoutMs?: number;
}

/**
 * Spawn a link-server (protocol 2) on a free port, with its identity store. Waits up to
 * `readyTimeoutMs` for the server's /health endpoint to respond 200.
 *
 * A room exists only once its creator's first membership commit made it
 * (genesis: the `CREATOR` and `UID` template parameters of the server link
 * language, filled in by `neighbourhood.create`); auth to any other room
 * answers 404. A key is admitted only when the store says it speaks for a
 * person the room's membership links list.
 *
 * Callers should invoke `handle.kill()` in an `after()` hook.
 */
export async function startLinkServer(opts: LinkServerOptions = {}): Promise<LinkServerHandle> {
    const readyTimeoutMs = opts.readyTimeoutMs ?? 15000;
    if (!fs.existsSync(LINK_SERVER_ENTRY)) {
        throw new Error(
            `link-server entry not found at ${LINK_SERVER_ENTRY} — run \`pnpm --filter @coasys/link-server build\` first`,
        );
    }

    let store = opts.store;
    let ownStore: { store: IdentityStore; port: number } | null = null;
    if (!store) {
        requireIdentityTools();
        const [storePort] = await getFreePorts(1);
        registerPorts([storePort]);
        const storeDir = await fs.mkdtemp(path.join(os.tmpdir(), "link-server-store-"));
        try {
            store = await IdentityStore.start(storeDir, storePort);
        } catch (e) {
            deregisterPorts([storePort]);
            throw e;
        }
        ownStore = { store, port: storePort };
    }

    const port = await getFreePort();
    const dataDir = await fs.mkdtemp(path.join(os.tmpdir(), "link-server-test-"));

    const url = `http://127.0.0.1:${port}`;
    // The origin auth is signed for: the URL the executors' server link language is given.
    const args = [LINK_SERVER_ENTRY, "--port", String(port), "--data", dataDir, "--public-origin", url];
    args.push("--store-url", store.url, "--store-head-keys", store.key);
    // Nothing auto-admits: whatever a developer shell or CI sets, the server
    // runs as a production one would (its own NODE_ENV aside).
    const {
        AUTO_ADMIT: _autoAdmit, ALLOW_AUTO_ADMIT_DEV: _allowAutoAdmit, REFRESH_INTERVAL_MS: _refresh, ...env
    } = process.env;
    const proc = spawn(
        process.execPath,
        args,
        {
            env: {
                ...env,
                PORT: String(port),
                DATA_DIR: dataDir,
                ...(opts.refreshIntervalMs ? { REFRESH_INTERVAL_MS: String(opts.refreshIntervalMs) } : {}),
            },
            stdio: ["ignore", "pipe", "pipe"],
        },
    );

    proc.stdout?.on("data", (b) => process.stdout.write(`[link-server:${port}] ${b}`));
    proc.stderr?.on("data", (b) => process.stderr.write(`[link-server:${port}] ${b}`));

    // Track exit so kill() can await it deterministically. `child.killed`
    // flips as soon as the signal is delivered, not when the process
    // actually exits — polling it can race with SQLite WAL flush.
    let exited = false;
    const exitPromise: Promise<void> = new Promise((resolve) => {
        proc.once("exit", () => { exited = true; resolve(); });
    });

    async function killAndCleanup() {
        if (!exited) {
            proc.kill("SIGTERM");
            const graceful = await Promise.race([
                exitPromise.then(() => true),
                sleep(2000).then(() => false),
            ]);
            if (!graceful && !exited) {
                proc.kill("SIGKILL");
                await exitPromise;
            }
        }
        try { await fs.remove(dataDir); } catch { /* best-effort — CI cleans /tmp anyway */ }
        if (ownStore) {
            await ownStore.store.stop();
            deregisterPorts([ownStore.port]);
            try { await fs.remove(ownStore.store.dir); } catch { /* best-effort */ }
            ownStore = null;
        }
    }

    // Readiness loop with a bounded per-request timeout so a hung fetch
    // can't consume the entire outer budget. Fall through to killAndCleanup
    // on timeout so we don't leak the process or its data dir.
    const deadline = Date.now() + readyTimeoutMs;
    let ready = false;
    while (Date.now() < deadline) {
        try {
            const res = await fetch(`${url}/health`, {
                signal: AbortSignal.timeout(1000),
            });
            if (res.ok) { ready = true; break; }
        } catch { /* still starting, or per-request timeout */ }
        await sleep(250);
    }
    if (!ready) {
        await killAndCleanup();
        throw new Error(`link-server on port ${port} did not become ready within ${readyTimeoutMs}ms`);
    }

    return {
        url,
        port,
        dataDir,
        process: proc,
        store,
        kill: killAndCleanup,
    };
}
