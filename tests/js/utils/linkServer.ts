// Test lifecycle helper for the link-server used by the server-link-language
// integration matrix. Spawns `link-server` from its built entrypoint on an
// OS-assigned free port with a temp data dir, waits for the /health endpoint
// to become ready, and returns a handle with `{url, kill}`.
//
// Modelled on runHcLocalServices in utils.ts — same shape so it slots into
// the existing test before()/after() hooks without new conventions.

import { spawn, ChildProcess } from "child_process";
import fs from "fs-extra";
import os from "os";
import path from "path";
import { fileURLToPath } from "url";
import { getFreePort } from "../helpers/ports.js";
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
    kill: () => Promise<void>;
}

export interface LinkServerOptions {
    /**
     * The identity store whose key logs say which person each key speaks for
     * (`--store-url`). Without it the server admits no person, and only
     * `autoAdmit` lets a key in.
     */
    storeUrl?: string;
    /** The store keys whose signed heads the server trusts (`--store-head-keys`). */
    storeHeadKeys?: string[];
    /** Admit every key that authenticates (default true). */
    autoAdmit?: boolean;
    readyTimeoutMs?: number;
}

/**
 * Spawn a link-server (protocol 2) on a free port. Waits up to `readyTimeoutMs`
 * for the server's /health endpoint to respond 200.
 *
 * A room exists only once its creator's first membership commit made it
 * (genesis: the `CREATOR` and `UID` template parameters of the server link
 * language); auth to any other room answers 404. `autoAdmit` admits every key
 * that authenticates to a room that exists; it does not make rooms.
 *
 * Callers should invoke `handle.kill()` in an `after()` hook.
 */
export async function startLinkServer(opts: LinkServerOptions = {}): Promise<LinkServerHandle> {
    const readyTimeoutMs = opts.readyTimeoutMs ?? 15000;
    const autoAdmit = opts.autoAdmit ?? true;
    if (!fs.existsSync(LINK_SERVER_ENTRY)) {
        throw new Error(
            `link-server entry not found at ${LINK_SERVER_ENTRY} — run \`pnpm --filter @coasys/link-server build\` first`,
        );
    }

    const port = await getFreePort();
    const dataDir = await fs.mkdtemp(path.join(os.tmpdir(), "link-server-test-"));

    const args = [LINK_SERVER_ENTRY, "--port", String(port), "--data", dataDir];
    if (opts.storeUrl) args.push("--store-url", opts.storeUrl);
    if (opts.storeHeadKeys?.length) args.push("--store-head-keys", opts.storeHeadKeys.join(","));
    // The server refuses AUTO_ADMIT under NODE_ENV=production, which a
    // developer shell may set: the test server is never production.
    const { NODE_ENV: _nodeEnv, ...env } = process.env;
    const proc = spawn(
        process.execPath,
        args,
        {
            env: {
                ...env,
                PORT: String(port),
                DATA_DIR: dataDir,
                AUTO_ADMIT: autoAdmit ? "true" : "false",
                // The server starts with AUTO_ADMIT only when marked a development one.
                ALLOW_AUTO_ADMIT_DEV: autoAdmit ? "true" : "false",
            },
            stdio: ["ignore", "pipe", "pipe"],
        },
    );

    proc.stdout?.on("data", (b) => process.stdout.write(`[link-server:${port}] ${b}`));
    proc.stderr?.on("data", (b) => process.stderr.write(`[link-server:${port}] ${b}`));

    const url = `http://127.0.0.1:${port}`;

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
        kill: killAndCleanup,
    };
}
