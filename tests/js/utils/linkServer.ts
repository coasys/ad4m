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

/**
 * Spawn a link-server on a free port. Waits up to `readyTimeoutMs` for the
 * server's /health endpoint to respond 200. Auto-admits every agent (tests
 * don't rely on the ACL flow — that's covered by the language's unit tests).
 *
 * Callers should invoke `handle.kill()` in an `after()` hook.
 */
export async function startLinkServer(readyTimeoutMs = 15000): Promise<LinkServerHandle> {
    if (!fs.existsSync(LINK_SERVER_ENTRY)) {
        throw new Error(
            `link-server entry not found at ${LINK_SERVER_ENTRY} — run \`pnpm --filter @coasys/link-server build\` first`,
        );
    }

    const port = await getFreePort();
    const dataDir = await fs.mkdtemp(path.join(os.tmpdir(), "link-server-test-"));

    const proc = spawn(
        process.execPath,
        [LINK_SERVER_ENTRY, "--port", String(port), "--data", dataDir],
        {
            // SKIP_LINK_VERIFICATION: the Rust executor signs links as
            // sha256(serde_json(link.data) ++ timestamp_bytes) but link-server's
            // canonicalLinkPayload builds a different JSON envelope (all fields
            // inline including author + timestamp). Verification fails on every
            // executor-produced link. Auth is still enforced (JWT bound to DID);
            // link signature is defense-in-depth on top. Unify the schemes in a
            // follow-up so this env var can be removed.
            env: {
                ...process.env,
                PORT: String(port),
                DATA_DIR: dataDir,
                AUTO_ADMIT: "true",
                SKIP_LINK_VERIFICATION: "true",
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
