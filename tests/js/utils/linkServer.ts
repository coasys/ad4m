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
            env: { ...process.env, PORT: String(port), DATA_DIR: dataDir, AUTO_ADMIT: "true" },
            stdio: ["ignore", "pipe", "pipe"],
        },
    );

    proc.stdout?.on("data", (b) => process.stdout.write(`[link-server:${port}] ${b}`));
    proc.stderr?.on("data", (b) => process.stderr.write(`[link-server:${port}] ${b}`));

    const url = `http://127.0.0.1:${port}`;
    const deadline = Date.now() + readyTimeoutMs;
    let ready = false;
    while (Date.now() < deadline) {
        try {
            const res = await fetch(`${url}/health`);
            if (res.ok) { ready = true; break; }
        } catch { /* still starting */ }
        await sleep(250);
    }
    if (!ready) {
        proc.kill("SIGKILL");
        throw new Error(`link-server on port ${port} did not become ready within ${readyTimeoutMs}ms`);
    }

    return {
        url,
        port,
        dataDir,
        process: proc,
        async kill() {
            proc.kill("SIGTERM");
            // Give it a moment to flush the SQLite WAL, then hard-kill if needed.
            await sleep(500);
            if (!proc.killed) proc.kill("SIGKILL");
            try { await fs.remove(dataDir); } catch { /* best-effort */ }
        },
    };
}
