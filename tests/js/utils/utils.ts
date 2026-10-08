import { ChildProcess, exec, ExecException, execSync, spawn } from "node:child_process";
import { mkdirSync, rmSync, symlinkSync } from "node:fs";
import { createHash } from "node:crypto";
import os from "node:os";
import path from "path";
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import { configureSharedStores } from './sharedStores';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// WebSocket is available natively in Node 21+.

export async function isProcessRunning(processName: string): Promise<boolean> {
    const cmd = (() => {
      switch (process.platform) {
        case 'win32': return `tasklist`
        case 'darwin': return `ps -ax | grep ${processName}`
        case 'linux': return `ps -A`
        default: return false
      }
    })()

    if (!cmd) throw new Error("Invalid OS");

    return new Promise((resolve, reject) => {
      //@ts-ignore
      exec(cmd, (err: ExecException, stdout: string, stderr: string) => {
        if (err) reject(err)

        resolve(stdout.toLowerCase().indexOf(processName.toLowerCase()) > -1)
      })
    })
}

export async function runHcLocalServices(): Promise<{proxyUrl: string | null, bootstrapUrl: string | null, relayUrl: string | null, process: ChildProcess}> {
    // Prefer the workspace-local, version-pinned kitsune2-bootstrap-srv
    // installed by scripts/install-hc-toolchain.sh into $REPO/.hc-toolchain/bin/.
    // This MUST match the kitsune2 version the executor is linked against
    // (crates.io kitsune2 0.5.0 as of HC 0.7.0), otherwise the standalone
    // bootstrap-srv and the executor speak different wire protocols and
    // signals silently fail to route between agents on different nodes.
    // Falls back to $PATH for dev convenience.
    // tests/js/utils/utils.ts -> repo root is three parents up.
    // (fs/path are already imported at the top of this module.)
    const fs = await import("node:fs");
    const repoRoot = path.resolve(__dirname, "..", "..", "..");
    const localBin = path.join(repoRoot, ".hc-toolchain", "bin", "kitsune2-bootstrap-srv");
    const bootstrapBin = fs.existsSync(localBin) ? localBin : "kitsune2-bootstrap-srv";
    console.log(`runHcLocalServices: using ${bootstrapBin}`);
    let servicesProcess = exec(bootstrapBin);

    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let relayUrl: string | null = null;
    let bootstrapPort: string | null = null;
    let relayPort: string | null = null;

    let servicesReady = new Promise<void>((resolve, reject) => {
        const SERVICES_READY_TIMEOUT_MS = 60000; // 60 seconds timeout
        const stdoutBuffer: string[] = [];
        const stderrBuffer: string[] = [];
        let timeoutId: NodeJS.Timeout | null = null;
        let resolved = false;

        const cleanup = () => {
            if (timeoutId) {
                clearTimeout(timeoutId);
                timeoutId = null;
            }
            servicesProcess.stdout!.removeListener('data', stdoutHandler);
            servicesProcess.stderr!.removeListener('data', stderrHandler);
        };

        const stdoutHandler = (data: Buffer) => {
            const dataStr = data.toString();
            stdoutBuffer.push(dataStr);
            console.log("Bootstrap server output: ", dataStr);

            // Look for the bootstrap server listening message
            if (dataStr.includes("#kitsune2_bootstrap_srv#listening#")) {
                const lines = dataStr.split("\n");
                //@ts-ignore
                const portLine = lines.find(line => line.includes("#kitsune2_bootstrap_srv#listening#"));
                if (portLine) {
                    const parts = portLine.split('#');
                    const portPart = parts[3]; // "127.0.0.1:36353"
                    bootstrapPort = portPart.split(':')[1];
                    console.log("Bootstrap Port: ", bootstrapPort);
                    // kitsune2-bootstrap-srv serves PLAIN HTTP when no TLS
                    // cert/key is configured (the self-signed cert it logs is
                    // only for the QUIC/QAD listener). Both consumers require
                    // http:// here:
                    //   - bootstrap: kitsune2_bootstrap_client uses ureq; an
                    //     https:// URL fails the TLS handshake outright.
                    //   - relay (proxyUrl → conductor relay_url): iroh treats
                    //     wss/https as TLS and fails the same way. http:// is
                    //     accepted because holochain's test_utils feature sets
                    //     relay_allow_plain_text=true. Holochain's own
                    //     sweettest rendezvous uses http:// for both, too.
                    bootstrapUrl = `http://127.0.0.1:${bootstrapPort}`;
                    proxyUrl = `http://127.0.0.1:${bootstrapPort}`;
                    console.log("Bootstrap URL: ", bootstrapUrl);
                    console.log("Proxy URL: ", proxyUrl);
                }
            }

            // Look for the iroh relay server message
            if (dataStr.includes("Internal iroh relay server started at")) {
                const match = dataStr.match(/Internal iroh relay server started at ([\d.]+:\d+)/);
                if (match) {
                    const address = match[1];
                    relayPort = address.split(':')[1];
                    console.log("Iroh Relay Port: ", relayPort);
                    relayUrl = `http://127.0.0.1:${relayPort}`;
                    console.log("Relay URL: ", relayUrl);
                }
            }

            // Resolve when we have bootstrap port (relay is now internal to bootstrap server)
            // The new kitsune2-bootstrap-srv (0.4.0+) doesn't output a separate relay port
            if (bootstrapPort && !resolved) {
                resolved = true;
                cleanup();
                resolve();
            }
        };

        const stderrHandler = (data: Buffer) => {
            const dataStr = data.toString();
            stderrBuffer.push(dataStr);
            console.log("Bootstrap server stderr: ", dataStr);

            if (!resolved && dataStr.includes('command not found')) {
                resolved = true;
                cleanup();
                try {
                    servicesProcess.kill('SIGKILL');
                } catch {}
                reject(new Error(`kitsune2-bootstrap-srv unavailable: ${dataStr.trim()}`));
            }
        };

        servicesProcess.stdout!.on('data', stdoutHandler);
        servicesProcess.stderr!.on('data', stderrHandler);

        // Set up timeout to prevent hanging forever
        timeoutId = setTimeout(() => {
            if (!resolved) {
                resolved = true;
                cleanup();

                console.error("=== Services startup timeout ===");
                console.error(`Timeout after ${SERVICES_READY_TIMEOUT_MS}ms waiting for bootstrap and relay services`);
                console.error(`Bootstrap port found: ${bootstrapPort ?? 'NO'}`);
                console.error(`Relay port found: ${relayPort ?? 'NO'}`);
                console.error("--- Collected stdout ---");
                console.error(stdoutBuffer.join(''));
                console.error("--- Collected stderr ---");
                console.error(stderrBuffer.join(''));
                console.error("========================");

                // Kill the services process
                try {
                    servicesProcess.kill('SIGKILL');
                } catch (killErr) {
                    console.error("Error killing services process:", killErr);
                }

                reject(new Error(`Services startup timeout: bootstrapPort=${bootstrapPort}, relayPort=${relayPort}`));
            }
        }, SERVICES_READY_TIMEOUT_MS);
    });

    await servicesReady;
    return {proxyUrl, bootstrapUrl, relayUrl, process: servicesProcess};
}

// Shared per-process local services, lazily started by startExecutor for
// suites that don't manage their own bootstrap server. One server per mocha
// process: executors started by the same suite share it, so two-node suites
// that rely on the fallback still discover each other via bootstrap.
let sharedLocalServices: ReturnType<typeof runHcLocalServices> | null = null;

function ensureSharedLocalServices(): ReturnType<typeof runHcLocalServices> {
    if (!sharedLocalServices) {
        sharedLocalServices = runHcLocalServices().then((services) => {
            // Tear the server down when mocha exits (--exit / SIGTERM paths
            // both end in 'exit'). spawn() in runHcLocalServices runs the
            // binary directly, so the signal reaches the actual server.
            process.once('exit', () => {
                try { services.process.kill('SIGKILL'); } catch {}
            });
            return services;
        });
        // On startup failure, allow a later retry instead of caching rejection.
        sharedLocalServices.catch(() => { sharedLocalServices = null; });
    }
    return sharedLocalServices;
}

/**
 * How long startExecutor() waits for the readiness markers before it kills
 * the executor and rejects. A healthy start takes well under this; the bound
 * turns a stalled start into a logged failure instead of mocha's 1200 s hang.
 */
const EXECUTOR_STARTUP_TIMEOUT_MS = 300_000;
/** Output lines startExecutor() includes when the executor never gets ready. */
const STARTUP_LOG_TAIL_LINES = 50;

export async function startExecutor(dataPath: string,
    bootstrapSeedPath: string,
    apiPort: number,
    hcAdminPort: number,
    hcAppPort: number,
    languageLanguageOnly: boolean = false,
    adminCredential?: string,
    // NEVER default to dev-test-bootstrap2.holochain.org — that server is
    // an outdated HC test bootstrap that our HC 0.7.0 fork does not target.
    // Multi-node suites should pass the local kitsune2-bootstrap-srv URLs
    // from their own runHcLocalServices() call so all their executors share
    // one server. When omitted, a per-process shared local bootstrap-srv is
    // started lazily — never a public server.
    proxyUrl?: string,
    bootstrapUrl?: string,
    relayUrl?: string,
    enableMcp: boolean = false,
    mcpPort?: number,
    // Expose the dynamic per-class SHACL tools over MCP (`--dynamic-class-tools`).
    // Off by default, matching the executor's default: only the static
    // instance_* surface is advertised.
    dynamicClassTools: boolean = false,
    runHolochain: boolean = true,
    sharedStores: boolean = true,
): Promise<ChildProcess> {
    if (runHolochain && (!proxyUrl || !bootstrapUrl)) {
        const services = await ensureSharedLocalServices();
        proxyUrl = services.proxyUrl!;
        bootstrapUrl = services.bootstrapUrl!;
        if (!relayUrl && services.relayUrl) {
            relayUrl = services.relayUrl;
        }
    }
    const command = executorBinary();

    const effectiveDataPath = path.join(
        os.tmpdir(),
        `ad4m-${createHash('sha1').update(dataPath).digest('hex').slice(0, 12)}`,
    );

    console.log(bootstrapSeedPath);
    console.log(dataPath);
    if (effectiveDataPath !== dataPath) {
        console.log(`Using shortened executor data path: ${effectiveDataPath}`);
    }
    rmSync(dataPath, { recursive: true, force: true })
    rmSync(effectiveDataPath, { recursive: true, force: true })
    execSync(`${command} init --data-path ${effectiveDataPath} --network-bootstrap-seed ${bootstrapSeedPath}`, {cwd: process.cwd()})

    // Shared mode for the local language-language and neighbourhood store,
    // so executors see each other's published languages and neighbourhoods
    // (see sharedStores.ts). Off only for tests of the default KV mode.
    if (sharedStores) {
        configureSharedStores(effectiveDataPath, bootstrapSeedPath);
    }

    // Symlink legacy dataPath → effectiveDataPath so test helpers that
    // reference the original path (e.g. injectPublishingAgent.js) still work.
    if (effectiveDataPath !== dataPath) {
        mkdirSync(path.dirname(dataPath), { recursive: true });
        symlinkSync(effectiveDataPath, dataPath);
    }
    
    console.log("Starting executor")

    console.log("USING LOCAL BOOTSTRAP & PROXY URL: ", bootstrapUrl, proxyUrl);
    if (relayUrl) {
        console.log("USING RELAY URL: ", relayUrl);
    }

    // Build args array explicitly so spawn() can run the executor directly
    // (no shell wrapper). This is critical: exec() spawns `sh -c "..."` and
    // kill() only kills the shell, leaving the actual executor running.
    // spawn() runs the binary directly, so kill() / SIGKILL actually reach it.
    const args = [
        'run',
        '--app-data-path', effectiveDataPath,
        '--port', String(apiPort),
        '--language-language-only', String(languageLanguageOnly),
        '--run-dapp-server', 'false',
    ];
    if (runHolochain) {
        args.push(
            '--hc-admin-port', String(hcAdminPort),
            '--hc-app-port', String(hcAppPort),
            '--hc-proxy-url', proxyUrl!,
            '--hc-bootstrap-url', bootstrapUrl!,
            '--hc-use-bootstrap', 'true',
            '--hc-use-proxy', 'true',
            '--hc-use-local-proxy', 'true',
            '--hc-use-mdns', 'true',
        );
    } else {
        args.push('--run-holochain', 'false');
    }
    if (relayUrl) { args.push('--hc-relay-url', relayUrl); }
    if (enableMcp) { args.push('--enable-mcp', 'true'); }
    if (mcpPort) { args.push('--mcp-port', String(mcpPort)); }
    if (dynamicClassTools) { args.push('--dynamic-class-tools', 'true'); }
    // Without a credential the executor refuses to start unless it is told
    // that this is a test run; the empty token is then the operator.
    if (adminCredential) { args.push('--admin-credential', adminCredential); }
    else { args.push('--insecure-no-admin-credential'); }

    return spawnExecutor(args, apiPort, { enableMcp });
}

/** The `ad4m-executor` binary the suites run. */
export function executorBinary(): string {
    return path.resolve(__dirname, '..', '..', '..', 'target', 'release', 'ad4m-executor');
}

/**
 * Spawns `ad4m-executor <args>` and resolves once the RPC port (and MCP, if
 * `enableMcp`) logs that it is listening; rejects if the process exits
 * first. `env` is added to this process's environment.
 */
export async function spawnExecutor(
    args: string[],
    apiPort: number,
    opts: {
        enableMcp?: boolean;
        env?: Record<string, string>;
        /** Receives every chunk of stdout and stderr from the start. */
        onOutput?: (text: string) => void;
    } = {},
): Promise<ChildProcess> {
    const { enableMcp = false, env = {}, onOutput } = opts;
    const executorProcess = spawn(executorBinary(), args, {
        stdio: ['ignore', 'pipe', 'pipe'],
        env: { ...process.env, ...env },
    });
    // Decode as a stream, so a multibyte character split across two chunks
    // survives in the startup-failure tail. Every data handler below gets strings.
    executorProcess.stdout!.setEncoding('utf8');
    executorProcess.stderr!.setEncoding('utf8');
    // The last output lines, for the error when the executor never gets ready.
    const recentOutput: string[] = [];
    const recordOutput = (data: any) => {
        recentOutput.push(...data.toString().split('\n').filter((line: string) => line.trim()));
        recentOutput.splice(0, Math.max(0, recentOutput.length - STARTUP_LOG_TAIL_LINES));
    };
    executorProcess.stdout!.on('data', recordOutput);
    executorProcess.stderr!.on('data', recordOutput);

    let executorReady = new Promise<void>((resolve, reject) => {
        // REST branch no longer emits the old `listening on http://127.0.0.1:<port>`
        // marker consistently. Accept either the legacy marker or the REST startup log so tests
        // can run against both pre-REST and REST executors.
        const legacyApiMarker = `listening on http://127.0.0.1:${apiPort}`;
        const restApiMarker = `API server starting on http://127.0.0.1:${apiPort}/api/v1`;
        const mcpMarker = 'MCP HTTP server listening';
        let apiReady = false;
        let mcpReady = !enableMcp;
        let resolved = false;

        // Without these, an executor that dies or stalls before the marker
        // (e.g. the API port is taken: it logs the bind error and exits 1)
        // leaves this promise pending until mocha's 1200 s timeout.
        const fail = (reason: string) => {
            if (resolved) return;
            resolved = true;
            clearTimeout(timer);
            reject(new Error(
                `Executor on API port ${apiPort} ${reason}. Last ${recentOutput.length} output lines:\n` +
                recentOutput.join('\n'),
            ));
        };
        // 'close', not 'exit': 'exit' can fire while the pipes still hold the
        // executor's last lines, which are the ones that say why it died.
        const onClose = (code: number | null, signal: NodeJS.Signals | null) =>
            fail(`exited before it was ready (code ${code}, signal ${signal})`);
        const onError = (error: Error) => fail(`could not be started: ${error.message}`);
        const timer = setTimeout(() => {
            // Kill it, so it does not keep holding its ports after we give up.
            executorProcess!.kill('SIGKILL');
            fail(`was not ready after ${EXECUTOR_STARTUP_TIMEOUT_MS / 1000} s`);
        }, EXECUTOR_STARTUP_TIMEOUT_MS);
        executorProcess!.once('close', onClose);
        executorProcess!.once('error', onError);

        const maybeResolve = () => {
            if (!resolved && apiReady && mcpReady) {
                resolved = true;
                clearTimeout(timer);
                executorProcess!.off('close', onClose);
                executorProcess!.off('error', onError);
                resolve();
            }
        };

        const checkReady = (data: string) => {
            if (data.includes(legacyApiMarker) || data.includes(restApiMarker)) {
                apiReady = true;
            }
            if (enableMcp && data.includes(mcpMarker)) {
                mcpReady = true;
            }
            maybeResolve();
        };

        executorProcess.stdout!.on('data', (data: any) => checkReady(data.toString()));
        executorProcess.stderr!.on('data', (data: any) => checkReady(data.toString()));
    })

    executorProcess.stdout!.on('data', (data) => {
        console.log(`${data}`);
        onOutput?.(data.toString());
    });
    executorProcess.stderr!.on('data', (data) => {
        console.log(`${data}`);
        onOutput?.(data.toString());
    });

    console.log("Waiting for executor to settle...")
    await executorReady
    return executorProcess;
}

export function baseUrl(port: number): string {
    return `http://127.0.0.1:${port}`;
}

export function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Poll a predicate until it returns true, or throw with `label` after
 * `timeoutMs`.
 *
 * Replaces the `await sleep(N); expect(x)` pattern: the test completes as
 * soon as the condition holds and only fails after a bounded timeout.
 * Only for POSITIVE conditions: an absence check ("X does not happen") passes
 * on the first tick. Use `assertStaysFalse` after a positive barrier for those.
 *
 * The predicate may be sync or async. Exceptions from it are treated as
 * "not yet true"; the last one is reported in the timeout message.
 */
export async function pollUntil(
    predicate: () => boolean | Promise<boolean>,
    opts: { timeoutMs?: number; intervalMs?: number; label?: string } = {},
): Promise<void> {
    const { timeoutMs = 15000, intervalMs = 200, label = "condition" } = opts;
    const deadline = Date.now() + timeoutMs;
    let lastError: unknown;
    while (Date.now() < deadline) {
        try {
            if (await predicate()) return;
        } catch (err) { lastError = err; /* treat as "not yet" */ }
        await sleep(intervalMs);
    }
    const suffix = lastError ? ` (last error: ${lastError instanceof Error ? lastError.message : String(lastError)})` : "";
    throw new Error(`pollUntil timed out after ${timeoutMs}ms waiting for: ${label}${suffix}`);
}

/**
 * Resolves true once `child` has exited (or had already), false after `timeoutMs`.
 * Use this, not `child.killed`: `killed` only records that a signal was delivered.
 */
export function waitForExit(child: ChildProcess, timeoutMs: number): Promise<boolean> {
    if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve(true);
    return new Promise((resolve) => {
        const onExit = () => { clearTimeout(timer); resolve(true); };
        const timer = setTimeout(() => { child.off("exit", onExit); resolve(false); }, timeoutMs);
        child.once("exit", onExit);
    });
}

/** SIGTERM `child`, and SIGKILL it if it has not exited within `graceMs`; waits for the exit either way. */
export async function stopChildProcess(child: ChildProcess, graceMs = 5000): Promise<void> {
    child.kill("SIGTERM");
    if (await waitForExit(child, graceMs)) return;
    child.kill("SIGKILL");
    await waitForExit(child, graceMs);
}

/**
 * Actively polls a predicate for `waitMs` and fails as soon as it becomes true.
 * Use this for negative assertions ("X should NOT happen"), and run it after a
 * positive barrier or with a window at least as long as the latency of the
 * thing that must not happen.
 *
 * A predicate that throws counts as "not evaluable", not as false: if it never
 * evaluated successfully during the window, this fails with the last error, so
 * a broken predicate cannot pass the check vacuously.
 */
export async function assertStaysFalse(
    predicate: () => boolean | Promise<boolean>,
    opts: { waitMs?: number; intervalMs?: number; label?: string } = {},
): Promise<void> {
    const { waitMs = 1000, intervalMs = 100, label = "condition" } = opts;
    const deadline = Date.now() + waitMs;
    let evaluations = 0;
    let lastError: unknown;
    while (Date.now() < deadline) {
        let value: boolean;
        try {
            value = await predicate();
            evaluations++;
        } catch (err) {
            lastError = err;
            await sleep(intervalMs);
            continue;
        }
        if (value) throw new Error(`assertStaysFalse failed: ${label} became true`);
        await sleep(intervalMs);
    }
    if (evaluations === 0) {
        const detail = lastError instanceof Error ? lastError.message : String(lastError);
        throw new Error(`assertStaysFalse: ${label} never evaluated in ${waitMs}ms (last error: ${detail})`);
    }
}

/**
 * Clears all links in a perspective in a single removeLinks() batch call.
 * Import from here or from helpers/assertions to get a clean slate before each test.
 */
export async function wipePerspective(
  perspective: import("@coasys/ad4m").PerspectiveProxy,
): Promise<void> {
  const { LinkQuery } = await import("@coasys/ad4m");
  const links = await perspective.get(new LinkQuery({}));
  if (links.length > 0) {
    await perspective.removeLinks(links);
  }
  // Clear the per-instance SHACL registration cache so that
  // subsequent register() calls re-add SHACL definitions.
  if (typeof perspective.clearEnsuredSubjectClasses === 'function') {
    perspective.clearEnsuredSubjectClasses();
  }
}

/**
 * Kill any process listening on the given ports.
 * Uses SIGTERM → wait → SIGKILL escalation for graceful shutdown.
 * Use this in after() hooks as a safety net.
 */
export function killByPorts(ports: number[]): void {
    for (const port of ports) {
        try {
            // First try SIGTERM for graceful shutdown
            execSync(`lsof -ti:${port} | xargs -r kill -TERM`, { stdio: 'ignore' });
        } catch (e) {
            // Port not in use — fine
        }
    }
    // Give processes a moment to shut down gracefully
    try { execSync('sleep 2', { stdio: 'ignore' }); } catch (e) { /* ignore */ }
    for (const port of ports) {
        try {
            // SIGKILL anything still lingering
            execSync(`lsof -ti:${port} | xargs -r kill -9`, { stdio: 'ignore' });
        } catch (e) {
            // Port not in use — fine
        }
    }
}

/**
 * Gracefully shut down a ChildProcess using SIGTERM → wait → SIGKILL escalation.
 * Replaces the common pattern of `while (!process.killed) { process.kill(); await sleep(500); }`
 * which sends repeated SIGTERM signals unnecessarily.
 *
 * @param proc - The ChildProcess to shut down
 * @param label - Label for logging
 * @param timeoutMs - How long to wait for graceful shutdown before SIGKILL (default: 10s)
 */
export async function gracefulShutdown(proc: ChildProcess | null | undefined, label: string = "process", timeoutMs: number = 10000): Promise<void> {
    if (!proc || proc.killed) return;

    console.log(`Sending SIGTERM to ${label} (PID ${proc.pid})...`);
    proc.kill('SIGTERM');

    // Wait for the process to actually exit (not just signal sent)
    const exited = await new Promise<boolean>((resolve) => {
        const timer = setTimeout(() => resolve(false), timeoutMs);
        proc!.on('close', () => {
            clearTimeout(timer);
            resolve(true);
        });
    });

    if (!exited) {
        console.log(`${label} did not exit after ${timeoutMs}ms, sending SIGKILL...`);
        proc.kill('SIGKILL');
        // Wait for SIGKILL to take effect
        await new Promise<void>((resolve) => {
            const timer = setTimeout(resolve, 5000);
            proc!.on('close', () => {
                clearTimeout(timer);
                resolve();
            });
        });
    }

    console.log(`${label} shut down (pid=${proc.pid})`);
}

/**
 * Gracefully quit an executor by calling the REST runtime quit endpoint,
 * then falling back to gracefulShutdown (SIGTERM → SIGKILL) if needed.
 */
export async function quitExecutor(
    executorProcess: ChildProcess,
    apiPort: number,
    adminCredential?: string,
    timeoutMs: number = 8000,
): Promise<void> {
    if (executorProcess.exitCode !== null) return;

    // The REST runtime quit endpoint returns before scheduling process exit.
    // If the connection drops while the executor is exiting, that's expected.
    try {
        const headers: Record<string, string> = {
            'Content-Type': 'application/json',
        };
        if (adminCredential !== undefined) {
            headers['Authorization'] = `Bearer ${adminCredential}`;
        }

        await Promise.race([
            fetch(`http://127.0.0.1:${apiPort}/api/v1/runtime/quit`, {
                method: 'POST',
                headers,
            }).then(async (res) => {
                if (!res.ok) {
                    throw new Error(await res.text());
                }
                return res;
            }),
            new Promise((_, reject) => setTimeout(() => reject(new Error('runtime quit timeout')), 3000)),
        ]);
    } catch (_e) {
        // Expected: connection dropped or timed out while the executor is exiting.
    }

    // Wait for natural exit after runtime quit
    const exited = await new Promise<boolean>((resolve) => {
        if (executorProcess.exitCode !== null) { resolve(true); return; }
        const timer = setTimeout(() => resolve(false), timeoutMs);
        executorProcess.once('exit', () => { clearTimeout(timer); resolve(true); });
    });

    if (!exited) {
        // runtime quit didn't work — fall back to SIGTERM/SIGKILL escalation
        console.warn(`quitExecutor: executor (port ${apiPort}) still running after runtime quit, falling back to gracefulShutdown`);
        await gracefulShutdown(executorProcess, `executor:${apiPort}`);
    }
}
