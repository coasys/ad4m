import { ChildProcess, exec, ExecException, execSync, spawn } from "node:child_process";
import { rmSync } from "node:fs";
import { createHash } from "node:crypto";
import os from "node:os";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";
import path from "path";
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

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
    let servicesProcess = exec(`kitsune2-bootstrap-srv`);

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
                    bootstrapUrl = `https://127.0.0.1:${bootstrapPort}`;
                    proxyUrl = `wss://127.0.0.1:${bootstrapPort}`;
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

export async function startExecutor(dataPath: string,
    bootstrapSeedPath: string,
    gqlPort: number,
    hcAdminPort: number,
    hcAppPort: number,
    languageLanguageOnly: boolean = false,
    adminCredential?: string,
    proxyUrl: string = "wss://dev-test-bootstrap2.holochain.org",
    bootstrapUrl: string = "https://dev-test-bootstrap2.holochain.org",
    relayUrl?: string,
    enableMcp: boolean = false,
    mcpPort?: number,
): Promise<ChildProcess> {
    const command = path.resolve(__dirname, '..', '..', '..','target', 'release', 'ad4m-executor');

    const effectiveDataPath = path.join(
        os.tmpdir(),
        `ad4m-${createHash('sha1').update(dataPath).digest('hex').slice(0, 12)}`,
    );

    console.log(bootstrapSeedPath);
    console.log(dataPath);
    if (effectiveDataPath !== dataPath) {
        console.log(`Using shortened executor data path: ${effectiveDataPath}`);
    }
    let executorProcess = null as ChildProcess | null;
    rmSync(dataPath, { recursive: true, force: true })
    rmSync(effectiveDataPath, { recursive: true, force: true })
    execSync(`${command} init --data-path ${effectiveDataPath} --network-bootstrap-seed ${bootstrapSeedPath}`, {cwd: process.cwd()})
    
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
        '--gql-port', String(gqlPort),
        '--hc-admin-port', String(hcAdminPort),
        '--hc-app-port', String(hcAppPort),
        '--hc-proxy-url', proxyUrl,
        '--hc-bootstrap-url', bootstrapUrl,
        '--hc-use-bootstrap', 'true',
        '--hc-use-proxy', 'true',
        '--hc-use-local-proxy', 'true',
        '--hc-use-mdns', 'true',
        '--language-language-only', String(languageLanguageOnly),
        '--run-dapp-server', 'false',
    ];
    if (relayUrl) { args.push('--hc-relay-url', relayUrl); }
    if (enableMcp) { args.push('--enable-mcp', 'true'); }
    if (mcpPort) { args.push('--mcp-port', String(mcpPort)); }
    if (adminCredential) { args.push('--admin-credential', adminCredential); }

    executorProcess = spawn(command, args, { stdio: ['ignore', 'pipe', 'pipe'] });
    let executorReady = new Promise<void>((resolve, reject) => {
        // REST branch no longer emits the old GraphQL-era `listening on http://127.0.0.1:<port>`
        // marker consistently. Accept either the legacy marker or the REST startup log so tests
        // can run against both pre-REST and REST executors.
        const legacyApiMarker = `listening on http://127.0.0.1:${gqlPort}`;
        const restApiMarker = `REST API server starting on http://127.0.0.1:${gqlPort}/api/v1`;
        const mcpMarker = 'MCP HTTP server listening';
        let apiReady = false;
        let mcpReady = !enableMcp;
        let resolved = false;

        const maybeResolve = () => {
            if (!resolved && apiReady && mcpReady) {
                resolved = true;
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

        executorProcess!.stdout!.on('data', (data: any) => checkReady(data.toString()));
        executorProcess!.stderr!.on('data', (data: any) => checkReady(data.toString()));
    })

    executorProcess!.stdout!.on('data', (data) => {
        console.log(`${data}`);
    });
    executorProcess!.stderr!.on('data', (data) => {
        console.log(`${data}`);
    });

    console.log("Waiting for executor to settle...")
    await executorReady
    return executorProcess;
}

export function apolloClient(port: number, token?: string): ApolloClient<any> {
    //@ts-ignore
    const wsLink = new GraphQLWsLink(createClient({
        url: `ws://127.0.0.1:${port}/graphql`,
        webSocketImpl: Websocket,
        connectionParams: () => {
            return {
                headers: {
                    authorization: token || ""
                }
            }
        },
    }));
    wsLink.client.on('message' as any, (data: any) => {
        if (data.payload) {
            if (data.payload.errors) {
                console.dir(data.payload.errors, { depth: null });
            }
        }
    });

    let client = new ApolloClient({
        link: wsLink,
        cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
        defaultOptions: {
            watchQuery: {
                fetchPolicy: "no-cache",
            },
            query: {
                fetchPolicy: "no-cache",
            },
            mutate: {
                fetchPolicy: "no-cache"
            }
        },
    });

    return client;
}

export function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
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
    gqlPort: number,
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
            fetch(`http://127.0.0.1:${gqlPort}/api/v1/runtime/quit`, {
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
        console.warn(`quitExecutor: executor (port ${gqlPort}) still running after runtime quit, falling back to gracefulShutdown`);
        await gracefulShutdown(executorProcess, `executor:${gqlPort}`);
    }
}
