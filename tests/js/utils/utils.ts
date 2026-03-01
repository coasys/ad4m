import { ChildProcess, exec, ExecException, execSync, spawn } from "node:child_process";
import { rmSync } from "node:fs";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, gql, InMemoryCache } from "@apollo/client/core/index.js";
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

            // Resolve when we have both ports
            if (bootstrapPort && relayPort && !resolved) {
                resolved = true;
                cleanup();
                resolve();
            }
        };

        const stderrHandler = (data: Buffer) => {
            const dataStr = data.toString();
            stderrBuffer.push(dataStr);
            console.log("Bootstrap server stderr: ", dataStr);
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
): Promise<ChildProcess> {
    const command = path.resolve(__dirname, '..', '..', '..','target', 'release', 'ad4m-executor');

    console.log(bootstrapSeedPath);
    console.log(dataPath);
    let executorProcess = null as ChildProcess | null;
    rmSync(dataPath, { recursive: true, force: true })
    execSync(`${command} init --data-path ${dataPath} --network-bootstrap-seed ${bootstrapSeedPath}`, {cwd: process.cwd()})
    
    console.log("Starting executor")

    console.log("USING LOCAL BOOTSTRAP & PROXY URL: ", bootstrapUrl, proxyUrl);
    if (relayUrl) {
        console.log("USING RELAY URL: ", relayUrl);
    }

    // Build args array explicitly so spawn() can run the executor directly
    // (no shell wrapper). exec() spawns `sh -c "..."` — kill() only kills
    // the shell, leaving the actual executor running as an orphan.
    // spawn() runs the binary directly so kill()/SIGKILL actually reach it.
    const args = [
        'run',
        '--app-data-path', dataPath,
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
    if (adminCredential) { args.push('--admin-credential', adminCredential); }

    executorProcess = spawn(command, args, { stdio: ['ignore', 'pipe', 'pipe'] });
    let executorReady = new Promise<void>((resolve, reject) => {
        executorProcess!.stdout!.on('data', (data) => {
            if (data.includes(`listening on http://127.0.0.1:${gqlPort}`)) {
                resolve()
            }
        });
        executorProcess!.stderr!.on('data', (data) => {
            if (data.includes(`listening on http://127.0.0.1:${gqlPort}`)) {
                resolve()
            }
        });
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
    // Suppress connection errors during teardown (executor may have exited).
    wsLink.client.on('error' as any, () => {});

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
 * Kill any process listening on the given ports.
 * Use as a last-resort safety net — prefer quitExecutor() for normal teardown.
 */
export function killByPorts(ports: number[]): void {
    for (const port of ports) {
        try {
            execSync(`lsof -ti TCP:${port} -s TCP:LISTEN | xargs -r kill -9`, { stdio: 'ignore' });
        } catch (e) {
            // Port not in use — fine
        }
    }
}

/**
 * Gracefully shut down an executor via the runtimeQuit GraphQL mutation,
 * then wait for the process to exit naturally.
 *
 * The executor calls std::process::exit(0) immediately on runtimeQuit, so the
 * WebSocket connection drops mid-call — that's expected, not an error. We wait
 * for the OS-level 'exit' event to confirm the process is gone. If it doesn't
 * exit within the timeout we escalate to SIGTERM → SIGKILL → port kill.
 *
 * @param executorProcess - The ChildProcess returned by startExecutor()
 * @param gqlPort         - The GQL port the executor is listening on
 * @param adminCredential - Optional admin credential (pass if executor was
 *                          started with --admin-credential)
 * @param timeoutMs       - How long to wait for natural exit (default 8s)
 */
export async function quitExecutor(
    executorProcess: ChildProcess,
    gqlPort: number,
    adminCredential?: string,
    hcAdminPort?: number,
    hcAppPort?: number,
    timeoutMs: number = 8000,
): Promise<void> {
    // Collect all ports to clean up (GQL + optional HC ports).
    const allPorts = [gqlPort, ...(hcAdminPort ? [hcAdminPort] : []), ...(hcAppPort ? [hcAppPort] : [])];

    // Only check exitCode (set when OS process actually exits).
    // Do NOT check executorProcess.killed — that's set as soon as kill() is
    // CALLED, not when the process has actually terminated. Using it as a
    // "process is gone" guard would cause SIGKILL to never fire after SIGTERM.
    if (executorProcess.exitCode !== null) return;

    // One shared exitPromise reused across all wait stages below.
    const exitPromise = new Promise<void>((resolve) => {
        if (executorProcess.exitCode !== null) { resolve(); return; }
        executorProcess.once('exit', () => resolve());
        executorProcess.once('close', () => resolve());
    });

    // Fire the runtimeQuit mutation. The executor calls process::exit(0) which
    // kills it before it can send a GraphQL response, so we'll get a WebSocket
    // close error — that's fine, it means the quit worked.
    try {
        const client = apolloClient(gqlPort, adminCredential);
        await Promise.race([
            client.mutate({ mutation: gql`mutation { runtimeQuit }` }),
            new Promise((_, reject) => setTimeout(() => reject(new Error('runtimeQuit timeout')), 3000)),
        ]);
    } catch (_e) {
        // Expected: either the connection dropped (executor exited) or it timed out.
    }

    // Wait for natural exit
    const gracefullyExited = await Promise.race([
        exitPromise.then(() => true),
        new Promise<boolean>((resolve) => setTimeout(() => resolve(false), timeoutMs)),
    ]);
    if (gracefullyExited) { killByPorts(allPorts); return; }

    // Escalate: SIGTERM
    console.warn(`quitExecutor: executor (port ${gqlPort}) still running after ${timeoutMs}ms, sending SIGTERM`);
    executorProcess.kill('SIGTERM');
    const termExited = await Promise.race([
        exitPromise.then(() => true),
        new Promise<boolean>((resolve) => setTimeout(() => resolve(false), 3000)),
    ]);
    if (termExited) { killByPorts(allPorts); return; }

    // Final escalation: SIGKILL
    console.warn(`quitExecutor: executor (port ${gqlPort}) survived SIGTERM, sending SIGKILL`);
    executorProcess.kill('SIGKILL');
    await Promise.race([
        exitPromise,
        new Promise<void>((resolve) => setTimeout(resolve, 2000)),
    ]);

    // Always ensure the port is freed, regardless of kill outcome.
    killByPorts(allPorts);
}