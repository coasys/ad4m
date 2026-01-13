import { ChildProcess, exec, ExecException, execSync } from "node:child_process";
import { rmSync } from "node:fs";
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
        servicesProcess.stdout!.on('data', (data) => {
            const dataStr = data.toString();
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
                    relayUrl = `https://127.0.0.1:${relayPort}`;
                    console.log("Relay URL: ", relayUrl);
                }
            }
            
            // Resolve when we have both ports
            if (bootstrapPort && relayPort) {
                resolve();
            }
        });
        
        servicesProcess.stderr!.on('data', (data) => {
            console.log("Bootstrap server stderr: ", data.toString());
        });
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

    const execOptions = {
        maxBuffer: 100 * 1024 * 1024, // 100MB instead of 1MB
    }

    const relayUrlArg = relayUrl ? `--hc-relay-url ${relayUrl}` : '';

    if (!adminCredential) {
        executorProcess = exec(`${command} run --app-data-path ${dataPath} --gql-port ${gqlPort} --hc-admin-port ${hcAdminPort} --hc-app-port ${hcAppPort} --hc-proxy-url ${proxyUrl} --hc-bootstrap-url ${bootstrapUrl} ${relayUrlArg} --hc-use-bootstrap true --hc-use-proxy true --hc-use-local-proxy true --hc-use-mdns true --language-language-only ${languageLanguageOnly} --run-dapp-server false`, execOptions)
    } else {
        executorProcess = exec(`${command} run --app-data-path ${dataPath} --gql-port ${gqlPort} --hc-admin-port ${hcAdminPort} --hc-app-port ${hcAppPort} --hc-proxy-url ${proxyUrl} --hc-bootstrap-url ${bootstrapUrl} ${relayUrlArg} --hc-use-bootstrap true --hc-use-proxy true --hc-use-local-proxy true --hc-use-mdns true --language-language-only ${languageLanguageOnly} --admin-credential ${adminCredential} --run-dapp-server false`, execOptions)
    }
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