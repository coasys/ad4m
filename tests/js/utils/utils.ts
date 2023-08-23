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

export async function startExecutor(dataPath: string, 
    bootstrapSeedPath: string, 
    gqlPort: number,
    hcAdminPort: number,
    hcAppPort: number,
    ipfsSwarmPort: number,
    languageLanguageOnly: boolean = false,
    adminCredential?: string
): Promise<ChildProcess> {
    const command = path.resolve(__dirname, '..', '..', '..','target', 'release', 'ad4m');

    console.log(bootstrapSeedPath);
    console.log(dataPath);
    let executorProcess = null as ChildProcess | null;
    rmSync(dataPath, { recursive: true, force: true })
    execSync(`${command} init --data-path ${dataPath} --network-bootstrap-seed ${bootstrapSeedPath}`, {cwd: process.cwd()})
    
    console.log("Starting executor")
    try {
        execSync("killall ad4m")
    } catch (e) {
        console.log("No ad4m process running")
    }

    console.log(`Current directory: ${process.cwd()}`);
    const hc = path.resolve(__dirname, '..', 'tst-tmp', 'agents', 'p','binary', 'hc');
    let servicesProcess = exec(`${hc} run-local-services`);

    let bootstrapUrl = null;
    let proxyUrl = null;

    let servicesReady = new Promise<void>((resolve, reject) => {
        servicesProcess.stdout!.on('data', (data) => {
            if (data.includes("HC BOOTSTRAP - ADDR")) {
                bootstrapUrl = data.split(" ")[5];
                bootstrapUrl = bootstrapUrl.substring(0, bootstrapUrl.length - 3);
            }

            if (data.includes("HC SIGNAL - ADDR")) {
                proxyUrl = data.split(" ")[5];
                proxyUrl = proxyUrl.substring(0, proxyUrl.length - 3);
                resolve();
            }
        });
    });

    await servicesReady;

    console.log("USING LOCAL BOOTSTRAP & PROXY URL: ", bootstrapUrl, proxyUrl);
    
    if (!adminCredential) {
        executorProcess = exec(`${command} run --app-data-path ${dataPath} --gql-port ${gqlPort} --hc-admin-port ${hcAdminPort} --hc-app-port ${hcAppPort} --ipfs-swarm-port ${ipfsSwarmPort} --hc-proxy-url ${proxyUrl} --hc-bootstrap-url ${bootstrapUrl} --hc-use-bootstrap false --hc-use-proxy true --hc-use-local-proxy true --hc-use-mdns true --language-language-only ${languageLanguageOnly} --run-dapp-server false`, {})
    } else {
        executorProcess = exec(`${command} run --app-data-path ${dataPath} --gql-port ${gqlPort} --hc-admin-port ${hcAdminPort} --hc-app-port ${hcAppPort} --ipfs-swarm-port ${ipfsSwarmPort} --hc-proxy-url ${proxyUrl} --hc-bootstrap-url ${bootstrapUrl} --hc-use-bootstrap false --hc-use-proxy true --hc-use-local-proxy true --hc-use-mdns true --language-language-only ${languageLanguageOnly} --admin-credential ${adminCredential} --run-dapp-server false`, {})
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