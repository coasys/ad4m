import { ChildProcess, exec, ExecException, execSync } from "node:child_process";
import { rmSync } from "node:fs";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import { HttpLink } from "@apollo/client/link/http/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";

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

export async function startExecutor(relativeDataPath: string, 
    bootstrapSeedPath: string, 
    gqlPort: number,
    hcAdminPort: number,
    hcAppPort: number,
    ipfsSwarmPort: number,
    languageLanguageOnly: boolean = false,
    reqCredential?: string
): Promise<ChildProcess> {
    console.log(bootstrapSeedPath);
    let executorProcess = null as ChildProcess | null;
    rmSync(relativeDataPath, { recursive: true, force: true })
    console.log("Initialzing executor data directory")
    execSync(`../../host/dist/ad4m-macos-x64 init --dataPath ${relativeDataPath} --networkBootstrapSeed ${bootstrapSeedPath} --overrideConfig true`, {})
    
    console.log("Starting executor")
    try {
        execSync("killall holochain")
    } catch (e) {
        console.log("No holochain process running")
    }
    
    if (!reqCredential) {
        executorProcess = exec(`../../host/dist/ad4m-macos-x64 serve --dataPath ${relativeDataPath} --port ${gqlPort} --hcAdminPort ${hcAdminPort} --hcAppPort ${hcAppPort} --ipfsPort ${ipfsSwarmPort} --hcUseBootrap false --hcUseProxy false --hcUseLocalProxy false --hcUseMdns true --languageLanguageOnly ${languageLanguageOnly}`, {})
    } else {
        executorProcess = exec(`../../host/dist/ad4m-macos-x64 serve --dataPath ${relativeDataPath} --port ${gqlPort} --hcAdminPort ${hcAdminPort} --hcAppPort ${hcAppPort} --ipfsPort ${ipfsSwarmPort} --hcUseBootrap false --hcUseProxy false --hcUseLocalProxy false --hcUseMdns true --languageLanguageOnly ${languageLanguageOnly} --reqCredential ${reqCredential}`, {})
    }
    let executorReady = new Promise<void>((resolve, reject) => {
        executorProcess!.stdout!.on('data', (data) => {
            if (data.includes("GraphQL server started")) {
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
        url: `ws://localhost:${port}/graphql`,
        webSocketImpl: Websocket,
        connectionParams: () => {
            return {
                headers: {
                    authorization: token || ""
                }
            }
        },
    }));

    const link = new HttpLink({
        uri: "http://localhost:4000/graphql",
        //@ts-ignore
        fetch
    });
  
    return new ApolloClient({
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
}

export function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}