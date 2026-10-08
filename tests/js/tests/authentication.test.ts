import path from "path";
import { Ad4mClient, AuthInfoInput, CapabilityInput } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { baseUrl, sleep, startExecutor, quitExecutor, pollUntil, waitForExit, stopChildProcess } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess, execFileSync, spawn } from 'node:child_process';
import type { EventMap } from "@coasys/ad4m";
import { callMcpTool, initializeMcp } from './mcp-utils';

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Authentication integration tests", () => {
    // Secure by default (#1215): `run` without an admin credential and without
    // --insecure-no-admin-credential must exit, not serve every caller as admin.
    // startExecutor() passes the testing flag whenever it gets no credential, so
    // these cases spawn the binary directly.
    describe("executor refuses to start without an admin credential", () => {
        const executorBin = path.resolve(__dirname, "..", "..", "..", "target", "release", "ad4m-executor");
        const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
        const appDataPath = path.join(`${__dirname}/../tst-tmp`, "agents", "no-credential-agent");

        before(() => {
            fs.removeSync(appDataPath);
            fs.mkdirSync(appDataPath, { recursive: true });
            execFileSync(executorBin, ["init", "--data-path", appDataPath, "--network-bootstrap-seed", bootstrapSeedPath]);
        })

        after(() => {
            fs.removeSync(appDataPath);
        })

        async function runWithoutCredential(extraArgs: string[], extraEnv: Record<string, string> = {}) {
            const [apiPort] = await getFreePorts(1);
            const childEnv: NodeJS.ProcessEnv = { ...process.env };
            delete childEnv.AD4M_ADMIN_CREDENTIAL;
            delete childEnv.AD4M_ADMIN_CREDENTIAL_FILE;
            delete childEnv.AD4M_INSECURE_NO_ADMIN_CREDENTIAL;
            Object.assign(childEnv, extraEnv);
            const child = spawn(executorBin, [
                "run",
                "--app-data-path", appDataPath,
                "--port", String(apiPort),
                "--run-dapp-server", "false",
                "--run-holochain", "false",
                ...extraArgs,
            ], { stdio: ["ignore", "pipe", "pipe"], env: childEnv });
            let output = "";
            child.stdout!.on("data", (d) => { output += d.toString(); });
            child.stderr!.on("data", (d) => { output += d.toString(); });
            // The check runs before any service starts, so the exit is quick;
            // 60 s leaves room for a loaded CI box.
            const exited = await waitForExit(child, 60000);
            if (!exited) await stopChildProcess(child);
            return { exited, code: child.exitCode, output };
        }

        function expectRefusal(result: { exited: boolean, code: number | null, output: string }) {
            expect(result.exited, `executor kept running:\n${result.output.slice(-2000)}`).to.be.true;
            expect(result.code).to.not.equal(0);
            expect(result.output).to.contain("no admin credential");
            // The message names both ways out.
            expect(result.output).to.contain("--admin-credential");
            expect(result.output).to.contain("AD4M_ADMIN_CREDENTIAL");
            expect(result.output).to.contain("--insecure-no-admin-credential");
        }

        it("run without a credential and without the testing flag exits with an error", async () => {
            expectRefusal(await runWithoutCredential([]));
        })

        // An empty credential never becomes a credential: the CLI rejects
        // the empty value itself and names where it came from (a library
        // caller's Some("") is turned into None by Ad4mConfig::prepare()).
        it("an empty credential is refused, naming the flag or the variable", async () => {
            const flag = await runWithoutCredential(["--admin-credential", ""]);
            expect(flag.exited, `executor kept running:\n${flag.output.slice(-2000)}`).to.be.true;
            expect(flag.code).to.not.equal(0);
            expect(flag.output).to.contain("--admin-credential needs a value");

            const variable = await runWithoutCredential([], { AD4M_ADMIN_CREDENTIAL: "" });
            expect(variable.exited, `executor kept running:\n${variable.output.slice(-2000)}`).to.be.true;
            expect(variable.code).to.not.equal(0);
            expect(variable.output).to.contain("AD4M_ADMIN_CREDENTIAL is set but empty");
        })

        // With the testing flag and no credential, MCP binds loopback and
        // lets a tokenless caller read, as REST does.
        it("without a credential the testing flag opens MCP to a tokenless caller on loopback", async () => {
            const [apiPort, mcpPort] = await getFreePorts(2);
            registerPorts([apiPort, mcpPort]);
            const childEnv = { ...process.env };
            delete childEnv.AD4M_ADMIN_CREDENTIAL;
            delete childEnv.AD4M_INSECURE_NO_ADMIN_CREDENTIAL;
            delete childEnv.MCP_HOST;
            const child = spawn(executorBin, [
                "run",
                "--app-data-path", appDataPath,
                "--port", String(apiPort),
                "--run-dapp-server", "false",
                "--run-holochain", "false",
                "--insecure-no-admin-credential",
                "--enable-mcp", "true",
                "--mcp-port", String(mcpPort),
            ], { stdio: ["ignore", "pipe", "pipe"], env: childEnv });
            let output = "";
            child.stdout!.on("data", (d) => { output += d.toString(); });
            child.stderr!.on("data", (d) => { output += d.toString(); });
            try {
                await pollUntil(async () => output.includes("MCP HTTP server listening"),
                    { timeoutMs: 120000, label: "MCP server listening" });
                expect(output).to.contain(`MCP HTTP server listening on 127.0.0.1:${mcpPort}`);

                // REST: the empty token is the operator.
                const client = new Ad4mClient(`http://127.0.0.1:${apiPort}`, "");
                await pollUntil(async () => { await client.agent.status(); return true; },
                    { timeoutMs: 15000, label: "executor API ready" });
                await client.agent.generate("test-passphrase");
                const perspective = await client.perspective.add("empty-credential");

                // MCP: the same tokenless caller reads that perspective.
                const mcpUrl = `http://127.0.0.1:${mcpPort}/mcp`;
                const { sessionId } = await initializeMcp(mcpUrl);
                const links = await callMcpTool(mcpUrl, "query_links",
                    { perspective_id: perspective.uuid }, sessionId);
                expect(links, JSON.stringify(links)).to.be.an("array");
            } finally {
                await stopChildProcess(child);
                deregisterPorts([apiPort, mcpPort]);
            }
        })
    })

    describe("admin credential is not set", () => {
        const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
        const appDataPath = path.join(TEST_DIR, "agents", "unauth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
        let apiPort: number;
        let hcAdminPort: number;
        let hcAppPort: number;

        let executorProcess: ChildProcess | null = null
        let ad4mClient: Ad4mClient | null = null

        before(async () => {
            [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
            registerPorts([apiPort, hcAdminPort, hcAppPort]);
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
                apiPort, hcAdminPort, hcAppPort);

            // Retry the very first RPC call on a fresh connection.
            //
            // Observed flake (2026-09-02): the executor's readiness marker
            // (which startExecutor already waited for above) can fire
            // slightly before the WS upgrade path is actually stable under
            // CI resource pressure (multiple parallel ci-workdir executors
            // competing for CPU) — the socket opens, this first call goes
            // out, then the connection drops before a reply arrives,
            // surfacing as `RpcError 503: WebSocket connection closed`.
            // mocha's `this.retries()` doesn't retry `before()` hooks, so
            // retry manually here instead. A fresh Ad4mClient per attempt
            // avoids relying on the previous instance's half-torn-down
            // socket/reconnect state.
            let lastErr: unknown
            for (let attempt = 1; attempt <= 3; attempt++) {
                ad4mClient = new Ad4mClient(baseUrl(apiPort))
                try {
                    await ad4mClient.agent.generate("passphrase")
                    lastErr = null
                    break
                } catch (e) {
                    lastErr = e
                    console.log(`agent.generate attempt ${attempt}/3 failed, retrying:`, e)
                    await sleep(1000)
                }
            }
            if (lastErr) throw lastErr
        })

        after(async () => {
            if (executorProcess) {
                await quitExecutor(executorProcess, apiPort);
            }
            deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
        })

        it("unauthenticated user has all the capabilities", async () => {
            let status = await ad4mClient!.agent.status()
            expect(status.isUnlocked).to.be.true;

            let requestId = await ad4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            expect(requestId).match(/.+/);

            let rand = await ad4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            expect(rand).match(/\d+/);

            let jwt = await ad4mClient!.agent.generateJwt(requestId, rand)
            expect(jwt).match(/.+/);
        })
    })

    describe("admin credential is set", () => {
        const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
        const appDataPath = path.join(TEST_DIR, "agents", "auth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
        let apiPort: number;
        let hcAdminPort: number;
        let hcAppPort: number;

        let executorProcess: ChildProcess | null = null
        let adminAd4mClient: Ad4mClient | null = null
        let unAuthenticatedAppAd4mClient: Ad4mClient | null = null

        before(async () => {
            [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
            registerPorts([apiPort, hcAdminPort, hcAppPort]);
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
                apiPort, hcAdminPort, hcAppPort, false, "123");
       
            adminAd4mClient = new Ad4mClient(baseUrl(apiPort), "123")
            await adminAd4mClient.agent.generate("passphrase")
            
            unAuthenticatedAppAd4mClient = new Ad4mClient(baseUrl(apiPort))
        })

        after(async () => {
            if (executorProcess) {
                await quitExecutor(executorProcess, apiPort, "123");
            }
            deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
        })

        it("unauthenticated user can not query agent status", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Capability is not matched");
        })

        it("unauthenticated user can request capability", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                    appName: "demo-app",
                    appDesc: "demo-desc",
                    appDomain: "test.ad4m.org",
                    appUrl: "https://demo-link",
                    capabilities: [
                        {
                            with: {
                                domain:"agent",
                                pointers:["*"]
                            },
                            can: ["READ"]
                        }
                    ] as CapabilityInput[]
                } as AuthInfoInput)
            }

            expect(await call()).to.be.ok.match(/.+/);
        })

        it("admin user can permit capability", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            const call = async () => {
                return await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            }

            expect(await call()).to.be.ok.match(/\d+/);
        })

        it("unauthenticated user can generate jwt with a secret", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)

            const call = async () => {
                return await adminAd4mClient!.agent.generateJwt(requestId, rand)
            }

            expect(await call()).to.be.ok.match(/.+/);
        })

        it("authenticated user can query agent status if capability matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(apiPort), jwt)
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).to.be.true;
        })

        it("user with invalid jwt can not query agent status", async () => {
            // @ts-ignore
            let ad4mClient = new Ad4mClient(baseUrl(apiPort), "invalid-jwt")

            const call = async () => {
                return await ad4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("InvalidToken");
        })

        it("authenticated user can not query agent status if capability is not matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["CREATE"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["CREATE"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(apiPort), jwt)

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Capability is not matched");
        })

        it("user with revoked token can not query agent status", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(apiPort), jwt)
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).to.be.true;

            let oldApps = await adminAd4mClient!.agent.getApps();
            let newApps = await adminAd4mClient!.agent.revokeToken(requestId);
            // revoking token should not change the number of apps
            expect(newApps.length).to.be.equal(oldApps.length);
            newApps.forEach((app, i) => {
                if(app.requestId === requestId) {
                    expect(app.revoked).to.be.true;
                }
            })

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Unauthorized access");
        })

        it("requesting a capability toke should trigger a CapabilityRequested exception", async () => {
            let excpetions: EventMap['exception-occurred']['exception'][] = [];
            adminAd4mClient!.on('exception-occurred', ({ exception }) => { excpetions.push(exception) })
            // Subscription-init delay: the subscription registered with the
            // callback does not wait for the server, and exceptions are not redelivered.
            await sleep(1000);

            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)

            await pollUntil(() => excpetions.length >= 1, { timeoutMs: 5000, label: "capability request exception fires" });

            expect(excpetions.length).to.be.equal(1);
            expect(excpetions[0].type).to.be.equal("CAPABILITY_REQUESTED");
            let auth_info = JSON.parse(excpetions[0].addon!);
            expect(auth_info.requestId).to.be.equal(requestId);
        })
    })
})
