import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { sleep, startExecutor, killByPorts } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from 'node:child_process';
import { execFileSync } from 'node:child_process';

// Keep Node's native fetch for REST client calls. The node-fetch override here
// breaks web-stream/EventSource expectations used by the REST/MCP stack.

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * MCP mcporter Integration Tests
 *
 * Tests that mcporter CLI can connect to AD4M MCP server and use admin credential auth.
 * This ensures third-party MCP clients work with AD4M.
 */

let MCP_PORT: number;
let MCP_BASE_URL: string;

// ============================================================================
// Test Suite
// ============================================================================

describe("MCP mcporter Integration Tests", function() {
    this.timeout(180000);

    const TEST_DIR = path.join(__dirname + "/../tst-tmp");
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-mcporter-test");
    const bootstrapSeedPath = path.join(__dirname + "/../bootstrapSeed.json");
    // Unique ports for mcporter tests
    let gqlPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;
    const adminCredential = "mcporter-test-admin-credential-12345";

    let executorProcess: ChildProcess | null = null;
    let mcporterConfigPath: string;

    before(async () => {
        [gqlPort, hcAdminPort, hcAppPort, MCP_PORT] = await getFreePorts(4);
        MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;
        registerPorts([gqlPort, hcAdminPort, hcAppPort, MCP_PORT]);

        // Clean up and create test directory
        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        // Start executor with MCP enabled
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            gqlPort,
            hcAdminPort,
            hcAppPort,
            true,               // languageLanguageOnly
            adminCredential,
            undefined,
            undefined,
            undefined,
            true,               // enableMcp
            MCP_PORT,           // mcpPort
        );

        await sleep(3000);

        // Generate agent via REST
        const adminClient = new Ad4mClient(`http://127.0.0.1:${gqlPort}`, adminCredential, false);
        await adminClient.agent.generate("test-passphrase");
        console.log("Agent generated via REST");

        // Create mcporter config
        const mcporterDir = path.join(appDataPath, "mcporter-config");
        fs.mkdirSync(mcporterDir, { recursive: true });
        mcporterConfigPath = path.join(mcporterDir, "mcporter.json");
        
        const mcporterConfig = {
            mcpServers: {
                ad4m: {
                    baseUrl: MCP_BASE_URL,
                    headers: {
                        Authorization: `Bearer ${adminCredential}`
                    }
                }
            },
            imports: []
        };
        fs.writeFileSync(mcporterConfigPath, JSON.stringify(mcporterConfig, null, 2));
        console.log("mcporter config created at:", mcporterConfigPath);
    });

    after(async () => {
        if (executorProcess) {
            executorProcess.kill('SIGTERM');
            await sleep(1000);
            if (!executorProcess.killed) {
                executorProcess.kill('SIGKILL');
            }
        }
        killByPorts([gqlPort, hcAdminPort, hcAppPort, MCP_PORT]);
        deregisterPorts([gqlPort, hcAdminPort, hcAppPort, MCP_PORT]);
    });

    // ========================================================================
    // 1. mcporter Basic Connectivity
    // ========================================================================

    describe("1. mcporter Basic Connectivity", function() {
        it("should list AD4M server via mcporter", async function() {
            const result = execFileSync(
                "mcporter",
                ["list", "ad4m", "--config", mcporterConfigPath],
                { encoding: 'utf-8', timeout: 10000 }
            );
            expect(result).to.include("ad4m");
            console.log("mcporter list result:", result);
        });

        it("should list tools via mcporter", async function() {
            const result = execFileSync(
                "mcporter",
                ["list", "ad4m", "--schema", "--config", mcporterConfigPath],
                { encoding: 'utf-8', timeout: 10000 }
            );
            expect(result).to.include("list_perspectives");
            expect(result).to.include("add_perspective");
            console.log("mcporter tools listed successfully");
        });
    });

    // ========================================================================
    // 2. mcporter with Admin Credential Auth
    // ========================================================================

    describe("2. mcporter Admin Credential Auth", function() {
        it("should call list_perspectives with admin credential", async function() {
            const result = execFileSync(
                "mcporter",
                ["call", "ad4m.list_perspectives", "--config", mcporterConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            const parsed = JSON.parse(result);
            expect(parsed).to.be.an('array');
            console.log("mcporter list_perspectives result:", JSON.stringify(parsed));
        });

        it("should create a perspective via mcporter", async function() {
            const result = execFileSync(
                "mcporter",
                ["call", "ad4m.add_perspective", "name=mcporter-test-perspective", "--config", mcporterConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            const parsed = JSON.parse(result);
            expect(parsed.success).to.be.true;
            expect(parsed.uuid).to.be.a('string');
            console.log("mcporter add_perspective result:", JSON.stringify(parsed));
        });

        it("should get agent profile via mcporter", async function() {
            const result = execFileSync(
                "mcporter",
                ["call", "ad4m.get_agent_profile", "--config", mcporterConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            const parsed = JSON.parse(result);
            // Profile might be empty but should not error
            console.log("mcporter get_agent_profile result:", JSON.stringify(parsed));
        });
    });

    // ========================================================================
    // 3. mcporter Auth Failure Cases
    // ========================================================================

    describe("3. mcporter Auth Failure Cases", function() {
        it("should reject requests with wrong admin credential", async function() {
            const wrongConfigPath = path.join(appDataPath, "mcporter-config-wrong.json");
            const wrongConfig = {
                mcpServers: {
                    ad4m: {
                        baseUrl: MCP_BASE_URL,
                        headers: {
                            Authorization: "Bearer wrong-credential"
                        }
                    }
                },
                imports: []
            };
            fs.writeFileSync(wrongConfigPath, JSON.stringify(wrongConfig, null, 2));

            let result: string;
            try {
                result = execFileSync(
                    "mcporter",
                    ["call", "ad4m.list_perspectives", "--config", wrongConfigPath, "--output", "json"],
                    { encoding: 'utf-8', timeout: 10000 }
                );
            } catch (e: any) {
                // mcporter may exit with non-zero code on auth failure
                result = (e.stdout || e.stderr || e.message || "").toString();
            }
            console.log("mcporter list_perspectives with wrong credential result:", result);
            expect(result).to.include("Authentication required");
        });

        it("should reject requests without admin credential", async function() {
            const noAuthConfigPath = path.join(appDataPath, "mcporter-config-noauth.json");
            const noAuthConfig = {
                mcpServers: {
                    ad4m: {
                        baseUrl: MCP_BASE_URL,
                        headers: {}
                    }
                },
                imports: []
            };
            fs.writeFileSync(noAuthConfigPath, JSON.stringify(noAuthConfig, null, 2));

            let result: string;
            try {
                result = execFileSync(
                    "mcporter",
                    ["call", "ad4m.list_perspectives", "--config", noAuthConfigPath, "--output", "json"],
                    { encoding: 'utf-8', timeout: 10000 }
                );
            } catch (e: any) {
                result = (e.stdout || e.stderr || e.message || "").toString();
            }
            console.log("mcporter list_perspectives without credential result:", result);
            expect(result).to.include("Authentication required");
        });
    });

    // ========================================================================
    // 4. mcporter with JWT Auth (after capability request)
    // ========================================================================

    describe("4. mcporter JWT Auth Flow", function() {
        it("should authenticate via request_capability + generate_jwt", async function() {
            // Step 1: Request capability
            const capResult = execFileSync(
                "mcporter",
                ["call", "ad4m.request_capability", "app_name=mcporter-jwt-test", "app_desc=Testing JWT auth", "--config", mcporterConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            const capParsed = JSON.parse(capResult);
            expect(capParsed.request_id).to.be.a('string');
            expect(capParsed.code).to.be.a('string');
            console.log("Capability requested:", capParsed.request_id);

            // Step 2: Generate JWT
            const jwtArgs = JSON.stringify({request_id: capParsed.request_id, code: capParsed.code});
            const jwtResult = execFileSync(
                "mcporter",
                ["call", "ad4m.generate_jwt", "--args", jwtArgs, "--config", mcporterConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            const jwtParsed = JSON.parse(jwtResult);
            expect(jwtParsed.success).to.be.true;
            expect(jwtParsed.token).to.be.a('string');
            console.log("JWT generated successfully");

            // Step 3: Create new config with JWT
            const jwtConfigPath = path.join(appDataPath, "mcporter-config-jwt.json");
            const jwtConfig = {
                mcpServers: {
                    ad4m: {
                        baseUrl: MCP_BASE_URL,
                        headers: {
                            Authorization: `Bearer ${jwtParsed.token}`
                        }
                    }
                },
                imports: []
            };
            fs.writeFileSync(jwtConfigPath, JSON.stringify(jwtConfig, null, 2));

            // Step 4: Use JWT to call protected tool
            const listResult = execFileSync(
                "mcporter",
                ["call", "ad4m.list_perspectives", "--config", jwtConfigPath, "--output", "json"],
                { encoding: 'utf-8', timeout: 10000 }
            );
            console.log("mcporter list_perspectives with JWT auth result:", listResult);
            const listParsed = JSON.parse(listResult);
            expect(listParsed).to.be.an('array');
            console.log("mcporter with JWT auth works!");
        });
    });
});