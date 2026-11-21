/**
 * A headless executor started from a config file (`ad4m-executor run
 * --config <file>`): the file's ports, TLS, MCP and multi-user settings take
 * effect, the admin credential comes from AD4M_ADMIN_CREDENTIAL_FILE, and
 * a restarted executor unlocks its agent from the keyring without anyone
 * calling agent.unlock (AD4M_UNLOCK_PASSPHRASE_FILE is retired: it is not
 * read and a warning says so). A config file holding a secret value,
 * an empty admin credential file, or an empty AD4M_<FLAG> variable stops
 * `run`. Holochain is off (AD4M_RUN_HOLOCHAIN=false): the conductor is not
 * what these settings are about.
 */
import path from "path";
import os from "os";
import fs from "fs";
import https from "https";
import { execFileSync, spawn, ChildProcess } from "child_process";
import { fileURLToPath } from "url";
import { Ad4mClient } from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { baseUrl, executorBinary, pollUntil, quitExecutor, spawnExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { initializeMcp } from "./mcp-utils";

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const BOOTSTRAP_SEED = path.join(__dirname, "..", "bootstrapSeed.json");

const ADMIN_CREDENTIAL = "config-file-test-admin";
const PASSPHRASE = "config-file-test-passphrase";

function writeSecret(file: string, content: string) {
    fs.writeFileSync(file, content, { mode: 0o600 });
    fs.chmodSync(file, 0o600);
}

function httpsGet(url: string, ca: string): Promise<string> {
    return new Promise((resolve, reject) => {
        https
            .get(url, { ca }, (res) => {
                let body = "";
                res.on("data", (chunk) => (body += chunk));
                res.on("end", () => resolve(body));
            })
            .on("error", reject);
    });
}

describe("Executor config file", () => {
    let dir: string;
    let configPath: string;
    let adminCredentialFile: string;
    let cert: string;
    let apiPort: number, hcAdminPort: number, hcAppPort: number, mcpPort: number, tlsPort: number;
    let executor: ChildProcess | null = null;
    let output = "";

    async function start(env: Record<string, string> = {}): Promise<ChildProcess> {
        output = "";
        return spawnExecutor(["run", "--config", configPath], apiPort, {
            enableMcp: true,
            env: {
                AD4M_RUN_HOLOCHAIN: "false",
                AD4M_ADMIN_CREDENTIAL_FILE: adminCredentialFile,
                ...env,
            },
            onOutput: (text) => (output += text),
        });
    }

    async function stop() {
        if (executor) {
            await quitExecutor(executor, apiPort, ADMIN_CREDENTIAL);
            executor = null;
        }
    }

    before(async () => {
        [apiPort, hcAdminPort, hcAppPort, mcpPort, tlsPort] = await getFreePorts(5);
        registerPorts([apiPort, hcAdminPort, hcAppPort, mcpPort, tlsPort]);

        dir = fs.mkdtempSync(path.join(os.tmpdir(), "ad4m-cfg-"));
        const dataPath = path.join(dir, "data");
        execFileSync(executorBinary(), [
            "init", "--data-path", dataPath, "--network-bootstrap-seed", BOOTSTRAP_SEED,
        ]);

        const certFile = path.join(dir, "cert.pem");
        const keyFile = path.join(dir, "key.pem");
        execFileSync("openssl", [
            "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
            "-keyout", keyFile, "-out", certFile,
            "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1",
        ], { stdio: "ignore" });
        cert = fs.readFileSync(certFile, "utf8");

        const secrets = path.join(dir, "secrets");
        fs.mkdirSync(secrets, { mode: 0o700 });
        adminCredentialFile = path.join(secrets, "admin-credential");
        writeSecret(adminCredentialFile, `${ADMIN_CREDENTIAL}\n`);

        configPath = path.join(dir, "executor-config.json");
        fs.writeFileSync(configPath, JSON.stringify({
            app_data_path: dataPath,
            port: apiPort,
            hc_admin_port: hcAdminPort,
            hc_app_port: hcAppPort,
            run_dapp_server: false,
            multi_user_config: {
                enabled: true,
                smtp_config: null,
                tls_config: {
                    enabled: true, cert_file_path: certFile, key_file_path: keyFile, tls_port: tlsPort,
                },
            },
            log_config: { rust_executor: "info" },
            mcp_enabled: true,
            mcp_port: mcpPort,
        }, null, 2));
    });

    after(async () => {
        await stop();
        deregisterPorts([apiPort, hcAdminPort, hcAppPort, mcpPort, tlsPort]);
        fs.rmSync(dir, { recursive: true, force: true });
    });

    it("starts with the file's ports, multi-user, TLS and MCP, and the admin credential from its file", async () => {
        executor = await start();

        const admin = new Ad4mClient(baseUrl(apiPort), ADMIN_CREDENTIAL);
        await pollUntil(async () => { await admin.agent.status(); return true; },
            { timeoutMs: 15000, label: "executor API ready" });
        await admin.agent.generate(PASSPHRASE);
        expect(await admin.runtime.multiUserEnabled()).to.be.true;

        const anonymous = new Ad4mClient(baseUrl(apiPort));
        await expect(anonymous.agent.status()).to.be.rejectedWith("Capability is not matched");

        const health = await httpsGet(`https://localhost:${tlsPort}/health`, cert);
        expect(JSON.parse(health)).to.deep.equal({ status: "ok" });

        const mcp = await initializeMcp(`http://127.0.0.1:${mcpPort}/mcp`);
        expect(mcp.serverInfo).to.exist;

        admin.close();
        anonymous.close();
    });

    it("unlocks its agent at startup from the keyring, with no passphrase file", async () => {
        await stop();

        // The agent was generated with a keyring: its passphrase comes from the keyring,
        // and nobody types it again.
        executor = await start();
        const client = new Ad4mClient(baseUrl(apiPort), ADMIN_CREDENTIAL);
        // The wallet unlocks first; the log line follows once the system
        // languages are loaded.
        await pollUntil(async () => (await client.agent.status()).isUnlocked,
            { timeoutMs: 60000, label: "agent unlocked from the keyring" });
        await pollUntil(() => output.includes("Agent unlocked at startup from the keyring"),
            { timeoutMs: 60000, label: "the startup unlock is logged" });
        // The lines themselves, so a failure shows which service failed.
        expect(output.split("\n").filter((line) => line.includes("starting its services failed")))
            .to.deep.equal([]);
        expect(output).to.not.include(PASSPHRASE);
        // agent.lock and agent.unlock still pair up on the caller's passphrase.
        await client.agent.lock("a-lock-passphrase");
        expect((await client.agent.status()).isUnlocked).to.be.false;
        await expect(client.agent.unlock("not-the-lock-passphrase", false)).to.be.rejected;
        await client.agent.unlock("a-lock-passphrase", false);
        expect((await client.agent.status()).isUnlocked).to.be.true;
        client.close();
    });

    it("needs AD4M_UNLOCK_PASSPHRASE_FILE no more, and says so", async () => {
        await stop();
        const passphraseFile = path.join(dir, "secrets", "unlock-passphrase");

        // Whatever the file holds, the keyring unlocks the agent; the file is not read.
        writeSecret(passphraseFile, "not-the-passphrase\n");
        executor = await start({ AD4M_UNLOCK_PASSPHRASE_FILE: passphraseFile });
        const client = new Ad4mClient(baseUrl(apiPort), ADMIN_CREDENTIAL);
        await pollUntil(async () => (await client.agent.status()).isUnlocked,
            { timeoutMs: 60000, label: "agent unlocked from the keyring" });
        await pollUntil(() => output.includes("AD4M_UNLOCK_PASSPHRASE_FILE is no longer needed"),
            { timeoutMs: 60000, label: "the retired variable is logged" });
        expect(output).to.not.include("not-the-passphrase");
        expect(output).to.not.include("Unlocking the agent at startup with the passphrase");
        client.close();
    });

    it("refuses a config file with a secret value in it", async () => {
        await stop();
        const inline = path.join(dir, "inline-secret.json");
        fs.writeFileSync(inline, JSON.stringify({ admin_credential: "inline-secret-value" }));
        const proc = spawn(executorBinary(), ["run", "--config", inline], {
            stdio: ["ignore", "pipe", "pipe"],
        });
        let stderr = "";
        proc.stderr!.on("data", (d) => (stderr += d.toString()));
        proc.stdout!.on("data", (d) => (stderr += d.toString()));
        const code = await new Promise<number | null>((resolve) => proc.once("close", resolve));
        // 78: a refused start, which a supervisor does not retry.
        expect(code).to.equal(78);
        expect(stderr).to.include("admin_credential is not allowed in the config file");
        expect(stderr).to.not.include("inline-secret-value");
    });

    it("refuses an empty admin credential file", async () => {
        await stop();
        // An empty credential would match the empty token of a client that
        // sends none, and give it admin access.
        const empty = path.join(dir, "secrets", "empty-admin-credential");
        writeSecret(empty, "\n");
        const proc = spawn(executorBinary(), ["run", "--config", configPath], {
            stdio: ["ignore", "pipe", "pipe"],
            env: { ...process.env, AD4M_RUN_HOLOCHAIN: "false", AD4M_ADMIN_CREDENTIAL_FILE: empty },
        });
        let stderr = "";
        proc.stderr!.on("data", (d) => (stderr += d.toString()));
        proc.stdout!.on("data", (d) => (stderr += d.toString()));
        const code = await new Promise<number | null>((resolve) => proc.once("close", resolve));
        // 78: a refused start, which a supervisor does not retry.
        expect(code).to.equal(78);
        expect(stderr).to.include(`the secret in ${empty} is empty`);
    });

    it("refuses an empty AD4M_APP_DATA_PATH instead of laying it over the file", async () => {
        await stop();
        // `AD4M_APP_DATA_PATH=${DATA_DIR}` with DATA_DIR unset would
        // otherwise put the data directory relative to the working directory.
        const proc = spawn(executorBinary(), ["run", "--config", configPath], {
            stdio: ["ignore", "pipe", "pipe"],
            env: { ...process.env, AD4M_RUN_HOLOCHAIN: "false", AD4M_APP_DATA_PATH: "" },
        });
        let stderr = "";
        proc.stderr!.on("data", (d) => (stderr += d.toString()));
        proc.stdout!.on("data", (d) => (stderr += d.toString()));
        const code = await new Promise<number | null>((resolve) => proc.once("close", resolve));
        // 78: a refused start, which a supervisor does not retry.
        expect(code).to.equal(78);
        expect(stderr).to.include("AD4M_APP_DATA_PATH is set but empty");
    });
});
