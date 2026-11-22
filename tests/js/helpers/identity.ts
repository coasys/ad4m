// The identity suite's tools: a real identity-store on a temp SQLite file, the test holder
// (ad4m-identity-client's `holder` example: the client's flows with master keys in memory),
// the test vouching service (identity-store's `vouch_service` example) and
// `ad4m-executor claim`. The store and the holder are built with cargo in the repo root,
// as the agent language suites build the store (bootstrap-languages/centralized-agent-language).
import { ChildProcess, execSync, spawn } from "child_process";
import { copyFileSync, existsSync, mkdirSync, rmSync } from "fs";
import path from "path";
import readline from "readline";
import { fileURLToPath } from "url";
import { executorBinary, stopChildProcess } from "../utils/utils";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
export const REPO_ROOT = path.resolve(__dirname, "..", "..", "..");

function targetDir(): string {
    return process.env.CARGO_TARGET_DIR
        ? path.resolve(process.env.CARGO_TARGET_DIR)
        : path.join(REPO_ROOT, "target");
}

const storeBinary = () => path.join(targetDir(), "release", "identity-store");
const holderBinary = () => path.join(targetDir(), "release", "examples", "holder");
const vouchBinary = () => path.join(targetDir(), "release", "examples", "vouch_service");

/**
 * Release builds of `identity-store`, the test holder and the test vouching service. CI's `build` job builds and stashes
 * them (the integration runners compile no Rust); locally this builds them when missing.
 */
export function buildIdentityTools() {
    // Always ask cargo: it rebuilds only what changed. A binary that merely exists can be one an
    // older checkout built into the same target, and its holder lacks this branch's commands.
    const opts = { cwd: REPO_ROOT, stdio: "inherit" as const };
    // One invocation with `ad4m`, as CI builds them: only `ad4m`'s graph vendors OpenSSL.
    execSync(
        "cargo build --release -p ad4m -p identity-store -p ad4m-identity-client " +
            "--bin identity-store --example holder --example vouch_service " +
            "--features ad4m-identity-client/ws,ad4m-identity-client/test-vault",
        opts,
    );
}

/**
 * The test vouching service: it implements the platform's redeem contract
 * (`POST /api/vouch/redeem`) and signs notes with the test vouching key. A store started with
 * `vouch` set to it redeems the tokens given here.
 */
export class VouchService {
    url = "";
    key = "";
    credential = "";
    private child: ChildProcess;
    private lines: readline.Interface;
    private answers: string[] = [];
    private waiting: ((line: string) => void)[] = [];

    private constructor() {
        this.child = spawn(vouchBinary(), [], { stdio: ["pipe", "pipe", "inherit"] });
        this.lines = readline.createInterface({ input: this.child.stdout! });
        this.lines.on("line", (line) => {
            const next = this.waiting.shift();
            if (next) next(line);
            else this.answers.push(line);
        });
    }

    static async start(): Promise<VouchService> {
        const service = new VouchService();
        // "vouch-service listening on <url> key <multikey> credential <credential>"
        const ready = await service.next(30_000);
        const [, url, key, credential] = ready.match(/listening on (\S+) key (\S+) credential (\S+)/)!;
        service.url = url;
        service.key = key;
        service.credential = credential;
        return service;
    }

    private next(timeoutMs: number): Promise<string> {
        if (this.answers.length) return Promise.resolve(this.answers.shift()!);
        return new Promise<string>((resolve, reject) => {
            const timer = setTimeout(() => reject(new Error("vouch-service did not answer")), timeoutMs);
            this.waiting.push((line) => {
                clearTimeout(timer);
                resolve(line);
            });
        });
    }

    private async send(command: Record<string, unknown>): Promise<any> {
        this.child.stdin!.write(JSON.stringify(command) + "\n");
        const answer = JSON.parse(await this.next(10_000));
        if (answer.error) throw new Error(answer.error);
        return answer.ok;
    }

    /** Gives `token` for a check of `email`: its first redeem vouches the DID it names. */
    async give(token: string, email: string) {
        await this.send({ op: "give", token, email });
    }

    /** How many redeems reached the service. */
    async redeems(): Promise<number> {
        return this.send({ op: "redeems" });
    }

    async stop() {
        this.child.stdin!.end();
        await stopChildProcess(this.child);
    }
}

/** What a store is started with. */
export interface StoreOptions {
    /** The vouching service whose tokens the store redeems; without it every token is `503`. */
    vouch?: VouchService;
}

/** A running identity-store. `restart` keeps its port, key and database file. */
export class IdentityStore {
    url = "";
    key = "";
    private process: ChildProcess | null = null;

    constructor(readonly dir: string, readonly port: number, readonly options: StoreOptions = {}) {}

    get dbPath(): string {
        return path.join(this.dir, "store.sqlite");
    }

    static async start(dir: string, port: number, options: StoreOptions = {}): Promise<IdentityStore> {
        rmSync(dir, { recursive: true, force: true });
        mkdirSync(dir, { recursive: true });
        const store = new IdentityStore(dir, port, options);
        await store.run();
        return store;
    }

    private async run() {
        const binary = storeBinary();
        const child = spawn(binary, ["serve"], {
            stdio: ["ignore", "pipe", "pipe"],
            env: {
                ...process.env,
                IDENTITY_STORE_DB: this.dbPath,
                IDENTITY_STORE_KEY: path.join(this.dir, "store-key"),
                IDENTITY_STORE_LISTEN: `127.0.0.1:${this.port}`,
                POW_BITS: "8",
                // A suite makes an identity's writes (offers, inbox puts, record writes, posts)
                // within seconds; the store's per-identity bucket is not under test here.
                IDENTITY_BURST: "100",
                // Every identity of a suite starts its log with no vouching note, all from
                // 127.0.0.1; the brake's buckets have their own store tests.
                BRAKE_ADDR_BURST: "100000",
                BRAKE_GLOBAL_PER_HOUR: "100000",
                ...(this.options.vouch
                    ? {
                          VOUCH_URL: this.options.vouch.url,
                          VOUCH_CREDENTIAL: this.options.vouch.credential,
                          VOUCH_KEYS: this.options.vouch.key,
                      }
                    : {}),
            },
        });
        child.stderr!.on("data", (d) => console.log(`[identity-store] ${d}`));
        const ready = await new Promise<string>((resolve, reject) => {
            const timer = setTimeout(() => reject(new Error("identity-store did not start in 30 s")), 30_000);
            const lines = readline.createInterface({ input: child.stdout! });
            lines.on("line", (line) => {
                console.log(`[identity-store] ${line}`);
                if (line.startsWith("identity-store listening on ")) {
                    clearTimeout(timer);
                    resolve(line);
                }
            });
            child.once("exit", (code) => {
                clearTimeout(timer);
                reject(new Error(`identity-store exited with ${code}`));
            });
        });
        // "identity-store listening on http://<addr> store-key <multikey>"
        const [, url, key] = ready.match(/listening on (\S+) store-key (\S+)/)!;
        this.url = url;
        this.key = key;
        this.process = child;
    }

    async stop() {
        if (this.process) {
            await stopChildProcess(this.process);
            this.process = null;
        }
    }

    async restart() {
        await this.stop();
        await this.run();
    }

    /** Copies the database (stopped store) to `to`. */
    saveDb(to: string) {
        for (const suffix of ["", "-wal", "-shm"]) {
            rmSync(to + suffix, { force: true });
            if (existsSync(this.dbPath + suffix)) copyFileSync(this.dbPath + suffix, to + suffix);
        }
    }

    /** Puts the copy `from` back in place of the database (stopped store). */
    restoreDb(from: string) {
        for (const suffix of ["", "-wal", "-shm"]) {
            rmSync(this.dbPath + suffix, { force: true });
            if (existsSync(from + suffix)) copyFileSync(from + suffix, this.dbPath + suffix);
        }
    }

    /** The executor settings that point an executor at this store. */
    executorEnv(): Record<string, string> {
        return {
            AD4M_IDENTITY_STORE_URL: this.url,
            AD4M_IDENTITY_STORE_KEYS: this.key,
        };
    }
}

/** The test holder: the client's flows over JSON lines. */
export class Holder {
    private child: ChildProcess;
    private lines: readline.Interface;
    private answers: string[] = [];
    private waiting: ((line: string) => void)[] = [];

    constructor() {
        const binary = holderBinary();
        this.child = spawn(binary, [], { stdio: ["pipe", "pipe", "inherit"] });
        this.lines = readline.createInterface({ input: this.child.stdout! });
        this.lines.on("line", (line) => {
            const next = this.waiting.shift();
            if (next) next(line);
            else this.answers.push(line);
        });
    }

    /** Sends one command; resolves with its answer, rejects with its `error`. */
    async send(command: Record<string, unknown>): Promise<any> {
        this.child.stdin!.write(JSON.stringify(command) + "\n");
        const line = this.answers.length
            ? this.answers.shift()!
            : await new Promise<string>((resolve) => this.waiting.push(resolve));
        const answer = JSON.parse(line);
        // `null` is an answer too (`takeMaster` with none waiting, `record` before the first write).
        if (answer?.error) throw new Error(answer.error);
        return answer;
    }

    async stop() {
        this.child.stdin!.end();
        await stopChildProcess(this.child);
    }
}

/** `ad4m-executor claim <offer>` on the executor at `port`, running until it ends. */
export class ClaimCommand {
    readonly output: string[] = [];
    readonly exited: Promise<number | null>;
    private child: ChildProcess;

    constructor(port: number, offer: string, hint?: string) {
        const args = ["--executor-url", `http://127.0.0.1:${port}`, "claim", offer];
        if (hint) args.push("--hint", hint);
        this.child = spawn(executorBinary(), args, { stdio: ["ignore", "pipe", "pipe"] });
        const take = (d: Buffer) => {
            const text = d.toString();
            console.log(`[claim] ${text}`);
            this.output.push(...text.split("\n").filter((l) => l.trim()));
        };
        this.child.stdout!.on("data", take);
        this.child.stderr!.on("data", take);
        this.exited = new Promise((resolve) => this.child.once("close", (code) => resolve(code)));
    }

    /** The match code it printed, once it printed one. */
    async matchCode(timeoutMs = 60_000): Promise<string> {
        const start = Date.now();
        while (Date.now() - start < timeoutMs) {
            const line = this.output.find((l) => l.startsWith("Match code: "));
            if (line) return line.slice("Match code: ".length).trim();
            if (this.child.exitCode !== null) break;
            await new Promise((r) => setTimeout(r, 200));
        }
        throw new Error(`claim printed no match code: ${this.output.join(" | ")}`);
    }
}
