// The identity suite's tools: a real identity-store on a temp SQLite file, the test holder
// (ad4m-identity-client's `holder` example: the client's flows with master keys in memory),
// and `ad4m-executor claim`. The store and the holder are built with cargo in the repo root,
// as the agent language suites build the store (bootstrap-languages/centralized-agent-language).
import { ChildProcess, execSync, spawn } from "child_process";
import { copyFileSync, existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from "fs";
import path from "path";
import readline from "readline";
import { fileURLToPath } from "url";
import { baseUrl, executorBinary, stopChildProcess } from "../utils/utils";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
export const REPO_ROOT = path.resolve(__dirname, "..", "..", "..");

function targetDir(): string {
    return process.env.CARGO_TARGET_DIR
        ? path.resolve(process.env.CARGO_TARGET_DIR)
        : path.join(REPO_ROOT, "target");
}

const storeBinary = () => path.join(targetDir(), "release", "identity-store");
const holderBinary = () => path.join(targetDir(), "release", "examples", "holder");

/**
 * Release builds of `identity-store` and the test holder. CI's `build` job builds and stashes
 * them (the integration runners compile no Rust); locally this builds them when missing.
 */
export function buildIdentityTools() {
    // Always ask cargo: it rebuilds only what changed. A binary that merely exists can be one an
    // older checkout built into the same target, and its holder lacks this branch's commands.
    const opts = { cwd: REPO_ROOT, stdio: "inherit" as const };
    // One invocation with `ad4m`, as CI builds them: only `ad4m`'s graph vendors OpenSSL.
    execSync(
        "cargo build --release -p ad4m -p identity-store -p ad4m-identity-client " +
            "--bin identity-store --example holder " +
            "--features ad4m-identity-client/ws,ad4m-identity-client/test-vault",
        opts,
    );
}

/**
 * Checks that `identity-store` and the test holder are built, without building them: suites that
 * only use them (the server-link suites) run on CI's stashed binaries and compile no Rust.
 */
export function requireIdentityTools() {
    for (const binary of [storeBinary(), holderBinary()]) {
        if (!existsSync(binary)) {
            throw new Error(
                `${binary} is missing: build it with \`cargo build --release -p ad4m -p identity-store ` +
                    "-p ad4m-identity-client --bin identity-store --example holder " +
                    "--features ad4m-identity-client/ws,ad4m-identity-client/test-vault` in the repo root",
            );
        }
    }
}

/** A running identity-store. `restart` keeps its port, key and database file. */
export class IdentityStore {
    url = "";
    key = "";
    private process: ChildProcess | null = null;

    constructor(readonly dir: string, readonly port: number) {}

    get dbPath(): string {
        return path.join(this.dir, "store.sqlite");
    }

    static async start(dir: string, port: number): Promise<IdentityStore> {
        rmSync(dir, { recursive: true, force: true });
        mkdirSync(dir, { recursive: true });
        const store = new IdentityStore(dir, port);
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

const TESTS_JS = path.resolve(__dirname, "..");

/**
 * Writes, at `to`, the local test seed (`tests/js/bootstrapSeed.json`) with what an executor
 * needs to speak for a person and make spaces: the centralized agent language (which resolves a
 * person through the identity store) as its agent language, and the server-link language among
 * its known link languages (`neighbourhood.create` templates it). Both addresses come from
 * `prepare-test-local` (`scripts/*-hash`). Returns `to`.
 */
export function writePersonSeed(to: string): string {
    const read = (name: string) => {
        const file = path.join(TESTS_JS, "scripts", name);
        const hash = existsSync(file) ? readFileSync(file, "utf8").trim() : "";
        if (!hash) throw new Error(`${file} is missing or empty: run prepare-test-local first`);
        return hash;
    };
    const seed = JSON.parse(readFileSync(path.join(TESTS_JS, "bootstrapSeed.json"), "utf8"));
    seed.agentLanguage = read("centralized-agent-language-hash");
    const serverLink = read("server-link-language-hash");
    seed.knownLinkLanguages = [...new Set([...(seed.knownLinkLanguages ?? []), serverLink])];
    mkdirSync(path.dirname(to), { recursive: true });
    writeFileSync(to, JSON.stringify(seed));
    return to;
}

/** A person made on an executor by the test holder, which keeps the person's master keys. */
export interface Person {
    /** The person's DID (`did:scid:coasys:1:…`). */
    did: string;
    /** The holder with the person's master keys, connected to the executor it was made on. */
    holder: Holder;
}

/**
 * Gives the executor at `apiPort` a person: the test holder connects to it (with `token`, the
 * admin credential or a user's token in multi-user mode) and runs the client's `create`, so the
 * executor's agent (that session's agent) becomes the person's first device key, `d1`. The
 * executor must run with an identity store (`IdentityStore.executorEnv()`) and an agent language
 * that knows people (`writePersonSeed`). Stop `person.holder` when the test is done.
 */
export async function givePerson(apiPort: number, token = ""): Promise<Person> {
    requireIdentityTools();
    const holder = new Holder();
    try {
        await holder.send({ cmd: "connect", url: baseUrl(apiPort), token });
        const created = await holder.send({ cmd: "create" });
        if (!/^did:scid:coasys:1:/.test(created.did ?? "")) {
            throw new Error(`the holder made no person: ${JSON.stringify(created)}`);
        }
        return { did: created.did, holder };
    } catch (e) {
        await holder.stop();
        throw e;
    }
}

/**
 * Links a second executor (`apiPort`, started with the store's env, a person seed and the init
 * argument `--operator <person.did>`, which a claim needs; no person yet) to `person` as its next
 * device key, with `ad4m-executor claim`: the holder opens an
 * offer, the executor claims it, the holder confirms. Returns the new key's id (`d2`, …).
 */
export async function linkDevice(person: Person, store: IdentityStore, apiPort: number): Promise<string> {
    const opened = await person.holder.send({ cmd: "open", did: person.did, store: store.url });
    const claim = new ClaimCommand(apiPort, opened.link, "second device");
    await claim.matchCode();
    await person.holder.send({ cmd: "waitClaim" });
    await person.holder.send({ cmd: "confirm" });
    const code = await claim.exited;
    if (code !== 0) throw new Error(`ad4m-executor claim exited with ${code}: ${claim.output.join(" | ")}`);
    const joined = claim.output.find((l) => l.startsWith(`Joined ${person.did} as `));
    if (!joined) throw new Error(`claim did not join ${person.did}: ${claim.output.join(" | ")}`);
    // "Joined <did> as d2 (event 1)."
    return joined.slice(`Joined ${person.did} as `.length).trim().split(/\s+/)[0];
}
