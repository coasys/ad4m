import { test } from "node:test";
import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

// A Language author installs the published tarball, not this directory. These
// tests pack the LDK, unpack it into a project outside the monorepo and type
// check a consumer against it, so the public type entry has to bring the
// `ad4m:host` declaration along by itself.

const PKG_DIR = fileURLToPath(new URL("..", import.meta.url));
const TSC = join(PKG_DIR, "node_modules", "typescript", "bin", "tsc");

const CONSUMER = `import { agentDid, storagePut, HostFunctionMissing } from "@coasys/ad4m-ldk";
const did: string = agentDid();
storagePut("key", did);
const missing: HostFunctionMissing = new HostFunctionMissing("agentDid");
console.log(missing.functionName);
`;

function packedConsumer(source: string): string {
    const dir = mkdtempSync(join(tmpdir(), "ad4m-ldk-consumer-"));
    execFileSync(process.execPath, [TSC, "-p", PKG_DIR], { stdio: "pipe" });
    execFileSync("npm", ["pack", "--silent", "--pack-destination", dir], { cwd: PKG_DIR, stdio: "pipe" });
    const tarball = readdirSync(dir).find((f) => f.endsWith(".tgz"));
    assert.ok(tarball, "npm pack wrote no tarball");
    const installed = join(dir, "node_modules", "@coasys", "ad4m-ldk");
    mkdirSync(installed, { recursive: true });
    execFileSync("tar", ["-xzf", join(dir, tarball), "-C", installed, "--strip-components=1"]);
    writeFileSync(join(dir, "package.json"), JSON.stringify({ type: "module", private: true }));
    writeFileSync(join(dir, "consumer.ts"), source);
    return dir;
}

function typecheck(dir: string, moduleResolution: string, extra: string[] = []) {
    const module = moduleResolution === "Bundler" ? "ES2022" : "NodeNext";
    const run = spawnSync(
        process.execPath,
        [TSC, "--strict", "--noEmit", "--target", "ES2022", "--module", module, "--moduleResolution", moduleResolution, ...extra, "consumer.ts"],
        { cwd: dir, encoding: "utf8" },
    );
    return { status: run.status, output: run.stdout + run.stderr };
}

for (const moduleResolution of ["Bundler", "NodeNext"]) {
    test(`packed LDK type checks in a strict consumer (${moduleResolution})`, () => {
        const dir = packedConsumer(CONSUMER);
        try {
            const result = typecheck(dir, moduleResolution);
            assert.equal(result.status, 0, result.output);
        } finally {
            rmSync(dir, { recursive: true, force: true });
        }
    });
}

test("packed LDK keeps the host function types (skipLibCheck does not turn them into any)", () => {
    const dir = packedConsumer(`import { agentDid } from "@coasys/ad4m-ldk";\nconst n: number = agentDid();\n`);
    try {
        const result = typecheck(dir, "Bundler", ["--skipLibCheck"]);
        assert.notEqual(result.status, 0);
        assert.match(result.output, /TS2322: Type 'string' is not assignable to type 'number'/);
    } finally {
        rmSync(dir, { recursive: true, force: true });
    }
});
