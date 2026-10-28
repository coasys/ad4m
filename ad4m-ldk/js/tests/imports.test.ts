import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";
import ts from "typescript";

// The executor links a Language bundle against `ad4m:host` as an ES module, so
// a named import of a function the host does not export fails the whole bundle
// at link time. These tests link `src/imports.ts` the same way: as a
// `vm.SourceTextModule` whose `ad4m:host` is a `vm.SyntheticModule` exporting
// only the functions a given executor has.

const IMPORTS_SRC = transpile(readFileSync(new URL("../src/imports.ts", import.meta.url), "utf8"));

function transpile(source: string): string {
    return ts.transpileModule(source, {
        compilerOptions: { module: ts.ModuleKind.ES2022, target: ts.ScriptTarget.ES2022 },
    }).outputText;
}

async function link(source: string, hostExports: Record<string, unknown>) {
    const context = vm.createContext({});
    const hostModule = new (vm as any).SyntheticModule(
        Object.keys(hostExports),
        function (this: any) {
            for (const [name, value] of Object.entries(hostExports)) this.setExport(name, value);
        },
        { context },
    );
    const mod = new (vm as any).SourceTextModule(source, { context });
    await mod.link((specifier: string) => {
        if (specifier !== "ad4m:host") throw new Error(`unexpected import ${specifier}`);
        return hostModule;
    });
    await mod.evaluate();
    return mod.namespace as Record<string, any>;
}

// Every function the wrappers name, read from the module itself so a new host
// function is covered without editing this list.
async function wrapperNames(): Promise<string[]> {
    const all = await link(IMPORTS_SRC, {});
    return Object.keys(all).filter((name) => name !== "HostFunctionMissing");
}

test("the harness reproduces the executor's link error for a static re-export", async () => {
    const staticReExport = `export { agentDid, agentOpenSealed } from "ad4m:host";`;
    await assert.rejects(
        link(staticReExport, { agentDid: () => "did:key:z6Mk" }),
        /does not provide an export named 'agentOpenSealed'/,
    );
});

test("the wrappers load on a host that lacks a function", async () => {
    const names = await wrapperNames();
    assert.ok(names.includes("agentOpenSealed"));
    const olderHost = Object.fromEntries(
        names.filter((n) => n !== "agentOpenSealed").map((n) => [n, () => `${n} ran`]),
    );

    const wrappers = await link(IMPORTS_SRC, olderHost);

    assert.equal(wrappers.agentDid(), "agentDid ran");
    assert.throws(
        () => wrappers.agentOpenSealed({ ephemeralPublicKey: "", nonce: "", ciphertext: "" }),
        (err: any) =>
            err.name === "HostFunctionMissing" &&
            err.functionName === "agentOpenSealed" &&
            /does not provide `agentOpenSealed`/.test(err.message),
    );
});

test("every host function declared in host.d.ts has a wrapper", async () => {
    const declared = [
        ...readFileSync(new URL("../src/host.d.ts", import.meta.url), "utf8").matchAll(/export function (\w+)/g),
    ].map((m) => m[1]);
    assert.deepEqual((await wrapperNames()).sort(), declared.sort());
});

test("each wrapper passes its arguments and return value through", async () => {
    const names = await wrapperNames();
    const host = Object.fromEntries(
        names.map((n) => [n, (...args: unknown[]) => ({ called: n, args })]),
    );

    const wrappers = await link(IMPORTS_SRC, host);

    for (const name of names) {
        assert.deepEqual(wrappers[name]("a", 2), { called: name, args: ["a", 2] }, name);
    }
});

test("a host function that rejects keeps its rejection", async () => {
    const wrappers = await link(IMPORTS_SRC, {
        httpFetch: async () => {
            throw new Error("timed out after 10 s: https://example.org");
        },
    });
    await assert.rejects(wrappers.httpFetch("https://example.org", "GET", "{}", ""), /timed out after 10 s/);
});
