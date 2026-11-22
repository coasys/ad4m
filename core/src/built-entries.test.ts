/**
 * Tests against the BUILT package entries (`core/lib`), not the sources —
 * they guard the packaging the sources compile into:
 *
 * 1. The root entry re-exports the model subsystem. `src/index.ts` says
 *    `export * from "./model"`; a `src/model.ts` file next to `src/model/`
 *    shadows the directory barrel, and `Ad4mModel`, `ModelQueryBuilder`,
 *    hydration and the query helpers vanish from the built root. That is
 *    why the `./model` subpath's source is `src/model-entry.ts`.
 * 2. `index` and `model` come out of ONE Rollup build with a shared chunk
 *    per format, so both entries import the same `decorators` module.
 *    Two separate bundles would each inline their own `propertyRegistry`/
 *    `relationRegistry`, and a class decorated through `@coasys/ad4m/model`
 *    would be invisible to `getPropertiesMetadata` from `@coasys/ad4m`.
 *
 * Both checks need `core/lib` — they run after `pnpm run build` (CI builds
 * core before the turbo test pass) and fail loudly without it.
 */
import { execFileSync } from "child_process";
import * as fs from "fs";
import * as path from "path";
import { pathToFileURL } from "url";

const libDir = path.resolve(__dirname, "..", "lib");
const libUrl = (file: string) => pathToFileURL(path.join(libDir, file)).href;
const libPath = (file: string) => path.join(libDir, file);

const runNode = (args: string[]) =>
    execFileSync(process.execPath, args, { encoding: "utf8", cwd: libDir });

const runJson = (args: string[]) => JSON.parse(runNode(args));

const REQUIRED = ["index.js", "index.cjs", "model.js", "model.cjs"];
const libReady = REQUIRED.every((f) => fs.existsSync(libPath(f)));

const EXPORTS_PROBE = `
import { Ad4mModel, ModelQueryBuilder, Model, Property, Optional, ReadOnly, Flag,
         HasMany, HasOne, getPropertiesMetadata, getRelationsMetadata } from ${JSON.stringify(libUrl("index.js"))};
const named = { Ad4mModel, ModelQueryBuilder, Model, Property, Optional, ReadOnly, Flag,
                HasMany, HasOne, getPropertiesMetadata, getRelationsMetadata };
console.log(JSON.stringify({ missing: Object.keys(named).filter(k => named[k] === undefined) }));
`;

const EXPORTS_PROBE_CJS = `
const { Ad4mModel, ModelQueryBuilder, Model, Property, Optional, ReadOnly, Flag,
        HasMany, HasOne, getPropertiesMetadata, getRelationsMetadata } = require(${JSON.stringify(libPath("index.cjs"))});
const named = { Ad4mModel, ModelQueryBuilder, Model, Property, Optional, ReadOnly, Flag,
                HasMany, HasOne, getPropertiesMetadata, getRelationsMetadata };
console.log(JSON.stringify({ missing: Object.keys(named).filter(k => named[k] === undefined) }));
`;

const CROSS_ENTRY_PROBE = `
import { getPropertiesMetadata, getRelationsMetadata } from ${JSON.stringify(libUrl("index.js"))};
import { Model, Optional, HasMany } from ${JSON.stringify(libUrl("model.js"))};
class CrossEntry {}
Model({ name: "CrossEntry", namespace: "xentry://" })(CrossEntry);
Optional({ through: "xentry://title" })(CrossEntry.prototype, "title");
HasMany({ through: "xentry://member" })(CrossEntry.prototype, "members");
const props = getPropertiesMetadata(CrossEntry);
const rels = getRelationsMetadata(CrossEntry);
console.log(JSON.stringify({
  title: props.title?.through ?? null,
  members: rels.members?.predicate ?? null,
  generateSHACL: typeof CrossEntry.generateSHACL,
}));
`;

const CROSS_ENTRY_PROBE_CJS = `
const { getPropertiesMetadata, getRelationsMetadata } = require(${JSON.stringify(libPath("index.cjs"))});
const { Model, Optional, HasMany } = require(${JSON.stringify(libPath("model.cjs"))});
class CrossEntry {}
Model({ name: "CrossEntry", namespace: "xentry://" })(CrossEntry);
Optional({ through: "xentry://title" })(CrossEntry.prototype, "title");
HasMany({ through: "xentry://member" })(CrossEntry.prototype, "members");
const props = getPropertiesMetadata(CrossEntry);
const rels = getRelationsMetadata(CrossEntry);
console.log(JSON.stringify({
  title: props.title?.through ?? null,
  members: rels.members?.predicate ?? null,
  generateSHACL: typeof CrossEntry.generateSHACL,
}));
`;

describe("built package entries", () => {
    beforeAll(() => {
        if (!libReady) {
            throw new Error(
                `core/lib is missing ${REQUIRED.filter((f) => !fs.existsSync(libPath(f))).join(", ")} ` +
                `— run \`pnpm run build\` in core/ first`
            );
        }
    });

    it("the built root entry exports the model subsystem (esm)", () => {
        const out = runJson(["--input-type=module", "--eval", EXPORTS_PROBE]);
        expect(out.missing).toEqual([]);
    });

    it("the built root entry exports the model subsystem (cjs)", () => {
        const out = runJson(["--eval", EXPORTS_PROBE_CJS]);
        expect(out.missing).toEqual([]);
    });

    it("a class decorated through the built model entry registers with the root entry (esm)", () => {
        const out = runJson(["--input-type=module", "--eval", CROSS_ENTRY_PROBE]);
        expect(out).toEqual({
            title: "xentry://title",
            members: "xentry://member",
            generateSHACL: "function",
        });
    });

    it("a class decorated through the built model entry registers with the root entry (cjs)", () => {
        const out = runJson(["--eval", CROSS_ENTRY_PROBE_CJS]);
        expect(out).toEqual({
            title: "xentry://title",
            members: "xentry://member",
            generateSHACL: "function",
        });
    });
});
