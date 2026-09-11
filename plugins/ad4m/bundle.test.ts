/**
 * Bundle-level guard for the de-forked waker.
 *
 * The plugin's other tests import TypeScript source, so they prove that
 * `core/src/perspectives/WakerSubscriptionManager.ts` behaves. They cannot
 * prove that the *published artifact* contains that code, because what lands
 * in `dist/index.cjs` is decided by the build, not by the imports: adding
 * `--external:@coasys/ad4m` leaves the whole source suite green while shipping
 * a bundle that resolves the package on the installing host instead. And
 * `plugins/ad4m` is a separate npm project whose `node_modules/@coasys/ad4m`
 * is the last released tarball, so that host copy is a real, older
 * implementation — the mistake behind `7ea653482`, worked around by
 * `c5287505d`.
 *
 * So this suite builds `dist/index.cjs` and asserts against the bundle itself.
 */
import { describe, it, expect, beforeAll, vi } from "vitest";
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import * as path from "node:path";

const pkgDir = __dirname;
const distPath = path.join(pkgDir, "dist", "index.cjs");
const requireCjs = createRequire(path.join(pkgDir, "bundle.test.ts"));

let bundle: any;
let bundleSource: string;

describe("shipped bundle", () => {
  beforeAll(() => {
    // Build here rather than relying on a prior `npm run build`: a stale dist
    // would otherwise let this suite pass against code that is no longer ours.
    execFileSync("npm", ["run", "build"], { cwd: pkgDir, stdio: "pipe" });
    bundle = requireCjs(distPath);
    bundleSource = readFileSync(distPath, "utf8");
  }, 180_000);

  it("ships the waker manager from core, which rejects a refused subscription", async () => {
    const { WakerSubscriptionManager } = bundle;
    expect(typeof WakerSubscriptionManager).toBe("function");

    const proxy = {
      initialized: Promise.resolve(),
      subscribe: vi.fn(() =>
        Promise.reject(new Error("RPC error 403: main key not found")),
      ),
      dispose: vi.fn(),
      onResult: vi.fn(),
    };

    const manager = new WakerSubscriptionManager({
      perspectiveClient: {
        querySparql: vi.fn(() => Promise.resolve({ results: { bindings: [] } })),
      },
      logger: { info: vi.fn(), warn: vi.fn(), error: vi.fn(), debug: vi.fn() },
      QuerySubscriptionProxy: vi.fn(function () {
        return proxy;
      }),
      debounceMs: 10,
      onWake: () => {},
      onPersist: () => {},
    });

    // The released manager resolved silently here, so the subscribe tools
    // answered "Subscribed..." for a subscription that was never registered.
    await expect(
      manager.subscribe({
        id: "bundle-fail",
        type: "mention",
        perspective: "fake-uuid",
        channel: "",
        query: "SELECT * FROM link",
      }),
    ).rejects.toThrow(/main key not found/);

    expect(manager.has("bundle-fail")).toBe(false);
    expect(manager.getActive()).toHaveLength(0);
    expect(manager.getPending().map((s: any) => s.id)).toEqual(["bundle-fail"]);
  });

  it("resolves @coasys/ad4m at build time, not from the installing host", () => {
    // `dependencies` is empty, so anything left as a runtime require would be
    // unresolvable after `npm install @coasys/openclaw-ad4m` — and if it did
    // resolve, it would be whatever version that host happens to have.
    const externalRequires = bundleSource.match(/require\("@coasys\/ad4m[^"]*"\)/g) ?? [];
    expect(externalRequires).toEqual([]);
  });
});
