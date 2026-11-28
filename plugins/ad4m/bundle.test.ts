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

/**
 * The parts of the executor client a live query uses, with `perspective.subscribe` answering
 * as `subscribe` says.
 */
function liveClient(subscribe: () => Promise<unknown>) {
  return {
    subscribe: vi.fn(subscribe),
    onQueryUpdate: vi.fn(() => () => {}),
    onReconnect: vi.fn(() => () => {}),
    onSubscriptionEnded: vi.fn(() => () => {}),
    disposeQuerySubscription: vi.fn(() => Promise.resolve(true)),
    resyncSubscription: vi.fn(() => Promise.resolve()),
  };
}

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

    const manager = new WakerSubscriptionManager({
      perspectiveClient: {
        ...liveClient(() =>
          Promise.reject(new Error("RPC error 403: main key not found")),
        ),
        querySparql: vi.fn(() => Promise.resolve({ results: { bindings: [] } })),
      },
      logger: { info: vi.fn(), warn: vi.fn(), error: vi.fn(), debug: vi.fn() },
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
        // What get_mention_waker_config returns.
        className: "ad4m://TextContent",
        query: "{}",
        terms: ["Data", "did:key:zAgent"],
        did: "did:key:zAgent",
        mentionClass: "ad4m://Mentioning",
        mentionQuery: "{}",
      }),
    ).rejects.toThrow(/main key not found/);

    expect(manager.has("bundle-fail")).toBe(false);
    expect(manager.getActive()).toHaveLength(0);
    expect(manager.getPending().map((s: any) => s.id)).toEqual(["bundle-fail"]);
  });

  it("ships a manager that times out a hung handshake and keeps it pending (#1016)", async () => {
    const { WakerSubscriptionManager } = bundle;
    const manager = new WakerSubscriptionManager({
      perspectiveClient: {
        // The executor accepted the socket but never answers the subscribe RPC.
        ...liveClient(() => new Promise(() => {})),
        querySparql: vi.fn(() => Promise.resolve({ results: { bindings: [] } })),
      },
      logger: { info: vi.fn(), warn: vi.fn(), error: vi.fn(), debug: vi.fn() },
      debounceMs: 10,
      subscribeTimeoutMs: 50,
      retryPendingMs: 60_000,
      onWake: () => {},
      onPersist: () => {},
    });

    // A released manager without the deadline never settles here, and the
    // subscription ends up in neither the active nor the pending list.
    await expect(
      manager.subscribe({
        id: "bundle-hang",
        type: "mention",
        perspective: "fake-uuid",
        channel: "",
        // What get_mention_waker_config returns.
        className: "ad4m://TextContent",
        query: "{}",
        terms: ["Data", "did:key:zAgent"],
        did: "did:key:zAgent",
        mentionClass: "ad4m://Mentioning",
        mentionQuery: "{}",
      }),
    ).rejects.toThrow(/timed out after 50ms/);

    expect(manager.getActive()).toHaveLength(0);
    expect(manager.getPending().map((s: any) => s.id)).toEqual(["bundle-hang"]);
    manager.disposeAll();
  });

  it("ships a manager that opens a class's model query through perspective.subscribe", async () => {
    // Mention and children subscriptions are model queries over a class
    // (#1446). A client that dropped the class sent the JSON as SPARQL, which
    // the executor rejects.
    const { WakerSubscriptionManager } = bundle;
    const client = liveClient(() =>
      Promise.resolve({
        subscriptionId: "sub-1",
        revision: 0,
        result: { instances: [], totalCount: 0 },
      }),
    );
    const manager = new WakerSubscriptionManager({
      perspectiveClient: client,
      logger: { info: vi.fn(), warn: vi.fn(), error: vi.fn(), debug: vi.fn() },
      debounceMs: 10,
      onWake: () => {},
      onPersist: () => {},
    });
    const queryJson = '{"where":{"id":"test://call"}}';
    await manager.subscribe({
      id: "bundle-class",
      type: "channel-messages",
      perspective: "perspective-uuid",
      channel: "test://call",
      query: queryJson,
      className: "ad4m://Container",
    });

    expect(client.subscribe).toHaveBeenCalledWith(
      "perspective-uuid",
      { className: "ad4m://Container", queryJson },
      { signal: expect.any(AbortSignal) },
    );
    expect(manager.has("bundle-class")).toBe(true);
    manager.disposeAll();
  });

  it("resolves @coasys/ad4m at build time, not from the installing host", () => {
    // `dependencies` is empty, so anything left as a runtime require would be
    // unresolvable after `npm install @coasys/openclaw-ad4m` — and if it did
    // resolve, it would be whatever version that host happens to have.
    const externalRequires = bundleSource.match(/require\("@coasys\/ad4m[^"]*"\)/g) ?? [];
    expect(externalRequires).toEqual([]);
  });
});
