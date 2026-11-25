import { Ad4mModel } from "./Ad4mModel";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { SHACLShape } from "../shacl/SHACLShape";

// A class whose registered shape declares `call` actions (a mounted class:
// its records live in a language) writes through the validated
// `perspective.instance*` RPCs, which run those actions on the executor.
// Every other class keeps its `executeAction` / subject writes. These tests
// pin the SDK half: which RPC goes out, under which class key, with which
// values. The executor half — that the call reaches the language — is in
// `rust-executor/src/perspectives/perspective_instance/call_actions_tests.rs`.

const update = [{ action: "call", source: "this", predicate: "update", target: "{}" }];

/** A mount-test-like shape: a `call` constructor, setters and destructor. */
function callShape(target: string, opts: { flag?: boolean } = {}) {
  return SHACLShape.fromJSON({
    target_class: target,
    constructor_actions: [{ action: "call", source: "mounttest://class/Thing", predicate: "create", target: "{}" }],
    destructor_actions: [{ action: "call", source: "this", predicate: "delete", target: "{}" }],
    properties: [
      ...(opts.flag ? [{ path: "rdf://type", name: "type", has_value: target, min_count: 1, max_count: 1 }] : []),
      { path: "mounttest://title", name: "title", max_count: 1, writable: true, setter: update },
      { path: "mounttest://tag", name: "tags", collection: true, datatype: "xsd://string", writable: true, setter: update },
    ],
  } as any);
}

const MINTED = "mounttest://t1";

function proxyWith(record: Record<string, any> = { id: MINTED, title: "a", tags: ["x", "y"] }) {
  const sent: any[][] = [];
  const answers: Record<string, (...args: any[]) => any> = {
    queryLinks: () => [],
    createBatch: () => "batch-1",
    commitBatch: () => ({ additions: [], removals: [] }),
    executeCommands: () => true,
    createSubject: () => true,
    updateSubject: () => true,
    removeSubject: () => true,
    removeLinks: () => [],
    modelQuery: () => ({ instances: [record], totalCount: 1 }),
    instanceCreate: (_uuid: string, body: any) => ({
      className: body.className, baseUri: MINTED, propertiesSet: [], collectionsSet: [], parent: null,
    }),
    instanceUpdate: (_uuid: string, body: any) => ({ className: body.className, baseUri: body.baseUri, updatedProperties: [] }),
    instanceRemove: (_uuid: string, body: any) => ({ className: body.className, baseUri: body.baseUri, linksRemoved: 0 }),
    instanceAddToCollection: (_uuid: string, body: any) => ({ ...body, linksAdded: 0, alreadyMember: false, languageCall: true }),
    instanceRemoveFromCollection: (_uuid: string, body: any) => ({ ...body, linksRemoved: 0, languageCall: true }),
  };
  const client = new Proxy({} as any, {
    get: (_target, method: string) => {
      if (method === "then") return undefined;
      return async (...args: any[]) => {
        sent.push([method, ...args]);
        const answer = answers[method];
        if (!answer) throw new Error(`unexpected RPC ${method}`);
        return answer(...args);
      };
    },
  });
  const proxy = new PerspectiveProxy(
    { uuid: "p-uuid", name: "p", owners: [], sharedUrl: null, neighbourhood: null, state: "Synced" } as any,
    client,
  );
  return { proxy, sent };
}

/** The bodies of every `method` RPC sent (call[1] is the perspective uuid). */
const bodies = (sent: any[][], method: string) => sent.filter((c) => c[0] === method).map((c) => c[2]);
const methods = (sent: any[][]) => sent.map((c) => c[0]);

/** A fetched row: hydrated and snapshotted, as a query or get() leaves it. */
async function fetched(Cls: typeof Ad4mModel, proxy: PerspectiveProxy) {
  const row = new (Cls as any)(proxy, MINTED);
  await row.get();
  return row;
}

test.each([false, true])("awaited adds retain unmirrored members (second instance: %s)", async (anotherInstance) => {
  const Thing = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");
  const { proxy, sent } = proxyWith({ id: MINTED, title: "a", tags: ["x"] });
  const row = await fetched(Thing, proxy);
  const next = anotherInstance ? await fetched(Thing, proxy) : row;
  await row.addRelationValue("tags", "z");
  await next.addRelationValue("tags", "w");
  expect(bodies(sent, "instanceUpdate").map((b) => b.properties.tags)).toEqual([
    ["x", "z"], ["x", "z", "w"]
  ]);
});

test("pending removals survive stale reads and retire when mirrored", async () => {
  const Thing = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");
  const record = { id: MINTED, title: "a", tags: ["x", "y"] };
  const { proxy, sent } = proxyWith(record);
  const row = await fetched(Thing, proxy);
  await row.removeRelationValue("tags", "x");
  await row.addRelationValue("tags", "z");
  record.tags = ["y", "z", "external"];
  await row.addRelationValue("tags", "w");
  record.tags = ["x", "y", "external", "w"];
  await row.addRelationValue("tags", "last");
  expect(bodies(sent, "instanceUpdate").map((b) => b.properties.tags)).toEqual([
    ["y"], ["y", "z"], ["y", "z", "external", "w"], ["x", "y", "external", "w", "last"]
  ]);
});
