import { Ad4mModel } from "./Ad4mModel";
import { Model, Property } from "./decorators";
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
    instanceAddToCollection: (_uuid: string, body: any) => ({ ...body, linksAdded: 0, alreadyMember: false }),
    instanceRemoveFromCollection: (_uuid: string, body: any) => ({ ...body, linksRemoved: 0 }),
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

describe("a call-backed class's writes", () => {
  describe("address the class by its target class IRI (#1666)", () => {
    // Two registered classes may share a name; the executor refuses the
    // bare name as ambiguous, so every instance* call must carry the IRI.
    const A = Ad4mModel.fromSHACL(callShape("a://Task"), "Task");
    const B = Ad4mModel.fromSHACL(callShape("b://Task"), "Task");

    it("on create", async () => {
      for (const [Cls, iri] of [[A, "a://Task"], [B, "b://Task"]] as const) {
        const { proxy, sent } = proxyWith();
        const row = new (Cls as any)(proxy);
        row.title = "a";
        await row.save();
        expect(bodies(sent, "instanceCreate").map((b) => b.className)).toEqual([iri]);
      }
    });

    it("on update, property and relation writes, and delete", async () => {
      for (const [Cls, iri] of [[A, "a://Task"], [B, "b://Task"]] as const) {
        const { proxy, sent } = proxyWith();
        const row = await fetched(Cls, proxy);
        row.title = "b";
        await row.save();
        await (row as any).setProperty("title", "c");
        await (row as any).setRelationValues("tags", ["z"]);
        await row.delete();
        const keys = [...bodies(sent, "instanceUpdate"), ...bodies(sent, "instanceRemove")].map((b) => b.className);
        expect(keys.length).toBe(4);
        expect(new Set(keys)).toEqual(new Set([iri]));
      }
    });
  });

  describe("collections reach the declared setter", () => {
    const Thing = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");

    it("save() of a collection cleared to [] sends the empty list to instanceUpdate", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      row.tags = [];
      await row.save();
      expect(bodies(sent, "instanceUpdate")).toEqual([
        { className: "mounttest://Thing", baseUri: MINTED, properties: { tags: [] } },
      ]);
      expect(methods(sent)).not.toContain("executeCommands");
    });

    it("an add is the full new member list through the setter, not a raw membership link", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await (row as any).addRelationValue("tags", "z");
      expect(bodies(sent, "instanceUpdate")).toEqual([
        { className: "mounttest://Thing", baseUri: MINTED, properties: { tags: ["x", "y", "z"] } },
      ]);
      expect(methods(sent)).not.toContain("instanceAddToCollection");
      expect(methods(sent)).not.toContain("executeCommands");
    });

    it("a remove is the full remaining member list through the setter", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await (row as any).removeRelationValue("tags", "x");
      expect(bodies(sent, "instanceUpdate")).toEqual([
        { className: "mounttest://Thing", baseUri: MINTED, properties: { tags: ["y"] } },
      ]);
      expect(methods(sent)).not.toContain("instanceRemoveFromCollection");
      expect(methods(sent)).not.toContain("executeCommands");
    });

    it("an adder envelope in save() is the full new member list too", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      row.tags = { action: "adder", value: ["z"] } as any;
      await row.save();
      expect(bodies(sent, "instanceUpdate")).toEqual([
        { className: "mounttest://Thing", baseUri: MINTED, properties: { tags: ["x", "y", "z"] } },
      ]);
      expect(methods(sent)).not.toContain("instanceAddToCollection");
    });
  });

  describe("a declared call adder or remover runs, not a full-list replace", () => {
    // `tags` declares a `call` adder and remover and no setter: a full-list
    // instanceUpdate would reach no declared action (the executor refuses
    // it), so add and remove go to instanceAddToCollection /
    // instanceRemoveFromCollection, which run the adder and remover.
    const shape = SHACLShape.fromJSON({
      target_class: "mounttest://Thing",
      constructor_actions: [{ action: "call", source: "mounttest://class/Thing", predicate: "create", target: "{}" }],
      properties: [
        { path: "mounttest://title", name: "title", max_count: 1, writable: true, setter: update },
        {
          path: "mounttest://tag", name: "tags", collection: true, datatype: "xsd://string", writable: true,
          adder: [{ action: "call", source: "this", predicate: "addTags", target: "{}" }],
          remover: [{ action: "call", source: "this", predicate: "removeTags", target: "{}" }],
        },
      ],
    } as any);
    const Thing = Ad4mModel.fromSHACL(shape, "MountThing");
    const item = (itemUri: string) => ({ className: "mounttest://Thing", baseUri: MINTED, collection: "tags", itemUri });

    it("add() runs the adder", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await (row as any).addRelationValue("tags", "z");
      expect(bodies(sent, "instanceAddToCollection")).toEqual([item("z")]);
      expect(methods(sent)).not.toContain("instanceUpdate");
    });

    it("remove() runs the remover", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await (row as any).removeRelationValue("tags", "x");
      expect(bodies(sent, "instanceRemoveFromCollection")).toEqual([item("x")]);
      expect(methods(sent)).not.toContain("instanceUpdate");
    });

    it("an adder envelope in save() runs the adder per member", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      row.title = "b";
      row.tags = { action: "adder", value: ["z", "w"] } as any;
      await row.save();
      expect(bodies(sent, "instanceAddToCollection")).toEqual([item("z"), item("w")]);
      expect(bodies(sent, "instanceUpdate")).toEqual([
        { className: "mounttest://Thing", baseUri: MINTED, properties: { title: "b" } },
      ]);
    });

    it("a caller's batch goes with them, for the executor to refuse", async () => {
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await (row as any).addRelationValue("tags", "z", "batch-1");
      expect(bodies(sent, "instanceAddToCollection")).toEqual([{ ...item("z"), batchId: "batch-1" }]);
    });
  });

  describe("a subclass reads the shape it addresses", () => {
    const Imported = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");

    @Model({ name: "LocalThing", namespace: "local://" })
    class LocalThing extends (Imported as typeof Ad4mModel) {
      @Property({ through: "local://title", required: true })
      title: string = "";
    }

    class SameThing extends (Imported as typeof Ad4mModel) {}

    it("a decorated subclass is its own link-backed class", async () => {
      expect((LocalThing as any).hasCallActions()).toBe(false);
      expect(LocalThing.getTargetClass()).toBe("local://LocalThing");
      const { proxy, sent } = proxyWith({ id: "ad4m://l1", title: "a" });
      const created = new LocalThing(proxy);
      created.title = "a";
      await created.save();
      const row = new LocalThing(proxy, "ad4m://l1");
      await row.get();
      row.title = "b";
      await row.save();
      await row.delete();
      expect(methods(sent).filter((m) => m.startsWith("instance"))).toEqual([]);
      expect(methods(sent)).toContain("createSubject");
      expect(methods(sent)).toContain("removeSubject");
    });

    it("an undecorated subclass is the imported class", () => {
      expect((SameThing as any).hasCallActions()).toBe(true);
      expect(SameThing.getTargetClass()).toBe("mounttest://Thing");
    });
  });

  describe("fromSHACL keeps the shape's call actions", () => {
    it("delete() of an imported call-backed shape with no flag runs the destructor (instanceRemove)", async () => {
      // No `has_value` flag and every property optional: the property
      // metadata alone suggests no destructor. The shape declares one.
      const Thing = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");
      const { proxy, sent } = proxyWith();
      const row = await fetched(Thing, proxy);
      await row.delete();
      expect(bodies(sent, "instanceRemove")).toEqual([{ className: "mounttest://Thing", baseUri: MINTED }]);
      expect(methods(sent)).not.toContain("removeLinks");
      expect((Thing as any).hasCallActions()).toBe(true);
    });
  });

  it("a caller's batch goes to the executor, which refuses it, rather than a silent legacy write", async () => {
    const Thing = Ad4mModel.fromSHACL(callShape("mounttest://Thing"), "MountThing");
    const { proxy, sent } = proxyWith();
    const row = await fetched(Thing, proxy);
    row.title = "b";
    await row.save("batch-1");
    expect(bodies(sent, "instanceUpdate")).toEqual([
      { className: "mounttest://Thing", baseUri: MINTED, properties: { title: "b" }, batchId: "batch-1" },
    ]);
    expect(methods(sent)).not.toContain("executeCommands");
  });
});

describe("an ordinary link-backed class keeps its established writes", () => {
  @Model({ name: "Note", namespace: "plain://" })
  class Note extends Ad4mModel {
    @Property({ through: "plain://title", required: true })
    title: string = "";
  }

  it("has no call actions", () => {
    expect((Note as any).hasCallActions()).toBe(false);
  });

  it("an unbatched update, setProperty and delete send no instance* RPC", async () => {
    const { proxy, sent } = proxyWith({ id: "ad4m://n1", title: "a" });
    const row = new Note(proxy, "ad4m://n1");
    await row.get();
    row.title = "b";
    await row.save();
    await (row as any).setProperty("title", "c");
    await row.delete();
    expect(methods(sent).filter((m) => m.startsWith("instance"))).toEqual([]);
    expect(methods(sent)).toContain("removeSubject");
  });

  it("an unbatched create sends no instanceCreate", async () => {
    const { proxy, sent } = proxyWith({ id: "ad4m://n1", title: "a" });
    const row = new Note(proxy);
    row.title = "a";
    await row.save();
    expect(methods(sent)).not.toContain("instanceCreate");
    expect(methods(sent)).toContain("createSubject");
  });
});
