import { Ad4mModel } from "./Ad4mModel";
import { HasMany, Model, Property } from "./decorators";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";

// A `call` constructor mints the record inside its language and
// `perspective.createSubject` answers the minted URL (`<lang>://<id>`),
// not `true`. These tests pin the SDK half of that contract: the URL must
// reach the caller and the model's `_baseExpression`, or every later write
// targets an address the language never created.
// (The executor half — that such an update really does call the language —
// is pinned by the mount-test Rust tests in
// `perspective_instance/call_actions_tests.rs`.)

@Model({ name: "MountThing" })
class MountThing extends Ad4mModel {
  @Property({ through: "mounttest://title" })
  title: string = "";

  @HasMany({ through: "mounttest://child" })
  children: string[] = [];
}

const MINTED = "mounttest://t1";

function callingProxy(createSubjectResult: boolean | string) {
  const sent: any[][] = [];
  const answers: Record<string, (...args: any[]) => any> = {
    queryLinks: () => [],
    createBatch: () => "batch-1",
    commitBatch: () => ({ additions: [], removals: [] }),
    createSubject: () => createSubjectResult,
    updateSubject: () => true,
    executeCommands: () => true,
    removeLinks: () => [],
    modelQuery: () => ({ instances: [], totalCount: 0 }),
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

// The expression address an `executeCommands` or `updateSubject` RPC named
// (call[0] is the method name, call[1] the perspective uuid).
const writtenTo = (call: any[]) => call[3];

describe("a call constructor's minted URL", () => {

  it("createSubject returns the URL the RPC answered, not the input address", async () => {
    const { proxy } = callingProxy(MINTED);
    const result = await proxy.createSubject("MountThing", "ignored://input");
    expect(result).toBe(MINTED);
  });

  it("createSubject still returns the input address when the RPC answers true", async () => {
    const { proxy } = callingProxy(true);
    const result = await proxy.createSubject("MountThing", "input://addr");
    expect(result).toBe("input://addr");
  });

  it("createSubject returns the minted URL through the decorated-class overload", async () => {
    const { proxy } = callingProxy(MINTED);
    const result = await proxy.createSubject(new MountThing(undefined as any), "ignored://input");
    expect(result).toBe(MINTED);
  });

  it("save() adopts the minted URL as the instance id before writing relations", async () => {
    const { proxy, sent } = callingProxy(MINTED);
    const thing = new MountThing(proxy);
    thing.title = "a";
    thing.children = ["mounttest://kid"];
    await thing.save();

    expect(thing.id).toBe(MINTED);
    const writes = sent.filter((c) => c[0] === "executeCommands");
    expect(writes.length).toBeGreaterThan(0);
    for (const w of writes) {
      expect(writtenTo(w)).toBe(MINTED);
    }
  });

  it("a follow-up update targets the minted URL, so the call reaches the language", async () => {
    const { proxy, sent } = callingProxy(MINTED);
    const thing = new MountThing(proxy);
    thing.title = "a";
    await thing.save();

    thing.title = "b";
    await thing.save();

    const writes = sent.filter((c) => c[0] === "executeCommands" || c[0] === "updateSubject");
    expect(writes.length).toBeGreaterThan(0);
    for (const w of writes) {
      expect(writtenTo(w)).toBe(MINTED);
    }
  });
});
