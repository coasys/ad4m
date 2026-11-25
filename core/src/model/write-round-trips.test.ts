import { Ad4mModel } from "./Ad4mModel";
import { HasMany, Model, Property } from "./decorators";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";

// Every RPC that save() and delete() send, counted on a client double. A write
// is one `perspective.mutate` call.

@Model({ name: "RoundTripNote" })
class RoundTripNote extends Ad4mModel {
  @Property({ through: "rt://title", required: true, initial: "literal://string:untitled" })
  title: string = "";
}

@Model({ name: "RoundTripReadable", abstract: true })
class RoundTripReadable extends Ad4mModel {
  @Property({ through: "rt://title" })
  title: string = "";
}

// Every relation defaults to [], as WE's generated classes declare them.
@Model({ name: "RoundTripCall" })
class RoundTripCall extends Ad4mModel {
  @Property({ through: "rt://title", required: true, initial: "literal://string:untitled" })
  title: string = "";

  @HasMany({ through: "rt://participant" })
  participants: string[] = [];

  @HasMany({ through: "rt://tag" })
  tags: string[] = [];

  @HasMany({ through: "rt://attachment" })
  attachments: string[] = [];
}

@Model({ name: "RoundTripOptional" })
class RoundTripOptional extends Ad4mModel {
  @Property({ through: "rt://note" })
  note: string = "";

  @Property({ through: "rt://rank" })
  rank: string = "";
}

@Model({ name: "RoundTripRequired" })
class RoundTripRequired extends Ad4mModel {
  @Property({ through: "rt://title", required: true })
  title: string = "";
}

function countingProxy(rttMs = 0) {
  const calls: string[] = [];
  const sent: any[][] = [];
  const answers: Record<string, (...args: any[]) => any> = {
    mutate: () => ({ ids: [] }),
    modelQuery: () => ({ instances: [], totalCount: 0 }),
  };
  const client = new Proxy({} as any, {
    get: (_target, method: string) => {
      if (method === "then") return undefined;
      return async (...args: any[]) => {
        calls.push(method);
        sent.push([method, ...args]);
        if (rttMs) await new Promise((r) => setTimeout(r, rttMs));
        const answer = answers[method];
        if (!answer) throw new Error(`unexpected RPC ${method}`);
        return answer(...args);
      };
    },
  });
  const proxy = new PerspectiveProxy(
    { uuid: "rt-uuid", name: "rt", owners: [], sharedUrl: null, neighbourhood: null, state: "Synced" } as any,
    client,
  );
  return { proxy, calls, sent };
}

/** The ops of every `mutate` call, in order. */
const opsSent = (sent: any[][]) => sent.filter((c) => c[0] === "mutate").flatMap((c) => c[2]);

async function createNote(proxy: PerspectiveProxy) {
  const note = new RoundTripNote(proxy, "rt://note/1");
  note.title = "hello";
  await note.save();
  return note;
}

describe("RPCs per write", () => {
  it("create of a decorated model is one mutate, then the refresh read", async () => {
    const { proxy, calls, sent } = countingProxy();
    await createNote(proxy);
    expect(calls).toEqual(["mutate", "modelQuery"]);
    expect(sent[0]).toEqual([
      "mutate",
      "rt-uuid",
      [{ op: "create", class: "rt://RoundTripNote", id: "rt://note/1", values: { title: "hello" } }],
      {},
    ]);
  });

  it("create sends no setter for an empty relation", async () => {
    const { proxy, calls, sent } = countingProxy();
    const call = new RoundTripCall(proxy);
    call.title = "standup";
    await call.save();
    expect(calls).toEqual(["mutate", "modelQuery"]);
    expect(opsSent(sent)).toEqual([
      expect.objectContaining({ op: "create", values: { title: "standup" } }),
    ]);
  });

  it("Model.create sends one mutate and nothing else", async () => {
    const { proxy, calls } = countingProxy();
    await RoundTripCall.create(proxy, { title: "standup" });
    expect(calls).toEqual(["mutate"]);
  });

  it("Model.create with returns: 'row' reads once after the write", async () => {
    const { proxy, calls } = countingProxy();
    // This fake's query finds no row, so the create rejects: a row that
    // cannot be read is an error, not a silent half-filled instance.
    await expect(RoundTripCall.create(proxy, { title: "standup" }, { returns: "row" }))
      .rejects.toThrow(/was created, but reading it back failed/);
    expect(calls).toEqual(["mutate", "modelQuery"]);
  });

  it("create writes a non-empty relation in the same op, and only that one", async () => {
    const { proxy, calls, sent } = countingProxy();
    await RoundTripCall.create(proxy, { title: "standup", tags: ["rt://tag/a", "rt://tag/b"] });
    expect(calls).toEqual(["mutate"]);
    expect(opsSent(sent)).toEqual([
      expect.objectContaining({ op: "create", values: { title: "standup", tags: ["rt://tag/a", "rt://tag/b"] } }),
    ]);
  });

  it("update of an existing record with [] clears the relation", async () => {
    const { proxy, calls, sent } = countingProxy();
    const call = new RoundTripCall(proxy, "rt://call/1");
    call.tags = ["rt://tag/a"];
    (call as any).takeSnapshot();
    call.tags = [];
    await call.save("batch-1");
    expect(calls).toEqual(["mutate"]);
    expect(sent[0]).toEqual([
      "mutate",
      "rt-uuid",
      [{ op: "update", class: "rt://RoundTripCall", id: "rt://call/1", set: { tags: [] } }],
      { batch: "batch-1" },
    ]);
  });

  it("an add of a relation's member is one update op with `add`", async () => {
    const { proxy, calls, sent } = countingProxy();
    const call = new RoundTripCall(proxy, "rt://call/1");
    await (call as any).addTags("rt://tag/a", "batch-1");
    expect(calls).toEqual(["mutate"]);
    expect(opsSent(sent)).toEqual([
      { op: "update", class: "rt://RoundTripCall", id: "rt://call/1", add: { tags: ["rt://tag/a"] } },
    ]);
  });

  it("delete of a decorated model is one mutate", async () => {
    const { proxy, calls, sent } = countingProxy();
    const note = new RoundTripNote(proxy, "rt://note/1");
    await note.delete();
    // The executor runs the destructor, then removes the incoming links.
    expect(calls).toEqual(["mutate"]);
    expect(opsSent(sent)).toEqual([{ op: "delete", class: "rt://RoundTripNote", id: "rt://note/1" }]);
  });

  it("deletes an abstract-class record under the abstract class's IRI", async () => {
    const { proxy, calls, sent } = countingProxy();
    await new RoundTripReadable(proxy, "rt://note/1").delete();
    expect(sent[0]).toEqual([
      "mutate",
      "rt-uuid",
      [{ op: "delete", class: "rt://RoundTripReadable", id: "rt://note/1" }],
      {},
    ]);
    expect(calls).toEqual(["mutate"]);
  });
});

// Staging-like wall time: SDK_RTT_MS=336 npx jest write-round-trips -t "wall time"
const rtt = Number(process.env.SDK_RTT_MS || 0);
(rtt ? describe : describe.skip)("wall time per write at a fixed round trip", () => {
  const runs = Number(process.env.SDK_RTT_RUNS || 10);
  const stats = (ms: number[]) => {
    const s = [...ms].sort((a, b) => a - b);
    return `median ${s[Math.floor(s.length / 2)].toFixed(0)} ms, min ${s[0].toFixed(0)}, max ${s[s.length - 1].toFixed(0)}`;
  };

  async function time(write: (proxy: PerspectiveProxy) => Promise<unknown>) {
    const ms: number[] = [];
    let rpcs = 0;
    for (let i = 0; i < runs; i++) {
      const { proxy, calls } = countingProxy(rtt);
      const start = performance.now();
      await write(proxy);
      ms.push(performance.now() - start);
      rpcs = calls.length;
    }
    return `${rpcs} RPCs, ${stats(ms)}`;
  }

  it("wall time", async () => {
    const rows = {
      "create (save)": await time(createNote),
      "create with 3 empty relations": await time((p) => RoundTripCall.create(p, { title: "standup" })),
      "delete": await time((p) => new RoundTripNote(p, "rt://note/1").delete()),
      "delete through abstract class": await time((p) => new RoundTripReadable(p, "rt://note/1").delete()),
    };
    console.log(`RTT ${rtt} ms, ${runs} runs\n` + Object.entries(rows).map(([k, v]) => `${k}: ${v}`).join("\n"));
  });
});

describe("an empty value clears a property", () => {
  const existing = async (proxy: PerspectiveProxy, note: string | null | undefined) => {
    const opt = new RoundTripOptional(proxy, "rt://opt/1");
    opt.note = "old";
    (opt as any).takeSnapshot();
    (opt as any).note = note;
    return opt;
  };

  it.each([[""], [null]])("update with %p sets the property to null, in the caller's batch", async (empty) => {
    const { proxy, sent } = countingProxy();
    const opt = await existing(proxy, empty as any);
    await opt.save("batch-1");
    expect(sent).toEqual([[
      "mutate",
      "rt-uuid",
      [{ op: "update", class: "rt://RoundTripOptional", id: "rt://opt/1", set: { note: null } }],
      { batch: "batch-1" },
    ]]);
  });

  it.each([[""], [null]])("Model.create with %p sends no value for it", async (empty) => {
    const { proxy, calls, sent } = countingProxy();
    await RoundTripOptional.create(proxy, { note: empty as any, rank: "1" });
    expect(calls).toEqual(["mutate"]);
    expect(opsSent(sent)).toEqual([expect.objectContaining({ op: "create", values: { rank: "1" } })]);
  });

  it("an absent value writes nothing", async () => {
    const { proxy, calls } = countingProxy();
    const opt = await existing(proxy, undefined);
    await opt.save("batch-1");
    expect(calls).toEqual([]);
  });

  it("a property already cleared, set to '' again, sends the null and reads nothing", async () => {
    const { proxy, calls } = countingProxy();
    const opt = await existing(proxy, "");
    await opt.save("batch-1");
    expect(calls).toEqual(["mutate"]);
  });

  // The executor refuses it (`missingRequired`); the SDK does not decide.
  it.each([[""], [null]])("update with %p on a required property is sent as null for the executor to refuse", async (empty) => {
    const { proxy, sent } = countingProxy();
    const req = new RoundTripRequired(proxy, "rt://req/1");
    req.title = "old";
    (req as any).takeSnapshot();
    (req as any).title = empty;
    await req.save("batch-1");
    expect(opsSent(sent)).toEqual([
      { op: "update", class: "rt://RoundTripRequired", id: "rt://req/1", set: { title: null } },
    ]);
  });

  describe("on a class built by fromSHACL", () => {
    // Decorator -> SHACL -> fromSHACL: the shape's minCount 1 must come back as required.
    const fromShape = (cls: any) => Ad4mModel.fromSHACL(cls.generateSHACL().shape, cls.getClassName());

    it("a scalar with minCount 1 is required, and one without is not (seen in the shape it generates back)", () => {
      const minCount = (cls: any) => fromShape(cls).generateSHACL().shape.properties.find((p: any) => p.name).minCount;
      expect(minCount(RoundTripRequired)).toBe(1);
      expect(minCount(RoundTripOptional)).toBeUndefined();
    });

    it.each([[""], [null]])("update with %p on a scalar clears through the same op", async (empty) => {
      const Opt = fromShape(RoundTripOptional) as any;
      const { proxy, sent } = countingProxy();
      const opt = new Opt(proxy, "rt://opt/1");
      opt.note = "old";
      (opt as any).takeSnapshot();
      opt.note = empty;
      await opt.save("batch-1");
      expect(opsSent(sent)).toEqual([
        expect.objectContaining({ op: "update", id: "rt://opt/1", set: { note: null } }),
      ]);
    });
  });

  it("update clears one property and sets another in one op", async () => {
    const { proxy, calls, sent } = countingProxy();
    const opt = await existing(proxy, "");
    opt.rank = "2";
    await opt.save("batch-1");
    expect(calls).toEqual(["mutate"]);
    expect(opsSent(sent)).toEqual([
      { op: "update", class: "rt://RoundTripOptional", id: "rt://opt/1", set: { note: null, rank: "2" } },
    ]);
  });
});
