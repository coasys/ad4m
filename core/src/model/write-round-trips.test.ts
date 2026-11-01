import { Ad4mModel } from "./Ad4mModel";
import { Model, Property } from "./decorators";
import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";

// Every RPC that save() and delete() send, counted on a client double that
// keeps the registered classes' SHACL links like the executor does.

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

const link = (source: string, predicate: string, target: string) => ({
  author: "did:test:agent",
  timestamp: "1",
  data: { source, predicate, target },
});

/** The SHACL links the executor stores for `classes`, with the class marker
 *  it adds (`shacl_parser.rs`). */
function shaclLinks(classes: any[]) {
  return classes.flatMap((cls) => {
    const { shape } = cls.generateSHACL();
    return [
      link(shape.targetClass, "rdf://type", "ad4m://SubjectClass"),
      ...shape.toLinks().map((l: any) => link(l.source, l.predicate, l.target)),
    ];
  });
}

function countingProxy(registered: any[], rttMs = 0) {
  const store = shaclLinks(registered);
  const calls: string[] = [];
  const sent: any[][] = [];
  const answers: Record<string, (...args: any[]) => any> = {
    queryLinks: (_uuid: string, q: any) =>
      store.filter(
        (l) =>
          (!q.source || l.data.source === q.source) &&
          (!q.predicate || l.data.predicate === q.predicate) &&
          (!q.target || l.data.target === q.target),
      ),
    createBatch: () => "batch-1",
    commitBatch: () => ({ additions: [], removals: [] }),
    createSubject: () => true,
    removeSubject: () => true,
    executeCommands: () => true,
    removeLinks: () => [],
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

async function createNote(proxy: PerspectiveProxy) {
  const note = new RoundTripNote(proxy, "rt://note/1");
  note.title = "hello";
  await note.save();
  return note;
}

describe("RPCs per write", () => {
  it("create of a decorated model sends no class-list lookup", async () => {
    const { proxy, calls } = countingProxy([RoundTripNote]);
    await createNote(proxy);
    expect(calls).toEqual(["createBatch", "createSubject", "commitBatch", "modelQuery"]);
  });

  it("delete of a decorated model sends no class-list lookup", async () => {
    const { proxy, calls } = countingProxy([RoundTripNote]);
    const note = new RoundTripNote(proxy, "rt://note/1");
    await note.delete();
    // abstract markers, destructor, the destructor's commands, incoming links
    expect(calls).toEqual(["queryLinks", "queryLinks", "executeCommands", "queryLinks"]);
  });
});

describe("class name a write resolves", () => {
  @Model({ name: "RoundTripDerivedNote" })
  class RoundTripDerivedNote extends RoundTripNote {
    @Property({ through: "rt://body" })
    body: string = "";
  }

  const createdAs = (sent: any[][]) => JSON.parse(sent.find((c) => c[0] === "createSubject")![2]).className;

  async function resolve(registered: any[], subject: any) {
    const { proxy } = countingProxy(registered);
    return proxy.stringOrTemplateObjectToSubjectClassName(subject);
  }

  it("is the decorated name of an instance and of its class", async () => {
    expect(await resolve([RoundTripNote], new RoundTripNote(undefined as any))).toBe("RoundTripNote");
    expect(await resolve([RoundTripNote], RoundTripNote)).toBe("RoundTripNote");
  });

  it("is the base class for a base instance when a derived class has its properties too", async () => {
    const both = [RoundTripDerivedNote, RoundTripNote];
    expect(await resolve(both, new RoundTripNote(undefined as any))).toBe("RoundTripNote");
    expect(await resolve(both, new RoundTripDerivedNote(undefined as any))).toBe("RoundTripDerivedNote");
  });

  it("is the decorated name even when the class is not registered and another class has its properties", async () => {
    const { proxy, calls } = countingProxy([]);
    const byProperties = jest.spyOn(proxy as any, "findClassByProperties").mockResolvedValue("RoundTripTwin" as never);
    expect(await proxy.stringOrTemplateObjectToSubjectClassName(new RoundTripNote(undefined as any))).toBe("RoundTripNote");
    expect(byProperties).not.toHaveBeenCalled();
    expect(calls).toEqual([]);
  });

  it("is a template's own className", async () => {
    expect(await resolve([RoundTripNote], { className: "RoundTripNote" })).toBe("RoundTripNote");
  });

  it("is the abstract name for an abstract class", async () => {
    expect(await resolve([RoundTripReadable], RoundTripReadable)).toBe("RoundTripReadable");
  });

  it("is a string as given", async () => {
    expect(await resolve([], "Anything")).toBe("Anything");
  });

  it("matches an undecorated object by its properties", async () => {
    const { proxy, calls } = countingProxy([RoundTripNote]);
    const byProperties = jest.spyOn(proxy as any, "findClassByProperties").mockResolvedValue("RoundTripNote" as never);
    expect(await proxy.stringOrTemplateObjectToSubjectClassName({ title: "" })).toBe("RoundTripNote");
    expect(byProperties).toHaveBeenCalled();
    expect(calls).toEqual([]);
  });

  it("creates the record under the model's name", async () => {
    const { proxy, sent } = countingProxy([RoundTripNote]);
    await createNote(proxy);
    expect(createdAs(sent)).toBe("RoundTripNote");
  });

  it("deletes an abstract-class record through the executor under the abstract name", async () => {
    const { proxy, calls, sent } = countingProxy([RoundTripReadable]);
    await new RoundTripReadable(proxy, "rt://note/1").delete();
    expect(sent.find((c) => c[0] === "removeSubject")).toEqual(
      ["removeSubject", "rt-uuid", "RoundTripReadable", "rt://note/1", undefined],
    );
    // abstract markers, removeSubject, incoming links
    expect(calls).toEqual(["queryLinks", "removeSubject", "queryLinks"]);
  });

  it("createSubject with a decorated instance sends its name in one RPC", async () => {
    const { proxy, calls, sent } = countingProxy([RoundTripNote]);
    await proxy.createSubject(new RoundTripNote(undefined as any), "rt://note/2");
    expect(calls).toEqual(["createSubject"]);
    expect(createdAs(sent)).toBe("RoundTripNote");
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
      const { proxy, calls } = countingProxy([RoundTripNote, RoundTripReadable], rtt);
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
      "delete": await time((p) => new RoundTripNote(p, "rt://note/1").delete()),
      "delete through abstract class": await time((p) => new RoundTripReadable(p, "rt://note/1").delete()),
      "createSubject(decorated instance)": await time((p) => p.createSubject(new RoundTripNote(undefined as any), "rt://n")),
    };
    console.log(`RTT ${rtt} ms, ${runs} runs\n` + Object.entries(rows).map(([k, v]) => `${k}: ${v}`).join("\n"));
  });
});
