import { Ad4mModel } from "./Ad4mModel";
import { Literal } from "../Literal";

describe("fromJSONSchema: reserved property names", () => {
  for (const name of [
    "id",
    "timestamp",
    "createdAt",
    "updatedAt",
    "baseExpression",
    "author",
    "__links",
  ]) {
    it(`refuses a top-level "${name}" property`, () => {
      const schema = {
        title: "Quest",
        type: "object",
        properties: { [name]: { type: "string" }, title: { type: "string" } },
      };
      expect(() =>
        Ad4mModel.fromJSONSchema(schema, {
          name: "Quest",
          namespace: "quest://",
        }),
      ).toThrow(`"${name}"`);
    });
  }

  it("names every reserved property in one error", () => {
    const schema = {
      title: "Event",
      type: "object",
      properties: { id: { type: "string" }, timestamp: { type: "number" } },
    };
    expect(() =>
      Ad4mModel.fromJSONSchema(schema, {
        name: "Event",
        namespace: "event://",
      }),
    ).toThrow(/"id", "timestamp"/);
  });

  it("keeps Ad4mModel's id getter once the schema renames id", () => {
    const schema = {
      title: "Quest",
      type: "object",
      properties: { questId: { type: "string" } },
      required: ["questId"],
    };
    const Quest: any = Ad4mModel.fromJSONSchema(schema, {
      name: "Quest",
      namespace: "quest://",
    });
    expect(new Quest(undefined, "holons://Holons/x/quests/q1").id).toBe(
      "holons://Holons/x/quests/q1",
    );
  });
});

describe("fromJSONSchema: enum", () => {
  const schema = {
    title: "FedRequest",
    type: "object",
    properties: {
      status: { type: "string", enum: ["pending", "accepted", "declined"] },
      hops: { type: "number", enum: [1, 2] },
      note: { type: "string" },
    },
    required: ["status"],
  };

  it("maps a string enum to sh:in", () => {
    const Cls: any = Ad4mModel.fromJSONSchema(schema, {
      name: "FedRequest",
      namespace: "fed://",
    });
    const props = Object.fromEntries(
      Cls.generateSHACL().shape.properties.map((p: any) => [p.name, p]),
    );
    expect(props.status.in).toEqual([
      { value: "pending" },
      { value: "accepted" },
      { value: "declined" },
    ]);
    expect(props.hops.in).toBeUndefined(); // not a string enum: left unconstrained
    expect(props.note.in).toBeUndefined();
    expect(Cls.getModelMetadata().properties.status.required).toBe(true);
  });

  it("lets propertyOptions override the enum", () => {
    const Cls: any = Ad4mModel.fromJSONSchema(schema, {
      name: "FedRequest2",
      namespace: "fed://",
      propertyOptions: {
        status: { options: [{ value: "fed://pending", label: "Pending" }] },
      },
    });
    const status = Cls.generateSHACL().shape.properties.find(
      (p: any) => p.name === "status",
    );
    expect(status.in).toEqual([{ value: "fed://pending", label: "Pending" }]);
  });
});

describe("fromJSONSchema: array of objects", () => {
  // `any`: schemas come from JSON, and real ones leave out `items.type`.
  const schema: any = {
    title: "Expense",
    type: "object",
    properties: {
      title: { type: "string" },
      split: {
        type: "array",
        items: {
          type: "object",
          properties: { did: { type: "string" }, share: { type: "number" } },
        },
      },
      participants: {
        type: "array",
        items: {
          properties: { id: { type: "string" }, username: { type: "string" } },
        },
      },
      owedBy: { type: "array", items: { type: "string" } },
      tags: { type: "array" },
    },
    required: ["split"],
  };
  const build = (name: string): any =>
    Ad4mModel.fromJSONSchema(schema, { name, namespace: "expense://" });

  it("makes it one property, not a relation", () => {
    const Cls = build("ExpenseA");
    const meta = Cls.getModelMetadata();
    for (const name of ["split", "participants"]) {
      expect(meta.properties[name]).toBeDefined();
      expect(meta.relations[name]).toBeUndefined();
    }
    const shape = Cls.generateSHACL().shape;
    const split = shape.properties.find((p: any) => p.name === "split");
    expect(split.path).toBe("expense://split");
    expect(split.maxCount).toBe(1);
    expect(meta.properties.split.initial).toBe("literal:json:[]");
  });

  it("keeps arrays of scalars and arrays without items as relations", () => {
    const meta = build("ExpenseB").getModelMetadata();
    expect(meta.relations.owedBy).toBeDefined();
    expect(meta.relations.tags).toBeDefined();
    expect(meta.properties.owedBy).toBeUndefined();
    expect(meta.properties.tags).toBeUndefined();
  });

  it("writes the whole array as one JSON literal and reads it back", async () => {
    const Cls = build("ExpenseC");
    const split = [
      { did: "did:key:a", share: 2 },
      { did: "did:key:b", share: 1 },
    ];
    const writes = await (
      new Cls(undefined, "expense://e1") as any
    ).planProperty("split", split);
    expect(writes).toHaveLength(1);
    const value = writes[0].parameters[0].value;
    expect(value).toMatch(/^literal:json:/);
    expect(value).not.toContain("object%20Object");
    expect(Literal.fromUrl(value).get()).toEqual(split);
  });
});
