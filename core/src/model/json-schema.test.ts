import { Ad4mModel } from "./Ad4mModel";

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
