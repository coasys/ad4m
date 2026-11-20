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
