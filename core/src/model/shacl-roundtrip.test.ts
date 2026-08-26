/**
 * Decorator → SHACL writer round-trip tests.
 *
 * Spec R11 gate on the TypeScript side: any field a decorator sets must
 * survive into the SHACL graph produced by `generateSHACL()`.  These tests
 * compare against the SHACL link emission (the format the Rust executor
 * stores and reads), not against `getModelMetadata()` — which is the
 * dying shape transport, not the source of truth.
 */
import { Ad4mModel } from "./Ad4mModel";
import {
  Model,
  Property,
  Flag,
  HasMany,
  HasOne,
  BelongsToOne,
  BelongsToMany,
} from "./decorators";

function flatten(
  shape: any,
): Array<{ source: string; predicate: string; target: string }> {
  return shape.toLinks().map((l: any) => ({
    source: l.source,
    predicate: l.predicate,
    target: l.target,
  }));
}

function findLinkTarget(
  links: any[],
  source: string,
  predicate: string,
): string | undefined {
  return links.find((l) => l.source === source && l.predicate === predicate)
    ?.target;
}

describe("Decorators → SHACL writer round-trip", () => {
  it("emits ad4m:relationKind for every relation kind", () => {
    @Model({ name: "Target" })
    class Target extends Ad4mModel {}

    @Model({ name: "Holder" })
    class Holder extends Ad4mModel {
      @HasMany({ through: "ns://hm", target: () => Target })
      hmRel: string[] = [];

      @HasOne({ through: "ns://ho", target: () => Target })
      hoRel: string = "";

      @BelongsToOne({ through: "ns://bto", target: () => Target })
      btoRel: string = "";

      @BelongsToMany({ through: "ns://btm", target: () => Target })
      btmRel: string[] = [];
    }

    const { shape } = Holder.generateSHACL();
    const links = flatten(shape);
    const propUri = (name: string) => `ns://Holder.${name}`;

    expect(findLinkTarget(links, propUri("hmRel"), "ad4m://relationKind")).toBe(
      "literal:string:hasMany",
    );
    expect(findLinkTarget(links, propUri("hoRel"), "ad4m://relationKind")).toBe(
      "literal:string:hasOne",
    );
    expect(
      findLinkTarget(links, propUri("btoRel"), "ad4m://relationKind"),
    ).toBe("literal:string:belongsToOne");
    expect(
      findLinkTarget(links, propUri("btmRel"), "ad4m://relationKind"),
    ).toBe("literal:string:belongsToMany");
  });

  it("emits maxCount=1 for scalar relations", () => {
    @Model({ name: "Target2" })
    class Target2 extends Ad4mModel {}

    @Model({ name: "Holder2" })
    class Holder2 extends Ad4mModel {
      @HasOne({ through: "ns://ho", target: () => Target2 })
      hoRel: string = "";

      @BelongsToOne({ through: "ns://bto", target: () => Target2 })
      btoRel: string = "";
    }

    const { shape } = Holder2.generateSHACL();
    const links = flatten(shape);

    expect(findLinkTarget(links, "ns://Holder2.hoRel", "sh://maxCount")).toBe(
      "literal:1^^xsd:integer",
    );
    expect(findLinkTarget(links, "ns://Holder2.btoRel", "sh://maxCount")).toBe(
      "literal:1^^xsd:integer",
    );
  });

  it("emits ad4m:targetClassName for relations with a target", () => {
    @Model({ name: "Ingredient" })
    class Ingredient extends Ad4mModel {}

    @Model({ name: "Recipe" })
    class Recipe extends Ad4mModel {
      @HasMany({ through: "ns://ing", target: () => Ingredient })
      ingredients: string[] = [];
    }

    const { shape } = Recipe.generateSHACL();
    const links = flatten(shape);

    expect(
      findLinkTarget(
        links,
        "ns://Recipe.ingredients",
        "ad4m://targetClassName",
      ),
    ).toBe("literal:string:Ingredient");
  });

  it("emits sh:hasValue for @Flag properties", () => {
    @Model({ name: "Channel" })
    class Channel extends Ad4mModel {
      @Flag({ through: "ns://type", value: "ns://Channel" })
      type: string = "";
    }

    const { shape } = Channel.generateSHACL();
    const links = flatten(shape);

    const hasValue = findLinkTarget(
      links,
      "ns://Channel.type",
      "sh://hasValue",
    );
    expect(hasValue).toBeDefined();
    // Writer emits literal:<value> for URI-style flags.
    expect(hasValue).toContain("ns://Channel");
  });

  it("encodes ad4m:filter=false when explicitly disabled", () => {
    @Model({ name: "Tag" })
    class Tag extends Ad4mModel {}

    @Model({ name: "Doc" })
    class Doc extends Ad4mModel {
      @HasMany({ through: "ns://tag", target: () => Tag, filter: false })
      tags: string[] = [];
    }

    const { shape } = Doc.generateSHACL();
    const links = flatten(shape);

    expect(findLinkTarget(links, "ns://Doc.tags", "ad4m://filter")).toBe(
      "literal:false",
    );
  });

  it("does not emit ad4m:filter when defaulted to true", () => {
    @Model({ name: "Tag2" })
    class Tag2 extends Ad4mModel {}

    @Model({ name: "Doc2" })
    class Doc2 extends Ad4mModel {
      @HasMany({ through: "ns://tag", target: () => Tag2 })
      tags: string[] = [];
    }

    const { shape } = Doc2.generateSHACL();
    const links = flatten(shape);

    expect(
      findLinkTarget(links, "ns://Doc2.tags", "ad4m://filter"),
    ).toBeUndefined();
  });

  it("emits scalar property predicates", () => {
    @Model({ name: "Profile" })
    class Profile extends Ad4mModel {
      @Property({ through: "ns://name" })
      name: string = "";
    }

    const { shape } = Profile.generateSHACL();
    const links = flatten(shape);

    expect(findLinkTarget(links, "ns://Profile.name", "sh://path")).toBe(
      "ns://name",
    );
    // A bare @Property is deterministic-literal by default — no
    // resolveLanguage link is emitted.
    expect(
      findLinkTarget(links, "ns://Profile.name", "ad4m://resolveLanguage"),
    ).toBeUndefined();
  });

  it("emits ad4m://interpretation_hint on the class shape and per property", () => {
    @Model({
      name: "Belief",
      interpretationHint:
        "A claim a participant holds to be true about the world or the group. Not a task or a question.",
    })
    class Belief extends Ad4mModel {
      @Property({
        through: "ns://title",
        required: true,
        resolveLanguage: "literal",
        interpretationHint: "One-sentence statement in the claimant's framing.",
      })
      title: string = "";

      @Property({ through: "ns://body", resolveLanguage: "literal" })
      body: string = "";
    }

    const { shape } = Belief.generateSHACL();
    const links = flatten(shape);

    expect(findLinkTarget(links, shape.nodeShapeUri, "ad4m://interpretation_hint"))
      .toBe(
        "literal:string:A claim a participant holds to be true about the " +
        "world or the group. Not a task or a question.",
      );
    expect(
      findLinkTarget(links, "ns://Belief.title", "ad4m://interpretation_hint"),
    ).toBe(
      "literal:string:One-sentence statement in the claimant's framing.",
    );
    expect(
      findLinkTarget(links, "ns://Belief.body", "ad4m://interpretation_hint"),
    ).toBeUndefined();
  });

  it("emits ad4m://interpretation_hint on relation properties", () => {
    // Relation-level interpretation hints (RelationOptions.interpretationHint)
    // are read by the Rust harness's `ShaclProperty.interpretation_hint` and
    // rendered into the `_propose_link_child` predicate-field description so
    // the LLM knows what each relation MEANS semantically, not just that it
    // exists. Regression against the follow-up-branch scope creep: pulled
    // into PR #911 per Nico's 2026-08-24 ask.
    @Model({ name: "Intention" })
    class Intention extends Ad4mModel {
      @Property({
        through: "ns://title",
        required: true,
        interpretationHint: "Imperative statement of intent.",
      })
      title: string = "";

      @HasMany(() => Belief, {
        through: "ns://basedOn",
        interpretationHint:
          "The prior beliefs this intention derives from.",
      })
      basedOn: Belief[] = [];

      @HasMany(() => Belief, { through: "ns://contradicts" })
      contradicts: Belief[] = [];
    }
    @Model({ name: "Belief" })
    class Belief extends Ad4mModel {
      @Property({ through: "ns://title", required: true }) title: string = "";
    }

    const { shape } = Intention.generateSHACL();
    const links = flatten(shape);

    // Named relation with a hint → link present on the relation's property
    // node with the hint text.
    expect(
      findLinkTarget(links, "ns://Intention.basedOn", "ad4m://interpretation_hint"),
    ).toBe(
      "literal:string:The prior beliefs this intention derives from.",
    );

    // Named relation WITHOUT a hint → no interpretation_hint link (the
    // scalar-property "no hint = no link" path is what relations follow too).
    expect(
      findLinkTarget(
        links,
        "ns://Intention.contradicts",
        "ad4m://interpretation_hint",
      ),
    ).toBeUndefined();
  });

  it("round-trips interpretationHint through toJSON/fromJSON", () => {
    @Model({
      name: "Task",
      interpretationHint:
        "Something a participant commits to doing - actionable outcome.",
    })
    class Task extends Ad4mModel {
      @Property({
        through: "ns://title",
        required: true,
        resolveLanguage: "literal",
        interpretationHint: "Imperative summary of the work.",
      })
      title: string = "";
    }

    const { shape } = Task.generateSHACL();
    const json: any = shape.toJSON();

    expect(json.interpretation_hint).toBe(
      "Something a participant commits to doing - actionable outcome.",
    );
    const titleProp = json.properties.find((p: any) => p.name === "title");
    expect(titleProp.interpretation_hint).toBe(
      "Imperative summary of the work.",
    );

    const rebuilt = (shape.constructor as any).fromJSON(json);
    expect(rebuilt.interpretationHint).toBe(
      "Something a participant commits to doing - actionable outcome.",
    );
    const rebuiltTitle = rebuilt.properties.find(
      (p: any) => p.name === "title",
    );
    expect(rebuiltTitle.interpretationHint).toBe(
      "Imperative summary of the work.",
    );
  });
});
