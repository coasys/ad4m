/**
 * What `registerShapes` sends: the shape a model generates, as JSON the
 * executor hashes and diffs. Every test here pins a property the executor's
 * "unchanged" answer depends on: the same model gives the same JSON, and the
 * JSON is the whole shape.
 */
import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, Flag, HasMany } from "./decorators";
import { SHACLShape } from "../shacl/SHACLShape";
import type { PropertyShapeJson } from "../generated/api/PropertyShapeJson";
import type { ShapeJson } from "../generated/api/ShapeJson";

describe("registerShapes payload", () => {
  it("@Model({ namespace }) pins the class namespace whatever the first field", () => {
    @Model({ name: "Pinned", namespace: "app://" })
    class Pinned extends Ad4mModel {
      @Property({ through: "shared://title" })
      title: string = "";

      @Property({ through: "app://body" })
      body: string = "";
    }

    @Model({ name: "Derived" })
    class Derived extends Ad4mModel {
      @Property({ through: "shared://title" })
      title: string = "";
    }

    const pinned = Pinned.generateSHACL().shape as SHACLShape;
    expect(pinned.targetClass).toBe("app://Pinned");
    expect(pinned.nodeShapeUri).toBe("app://PinnedShape");
    // Without it, the first property's scheme still names the class.
    expect((Derived.generateSHACL().shape as SHACLShape).targetClass).toBe("shared://Derived");
  });

  it("a relation filter read before its target class exists is resolved once it does", () => {
    let defined: typeof Ad4mModel | undefined;

    @Model({ name: "Owner" })
    class Owner extends Ad4mModel {
      @HasMany({
        through: "ns://item",
        target: () => {
          if (!defined) throw new ReferenceError("Item is not defined yet");
          return defined;
        },
      })
      items: string[] = [];
    }

    const shape = Owner.generateSHACL().shape as SHACLShape;
    const items = () => shape.properties.find(p => p.name === "items")!;
    // First read: the target class does not exist yet.
    expect(items().getter).toBeUndefined();

    @Model({ name: "Item" })
    class Item extends Ad4mModel {
      @Flag({ through: "ns://kind", value: "ns://item" })
      kind: string = "";
    }
    defined = Item;

    expect(items().conformanceConditions).toEqual([
      { type: "flag", predicate: "ns://kind", value: "ns://item" },
    ]);
    expect(items().getter).toContain("ns://kind");
  });

  it("unnamed properties get distinct nodes named by their path", () => {
    const shape = new SHACLShape("ns://Thing");
    shape.addProperty({ path: "ns://first" });
    shape.addProperty({ path: "ns://second" });

    const nodes = shape.toLinks()
      .filter(l => l.source === "ns://ThingShape" && l.predicate === "sh://property")
      .map(l => l.target);
    expect(nodes).toEqual(["ns://Thing.first", "ns://Thing.second"]);
  });

  it("a property name with a dot reads back whole", () => {
    const shape = new SHACLShape("ns://Thing");
    shape.addProperty({ name: "geo.lat", path: "ns://lat" });

    const back = SHACLShape.fromLinks(shape.toLinks(), shape.nodeShapeUri);
    expect(back.properties.map(p => p.name)).toEqual(["geo.lat"]);
  });

  it("JSON literals spell every object with sorted keys", () => {
    const shape = new SHACLShape("ns://Thing");
    shape.setConstructorActions([{ action: "addLink", source: "this", predicate: "ns://kind", target: "ns://thing" }]);
    const constructor = shape.toLinks().find(l => l.predicate === "ad4m://constructor")!;
    expect(constructor.target).toBe(
      'literal:string:[{"action":"addLink","predicate":"ns://kind","source":"this","target":"ns://thing"}]',
    );
  });

  it("toJSON carries every field of the wire shape, and fromJSON reads them all back", () => {
    // `Required<…>` makes the compiler list every field the executor takes:
    // a field added to the wire type and missed here fails to compile.
    const property: Required<PropertyShapeJson> = {
      path: "ns://name",
      name: "name",
      interpretation_hint: "what it is called",
      identity: true,
      ordering: "lexorank",
      datatype: "xsd://string",
      min_count: 1,
      max_count: 3,
      writable: true,
      local: true,
      resolve_language: "literal",
      node_kind: "Literal",
      collection: true,
      setter: [{ action: "setSingleTarget", source: "this", predicate: "ns://name", target: "value" }],
      adder: [{ action: "addLink", source: "this", predicate: "ns://name", target: "value" }],
      remover: [{ action: "removeLink", source: "this", predicate: "ns://name", target: "value" }],
      getter: "SELECT ?v WHERE { ?s <ns://name> ?v }",
      conformance_conditions: [{ type: "flag", predicate: "ns://kind", value: "ns://x" }],
      class: "ns://OtherShape",
      relation_kind: "hasMany",
      target_class_name: "Other",
      where_filter: { status: "open" },
      where_predicates: { status: "ns://status" },
      filter: false,
      has_value: "ns://x",
      transform: { type: "focus" },
      in: [{ value: "a", label: "A" }],
      pattern: "^a",
      min_inclusive: 0,
      max_inclusive: 9,
      alias: { TextContent: "text" },
    };
    const json: Required<ShapeJson> = {
      node_shape_uri: "ns://ThingShape",
      target_class: "ns://Thing",
      properties: [property],
      interpretation_hint: "a thing",
      constructor_actions: [{ action: "addLink", source: "this", predicate: "ns://kind", target: "ns://thing" }],
      destructor_actions: [{ action: "removeLink", source: "this", predicate: "ns://kind", target: "ns://thing" }],
      parent_shapes: ["ns://BaseShape"],
      abstract: true,
    };

    const sent = JSON.parse(JSON.stringify(SHACLShape.fromJSON(json).toJSON()));
    expect(sent).toEqual(json);
  });
});
