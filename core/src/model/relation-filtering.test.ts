/**
 * Tests for relation filtering via conformance getters stored in SHACL shapes.
 *
 * Validates that:
 * - buildConformanceFilter() correctly derives conditions from target model metadata
 * - SHACL shapes serialize & deserialize getter + conformanceConditions through
 *   all serialization paths (toLinks/fromLinks, toJSON/fromJSON)
 * - generateSHACL() populates getter on relation shapes when target has flags/required
 * - filter:false and explicit getter options are respected
 */
import { Ad4mModel } from "./Ad4mModel";
import {
  Model,
  Property,
  Flag,
  HasMany,
  Optional,
  buildConformanceFilter,
} from "./decorators";
import {
  SHACLShape,
  SHACLPropertyShape,
  ConformanceCondition,
} from "../shacl/SHACLShape";

// ============================================================================
// Test models
// ============================================================================

@Model({ name: "FlaggedTarget" })
class FlaggedTarget extends Ad4mModel {
  @Flag({ through: "test://type", value: "test://flagged_type" })
  type: string = "";

  @Property({ through: "test://name", required: true })
  name: string = "";
}

@Model({ name: "MinimalTarget" })
class MinimalTarget extends Ad4mModel {
  @Optional({ through: "test://optionalProp" })
  optionalProp: string = "";
}

@Model({ name: "ParentWithRelations" })
class ParentWithRelations extends Ad4mModel {
  @HasMany({
    through: "test://has_flagged",
    target: () => FlaggedTarget,
  })
  flaggedItems: string[] = [];

  @HasMany({
    through: "test://has_minimal",
    target: () => MinimalTarget,
  })
  minimalItems: string[] = [];

  @HasMany({
    through: "test://has_unfiltered",
    target: () => FlaggedTarget,
    filter: false,
  })
  unfilteredItems: string[] = [];

  @HasMany({
    getter: "(<-link[WHERE predicate = 'test://custom'].in.uri)",
  })
  customItems: string[] = [];
}

// ============================================================================
// buildConformanceFilter tests
// ============================================================================

describe("buildConformanceFilter()", () => {
  it("should derive flag + required conditions from target model", () => {
    const result = buildConformanceFilter("test://has_flagged", FlaggedTarget);

    expect(result).toBeDefined();
    expect(result!.conformanceConditions).toHaveLength(2);

    // Flag condition
    const flagCond = result!.conformanceConditions.find(c => c.type === "flag");
    expect(flagCond).toBeDefined();
    expect(flagCond!.predicate).toBe("test://type");
    expect(flagCond!.value).toBe("test://flagged_type");

    // Required condition
    const reqCond = result!.conformanceConditions.find(c => c.type === "required");
    expect(reqCond).toBeDefined();
    expect(reqCond!.predicate).toBe("test://name");
    expect(reqCond!.value).toBeUndefined();

    // Getter string should be a SurrealQL expression
    expect(result!.getter).toContain("->link[WHERE predicate =");
    expect(result!.getter).toContain("test://has_flagged");
    expect(result!.getter).toContain("test://type");
    expect(result!.getter).toContain("test://flagged_type");
    expect(result!.getter).toContain("test://name");
  });

  it("should return undefined for a target with no conformance conditions", () => {
    const result = buildConformanceFilter("test://has_minimal", MinimalTarget);
    expect(result).toBeUndefined();
  });

  it("should handle class with only flags (no required properties)", () => {
    @Model({ name: "FlagOnly" })
    class FlagOnly extends Ad4mModel {
      @Flag({ through: "test://kind", value: "test://special" })
      kind: string = "";
    }

    const result = buildConformanceFilter("test://pred", FlagOnly);
    expect(result).toBeDefined();
    expect(result!.conformanceConditions).toHaveLength(1);
    expect(result!.conformanceConditions[0].type).toBe("flag");
    expect(result!.conformanceConditions[0].predicate).toBe("test://kind");
    expect(result!.conformanceConditions[0].value).toBe("test://special");
  });

  it("should skip properties with custom getters in required check", () => {
    @Model({ name: "CustomGetterTarget" })
    class CustomGetterTarget extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://cgt" })
      type: string = "";

      @Property({
        through: "test://computed",
        required: true,
        getter: "(<-link[WHERE predicate = 'test://custom'].in.uri)[0]",
      })
      computed: string = "";
    }

    const result = buildConformanceFilter("test://pred", CustomGetterTarget);
    expect(result).toBeDefined();
    // Should have flag condition but NOT required condition for 'computed' (has custom getter)
    expect(result!.conformanceConditions).toHaveLength(1);
    expect(result!.conformanceConditions[0].type).toBe("flag");
  });
});

// ============================================================================
// SHACL serialization round-trip tests
// ============================================================================

describe("SHACL shape getter serialization", () => {
  const testGetter = "(->link[WHERE predicate = 'test://pred'].out[WHERE count(->link[WHERE predicate = 'test://type' AND out.uri = 'test://flag']) > 0].uri)";
  const testConditions: ConformanceCondition[] = [
    { type: "flag", predicate: "test://type", value: "test://flag" },
    { type: "required", predicate: "test://name" },
  ];

  it("should round-trip getter + conditions through toLinks/fromLinks", () => {
    const shape = new SHACLShape("test://Parent");
    const prop: SHACLPropertyShape = {
      name: "items",
      path: "test://has_items",
      nodeKind: "IRI",
      getter: testGetter,
      conformanceConditions: testConditions,
    };
    shape.addProperty(prop);

    const links = shape.toLinks();

    // Verify getter link exists
    const getterLink = links.find(l => l.predicate === "ad4m://getter");
    expect(getterLink).toBeDefined();
    expect(getterLink!.target).toBe(`literal://string:${testGetter}`);

    // Verify conditions link exists
    const conditionsLink = links.find(l => l.predicate === "ad4m://conformanceConditions");
    expect(conditionsLink).toBeDefined();

    // Reconstruct from links
    const shapeUri = shape.nodeShapeUri;
    const linkObjs = links.map(l => ({
      source: l.source,
      predicate: l.predicate,
      target: l.target,
    }));
    const reconstructed = SHACLShape.fromLinks(linkObjs as any, shapeUri);

    expect(reconstructed.properties).toHaveLength(1);
    expect(reconstructed.properties[0].getter).toBe(testGetter);
    expect(reconstructed.properties[0].conformanceConditions).toEqual(testConditions);
  });

  it("should round-trip getter + conditions through toJSON/fromJSON", () => {
    const shape = new SHACLShape("test://Parent");
    const prop: SHACLPropertyShape = {
      name: "items",
      path: "test://has_items",
      getter: testGetter,
      conformanceConditions: testConditions,
    };
    shape.addProperty(prop);

    const json = shape.toJSON();
    const reconstructed = SHACLShape.fromJSON(json);

    expect(reconstructed.properties).toHaveLength(1);
    expect(reconstructed.properties[0].getter).toBe(testGetter);
    expect(reconstructed.properties[0].conformanceConditions).toEqual(testConditions);
  });

  it("should omit getter/conditions links when not set", () => {
    const shape = new SHACLShape("test://Basic");
    const prop: SHACLPropertyShape = {
      name: "simple",
      path: "test://simple",
    };
    shape.addProperty(prop);

    const links = shape.toLinks();
    expect(links.find(l => l.predicate === "ad4m://getter")).toBeUndefined();
    expect(links.find(l => l.predicate === "ad4m://conformanceConditions")).toBeUndefined();
  });
});

// ============================================================================
// generateSHACL integration tests
// ============================================================================

describe("generateSHACL() relation getter population", () => {
  it("should populate getter on relation with flagged target", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    // Find the flaggedItems property shape
    const flaggedProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "flaggedItems"
    );
    expect(flaggedProp).toBeDefined();
    expect(flaggedProp!.getter).toBeDefined();
    expect(flaggedProp!.getter).toContain("test://has_flagged");
    expect(flaggedProp!.getter).toContain("test://type");
    expect(flaggedProp!.getter).toContain("test://flagged_type");
    expect(flaggedProp!.conformanceConditions).toBeDefined();
    expect(flaggedProp!.conformanceConditions!.length).toBeGreaterThan(0);
  });

  it("should NOT populate getter when target has no conformance conditions", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    const minimalProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "minimalItems"
    );
    expect(minimalProp).toBeDefined();
    // MinimalTarget has only optional property — no flags/required — no getter
    expect(minimalProp!.getter).toBeUndefined();
    expect(minimalProp!.conformanceConditions).toBeUndefined();
  });

  it("should NOT populate getter when filter is false", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    const unfilteredProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "unfilteredItems"
    );
    expect(unfilteredProp).toBeDefined();
    // filter: false → no auto-generated getter even though target has flags
    expect(unfilteredProp!.getter).toBeUndefined();
    expect(unfilteredProp!.conformanceConditions).toBeUndefined();
  });

  it("should use explicit getter when provided (ignoring auto-generation)", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    // Getter-only relations have no predicate and are excluded from SHACL shapes
    const customProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "customItems"
    );
    expect(customProp).toBeUndefined();
  });

  it("should survive full round-trip: generateSHACL → toLinks → fromLinks", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();
    const links = shape.toLinks();
    const reconstructed = SHACLShape.fromLinks(
      links.map((l: any) => ({
        source: l.source,
        predicate: l.predicate,
        target: l.target,
      })),
      shape.nodeShapeUri
    );

    // Find the flaggedItems property in reconstructed shape
    const flaggedProp = reconstructed.properties.find(
      (p: SHACLPropertyShape) => p.name === "flaggedItems"
    );
    expect(flaggedProp).toBeDefined();
    expect(flaggedProp!.getter).toBeDefined();
    expect(flaggedProp!.getter).toContain("test://has_flagged");
    expect(flaggedProp!.conformanceConditions).toBeDefined();
    expect(flaggedProp!.conformanceConditions!.length).toBeGreaterThan(0);

    // The custom getter-only relation should NOT be in the SHACL shape
    const customProp = reconstructed.properties.find(
      (p: SHACLPropertyShape) => p.name === "customItems"
    );
    expect(customProp).toBeUndefined();
  });
});

// ============================================================================
// Ad4mModel API tests — getModelMetadata() relation fields
// ============================================================================

describe("Ad4mModel.getModelMetadata() relation filtering fields", () => {
  it("should expose target thunk on relations with target", () => {
    const metadata = ParentWithRelations.getModelMetadata();

    expect(metadata.relations.flaggedItems).toBeDefined();
    expect(metadata.relations.flaggedItems.target).toBeDefined();
    expect(metadata.relations.flaggedItems.target!()).toBe(FlaggedTarget);

    expect(metadata.relations.minimalItems.target).toBeDefined();
    expect(metadata.relations.minimalItems.target!()).toBe(MinimalTarget);
  });

  it("should expose filter:false on opt-out relations", () => {
    const metadata = ParentWithRelations.getModelMetadata();

    // Default (not set) — filter should be undefined (treated as true)
    expect(metadata.relations.flaggedItems.filter).toBeUndefined();

    // Explicit opt-out
    expect(metadata.relations.unfilteredItems.filter).toBe(false);
  });

  it("should expose explicit getter on relations with custom getter", () => {
    const metadata = ParentWithRelations.getModelMetadata();

    // Explicit getter provided via decorator (getter-only, no predicate)
    expect(metadata.relations.customItems.getter).toBe(
      "(<-link[WHERE predicate = 'test://custom'].in.uri)"
    );
    expect(metadata.relations.customItems.predicate).toBe("");

    // Auto-generated getter is NOT in decorator metadata (it's in the SHACL shape)
    expect(metadata.relations.flaggedItems.getter).toBeUndefined();
  });

  it("should produce matching getter between getModelMetadata target and SHACL shape", () => {
    // This verifies that the auto-generated getter in the SHACL shape
    // is consistent with what buildConformanceFilter produces from the
    // target model that getModelMetadata() exposes.
    const metadata = ParentWithRelations.getModelMetadata();
    const { shape } = (ParentWithRelations as any).generateSHACL();

    const flaggedRel = metadata.relations.flaggedItems;
    const TargetClass = flaggedRel.target!();

    // Build filter from the target class exposed by metadata
    const filter = buildConformanceFilter(flaggedRel.predicate, TargetClass);
    expect(filter).toBeDefined();

    // Should match what generateSHACL() stored on the property shape
    const shapeProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "flaggedItems"
    );
    expect(shapeProp!.getter).toBe(filter!.getter);
    expect(shapeProp!.conformanceConditions).toEqual(filter!.conformanceConditions);
  });

  it("should have correct direction for forward relations", () => {
    const metadata = ParentWithRelations.getModelMetadata();

    expect(metadata.relations.flaggedItems.direction).toBe("forward");
    expect(metadata.relations.minimalItems.direction).toBe("forward");
    expect(metadata.relations.unfilteredItems.direction).toBe("forward");
    expect(metadata.relations.customItems.direction).toBe("forward");
  });
});

// ============================================================================
// Validation tests
// ============================================================================

describe("Relation decorator validation", () => {
  it("should throw if both getter and target are provided", () => {
    expect(() => {
      @Model({ name: "InvalidGetterTarget" })
      class _Invalid extends Ad4mModel {
        @HasMany({
          getter: "(<-link.in.uri)",
          target: () => FlaggedTarget,
        })
        items: string[] = [];
      }
    }).toThrow(/getter.*target.*mutually exclusive/i);
  });

  it("should throw if both getter and through are provided", () => {
    expect(() => {
      @Model({ name: "InvalidGetterThrough" })
      class _Invalid extends Ad4mModel {
        @HasMany({
          getter: "(<-link.in.uri)",
          through: "test://pred",
        })
        items: string[] = [];
      }
    }).toThrow(/getter.*through.*mutually exclusive/i);
  });

  it("should allow getter-only relations (read-only)", () => {
    expect(() => {
      @Model({ name: "ValidGetterOnly" })
      class _Valid extends Ad4mModel {
        @HasMany({
          getter: "(<-link.in.uri)",
        })
        items: string[] = [];
      }
    }).not.toThrow();
  });
});

// ============================================================================
// Default through tests
// ============================================================================

describe("Default through predicate", () => {
  it("should default through to ad4m://has_child when omitted", () => {
    @Model({ name: "DefaultThrough" })
    class DefaultThrough extends Ad4mModel {
      @HasMany(() => FlaggedTarget)
      items: string[] = [];
    }

    const { shape } = (DefaultThrough as any).generateSHACL();
    const itemsProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "items"
    );
    expect(itemsProp).toBeDefined();
    expect(itemsProp!.path).toBe("ad4m://has_child");
  });

  it("should use explicit through when provided", () => {
    @Model({ name: "ExplicitThrough" })
    class ExplicitThrough extends Ad4mModel {
      @HasMany(() => FlaggedTarget, { through: "test://custom_pred" })
      items: string[] = [];
    }

    const { shape } = (ExplicitThrough as any).generateSHACL();
    const itemsProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "items"
    );
    expect(itemsProp).toBeDefined();
    expect(itemsProp!.path).toBe("test://custom_pred");
  });
});

// ============================================================================
// sh:class tests
// ============================================================================

describe("sh:class target shape reference", () => {
  it("should set sh:class on relation shapes when target is provided", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    const flaggedProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "flaggedItems"
    );
    expect(flaggedProp).toBeDefined();
    expect(flaggedProp!.class).toBeDefined();
    expect(flaggedProp!.class).toContain("FlaggedTarget");
    expect(flaggedProp!.class).toContain("Shape");
  });

  it("should set sh:class even when filter is false", () => {
    const { shape } = (ParentWithRelations as any).generateSHACL();

    const unfilteredProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "unfilteredItems"
    );
    expect(unfilteredProp).toBeDefined();
    // filter:false suppresses getter but NOT sh:class
    expect(unfilteredProp!.getter).toBeUndefined();
    expect(unfilteredProp!.class).toBeDefined();
    expect(unfilteredProp!.class).toContain("FlaggedTarget");
  });

  it("should NOT set sh:class when no target is provided", () => {
    @Model({ name: "NoTargetParent" })
    class NoTargetParent extends Ad4mModel {
      @HasMany({ through: "test://pred" })
      items: string[] = [];
    }

    const { shape } = (NoTargetParent as any).generateSHACL();
    const itemsProp = shape.properties.find(
      (p: SHACLPropertyShape) => p.name === "items"
    );
    expect(itemsProp).toBeDefined();
    expect(itemsProp!.class).toBeUndefined();
  });

  it("should round-trip sh:class through toLinks/fromLinks", () => {
    const shape = new SHACLShape("test://Parent");
    const prop: SHACLPropertyShape = {
      name: "items",
      path: "test://has_items",
      nodeKind: "IRI",
      class: "test://TargetShape",
    };
    shape.addProperty(prop);

    const links = shape.toLinks();
    const classLink = links.find(l => l.predicate === "sh://class");
    expect(classLink).toBeDefined();
    expect(classLink!.target).toBe("test://TargetShape");

    const reconstructed = SHACLShape.fromLinks(
      links.map((l: any) => ({ source: l.source, predicate: l.predicate, target: l.target })),
      shape.nodeShapeUri
    );
    expect(reconstructed.properties[0].class).toBe("test://TargetShape");
  });

  it("should round-trip sh:class through toJSON/fromJSON", () => {
    const shape = new SHACLShape("test://Parent");
    const prop: SHACLPropertyShape = {
      name: "items",
      path: "test://has_items",
      class: "test://TargetShape",
    };
    shape.addProperty(prop);

    const json = shape.toJSON();
    const reconstructed = SHACLShape.fromJSON(json);
    expect(reconstructed.properties[0].class).toBe("test://TargetShape");
  });
});
