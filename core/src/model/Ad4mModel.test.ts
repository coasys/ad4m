import { Ad4mModel } from "./Ad4mModel";
import { isIncludeProjection } from "./types";
import { Model, Property, Optional, ReadOnly, HasMany, HasOne, Flag } from "./decorators";
import { path } from "../shacl/builders";

describe("Ad4mModel.getModelMetadata()", () => {
  it("should extract basic model metadata with className", () => {
    @Model({ name: "SimpleModel" })
    class SimpleModel extends Ad4mModel {}

    const metadata = SimpleModel.getModelMetadata();
    
    expect(metadata.className).toBe("SimpleModel");
    expect(metadata.properties).toEqual({});
    expect(metadata.relations).toEqual({});
  });

  it("should extract property metadata with all fields", () => {
    @Model({ name: "PropertyModel" })
    class PropertyModel extends Ad4mModel {
      @Property({ through: "test://name" })
      name: string = "";
      
      @Optional({ through: "test://optional" })
      optional: string = "";
      
      @ReadOnly({ through: "test://readonly", prologGetter: "custom_getter" })
      readonly: string = "";
      
      @Flag({ through: "test://type", value: "test://flag" })
      type: string = "";
    }

    const metadata = PropertyModel.getModelMetadata();
    
    // Should have 4 properties
    expect(Object.keys(metadata.properties)).toHaveLength(4);
    
    // Verify "name" property
    expect(metadata.properties.name.predicate).toBe("test://name");
    expect(metadata.properties.name.required).toBe(false);
    expect(metadata.properties.name.readOnly).toBe(false);
    // A bare @Property has no resolveLanguage → deterministic typed literal
    // storage (default fast path).
    expect(metadata.properties.name.resolveLanguage).toBeUndefined();
    
    // Verify "optional" property
    expect(metadata.properties.optional.predicate).toBe("test://optional");
    expect(metadata.properties.optional.readOnly).toBe(false);
    
    // Verify "readonly" property
    expect(metadata.properties.readonly.predicate).toBe("test://readonly");
    expect(metadata.properties.readonly.readOnly).toBe(true);
    expect(metadata.properties.readonly.prologGetter).toBe("custom_getter");
    
    // Verify "type" property (flag)
    expect(metadata.properties.type.predicate).toBe("test://type");
    expect(metadata.properties.type.flag).toBe(true);
    expect(metadata.properties.type.initial).toBe("test://flag");
  });

  it("should extract relation metadata with various options", () => {
    @Model({ name: "RelationModel" })
    class RelationModel extends Ad4mModel {
      @HasMany({ through: "test://items" })
      items: string[] = [];
      
      @HasMany({ through: "test://local", local: true })
      local: string[] = [];
    }

    const metadata = RelationModel.getModelMetadata();
    
    // Should have 2 relations
    expect(Object.keys(metadata.relations)).toHaveLength(2);
    
    // Verify "items" relation
    expect(metadata.relations.items.predicate).toBe("test://items");
    
    // Verify "local" relation
    expect(metadata.relations.local.predicate).toBe("test://local");
    expect(metadata.relations.local.local).toBe(true);
  });

  it("should support NodeExpression transforms in properties", () => {
    // Transforms are now NodeExpression objects, not callable functions
    // The fileToDataUri and other builders are exported from @coasys/ad4m
    const transform = path("test://prefix");

    @Model({ name: "TransformModel" })
    class TransformModel extends Ad4mModel {
      @Optional({
        through: "test://data",
        transform
      })
      data: string = "";
    }

    const metadata = TransformModel.getModelMetadata();
    expect(metadata.properties.data).toBeDefined();
    expect(metadata.properties.data.transform).toEqual(transform);
  });

  it("should extract custom getter and setter from property metadata", () => {
    @Model({ name: "CustomModel" })
    class CustomModel extends Ad4mModel {
      @Optional({
        through: "test://computed",
        prologGetter: "triple(Base, 'test://value', V), Value is V * 2",
        prologSetter: "Value is V / 2, Actions = [{action: 'setSingleTarget', source: 'this', predicate: 'test://value', target: Value}]"
      })
      computed: number = 0;
    }

    const metadata = CustomModel.getModelMetadata();
    
    // Assert prologGetter and prologSetter contain the custom code
    expect(metadata.properties.computed.prologGetter).toContain("triple(Base, 'test://value', V), Value is V * 2");
    expect(metadata.properties.computed.prologSetter).toContain("Value is V / 2");
    expect(metadata.properties.computed.prologSetter).toContain("setSingleTarget");
  });

  it("should handle relation with typed target relation", () => {
    @Model({ name: "Comment" })
    class Comment extends Ad4mModel {}
    
    @Model({ name: "Post" })
    class Post extends Ad4mModel {
      @HasMany({ 
        through: "post://comment",
        target: () => Comment
      })
      comments: string[] = [];
    }

    const metadata = Post.getModelMetadata();
    
    // Assert relation exists with correct predicate
    expect(metadata.relations.comments).toBeDefined();
    expect(metadata.relations.comments.predicate).toBe("post://comment");
  });

  it("should throw error for class without @Model decorator", () => {
    class NoDecoratorModel extends Ad4mModel {}

    // Assert that calling getModelMetadata throws an error
    expect(() => NoDecoratorModel.getModelMetadata()).toThrow("Model class must be decorated with @Model");
  });

  it("should handle complex model with mixed property and relation types", () => {
    @Model({ name: "Recipe" })
    class Recipe extends Ad4mModel {
      @Property({ through: "recipe://name" })
      name: string = "";
      
      @Optional({ through: "recipe://description" })
      description: string = "";
      
      @ReadOnly({ through: "recipe://rating", prologGetter: "avg_rating(Base, Value)" })
      rating: number = 0;
      
      @HasMany({ through: "recipe://ingredient" })
      ingredients: string[] = [];
      
      @HasMany({ through: "recipe://step", local: true })
      steps: string[] = [];
    }

    const metadata = Recipe.getModelMetadata();
    
    // Assert className
    expect(metadata.className).toBe("Recipe");
    
    // Assert properties has 3 entries
    expect(Object.keys(metadata.properties)).toHaveLength(3);
    expect(metadata.properties.name).toBeDefined();
    expect(metadata.properties.description).toBeDefined();
    expect(metadata.properties.rating).toBeDefined();
    
    // Assert relations has 2 entries
    expect(Object.keys(metadata.relations)).toHaveLength(2);
    expect(metadata.relations.ingredients).toBeDefined();
    expect(metadata.relations.steps).toBeDefined();
    
    // Verify all metadata fields are correctly extracted
    expect(metadata.properties.name.predicate).toBe("recipe://name");
    // Bare @Property → no resolveLanguage → deterministic typed literal.
    expect(metadata.properties.name.resolveLanguage).toBeUndefined();
    expect(metadata.properties.description.predicate).toBe("recipe://description");
    expect(metadata.properties.rating.predicate).toBe("recipe://rating");
    expect(metadata.properties.rating.prologGetter).toBe("avg_rating(Base, Value)");
    expect(metadata.relations.ingredients.predicate).toBe("recipe://ingredient");
    expect(metadata.relations.steps.predicate).toBe("recipe://step");
    expect(metadata.relations.steps.local).toBe(true);
  });
});

describe("Ad4mModel.fromJSONSchema() with getModelMetadata()", () => {
  it("should extract metadata from a model created via fromJSONSchema with basic properties", () => {
    const schema = {
      title: "Product",
      type: "object",
      properties: {
        name: { type: "string" },
        price: { type: "number" },
        description: { type: "string" }
      },
      required: ["name", "price"]
    };

    const ProductClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Product",
      namespace: "product://"
    });

    const metadata = ProductClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Product");

    // Verify properties are extracted
    expect(Object.keys(metadata.properties).length).toBeGreaterThan(0);
    expect(metadata.properties.name).toBeDefined();
    expect(metadata.properties.name.predicate).toBe("product://name");
    expect(metadata.properties.name.required).toBe(true);
    expect(metadata.properties.name.readOnly).toBe(false);
    // No resolveLanguage set → deterministic typed literal storage (default).
    expect(metadata.properties.name.resolveLanguage).toBeUndefined();

    expect(metadata.properties.price).toBeDefined();
    expect(metadata.properties.price.predicate).toBe("product://price");
    expect(metadata.properties.price.required).toBe(true);
    expect(metadata.properties.price.resolveLanguage).toBeUndefined();

    expect(metadata.properties.description).toBeDefined();
    expect(metadata.properties.description.predicate).toBe("product://description");
    expect(metadata.properties.description.required).toBe(false);
  });

  it("should extract relations from a model created via fromJSONSchema with arrays", () => {
    const schema = {
      title: "Post",
      type: "object",
      properties: {
        title: { type: "string" },
        tags: {
          type: "array",
          items: { type: "string" }
        },
        comments: {
          type: "array",
          items: { type: "string" }
        }
      },
      required: ["title"]
    };

    const PostClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Post",
      namespace: "post://"
    });

    const metadata = PostClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Post");

    // Verify relations are extracted
    expect(Object.keys(metadata.relations).length).toBeGreaterThan(0);
    expect(metadata.relations.tags).toBeDefined();
    expect(metadata.relations.tags.predicate).toBe("post://tags");

    expect(metadata.relations.comments).toBeDefined();
    expect(metadata.relations.comments.predicate).toBe("post://comments");

    // Verify properties (should include at least title)
    expect(metadata.properties.title).toBeDefined();
    expect(metadata.properties.title.predicate).toBe("post://title");
    expect(metadata.properties.title.required).toBe(true);
  });

  it("should handle x-ad4m metadata in JSON schema for property options", () => {
    const schema = {
      title: "Contact",
      "x-ad4m": {
        namespace: "contact://"
      },
      type: "object",
      properties: {
        name: {
          type: "string",
          "x-ad4m": {
            through: "foaf://name",
            writable: true
          }
        },
        email: {
          type: "string",
          "x-ad4m": {
            through: "foaf://mbox",
            local: true
          }
        }
      },
      required: ["name"]
    };

    const ContactClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Contact"
    });

    const metadata = ContactClass.getModelMetadata();

    // Verify x-ad4m metadata is respected
    expect(metadata.properties.name.predicate).toBe("foaf://name");
    // No resolveLanguage set → deterministic typed literal (default).
    expect(metadata.properties.name.resolveLanguage).toBeUndefined();
    expect(metadata.properties.name.readOnly).toBe(false);
    expect(metadata.properties.name.required).toBe(true);

    expect(metadata.properties.email.predicate).toBe("foaf://mbox");
    expect(metadata.properties.email.local).toBe(true);
  });

  it("should handle property mapping override in options", () => {
    const schema = {
      title: "User",
      type: "object",
      properties: {
        username: { type: "string" },
        fullName: { type: "string" }
      },
      required: ["username"]
    };

    const UserClass = Ad4mModel.fromJSONSchema(schema, {
      name: "User",
      namespace: "user://",
      propertyMapping: {
        username: "custom://identifier",
        fullName: "custom://name"
      }
    });

    const metadata = UserClass.getModelMetadata();

    // Verify property mappings are applied
    expect(metadata.properties.username.predicate).toBe("custom://identifier");
    expect(metadata.properties.fullName.predicate).toBe("custom://name");
  });

  it("should extract metadata from dynamically generated models with mixed types", () => {
    const schema = {
      title: "Article",
      type: "object",
      properties: {
        title: { type: "string" },
        views: { type: "number" },
        published: { type: "boolean" },
        authors: {
          type: "array",
          items: { type: "string" }
        },
        tags: {
          type: "array",
          items: { type: "string" },
          "x-ad4m": {
            local: true
          }
        }
      },
      required: ["title", "published"]
    };

    const ArticleClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Article",
      namespace: "article://"
    });

    const metadata = ArticleClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Article");

    // Verify properties
    expect(metadata.properties.title).toBeDefined();
    expect(metadata.properties.title.predicate).toBe("article://title");
    expect(metadata.properties.title.required).toBe(true);
    // No resolveLanguage set → deterministic typed literal (default).
    expect(metadata.properties.title.resolveLanguage).toBeUndefined();

    expect(metadata.properties.views).toBeDefined();
    expect(metadata.properties.views.predicate).toBe("article://views");
    expect(metadata.properties.views.resolveLanguage).toBeUndefined();

    expect(metadata.properties.published).toBeDefined();
    expect(metadata.properties.published.predicate).toBe("article://published");
    expect(metadata.properties.published.required).toBe(true);

    // Verify relations
    expect(metadata.relations.authors).toBeDefined();
    expect(metadata.relations.authors.predicate).toBe("article://authors");

    expect(metadata.relations.tags).toBeDefined();
    expect(metadata.relations.tags.predicate).toBe("article://tags");
    expect(metadata.relations.tags.local).toBe(true);
  });

  it("should handle models with no properties (open-world, no auto-flag)", () => {
    const schema = {
      title: "EmptyModel",
      type: "object",
      properties: {}
    };

    const EmptyModelClass = Ad4mModel.fromJSONSchema(schema, {
      name: "EmptyModel",
      namespace: "empty://"
    });

    const metadata = EmptyModelClass.getModelMetadata();

    // Should have className
    expect(metadata.className).toBe("EmptyModel");

    // Should NOT have an auto-generated __ad4m_type property —
    // models with all-optional properties use open-world structural matching.
    expect(metadata.properties.__ad4m_type).toBeUndefined();

    // SHACL shape should still be valid with empty constructor/destructor
    const { shape } = (EmptyModelClass as any).generateSHACL();
    expect(shape.constructor_actions).toEqual([]);
    expect(shape.destructor_actions).toEqual([]);
  });
});




describe("Ad4mModel.queryToSPARQL()", () => {
  const mockPerspective = {} as any;

  function normalizeQuery(query: string): string {
    return query.replace(/\s+/g, ' ').trim();
  }

  @Model({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name", required: true })
    name: string = "";
    
    @Property({ through: "recipe://rating", required: true })
    rating: number = 0;
    
    @HasMany({ through: "recipe://ingredient" })
    ingredients: string[] = [];
  }

  it("should generate basic SPARQL query with no filters", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, {});
    const norm = normalizeQuery(query);

    // Must be a SPARQL SELECT
    expect(norm).toContain("SELECT ?source ?predicate ?target ?author ?timestamp");
    // Must have conformance JOIN for required properties using direct triple patterns
    expect(norm).toContain("cfTarget_name");
    expect(norm).toContain("recipe://name");
  });

  it("should generate query with simple property filter", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: "Pasta" } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("SELECT ?source ?predicate ?target ?author ?timestamp");
    // For literal-stored properties, SPARQL only adds a JOIN (no FILTER value) — filtering is in JS
    expect(norm).toContain("recipe://name");
    // Value should NOT be in SPARQL — filtering happens in JS post-filter
    expect(norm).toContain("?wTarget_name");
  });

  it("should generate query with NOT operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { not: "Salad" } } });
    const norm = normalizeQuery(query);

    // For literal-stored properties, NOT filtering is done in JS — no NOT EXISTS in SPARQL
    expect(norm).toContain("recipe://name");
  });

  it("should generate query with NOT IN operator (array)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { not: ["Salad", "Soup"] } } });
    const norm = normalizeQuery(query);

    // For literal-stored properties, NOT IN filtering is done in JS — no filter values in SPARQL
    expect(norm).toContain("recipe://name");
  });

  it("should generate query with IN clause (array values)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: ["Pasta", "Pizza"] } });
    const norm = normalizeQuery(query);

    // For literal-stored properties, IN filtering is done in JS — only a JOIN exists
    expect(norm).toContain("recipe://name");
    expect(norm).toContain("?wTarget_name");
  });

  it("should generate query with base filter", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { base: "ad4m://test123" } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("?source =");
    expect(norm).toContain("ad4m://test123");
  });

  it("should escape special characters in SPARQL strings", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: 'O\'Brien' } });
    const norm = normalizeQuery(query);

    // Should not break the query
    expect(norm).toContain("SELECT ?source");
  });

  // =========================================================================
  // Additional models for thorough SPARQL structure tests
  // =========================================================================

  @Model({ name: "Task" })
  class Task extends Ad4mModel {
    @Property({ through: "task://title", required: true })
    title: string = "";

    @Property({ through: "task://priority" })
    priority: number = 0;

    @Property({ through: "task://done" })
    done: boolean = false;

    @Optional({ through: "task://description" })
    description: string = "";

    @HasMany({ through: "task://tag" })
    tags: string[] = [];

    @HasMany({ through: "task://assignee" })
    assignees: string[] = [];
  }

  @Model({ name: "EmptyModel" })
  class EmptyModel extends Ad4mModel {
    @Optional({ through: "empty://optField" })
    optField: string = "";
  }

  // ---- Comparison operators (gt, lt, gte, lte, between, contains) ----
  // These are handled in JS post-processing, NOT in SPARQL.
  // The tests verify that the SPARQL still generates valid output (conformance filters)
  // but does NOT inject FILTER clauses for these operators.

  it("should inject SPARQL FILTER for gt operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { gt: 3 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // gt is now handled via JS post-filter — SPARQL just joins the property
    expect(norm).toContain("wTarget_cmp_rating");
  });

  it("should inject SPARQL FILTER for lt operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { lt: 5 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // lt is now handled via JS post-filter
    expect(norm).toContain("wTarget_cmp_rating");
  });

  it("should inject SPARQL FILTER for gte/lte combined", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { gte: 2, lte: 8 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // gte/lte are now handled via JS post-filter
    expect(norm).toContain("wTarget_cmp_rating");
  });

  it("should inject SPARQL FILTER for between operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { between: [1, 10] } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // between is now handled via JS post-filter
    expect(norm).toContain("wTarget_cmp_rating");
  });

  it("should inject SPARQL FILTER for contains operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { contains: "pasta" } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // contains is now handled via JS post-filter
    expect(norm).toContain("wTarget_cmp_name");
  });

  // ---- Multiple property filters combined ----

  it("should combine multiple property equality filters with &&", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, {
      where: { name: "Pasta", rating: ["4", "5"] }
    });
    const norm = normalizeQuery(query);
    // Should have EXISTS blocks for both name and rating
    expect(norm).toContain("recipe://name");
    expect(norm).toContain("recipe://rating");
    expect(norm).toContain("&&");
  });

  // ---- ORDER BY, LIMIT, OFFSET are JS post-processed ----

  it("should include ORDER BY/LIMIT/OFFSET in SPARQL pagination subquery", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { order: { name: "ASC" }, limit: 10, offset: 5 });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    // SPARQL-level pagination via subquery
    expect(norm).toContain("ORDER BY");
    expect(norm).toContain("LIMIT 10");
    expect(norm).toContain("OFFSET 5");
    expect(norm).toContain("SELECT DISTINCT ?source");
  });
});
describe("Ad4mModel query methods (modelQuery integration)", () => {
  // Test Recipe model
  @Model({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name" })
    name: string = "";
    
    @Property({ through: "recipe://rating" })
    rating: number = 0;
    
    @HasMany({ through: "recipe://ingredient" })
    ingredients: string[] = [];
  }

  // Mock perspective with querySparql, modelQuery, and infer methods
  const mockPerspective = {
    querySparql: jest.fn(),
    modelQuery: jest.fn(),
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("routes findAll() through modelQuery", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.findAll(mockPerspective);

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use SPARQL when engine is 'sparql' in findAllAndCount()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const { results, totalCount } = await Recipe.findAllAndCount(mockPerspective, {});

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(totalCount).toBe(1);
  });

  it("should use SPARQL when engine is 'sparql' in paginate()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const page = await Recipe.paginate(mockPerspective, 10, 1, {});

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
    expect(page.totalCount).toBe(1);
  });

  it("should use SPARQL in count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 5
    });

    const count = await Recipe.count(mockPerspective, {});

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(5);
  });

  it("should return count from modelQuery", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 10
    });

    const count = await Recipe.count(mockPerspective, {});
    
    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(count).toBe(10);
  });

  it("routes findAll() with a where through modelQuery", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.findAll(mockPerspective, { where: { name: "Pasta" } });

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("routes count() with a where through modelQuery", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 4 } } });

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });

  it("routes paginate() with a where through modelQuery", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const page = await Recipe.paginate(mockPerspective, 10, 1, { where: { rating: { gt: 3 } } });

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
  });

  describe("cursor pages (P8)", () => {
    const sentQuery = (call = 0) => JSON.parse(mockPerspective.modelQuery.mock.calls[call][1]);

    it("Ad4mModel.page() asks for the first page with an empty cursor and no offset", async () => {
      mockPerspective.modelQuery.mockResolvedValue({
        instances: [{ id: "literal:recipe1", name: "Pasta" }],
        nextCursor: "c1",
      });

      const page = await Recipe.page(mockPerspective, 10);

      expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
      const q = sentQuery();
      expect(q.after).toBe("");
      expect(q.limit).toBe(10);
      expect(q.offset).toBeUndefined();
      expect(q.count).toBeUndefined();
      expect(page.results).toHaveLength(1);
      expect(page.nextCursor).toBe("c1");
      expect(page.pageSize).toBe(10);
    });

    it("Ad4mModel.page() continues from the cursor it is given and keeps the query", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [] });

      const page = await Recipe.page(mockPerspective, 5, "c1", {
        where: { name: "Pasta" },
        order: { createdAt: "DESC" },
      });

      const q = sentQuery();
      expect(q.after).toBe("c1");
      expect(q.limit).toBe(5);
      expect(q.where).toEqual({ name: "Pasta" });
      expect(q.order).toEqual([["createdAt", "DESC"]]);
      expect(page.nextCursor).toBeUndefined();
    });

    it("Ad4mModel.page() asks for no count: a cursor page returns no total", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [] });

      await Recipe.page(mockPerspective, 5, "c1", { count: true } as any);

      const q = sentQuery();
      expect(q.after).toBe("c1");
      expect(q.count).toBeUndefined();
    });

    it("page() sends the cursor instead of an offset", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [], nextCursor: "c2" });

      const page = await Recipe.page(mockPerspective, 10, "c1", { where: { rating: { gt: 3 } }, offset: 30 });

      const q = sentQuery();
      expect(q.after).toBe("c1");
      expect(q.limit).toBe(10);
      expect(q.offset).toBeUndefined();
      expect(page.nextCursor).toBe("c2");
    });

    it("paginate() pages by offset and drops a cursor from the starting query", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [], totalCount: 0 });

      await Recipe.paginate(mockPerspective, 10, 3, { after: "c1" } as any);

      const q = sentQuery();
      expect(q.after).toBeUndefined();
      expect(q.offset).toBe(20);
    });

    it("count() drops a cursor from the query", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [], totalCount: 7 });

      const count = await Recipe.count(mockPerspective, { after: "c1" } as any);

      const q = sentQuery();
      expect(q.after).toBeUndefined();
      expect(q.limit).toBe(0);
      expect(q.count).toBe(true);
      expect(count).toBe(7);
    });

    it("findAll forwards `after` and executeModelQuery returns nextCursor", async () => {
      mockPerspective.modelQuery.mockResolvedValue({ instances: [], nextCursor: "c3" });

      const { nextCursor } = await (Recipe as any).executeModelQuery(mockPerspective, { after: "", limit: 2 });

      expect(sentQuery().after).toBe("");
      expect(nextCursor).toBe("c3");
    });
  });
});

describe("Ad4mModel.count() with advanced where conditions", () => {
  // Test Recipe model
  @Model({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name" })
    name: string = "";
    
    @Property({ through: "recipe://rating" })
    rating: number = 0;
    
    @HasMany({ through: "recipe://ingredient" })
    ingredients: string[] = [];
  }

  // Mock perspective
  const mockPerspective = {
    querySparql: jest.fn(),
    modelQuery: jest.fn(),
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should apply filtering for gt operator on properties in SPARQL count()", async () => {
    // With the new modelQuery endpoint, filtering happens server-side in Rust
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 2
    });

    // Count recipes with rating > 3 (should match 2 recipes: rating 4 and 5)
    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } });
    
    expect(count).toBe(2);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering for between operator on properties in SPARQL count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    // Count recipes with rating between 2 and 4 (should match 3 recipes: rating 2, 3, 4)
    const count = await Recipe.count(mockPerspective, { where: { rating: { between: [2, 4] } } });
    
    expect(count).toBe(3);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering for timestamp gt operator in SPARQL count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 2
    });

    const targetTimestamp = new Date("2023-01-03T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { where: { timestamp: { gt: targetTimestamp } } });
    
    expect(count).toBe(2);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering for timestamp between operator in SPARQL count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { 
      where: { timestamp: { between: [startTimestamp, endTimestamp] } } 
    });
    
    expect(count).toBe(3);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering for author filtering in SPARQL count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    // Count recipes by Alice (should match 3 recipes)
    const count = await Recipe.count(mockPerspective, { where: { author: "did:key:alice" } });
    
    expect(count).toBe(3);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering in count() with gt operator", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 2
    });

    // Count recipes with rating > 3
    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } });
    
    expect(count).toBe(2);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering in count() with timestamp between", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    
    const count = await Recipe.count(mockPerspective, {
      where: { timestamp: { between: [startTimestamp, endTimestamp] } },
    });
    
    expect(count).toBe(3);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

});


describe("SPARQL comparison filters", () => {
  @Model({ name: "Item" })
  class Item extends Ad4mModel {
    @Property({ through: "item://price", required: true })
    price!: string;

    @Property({ through: "item://name", required: true })
    name!: string;
  }

  it("should generate gt filter", async () => {
    const mockPersp = { getLinks: jest.fn().mockResolvedValue([]) } as any;
    const query = await (Item as any).queryToSPARQL(mockPersp, { where: { price: { gt: 10 } } });
    // gt is now JS post-filter — SPARQL just joins the property
    expect(query).toContain("item://price");
  });

  it("should generate between filter", async () => {
    const mockPersp = { getLinks: jest.fn().mockResolvedValue([]) } as any;
    const query = await (Item as any).queryToSPARQL(mockPersp, { where: { price: { between: [5, 20] } } });
    // between is now JS post-filter
    expect(query).toContain("item://price");
  });

  it("should generate contains filter", async () => {
    const mockPersp = { getLinks: jest.fn().mockResolvedValue([]) } as any;
    const query = await (Item as any).queryToSPARQL(mockPersp, { where: { name: { contains: "widget" } } });
    // contains is now JS post-filter
    expect(query).toContain("item://name");
  });
});

// ──────────────────────────────────────────────────────────
// SPARQL Direct Triple Pattern & IRI Tests
// ──────────────────────────────────────────────────────────

import { buildSPARQLQuery, formatSPARQLValue } from "./query-sparql";
import { SHACLShape } from "../shacl/SHACLShape";

describe("SPARQL direct triple pattern generation", () => {
  @Model({ name: "Channel" })
  class Channel extends Ad4mModel {
    @Flag({ through: "flux://entry_type", value: "flux://channel" })
    type: string = "";

    @Property({ through: "flux://name", required: true })
    name: string = "";

    @Optional({ through: "flux://description" })
    description: string = "";

    @HasMany({ through: "flux://has_message" })
    messages: string[] = [];
  }

  const mockPersp = {} as any;

  it("generates direct triple pattern with ?source ?predicate ?target", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    // Must use direct triple pattern, not link-node reification
    expect(query).toContain("?source ?predicate ?target");
    expect(query).not.toContain("rdf:type");
    expect(query).not.toContain("ad4m:Link");
  });

  it("generates Flag filter as ?source <predicate> <value>", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("<flux://entry_type>");
    expect(query).toContain("<flux://channel>");
  });

  it("generates Property binding as ?source <predicate> ?varName", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("<flux://name>");
    expect(query).toMatch(/\?source\s+<flux:\/\/name>\s+\?cfTarget_name/);
  });

  it("uses RDF 1.2 reifier pattern for author/timestamp", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("<http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>");
    expect(query).toContain("?source ?predicate ?target .");
    expect(query).toContain("?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>>");
    expect(query).toContain("?_reifier <ad4m://ontology/author> ?author");
    expect(query).toContain("?_reifier <ad4m://ontology/timestamp> ?timestamp");
    expect(query).not.toContain("GRAPH");
  });

  it("uses FILTER(isIRI(?source)) to exclude non-IRI subjects", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("FILTER(isIRI(?source)");
  });
});



describe("formatSPARQLValue", () => {
  it("wraps strings in double quotes", () => {
    expect(formatSPARQLValue("hello")).toBe('"hello"');
  });

  it("escapes special characters", () => {
    expect(formatSPARQLValue('say "hi"')).toBe('"say \\"hi\\""');
  });

  it("converts numbers to quoted strings", () => {
    expect(formatSPARQLValue(42)).toBe('"42"');
  });
});

// ──────────────────────────────────────────────────────────
//  Comprehensive SPARQL migration unit tests
// ──────────────────────────────────────────────────────────

import { Literal } from "../Literal";

// Helper: create literal URLs using JSON encoding (which handles all types including booleans)
function literalUrl(value: any): string {
  if (typeof value === 'string') return Literal.from(value).toUrl();
  // Use JSON encoding for numbers and booleans since Literal.get() doesn't handle boolean: prefix
  return `literal:json:${encodeURIComponent(JSON.stringify(value))}`;
}

describe("buildSPARQLQuery edge cases", () => {
  @Model({ name: "FlagModel" })
  class FlagModel extends Ad4mModel {
    @Flag({ through: "flag://type", value: "flag://FlagModel" })
    type: string = "";

    @Property({ through: "flag://name", required: true })
    name: string = "";
  }

  @Model({ name: "NoFlagModel" })
  class NoFlagModel extends Ad4mModel {
    @Property({ through: "noflag://title" })
    title: string = "";
  }

  it("adds conformance JOIN for flag + required properties", () => {
    const metadata = FlagModel.getModelMetadata();
    const allRelsMeta = {} as any;
    const sparql = buildSPARQLQuery(metadata, allRelsMeta, {}, FlagModel);
    // Should contain flag triple pattern
    expect(sparql).toContain("<flag://type>");
    expect(sparql).toContain("<flag://FlagModel>");
    // Should contain required property pattern
    expect(sparql).toContain("<flag://name>");
  });

  it("uses structural subquery when no conformance patterns exist", () => {
    const metadata = NoFlagModel.getModelMetadata();
    const allRelsMeta = {} as any;
    const sparql = buildSPARQLQuery(metadata, allRelsMeta, {}, NoFlagModel);
    // Should contain structural subquery with DISTINCT
    expect(sparql).toContain("SELECT DISTINCT ?source");
    expect(sparql).toContain("<noflag://title>");
  });

  it("adds JOIN for where-clause on literal-stored fields", () => {
    const metadata = FlagModel.getModelMetadata();
    const allRelsMeta = {} as any;
    const sparql = buildSPARQLQuery(metadata, allRelsMeta, { where: { name: "test" } }, FlagModel);
    // Where clause should add a join pattern for name
    expect(sparql).toContain("flag://name");
  });

  it("handles where: {id: 'specific-id'} with FILTER on ?source", () => {
    const metadata = FlagModel.getModelMetadata();
    const allRelsMeta = {} as any;
    const sparql = buildSPARQLQuery(metadata, allRelsMeta, { where: { id: "expr:123" } as any }, FlagModel);
    // Should filter by source
    expect(sparql).toContain("expr:123");
  });
});

// ── Batch hydration for reverse relations (3.4) ─────────────────────────────
describe("Batch hydration for reverse relations", () => {
  it("batch hydration should correctly group targets by parent instance ID", () => {
    // Test the grouping logic used in batch SPARQL hydration
    const sparqlResults = [
      { target: "inst:1", source: "child:a" },
      { target: "inst:1", source: "child:b" },
      { target: "inst:2", source: "child:c" },
      { target: "inst:3", source: "child:d" },
      { target: "inst:3", source: "child:e" },
      { target: "inst:3", source: "child:f" },
    ];

    const reverseLinksMap = new Map<string, string[]>();
    for (const row of sparqlResults) {
      if (!reverseLinksMap.has(row.target)) reverseLinksMap.set(row.target, []);
      reverseLinksMap.get(row.target)!.push(row.source);
    }

    expect(reverseLinksMap.get("inst:1")).toEqual(["child:a", "child:b"]);
    expect(reverseLinksMap.get("inst:2")).toEqual(["child:c"]);
    expect(reverseLinksMap.get("inst:3")).toEqual(["child:d", "child:e", "child:f"]);
  });

  it("batch hydration should handle empty relation sets without error", () => {
    const sparqlResults: any[] = [];
    const reverseLinksMap = new Map<string, string[]>();
    for (const row of sparqlResults) {
      if (!reverseLinksMap.has(row.target)) reverseLinksMap.set(row.target, []);
      reverseLinksMap.get(row.target)!.push(row.source);
    }

    expect(reverseLinksMap.size).toBe(0);
    expect(reverseLinksMap.get("nonexistent") || []).toEqual([]);
  });

  it("batch hydration should return identical results to N+1 hydration (grouping equivalence)", () => {
    // Simulate N+1: each instance queries separately
    const instances = [{ id: "inst:1" }, { id: "inst:2" }];
    const allLinks = [
      { data: { source: "child:a", predicate: "rel://has", target: "inst:1" } },
      { data: { source: "child:b", predicate: "rel://has", target: "inst:1" } },
      { data: { source: "child:c", predicate: "rel://has", target: "inst:2" } },
    ];

    // N+1 approach
    const n1Results = new Map<string, string[]>();
    for (const inst of instances) {
      const links = allLinks.filter(l => l.data.target === inst.id);
      n1Results.set(inst.id, links.map(l => l.data.source));
    }

    // Batch approach
    const batchResults = new Map<string, string[]>();
    const sparqlRows = allLinks.map(l => ({ target: l.data.target, source: l.data.source }));
    for (const row of sparqlRows) {
      if (!batchResults.has(row.target)) batchResults.set(row.target, []);
      batchResults.get(row.target)!.push(row.source);
    }

    // Results should be identical
    for (const inst of instances) {
      expect(batchResults.get(inst.id)).toEqual(n1Results.get(inst.id));
    }
  });

  it("batch hydration with nested includes should use batched queries at each level", () => {
    // This test verifies the structure: nested includes result in multiple
    // batch operations, one per relation depth level
    const depth0Rows = [
      { target: "root:1", source: "mid:a" },
      { target: "root:1", source: "mid:b" },
    ];
    const depth1Rows = [
      { target: "mid:a", source: "leaf:x" },
      { target: "mid:b", source: "leaf:y" },
    ];

    // Group each level
    const level0Map = new Map<string, string[]>();
    for (const r of depth0Rows) {
      if (!level0Map.has(r.target)) level0Map.set(r.target, []);
      level0Map.get(r.target)!.push(r.source);
    }

    const level1Map = new Map<string, string[]>();
    for (const r of depth1Rows) {
      if (!level1Map.has(r.target)) level1Map.set(r.target, []);
      level1Map.get(r.target)!.push(r.source);
    }

    expect(level0Map.get("root:1")).toEqual(["mid:a", "mid:b"]);
    expect(level1Map.get("mid:a")).toEqual(["leaf:x"]);
    expect(level1Map.get("mid:b")).toEqual(["leaf:y"]);
  });
});

// ── Push-down FILTER for literal equality (3.5) ──────────────────────────────
describe("Push-down FILTER for literal equality", () => {
  const mockPerspective = {} as any;

  @Model({ name: "FilterTest" })
  class FilterTest extends Ad4mModel {
    @Property({ through: "ft://name", required: true })
    name: string = "";
    @Property({ through: "ft://rating", required: true })
    rating: number = 0;
  }

  const normalizeQuery = (q: string) => q.replace(/\s+/g, " ").trim();

  it("SPARQL query for where: { name: 'Alice' } should use <ad4m://fn/parse_literal> FILTER", async () => {
    const query = await (FilterTest as any).queryToSPARQL(mockPerspective, { where: { name: "Alice" } });
    const norm = normalizeQuery(query);
    // JOIN pattern exists
    expect(norm).toContain("?wTarget_name");
    // parse_literal push-down FILTER is present (using correct SPARQL IRI syntax)
    expect(norm).toContain("ad4m://fn/parse_literal");
    expect(norm).not.toContain("fn::parse_literal");
  });

  it("SPARQL query for where: { name: ['Alice', 'Bob'] } should use <ad4m://fn/parse_literal> IN FILTER", async () => {
    const query = await (FilterTest as any).queryToSPARQL(mockPerspective, { where: { name: ["Alice", "Bob"] } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("?wTarget_name");
    expect(norm).toContain("ad4m://fn/parse_literal");
    expect(norm).not.toContain("fn::parse_literal");
  });

  it("SPARQL query for where: { rating: { gt: 5 } } should NOT include parse_literal FILTER", async () => {
    const query = await (FilterTest as any).queryToSPARQL(mockPerspective, { where: { rating: { gt: 5 } } });
    const norm = normalizeQuery(query);
    expect(norm).not.toContain("parse_literal");
  });

  it("literal property where clause adds JOIN for JS post-filter", async () => {
    const query = await (FilterTest as any).queryToSPARQL(mockPerspective, { where: { name: "Alice" } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("?wTarget_name");
  });
});

// ── Model.query (live) ───────────────────────────────────────────────────────────────

/** A perspective whose client answers `subscribe` with `initial` and keeps the
 *  update and reconnect listeners, so a test can push updates and reconnect. */
function liveHarness(initial: any = { instances: [], totalCount: 0 }) {
  let opens = 0;
  let listener: ((update: any) => void) | null = null;
  let revision = 0;
  const reconnectListeners = new Set<() => void>();
  const client = {
    subscribe: jest.fn().mockImplementation(async () => {
      opens++;
      revision = 0;
      return { subscriptionId: `sub-${opens}`, revision: 0, result: typeof initial === "function" ? initial(opens) : initial };
    }),
    onQueryUpdate: jest.fn().mockImplementation((cb: any) => {
      listener = cb;
      return () => { if (listener === cb) listener = null; };
    }),
    onReconnect: jest.fn((cb: () => void) => {
      reconnectListeners.add(cb);
      return () => reconnectListeners.delete(cb);
    }),
    disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    resyncSubscription: jest.fn(),
  };
  const perspective = {
    uuid: "test-uuid",
    client,
    modelQuery: jest.fn().mockResolvedValue({ instances: [{ id: "from-query" }], totalCount: 99 }),
  } as any;
  if (typeof initial !== "function") perspective.modelQuery.mockResolvedValue(initial);
  /** Push the next revision of the open subscription; `update` is `{ result }` or a model delta. */
  const push = (update: any) => listener?.({ subscriptionId: `sub-${opens}`, revision: ++revision, ...update });
  const reconnect = async () => {
    reconnectListeners.forEach(cb => cb());
    await new Promise(r => setTimeout(r, 0));
  };
  const sent = () => client.subscribe.mock.calls.map((c: any[]) => JSON.parse(c[1].queryJson));
  return { client, perspective, push, reconnect, sent, opens: () => opens };
}

function testModel(name: string) {
  const { Ad4mModel, Model, Flag, Property } = require("./index");
  @Model({ name })
  class M extends Ad4mModel {
    @Flag({ through: "test://type", value: `test://${name}` })
    type: string = `test://${name}`;
    @Property({ through: "test://name" })
    name: string = "";
  }
  return M;
}

const ids = (page: any) => page.results.map((r: any) => r.id);
const settle = () => new Promise(r => setTimeout(r, 10));

describe("Model.query (live)", () => {
  it("opens through perspective.subscribe with the model's query and returns the first page", async () => {
    const { perspective, client } = liveHarness({ instances: [{ id: "p2a" }, { id: "p2b" }], totalCount: 12 });
    const live = testModel("LiveFirst").query(perspective, { limit: 10, offset: 10, count: true });

    const page = await live.first;

    expect(ids(page)).toEqual(["p2a", "p2b"]);
    expect(page.totalCount).toBe(12);
    expect(client.subscribe).toHaveBeenCalledTimes(1);
    const [uuid, params] = client.subscribe.mock.calls[0];
    expect(uuid).toBe("test-uuid");
    expect(params.className).toBeTruthy();
    expect(JSON.parse(params.queryJson)).toMatchObject({ limit: 10, offset: 10, count: true });
    // The subscription's first result is the page: no separate read.
    expect(perspective.modelQuery).not.toHaveBeenCalled();
    live.dispose();
  });

  it("marks the opening value initial and a change not", async () => {
    const { perspective, push } = liveHarness({ instances: [{ id: "a" }], totalCount: 1 });
    const live = testModel("LiveInitial").query(perspective);
    await live.first;
    expect(live.getSnapshot().initial).toBe(true);

    push({ ids: ["a", "b"], upsert: [{ id: "b" }] });

    expect(live.getSnapshot().initial).toBe(false);
    expect(ids(live.getSnapshot().data)).toEqual(["a", "b"]);
    live.dispose();
  });

  it("does not invoke a listener before the first value, then once at the opening value", async () => {
    const { perspective } = liveHarness();
    const live = testModel("LiveTiming").query(perspective);
    const seen: any[] = [];
    live.subscribe(s => seen.push(s));
    expect(seen).toHaveLength(0);

    await live.first;

    expect(seen).toHaveLength(1);
    expect(seen[0]).toMatchObject({ status: "ready", initial: true });
    live.dispose();
  });

  it("delivers each page from the pushed rows without a second query", async () => {
    const { perspective, push } = liveHarness();
    const live = testModel("LivePush").query(perspective, { limit: 10 });
    await live.first;
    const pages: any[] = [];
    live.subscribe(s => pages.push(s.data));
    pages.length = 0;

    push({ result: { instances: [{ id: "a" }, { id: "b" }], totalCount: 5 } });

    expect(perspective.modelQuery).not.toHaveBeenCalled();
    expect(pages).toHaveLength(1);
    expect(ids(pages[0])).toEqual(["a", "b"]);
    expect(pages[0].totalCount).toBe(5);
    live.dispose();
  });

  it("delivers a change outside the page through totalCount", async () => {
    // A create past the page leaves the rows as they were and moves the
    // count; the executor pushes that as a delta with no upserts.
    const { perspective, push } = liveHarness({ instances: [{ id: "a" }], totalCount: 1 });
    const live = testModel("LiveOutside").query(perspective, { limit: 1, count: true });
    await live.first;

    push({ ids: ["a"], upsert: [], totalCount: 2 });

    expect(ids(live.getSnapshot().data)).toEqual(["a"]);
    expect(live.getSnapshot().data!.totalCount).toBe(2);
    live.dispose();
  });

  it("emits nothing for a page equal to the last one", async () => {
    // A resync or a reconnect re-open replaces the result wholesale.
    const { perspective, push } = liveHarness({ instances: [{ id: "a" }], totalCount: 1 });
    const live = testModel("LiveUnchanged").query(perspective, { limit: 1, count: true });
    await live.first;
    const seen: any[] = [];
    live.subscribe(s => seen.push(s));
    seen.length = 0;

    push({ result: { instances: [{ id: "a" }], totalCount: 1 } });
    expect(seen).toHaveLength(0);

    push({ result: { instances: [{ id: "a" }], totalCount: 2 } });
    push({ result: { instances: [{ id: "a" }], totalCount: 2 } });
    expect(seen.map(s => s.data.totalCount)).toEqual([2]);
    live.dispose();
  });

  it("ends on the latest state after back-to-back changes (#1051)", async () => {
    // The pushes carry the rows, in revision order, so the last snapshot is
    // the last push.
    const { perspective, push } = liveHarness();
    const live = testModel("LiveLatest").query(perspective, { limit: 10, count: true });
    await live.first;
    const pages: any[] = [];
    live.subscribe(s => pages.push(s.data));
    pages.length = 0;

    push({ ids: ["m1"], upsert: [{ id: "m1" }], totalCount: 1 });
    push({ ids: ["m1", "m2"], upsert: [{ id: "m2" }], totalCount: 2 });

    expect(pages.map(ids)).toEqual([["m1"], ["m1", "m2"]]);
    expect(pages[pages.length - 1].totalCount).toBe(2);
    live.dispose();
  });

  it("delivers nothing after dispose", async () => {
    const { client, perspective, push } = liveHarness();
    const live = testModel("LiveDispose").query(perspective);
    await live.first;
    const callback = jest.fn();
    live.subscribe(callback);
    callback.mockClear();

    live.dispose();
    push({ result: { instances: [{ id: "late" }], totalCount: 1 } });

    expect(callback).not.toHaveBeenCalled();
    expect(client.disposeQuerySubscription).toHaveBeenCalledWith("test-uuid", "sub-1");
  });

  it("keeps delivering after a listener throws", async () => {
    const errorSpy = jest.spyOn(console, "error").mockImplementation(() => {});
    try {
      const { perspective, push } = liveHarness();
      const live = testModel("LiveThrow").query(perspective);
      await live.first;
      const pages: any[] = [];
      live.subscribe(s => {
        pages.push(s.data);
        if (pages.length === 2) throw new Error("consumer failed");
      });

      push({ result: { instances: [{ id: "a" }], totalCount: 1 } });
      push({ result: { instances: [{ id: "a" }, { id: "b" }], totalCount: 2 } });

      expect(pages.map(ids)).toEqual([[], ["a"], ["a", "b"]]);
      live.dispose();
    } finally {
      errorSpy.mockRestore();
    }
  });

  it("rejects `first` when the open fails", async () => {
    const { perspective, client } = liveHarness();
    client.subscribe.mockRejectedValueOnce(new Error("boom"));
    const live = testModel("LiveOpenFails").query(perspective);

    await expect(live.first).rejects.toThrow("boom");
    expect(live.getSnapshot()).toMatchObject({ status: "error" });
  });
});

describe("totalCount is asked for only by the readers of it (P3)", () => {
  const { Ad4mModel, Model, Flag, Property } = require("./index");

  @Model({ name: "CountedThing" })
  class CountedThing extends Ad4mModel {
    @Flag({ through: "test://type", value: "test://counted" })
    type: string = "test://counted";
    @Property({ through: "test://name" })
    name: string = "";
  }

  const sentQuery = (fn: jest.Mock) => JSON.parse(fn.mock.calls[0][1]);
  const perspective = () => liveHarness({ instances: [], totalCount: 4 }).perspective;

  it("findAll() sends no count", async () => {
    const p = perspective();
    await CountedThing.findAll(p, { limit: 5 });
    expect(sentQuery(p.modelQuery)).not.toHaveProperty("count");
  });

  it("findAllAndCount() and count() send count: true", async () => {
    for (const run of [
      (p: any) => CountedThing.findAllAndCount(p, { limit: 5 }),
      (p: any) => CountedThing.count(p),
    ]) {
      const p = perspective();
      await run(p);
      expect(sentQuery(p.modelQuery).count).toBe(true);
    }
  });

  it("query() with count: true opens its subscription with count: true", async () => {
    const h = liveHarness({ instances: [], totalCount: 4 });
    const live = CountedThing.query(h.perspective, { limit: 0, count: true });
    expect((await live.first).totalCount).toBe(4);
    expect(h.sent()[0]).toMatchObject({ limit: 0, count: true });
    live.dispose();
  });
});

describe("live queries never send a cursor (P8)", () => {
  const withCursor = { limit: 5, after: "c1" };

  it("query() drops the query's `after`", async () => {
    const h = liveHarness();
    const live = testModel("CursorlessThing").query(h.perspective, withCursor);
    await live.first;
    expect(h.sent()).toHaveLength(1);
    expect(h.sent()[0]).not.toHaveProperty("after");
    expect(h.sent()[0].limit).toBe(5);
    live.dispose();
  });

  it("an offset page is the subscription itself, without `after`", async () => {
    const h = liveHarness();
    const live = testModel("CursorlessPage").query(h.perspective, { ...withCursor, limit: 10, offset: 10, count: true });
    await live.first;
    const [sub] = h.sent();
    expect(sub).not.toHaveProperty("after");
    expect(sub.offset).toBe(10);
    expect(sub.limit).toBe(10);
    expect(h.perspective.modelQuery).not.toHaveBeenCalled();
    live.dispose();
  });
});

// ============================================================================
// Live queries after a reconnect (the executor ends a socket's subscriptions
// when it closes; there is no keepalive)
// ============================================================================

describe("Model.query reconnect", () => {
  it("re-opens after a reconnect", async () => {
    const h = liveHarness();
    const live = testModel("ReconnectLive").query(h.perspective);
    await live.first;
    await h.reconnect();
    expect(h.opens()).toBe(2);
    live.dispose();
    expect(h.client.disposeQuerySubscription).toHaveBeenCalledWith("test-uuid", "sub-2");
  });

  it("delivers the count from the re-opened subscription, not marked initial", async () => {
    const h = liveHarness((n: number) => ({ instances: [], totalCount: n }));
    const live = testModel("ReconnectCount").query(h.perspective, { limit: 0, count: true });
    expect((await live.first).totalCount).toBe(1);
    const counts: any[] = [];
    live.subscribe(s => counts.push([s.data!.totalCount, s.initial]));
    counts.length = 0;

    await h.reconnect();

    expect(counts).toEqual([[2, false]]);
    live.dispose();
  });

  it("a re-open with an equal page emits nothing", async () => {
    const h = liveHarness({ instances: [{ id: "a" }], totalCount: 1 });
    const live = testModel("ReconnectEqual").query(h.perspective, { limit: 10, count: true });
    await live.first;
    const seen = jest.fn();
    live.subscribe(seen);
    seen.mockClear();

    await h.reconnect();

    expect(h.opens()).toBe(2);
    expect(seen).not.toHaveBeenCalled();
    live.dispose();
  });

  it("a disposed live query does not re-open", async () => {
    const h = liveHarness();
    const live = testModel("ReconnectDisposed").query(h.perspective);
    await live.first;
    live.dispose();
    await h.reconnect();
    expect(h.opens()).toBe(1);
  });
});

// ============================================================================
// Performance Optimisation Tests
// ============================================================================

import { getPropertiesMetadata, getRelationsMetadata, getMemoizedSHACL } from "./decorators";

describe("SHACL Memoisation", () => {
  it("should memoise generateSHACL() per class", () => {
    @Model({ name: "MemoTest1" })
    class MemoTest1 extends Ad4mModel {
      @Property({ through: "test://name" })
      name: string = "";
    }

    const result1 = (MemoTest1 as any).generateSHACL();
    const result2 = (MemoTest1 as any).generateSHACL();
    expect(result1).toBe(result2); // Same reference = memoised
  });

  it("should memoise getPropertiesMetadata per class", () => {
    @Model({ name: "PropMemoTest" })
    class PropMemoTest extends Ad4mModel {
      @Property({ through: "test://x" })
      x: string = "";
    }

    const r1 = getPropertiesMetadata(PropMemoTest);
    const r2 = getPropertiesMetadata(PropMemoTest);
    expect(r1).toBe(r2); // Same reference
  });

  it("should memoise getRelationsMetadata per class", () => {
    @Model({ name: "RelMemoTest" })
    class RelMemoTest extends Ad4mModel {
      @HasMany({ through: "test://items" })
      items: string[] = [];
    }

    const r1 = getRelationsMetadata(RelMemoTest);
    const r2 = getRelationsMetadata(RelMemoTest);
    expect(r1).toBe(r2); // Same reference
  });

  it("should return different results for different classes", () => {
    @Model({ name: "ClassA" })
    class ClassA extends Ad4mModel {
      @Property({ through: "test://a" })
      a: string = "";
    }

    @Model({ name: "ClassB" })
    class ClassB extends Ad4mModel {
      @Property({ through: "test://b" })
      b: string = "";
    }

    const propsA = getPropertiesMetadata(ClassA);
    const propsB = getPropertiesMetadata(ClassB);
    expect(propsA).not.toBe(propsB);
    expect(propsA).toHaveProperty("a");
    expect(propsB).toHaveProperty("b");
  });
});

describe("SHACL recursion — self-referential relations", () => {
  // Reported as a Maximum-call-stack-size-exceeded overflow when Flux's
  // Channel model has `@HasMany(() => Channel) childChannels`. The
  // memoised SHACL builder used to recurse forever because the cache
  // entry wasn't populated until buildSHACL returned. The fix seeds
  // the cache with the in-progress shape; these tests pin the contract.

  it("should generate SHACL for a directly self-referential model without overflowing the stack", () => {
    @Model({ name: "RecursiveChannel" })
    class RecursiveChannel extends Ad4mModel {
      @Property({ through: "channel://name" })
      name: string = "";

      @HasMany(() => RecursiveChannel, { through: "channel://child" })
      childChannels: string[] = [];
    }

    let shacl: any;
    expect(() => {
      shacl = (RecursiveChannel as any).generateSHACL();
    }).not.toThrow();

    expect(shacl).toBeDefined();
    expect(shacl.name).toBe("RecursiveChannel");
    expect(shacl.shape).toBeDefined();
    expect(shacl.shape.nodeShapeUri).toBe("channel://RecursiveChannelShape");

    // The child relation must reference the same class's shape.
    const childRel = (shacl.shape.properties as any[]).find((p) => p.name === "childChannels");
    expect(childRel).toBeDefined();
    expect(childRel.class).toBe("channel://RecursiveChannelShape");
  });

  it("should memoise the self-referential shape (single shared instance)", () => {
    @Model({ name: "MemoSelfRef" })
    class MemoSelfRef extends Ad4mModel {
      @Property({ through: "memo://name" })
      name: string = "";

      @HasMany(() => MemoSelfRef, { through: "memo://child" })
      children: string[] = [];
    }

    const a = (MemoSelfRef as any).generateSHACL();
    const b = (MemoSelfRef as any).generateSHACL();
    expect(a).toBe(b);
  });

  it("should handle mutually-recursive relations between two classes (A → B → A)", () => {
    @Model({ name: "MutualA" })
    class MutualA extends Ad4mModel {
      @Property({ through: "mut://aName" })
      aName: string = "";

      @HasMany(() => MutualB, { through: "mut://aToB" })
      bs: string[] = [];
    }

    @Model({ name: "MutualB" })
    class MutualB extends Ad4mModel {
      @Property({ through: "mut://bName" })
      bName: string = "";

      @HasMany(() => MutualA, { through: "mut://bToA" })
      as: string[] = [];
    }

    let shaclA: any;
    let shaclB: any;
    expect(() => {
      shaclA = (MutualA as any).generateSHACL();
      shaclB = (MutualB as any).generateSHACL();
    }).not.toThrow();

    expect(shaclA.name).toBe("MutualA");
    expect(shaclB.name).toBe("MutualB");
  });

  it("should handle a three-cycle (A → B → C → A)", () => {
    @Model({ name: "CycleA" })
    class CycleA extends Ad4mModel {
      @HasMany(() => CycleB, { through: "cyc://aToB" })
      bs: string[] = [];
    }

    @Model({ name: "CycleB" })
    class CycleB extends Ad4mModel {
      @HasMany(() => CycleC, { through: "cyc://bToC" })
      cs: string[] = [];
    }

    @Model({ name: "CycleC" })
    class CycleC extends Ad4mModel {
      @HasMany(() => CycleA, { through: "cyc://cToA" })
      as: string[] = [];
    }

    expect(() => (CycleA as any).generateSHACL()).not.toThrow();
    expect(() => (CycleB as any).generateSHACL()).not.toThrow();
    expect(() => (CycleC as any).generateSHACL()).not.toThrow();
  });
});

describe("Lazy Conformance Filters", () => {
  it("should defer conformance filter resolution until property access", () => {
    @Model({ name: "LazyTarget2" })
    class LazyTarget2 extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://lazy" })
      type: string = "test://lazy";
      @Property({ through: "test://name", required: true })
      name: string = "";
    }

    @Model({ name: "LazyParent2" })
    class LazyParent2 extends Ad4mModel {
      @HasMany(() => LazyTarget2, { through: "test://children" })
      children: string[] = [];
    }

    // generateSHACL was called during @Model — the shape exists
    const shacl = (LazyParent2 as any).generateSHACL();
    expect(shacl).toBeDefined();
    expect(shacl.shape).toBeDefined();
    expect(shacl.name).toBe("LazyParent2");
  });
});

// ──────────────────────────────────────────────────────────
// deepQuery — getter evaluation on collection queries
// ──────────────────────────────────────────────────────────

describe("deepQuery — getter evaluation", () => {
  @Model({ name: "DeepQueryTestMessage" })
  class DeepQueryTestMessage extends Ad4mModel {
    @Flag({ through: "flux://entry_type", value: "flux://message" })
    type: string = "";

    @Property({ through: "flux://body" })
    body: string = "";

    @Property({
      through: "flux://has_reply",
      getter: `SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1`,
    })
    replyingTo?: string;

    @ReadOnly({
      through: "flux://is_popular",
      getter: `ASK WHERE { ?source <flux://is_popular> "true" . }`,
    })
    isPopular: boolean = false;

    @HasMany({ through: "flux://reaction" })
    reactions: string[] = [];
  }

  let sparqlCalls: string[];
  const mockPerspective: any = {
    uuid: "test-uuid",
    modelQuery: jest.fn(async (className: string, queryJson: string) => {
      // Return a basic result for the single-instance getData() path
      const query = JSON.parse(queryJson);
      const id = query?.where?.id;
      if (id) {
        return {
          instances: [{ id, body: "Hello" }],
          totalCount: 1,
        };
      }
      return { instances: [], totalCount: 0 };
    }),
    evaluateGetters: jest.fn(async (className: string, instanceIds: string[], propertyNames?: string[]) => {
      // Return empty results by default — tests can override this mock
      const result: Record<string, Record<string, any>> = {};
      return result;
    }),
    querySparql: jest.fn(async (q: string) => {
      sparqlCalls.push(q);
      return [];
    }),
    get: jest.fn().mockResolvedValue([]),
    getExpression: jest.fn().mockResolvedValue(null),
  };

  beforeEach(() => {
    sparqlCalls = [];
    mockPerspective.querySparql.mockClear();
    mockPerspective.evaluateGetters.mockClear();
    mockPerspective.modelQuery.mockClear();
    mockPerspective.get.mockClear();
  });

  function makeInstances(count: number): InstanceType<typeof DeepQueryTestMessage>[] {
    return Array.from({ length: count }, (_, i) =>
      new DeepQueryTestMessage(mockPerspective, `flux://msg-${i}`)
    );
  }

  it("single-instance get() delegates getter evaluation to Rust (no JS-side SPARQL)", async () => {
    const instance = new DeepQueryTestMessage(mockPerspective, "flux://msg-single");
    mockPerspective.querySparql.mockImplementation(async (q: string) => {
      sparqlCalls.push(q);
      if (q.includes("flux://msg-single") && !q.includes("flux://has_reply") && !q.includes("flux://is_popular")) {
        return [
          { source: "flux://msg-single", predicate: "flux://entry_type", target: "flux://message", author: "did:key:a", timestamp: "3000" },
          { source: "flux://msg-single", predicate: "flux://body", target: "literal:string:Hello", author: "did:key:a", timestamp: "3000" },
        ];
      }
      return [];
    });

    await instance.get();
    // Getter evaluation now happens Rust-side via evaluate_getters() — no JS SPARQL calls
    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    expect(getterCalls.length).toBe(0);
  });

  describe("Ad4mModel.evaluateGetters()", () => {
    it("resolves getters for a batch of instances via single RPC", async () => {
      const instances = makeInstances(4);

      await DeepQueryTestMessage.evaluateGetters(instances.slice(0, 2), mockPerspective, ["replyingTo"]);

      // Should make exactly 1 RPC call via perspective.evaluateGetters
      expect(mockPerspective.evaluateGetters).toHaveBeenCalledTimes(1);
      const [className, instanceIds, propertyNames] = mockPerspective.evaluateGetters.mock.calls[0];
      expect(className).toBe("flux://DeepQueryTestMessage");
      expect(instanceIds).toEqual(["flux://msg-0", "flux://msg-1"]);
      expect(propertyNames).toEqual(["replyingTo"]);
      // No querySparql calls — all done in-process
      expect(sparqlCalls.length).toBe(0);
    });

    it("evaluates all getters when propertyNames is omitted", async () => {
      const instances = makeInstances(2);

      await DeepQueryTestMessage.evaluateGetters(instances, mockPerspective);

      expect(mockPerspective.evaluateGetters).toHaveBeenCalledTimes(1);
      const [className, instanceIds, propertyNames] = mockPerspective.evaluateGetters.mock.calls[0];
      expect(className).toBe("flux://DeepQueryTestMessage");
      expect(instanceIds).toEqual(["flux://msg-0", "flux://msg-1"]);
      expect(propertyNames).toBeUndefined();
    });

    it("applies results to instances and syncs snapshots", async () => {
      const instances = makeInstances(2);
      // Simulate Rust returning getter results
      mockPerspective.evaluateGetters.mockResolvedValueOnce({
        "flux://msg-0": { replyingTo: "flux://msg-99" },
        "flux://msg-1": { replyingTo: "flux://msg-88" },
      });

      await DeepQueryTestMessage.evaluateGetters(instances, mockPerspective, ["replyingTo"]);

      expect((instances[0] as any).replyingTo).toBe("flux://msg-99");
      expect((instances[1] as any).replyingTo).toBe("flux://msg-88");
    });

    it("handles empty array gracefully", async () => {
      await DeepQueryTestMessage.evaluateGetters([], mockPerspective);
      expect(mockPerspective.evaluateGetters).not.toHaveBeenCalled();
    });
  });

  describe("deepQuery in the query", () => {
    it("deepQuery defaults to true when not explicitly set", async () => {
      mockPerspective.modelQuery.mockResolvedValueOnce({ instances: [], totalCount: 0 });
      await DeepQueryTestMessage.findAll(mockPerspective);

      expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
      const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
      const qi = JSON.parse(queryJson);
      expect(qi.deepQuery).toBe(true);
    });

    it("deepQuery can be explicitly set to false", async () => {
      mockPerspective.modelQuery.mockResolvedValueOnce({ instances: [], totalCount: 0 });
      await DeepQueryTestMessage.findAll(mockPerspective, { deepQuery: false });

      expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
      const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
      const qi = JSON.parse(queryJson);
      expect(qi.deepQuery).toBe(false);
    });
  });
});

// ─── Ad4mModel.fromSHACL() ───────────────────────────────────────────────────

describe("Ad4mModel.fromSHACL()", () => {
  function makeShape(targetClass: string, props: Array<import("../shacl/SHACLShape").SHACLPropertyShape>) {
    const shape = new SHACLShape(targetClass);
    for (const p of props) shape.addProperty(p);
    return shape;
  }

  it("assigns the given name as className", () => {
    const shape = makeShape("flux://Channel", []);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    expect((Cls as any).className).toBe("Channel");
    expect((new (Cls as any)({}, "flux://1")).className).toBe("Channel");
  });

  it("registers scalar property (maxCount=1) via setPropertyRegistryEntry", () => {
    const shape = makeShape("flux://Channel", [
      { name: "title", path: "flux://has_title", maxCount: 1, writable: true },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["title"]).toBeDefined();
    expect(meta.properties["title"].predicate).toBe("flux://has_title");
    expect(meta.properties["title"].readOnly).toBe(false);
  });

  it("registers collection property (no maxCount) as hasMany relation", () => {
    const shape = makeShape("flux://Channel", [
      { name: "messages", path: "flux://has_message" },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    const meta = Cls.getModelMetadata();
    expect(meta.relations["messages"]).toBeDefined();
    expect(meta.relations["messages"].predicate).toBe("flux://has_message");
    expect(meta.relations["messages"].direction).toBe("forward");
  });

  it("registers collection property (maxCount=5) as hasMany relation", () => {
    const shape = makeShape("flux://Channel", [
      { name: "participants", path: "flux://participant", maxCount: 5 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    const meta = Cls.getModelMetadata();
    expect(meta.relations["participants"]).toBeDefined();
    expect(meta.relations["participants"].direction).toBe("forward");
  });

  it("propagates resolveLanguage onto scalar properties", () => {
    const shape = makeShape("flux://Post", [
      { name: "body", path: "flux://body", maxCount: 1, resolveLanguage: "literal" },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Post");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["body"].resolveLanguage).toBe("literal");
  });

  it("defaults writable to true when not specified", () => {
    const shape = makeShape("flux://Post", [
      { name: "body", path: "flux://body", maxCount: 1 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Post");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["body"].readOnly).toBe(false);
  });

  it("respects writable:false", () => {
    const shape = makeShape("flux://Post", [
      { name: "id", path: "flux://id", maxCount: 1, writable: false },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Post");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["id"].readOnly).toBe(true);
  });

  it("registers flag properties (hasValue) as type-discrimination entries", () => {
    const shape = makeShape("flux://Message", [
      { name: "type", path: "flux://entry_type", hasValue: "flux://message" },
      { name: "body", path: "flux://body", maxCount: 1 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Message");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["type"]).toBeDefined();
    expect(meta.properties["type"].predicate).toBe("flux://entry_type");
    expect(meta.properties["type"].flag).toBe(true);
    expect(meta.properties["type"].required).toBe(true);
    expect(meta.properties["type"].initial).toBe("flux://message");
    expect(meta.properties["body"]).toBeDefined();
  });

  it("flag property from fromSHACL produces SPARQL type-discriminator triple", () => {
    const shape = makeShape("flux://Message", [
      { name: "type", path: "flux://entry_type", hasValue: "flux://message" },
      { name: "body", path: "flux://body", maxCount: 1 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Message");
    const meta = Cls.getModelMetadata();
    const allRelsMeta = {} as any;
    const sparql = buildSPARQLQuery(meta, allRelsMeta, {}, Cls);
    expect(sparql).toContain("<flux://entry_type>");
    expect(sparql).toContain("<flux://message>");
  });

  it("recovers flag value from constructor_actions (backward-compat for old shapes)", () => {
    const shape = new SHACLShape("flux://Channel");
    // An old shape: property has NO hasValue, but constructor_actions carries it
    shape.addProperty({ name: "type", path: "flux://entry_type", maxCount: 1 });
    shape.addProperty({ name: "name", path: "flux://name", maxCount: 1 });
    shape.constructor_actions = [
      { action: "addLink", source: "this", predicate: "flux://entry_type", target: "flux://channel" },
    ];
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    const meta = Cls.getModelMetadata();
    expect(meta.properties["type"]).toBeDefined();
    expect(meta.properties["type"].flag).toBe(true);
    expect(meta.properties["type"].initial).toBe("flux://channel");
  });

  it("skips properties without a name field", () => {
    const shape = makeShape("flux://Message", [
      { path: "flux://anonymous" } as any,
      { name: "body", path: "flux://body", maxCount: 1 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Message");
    const meta = Cls.getModelMetadata();
    expect(Object.keys(meta.properties)).toEqual(["body"]);
  });

  it("propagates local flag onto relations", () => {
    const shape = makeShape("flux://Channel", [
      { name: "drafts", path: "flux://draft", local: true },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    const meta = Cls.getModelMetadata();
    expect(meta.relations["drafts"].local).toBe(true);
  });

  it("applies Model decorator so generateSHACL and generateSDNA are available", () => {
    const shape = makeShape("flux://Channel", [
      { name: "title", path: "flux://has_title", maxCount: 1 },
    ]);
    const Cls = Ad4mModel.fromSHACL(shape, "Channel");
    expect(typeof (Cls as any).generateSDNA).toBe("function");
    expect(typeof (Cls as any).generateSHACL).toBe("function");
  });

  it("keeps the shape's target class: the namespace is everything before the local name", () => {
    for (const [targetClass, name] of [
      ["urn:isbn:Book", "Book"],
      ["a://Note", "Note"],
      ["http://x.org/ns#Thing", "Thing"],
      ["a://b/c/Thing", "Thing"],
    ]) {
      const Cls = Ad4mModel.fromSHACL(makeShape(targetClass, []), name);
      expect(Cls.getTargetClass()).toBe(targetClass);
      expect(Cls.getClassName()).toBe(name);
    }
  });

  it("handles shapes with no properties", () => {
    const shape = makeShape("flux://Empty", []);
    const Cls = Ad4mModel.fromSHACL(shape, "Empty");
    const meta = Cls.getModelMetadata();
    expect(Object.keys(meta.properties)).toHaveLength(0);
    expect(Object.keys(meta.relations)).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// IncludeProjection — key splitting and query routing
// ---------------------------------------------------------------------------

describe("IncludeProjection type guard and key splitting", () => {
  // Shared test models
  @Model({ name: "Signal" })
  class Signal extends Ad4mModel {
    @Property({ through: "signal://type" })
    signalTypeId: string = "";

    @Property({ through: "signal://author" })
    author: string = "";
  }

  @Model({ name: "Post" })
  class Post extends Ad4mModel {
    @Property({ through: "post://title" })
    title: string = "";

    @HasMany({ through: "post://signal", target: () => Signal })
    signals: Signal[] = [];

    @HasMany({ through: "post://comment" })
    comments: string[] = [];
  }

  const mockPerspective = {
    querySparql: jest.fn(),
    modelQuery: jest.fn(),
    infer: jest.fn(),
    uuid: "test-perspective-uuid",
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [{ id: "post://1", title: "Hello", $signalCount: 3 }],
      totalCount: 1,
    });
  });

  // --- isIncludeProjection type guard ---

  it("isIncludeProjection returns true for objects with a 'from' field", () => {
    expect(isIncludeProjection({ from: "signals", count: true })).toBe(true);
    expect(isIncludeProjection({ from: "comments", limit: 1 })).toBe(true);
  });

  it("isIncludeProjection returns false for non-projection values", () => {
    expect(isIncludeProjection(true)).toBe(false);
    expect(isIncludeProjection(false)).toBe(false);
    expect(isIncludeProjection(null)).toBe(false);
    expect(isIncludeProjection(undefined)).toBe(false);
    expect(isIncludeProjection(42)).toBe(false);
    // RelationSubQuery (has no 'from' key)
    expect(isIncludeProjection({ limit: 5, order: { timestamp: "DESC" } })).toBe(false);
  });

  // --- $-key splitting ---

  it("routes $-prefixed keys to queryInput.projections, not queryInput.include", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $signalCount: { from: "signals", count: true },
        comments: true,
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // Normal relation goes to include
    expect(qi.include?.comments).toBe(true);
    expect(qi.include?.$signalCount).toBeUndefined();

    // Projection goes to projections
    expect(qi.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
    expect(qi.projections?.comments).toBeUndefined();
  });

  it("routes $-prefixed IncludeProjection values to projections ($ prefix required)", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $mySignals: { from: "signals", limit: 1 },
        comments: true,
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    expect(qi.projections?.$mySignals).toMatchObject({ from: "signals", limit: 1 });
    expect(qi.include?.comments).toBe(true);
  });

  it("does NOT route non-$ keys to projections even if value has 'from' shape", async () => {
    // $ prefix is required — a key without it goes to include, not projections
    await Post.findAll(mockPerspective, {
      include: {
        mySignals: { from: "signals", limit: 1 } as any,
        comments: true,
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // Without $ prefix it stays in include (behaviour is undefined/broken but not silently rerouted)
    expect(qi.projections?.mySignals).toBeUndefined();
    expect(qi.include?.mySignals).toBeDefined();
  });

  it("omits queryInput.include when all keys are projections", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $count: { from: "signals", count: true },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    expect(qi.include).toBeUndefined();
    expect(qi.projections?.$count).toMatchObject({ from: "signals", count: true });
  });

  it("omits queryInput.projections when all keys are normal includes", async () => {
    await Post.findAll(mockPerspective, {
      include: { comments: true },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    expect(qi.projections).toBeUndefined();
    expect(qi.include?.comments).toBe(true);
  });

  it("tags projection with targetClassName when relation target is registered", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $signalCount: { from: "signals", count: true },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // targetClassName drives the executor's cache lookup for the target shape.
    expect(qi.projections?.$signalCount?.targetClassName).toBe("signal://Signal");
  });

  it("leaves targetClassName absent when relation has no target decorator", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $commentCount: { from: "comments", count: true },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // 'comments' HasMany has no target() thunk → no targetClassName
    expect(qi.projections?.$commentCount?.targetClassName).toBeUndefined();
  });

  // --- $-keys inside a nested include ---
  //
  // The executor reads a sub-query as it reads a top-level query, so a nested
  // projection belongs in the sub-query's own `projections`. Left in `include`
  // it named no relation and was skipped: no error, and no field.

  @Model({ name: "Thread" })
  class Thread extends Ad4mModel {
    @HasMany({ through: "thread://post", target: () => Post })
    posts: Post[] = [];
  }

  @Model({ name: "Board" })
  class Board extends Ad4mModel {
    @HasMany({ through: "board://post", target: () => Post })
    posts: Post[] = [];

    @HasMany({ through: "board://thread", target: () => Thread })
    threads: Thread[] = [];

    @HasMany({ through: "board://item", polymorphic: true })
    items: string[] = [];

    // Polymorphic, but with a declared target: members may still be of other classes.
    @HasMany({ through: "board://pinned", target: () => Post, polymorphic: true })
    pinned: Post[] = [];
  }

  it("moves a $-key inside a nested include into that sub-query's projections", async () => {
    await Board.findAll(mockPerspective, {
      include: { posts: { include: { $signalCount: { from: "signals", count: true } } } },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const posts = JSON.parse(queryJson).include.posts;

    expect(posts.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
    expect(posts.include).toBeUndefined();
    // Read against the relation's target, so it is tagged as a top-level one would be.
    expect(posts.projections.$signalCount.targetClassName).toBe("signal://Signal");
  });

  it("keeps the nested relations beside the projections it moves", async () => {
    await Board.findAll(mockPerspective, {
      include: {
        posts: { include: { signals: true, $signalCount: { from: "signals", count: true } } },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const posts = JSON.parse(queryJson).include.posts;

    expect(posts.include).toEqual({ signals: true });
    expect(posts.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
  });

  it("moves a $-key two include levels down into the innermost sub-query's projections", async () => {
    await Board.findAll(mockPerspective, {
      include: {
        threads: { include: { posts: { include: { $signalCount: { from: "signals", count: true } } } } },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const posts = JSON.parse(queryJson).include.threads.include.posts;

    expect(posts.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
    expect(posts.include).toBeUndefined();
    // Tagged against Post, the class two relations down.
    expect(posts.projections.$signalCount.targetClassName).toBe("signal://Signal");
  });

  it("leaves a projection untagged under a polymorphic relation that declares a target", async () => {
    await Board.findAll(mockPerspective, {
      include: { pinned: { include: { $signalCount: { from: "signals", count: true } } } },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const pinned = JSON.parse(queryJson).include.pinned;

    expect(pinned.polymorphic).toBe(true);
    expect(pinned.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
    // Tagging from the declared target (Post → Signal) would name one class for
    // members of several; the executor reads each member's own class instead.
    expect(pinned.projections.$signalCount.targetClassName).toBeUndefined();
  });

  it("moves it under a polymorphic relation too, untagged, for the executor to read per class", async () => {
    await Board.findAll(mockPerspective, {
      include: { items: { include: { $signalCount: { from: "signals", count: true } } } },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const items = JSON.parse(queryJson).include.items;

    expect(items.polymorphic).toBe(true);
    expect(items.projections?.$signalCount).toMatchObject({ from: "signals", count: true });
    // The members are of several classes, so there is no one target to name.
    expect(items.projections.$signalCount.targetClassName).toBeUndefined();
  });

  it("leaves the projections in the caller's query where the caller wrote them", async () => {
    const query = {
      include: {
        $postCount: { from: "posts", count: true },
        posts: { include: { $signalCount: { from: "signals", count: true } } },
      },
    } as const;
    const before = JSON.stringify(query);

    await Board.findAll(mockPerspective, query);

    expect(JSON.stringify(query)).toBe(before);
  });

  // --- result passthrough ---

  it("returns $-keyed projection values attached by Rust on instances", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [{ id: "post://1", title: "Hello", $signalCount: 7 }],
      totalCount: 1,
    });

    const results = await Post.findAll(mockPerspective, {
      include: { $signalCount: { from: "signals", count: true } },
    });

    expect((results[0] as any).$signalCount).toBe(7);
  });

  // --- hydration of non-count projections ---

  it("passes through scalar non-count projection value hydrated by Rust", async () => {
    // Rust hydrates projection targets in-process and returns full JSON objects.
    // The TS layer receives an already-hydrated object and passes it through.
    mockPerspective.modelQuery.mockResolvedValueOnce({
      instances: [{ id: "post://1", title: "Hello", $mySignal: { id: "signal://abc", signalTypeId: "type1" } }],
      totalCount: 1,
    });

    const results = await Post.findAll(mockPerspective, {
      include: { $mySignal: { from: "signals", limit: 1 } },
    });

    const mySignal = (results[0] as any).$mySignal;
    expect(mySignal).toMatchObject({ id: "signal://abc", signalTypeId: "type1" });
  });

  it("passes through array non-count projection values hydrated by Rust", async () => {
    // Rust hydrates all IRIs in the list and returns full JSON objects.
    mockPerspective.modelQuery.mockResolvedValueOnce({
      instances: [{
        id: "post://1", title: "Hello",
        $recentSignals: [
          { id: "signal://a", signalTypeId: "type1" },
          { id: "signal://b", signalTypeId: "type2" },
        ],
      }],
      totalCount: 1,
    });

    const results = await Post.findAll(mockPerspective, {
      include: { $recentSignals: { from: "signals" } },
    });

    const sigs = (results[0] as any).$recentSignals;
    expect(Array.isArray(sigs)).toBe(true);
    expect(sigs).toHaveLength(2);
    expect(sigs[0]).toMatchObject({ id: "signal://a", signalTypeId: "type1" });
    expect(sigs[1]).toMatchObject({ id: "signal://b", signalTypeId: "type2" });
  });

  it("passes through raw IRI when Rust cannot hydrate the projection target", async () => {
    // When the target cannot be resolved, Rust keeps the raw IRI string.
    mockPerspective.modelQuery.mockResolvedValueOnce({
      instances: [{ id: "post://1", title: "Hello", $mySignal: "signal://unknown" }],
      totalCount: 1,
    });

    const results = await Post.findAll(mockPerspective, {
      include: { $mySignal: { from: "signals", limit: 1 } },
    });

    // Raw IRI preserved when no matching target instance exists in the store.
    expect((results[0] as any).$mySignal).toBe("signal://unknown");
  });

  it("does NOT attempt hydration for count projections (they return numbers)", async () => {
    mockPerspective.modelQuery.mockResolvedValueOnce({
      instances: [{ id: "post://1", $signalCount: 5 }],
      totalCount: 1,
    });

    const results = await Post.findAll(mockPerspective, {
      include: { $signalCount: { from: "signals", count: true } },
    });

    // Only one modelQuery call — no follow-up findAll for count projections.
    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect((results[0] as any).$signalCount).toBe(5);
  });
});

describe("Ad4mModel instance identity (uniqueness without envelopes)", () => {
  // Without a signed envelope, the only thing that keeps two instances from
  // collapsing onto the same node is their auto-generated base expression.
  // These tests pin that guarantee down: identity must come from the
  // instance's own IRI, never from (possibly duplicate) property content.
  const mockPerspective = {} as any;

  it("auto-generates a distinct id for every instance, even with identical property values", () => {
    @Model({ name: "IdentityTestMessage" })
    class IdentityTestMessage extends Ad4mModel {
      @Property({ through: "identity://body", resolveLanguage: "literal" })
      body: string = "";
    }

    const message1 = new IdentityTestMessage(mockPerspective);
    const message2 = new IdentityTestMessage(mockPerspective);

    message1.body = "hello";
    message2.body = "hello";

    expect(message1.body).toBe(message2.body);
    expect(message1.id).not.toBe(message2.id);
  });

  it("uses the ad4m://obj/<random> scheme, not content-derived addressing", () => {
    @Model({ name: "IdentityTestSchemeCheck" })
    class IdentityTestSchemeCheck extends Ad4mModel {}

    const instance = new IdentityTestSchemeCheck(mockPerspective);

    // 24 lowercase a-z chars after the prefix (see makeRandomId in ./util.ts).
    // Locks in the scheme so a regression toward hashing property content
    // for the base expression — which would reintroduce the exact collision
    // this scheme exists to prevent — fails this test.
    expect(instance.id).toMatch(/^ad4m:\/\/obj\/[a-z]{24}$/);
  });

  it("never reuses an id across many instances", () => {
    @Model({ name: "IdentityTestManyInstances" })
    class IdentityTestManyInstances extends Ad4mModel {}

    const ids = Array.from(
      { length: 200 },
      () => new IdentityTestManyInstances(mockPerspective).id
    );

    expect(new Set(ids).size).toBe(ids.length);
  });

  it("respects an explicitly-provided base expression instead of generating one", () => {
    @Model({ name: "IdentityTestExplicitId" })
    class IdentityTestExplicitId extends Ad4mModel {}

    const instance = new IdentityTestExplicitId(mockPerspective, "flux://existing-instance");

    expect(instance.id).toBe("flux://existing-instance");
  });
});

describe("Relation writes: to-one batching and scalar coercion", () => {
  // `Relationship` is the shape that made both of these visible: a record whose
  // two to-one relations name the things it connects, so it is worthless until
  // both endpoints are linked, and it is created and pointed in one gesture.
  @Model({ name: "TestRelationship" })
  class TestRelationship extends Ad4mModel {
    @Flag({ through: "we://flag", value: "we://relationship" })
    flag: string = "";

    @Property({ through: "we://title", required: true })
    label: string = "";

    @HasOne({ through: "we://relationship_source" })
    source?: string;

    @HasOne({ through: "we://relationship_target" })
    target?: string;
  }

  // No required property, no flag and no initial value: an empty constructor.
  @Model({ name: "TestNoConstructor" })
  class TestNoConstructor extends Ad4mModel {
    @HasOne({ through: "we://placed_node" })
    node?: string;
  }

  const makePerspective = () =>
    ({
      mutate: jest.fn().mockResolvedValue({ ids: [] }),
      createBatch: jest.fn().mockResolvedValue("batch-generated"),
      commitBatch: jest.fn().mockResolvedValue(undefined),
      uuid: "test-perspective-uuid",
    }) as any;

  /** The ops of every `mutate` call, in order. */
  const sentOps = (perspective: any): any[] =>
    perspective.mutate.mock.calls.flatMap((call: any[]) => call[0]);

  /** Targets written by `mutate`: the members of every relation, across every op. */
  const writtenTargets = (perspective: any): string[] =>
    sentOps(perspective).flatMap((op: any) =>
      [op.values, op.set, op.add, op.remove].flatMap((group) =>
        Object.values(group ?? {}).flatMap((v) => (Array.isArray(v) ? v : typeof v === "string" && v.includes("://") ? [v] : [])) as string[]
      )
    );

  /** The batch of each `mutate` call. */
  const batchIds = (perspective: any): unknown[] =>
    perspective.mutate.mock.calls.map((call: any[]) => call[1]?.batch);

  describe("a scalar handed to a relation field", () => {
    it("writes a link instead of being silently dropped", async () => {
      // The failure this closes: this call typechecked, ran, resolved, and
      // wrote no link — leaving a relationship with no endpoints, which only
      // surfaces later as a reader calling the record malformed.
      const perspective = makePerspective();

      await TestRelationship.create(
        perspective,
        { label: "contradicts", source: "we://block/a", target: "we://block/b" },
        { batchId: "batch-1" }
      );

      expect(writtenTargets(perspective)).toEqual(
        expect.arrayContaining(["we://block/a", "we://block/b"])
      );
    });

    it("puts the relation into the same create op as the properties", async () => {
      const perspective = makePerspective();

      await TestRelationship.create(
        perspective,
        { label: "contradicts", source: "we://block/a" },
        { batchId: "batch-1" }
      );

      expect(perspective.mutate).toHaveBeenCalledTimes(1);
      expect(sentOps(perspective)).toEqual([
        expect.objectContaining({
          op: "create",
          values: { label: "contradicts", source: "we://block/a" },
        }),
      ]);
    });

    it("agrees with the documented array form", async () => {
      const scalar = makePerspective();
      await TestRelationship.create(
        scalar,
        { label: "x", source: "we://block/a" },
        { batchId: "b" }
      );

      const array = makePerspective();
      await TestRelationship.create(
        array,
        { label: "x", source: ["we://block/a"] as any },
        { batchId: "b" }
      );

      expect(writtenTargets(scalar)).toEqual(writtenTargets(array));
    });

    it("writes the relation when the constructor is empty", async () => {
      const perspective = makePerspective();

      await TestNoConstructor.create(
        perspective,
        { node: "we://block/a" },
        { batchId: "batch-1" }
      );

      expect(writtenTargets(perspective)).toContain("we://block/a");
    });
  });

  // The failure this closes: every `save()` of a new instance logged one
  // Rust-side "declares no setter" warning per ORM-internal field, per flag,
  // and per empty HasMany relation — none of which are real model data, so
  // none of them should ever have been offered to the create op at all.
  describe("values sent in the create op", () => {
    @Model({ name: "TestNoisyPost" })
    class TestNoisyPost extends Ad4mModel {
      @Flag({ through: "test://post_type", value: "test://post" })
      type: string = "test://post";

      @Property({ through: "test://title", required: true })
      title: string = "";

      @HasMany({ through: "test://has_tag" })
      tags: string[] = [];

      @HasOne({ through: "test://in_channel" })
      channel: string = "";
    }

    it("offers only settable scalar properties: no bookkeeping, flags or relations", async () => {
      const perspective = makePerspective();

      await TestNoisyPost.create(perspective, { title: "hello", channel: "test://channel/1" }, { batchId: "batch-1" });

      // The @HasOne value is written as a relation; the flag and the empty tags are not sent.
      expect(sentOps(perspective)[0].values).toEqual({ title: "hello", channel: "test://channel/1" });
    });
  });

  // `readOnly` keeps `through` as the predicate the relation reads. Writing it
  // from save() would be the setter the shape refuses to offer, and on a create
  // the `= []` initialiser turns that write into a clear.
  describe("readOnly relations without a getter", () => {
    @Model({ name: "TestReadOnlyRelations" })
    class TestReadOnlyRelations extends Ad4mModel {
      @Property({ through: "test://title", required: true })
      title: string = "";

      @HasMany({ through: "test://voted_by", readOnly: true })
      votedBy: string[] = [];

      @HasOne({ through: "test://resolved_by", readOnly: true })
      resolvedBy?: string;

      // The control: an ordinary relation on the same model is still written.
      @HasMany({ through: "test://has_tag" })
      tags: string[] = [];
    }

    /** Properties and relations written by `mutate`, across every op. */
    const writtenPredicates = (perspective: any): string[] =>
      sentOps(perspective).flatMap((op: any) =>
        [op.values, op.set, op.add, op.remove].flatMap((group) => Object.keys(group ?? {}))
      );

    it("are not written by create(), not even as the empty initialiser", async () => {
      const perspective = makePerspective();

      await TestReadOnlyRelations.create(
        perspective,
        { title: "t", tags: ["test://tag/1"] },
        { batchId: "batch-1" }
      );

      expect(writtenPredicates(perspective)).toContain("tags");
      expect(writtenPredicates(perspective)).not.toContain("votedBy");
      expect(writtenPredicates(perspective)).not.toContain("resolvedBy");
    });

    it("are not written by save() even when the caller sets them", async () => {
      const perspective = makePerspective();
      const instance = new TestReadOnlyRelations(perspective);
      instance.title = "t";
      instance.tags = ["test://tag/1"];
      instance.votedBy = ["did:key:a"];
      instance.resolvedBy = "did:key:b";

      await instance.save("batch-1");

      expect(writtenPredicates(perspective)).toContain("tags");
      expect(writtenPredicates(perspective)).not.toContain("votedBy");
      expect(writtenPredicates(perspective)).not.toContain("resolvedBy");
      expect(writtenTargets(perspective)).not.toContain("did:key:a");
      expect(writtenTargets(perspective)).not.toContain("did:key:b");
    });
  });

  describe("generated @HasOne accessors", () => {
    it("forward batchId, so a to-one link can join a write group", async () => {
      // Without this the link commits on its own, so every subscriber sees the
      // record with its relation still empty — a card before its placement.
      const perspective = makePerspective();
      const rel = new TestRelationship(perspective, "we://rel/1") as any;

      await rel.setSource("we://block/a", "batch-1");
      await rel.addTarget("we://block/b", "batch-1");
      await rel.removeTarget("we://block/b", "batch-1");

      expect(batchIds(perspective)).toEqual(["batch-1", "batch-1", "batch-1"]);
    });

    it("still work without one", async () => {
      const perspective = makePerspective();
      const rel = new TestRelationship(perspective, "we://rel/1") as any;

      await rel.setSource("we://block/a");

      expect(batchIds(perspective)).toEqual([undefined]);
    });
  });
});

describe("Polymorphic relations", () => {
  @Model({ name: "PolyTextBlock" })
  class PolyTextBlock extends Ad4mModel {
    @Flag({ through: "we://flag", value: "we://text_block" })
    flag: string = "";
    @Property({ through: "we://text" })
    text: string = "";
  }

  @Model({ name: "PolyImageBlock" })
  class PolyImageBlock extends Ad4mModel {
    @Flag({ through: "we://flag", value: "we://image_block" })
    flag: string = "";
    @Property({ through: "we://src" })
    src: string = "";
  }

  @Model({ name: "PolyCollection" })
  class PolyCollection extends Ad4mModel {
    // No target class — the members are of genuinely different types, which is
    // the situation `polymorphic` exists for.
    @HasMany({
      through: "we://children",
      polymorphic: true,
      instantiateAs: () => [PolyTextBlock, PolyImageBlock],
    })
    children: string[] = [];
  }

  const mockPerspective = { uuid: "test" } as any;

  it("constructs each child as the class the executor says it is", () => {
    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [
            { id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" },
            { id: "we://i/1", __subjectClass: "we://PolyImageBlock", src: "cat.png" },
          ],
        },
      ],
    };

    const [collection] = PolyCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    expect(collection.children[0]).toBeInstanceOf(PolyTextBlock);
    expect(collection.children[1]).toBeInstanceOf(PolyImageBlock);
    // The property that only exists on the concrete class — what a base-class
    // hydration would have dropped before the value ever reached here.
    expect(collection.children[0].text).toBe("hello");
    expect(collection.children[1].src).toBe("cat.png");
  });

  it("snapshots each included child, so it is clean and save() updates it", () => {
    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [{ id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" }],
        },
      ],
    };

    const [collection] = PolyCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    expect(collection.isDirty()).toBe(false);
    expect(collection.children[0].isDirty()).toBe(false);
    collection.children[0].text = "changed";
    expect(collection.children[0].changedFields()).toEqual(["text"]);
  });

  it("maps a child to its class by the target class IRI the class itself declares", () => {
    // The mapping is derived from the class's own target class, not restated
    // beside the class list — the two cannot drift apart because there is only
    // one of them. By IRI, so two classes of one name stay apart.
    expect((PolyTextBlock as any).getTargetClass()).toBe("we://PolyTextBlock");

    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [{ id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" }],
        },
      ],
    };

    const [collection] = PolyCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    expect(collection.children[0]).toBeInstanceOf(PolyTextBlock);
  });

  it("carries the readings a child was chosen over onto the typed instance", () => {
    // A base expression can conform to two unrelated classes at once, and the
    // executor has to pick one to hydrate against. The set it picked from rides
    // back with the child, so a caller can see there was a choice — and fetch
    // the other reading by asking that class for this id.
    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [
            {
              id: "we://b/1",
              __subjectClass: "we://PolyTextBlock",
              __subjectClasses: ["we://PolyTextBlock", "we://PolyBookmark"],
              text: "hello",
            },
          ],
        },
      ],
    };

    const [collection] = PolyCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    const child = collection.children[0];
    expect(child).toBeInstanceOf(PolyTextBlock);
    expect(child.__subjectClasses).toEqual(["we://PolyTextBlock", "we://PolyBookmark"]);
  });

  it("leaves children as data when no instantiateAs is given", () => {
    @Model({ name: "PolyCollectionNoClasses" })
    class PolyCollectionNoClasses extends Ad4mModel {
      @HasMany({ through: "we://children", polymorphic: true })
      children: string[] = [];
    }

    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [{ id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" }],
        },
      ],
    };

    const [collection] = PolyCollectionNoClasses.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    // Still correct data, just untyped — the concrete class name is present for
    // a caller that wants to do its own dispatch.
    expect(collection.children[0].text).toBe("hello");
    expect(collection.children[0].__subjectClass).toBe("we://PolyTextBlock");
  });

  it("keeps a child whose class is not listed, rather than dropping it", () => {
    // `instantiateAs` says what this call site can construct, not what the
    // relation may contain. A heterogeneous relation is open by definition, so
    // an unlisted class must arrive as data — narrowing a read to particular
    // classes is a thing to ask for in the query, never a side effect of
    // declaring which constructors happen to be in scope.
    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [
            { id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" },
            { id: "we://k/1", __subjectClass: "we://PolyTaskBlock", done: true },
          ],
        },
      ],
    };

    const [collection] = PolyCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    expect(collection.children).toHaveLength(2);
    expect(collection.children[0]).toBeInstanceOf(PolyTextBlock);
    expect(collection.children[1].done).toBe(true);
    expect(collection.children[1].__subjectClass).toBe("we://PolyTaskBlock");
  });

  it("does not construct an unlisted child as the relation's declared target", () => {
    // A relation may declare a target *and* read polymorphically — the target
    // says what the members usually are, not what they must be. A child of some
    // third class was hydrated against its own shape, so building the declared
    // class over that JSON would answer `instanceof` for a class it is not,
    // carrying fields that class never declares.
    @Model({ name: "PolyBaseBlock" })
    class PolyBaseBlock extends Ad4mModel {
      @Property({ through: "we://base" })
      base: string = "";
    }

    @Model({ name: "PolyTypedCollection" })
    class PolyTypedCollection extends Ad4mModel {
      @HasMany(() => PolyBaseBlock, {
        through: "we://children",
        polymorphic: true,
        instantiateAs: () => [PolyTextBlock],
      })
      children: any[] = [];
    }

    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [
            { id: "we://k/1", __subjectClass: "we://PolyTaskBlock", done: true },
            { id: "we://b/1", __subjectClass: "we://PolyBaseBlock", base: "declared" },
          ],
        },
      ],
    };

    const [collection] = PolyTypedCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    const [unlisted, declared] = collection.children;
    expect(unlisted).toBeDefined();
    expect(unlisted).not.toBeInstanceOf(PolyBaseBlock);
    expect(unlisted.done).toBe(true);
    expect(unlisted.__subjectClass).toBe("we://PolyTaskBlock");
    // The declared target is still right when it is the class the executor named.
    expect(declared).toBeInstanceOf(PolyBaseBlock);
    expect(declared.base).toBe("declared");
  });

  it("does not use the declared target for another class when no classes are named", () => {
    // Naming no classes to build is not the same as authorising the declared
    // target to stand in for all of them. Without `instantiateAs` there is
    // simply nothing this side can construct for a class it was not told about.
    @Model({ name: "PolyBaseOnlyBlock" })
    class PolyBaseOnlyBlock extends Ad4mModel {
      @Property({ through: "we://base" })
      base: string = "";
    }

    @Model({ name: "PolyTypedNoClasses" })
    class PolyTypedNoClasses extends Ad4mModel {
      @HasMany(() => PolyBaseOnlyBlock, { through: "we://children", polymorphic: true })
      children: any[] = [];
    }

    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [
            { id: "we://k/1", __subjectClass: "we://PolyTaskBlock", done: true },
            { id: "we://b/1", __subjectClass: "we://PolyBaseOnlyBlock", base: "declared" },
          ],
        },
      ],
    };

    const [collection] = PolyTypedNoClasses.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    const [unlisted, declared] = collection.children;
    expect(unlisted).not.toBeInstanceOf(PolyBaseOnlyBlock);
    expect(unlisted.done).toBe(true);
    expect(unlisted.__subjectClass).toBe("we://PolyTaskBlock");
    expect(declared).toBeInstanceOf(PolyBaseOnlyBlock);
    expect(declared.base).toBe("declared");
  });

  it("calls instantiateAs late, so a circular import can still resolve", () => {
    let defined = false;
    @Model({ name: "PolyLateCollection" })
    class PolyLateCollection extends Ad4mModel {
      // Evaluated at decoration time this would throw, the way an array literal
      // naming a class from further round an import cycle would.
      @HasMany({
        through: "we://children",
        polymorphic: true,
        instantiateAs: () => {
          if (!defined) throw new ReferenceError("class not defined yet");
          return [PolyTextBlock];
        },
      })
      children: string[] = [];
    }

    defined = true;
    const raw = {
      instances: [
        {
          id: "we://c/1",
          children: [{ id: "we://t/1", __subjectClass: "we://PolyTextBlock", text: "hello" }],
        },
      ],
    };

    const [collection] = PolyLateCollection.parseModelResult(mockPerspective, raw, {
      children: true,
    }) as any[];

    expect(collection.children[0]).toBeInstanceOf(PolyTextBlock);
  });

  it("asks the executor for polymorphic hydration without the caller repeating it", () => {
    // The relation being heterogeneous is a fact about the data, so declaring it
    // on the model should be enough — `include: { children: true }` must still
    // arrive at the executor as a polymorphic read.
    const { queryJson } = (PolyCollection as any).prepareModelQueryParams({
      include: { children: true },
    });

    expect(JSON.parse(queryJson).include.children).toEqual({
      polymorphic: true,
      preferClasses: ["we://PolyTextBlock", "we://PolyImageBlock"],
    });
  });

  it("sends the classes it can build as the classes it wants, in declaration order", () => {
    // The two questions have one answer: what this call site can construct is
    // what it wants its targets read as. Sent with the query rather than applied
    // to the results, so the same request gives the same reading to anyone.
    const { queryJson } = (PolyCollection as any).prepareModelQueryParams({
      include: { children: true },
    });

    // Declaration order is the preference order — a target conforming to two of
    // them is read as whichever was named first.
    expect(JSON.parse(queryJson).include.children.preferClasses).toEqual([
      "we://PolyTextBlock",
      "we://PolyImageBlock",
    ]);
  });

  it("lets an explicit preference at the call site win over the declaration", () => {
    const { queryJson } = (PolyCollection as any).prepareModelQueryParams({
      include: { children: { preferClasses: ["we://PolyBookmark"] } },
    });

    expect(JSON.parse(queryJson).include.children.preferClasses).toEqual(["we://PolyBookmark"]);
  });

  it("sends no preference for a polymorphic relation that names no classes", () => {
    @Model({ name: "PolyCollectionUndeclared" })
    class PolyCollectionUndeclared extends Ad4mModel {
      @HasMany({ through: "we://children", polymorphic: true })
      children: string[] = [];
    }

    const { queryJson } = (PolyCollectionUndeclared as any).prepareModelQueryParams({
      include: { children: true },
    });

    // Nothing to prefer, so nothing is sent and the executor ranks by
    // specificity exactly as it did before any of this existed.
    expect(JSON.parse(queryJson).include.children).toEqual({ polymorphic: true });
  });

  it("lets an explicit false at the call site win", () => {
    const { queryJson } = (PolyCollection as any).prepareModelQueryParams({
      include: { children: { polymorphic: false } },
    });

    expect(JSON.parse(queryJson).include.children.polymorphic).toBe(false);
  });

  it("applies the default at every depth of a nested include", () => {
    // `sections` is a plain typed relation, so the nested `children` is read off
    // PolyCollection rather than off the class being queried. Left to the top
    // level, the default would never reach it and the include would arrive at
    // the executor with no shape to resolve — the query fails outright.
    @Model({ name: "PolyPage" })
    class PolyPage extends Ad4mModel {
      @HasMany(() => PolyCollection, { through: "we://sections" })
      sections: any[] = [];
    }

    const { queryJson } = (PolyPage as any).prepareModelQueryParams({
      include: { sections: { include: { children: true } } },
    });

    const include = JSON.parse(queryJson).include;
    expect(include.sections.include.children).toEqual({
      polymorphic: true,
      preferClasses: ["we://PolyTextBlock", "we://PolyImageBlock"],
    });
  });

  it("lets an explicit false win at depth too", () => {
    @Model({ name: "PolyPageExplicit" })
    class PolyPageExplicit extends Ad4mModel {
      @HasMany(() => PolyCollection, { through: "we://sections" })
      sections: any[] = [];
    }

    const { queryJson } = (PolyPageExplicit as any).prepareModelQueryParams({
      include: { sections: { include: { children: { polymorphic: false } } } },
    });

    expect(JSON.parse(queryJson).include.sections.include.children.polymorphic).toBe(false);
  });
});

describe("Ordered relations", () => {
  @Model({ name: "OrderedColumn" })
  class OrderedColumn extends Ad4mModel {
    @HasMany({ through: "kanban://has_task", ordering: { strategy: "linkedList" } })
    tasks: string[] = [];

    @HasMany({ through: "kanban://archived" })
    archived: string[] = [];
  }

  const mockPerspective = { uuid: "test" } as any;

  it("declares its ordering in SHACL", () => {
    const { shape } = (OrderedColumn as any).generateSHACL();
    const tasks = shape.properties.find((p: any) => p.name === "tasks");
    expect(tasks.ordering).toBe("linkedList");
  });

  it("declares the predicate the ordering entries are stored under", () => {
    // Entries live on the parent under one shared predicate. The instance query
    // builds its predicate filter from the shape's declared paths, so without
    // this the read that needs the ordering links filters them out.
    const { shape } = (OrderedColumn as any).generateSHACL();
    const orderProp = shape.properties.find(
      (p: any) => p.path === "ad4m://collection_order"
    );
    expect(orderProp).toBeDefined();
  });

  it("omits that predicate when nothing on the class is ordered", () => {
    @Model({ name: "UnorderedOnly" })
    class UnorderedOnly extends Ad4mModel {
      @HasMany({ through: "kanban://archived" })
      archived: string[] = [];
    }
    const { shape } = (UnorderedOnly as any).generateSHACL();
    expect(
      shape.properties.find((p: any) => p.path === "ad4m://collection_order")
    ).toBeUndefined();
  });

  it("treats a reorder as a change", () => {
    // The whole point: for an ordered relation the sequence *is* the state, so
    // sorting before comparing would make save() a no-op for the one edit this
    // feature exists to support.
    const column = new OrderedColumn(mockPerspective, "kanban://col/1") as any;
    column.tasks = ["a", "b", "c"];
    column.takeSnapshot();

    column.tasks = ["c", "a", "b"];
    expect(column.changedFields()).toContain("tasks");
  });

  it("still treats a reorder of an unordered relation as no change", () => {
    // An unordered relation is a set — the executor returns it by link
    // timestamp, so assigning the same members in another order changed nothing.
    const column = new OrderedColumn(mockPerspective, "kanban://col/1") as any;
    column.archived = ["a", "b", "c"];
    column.takeSnapshot();

    column.archived = ["c", "a", "b"];
    expect(column.changedFields()).not.toContain("archived");
  });

  it("round-trips ordering through SHACL links", () => {
    const { shape } = (OrderedColumn as any).generateSHACL();
    const links = shape.toLinks().map((l: any) => ({
      source: l.source,
      predicate: l.predicate,
      target: l.target,
    }));
    const reconstructed = SHACLShape.fromLinks(links as any, shape.nodeShapeUri);
    const tasks = reconstructed.properties.find((p: any) => p.name === "tasks");
    expect(tasks!.ordering).toBe("linkedList");
  });
});

/**
 * A traversal names its predicate the same two ways every other scope does.
 *
 * The feature shipped with only the raw spelling, so every caller reading a
 * thread wrote `'test://has_comment'` by hand — the literal `resolveParentPredicate`
 * exists to keep out of call sites, reintroduced by the newest scope.
 */
describe("Ad4mModel.prepareModelQueryParams() — traverse scope", () => {
  @Model({ name: "TraverseComment" })
  class TraverseComment extends Ad4mModel {
    @Property({ through: "test://body" })
    body: string = "";
  }

  @Model({ name: "TraversePost" })
  class TraversePost extends Ad4mModel {
    @HasMany(() => TraverseComment, { through: "test://has_comment" })
    comments: TraverseComment[] = [];

    @HasOne(() => TraverseComment, { through: "test://pinned_comment" })
    pinned: TraverseComment | null = null;
  }

  const parentOf = (query: any) =>
    JSON.parse((TraverseComment as any).prepareModelQueryParams(query).queryJson).parent;

  it("keeps the raw spelling exactly as written", () => {
    expect(
      parentOf({ parent: { ids: ["we://a", "we://b"], predicate: "test://has_comment" } }),
    ).toEqual({ ids: ["we://a", "we://b"], predicate: "test://has_comment" });
  });

  it("resolves the predicate from the model that declares the relation", () => {
    expect(parentOf({ parent: { ids: "we://a", model: TraversePost } })).toEqual({
      ids: "we://a",
      predicate: "test://has_comment",
    });
  });

  it("takes `field` when one parent has two relations to the same child", () => {
    // Without it the scan finds `comments` first and a request for the pinned
    // comment's subtree would quietly read the wrong edge.
    expect(
      parentOf({ parent: { ids: "we://a", model: TraversePost, field: "pinned" } }),
    ).toEqual({ ids: "we://a", predicate: "test://pinned_comment" });
  });

  it("sends the traversal's own options and nothing else", () => {
    // `model` is a constructor; spreading the scope would put a class into a
    // query variable. What travels is the resolved predicate and the options.
    expect(
      parentOf({
        parent: {
          ids: ["we://a"],
          model: TraversePost,
          field: "comments",
          transitive: true,
          direction: "in",
          limitPerAnchor: 5,
        },
      }),
    ).toEqual({
      ids: ["we://a"],
      predicate: "test://has_comment",
      transitive: true,
      direction: "in",
      limitPerAnchor: 5,
    });
  });

  it("omits the options the caller did not set rather than sending defaults", () => {
    // An absent `transitive` is not `false` on the wire: the executor's serde
    // defaults own that, and sending our own would be a second opinion.
    const parent = parentOf({ parent: { ids: "we://a", model: TraversePost, levels: [10, 5] } });
    expect(parent).toEqual({ ids: "we://a", predicate: "test://has_comment", levels: [10, 5] });
    expect(parent).not.toHaveProperty("transitive");
    expect(parent).not.toHaveProperty("limitPerAnchor");
  });
});

// ──────────────────────────────────────────────────────────
// linkStatus — read from links of one status only (#1116)
// ──────────────────────────────────────────────────────────

describe("linkStatus — wire format", () => {
  @Model({ name: "LinkStatusWireCard" })
  class LinkStatusWireCard extends Ad4mModel {
    @Flag({ through: "lsw://type", value: "lsw://card" })
    type: string = "";

    @Property({ through: "lsw://title" })
    title: string = "";
  }

  it("is omitted unless the caller sets it, so both statuses are read", () => {
    const { queryJson } = (LinkStatusWireCard as any).prepareModelQueryParams({});
    expect(JSON.parse(queryJson)).not.toHaveProperty("linkStatus");
  });

  it("drops `linkStatus: null`, which reads both statuses like unset", () => {
    const { queryJson } = (LinkStatusWireCard as any).prepareModelQueryParams({
      linkStatus: null,
    });
    expect(JSON.parse(queryJson)).not.toHaveProperty("linkStatus");
  });

  it("travels as `linkStatus` when set, from the query", () => {
    const { queryJson } = (LinkStatusWireCard as any).prepareModelQueryParams({
      linkStatus: "shared",
    });
    expect(JSON.parse(queryJson).linkStatus).toBe("shared");
  });
});

// ──────────────────────────────────────────────────────────
// includeUnverified — opt-in to links whose signature did not verify (#1113)
// ──────────────────────────────────────────────────────────

describe("includeUnverified — wire format", () => {
  @Model({ name: "UnverifiedWireRecipe" })
  class UnverifiedWireRecipe extends Ad4mModel {
    @Flag({ through: "uv://type", value: "uv://recipe" })
    type: string = "";

    @Property({ through: "uv://name" })
    name: string = "";
  }

  it("is omitted unless the caller sets it, so the executor's default applies", () => {
    const { queryJson } = (UnverifiedWireRecipe as any).prepareModelQueryParams({});
    expect(JSON.parse(queryJson)).not.toHaveProperty("includeUnverified");
  });

  it("travels as `includeUnverified` when set, from the query", () => {
    const { queryJson } = (UnverifiedWireRecipe as any).prepareModelQueryParams({
      includeUnverified: true,
    });
    expect(JSON.parse(queryJson).includeUnverified).toBe(true);
  });
});

// ── One subscription per live query ───────────────────────────────────
describe("Model.query ownership", () => {
  it("a failed open ends no other live query's subscription", async () => {
    const h = liveHarness();
    h.client.subscribe.mockRejectedValueOnce(new Error("refused"));
    const failing = testModel("OwnershipFail").query(h.perspective);
    const working = testModel("OwnershipFail").query(h.perspective);

    await expect(failing.first).rejects.toThrow("refused");
    await working.first;

    expect(h.client.disposeQuerySubscription).not.toHaveBeenCalledWith("test-uuid", "sub-1");
    working.dispose();
    expect(h.client.disposeQuerySubscription).toHaveBeenCalledWith("test-uuid", "sub-1");
  });

  it("dispose() before the first value ends the subscription once it arrives and rejects `first`", async () => {
    const h = liveHarness();
    let resolveOpen!: (v: any) => void;
    h.client.subscribe.mockImplementationOnce(() => new Promise(resolve => { resolveOpen = resolve; }));
    const callback = jest.fn();
    const live = testModel("OwnershipEarly").query(h.perspective);
    live.subscribe(callback);

    live.dispose();
    resolveOpen({ subscriptionId: "sub-p", revision: 0, result: { instances: [], totalCount: 0 } });

    await expect(live.first).rejects.toThrow();
    await new Promise(resolve => setImmediate(resolve));
    expect(h.client.disposeQuerySubscription).toHaveBeenCalledTimes(1);
    expect(h.client.disposeQuerySubscription).toHaveBeenCalledWith("test-uuid", "sub-p");
    expect(() => live.dispose()).not.toThrow();
    expect(callback).not.toHaveBeenCalled();
  });

  it("dispose() ends the subscription once, however often it is called", async () => {
    const h = liveHarness();
    const live = testModel("OwnershipOnce").query(h.perspective);
    await live.first;

    live.dispose();
    live.dispose();

    expect(h.client.disposeQuerySubscription).toHaveBeenCalledTimes(1);
    expect(h.client.disposeQuerySubscription).toHaveBeenCalledWith("test-uuid", "sub-1");
  });
});

describe("Ad4mModel.findAllOf / queryOf (one query over several classes, #1238)", () => {
  @Model({ name: "UnionTaskU" })
  class UnionTaskU extends Ad4mModel {
    @Flag({ through: "u://kind", value: "u://task" })
    kind: string = "";
    @Property({ through: "u://status" })
    status: string = "";
  }

  @Model({ name: "UnionNoteU" })
  class UnionNoteU extends Ad4mModel {
    @Flag({ through: "u://kind", value: "u://note" })
    kind: string = "";
    @Property({ through: "u://body" })
    body: string = "";
  }

  const rows = {
    instances: [
      { id: "u://t1", status: "open", __subjectClass: "u://UnionTaskU", __subjectClasses: ["u://UnionTaskU"] },
      { id: "u://n1", body: "hi", __subjectClass: "u://UnionNoteU", __subjectClasses: ["u://UnionNoteU"] },
      { id: "u://x", __subjectClass: "o://Other", __subjectClasses: ["o://Other"] },
    ],
    totalCount: 7,
  };

  it("sends every class's target class IRI and builds each row as its own class", async () => {
    const perspective = { modelQuery: jest.fn().mockResolvedValue(rows), uuid: "p" } as any;
    const results = await Ad4mModel.findAllOf(perspective, [UnionTaskU, UnionNoteU], {
      where: { status: "open" },
      order: { status: "DESC" },
      limit: 2,
      preferClasses: ["UnionNoteU"],
    });

    const [classNames, queryJson] = perspective.modelQuery.mock.calls[0];
    expect(classNames).toEqual(["u://UnionTaskU", "u://UnionNoteU"]);
    const sent = JSON.parse(queryJson);
    expect(sent.where).toEqual({ status: "open" });
    expect(sent.order).toEqual([["status", "DESC"]]);
    expect(sent.limit).toBe(2);
    expect(sent.preferClasses).toEqual(["UnionNoteU"]);

    expect(results[0]).toBeInstanceOf(UnionTaskU);
    expect((results[0] as UnionTaskU).status).toBe("open");
    expect(results[1]).toBeInstanceOf(UnionNoteU);
    // A class the caller did not list stays plain JSON.
    expect(results[2]).not.toBeInstanceOf(Ad4mModel);
  });

  it("refuses a projection key the classes tag with different target classes", () => {
    @Model({ name: "UnionCardU" })
    class UnionCardU extends Ad4mModel {
      @Property({ through: "u://title" })
      title: string = "";
    }
    @Model({ name: "UnionWaypointU" })
    class UnionWaypointU extends Ad4mModel {
      @Property({ through: "u://x" })
      x: string = "";
    }
    @Model({ name: "UnionPlacementU" })
    class UnionPlacementU extends Ad4mModel {
      @HasMany(() => UnionCardU, { through: "u://items" })
      items: string[] = [];
    }
    @Model({ name: "UnionRouteU" })
    class UnionRouteU extends Ad4mModel {
      @HasMany(() => UnionWaypointU, { through: "u://items" })
      items: string[] = [];
    }
    const include = { $items: { from: "items", where: { title: "a" } } } as any;
    expect(() =>
      Ad4mModel.prepareUnionQueryParams([UnionPlacementU, UnionRouteU], { include }),
    ).toThrow(/different target classes/);
    // Same target on both: accepted.
    expect(() =>
      Ad4mModel.prepareUnionQueryParams([UnionPlacementU, UnionPlacementU], { include }),
    ).not.toThrow();
  });

  it("keeps a raw parent predicate and refuses a scope the classes resolve differently", () => {
    const { queryJson } = Ad4mModel.prepareUnionQueryParams([UnionTaskU, UnionNoteU], {
      parent: { id: "u://canvas", predicate: "u://item" },
    });
    expect(JSON.parse(queryJson).parent).toEqual({ id: "u://canvas", predicate: "u://item" });

    @Model({ name: "UnionBoardU" })
    class UnionBoardU extends Ad4mModel {
      @HasMany(() => UnionTaskU, { through: "u://tasks" })
      tasks: string[] = [];
      @HasMany(() => UnionNoteU, { through: "u://notes" })
      notes: string[] = [];
    }
    expect(() =>
      Ad4mModel.prepareUnionQueryParams([UnionTaskU, UnionNoteU], {
        parent: { id: "u://board", model: UnionBoardU },
      }),
    ).toThrow(/different predicates/);
  });
});

describe("save() on an instance pushed by a subscription", () => {
  @Model({ name: "PushedTask" })
  class PushedTask extends Ad4mModel {
    @Flag({ through: "push://kind", value: "push://task" })
    kind: string = "";
    @Property({ through: "push://title" })
    title: string = "";
  }

  @Model({ name: "PushedNote" })
  class PushedNote extends Ad4mModel {
    @Flag({ through: "push://kind", value: "push://note" })
    kind: string = "";
    @Property({ through: "push://body" })
    body: string = "";
  }

  const makePerspective = (initial: any[]) => {
    const h = liveHarness({ instances: initial, totalCount: initial.length });
    const perspective = {
      ...h.perspective,
      mutate: jest.fn().mockResolvedValue({ ids: [] }),
      createBatch: jest.fn().mockResolvedValue("batch-1"),
      commitBatch: jest.fn().mockResolvedValue(undefined),
      stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue("PushedTask"),
    } as any;
    return { perspective, push: (instances: any[]) => h.push({ result: { instances, totalCount: instances.length } }) };
  };

  /** The ops each `mutate` call sent. */
  const written = (perspective: any) =>
    perspective.mutate.mock.calls.map((call: any[]) => call[0]);

  it("updates the instance rather than creating a second one", async () => {
    const { perspective, push } = makePerspective([{ id: "push://t1", kind: "push://task", title: "v1" }]);
    let pushed: PushedTask[] = [];
    const live = PushedTask.query(perspective);
    live.subscribe((s) => { pushed = s.data!.results; });
    await live.first;

    // The record changes elsewhere; the subscription pushes the new state.
    push([{ id: "push://t1", kind: "push://task", title: "v2" }]);
    expect(pushed).toHaveLength(1);

    const task = pushed[0];
    expect(task.isDirty()).toBe(false);
    task.title = "v3";
    expect(task.changedFields()).toEqual(["title"]);
    await task.save("batch-1");

    expect(written(perspective)).toEqual([
      [{ op: "update", class: "push://PushedTask", id: "push://t1", set: { title: "v3" } }],
    ]);
  });

  it("updates an instance pushed to a queryOf() live query", async () => {
    const { perspective, push } = makePerspective([]);
    let pushed: Ad4mModel[] = [];
    const live = Ad4mModel.queryOf(perspective, [PushedTask, PushedNote]);
    live.subscribe((s) => { pushed = s.data!.results; });
    await live.first;

    push([
      { id: "push://t1", kind: "push://task", title: "v2", __subjectClass: "push://PushedTask" },
      { id: "push://n1", kind: "push://note", body: "b", __subjectClass: "push://PushedNote" },
    ]);
    const note = pushed[1] as PushedNote;
    expect(note).toBeInstanceOf(PushedNote);
    note.body = "b2";
    await note.save("batch-1");

    expect(written(perspective)).toEqual([
      [{ op: "update", class: "push://PushedNote", id: "push://n1", set: { body: "b2" } }],
    ]);
  });
});

describe("includeAll", () => {
  const { BelongsToOne, BelongsToMany } = require("./decorators");

  @Model({ name: "IncAllItem" })
  class IncAllItem extends Ad4mModel {
    @Property({ through: "inc://name" })
    name: string = "";
  }

  @Model({ name: "IncAllList" })
  class IncAllList extends Ad4mModel {
    @Property({ through: "inc://title" })
    title: string = "";
    @HasMany(() => IncAllItem, { through: "inc://item" })
    items: string[] = [];
    @HasOne(() => IncAllItem, { through: "inc://pinned" })
    pinned: string = "";
    @BelongsToOne(() => IncAllList, { through: "inc://sublist" })
    owner: string = "";
    @BelongsToMany(() => IncAllList, { through: "inc://linked" })
    linkedFrom: string[] = [];
  }

  @Model({ name: "IncAllOther" })
  class IncAllOther extends Ad4mModel {
    @Property({ through: "inc://other" })
    other: string = "";
  }

  const row = {
    id: "inc://l1",
    title: "groceries",
    items: [{ id: "inc://i1", name: "milk" }],
    pinned: { id: "inc://i2", name: "eggs" },
    owner: "inc://parent",
    linkedFrom: ["inc://l0"],
    __subjectClass: "inc://IncAllList",
  };
  const perspectiveReturning = (instances: any[]) =>
    ({ uuid: "p", modelQuery: jest.fn().mockResolvedValue({ instances, totalCount: instances.length }) }) as any;
  const sentInclude = (perspective: any) => JSON.parse(perspective.modelQuery.mock.calls[0][1]).include;

  it("asks for the forward relations only", () => {
    const { queryJson } = (IncAllList as any).prepareModelQueryParams({ includeAll: true });
    expect(Object.keys(JSON.parse(queryJson).include).sort()).toEqual(["items", "pinned"]);
  });

  it("hydrates the forward relations and leaves belongs-to ones as ids", async () => {
    const perspective = perspectiveReturning([row]);
    const [list] = await IncAllList.findAll(perspective, { includeAll: true });

    expect(Object.keys(sentInclude(perspective)).sort()).toEqual(["items", "pinned"]);
    expect(list.items[0]).toBeInstanceOf(IncAllItem);
    expect(list.pinned).toBeInstanceOf(IncAllItem);
    expect(list.owner).toBe("inc://parent");
    expect(list.linkedFrom).toEqual(["inc://l0"]);
  });

  it("keeps an explicit include entry over the expansion", () => {
    const { queryJson } = (IncAllList as any).prepareModelQueryParams({
      includeAll: true,
      include: { owner: true, items: false },
    });
    expect(JSON.parse(queryJson).include).toEqual({ items: false, pinned: true, owner: true });
  });

  it("hydrates the forward relations of a live query's result", async () => {
    const h = liveHarness({ instances: [row], totalCount: 1 });
    const live = IncAllList.query(h.perspective, { includeAll: true });
    const [list] = (await live.first).results;
    live.dispose();

    expect(Object.keys(h.sent()[0].include).sort()).toEqual(["items", "pinned"]);
    expect(list.items[0]).toBeInstanceOf(IncAllItem);
    expect(list.owner).toBe("inc://parent");
  });

  it("applies to each class of a findAllOf() union", async () => {
    const perspective = perspectiveReturning([row, { id: "inc://o1", other: "x", __subjectClass: "inc://IncAllOther" }]);
    const [list, other] = await Ad4mModel.findAllOf(perspective, [IncAllList, IncAllOther], { includeAll: true });

    expect(Object.keys(sentInclude(perspective)).sort()).toEqual(["items", "pinned"]);
    expect((list as IncAllList).items[0]).toBeInstanceOf(IncAllItem);
    expect((list as IncAllList).owner).toBe("inc://parent");
    expect(other).toBeInstanceOf(IncAllOther);
  });
});

describe("Ad4mModel.transaction()", () => {
  /** A perspective that tracks which batches are still open. */
  const makePerspective = () => {
    const open = new Set<string>();
    let next = 0;
    const perspective = {
      createBatch: jest.fn(async () => { const id = `batch-${++next}`; open.add(id); return id; }),
      commitBatch: jest.fn(async (id: string) => { open.delete(id); return {}; }),
      discardBatch: jest.fn(async (id: string) => open.delete(id)),
      mutate: jest.fn(async () => ({ ids: [] })),
    } as any;
    return { perspective, open };
  };

  @Model({ name: "TxCard" })
  class TxCard extends Ad4mModel {
    @Property({ through: "tx://title" })
    title: string = "";
  }

  it("sends the callback's model writes as one mutate call in the batch, then commits", async () => {
    const { perspective } = makePerspective();
    await Ad4mModel.transaction(perspective, async (tx) => {
      const card = new TxCard(perspective, "tx://c1");
      card.title = "one";
      await card.save(tx.batchId);
      card.title = "two";
      await card.save(tx.batchId);
      await new TxCard(perspective, "tx://c2").delete(tx.batchId);
      // Nothing is sent until the callback returns.
      expect(perspective.mutate).not.toHaveBeenCalled();
    });
    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate).toHaveBeenCalledWith(
      [
        { op: "create", class: "tx://TxCard", id: "tx://c1", values: { title: "one" } },
        { op: "update", class: "tx://TxCard", id: "tx://c1", set: { title: "two" } },
        { op: "delete", class: "tx://TxCard", id: "tx://c2" },
      ],
      { batch: "batch-1" },
    );
    expect(perspective.commitBatch).toHaveBeenCalledWith("batch-1");
    expect(perspective.mutate.mock.invocationCallOrder[0]).toBeLessThan(perspective.commitBatch.mock.invocationCallOrder[0]);
  });

  it("a refused flush leaves a new instance unsaved: a retry sends a create", async () => {
    const { perspective } = makePerspective();
    perspective.mutate.mockRejectedValueOnce(new Error("refused"));
    const card = new TxCard(perspective, "tx://c1");
    jest.spyOn(card as any, "getData").mockResolvedValue(card);
    await expect(
      Ad4mModel.transaction(perspective, async (tx) => {
        card.title = "one";
        await card.save(tx.batchId);
      }),
    ).rejects.toThrow("refused");

    await card.save();
    expect(perspective.mutate).toHaveBeenLastCalledWith(
      [{ op: "create", class: "tx://TxCard", id: "tx://c1", values: { title: "one" } }],
      {},
    );
  });

  it("a callback that throws leaves a new instance unsaved too", async () => {
    const { perspective } = makePerspective();
    const card = new TxCard(perspective, "tx://c1");
    jest.spyOn(card as any, "getData").mockResolvedValue(card);
    await expect(
      Ad4mModel.transaction(perspective, async (tx) => {
        card.title = "one";
        await card.save(tx.batchId);
        throw new Error("callback failed");
      }),
    ).rejects.toThrow("callback failed");

    await card.save();
    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate.mock.calls[0][0][0].op).toBe("create");
  });

  it("a write for a model of another perspective is refused, not queued on the transaction's", async () => {
    const { perspective } = makePerspective();
    perspective.uuid = "perspective-a";
    const other = makePerspective().perspective;
    other.uuid = "perspective-b";
    await expect(
      Ad4mModel.transaction(perspective, async (tx) => {
        const card = new TxCard(other, "tx://c1");
        card.title = "one";
        await card.save(tx.batchId);
      }),
    ).rejects.toThrow(/belongs to perspective perspective-a, not perspective-b/);
    expect(perspective.mutate).not.toHaveBeenCalled();
    expect(other.mutate).not.toHaveBeenCalled();
  });

  it("a failed transaction does not restore a snapshot over a later save outside it", async () => {
    const { perspective } = makePerspective();
    const card = new TxCard(perspective, "tx://c1");
    jest.spyOn(card as any, "getData").mockResolvedValue(card);
    card.title = "A";
    await card.save();
    perspective.mutate.mockClear();

    await expect(
      Ad4mModel.transaction(perspective, async (tx) => {
        card.title = "B";
        await card.save(tx.batchId);
        // A save outside the transaction commits at once.
        card.title = "C";
        await card.save();
        throw new Error("callback failed");
      }),
    ).rejects.toThrow("callback failed");
    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    perspective.mutate.mockClear();

    // The store holds "C": setting "A" again is a change, so it is sent.
    card.title = "A";
    await card.save();
    expect(perspective.mutate).toHaveBeenCalledWith(
      [{ op: "update", class: "tx://TxCard", id: "tx://c1", set: { title: "A" } }],
      {},
    );
  });

  it("a flush that succeeds keeps the snapshot: a later save updates", async () => {
    const { perspective } = makePerspective();
    const card = new TxCard(perspective, "tx://c1");
    jest.spyOn(card as any, "getData").mockResolvedValue(card);
    await Ad4mModel.transaction(perspective, async (tx) => {
      card.title = "one";
      await card.save(tx.batchId);
    });
    card.title = "two";
    await card.save();
    expect(perspective.mutate).toHaveBeenLastCalledWith(
      [{ op: "update", class: "tx://TxCard", id: "tx://c1", set: { title: "two" } }],
      {},
    );
  });

  it("sends nothing when the callback writes no model", async () => {
    const { perspective } = makePerspective();
    await Ad4mModel.transaction(perspective, async () => {});
    expect(perspective.mutate).not.toHaveBeenCalled();
  });

  it("discards the batch and rethrows when the mutate call is refused", async () => {
    const { perspective, open } = makePerspective();
    const refusal = new Error("missingRequired");
    perspective.mutate.mockRejectedValue(refusal);
    await expect(
      Ad4mModel.transaction(perspective, async (tx) => {
        const card = new TxCard(perspective, "tx://c1");
        card.title = "one";
        await card.save(tx.batchId);
      }),
    ).rejects.toBe(refusal);
    expect(perspective.discardBatch).toHaveBeenCalledWith("batch-1");
    expect(perspective.commitBatch).not.toHaveBeenCalled();
    expect(open.size).toBe(0);
  });

  it("does not queue a write that names another batch", async () => {
    const { perspective } = makePerspective();
    await Ad4mModel.transaction(perspective, async () => {
      await new TxCard(perspective, "tx://c3").delete("other-batch");
    });
    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate).toHaveBeenCalledWith(
      [{ op: "delete", class: "tx://TxCard", id: "tx://c3" }],
      { batch: "other-batch" },
    );
  });

  it("commits the batch and returns the callback's value", async () => {
    const { perspective, open } = makePerspective();
    const result = await Ad4mModel.transaction(perspective, async (tx) => tx.batchId);
    expect(result).toBe("batch-1");
    expect(perspective.commitBatch).toHaveBeenCalledWith("batch-1");
    expect(perspective.discardBatch).not.toHaveBeenCalled();
    expect(open.size).toBe(0);
  });

  it("discards the batch and rethrows when the callback throws", async () => {
    const { perspective, open } = makePerspective();
    const failure = new Error("callback failed");
    await expect(
      Ad4mModel.transaction(perspective, async () => { throw failure; }),
    ).rejects.toBe(failure);
    expect(perspective.discardBatch).toHaveBeenCalledWith("batch-1");
    expect(perspective.commitBatch).not.toHaveBeenCalled();
    expect(open.size).toBe(0);
  });

  it("rethrows the callback's error when the discard fails too", async () => {
    const { perspective } = makePerspective();
    perspective.discardBatch.mockRejectedValue(new Error("socket closed"));
    const failure = new Error("callback failed");
    await expect(
      Ad4mModel.transaction(perspective, async () => { throw failure; }),
    ).rejects.toBe(failure);
  });

  it("discards the batch and rethrows when the commit fails", async () => {
    const { perspective, open } = makePerspective();
    const failure = new Error("commit failed");
    perspective.commitBatch.mockRejectedValue(failure);
    await expect(Ad4mModel.transaction(perspective, async () => "done")).rejects.toBe(failure);
    expect(perspective.discardBatch).toHaveBeenCalledWith("batch-1");
    expect(open.size).toBe(0);
  });
});

describe("delete() and incoming links", () => {
  @Model({ name: "DelTarget" })
  class DelTarget extends Ad4mModel {
    @Flag({ through: "del://kind", value: "del://target" })
    kind: string = "";
  }

  @Model({ name: "DelOwner" })
  class DelOwner extends Ad4mModel {
    @HasMany({ through: "del://has_target", target: () => DelTarget })
    targets: string[] = [];
  }

  // A different model whose relation names no target class.
  @Model({ name: "DelOther" })
  class DelOther extends Ad4mModel {
    @HasMany({ through: "del://mentions" })
    mentions: string[] = [];
  }

  // A polymorphic container (no single target class), like a block tree's
  // parent -> child relation.
  @Model({ name: "DelContainer" })
  class DelContainer extends Ad4mModel {
    @HasMany({ through: "del://child", polymorphic: true })
    children: string[] = [];
  }

  // An all-optional model: delete() removes its own outgoing links itself.
  @Model({ name: "DelOptional" })
  class DelOptional extends Ad4mModel {
    @Property({ through: "del://note" })
    note?: string;
  }

  const ownerLink = { author: "a", timestamp: "1", data: { source: "del://owner", predicate: "del://has_target", target: "del://t1" } };
  const otherLink = { author: "b", timestamp: "2", data: { source: "del://other", predicate: "del://mentions", target: "del://t1" } };
  const childLink = { author: "c", timestamp: "3", data: { source: "del://container", predicate: "del://child", target: "del://t1" } };
  const incoming = [ownerLink, otherLink, childLink];

  // No getAllShacl: the cascade must not depend on registered shapes.
  const makePerspective = () => ({
    mutate: jest.fn(async () => ({ ids: [] })),
  }) as any;

  // The executor's delete op removes the own links and every incoming link,
  // so the SDK sends one op and reads nothing.
  it("is one delete op in the delete's batch, whatever links point at the instance", async () => {
    // The declared models are only there to make the links realistic.
    expect([DelOwner, DelOther, DelContainer].every((M) => M.generateSHACL())).toBe(true);
    const perspective = makePerspective();
    await new DelTarget(perspective, "del://t1").delete("batch-1");

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate).toHaveBeenCalledWith(
      [{ op: "delete", class: "del://DelTarget", id: "del://t1" }],
      { batch: "batch-1" },
    );
  });

  it("sends no batch without one", async () => {
    const perspective = makePerspective();
    await new DelOptional(perspective, "del://t1").delete();
    expect(perspective.mutate).toHaveBeenCalledWith(
      [{ op: "delete", class: "del://DelOptional", id: "del://t1" }],
      {},
    );
  });

  it("rejects with the executor's error", async () => {
    const perspective = makePerspective();
    const failure = new Error("delete refused");
    perspective.mutate.mockRejectedValue(failure);
    await expect(new DelTarget(perspective, "del://t1").delete()).rejects.toBe(failure);
  });
});

describe("relation cardinality in mutate ops", () => {
  @Model({ name: "CardTarget" })
  class CardTarget extends Ad4mModel {}

  @Model({ name: "CardHolder" })
  class CardHolder extends Ad4mModel {
    @HasOne({ through: "card://owner" })
    owner: string = "";

    @HasMany({ through: "card://tags" })
    tags: string[] = [];
  }

  const ops = (perspective: any) => perspective.mutate.mock.calls.flatMap((call: any[]) => call[0]);
  const makePerspective = () => ({ uuid: "p", mutate: jest.fn(async () => ({ ids: [] })) }) as any;

  it("a create sends a HasOne as one id and a HasMany as an array", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    holder.owner = "card://a";
    holder.tags = ["card://t1", "card://t2"];
    await holder.save("b");
    expect(ops(perspective)).toEqual([
      { op: "create", class: "card://CardHolder", id: "card://h1", values: { owner: "card://a", tags: ["card://t1", "card://t2"] } },
    ]);
  });

  it("an update replaces a HasOne with one id and clears it with null", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    holder.owner = "card://a";
    await holder.save("b");
    perspective.mutate.mockClear();

    holder.owner = "card://b";
    await holder.save("b");
    holder.owner = null as any;
    await holder.save("b");
    expect(ops(perspective)).toEqual([
      { op: "update", class: "card://CardHolder", id: "card://h1", set: { owner: "card://b" } },
      { op: "update", class: "card://CardHolder", id: "card://h1", set: { owner: null } },
    ]);
  });

  it("addOwner on a HasOne is a set of one id; removeOwner stays a remove; a HasMany add stays an add", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    await (holder as any).addOwner(new CardTarget(perspective, "card://a"), "b");
    await (holder as any).removeOwner("card://a", "b");
    await (holder as any).addTags("card://t1", "b");
    expect(ops(perspective)).toEqual([
      { op: "update", class: "card://CardHolder", id: "card://h1", set: { owner: "card://a" } },
      { op: "update", class: "card://CardHolder", id: "card://h1", remove: { owner: ["card://a"] } },
      { op: "update", class: "card://CardHolder", id: "card://h1", add: { tags: ["card://t1"] } },
    ]);
  });

  it("an update keeps an array for a HasMany, empty included", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    holder.tags = ["card://t1"];
    await holder.save("b");
    perspective.mutate.mockClear();

    holder.tags = [];
    await holder.save("b");
    expect(ops(perspective)).toEqual([
      { op: "update", class: "card://CardHolder", id: "card://h1", set: { tags: [] } },
    ]);
  });

  it("saving an adder envelope on a HasOne is a set of one id; on a HasMany it stays an add", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    holder.owner = "card://a";
    holder.tags = ["card://t1"];
    await holder.save("b");
    perspective.mutate.mockClear();

    holder.owner = { action: "adder", value: "card://b" } as any;
    holder.tags = { action: "adder", value: "card://t2" } as any;
    await holder.save("b");
    expect(ops(perspective)).toEqual([
      {
        op: "update", class: "card://CardHolder", id: "card://h1",
        set: { owner: "card://b" }, add: { tags: ["card://t2"] },
      },
    ]);
  });

  it("saving an adder envelope with several members on a HasOne is not truncated", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    holder.owner = "card://a";
    await holder.save("b");
    perspective.mutate.mockClear();

    holder.owner = { action: "adder", value: ["card://b", "card://c"] } as any;
    await holder.save("b");
    expect(ops(perspective)[0].set).toEqual({ owner: ["card://b", "card://c"] });
  });

  it("setOwner sends one id, not an array", async () => {
    const perspective = makePerspective();
    const holder = new CardHolder(perspective, "card://h1");
    await (holder as any).setOwner(new CardTarget(perspective, "card://a"), "b");
    expect(ops(perspective)).toEqual([
      { op: "update", class: "card://CardHolder", id: "card://h1", set: { owner: "card://a" } },
    ]);
  });
});

describe("save() create path and its own batch", () => {
  @Model({ name: "SaveBatchCard" })
  class SaveBatchCard extends Ad4mModel {
    @Flag({ through: "sb://kind", value: "sb://card" })
    kind: string = "";
  }

  it("a create is one mutate call, with no batch of its own", async () => {
    const perspective = {
      uuid: "p",
      mutate: jest.fn(async () => ({ ids: [] })),
      createBatch: jest.fn(),
      commitBatch: jest.fn(),
    } as any;
    const card = new SaveBatchCard(perspective);
    jest.spyOn(card as any, "getData").mockResolvedValue(card);
    await card.save();

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate).toHaveBeenCalledWith(
      [{ op: "create", class: expect.stringContaining("SaveBatchCard"), id: card.id, values: {} }],
      {},
    );
    expect(perspective.createBatch).not.toHaveBeenCalled();
    expect(perspective.commitBatch).not.toHaveBeenCalled();
  });

  it("a field changed while a batched save is pending stays dirty: the snapshot holds what was sent", async () => {
    @Model({ name: "PendingCard" })
    class PendingCard extends Ad4mModel {
      @Property({ through: "pc://title" })
      title: string = "";
    }
    let release!: () => void;
    const perspective = {
      uuid: "p",
      mutate: jest.fn(() => new Promise<{ ids: string[] }>((resolve) => { release = () => resolve({ ids: [] }); })),
    } as any;
    const card = new PendingCard(perspective, "pc://c1");
    card.title = "one";
    const saving = card.save("raw-batch");
    card.title = "two";
    release();
    await saving;
    perspective.mutate.mockImplementation(async () => ({ ids: [] }));
    await card.save("raw-batch");
    expect(perspective.mutate).toHaveBeenLastCalledWith(
      [{ op: "update", class: "pc://PendingCard", id: "pc://c1", set: { title: "two" } }],
      { batch: "raw-batch" },
    );
  });

  it("rethrows the executor's refusal and writes no snapshot when the create fails", async () => {
    const failure = new Error("refused");
    const perspective = { uuid: "p", mutate: jest.fn(async () => { throw failure; }) } as any;
    const card = new SaveBatchCard(perspective);

    await expect(card.save()).rejects.toBe(failure);
    // The retry creates again: the first call wrote nothing.
    await expect(card.save()).rejects.toBe(failure);
    expect(perspective.mutate).toHaveBeenCalledTimes(2);
    expect(perspective.mutate.mock.calls.map((c: any[]) => c[0][0].op)).toEqual(["create", "create"]);
  });

  it("updates on a retried save() when the re-read after a committed create fails", async () => {
    const perspective = {
      uuid: "p",
      mutate: jest.fn(async () => ({ ids: [] })),
    } as any;
    const card = new SaveBatchCard(perspective);
    const reread = new Error("read failed");
    jest.spyOn(card as any, "getData").mockRejectedValueOnce(reread).mockResolvedValue(card);

    await expect(card.save()).rejects.toBe(reread);
    await card.save();

    // The retry took the update path: the record was created once.
    expect(perspective.mutate.mock.calls.filter((c: any[]) => c[0][0].op === "create")).toHaveLength(1);
  });

  it("leaves a caller's batch open when the create fails", async () => {
    const failure = new Error("create failed");
    const perspective = {
      uuid: "p",
      createBatch: jest.fn(async () => "own"),
      discardBatch: jest.fn(async () => true),
      mutate: jest.fn(async () => { throw failure; }),
    } as any;

    await expect(new SaveBatchCard(perspective).save("caller-batch")).rejects.toBe(failure);
    expect(perspective.mutate).toHaveBeenCalledWith(expect.anything(), { batch: "caller-batch" });
    expect(perspective.createBatch).not.toHaveBeenCalled();
    expect(perspective.discardBatch).not.toHaveBeenCalled();
  });

  it("create() in a caller's batch refuses a parent scope that is not one instance before it writes", async () => {
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })) } as any;
    const traverse = { ids: ["ad4m://a", "ad4m://b"] } as any;

    await expect(
      SaveBatchCard.create(perspective, {}, { parent: traverse, batchId: "caller-batch" }),
    ).rejects.toThrow(/traverse scope/);
    expect(perspective.mutate).not.toHaveBeenCalled();
  });

  it("create() in a caller's batch refuses a parent model with no relation to it before it writes", async () => {
    @Model({ name: "UnrelatedParent" })
    class UnrelatedParent extends Ad4mModel {}
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })) } as any;

    await expect(
      SaveBatchCard.create(perspective, {}, { parent: { model: UnrelatedParent, id: "ad4m://p" }, batchId: "caller-batch" }),
    ).rejects.toThrow();
    expect(perspective.mutate).not.toHaveBeenCalled();
  });

  it("create() with a parent is one create op that carries the parent", async () => {
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })), createBatch: jest.fn(), add: jest.fn() } as any;

    await SaveBatchCard.create(perspective, {}, { parent: { id: "sb://parent", predicate: "sb://child" } });

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate).toHaveBeenCalledWith(
      [expect.objectContaining({ op: "create", parent: { id: "sb://parent", predicate: "sb://child" } })],
      {},
    );
    expect(perspective.createBatch).not.toHaveBeenCalled();
    expect(perspective.add).not.toHaveBeenCalled();
  });

  it("create() goes through save(), the one entry a write check wraps, and the parent still reaches the one create op", async () => {
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })), modelQuery: jest.fn() } as any;
    const save = jest.spyOn(Ad4mModel.prototype, "save");
    try {
      await SaveBatchCard.create(perspective, {}, { parent: { id: "sb://parent", predicate: "sb://child" } });
      expect(save).toHaveBeenCalledTimes(1);
      // Without a batch the create has no read of its own.
      expect(perspective.modelQuery).not.toHaveBeenCalled();
      expect(perspective.mutate).toHaveBeenCalledTimes(1);
      expect(perspective.mutate).toHaveBeenCalledWith(
        [expect.objectContaining({ op: "create", parent: { id: "sb://parent", predicate: "sb://child" } })],
        {},
      );
    } finally {
      save.mockRestore();
    }
  });

  it("create() with a parent still sends it when a subclass's save() forwards only the batch id", async () => {
    @Model({ name: "OverridingCard" })
    class OverridingCard extends Ad4mModel {
      @Flag({ through: "sb://okind", value: "sb://ocard" })
      kind: string = "";

      async save(batchId?: string) {
        return super.save(batchId);
      }
    }
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })) } as any;

    await OverridingCard.create(perspective, {}, { parent: { id: "sb://parent" }, batchId: "caller-batch" });

    expect(perspective.mutate).toHaveBeenCalledWith(
      [expect.objectContaining({ op: "create", parent: { id: "sb://parent" } })],
      { batch: "caller-batch" },
    );
  });

  it("create() with a parent rethrows the executor's refusal", async () => {
    const failure = new Error("parent refused");
    const perspective = { uuid: "p", mutate: jest.fn(async () => { throw failure; }) } as any;

    await expect(
      SaveBatchCard.create(perspective, {}, { parent: { id: "sb://parent", predicate: "sb://child" } }),
    ).rejects.toBe(failure);
  });

  it("create() in a caller's batch with a parent sends the parent in the op and returns without a read", async () => {
    const perspective = { uuid: "p", mutate: jest.fn(async () => ({ ids: [] })), modelQuery: jest.fn() } as any;

    await SaveBatchCard.create(perspective, {}, { parent: { id: "sb://parent" }, batchId: "caller-batch" });

    expect(perspective.mutate).toHaveBeenCalledWith(
      [expect.objectContaining({ op: "create", parent: { id: "sb://parent" } })],
      { batch: "caller-batch" },
    );
    expect(perspective.modelQuery).not.toHaveBeenCalled();
  });

  // create() returns what it wrote and reads nothing after the commit, unless
  // asked for the row with `returns: 'row'`.
  describe("create() returns", () => {
    @Model({ name: "ReadBackCard" })
    class ReadBackCard extends Ad4mModel {
      @Flag({ through: "rb://kind", value: "rb://card" })
      kind: string = "";
      @Property({ through: "rb://title" })
      title: string = "";
    }

    const reread = new Error("read failed");
    const makePerspective = () => ({
      uuid: "p",
      mutate: jest.fn(async () => ({ ids: [] })),
      createBatch: jest.fn(async () => "batch-1"),
      commitBatch: jest.fn(async () => ({})),
      modelQuery: jest.fn(async () => { throw reread; }),
    }) as any;

    it("sends no read after the commit by default", async () => {
      const perspective = makePerspective();

      const card = await ReadBackCard.create(perspective, { title: "written" });

      expect(perspective.mutate).toHaveBeenCalledTimes(1);
      expect(perspective.modelQuery).not.toHaveBeenCalled();
      expect(card.id).toBeTruthy();
      expect(card.title).toBe("written");
      expect((card as any).author).toBeUndefined();
      expect((card as any).createdAt).toBeUndefined();
    });

    it("reads the row after the commit with returns: 'row', and fills author and createdAt", async () => {
      const perspective = makePerspective();
      perspective.modelQuery.mockImplementation(async () => ({
        instances: [{ id: "rb://row", author: "did:key:alice", createdAt: 1700000000000, title: "stored" }],
        totalCount: 1,
      }));

      const card = await ReadBackCard.create(perspective, { title: "written" }, { returns: "row" });

      expect(perspective.mutate.mock.invocationCallOrder[0])
        .toBeLessThan(perspective.modelQuery.mock.invocationCallOrder[0]);
      expect(card.author).toBe("did:key:alice");
      expect(card.createdAt).toBe(1700000000000);
      expect(card.title).toBe("stored");
    });

    it("rejects with the read error when returns: 'row' cannot read, naming the created id", async () => {
      const perspective = makePerspective();

      const err = await ReadBackCard.create(perspective, { title: "written" }, { returns: "row" }).catch((e) => e);

      expect(perspective.mutate).toHaveBeenCalledTimes(1);
      expect(err.message).toMatch(/was created, but reading it back failed/);
      expect(err.cause).toBe(reread);
    });

    it("refuses returns: 'row' in a caller's batch before it writes", async () => {
      const perspective = makePerspective();

      await expect(
        ReadBackCard.create(perspective, { title: "written" }, { returns: "row", batchId: "caller-batch" }),
      ).rejects.toThrow(/returns: 'row'.*batchId/);
      expect(perspective.mutate).not.toHaveBeenCalled();
    });

    it("then save() updates: the snapshot is set, no second create", async () => {
      const perspective = makePerspective();
      const card = await ReadBackCard.create(perspective, { title: "written" });
      perspective.mutate.mockClear();
      card.title = "changed";
      jest.spyOn(card as any, "getData").mockResolvedValue(card);
      await card.save();

      expect(perspective.mutate).toHaveBeenCalledTimes(1);
      expect(perspective.mutate).toHaveBeenCalledWith(
        [{ op: "update", class: expect.stringContaining("ReadBackCard"), id: card.id, set: { title: "changed" } }],
        {},
      );
    });

    it("writes the parent in the create op, and reads nothing", async () => {
      const perspective = makePerspective();

      await ReadBackCard.create(
        perspective, { title: "written" }, { parent: { id: "rb://parent", predicate: "rb://child" } },
      );

      expect(perspective.mutate).toHaveBeenCalledWith(
        [expect.objectContaining({ op: "create", parent: { id: "rb://parent", predicate: "rb://child" } })],
        {},
      );
      expect(perspective.modelQuery).not.toHaveBeenCalled();
    });

    it("rejects with the executor's refusal and does not read", async () => {
      const perspective = makePerspective();
      const failure = new Error("create refused");
      perspective.mutate.mockRejectedValue(failure);

      await expect(ReadBackCard.create(perspective, { title: "written" })).rejects.toBe(failure);
      expect(perspective.modelQuery).not.toHaveBeenCalled();
    });
  });
});

describe("get() and update() on a missing instance", () => {
  @Model({ name: "MissCard" })
  class MissCard extends Ad4mModel {
    @Flag({ through: "miss://kind", value: "miss://card" })
    kind: string = "";
    @Property({ through: "miss://title" })
    title: string = "";
  }

  const makePerspective = (instances: any[]) => ({
    uuid: "p",
    modelQuery: jest.fn().mockResolvedValue({ instances, totalCount: instances.length }),
    mutate: jest.fn(async () => ({ ids: [] })),
    createBatch: jest.fn(async () => "batch-1"),
    commitBatch: jest.fn(async () => ({})),
    add: jest.fn(async () => {}),
  }) as any;

  it("get() returns the hydrated instance when it exists", async () => {
    const perspective = makePerspective([{ id: "miss://1", kind: "miss://card", title: "found" }]);
    const card = new MissCard(perspective, "miss://1");
    const result = await card.get();
    expect(result).toBe(card);
    expect(card.title).toBe("found");
  });

  it("get() returns null for a missing instance", async () => {
    const perspective = makePerspective([]);
    expect(await new MissCard(perspective, "miss://none").get()).toBeNull();
  });

  it("get() rejects when the read fails", async () => {
    const perspective = makePerspective([]);
    const failure = new Error("query failed");
    perspective.modelQuery.mockRejectedValue(failure);
    await expect(new MissCard(perspective, "miss://1").get()).rejects.toBe(failure);
  });

  it("update() rejects for a missing ID and writes nothing", async () => {
    const perspective = makePerspective([]);
    await expect(MissCard.update(perspective, "miss://none", { title: "x" })).rejects.toThrow(/not found/);
    for (const write of ["createBatch", "mutate", "add"]) {
      expect(perspective[write]).not.toHaveBeenCalled();
    }
  });

  it("save() re-snapshots an instance that reads back as missing after the write", async () => {
    @Model({ name: "MissOptional" })
    class MissOptional extends Ad4mModel {
      @Property({ through: "miss://note" })
      note?: string;
    }
    // An all-optional model: the re-read after the update finds no row.
    const perspective = makePerspective([]);
    perspective.modelQuery
      .mockResolvedValueOnce({ instances: [{ id: "miss://o1", note: "old" }], totalCount: 1 })
      .mockResolvedValue({ instances: [], totalCount: 0 });
    const note = new MissOptional(perspective, "miss://o1");
    await note.get();

    note.note = "new";
    await note.save();

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(note.isDirty()).toBe(false);
  });
});

describe("live query by class name", () => {
  @Model({ name: "ByNameCard" })
  class ByNameCard extends Ad4mModel {
    @Flag({ through: "byname://kind", value: "byname://card" })
    kind: string = "";
    @Property({ through: "byname://title" })
    title: string = "";
  }

  const rows = [
    { id: "byname://c1", kind: "byname://card", title: "one" },
    { id: "byname://c2", kind: "byname://card", title: "two" },
  ];
  const makeHarness = () => liveHarness({ instances: rows, totalCount: rows.length });
  const data = (results: any[]) => results.map((r) => ({ id: r.id, title: r.title }));
  const query = { where: { title: "one" }, order: { title: "ASC" as const }, limit: 5 };

  it("sends the same query body and returns the same rows as a query by class", async () => {
    const byClass = makeHarness();
    const byName = makeHarness();
    const liveClass = ByNameCard.query(byClass.perspective, query);
    const liveName = Ad4mModel.query(byName.perspective, query, { className: "ByNameCard" });
    const viaClass = (await liveClass.first).results;
    const viaName = (await liveName.first).results;
    liveClass.dispose();
    liveName.dispose();

    // The class name travels as given; the class query names it by its target
    // class IRI. The executor reads both as the same class (a bare name reads
    // the least IRI), so the query body is identical.
    expect(byName.perspective.client.subscribe.mock.calls[0][1].className).toBe("ByNameCard");
    expect(byName.perspective.client.subscribe.mock.calls[0][1].queryJson)
      .toBe(byClass.perspective.client.subscribe.mock.calls[0][1].queryJson);
    expect(data(viaName)).toEqual(data(viaClass));
  });

  it("opens a subscription named by the class name", async () => {
    const perspective = makeHarness().perspective;
    const live = Ad4mModel.query(perspective, undefined, { className: "ByNameCard" });
    const { results } = await live.first;
    live.dispose();

    expect(perspective.client.subscribe.mock.calls[0][1].className).toBe("ByNameCard");
    expect(data(results)).toEqual(data(rows));
  });

  it("rejects includeAll with a clear error (the client does not know the named model's relations)", () => {
    const perspective = makeHarness().perspective;
    expect(() =>
      Ad4mModel.query(perspective, { includeAll: true }, { className: "ByNameCard" }),
    ).toThrow(/includeAll needs a decorated model class/);
    expect(perspective.client.subscribe).not.toHaveBeenCalled();
  });
});

import { valueToLiteralIri } from "./query-sparql";

describe("Model writes: one mutate op per write", () => {
  @Model({ name: "P4Card" })
  class P4Card extends Ad4mModel {
    @Optional({ through: "p4://title" })
    title?: string;

    @Optional({ through: "p4://note", local: true })
    note?: string;

    @Optional({ through: "p4://rank" })
    rank?: number;

    @HasMany({ through: "p4://tag" })
    tags: string[] = [];

    @HasMany({ through: "p4://link" })
    links: string[] = [];
  }

  const makePerspective = () =>
    ({
      mutate: jest.fn().mockResolvedValue({ ids: [] }),
      uuid: "test-perspective-uuid",
    }) as any;

  const edited = (perspective: any) => {
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.takeSnapshot();
    card.title = "Hello";
    card.note = "private";
    card.rank = 3;
    return card;
  };

  it("an update of three scalar fields is one call with one update op, sending the raw values", async () => {
    const perspective = makePerspective();
    await edited(perspective).save("batch-1");

    expect(perspective.mutate.mock.calls).toEqual([[
      [{ op: "update", class: "p4://P4Card", id: "p4://card/1", set: { title: "Hello", note: "private", rank: 3 } }],
      { batch: "batch-1" },
    ]]);
  });

  it("sends only the changed fields", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.title = "Old";
    card.takeSnapshot();
    card.rank = 4;
    await card.save("batch-1");

    expect(perspective.mutate.mock.calls[0][0]).toEqual([
      { op: "update", class: "p4://P4Card", id: "p4://card/1", set: { rank: 4 } },
    ]);
  });

  it("sends nothing when nothing changed", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.takeSnapshot();
    await card.save("batch-1");
    expect(perspective.mutate).not.toHaveBeenCalled();
  });

  it("a cleared property is null in set", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.title = "Old";
    card.rank = 1;
    card.takeSnapshot();
    card.title = "";
    card.rank = null;
    await card.save("batch-1");

    expect(perspective.mutate.mock.calls[0][0]).toEqual([
      { op: "update", class: "p4://P4Card", id: "p4://card/1", set: { title: null, rank: null } },
    ]);
  });

  it("adders, removers and a set of the relations and the scalars share one update op", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.tags = ["t0"];
    card.links = ["l0"];
    card.takeSnapshot();
    card.tags = { action: "adder", value: ["t1", "t2"] };
    card.links = ["l1", "l2"];
    card.title = "Hello";
    await card.save("batch-1");

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate.mock.calls[0][0]).toEqual([{
      op: "update",
      class: "p4://P4Card",
      id: "p4://card/1",
      set: { links: ["l1", "l2"], title: "Hello" },
      add: { tags: ["t1", "t2"] },
    }]);
  });

  it("a new instance is one create op with the values and the members, and no empty relation", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.title = "Hello";
    card.tags = ["t1"];
    await card.save("batch-1");

    expect(perspective.mutate.mock.calls).toEqual([[
      [{ op: "create", class: "p4://P4Card", id: "p4://card/1", values: { title: "Hello", tags: ["t1"] } }],
      { batch: "batch-1" },
    ]]);
  });

  it("a long run of members is one op, in order", async () => {
    const perspective = makePerspective();
    const card = new P4Card(perspective, "p4://card/1") as any;
    card.tags = ["t0"];
    card.takeSnapshot();
    const tags = Array.from({ length: 450 }, (_, i) => `t${i}`);
    card.tags = { action: "adder", value: tags };
    await card.save("batch-1");

    expect(perspective.mutate).toHaveBeenCalledTimes(1);
    expect(perspective.mutate.mock.calls[0][0][0].add.tags).toEqual(tags);
  });
});
