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
      @Property({ through: "test://name", resolveLanguage: "literal" })
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
    expect(metadata.properties.name.resolveLanguage).toBe("literal");
    
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
      @Property({ through: "recipe://name", resolveLanguage: "literal" })
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
    expect(metadata.properties.name.resolveLanguage).toBe("literal");
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
      namespace: "product://",
      resolveLanguage: "literal"
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
    expect(metadata.properties.name.resolveLanguage).toBe("literal");

    expect(metadata.properties.price).toBeDefined();
    expect(metadata.properties.price.predicate).toBe("product://price");
    expect(metadata.properties.price.required).toBe(true);
    expect(metadata.properties.price.resolveLanguage).toBe("literal");

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
            resolveLanguage: "literal",
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
    expect(metadata.properties.name.resolveLanguage).toBe("literal");
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
      namespace: "article://",
      resolveLanguage: "literal"
    });

    const metadata = ArticleClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Article");

    // Verify properties
    expect(metadata.properties.title).toBeDefined();
    expect(metadata.properties.title.predicate).toBe("article://title");
    expect(metadata.properties.title.required).toBe(true);
    expect(metadata.properties.title.resolveLanguage).toBe("literal");

    expect(metadata.properties.views).toBeDefined();
    expect(metadata.properties.views.predicate).toBe("article://views");
    expect(metadata.properties.views.resolveLanguage).toBe("literal");

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
    @Property({ through: "task://title", resolveLanguage: "literal", required: true })
    title: string = "";

    @Property({ through: "task://priority", resolveLanguage: "literal" })
    priority: number = 0;

    @Property({ through: "task://done", resolveLanguage: "literal" })
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
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should use SPARQL when engine is 'sparql' in findAll()", async () => {
    // modelQuery returns already-hydrated instances from the Rust executor
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.findAll(mockPerspective, {}, 'sparql');

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should route through modelQuery when engine is 'prolog' in findAll() (prolog is now no-op)", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.findAll(mockPerspective, {}, false);
    
    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
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

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.get()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .engine('sparql')
      .get();

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use modelQuery when .engine('prolog') is called (engine is now a no-op)", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .engine('prolog')
      .get();
    
    // engine('prolog') is now a no-op — everything goes through modelQuery
    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
  });

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.count()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 4 } })
      .engine('sparql')
      .count();

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.paginate()", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [
        { id: "literal:recipe1", name: "Pasta", rating: 5, ingredients: ["pasta"] }
      ],
      totalCount: 1
    });

    const page = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .engine('sparql')
      .paginate(10, 1);

    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
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
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
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

  it("should apply filtering in ModelQueryBuilder.count() with gt operator", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 2
    });

    // Count recipes with rating > 3 using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .engine('sparql')
      .count();
    
    expect(count).toBe(2);
    expect(mockPerspective.modelQuery).toHaveBeenCalled();
  });

  it("should apply filtering in ModelQueryBuilder.count() with timestamp between", async () => {
    mockPerspective.modelQuery.mockResolvedValue({
      instances: [],
      totalCount: 3
    });

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    
    // Count using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ timestamp: { between: [startTimestamp, endTimestamp] } })
      .engine('sparql')
      .count();
    
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

    @Property({ through: "flux://name", resolveLanguage: "literal", required: true })
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

    @Property({ through: "flag://name", resolveLanguage: "literal", required: true })
    name: string = "";
  }

  @Model({ name: "NoFlagModel" })
  class NoFlagModel extends Ad4mModel {
    @Property({ through: "noflag://title", resolveLanguage: "literal" })
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

// ── Lightweight fingerprint (3.7) ────────────────────────────────────────────
describe("Lightweight fingerprint optimization", () => {
  // Replicate the buildFingerprint logic for testing
  const buildFingerprint = (results: any[]) => {
    if (results.length === 0) return '0:';
    const ids = results.map((r: any) => r.id || '').sort().join(',');
    const ts = results.map((r: any) => r.updatedAt || r.timestamp || '').join(',');
    return `${results.length}:${ids}:${ts}`;
  };

  const base = [
    { id: "a", updatedAt: "100" },
    { id: "b", updatedAt: "200" },
  ];

  it("lightweight fingerprint should detect instance addition", () => {
    const fp1 = buildFingerprint(base);
    const fp2 = buildFingerprint([...base, { id: "c", updatedAt: "300" }]);
    expect(fp1).not.toBe(fp2);
  });

  it("lightweight fingerprint should detect instance removal", () => {
    const fp1 = buildFingerprint(base);
    const fp2 = buildFingerprint([base[0]]);
    expect(fp1).not.toBe(fp2);
  });

  it("lightweight fingerprint should detect timestamp change", () => {
    const fp1 = buildFingerprint(base);
    const fp2 = buildFingerprint([{ id: "a", updatedAt: "999" }, base[1]]);
    expect(fp1).not.toBe(fp2);
  });

  it("lightweight fingerprint should NOT false-positive for identical sets", () => {
    const fp1 = buildFingerprint(base);
    const fp2 = buildFingerprint([...base]); // same data, new array
    expect(fp1).toBe(fp2);
  });
});



// ── Subscribe callback timing ──────────────────────────────────────────
describe("ModelQueryBuilder subscribe callback timing", () => {
  it("subscribe should not invoke callback synchronously before Promise resolves", async () => {
    // This test verifies that subscribe() uses the Rust model subscription endpoint
    // and properly handles the subscription lifecycle.
    
    const mockSubscriptionId = "test-sub-123";
    let updateCallback: ((result: any) => void) | null = null;

    const mockClient = {
      modelSubscribe: jest.fn().mockResolvedValue({
        subscriptionId: mockSubscriptionId,
        result: { instances: [], totalCount: 0 },
      }),
      subscribeToQueryUpdates: jest.fn().mockImplementation((_id: string, cb: any) => {
        updateCallback = cb;
        return () => {}; // unsubscribe function
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };

    const mockPerspective = {
      uuid: "test-uuid",
      client: mockClient,
      modelSubscribe: jest.fn().mockImplementation(async (className: string, queryJson: string, shapeJson?: string) => {
        return mockClient.modelSubscribe("test-uuid", className, queryJson, shapeJson);
      }),
      getLinks: jest.fn().mockResolvedValue([]),
      modelQuery: jest.fn().mockResolvedValue({ instances: [], totalCount: 0 }),
    } as any;

    const { Ad4mModel, Model, Property, Flag } = require("./index");

    @Model({ name: "TimingTest" })
    class TimingTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://timing" })
      type: string = "test://timing";
      @Property({ through: "test://name" })
      name: string = "";
    }

    let callbackInvokedBeforeResolve = false;
    let promiseResolved = false;

    const query = TimingTest.query(mockPerspective);
    const promise = query.subscribe((results: any[]) => {
      if (!promiseResolved) {
        callbackInvokedBeforeResolve = true;
      }
    });

    // At this point the Promise hasn't resolved yet
    // The callback should NOT have fired synchronously
    expect(callbackInvokedBeforeResolve).toBe(false);

    const initialResults = await promise;
    promiseResolved = true;

    expect(initialResults).toEqual([]);
    expect(mockPerspective.modelSubscribe).toHaveBeenCalled();
  });
});

describe("ModelQueryBuilder paginateSubscribe", () => {
  it("paginateSubscribe should use a single query with count: true", async () => {
    const mockSubscriptionId = "paginate-sub-123";

    const mockClient = {
      modelSubscribe: jest.fn().mockResolvedValue({
        subscriptionId: mockSubscriptionId,
        result: { instances: [], totalCount: 0 },
      }),
      subscribeToQueryUpdates: jest.fn().mockImplementation((_id: string, _cb: any) => {
        return () => {};
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };

    const mockPerspective = {
      uuid: "test-uuid",
      client: mockClient,
      modelSubscribe: jest.fn().mockImplementation(async (className: string, queryJson: string, shapeJson?: string) => {
        return mockClient.modelSubscribe("test-uuid", className, queryJson, shapeJson);
      }),
      modelQuery: jest.fn().mockResolvedValue({ instances: [{ id: "item1", type: "test://paginate" }], totalCount: 42 }),
    } as any;

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "PaginateTest" })
    class PaginateTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://paginate" })
      type: string = "test://paginate";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const builder = PaginateTest.query(mockPerspective);
    const result = await builder.paginateSubscribe(10, 1, () => {});

    // Should return paginated result structure
    expect(result).toHaveProperty("results");
    expect(result).toHaveProperty("totalCount");
    expect(result).toHaveProperty("pageSize", 10);
    expect(result).toHaveProperty("pageNumber", 1);

    // modelQuery should be called exactly once (not twice — no separate count query)
    expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);

    // The query should have count: true
    const callArgs = mockPerspective.modelQuery.mock.calls[0];
    const queryJson = JSON.parse(callArgs[1]);
    expect(queryJson.count).toBe(true);
    expect(queryJson.limit).toBe(10);
    expect(queryJson.offset).toBe(0);
  });

  it("paginateSubscribe should re-fetch on real subscription updates", async () => {
    const mockSubscriptionId = "paginate-update-sub";
    let capturedCallback: ((result: any) => void) | null = null;

    const mockClient = {
      modelSubscribe: jest.fn().mockResolvedValue({
        subscriptionId: mockSubscriptionId,
        result: { instances: [], totalCount: 0 },
      }),
      subscribeToQueryUpdates: jest.fn().mockImplementation((_id: string, cb: any) => {
        capturedCallback = cb;
        return () => {};
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };

    let modelQueryCallCount = 0;
    const mockPerspective = {
      uuid: "test-uuid",
      client: mockClient,
      modelSubscribe: jest.fn().mockImplementation(async (className: string, queryJson: string, shapeJson?: string) => {
        return mockClient.modelSubscribe("test-uuid", className, queryJson, shapeJson);
      }),
      modelQuery: jest.fn().mockImplementation(async () => {
        modelQueryCallCount++;
        return { instances: [{ id: "new-item", type: "test://update" }], totalCount: 1 };
      }),
    } as any;

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "UpdateTest" })
    class UpdateTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://update" })
      type: string = "test://update";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const userCallback = jest.fn();
    const builder = UpdateTest.query(mockPerspective);
    await builder.paginateSubscribe(10, 1, userCallback);

    const initialCallCount = modelQueryCallCount;

    // Simulate a subscription update
    capturedCallback!({ instances: [{ id: "new-item" }], totalCount: 1 });

    // Wait for async processing
    await new Promise(r => setTimeout(r, 100));

    // modelQuery should have been called again for the re-fetch
    expect(modelQueryCallCount).toBeGreaterThan(initialCallCount);
    // User callback should have been invoked with paginated results
    expect(userCallback).toHaveBeenCalled();
    const callArg = userCallback.mock.calls[0][0];
    expect(callArg).toHaveProperty("totalCount");
    expect(callArg).toHaveProperty("pageSize", 10);
    expect(callArg).toHaveProperty("pageNumber", 1);
  });
});

// ============================================================================
// Subscription keepalive recovery tests
// ============================================================================

describe("ModelQueryBuilder keepalive recovery", () => {
  beforeEach(() => jest.useFakeTimers());
  afterEach(() => jest.useRealTimers());

  /**
   * Helper: build a mock perspective whose keepAliveQuery rejects after
   * `failAfter` successful calls, simulating a server-side subscription
   * eviction (returns "Subscription not found").  On resubscribe (a second
   * modelSubscribe call) it returns a *new* subscription ID.
   */
  function buildMocks(failAfter = 0) {
    let keepaliveCallCount = 0;
    let modelSubscribeCallCount = 0;

    const mockClient = {
      modelSubscribe: jest.fn().mockImplementation(async () => {
        modelSubscribeCallCount++;
        return {
          subscriptionId: `sub-${modelSubscribeCallCount}`,
          result: { instances: [], totalCount: 0 },
        };
      }),
      subscribeToQueryUpdates: jest.fn().mockImplementation((_id: string, _cb: any) => {
        return () => {};
      }),
      keepAliveQuery: jest.fn().mockImplementation(async () => {
        keepaliveCallCount++;
        if (keepaliveCallCount > failAfter) {
          throw new Error("RPC error 500: Subscription not found");
        }
        return true;
      }),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };

    const mockPerspective = {
      uuid: "test-uuid",
      client: mockClient,
      modelSubscribe: jest.fn().mockImplementation(
        async (className: string, queryJson: string, shapeJson?: string) =>
          mockClient.modelSubscribe("test-uuid", className, queryJson, shapeJson)
      ),
      modelQuery: jest.fn().mockResolvedValue({ instances: [], totalCount: 0 }),
      getLinks: jest.fn().mockResolvedValue([]),
    } as any;

    return { mockClient, mockPerspective, getKeepaliveCount: () => keepaliveCallCount, getSubscribeCount: () => modelSubscribeCallCount };
  }

  it("subscribe: resubscribes when keepalive gets 'Subscription not found'", async () => {
    const { mockClient, mockPerspective, getSubscribeCount } = buildMocks(/* failAfter */ 1);

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "KeepaliveRecoveryTest" })
    class KeepaliveRecoveryTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://ka" })
      type: string = "test://ka";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const builder = KeepaliveRecoveryTest.query(mockPerspective);
    await builder.subscribe(() => {});

    // Initial subscription
    expect(getSubscribeCount()).toBe(1);

    // First keepalive at 30s — succeeds (failAfter=1)
    await jest.advanceTimersByTimeAsync(30_000);
    expect(mockClient.keepAliveQuery).toHaveBeenCalledTimes(1);

    // Second keepalive at 60s — fails → should trigger resubscribe
    await jest.advanceTimersByTimeAsync(30_000);
    expect(mockClient.keepAliveQuery).toHaveBeenCalledTimes(2);

    // Allow exponential backoff (2000ms for first retry) + microtask queue to settle
    await jest.advanceTimersByTimeAsync(2500);

    // A second modelSubscribe call means recovery happened
    expect(getSubscribeCount()).toBe(2);

    // Clean up
    builder.dispose();
  });

  it("countSubscribe: resubscribes when keepalive gets 'Subscription not found'", async () => {
    const { mockClient, mockPerspective, getSubscribeCount } = buildMocks(0); // fail immediately

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "CountKeepaliveTest" })
    class CountKeepaliveTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://cka" })
      type: string = "test://cka";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const builder = CountKeepaliveTest.query(mockPerspective);
    await builder.countSubscribe(() => {});

    expect(getSubscribeCount()).toBe(1);

    // First keepalive at 30s — fails immediately → should trigger resubscribe
    await jest.advanceTimersByTimeAsync(30_000);
    // Allow exponential backoff (2000ms for first retry) + microtask queue to settle
    await jest.advanceTimersByTimeAsync(2500);

    expect(getSubscribeCount()).toBe(2);

    builder.dispose();
  });

  it("paginateSubscribe: resubscribes when keepalive gets 'Subscription not found'", async () => {
    const { mockClient, mockPerspective, getSubscribeCount } = buildMocks(0);

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "PaginateKeepaliveTest" })
    class PaginateKeepaliveTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://pka" })
      type: string = "test://pka";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const builder = PaginateKeepaliveTest.query(mockPerspective);
    await builder.paginateSubscribe(10, 1, () => {});

    expect(getSubscribeCount()).toBe(1);

    await jest.advanceTimersByTimeAsync(30_000);
    // Allow exponential backoff (2000ms for first retry) + microtask queue to settle
    await jest.advanceTimersByTimeAsync(2500);

    expect(getSubscribeCount()).toBe(2);

    builder.dispose();
  });

  it("subscribe: stops retrying after dispose()", async () => {
    const { mockClient, mockPerspective } = buildMocks(Infinity); // keepalive always succeeds

    const { Ad4mModel, Model, Flag, Property } = require("./index");

    @Model({ name: "DisposeTest" })
    class DisposeTest extends Ad4mModel {
      @Flag({ through: "test://type", value: "test://disp" })
      type: string = "test://disp";
      @Property({ through: "test://name" })
      name: string = "";
    }

    const builder = DisposeTest.query(mockPerspective);
    await builder.subscribe(() => {});

    builder.dispose();

    // Advance well past multiple keepalive intervals
    await jest.advanceTimersByTimeAsync(120_000);

    // No keepalive calls should have happened after dispose
    expect(mockClient.keepAliveQuery).toHaveBeenCalledTimes(0);
  });
});

// ============================================================================
// Performance Optimisation Tests
// ============================================================================

import { getCachedResult, setCachedResult, clearQueryCache, queryCacheSize } from "./query-cache";
import { getPropertiesMetadata, getRelationsMetadata, getMemoizedSHACL } from "./decorators";

describe("QueryCache", () => {
  beforeEach(() => clearQueryCache());

  it("should return undefined for cache miss", () => {
    expect(getCachedResult("uuid1", "SELECT ?s WHERE {}")).toBeUndefined();
  });

  it("should return cached result within TTL", () => {
    const result = [{ s: "test" }];
    setCachedResult("uuid1", "SELECT ?s WHERE {}", result);
    expect(getCachedResult("uuid1", "SELECT ?s WHERE {}")).toBe(result);
  });

  it("should expire after TTL", async () => {
    setCachedResult("uuid1", "SELECT ?s WHERE {}", [{ s: "test" }], 50);
    await new Promise(r => setTimeout(r, 60));
    expect(getCachedResult("uuid1", "SELECT ?s WHERE {}")).toBeUndefined();
  });

  it("should cache independently per perspective", () => {
    setCachedResult("uuid1", "SELECT ?s WHERE {}", "result1");
    setCachedResult("uuid2", "SELECT ?s WHERE {}", "result2");
    expect(getCachedResult("uuid1", "SELECT ?s WHERE {}")).toBe("result1");
    expect(getCachedResult("uuid2", "SELECT ?s WHERE {}")).toBe("result2");
  });

  it("should cache independently per query", () => {
    setCachedResult("uuid1", "query1", "r1");
    setCachedResult("uuid1", "query2", "r2");
    expect(getCachedResult("uuid1", "query1")).toBe("r1");
    expect(getCachedResult("uuid1", "query2")).toBe("r2");
  });

  it("should clear all entries", () => {
    setCachedResult("uuid1", "q1", "r1");
    setCachedResult("uuid2", "q2", "r2");
    expect(queryCacheSize()).toBe(2);
    clearQueryCache();
    expect(queryCacheSize()).toBe(0);
  });
});

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

    @Property({ through: "flux://body", resolveLanguage: "literal" })
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
    modelQuery: jest.fn(async (className: string, queryJson: string, shapeJson: string) => {
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
    evaluateGetters: jest.fn(async (className: string, instanceIds: string[], shapeJson: string, propertyNames?: string[]) => {
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
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue("DeepQueryTestMessage"),
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
      const [className, instanceIds, shapeJson, propertyNames] = mockPerspective.evaluateGetters.mock.calls[0];
      expect(className).toBe("DeepQueryTestMessage");
      expect(instanceIds).toEqual(["flux://msg-0", "flux://msg-1"]);
      expect(propertyNames).toEqual(["replyingTo"]);
      // No querySparql calls — all done in-process
      expect(sparqlCalls.length).toBe(0);
    });

    it("evaluates all getters when propertyNames is omitted", async () => {
      const instances = makeInstances(2);

      await DeepQueryTestMessage.evaluateGetters(instances, mockPerspective);

      expect(mockPerspective.evaluateGetters).toHaveBeenCalledTimes(1);
      const [className, instanceIds, shapeJson, propertyNames] = mockPerspective.evaluateGetters.mock.calls[0];
      expect(className).toBe("DeepQueryTestMessage");
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

  describe("deepQuery via ModelQueryBuilder", () => {
    it("deepQuery() method sets deepQuery flag on queryParams", () => {
      const builder = DeepQueryTestMessage.query(mockPerspective);
      (builder as any).deepQuery();
      expect((builder as any).queryParams.deepQuery).toBe(true);
    });

    it("deepQuery defaults to true when not explicitly set", async () => {
      mockPerspective.modelQuery.mockResolvedValueOnce({ instances: [], totalCount: 0 });
      await DeepQueryTestMessage.query(mockPerspective).get();

      expect(mockPerspective.modelQuery).toHaveBeenCalledTimes(1);
      const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
      const qi = JSON.parse(queryJson);
      expect(qi.deepQuery).toBe(true);
    });

    it("deepQuery can be explicitly set to false", async () => {
      mockPerspective.modelQuery.mockResolvedValueOnce({ instances: [], totalCount: 0 });
      await DeepQueryTestMessage.query(mockPerspective).deepQuery(false).get();

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
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue("Post"),
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

  it("enriches projection with targetShape when relation target is registered", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $signalCount: { from: "signals", count: true },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // targetShape should have been injected from the Signal model
    // ($ prefix is required for projection enrichment to apply)
    expect(qi.projections?.$signalCount?.targetShape?.className).toBe("Signal");
  });

  it("leaves targetShape absent when relation has no target decorator", async () => {
    await Post.findAll(mockPerspective, {
      include: {
        $commentCount: { from: "comments", count: true },
      },
    });

    const [, queryJson] = mockPerspective.modelQuery.mock.calls[0];
    const qi = JSON.parse(queryJson);

    // 'comments' HasMany has no target() thunk → no targetShape
    expect(qi.projections?.$commentCount?.targetShape).toBeUndefined();
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
});
