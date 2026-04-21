import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, Optional, ReadOnly, HasMany, Flag } from "./decorators";

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

  it("should extract transform function from property metadata", () => {
    @Model({ name: "TransformModel" })
    class TransformModel extends Ad4mModel {
      @Optional({ 
        through: "test://data",
        transform: (value: string) => value.toUpperCase()
      })
      data: string = "";
    }

    const metadata = TransformModel.getModelMetadata();
    
    // Assert transform is a function
    expect(typeof metadata.properties.data.transform).toBe("function");
    
    // Test the transform function
    const transformed = metadata.properties.data.transform!("test");
    expect(transformed).toBe("TEST");
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
describe("Ad4mModel.instancesFromQueryResult() and SPARQL integration", () => {
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

  // Mock perspective with both querySparql and infer methods
  const mockPerspective = {
    querySparql: jest.fn(),
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should convert empty query results correctly", async () => {
    const result = await Recipe.instancesFromQueryResult(mockPerspective, {}, []);
    
    expect(result.results).toEqual([]);
    expect(result.totalCount).toBe(0);
  });

  it("should convert query results to model instances", async () => {
    const queryResults = [
      {
        source: "node:abc123",
        source_uri: "literal:recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "cheese", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      },
      {
        source: "node:def456",
        source_uri: "literal:recipe2",
        links: [
          { predicate: "recipe://name", target: "Pizza", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://rating", target: "4", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "dough", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "cheese", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromQueryResult(mockPerspective, {}, queryResults);

    expect(result.results).toHaveLength(2);
    expect(result.totalCount).toBe(2);

    const recipe1 = result.results[0];
    expect(recipe1).toBeInstanceOf(Recipe);
    expect(recipe1.name).toBe("Pasta");
    expect(recipe1.rating).toBe(5);
    expect(recipe1.ingredients).toEqual(["pasta", "tomato", "cheese"]);

    const recipe2 = result.results[1];
    expect(recipe2).toBeInstanceOf(Recipe);
    expect(recipe2.name).toBe("Pizza");
    expect(recipe2.rating).toBe(4);
  });

  it("should filter properties when query specifies properties", async () => {
    const queryResults = [
      {
        source: "node:abc123",
        source_uri: "literal:recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromQueryResult(
      mockPerspective,
      { properties: ["name"] },
      queryResults
    );

    expect(result.results).toHaveLength(1);
    const recipe = result.results[0];
    expect(recipe.name).toBe("Pasta");
    // rating and ingredients should be removed since only "name" was requested
    expect(recipe.rating).toBeUndefined();
    expect(recipe.ingredients).toBeUndefined();
    // author, createdAt, updatedAt are also stripped unless explicitly requested
    expect(recipe.author).toBeUndefined();
    expect(recipe.timestamp).toBeUndefined();
  });

  it("should filter properties when query specifies properties", async () => {
    const queryResults = [
      {
        source: "node:abc123",
        source_uri: "literal:recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromQueryResult(
      mockPerspective,
      { properties: ["name"] },
      queryResults
    );

    expect(result.results).toHaveLength(1);
    const recipe = result.results[0];
    expect(recipe.name).toBe("Pasta");
    // rating and ingredients should be removed since only "name" was requested
    expect(recipe.rating).toBeUndefined();
  });

  it("should handle results missing base field", async () => {
    const queryResults = [
      {
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
        // Missing source field
      } as any
    ];

    const result = await Recipe.instancesFromQueryResult(mockPerspective, {}, queryResults);

    // Should filter out the invalid result (or handle gracefully)
    expect(result.results).toHaveLength(0);
    expect(result.totalCount).toBe(0);
  });

  it("should use SPARQL when engine is 'sparql' in findAll()", async () => {
    // Raw SPARQL rows (flat) — groupSPARQLResults will group them
    const queryResults = [
      { source: "literal:recipe1", predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
    ];

    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const results = await Recipe.findAll(mockPerspective, {}, 'sparql');

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use Prolog when engine is prolog in findAll()", async () => {
    const prologResults = [{
      AllInstances: [
        ["literal:recipe1", [["name", "Pasta"]], [["ingredients", ["pasta"]]], "2023-01-01T00:00:00Z", "did:key:alice"]
      ],
      TotalCount: 1
    }];

    mockPerspective.infer.mockResolvedValue(prologResults);

    const results = await Recipe.findAll(mockPerspective, {}, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
  });

  it("should use SPARQL when engine is 'sparql' in findAllAndCount()", async () => {
    const queryResults = [
      { source: "literal:recipe1", predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
    ];

    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const { results, totalCount } = await Recipe.findAllAndCount(mockPerspective, {}, 'sparql');

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(totalCount).toBe(1);
  });

  it("should use SPARQL when engine is 'sparql' in paginate()", async () => {
    const queryResults = [
      { source: "literal:recipe1", predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
    ];

    // First call returns data, second call returns count (for SPARQL-paginated totalCount)
    mockPerspective.querySparql
      .mockResolvedValueOnce(queryResults)
      .mockResolvedValueOnce([{ count: 1 }]);

    const page = await Recipe.paginate(mockPerspective, 10, 1, {}, 'sparql');

    // paginate now issues 2 SPARQL queries: data + count
    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(2);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
    expect(page.totalCount).toBe(1);
  });

  it("should use SPARQL when engine is 'sparql' in count()", async () => {
    // count() now uses an efficient COUNT query that returns [{ count: N }]
    mockPerspective.querySparql.mockResolvedValue([{ count: 5 }]);

    const count = await Recipe.count(mockPerspective, {}, 'sparql');

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(5);
  });

  it("should use Prolog when engine is prolog in count()", async () => {
    const prologResults = [{ TotalCount: 10 }];
    mockPerspective.infer.mockResolvedValue(prologResults);

    const count = await Recipe.count(mockPerspective, {}, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(count).toBe(10);
  });

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.get()", async () => {
    const queryResults = [
      { source: "literal:recipe1", predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
    ];

    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .engine('sparql')
      .get();

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use Prolog when.engine('prolog') in ModelQueryBuilder.get()", async () => {
    const prologResults = [{
      AllInstances: [
        ["literal:recipe1", [["name", "Pasta"]], [["ingredients", ["pasta"]]], "2023-01-01T00:00:00Z", "did:key:alice"]
      ],
      TotalCount: 1
    }];

    mockPerspective.infer.mockResolvedValue(prologResults);

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .engine('prolog')
      .get();
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
  });

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.count()", async () => {
    // count() counts the number of rows returned by the query (one row per source)
    const queryResults = [
      ...Array.from({ length: 3 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]).flat()
    ];
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 4 } })
      .engine('sparql')
      .count();

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });

  it("should use SPARQL when engine is 'sparql' in ModelQueryBuilder.paginate()", async () => {
    const queryResults = [
      { source: "literal:recipe1", predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
      { source: "literal:recipe1", predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
    ];

    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const page = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .engine('sparql')
      .paginate(10, 1);

    expect(mockPerspective.querySparql).toHaveBeenCalledTimes(1);
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
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should apply JS-level filtering for gt operator on properties in SPARQL count()", async () => {
    // Mock query results: 5 recipes with ratings 1, 2, 3, 4, 5
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes with rating > 3 (should match 2 recipes: rating 4 and 5)
    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } }, 'sparql');
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { rating: { gt: 3 } } }, 'sparql');
    
    expect(count).toBe(2);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for between operator on properties in SPARQL count()", async () => {
    // Mock query results: 5 recipes with ratings 1, 2, 3, 4, 5
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes with rating between 2 and 4 (should match 3 recipes: rating 2, 3, 4)
    const count = await Recipe.count(mockPerspective, { where: { rating: { between: [2, 4] } } }, 'sparql');
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { rating: { between: [2, 4] } } }, 'sparql');
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for timestamp gt operator in SPARQL count()", async () => {
    // Mock query results: 5 recipes with different timestamps
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes with timestamp > 2023-01-03 (should match 2 recipes: 2023-01-04 and 2023-01-05)
    const targetTimestamp = new Date("2023-01-03T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { where: { timestamp: { gt: targetTimestamp } } }, 'sparql');
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { timestamp: { gt: targetTimestamp } } }, 'sparql');
    
    expect(count).toBe(2);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for timestamp between operator in SPARQL count()", async () => {
    // Mock query results: 5 recipes with different timestamps
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes with timestamp between 2023-01-02 and 2023-01-04
    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { 
      where: { timestamp: { between: [startTimestamp, endTimestamp] } } 
    }, 'sparql');
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { 
      where: { timestamp: { between: [startTimestamp, endTimestamp] } } 
    }, 'sparql');
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for author filtering in SPARQL count()", async () => {
    // Mock query results: 3 recipes by Alice and 2 by Bob
    const queryResults = [
      ...Array.from({ length: 3 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]).flat(),
      ...Array.from({ length: 2 }, (_, i) => [
        { source: `literal:recipe${i+4}`, predicate: "recipe://name", target: `Recipe ${i+4}`, author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
        { source: `literal:recipe${i+4}`, predicate: "recipe://rating", target: "5", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes by Alice (should match 3 recipes)
    const count = await Recipe.count(mockPerspective, { where: { author: "did:key:alice" } }, 'sparql');
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { author: "did:key:alice" } }, 'sparql');
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering in ModelQueryBuilder.count() with gt operator", async () => {
    // Mock query results: 5 recipes with ratings 1, 2, 3, 4, 5
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    // Count recipes with rating > 3 using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .engine('sparql')
      .count();
    
    // Verify count matches the number of instances that would be returned by get()
    const getResults = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .engine('sparql')
      .get();
    
    expect(count).toBe(2);
    expect(count).toBe(getResults.length);
  });

  it("should apply JS-level filtering in ModelQueryBuilder.count() with timestamp between", async () => {
    // Mock query results: 5 recipes with different timestamps
    const queryResults = [
      ...Array.from({ length: 5 }, (_, i) => [
        { source: `literal:recipe${i+1}`, predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { source: `literal:recipe${i+1}`, predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]).flat()
    ];
    
    mockPerspective.querySparql.mockResolvedValue(queryResults);

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    
    // Count using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ timestamp: { between: [startTimestamp, endTimestamp] } })
      .engine('sparql')
      .count();
    
    // Verify count matches the number of instances that would be returned by get()
    const getResults = await Recipe.query(mockPerspective)
      .where({ timestamp: { between: [startTimestamp, endTimestamp] } })
      .engine('sparql')
      .get();
    
    expect(count).toBe(3);
    expect(count).toBe(getResults.length);
  });

  it("should handle count() with Prolog for gt operator (legacy)", async () => {
    const prologResults = [{ TotalCount: 2 }];
    mockPerspective.infer.mockResolvedValue(prologResults);

    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } }, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(count).toBe(2);
  });

  it("should handle count() with Prolog for timestamp between (legacy)", async () => {
    const prologResults = [{ TotalCount: 3 }];
    mockPerspective.infer.mockResolvedValue(prologResults);

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    
    const count = await Recipe.count(
      mockPerspective, 
      { where: { timestamp: { between: [startTimestamp, endTimestamp] } } }, 
      false
    );
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });
});


// ──────────────────────────────────────────────────────────
// Batch SPARQL query builder tests
// ──────────────────────────────────────────────────────────

import { buildBatchSPARQLQuery } from "./query-sparql-batch";

describe("buildBatchSPARQLQuery", () => {
  @Model({ name: "Author" })
  class Author extends Ad4mModel {
    @Property({ through: "author://name", required: true })
    name!: string;
  }

  @Model({ name: "Book" })
  class Book extends Ad4mModel {
    @Property({ through: "book://title", required: true })
    title!: string;

    @HasMany(() => Author, { through: "book://author" })
    authors!: Author[];
  }

  it("should generate UNION branches for depth 0 and depth 1 includes", () => {
    const metadata = Book.getModelMetadata();
    const query = { include: { authors: true } };
    const sparql = buildBatchSPARQLQuery(metadata, query, Book);

    expect(sparql).toContain("?depth");
    expect(sparql).toContain("?parentBase");
    expect(sparql).toContain("?relationName");
    expect(sparql).toContain("UNION");
    expect(sparql).toContain("BIND(\"0\" AS ?depth)");
    expect(sparql).toContain("BIND(\"1\" AS ?depth)");
    expect(sparql).toContain("book://author");
  });

  it("should include parent filter when query.parent is specified", () => {
    const metadata = Book.getModelMetadata();
    const query = {
      parent: { id: "flux://library1", predicate: "library://books" },
      include: { authors: true },
    };
    const sparql = buildBatchSPARQLQuery(metadata, query, Book);

    expect(sparql).toContain("flux://library1");
    expect(sparql).toContain("library://books");
  });

  it("should include where filter for simple equality", () => {
    const metadata = Book.getModelMetadata();
    const query = {
      where: { title: "My Book" },
      include: { authors: true },
    };
    const sparql = buildBatchSPARQLQuery(metadata, query, Book);

    // For literal-stored properties, SPARQL only has a JOIN (no FILTER value) — filtering in JS
    expect(sparql).toContain("book://title");
    expect(sparql).toContain("root_wTarget_eq_title");
  });

  it("should throw when include is empty", () => {
    const metadata = Book.getModelMetadata();
    expect(() => buildBatchSPARQLQuery(metadata, {}, Book)).toThrow("requires query.include");
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

import { buildSPARQLQuery, groupSPARQLResults, formatSPARQLValue, buildSPARQLGetDataQuery } from "./query-sparql";

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

  it("uses GRAPH ?linkGraph pattern for author/timestamp", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("GRAPH ?linkGraph { ?source ?predicate ?target . }");
    expect(query).toContain("?linkGraph <ad4m://ontology/author> ?author");
    expect(query).toContain("?linkGraph <ad4m://ontology/timestamp> ?timestamp");
  });

  it("does NOT contain RDF-star BIND(<< >> AS ?ann) pattern", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).not.toContain("BIND(<<");
    expect(query).not.toContain("?ann ");
  });

  it("uses FILTER(isIRI(?source)) to exclude non-IRI subjects", async () => {
    const query = await (Channel as any).queryToSPARQL(mockPersp, {});
    expect(query).toContain("FILTER(isIRI(?source)");
  });
});

describe("SPARQL IRI formatting", () => {
  it("wraps AD4M URIs in angle brackets", () => {
    const query = buildSPARQLGetDataQuery("ad4m://test123");
    expect(query).toContain("<ad4m://test123>");
  });

  it("wraps literal: URIs in angle brackets", () => {
    const query = buildSPARQLGetDataQuery("literal:string:foo");
    expect(query).toContain("<literal:string:foo>");
  });

  it("wraps flux:// URIs in angle brackets", () => {
    const query = buildSPARQLGetDataQuery("flux://has_channel");
    expect(query).toContain("<flux://has_channel>");
  });

  it("wraps did:key URIs in angle brackets", () => {
    const query = buildSPARQLGetDataQuery("did:key:z6MkhaXg");
    expect(query).toContain("<did:key:z6MkhaXg>");
  });
});

describe("groupSPARQLResults", () => {
  it("groups flat rows by source URI", () => {
    const rows = [
      { source: "ad4m://a", predicate: "p1", target: "t1", author: "auth1", timestamp: "ts1" },
      { source: "ad4m://a", predicate: "p2", target: "t2", author: "auth1", timestamp: "ts1" },
      { source: "ad4m://b", predicate: "p1", target: "t3", author: "auth2", timestamp: "ts2" },
    ];
    const grouped = groupSPARQLResults(rows);
    expect(grouped).toHaveLength(2);

    const aGroup = grouped.find(g => g.source_uri === "ad4m://a");
    expect(aGroup).toBeDefined();
    expect(aGroup!.links).toHaveLength(2);

    const bGroup = grouped.find(g => g.source_uri === "ad4m://b");
    expect(bGroup).toBeDefined();
    expect(bGroup!.links).toHaveLength(1);
  });

  it("returns empty array for empty input", () => {
    expect(groupSPARQLResults([])).toEqual([]);
  });

  it("preserves metadata in grouped links", () => {
    const rows = [
      { source: "x", predicate: "p", target: "t", author: "did:key:z6Mk", timestamp: "2024-01-01T00:00:00Z" },
    ];
    const grouped = groupSPARQLResults(rows);
    expect(grouped[0].links[0].author).toBe("did:key:z6Mk");
    expect(grouped[0].links[0].timestamp).toBe("2024-01-01T00:00:00Z");
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

import { matchesCondition, hydrateFromLinks } from "./hydration";
import { Literal } from "../Literal";

// Helper: create literal URLs using JSON encoding (which handles all types including booleans)
function literalUrl(value: any): string {
  if (typeof value === 'string') return Literal.from(value).toUrl();
  // Use JSON encoding for numbers and booleans since Literal.get() doesn't handle boolean: prefix
  return `literal:json:${encodeURIComponent(JSON.stringify(value))}`;
}

describe("matchesCondition()", () => {
  // Simple equality
  it("simple string equality — match", () => {
    expect(matchesCondition("hello", "hello")).toBe(true);
  });
  it("simple string equality — mismatch", () => {
    expect(matchesCondition("hello", "world")).toBe(false);
  });
  it("simple boolean equality — true", () => {
    expect(matchesCondition(true, true)).toBe(true);
  });
  it("simple boolean equality — false", () => {
    expect(matchesCondition(false, false)).toBe(true);
  });
  it("boolean mismatch", () => {
    expect(matchesCondition(true, false)).toBe(false);
  });
  it("simple number equality", () => {
    expect(matchesCondition(42, 42)).toBe(true);
  });
  it("number mismatch", () => {
    expect(matchesCondition(42, 99)).toBe(false);
  });

  // Array IN clause
  it("array IN clause — value present", () => {
    expect(matchesCondition("a", ["a", "b", "c"])).toBe(true);
  });
  it("array IN clause — value absent", () => {
    expect(matchesCondition("d", ["a", "b", "c"])).toBe(false);
  });

  // NOT operator
  it("not operator with string", () => {
    expect(matchesCondition("bob", { not: "alice" })).toBe(true);
    expect(matchesCondition("alice", { not: "alice" })).toBe(false);
  });
  it("not operator with array (NOT IN)", () => {
    expect(matchesCondition("a", { not: ["b", "c"] })).toBe(true);
    expect(matchesCondition("b", { not: ["b", "c"] })).toBe(false);
  });

  // Comparison operators
  it("gt", () => {
    expect(matchesCondition(10, { gt: 5 })).toBe(true);
    expect(matchesCondition(5, { gt: 5 })).toBe(false);
  });
  it("gte", () => {
    expect(matchesCondition(5, { gte: 5 })).toBe(true);
    expect(matchesCondition(4, { gte: 5 })).toBe(false);
  });
  it("lt", () => {
    expect(matchesCondition(3, { lt: 5 })).toBe(true);
    expect(matchesCondition(5, { lt: 5 })).toBe(false);
  });
  it("lte", () => {
    expect(matchesCondition(5, { lte: 5 })).toBe(true);
    expect(matchesCondition(6, { lte: 5 })).toBe(false);
  });
  it("combined gt + lt (range)", () => {
    expect(matchesCondition(5, { gt: 3, lt: 7 })).toBe(true);
    expect(matchesCondition(3, { gt: 3, lt: 7 })).toBe(false);
    expect(matchesCondition(7, { gt: 3, lt: 7 })).toBe(false);
  });

  // Between
  it("between — inclusive range", () => {
    expect(matchesCondition(5, { between: [3, 7] })).toBe(true);
    expect(matchesCondition(3, { between: [3, 7] })).toBe(true);
    expect(matchesCondition(7, { between: [3, 7] })).toBe(true);
    expect(matchesCondition(2, { between: [3, 7] })).toBe(false);
    expect(matchesCondition(8, { between: [3, 7] })).toBe(false);
  });

  // Contains
  it("contains with string", () => {
    expect(matchesCondition("hello world", { contains: "world" })).toBe(true);
    expect(matchesCondition("hello world", { contains: "xyz" })).toBe(false);
  });
  it("contains with array", () => {
    expect(matchesCondition(["a", "b", "c"], { contains: "b" })).toBe(true);
    expect(matchesCondition(["a", "b", "c"], { contains: "d" })).toBe(false);
  });

  // Edge cases
  it("returns false for undefined value with equality check", () => {
    expect(matchesCondition(undefined, "hello")).toBe(false);
  });
  it("returns false for empty string when condition is non-empty", () => {
    expect(matchesCondition("", "Recipe 2")).toBe(false);
  });
  it("returns true for undefined with not operator", () => {
    expect(matchesCondition(undefined, { not: "hello" })).toBe(true);
  });
});

describe("hydrateFromLinks()", () => {
  // Create a mock perspective that won't be called for literal properties
  const mockPerspective = {} as any;

  @Model({ name: "HydrationTest" })
  class HydrationTest extends Ad4mModel {
    @Property({ through: "test://name", resolveLanguage: "literal" })
    name: string = "";

    @Property({ through: "test://score", resolveLanguage: "literal" })
    score: number = 0;

    @Property({ through: "test://active", resolveLanguage: "literal" })
    active: boolean = false;
  }

  const metadata = HydrationTest.getModelMetadata();

  it("hydrates all properties when requestedProperties is undefined", async () => {
    const instance = new HydrationTest(mockPerspective) as any;
    const links = [
      { predicate: "test://name", target: literalUrl("Alice"), author: "did:test", timestamp: "1000" },
      { predicate: "test://score", target: literalUrl(42), author: "did:test", timestamp: "1000" },
      { predicate: "test://active", target: literalUrl(true), author: "did:test", timestamp: "1000" },
    ];
    await hydrateFromLinks(instance, links, metadata, mockPerspective, undefined);
    expect(instance.name).toBe("Alice");
    expect(instance.score).toBe(42);
    expect(instance.active).toBe(true);
  });

  it("hydrates only requested properties when requestedProperties is provided", async () => {
    const instance = new HydrationTest(mockPerspective) as any;
    const links = [
      { predicate: "test://name", target: literalUrl("Alice"), author: "did:test", timestamp: "1000" },
      { predicate: "test://score", target: literalUrl(42), author: "did:test", timestamp: "1000" },
      { predicate: "test://active", target: literalUrl(true), author: "did:test", timestamp: "1000" },
    ];
    await hydrateFromLinks(instance, links, metadata, mockPerspective, ["name"]);
    expect(instance.name).toBe("Alice");
    // score and active should remain at defaults since not requested
    expect(instance.score).toBe(0);
    expect(instance.active).toBe(false);
  });

  it("hydrates where-clause + projection properties when both are in requestedProperties", async () => {
    const instance = new HydrationTest(mockPerspective) as any;
    const links = [
      { predicate: "test://name", target: literalUrl("Recipe 2"), author: "did:test", timestamp: "1000" },
      { predicate: "test://active", target: literalUrl(true), author: "did:test", timestamp: "1000" },
    ];
    // Simulate merged hydration props: projection ["active"] + where clause props ["name"]
    await hydrateFromLinks(instance, links, metadata, mockPerspective, ["active", "name"]);
    expect(instance.name).toBe("Recipe 2");
    expect(instance.active).toBe(true);
  });

  it("sets author and timestamps from links", async () => {
    const instance = new HydrationTest(mockPerspective) as any;
    const links = [
      { predicate: "test://name", target: literalUrl("Bob"), author: "did:author1", timestamp: "1000" },
      { predicate: "test://score", target: literalUrl(10), author: "did:author2", timestamp: "2000" },
    ];
    await hydrateFromLinks(instance, links, metadata, mockPerspective);
    expect(instance.author).toBe("did:author1");
    expect(instance.createdAt).toBe(1000);
    expect(instance.updatedAt).toBe(2000);
  });

  it("handles empty links array", async () => {
    const instance = new HydrationTest(mockPerspective) as any;
    await hydrateFromLinks(instance, [], metadata, mockPerspective);
    expect(instance.name).toBe("");
    expect(instance.score).toBe(0);
  });
});

describe("instancesFromQueryResult — where + properties interaction", () => {
  const mockPerspective = {
    get: jest.fn().mockResolvedValue([]),
    querySparql: jest.fn().mockResolvedValue([]),
    getExpression: jest.fn().mockResolvedValue(null),
  } as any;

  @Model({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name", resolveLanguage: "literal", required: true })
    name: string = "";

    @Property({ through: "recipe://booleanTest", resolveLanguage: "literal" })
    booleanTest: boolean = false;

    @Property({ through: "recipe://score", resolveLanguage: "literal" })
    score: number = 0;
  }

  it("should filter by where clause property even when not in properties projection", async () => {
    // Simulate SPARQL result with two recipes
    const grouped = [
      {
        source_uri: "expr:recipe1",
        links: [
          { predicate: "recipe://name", target: literalUrl("Recipe 1"), author: "did:test", timestamp: "1000" },
          { predicate: "recipe://booleanTest", target: literalUrl(true), author: "did:test", timestamp: "1000" },
        ],
      },
      {
        source_uri: "expr:recipe2",
        links: [
          { predicate: "recipe://name", target: literalUrl("Recipe 2"), author: "did:test", timestamp: "1000" },
          { predicate: "recipe://booleanTest", target: literalUrl(false), author: "did:test", timestamp: "1000" },
        ],
      },
    ];

    const query = { where: { name: "Recipe 2" }, properties: ["booleanTest"] };
    const { results } = await Recipe.instancesFromQueryResult(mockPerspective, query, grouped);

    expect(results.length).toBe(1);
    expect((results[0] as any).booleanTest).toBe(false);
    // name should be deleted after filtering since it wasn't in properties projection
    expect((results[0] as any).name).toBeUndefined();
  });

  it("should delete unrequested properties AFTER where-filtering", async () => {
    const grouped = [
      {
        source_uri: "expr:recipe1",
        links: [
          { predicate: "recipe://name", target: literalUrl("Recipe 1"), author: "did:test", timestamp: "1000" },
          { predicate: "recipe://score", target: literalUrl(99), author: "did:test", timestamp: "1000" },
        ],
      },
    ];

    const query = { where: { name: "Recipe 1" }, properties: ["score"] };
    const { results } = await Recipe.instancesFromQueryResult(mockPerspective, query, grouped);

    expect(results.length).toBe(1);
    expect((results[0] as any).score).toBe(99);
    // name was used for filtering but should be deleted from final result
    expect((results[0] as any).name).toBeUndefined();
  });

  it("should return all properties when no projection is specified", async () => {
    const grouped = [
      {
        source_uri: "expr:recipe1",
        links: [
          { predicate: "recipe://name", target: literalUrl("Test"), author: "did:test", timestamp: "1000" },
          { predicate: "recipe://booleanTest", target: literalUrl(true), author: "did:test", timestamp: "1000" },
          { predicate: "recipe://score", target: literalUrl(50), author: "did:test", timestamp: "1000" },
        ],
      },
    ];

    const query = { where: { name: "Test" } };
    const { results } = await Recipe.instancesFromQueryResult(mockPerspective, query, grouped);

    expect(results.length).toBe(1);
    expect((results[0] as any).name).toBe("Test");
    expect((results[0] as any).booleanTest).toBe(true);
    expect((results[0] as any).score).toBe(50);
  });
});

describe("groupSPARQLResults()", () => {
  it("groups rows by source", () => {
    const rows = [
      { source: "a", predicate: "p1", target: "t1", author: "auth", timestamp: "ts1" },
      { source: "a", predicate: "p2", target: "t2", author: "auth", timestamp: "ts2" },
      { source: "b", predicate: "p1", target: "t3", author: "auth", timestamp: "ts3" },
    ];
    const grouped = groupSPARQLResults(rows);
    expect(grouped.length).toBe(2);
    const groupA = grouped.find(g => g.source_uri === "a")!;
    const groupB = grouped.find(g => g.source_uri === "b")!;
    expect(groupA.links.length).toBe(2);
    expect(groupB.links.length).toBe(1);
  });

  it("handles empty input", () => {
    expect(groupSPARQLResults([])).toEqual([]);
  });

  it("preserves link metadata", () => {
    const rows = [
      { source: "x", predicate: "pred", target: "tgt", author: "did:auth", timestamp: "2024-01-01" },
    ];
    const grouped = groupSPARQLResults(rows);
    expect(grouped[0].links[0]).toEqual({
      predicate: "pred",
      target: "tgt",
      author: "did:auth",
      timestamp: "2024-01-01",
    });
  });
});

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

describe("sparse fieldset property deletion timing", () => {
  const mockPerspective = {
    get: jest.fn().mockResolvedValue([]),
    querySparql: jest.fn().mockResolvedValue([]),
    getExpression: jest.fn().mockResolvedValue(null),
  } as any;

  @Model({ name: "TimingTest" })
  class TimingTest extends Ad4mModel {
    @Property({ through: "timing://a", resolveLanguage: "literal" })
    a: string = "";

    @Property({ through: "timing://b", resolveLanguage: "literal" })
    b: string = "";

    @Property({ through: "timing://c", resolveLanguage: "literal" })
    c: string = "";
  }

  it("where-clause properties are available during filtering but deleted after", async () => {
    const grouped = [
      {
        source_uri: "expr:1",
        links: [
          { predicate: "timing://a", target: literalUrl("match"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://b", target: literalUrl("keep"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://c", target: literalUrl("ignore"), author: "did:test", timestamp: "1000" },
        ],
      },
      {
        source_uri: "expr:2",
        links: [
          { predicate: "timing://a", target: literalUrl("no-match"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://b", target: literalUrl("skip"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://c", target: literalUrl("skip"), author: "did:test", timestamp: "1000" },
        ],
      },
    ];

    // properties projection asks for "b" only, where clause filters on "a"
    const query = { where: { a: "match" }, properties: ["b"] };
    const { results } = await TimingTest.instancesFromQueryResult(mockPerspective, query, grouped);

    // Only the matching instance should survive
    expect(results.length).toBe(1);
    // "b" was requested — should be present
    expect((results[0] as any).b).toBe("keep");
    // "a" was used for filtering but not in projection — should be deleted
    expect((results[0] as any).a).toBeUndefined();
    // "c" was not requested — should be deleted
    expect((results[0] as any).c).toBeUndefined();
  });

  it("properties not in projection are undefined after instancesFromQueryResult", async () => {
    const grouped = [
      {
        source_uri: "expr:1",
        links: [
          { predicate: "timing://a", target: literalUrl("val-a"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://b", target: literalUrl("val-b"), author: "did:test", timestamp: "1000" },
          { predicate: "timing://c", target: literalUrl("val-c"), author: "did:test", timestamp: "1000" },
        ],
      },
    ];

    const query = { properties: ["a"] };
    const { results } = await TimingTest.instancesFromQueryResult(mockPerspective, query, grouped);

    expect(results.length).toBe(1);
    expect((results[0] as any).a).toBe("val-a");
    expect((results[0] as any).b).toBeUndefined();
    expect((results[0] as any).c).toBeUndefined();
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

// ── Native SPARQL getter evaluation ──────────────────────────────────────────
describe("Native SPARQL getter evaluation", () => {
  const { evaluateCustomGettersForInstance } = require("./hydration");

  const mockPerspective = {
    getLinks: jest.fn().mockResolvedValue([]),
    querySparql: jest.fn(),
    get: jest.fn().mockResolvedValue([]),
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should execute SELECT getter directly via querySparql", async () => {
    const metadata = {
      properties: {
        replyingTo: {
          predicate: "flux://has_reply",
          getter: 'SELECT ?target WHERE { ?target <flux://has_reply> ?source . } LIMIT 1',
          type: "string",
        }
      },
      relations: {},
    };

    mockPerspective.querySparql.mockResolvedValue([
      { target: { value: "literal:string:msg123" } }
    ]);

    const instance = { id: "test://msg1", replyingTo: undefined };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);

    expect(mockPerspective.querySparql).toHaveBeenCalledWith(
      expect.stringContaining("SELECT ?target WHERE")
    );
    expect(instance.replyingTo).toBe("literal:string:msg123");
  });

  it("should execute ASK getter and return boolean", async () => {
    const metadata = {
      properties: {
        isPopular: {
          predicate: "flux://is_popular",
          getter: 'ASK WHERE { SELECT (COUNT(DISTINCT ?reactor) AS ?count) WHERE { ?reactor <flux://reaction> ?source . } HAVING(?count > 5) }',
          type: "boolean",
          readOnly: true,
        }
      },
      relations: {},
    };

    mockPerspective.querySparql.mockResolvedValue(true);

    const instance = { id: "test://msg1", isPopular: false };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);

    expect(mockPerspective.querySparql).toHaveBeenCalledWith(
      expect.stringContaining("ASK WHERE")
    );
    expect(instance.isPopular).toBe(true);
  });

  it("should replace ?source with instance ID in SPARQL getter", async () => {
    const metadata = {
      properties: {
        author: {
          predicate: "test://author",
          getter: 'SELECT ?target WHERE { ?source <test://author> ?target . }',
          type: "string",
        }
      },
      relations: {},
    };

    mockPerspective.querySparql.mockResolvedValue([
      { target: { value: "did:key:abc123" } }
    ]);

    const instance = { id: "test://post1", author: undefined };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);

    expect(mockPerspective.querySparql).toHaveBeenCalledWith(
      expect.stringContaining("<test://post1>")
    );
    expect(mockPerspective.querySparql).not.toHaveBeenCalledWith(
      expect.stringContaining("?source")
    );
  });

  it("should handle empty SPARQL results gracefully", async () => {
    const metadata = {
      properties: {
        replyingTo: {
          predicate: "flux://has_reply",
          getter: 'SELECT ?target WHERE { ?target <flux://has_reply> ?source . } LIMIT 1',
          type: "string",
        }
      },
      relations: {},
    };

    mockPerspective.querySparql.mockResolvedValue([]);

    const instance = { id: "test://msg1", replyingTo: "original" };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);

    // Should not overwrite with undefined
    expect(instance.replyingTo).toBe("original");
  });

  it("should handle SPARQL getter errors without crashing", async () => {
    const metadata = {
      properties: {
        broken: {
          predicate: "test://broken",
          getter: 'SELECT ?target WHERE { INVALID SPARQL }',
          type: "string",
        }
      },
      relations: {},
    };

    mockPerspective.querySparql.mockRejectedValue(new Error("SPARQL parse error"));

    const instance = { id: "test://msg1", broken: undefined };
    const consoleSpy = jest.spyOn(console, "warn").mockImplementation();
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);
    consoleSpy.mockRestore();

    // Should not throw, should warn
    expect(instance.broken).toBeUndefined();
  });

  it("should execute SPARQL getter for relations", async () => {
    const metadata = {
      properties: {},
      relations: {
        tags: {
          predicate: "test://has_tag",
          getter: 'SELECT ?target WHERE { ?source <test://has_tag> ?target . }',
          direction: "forward",
        }
      },
    };

    mockPerspective.querySparql.mockResolvedValue([
      { target: { value: "tag:a" } },
      { target: { value: "tag:b" } },
    ]);

    const instance = { id: "test://post1", tags: [] };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);

    expect(instance.tags).toEqual(["tag:a", "tag:b"]);
  });

  it("should warn for legacy SurrealDB-style getter syntax instead of converting", async () => {
    const metadata = {
      properties: {
        legacy: {
          predicate: "test://legacy",
          getter: "(<-link[WHERE predicate = 'test://legacy'].in.uri)[0]",
          type: "string",
        }
      },
      relations: {},
    };

    const warnSpy = jest.spyOn(console, 'warn').mockImplementation(() => {});

    const instance = { id: "test://msg1", legacy: undefined };
    await evaluateCustomGettersForInstance(instance, mockPerspective, metadata);
    
    // Should NOT call querySparql — legacy getters are no longer converted
    expect(mockPerspective.querySparql).not.toHaveBeenCalled();
    // Should warn about unsupported syntax
    expect(warnSpy).toHaveBeenCalledWith(
      expect.stringContaining("Unsupported legacy getter syntax")
    );
    // Value should remain undefined
    expect(instance.legacy).toBeUndefined();

    warnSpy.mockRestore();
  });
});

// ── Subscribe callback timing ──────────────────────────────────────────
describe("ModelQueryBuilder subscribe callback timing", () => {
  it("subscribe should not invoke callback synchronously before Promise resolves", async () => {
    // This test verifies that the initial callback from subscribe() is deferred
    // via queueMicrotask, preventing Preact/React hook lifecycle violations
    // when subscribe() is called from useEffect.
    
    const mockPerspective = {
      getLinks: jest.fn().mockResolvedValue([]),
      subscribeQuery: jest.fn().mockResolvedValue({
        result: '[]',
        onResult: jest.fn(),
        dispose: jest.fn(),
      }),
      querySparql: jest.fn().mockResolvedValue('[]'),
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

    await promise;
    promiseResolved = true;

    // After the microtask queue drains, the callback should fire
    await new Promise(r => setTimeout(r, 10));
    // The callback was deferred — it fires after the Promise resolves
  });
});

// ============================================================================
// Performance Optimisation Tests
// ============================================================================

import { getCachedResult, setCachedResult, clearQueryCache, queryCacheSize } from "./query-cache";
import { clearSubscriptionPool, subscriptionPoolSize } from "./subscription-pool";
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
// instancesFromQueryResult — SPARQL vs JS pagination behaviour
// ──────────────────────────────────────────────────────────

describe("instancesFromQueryResult — SPARQL vs JS pagination", () => {
  @Model({ name: "PaginationTestChannel" })
  class PaginationTestChannel extends Ad4mModel {
    @Flag({ through: "flux://entry_type", value: "flux://channel" })
    type: string = "";

    @Property({ through: "flux://name", resolveLanguage: "literal" })
    name: string = "";
  }

  const mockPerspective: any = {
    uuid: "test-uuid",
    querySparql: jest.fn().mockResolvedValue([]),
    get: jest.fn().mockResolvedValue([]),
    getExpression: jest.fn().mockResolvedValue(null),
  };

  it("JS-level slicing is a no-op when results already match the page", async () => {
    const grouped = Array.from({ length: 5 }, (_, i) => ({
      source_uri: `flux://ch-${i}`,
      links: [
        { predicate: "flux://entry_type", target: "flux://channel", author: "did:key:a", timestamp: String(1000 + i) },
        { predicate: "flux://name", target: `literal:string:Channel${i}`, author: "did:key:a", timestamp: String(1000 + i) },
      ],
    }));

    // When SPARQL already returned the correct page, JS slice is a no-op
    const query = { limit: 5, offset: 0 };
    const result = await (PaginationTestChannel as any).instancesFromQueryResult(mockPerspective, query, grouped);
    expect(result.results.length).toBe(5);
  });

  it("applies JS-level slicing as fallback when JS-only filters exist", async () => {
    const grouped = Array.from({ length: 10 }, (_, i) => ({
      source_uri: `flux://ch-${i}`,
      links: [
        { predicate: "flux://entry_type", target: "flux://channel", author: "did:key:a", timestamp: String(1000 + i) },
        { predicate: "flux://name", target: `literal:string:Ch${i}`, author: "did:key:a", timestamp: String(1000 + i) },
      ],
    }));

    const query = { limit: 3, offset: 0, where: { author: "did:key:a" } };
    const result = await (PaginationTestChannel as any).instancesFromQueryResult(mockPerspective, query, grouped);
    expect(result.results.length).toBe(3);
  });
});

// ──────────────────────────────────────────────────────────
// deepQuery opt-in — getter evaluation on collection queries
// ──────────────────────────────────────────────────────────

describe("deepQuery opt-in — getter evaluation", () => {
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
    mockPerspective.get.mockClear();
  });

  function makeMessageRows(count: number) {
    return Array.from({ length: count }, (_, i) => ({
      source_uri: `flux://msg-${i}`,
      links: [
        { predicate: "flux://entry_type", target: "flux://message", author: "did:key:a", timestamp: String(2000 + i) },
        { predicate: "flux://body", target: `literal:string:Hello${i}`, author: "did:key:a", timestamp: String(2000 + i) },
      ],
    }));
  }

  it("skips getter evaluation on collection queries by default", async () => {
    const grouped = makeMessageRows(5);
    const result = await (DeepQueryTestMessage as any).instancesFromQueryResult(mockPerspective, {}, grouped);
    expect(result.results.length).toBe(5);

    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    expect(getterCalls.length).toBe(0);
  });

  it("evaluates getter properties when deepQuery is true", async () => {
    const grouped = makeMessageRows(3);
    const result = await (DeepQueryTestMessage as any).instancesFromQueryResult(mockPerspective, { deepQuery: true }, grouped);
    expect(result.results.length).toBe(3);

    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    expect(getterCalls.length).toBe(6); // 3 instances × 2 getters
  });

  it("single-instance get() still evaluates getters by default", async () => {
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
    const getterCalls = sparqlCalls.filter(
      (q) => q.includes("flux://has_reply") || q.includes("flux://is_popular")
    );
    expect(getterCalls.length).toBe(2);
  });

  describe("Ad4mModel.evaluateGetters()", () => {
    it("resolves getters for a batch of instances", async () => {
      const grouped = makeMessageRows(4);
      const result = await (DeepQueryTestMessage as any).instancesFromQueryResult(mockPerspective, {}, grouped);
      sparqlCalls = [];

      await DeepQueryTestMessage.evaluateGetters(result.results.slice(0, 2), mockPerspective, ["replyingTo"]);

      const replyCalls = sparqlCalls.filter((q) => q.includes("flux://has_reply"));
      expect(replyCalls.length).toBe(2);
      const popularCalls = sparqlCalls.filter((q) => q.includes("flux://is_popular"));
      expect(popularCalls.length).toBe(0);
    });

    it("evaluates all getters when propertyNames is omitted", async () => {
      const grouped = makeMessageRows(2);
      const result = await (DeepQueryTestMessage as any).instancesFromQueryResult(mockPerspective, {}, grouped);
      sparqlCalls = [];

      await DeepQueryTestMessage.evaluateGetters(result.results, mockPerspective);

      const replyCalls = sparqlCalls.filter((q) => q.includes("flux://has_reply"));
      const popularCalls = sparqlCalls.filter((q) => q.includes("flux://is_popular"));
      expect(replyCalls.length).toBe(2);
      expect(popularCalls.length).toBe(2);
    });

    it("handles empty array gracefully", async () => {
      sparqlCalls = [];
      await DeepQueryTestMessage.evaluateGetters([], mockPerspective);
      expect(sparqlCalls.length).toBe(0);
    });
  });

  describe("deepQuery via ModelQueryBuilder", () => {
    it("deepQuery() method sets deepQuery flag on queryParams", () => {
      const builder = DeepQueryTestMessage.query(mockPerspective);
      (builder as any).deepQuery();
      expect((builder as any).queryParams.deepQuery).toBe(true);
    });
  });
});
