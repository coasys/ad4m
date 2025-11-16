import { Ad4mModel } from "./Ad4mModel";
import { ModelOptions, Property, Optional, ReadOnly, Collection, Flag } from "./decorators";

describe("Ad4mModel.getModelMetadata()", () => {
  it("should extract basic model metadata with className", () => {
    @ModelOptions({ name: "SimpleModel" })
    class SimpleModel extends Ad4mModel {}

    const metadata = SimpleModel.getModelMetadata();
    
    expect(metadata.className).toBe("SimpleModel");
    expect(metadata.properties).toEqual({});
    expect(metadata.collections).toEqual({});
  });

  it("should extract property metadata with all fields", () => {
    @ModelOptions({ name: "PropertyModel" })
    class PropertyModel extends Ad4mModel {
      @Property({ through: "test://name", resolveLanguage: "literal" })
      name: string = "";
      
      @Optional({ through: "test://optional", writable: true })
      optional: string = "";
      
      @ReadOnly({ through: "test://readonly", getter: "custom_getter" })
      readonly: string = "";
      
      @Flag({ through: "test://type", value: "test://flag" })
      type: string = "";
    }

    const metadata = PropertyModel.getModelMetadata();
    
    // Should have 4 properties
    expect(Object.keys(metadata.properties)).toHaveLength(4);
    
    // Verify "name" property
    expect(metadata.properties.name.predicate).toBe("test://name");
    expect(metadata.properties.name.required).toBe(true);
    expect(metadata.properties.name.writable).toBe(true);
    expect(metadata.properties.name.resolveLanguage).toBe("literal");
    
    // Verify "optional" property
    expect(metadata.properties.optional.predicate).toBe("test://optional");
    expect(metadata.properties.optional.writable).toBe(true);
    
    // Verify "readonly" property
    expect(metadata.properties.readonly.predicate).toBe("test://readonly");
    expect(metadata.properties.readonly.writable).toBe(false);
    expect(metadata.properties.readonly.getter).toBe("custom_getter");
    
    // Verify "type" property (flag)
    expect(metadata.properties.type.predicate).toBe("test://type");
    expect(metadata.properties.type.flag).toBe(true);
    expect(metadata.properties.type.initial).toBe("test://flag");
  });

  it("should extract collection metadata with where clauses", () => {
    @ModelOptions({ name: "CollectionModel" })
    class CollectionModel extends Ad4mModel {
      @Collection({ through: "test://items" })
      items: string[] = [];
      
      @Collection({ 
        through: "test://filtered",
        where: { condition: "triple(Target, 'test://active', 'true')" }
      })
      filtered: string[] = [];
      
      @Collection({ through: "test://local", local: true })
      local: string[] = [];
    }

    const metadata = CollectionModel.getModelMetadata();
    
    // Should have 3 collections
    expect(Object.keys(metadata.collections)).toHaveLength(3);
    
    // Verify "items" collection
    expect(metadata.collections.items.predicate).toBe("test://items");
    expect(metadata.collections.items.where).toBeUndefined();
    
    // Verify "filtered" collection
    expect(metadata.collections.filtered.predicate).toBe("test://filtered");
    expect(metadata.collections.filtered.where?.condition).toBe("triple(Target, 'test://active', 'true')");
    
    // Verify "local" collection
    expect(metadata.collections.local.predicate).toBe("test://local");
    expect(metadata.collections.local.local).toBe(true);
  });

  it("should extract transform function from property metadata", () => {
    @ModelOptions({ name: "TransformModel" })
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
    @ModelOptions({ name: "CustomModel" })
    class CustomModel extends Ad4mModel {
      @Optional({
        through: "test://computed",
        getter: "triple(Base, 'test://value', V), Value is V * 2",
        setter: "Value is V / 2, Actions = [{action: 'setSingleTarget', source: 'this', predicate: 'test://value', target: Value}]"
      })
      computed: number = 0;
    }

    const metadata = CustomModel.getModelMetadata();
    
    // Assert getter and setter contain the custom code
    expect(metadata.properties.computed.getter).toContain("triple(Base, 'test://value', V), Value is V * 2");
    expect(metadata.properties.computed.setter).toContain("Value is V / 2");
    expect(metadata.properties.computed.setter).toContain("setSingleTarget");
  });

  it("should handle collection with isInstance where clause", () => {
    @ModelOptions({ name: "Comment" })
    class Comment extends Ad4mModel {}
    
    @ModelOptions({ name: "Post" })
    class Post extends Ad4mModel {
      @Collection({ 
        through: "post://comment",
        where: { isInstance: Comment }
      })
      comments: string[] = [];
    }

    const metadata = Post.getModelMetadata();
    
    // Assert isInstance is defined
    expect(metadata.collections.comments.where?.isInstance).toBeDefined();
  });

  it("should throw error for class without @ModelOptions decorator", () => {
    class NoDecoratorModel extends Ad4mModel {}

    // Assert that calling getModelMetadata throws an error
    expect(() => NoDecoratorModel.getModelMetadata()).toThrow("Model class must be decorated with @ModelOptions");
  });

  it("should handle complex model with mixed property and collection types", () => {
    @ModelOptions({ name: "Recipe" })
    class Recipe extends Ad4mModel {
      @Property({ through: "recipe://name", resolveLanguage: "literal" })
      name: string = "";
      
      @Optional({ through: "recipe://description" })
      description: string = "";
      
      @ReadOnly({ through: "recipe://rating", getter: "avg_rating(Base, Value)" })
      rating: number = 0;
      
      @Collection({ through: "recipe://ingredient" })
      ingredients: string[] = [];
      
      @Collection({ through: "recipe://step", local: true })
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
    
    // Assert collections has 2 entries
    expect(Object.keys(metadata.collections)).toHaveLength(2);
    expect(metadata.collections.ingredients).toBeDefined();
    expect(metadata.collections.steps).toBeDefined();
    
    // Verify all metadata fields are correctly extracted
    expect(metadata.properties.name.predicate).toBe("recipe://name");
    expect(metadata.properties.name.resolveLanguage).toBe("literal");
    expect(metadata.properties.description.predicate).toBe("recipe://description");
    expect(metadata.properties.rating.predicate).toBe("recipe://rating");
    expect(metadata.properties.rating.getter).toBe("avg_rating(Base, Value)");
    expect(metadata.collections.ingredients.predicate).toBe("recipe://ingredient");
    expect(metadata.collections.steps.predicate).toBe("recipe://step");
    expect(metadata.collections.steps.local).toBe(true);
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
    expect(metadata.properties.name.writable).toBe(true);
    expect(metadata.properties.name.resolveLanguage).toBe("literal");

    expect(metadata.properties.price).toBeDefined();
    expect(metadata.properties.price.predicate).toBe("product://price");
    expect(metadata.properties.price.required).toBe(true);
    expect(metadata.properties.price.resolveLanguage).toBe("literal");

    expect(metadata.properties.description).toBeDefined();
    expect(metadata.properties.description.predicate).toBe("product://description");
    expect(metadata.properties.description.required).toBe(false);
  });

  it("should extract collections from a model created via fromJSONSchema with arrays", () => {
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

    // Verify collections are extracted
    expect(Object.keys(metadata.collections).length).toBeGreaterThan(0);
    expect(metadata.collections.tags).toBeDefined();
    expect(metadata.collections.tags.predicate).toBe("post://tags");

    expect(metadata.collections.comments).toBeDefined();
    expect(metadata.collections.comments.predicate).toBe("post://comments");

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
    expect(metadata.properties.name.writable).toBe(true);
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

    // Verify collections
    expect(metadata.collections.authors).toBeDefined();
    expect(metadata.collections.authors.predicate).toBe("article://authors");

    expect(metadata.collections.tags).toBeDefined();
    expect(metadata.collections.tags.predicate).toBe("article://tags");
    expect(metadata.collections.tags.local).toBe(true);
  });

  it("should handle models with only an auto-generated type flag", () => {
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

    // Should have the auto-generated __ad4m_type property
    expect(metadata.properties.__ad4m_type).toBeDefined();
    expect(metadata.properties.__ad4m_type.predicate).toBe("ad4m://type");
    expect(metadata.properties.__ad4m_type.initial).toBe("empty://instance");
    expect(metadata.properties.__ad4m_type.flag).toBe(true);
  });
});

describe("Ad4mModel.queryToSurrealQL()", () => {
  // Mock perspective proxy (minimal since queryToSurrealQL doesn't actually call it)
  const mockPerspective = {} as any;

  // Helper function to normalize whitespace in queries for easier comparison
  function normalizeQuery(query: string): string {
    return query.replace(/\s+/g, ' ').trim();
  }

  // Test Recipe model
  @ModelOptions({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name" })
    name: string = "";
    
    @Property({ through: "recipe://rating" })
    rating: number = 0;
    
    @Collection({ through: "recipe://ingredient" })
    ingredients: string[] = [];
  }

  it("should generate basic query with no filters", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {});
    
    expect(query).toContain("WITH candidate_bases AS");
    expect(query).toContain("SELECT DISTINCT source AS base");
    expect(query).toContain("FROM link");
    expect(query).toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'recipe://name' LIMIT 1) AS name");
    expect(query).toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'recipe://ingredient') AS ingredients");
    expect(query).toContain("(SELECT author FROM link WHERE source = cb.base LIMIT 1) AS author");
    expect(query).toContain("(SELECT timestamp FROM link WHERE source = cb.base LIMIT 1) AS timestamp");
    // Check that there's no WHERE clause in the CTE (candidate_bases should not filter)
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("FROM link )");
  });

  it("should generate query with simple property filter", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: "Pasta" } });
    
    expect(query).toContain("WHERE source IN (SELECT source FROM link WHERE predicate = 'recipe://name' AND target = 'Pasta')");
  });

  it("should generate query with multiple property filters", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: "Pasta", rating: 5 } });
    
    expect(query).toContain("WHERE");
    expect(query).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://name' AND target = 'Pasta')");
    expect(query).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://rating' AND target = 5)");
    expect(query).toContain("AND");
  });

  it("should handle gt operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { gt: 4 } } });
    
    expect(query).toContain("target > 4");
  });

  it("should handle lt operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { lt: 3 } } });
    
    expect(query).toContain("target < 3");
  });

  it("should handle gte and lte operators", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { gte: 3, lte: 5 } } });
    
    expect(query).toContain("target >= 3");
    expect(query).toContain("target <= 5");
  });

  it("should handle not operator with single value", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: { not: "Salad" } } });
    
    expect(query).toContain("target != 'Salad'");
  });

  it("should handle not operator with array (NOT IN)", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: { not: ["Salad", "Soup"] } } });
    
    expect(query).toContain("target NOT IN ['Salad', 'Soup']");
  });

  it("should handle between operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { between: [3, 5] } } });
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("target >= 3 AND");
    expect(normalized).toContain("target <= 5");
  });

  it("should handle array values (IN clause)", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: ["Pasta", "Pizza"] } });
    
    expect(query).toContain("target IN ['Pasta', 'Pizza']");
  });

  it("should handle special fields (author, timestamp) without subqueries", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { author: "did:key:alice" } });
    
    expect(query).toContain("WHERE author = 'did:key:alice'");
    // Ensure it's NOT using a subquery pattern for author in WHERE clause
    expect(normalizeQuery(query)).not.toMatch(/WHERE.*source IN.*SELECT source FROM link.*author/);
  });

  it("should generate ORDER BY clause", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { order: { timestamp: "DESC" } });
    
    expect(query).toContain("ORDER BY timestamp DESC");
  });

  it("should generate LIMIT clause", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { limit: 10 });
    
    expect(query).toContain("LIMIT 10");
  });

  it("should generate START clause for offset", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { offset: 20 });
    
    expect(query).toContain("START 20");
  });

  it("should generate complete query with all options", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "Pasta", rating: { gt: 4 } },
      order: { timestamp: "DESC" },
      limit: 10,
      offset: 20
    });
    
    expect(query).toContain("WHERE");
    expect(query).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://name' AND target = 'Pasta')");
    expect(query).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://rating' AND target > 4)");
    expect(query).toContain("ORDER BY timestamp DESC");
    expect(query).toContain("LIMIT 10");
    expect(query).toContain("START 20");
  });

  it("should only select requested properties", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { properties: ["name"] });
    
    expect(query).toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'recipe://name' LIMIT 1) AS name");
    // Should NOT contain rating property
    expect(query).not.toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'recipe://rating' LIMIT 1) AS rating");
    // Should still contain author/timestamp
    expect(query).toContain("AS author");
    expect(query).toContain("AS timestamp");
  });

  it("should only select requested collections", () => {
    @ModelOptions({ name: "MultiCollectionModel" })
    class MultiCollectionModel extends Ad4mModel {
      @Collection({ through: "test://coll1" })
      coll1: string[] = [];
      
      @Collection({ through: "test://coll2" })
      coll2: string[] = [];
    }

    const query = MultiCollectionModel.queryToSurrealQL(mockPerspective, { collections: ["coll1"] });
    
    expect(query).toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'test://coll1') AS coll1");
    // Should NOT contain coll2
    expect(query).not.toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'test://coll2') AS coll2");
  });

  it("should escape single quotes in string values", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { name: "O'Brien's Recipe" } });
    
    expect(query).toContain("'O''Brien''s Recipe'");
  });

  it("should handle numeric values without quotes", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: 5 } });
    
    // Numeric values should not have quotes around them
    expect(query).toContain("target = 5");
    expect(query).not.toContain("target = '5'");
  });

  it("should handle complex nested query", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        name: "Pasta",
        rating: { gte: 4, lte: 5 },
        author: "did:key:alice"
      },
      order: { rating: "DESC" },
      limit: 5
    });
    
    const normalized = normalizeQuery(query);
    
    // Verify all WHERE conditions are present
    expect(normalized).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://name' AND target = 'Pasta')");
    expect(normalized).toContain("target >= 4");
    expect(normalized).toContain("target <= 5");
    expect(normalized).toContain("author = 'did:key:alice'");
    
    // Verify ORDER BY and LIMIT
    expect(normalized).toContain("ORDER BY rating DESC");
    expect(normalized).toContain("LIMIT 5");
    
    // Verify query structure
    expect(normalized).toContain("WITH candidate_bases AS");
    expect(normalized).toContain("FROM candidate_bases cb");
  });

  it("should handle empty query object", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {});
    
    // Should generate valid query without WHERE clause in CTE, ORDER BY, LIMIT, or START
    expect(query).toContain("WITH candidate_bases AS");
    expect(query).toContain("SELECT DISTINCT source AS base");
    expect(query).toContain("FROM link");
    // Check that there's no WHERE clause in the CTE
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("FROM link )");
    expect(query).not.toContain("ORDER BY");
    expect(query).not.toContain("START");
    // Check that query ends with "FROM candidate_bases cb" (no LIMIT/ORDER BY/START at end)
    expect(normalized).toMatch(/FROM candidate_bases cb$/);
  });

  it("should handle base special field", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: "literal://test" } });
    
    expect(query).toContain("WHERE source = 'literal://test'");
  });

  it("should handle base special field with array (IN clause)", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: ["literal://test1", "literal://test2"] } });
    
    expect(query).toContain("WHERE source IN ['literal://test1', 'literal://test2']");
  });

  it("should handle base special field with not operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: { not: "literal://test" } } });
    
    expect(query).toContain("WHERE source != 'literal://test'");
  });

  it("should handle base special field with not operator and array (NOT IN)", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: { not: ["literal://test1", "literal://test2"] } } });
    
    expect(query).toContain("WHERE source NOT IN ['literal://test1', 'literal://test2']");
  });

  it("should handle base special field with between operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: { between: ["literal://a", "literal://z"] } } } as any);
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("WHERE source >= 'literal://a' AND source <= 'literal://z'");
  });

  it("should handle base special field with gt operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: { gt: "literal://m" } } } as any);
    
    expect(query).toContain("WHERE source > 'literal://m'");
  });

  it("should handle base special field with gte and lte operators", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { base: { gte: "literal://a", lte: "literal://z" } } } as any);
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("source >= 'literal://a'");
    expect(normalized).toContain("source <= 'literal://z'");
  });

  it("should handle timestamp special field with gt operator", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { timestamp: { gt: 1234567890 } } });
    
    expect(query).toContain("WHERE timestamp > 1234567890");
  });

  it("should handle multiple special fields", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        author: "did:key:alice",
        timestamp: { gt: 1000 }
      }
    });
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("author = 'did:key:alice'");
    expect(normalized).toContain("timestamp > 1000");
    expect(normalized).toContain("AND");
  });

  it("should handle mixed special and regular properties", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        name: "Pasta",
        author: "did:key:alice",
        rating: { gt: 4 }
      }
    });
    
    const normalized = normalizeQuery(query);
    // Regular properties use subqueries
    expect(normalized).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://name' AND target = 'Pasta')");
    expect(normalized).toContain("source IN (SELECT source FROM link WHERE predicate = 'recipe://rating' AND target > 4)");
    // Special fields use direct access
    expect(normalized).toContain("author = 'did:key:alice'");
  });

  it("should handle boolean values", () => {
    @ModelOptions({ name: "Task" })
    class Task extends Ad4mModel {
      @Property({ through: "task://completed" })
      completed: boolean = false;
    }

    const query = Task.queryToSurrealQL(mockPerspective, { where: { completed: true } });
    
    expect(query).toContain("target = true");
  });

  it("should handle array of numbers", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { rating: [4, 5] } });
    
    expect(query).toContain("target IN [4, 5]");
  });

  it("should skip unknown properties in where clause", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { where: { unknownProp: "value" } as any });
    
    // Should not throw error, just skip the unknown property
    expect(query).toContain("WITH candidate_bases AS");
    // Should not contain any condition for unknownProp
    expect(query).not.toContain("unknownProp");
  });

  it("should skip unknown properties in select clause", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { properties: ["name", "unknownProp"] as any });
    
    // Should include name
    expect(query).toContain("(SELECT target FROM link WHERE source = cb.base AND predicate = 'recipe://name' LIMIT 1) AS name");
    // Should not error on unknownProp, just skip it
    expect(query).not.toContain("unknownProp");
  });

  it("should handle order by regular property", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { order: { name: "ASC" } });
    
    expect(query).toContain("ORDER BY name ASC");
  });

  it("should generate query with only properties, no collections", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { properties: ["name", "rating"], collections: [] });
    
    expect(query).toContain("AS name");
    expect(query).toContain("AS rating");
    expect(query).not.toContain("AS ingredients");
  });

  it("should generate query with only collections, no properties", () => {
    const query = Recipe.queryToSurrealQL(mockPerspective, { properties: [], collections: ["ingredients"] });
    
    expect(query).toContain("AS ingredients");
    expect(query).not.toContain("AS name");
    expect(query).not.toContain("AS rating");
    // But should still include author and timestamp
    expect(query).toContain("AS author");
    expect(query).toContain("AS timestamp");
  });
});

