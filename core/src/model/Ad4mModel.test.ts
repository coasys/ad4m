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

describe("Ad4mModel.queryToSurrealQL()", () => {
  // Mock perspective proxy (minimal since queryToSurrealQL doesn't actually call it)
  const mockPerspective = {} as any;

  // Helper function to normalize whitespace in queries for easier comparison
  function normalizeQuery(query: string): string {
    return query.replace(/\s+/g, ' ').trim();
  }

  // Test Recipe model — explicit required: true to test required-property query filters
  @Model({ name: "Recipe" })
  class Recipe extends Ad4mModel {
    @Property({ through: "recipe://name", required: true })
    name: string = "";
    
    @Property({ through: "recipe://rating", required: true })
    rating: number = 0;
    
    @HasMany({ through: "recipe://ingredient" })
    ingredients: string[] = [];
  }

  it("should generate basic query with no filters", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {});

    expect(query).toContain("id AS source");
    expect(query).toContain("uri AS source_uri");
    expect(query).toContain("FROM node");
    expect(query).toContain("->link AS links");
    expect(query).toContain("WHERE");
    // Should have graph traversal filters for required properties
    expect(query).toContain("count(->link[WHERE");
  });

  it("should generate query with simple property filter", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: "Pasta" } });
    
    // Should have graph traversal filters for required properties and user filter
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name']) > 0");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://rating']) > 0");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0");
  });

  it("should generate query with multiple property filters", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: "Pasta", rating: 5 } });
    
    expect(query).toContain("WHERE");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://rating' AND fn::parse_literal(out.uri) = 5]) > 0");
    expect(query).toContain("AND");
  });

  it("should handle gt operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { gt: 4 } } });

    // Comparison operators for regular properties are now filtered in JavaScript post-query
    // The SQL just filters on the predicate existing
    expect(query).toContain("FROM node");
    
    // Should NOT contain comparison operators in SQL for regular properties
    expect(query).not.toContain("target > 4");
  });

  it("should handle lt operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { lt: 3 } } });

    // Comparison operators are filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target < 3");
  });

  it("should handle gte and lte operators (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { gte: 3, lte: 5 } } });

    // Comparison operators are filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target >= 3");
    expect(query).not.toContain("target <= 5");
  });

  it("should handle not operator with single value", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: { not: "Salad" } } });

    // Not operator uses graph traversal with count = 0
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Salad']) = 0");
  });

  it("should handle not operator with array (NOT IN)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: { not: ["Salad", "Soup"] } } });

    // Not operator with array uses graph traversal with count = 0
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) IN ['Salad', 'Soup']]) = 0");
  });

  it("should handle between operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: { between: [3, 5] } } });

    // Between operator for regular properties is now filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target >= 3");
    expect(query).not.toContain("target <= 5");
  });

  it("should handle contains operator on string property (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: { contains: "Past" } } });

    // Contains operator for regular properties is now filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target CONTAINS 'Past'");
  });

  it("should handle contains operator on regular property with substring (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: { contains: "Salad" } } });

    // Contains operator is filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target CONTAINS 'Salad'");
  });

  it.skip("should handle contains operator on special field (author)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { author: { contains: "alice" } } });
    
    expect(query).toContain("WHERE author CONTAINS 'alice'");
    // Should not use a subquery pattern
    expect(query).not.toContain("SELECT source FROM node");
  });

  it.skip("should handle contains operator on special field (id)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { contains: "test" } } });
    
    expect(query).toContain("WHERE source CONTAINS 'test'");
  });

  it("should handle array values (IN clause)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: ["Pasta", "Pizza"] } });
    
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) IN ['Pasta', 'Pizza']]) > 0");
  });

  it.skip("should handle special fields (author, timestamp) without subqueries", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { author: "did:key:alice" } });
    
    expect(query).toContain("WHERE author = 'did:key:alice'");
    // Ensure it's NOT using a subquery pattern for author in WHERE clause
    expect(normalizeQuery(query)).not.toMatch(/WHERE.*source IN.*SELECT source FROM node.*author/);
  });

  it("should not generate ORDER BY clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { order: { timestamp: "DESC" } });

    // ORDER BY is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("ORDER BY");
  });

  it("should not generate LIMIT clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { limit: 10 });

    // LIMIT is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("LIMIT");
  });

  it("should not generate START clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { offset: 20 });

    // START/offset is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("START");
  });

  it("should generate complete query with all options (ORDER BY, LIMIT, START handled in JS)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "Pasta", rating: { gt: 4 } },
      order: { timestamp: "DESC" },
      limit: 10,
      offset: 20
    });

    // WHERE clause uses graph traversal filters
    expect(query).toContain("WHERE");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0");
    // Comparison operators (gt) are filtered in JavaScript, not SQL
    expect(query).not.toContain("out.uri > 4");
    expect(query).not.toContain("target > 4");
    // ORDER BY, LIMIT, START are now handled in JavaScript post-query
    expect(query).not.toContain("ORDER BY");
    expect(query).not.toContain("LIMIT");
    expect(query).not.toContain("START");
  });

  it("should only select requested properties", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { properties: ["name"] });

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain("->link AS links");
    
  });

  it("should select all link data for relations", async () => {
    @Model({ name: "MultiRelationModel" })
    class MultiRelationModel extends Ad4mModel {
      @HasMany({ through: "test://coll1" })
      coll1: string[] = [];

      @HasMany({ through: "test://coll2" })
      coll2: string[] = [];
    }

    const query = await MultiRelationModel.queryToSurrealQL(mockPerspective, {});

    // With array::group(), all link data is selected
    expect(query).toContain("->link AS links");
    
  });

  it("should escape single quotes in string values", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { name: "O'Brien's Recipe" } });

    // Single quotes are escaped with backslash in SurrealDB
    expect(query).toContain("O\\'Brien\\'s Recipe");
  });

  it("should handle numeric values without quotes", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: 5 } });
    
    // Numeric values should not have quotes around them
    expect(query).toContain("fn::parse_literal(out.uri) = 5");
    expect(query).not.toContain("fn::parse_literal(out.uri) = '5'");
  });

  it("should handle complex nested query (comparisons, ORDER BY, LIMIT handled in JS)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        name: "Pasta",
        rating: { gte: 4, lte: 5 },
        author: "did:key:alice"
      },
      order: { rating: "DESC" },
      limit: 5
    });

    const normalized = normalizeQuery(query);

    // Verify graph traversal filters are present
    expect(normalized).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0");
    // Comparison operators (gte, lte) are filtered in JavaScript, not SQL
    expect(normalized).not.toContain("out.uri >= 4");
    expect(normalized).not.toContain("target >= 4");
    expect(normalized).not.toContain("out.uri <= 5");
    expect(normalized).not.toContain("target <= 5");
    // author and timestamp filtering is done in JavaScript post-query
    expect(normalized).not.toContain("author = 'did:key:alice'");

    // ORDER BY and LIMIT are handled in JavaScript post-query
    expect(normalized).not.toContain("ORDER BY");
    expect(normalized).not.toContain("LIMIT");

    // Verify query structure (FROM node, no GROUP BY)
    expect(normalized).toContain("FROM node");
  });

  it("should handle empty query object", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {});
    
    // Should generate valid query with WHERE clause for required properties (name and rating)
    expect(query).toContain("id AS source");
    expect(query).toContain("uri AS source_uri");
    expect(query).toContain("FROM node");
    
    // Should have WHERE clause filtering for required properties using graph traversal
    expect(query).toContain("WHERE");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://name']) > 0");
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://rating']) > 0");
    expect(query).not.toContain("ORDER BY");
    expect(query).not.toContain("START");
  });

  it("should handle id special field", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: "literal://test" } });
    
    expect(query).toContain("uri = 'literal://test'");
  });

  it("should handle id special field with array (IN clause)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: ["literal://test1", "literal://test2"] } });
    
    expect(query).toContain("uri IN ['literal://test1', 'literal://test2']");
  });

  it("should handle id special field with not operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { not: "literal://test" } } });
    
    expect(query).toContain("uri != 'literal://test'");
  });

  it("should handle id special field with not operator and array (NOT IN)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { not: ["literal://test1", "literal://test2"] } } });
    
    expect(query).toContain("uri NOT IN ['literal://test1', 'literal://test2']");
  });

  it("should handle id special field with between operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { between: ["literal://a", "literal://z"] } } } as any);
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("uri >= 'literal://a' AND uri <= 'literal://z'");
  });

  it("should handle id special field with gt operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { gt: "literal://m" } } } as any);
    
    expect(query).toContain("uri > 'literal://m'");
  });

  it("should handle id special field with gte and lte operators", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { id: { gte: "literal://a", lte: "literal://z" } } } as any);
    
    const normalized = normalizeQuery(query);
    expect(normalized).toContain("uri >= 'literal://a'");
    expect(normalized).toContain("uri <= 'literal://z'");
  });

  it("should handle timestamp special field with gt operator (filtered in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { timestamp: { gt: 1234567890 } } });

    // timestamp filtering is done in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("timestamp > 1234567890");
  });

  it("should handle multiple special fields (filtered in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        author: "did:key:alice",
        timestamp: { gt: 1000 }
      }
    });

    // author and timestamp filtering is done in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("author = 'did:key:alice'");
    expect(query).not.toContain("timestamp > 1000");
  });

  it("should handle mixed special and regular properties", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        name: "Pasta",
        author: "did:key:alice",
        rating: { gt: 4 }
      }
    });

    const normalized = normalizeQuery(query);
    // Regular properties use graph traversal filters
    expect(normalized).toContain("count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0");
    // Comparison operators (gt) are filtered in JavaScript, not SQL
    expect(normalized).not.toContain("out.uri > 4");
    expect(normalized).not.toContain("target > 4");
    // author filtering is done in JavaScript post-query
    expect(normalized).not.toContain("author = 'did:key:alice'");
  });

  it("should handle boolean values", async () => {
    @Model({ name: "Task" })
    class Task extends Ad4mModel {
      @Property({ through: "task://completed" })
      completed: boolean = false;
    }

    const query = await Task.queryToSurrealQL(mockPerspective, { where: { completed: true } });
    
    expect(query).toContain("fn::parse_literal(out.uri) = true");
  });

  it("should handle array of numbers", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { rating: [4, 5] } });
    
    expect(query).toContain("count(->link[WHERE predicate = 'recipe://rating' AND fn::parse_literal(out.uri) IN [4, 5]]) > 0");
  });

  it("should skip unknown properties in where clause", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { where: { unknownProp: "value" } as any });
    
    // Should not throw error, just skip the unknown property
    expect(query).toContain("source");
    
    // Should not contain any condition for unknownProp
    expect(query).not.toContain("unknownProp");
  });

  it.skip("should skip unknown properties in select clause", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { properties: ["name", "unknownProp"] as any });
    
    // Should include name using aggregation
    expect(query).toContain("array::first(target[WHERE predicate = 'recipe://name']) AS name");
    // Should not error on unknownProp, just skip it
    expect(query).not.toContain("unknownProp");
  });

  it("should handle order by regular property (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { order: { name: "ASC" } });

    // ORDER BY is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("ORDER BY");
  });

  it("should generate query with only properties specified", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { properties: ["name", "rating"] });

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain("->link AS links");
    
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
    expect(norm).toContain("PREFIX ad4m:");
    // Must have conformance filter for required properties
    expect(norm).toContain("EXISTS");
    expect(norm).toContain("recipe://name");
  });

  it("should generate query with simple property filter", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: "Pasta" } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("SELECT ?source ?predicate ?target ?author ?timestamp");
    // Should have filter for name = "Pasta" using parse_literal
    expect(norm).toContain("recipe://name");
    expect(norm).toContain("Pasta");
  });

  it("should generate query with NOT operator", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { not: "Salad" } } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("!=");
    expect(norm).toContain("Salad");
  });

  it("should generate query with NOT IN operator (array)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { not: ["Salad", "Soup"] } } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("Salad");
    expect(norm).toContain("Soup");
    expect(norm).toContain("IN");
  });

  it("should generate query with IN clause (array values)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: ["Pasta", "Pizza"] } });
    const norm = normalizeQuery(query);

    expect(norm).toContain("Pasta");
    expect(norm).toContain("Pizza");
    expect(norm).toContain("IN");
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

  it("should not inject SPARQL FILTER for gt operator (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { gt: 3 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).toContain("EXISTS");
    // gt is handled in JS, should NOT produce a > filter in SPARQL
    expect(norm).not.toMatch(/\?\w+\s*>\s*"3"/);
  });

  it("should not inject SPARQL FILTER for lt operator (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { lt: 5 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toMatch(/\?\w+\s*<\s*"5"/);
  });

  it("should not inject SPARQL FILTER for gte/lte combined (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { gte: 2, lte: 8 } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toMatch(/>=|<=/);
  });

  it("should not inject SPARQL FILTER for between operator (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { rating: { between: [1, 10] } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toContain("between");
  });

  it("should not inject SPARQL FILTER for contains operator (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { where: { name: { contains: "pasta" } } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toContain("CONTAINS");
    expect(norm).not.toContain("contains");
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

  it("should not include ORDER BY in SPARQL (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { order: { name: "ASC" } });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toContain("ORDER BY");
  });

  it("should not include LIMIT in SPARQL (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { limit: 10 });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toContain("LIMIT");
  });

  it("should not include OFFSET in SPARQL (JS post-processed)", async () => {
    const query = await (Recipe as any).queryToSPARQL(mockPerspective, { offset: 5 });
    const norm = normalizeQuery(query);
    expect(norm).toContain("SELECT ?source");
    expect(norm).not.toContain("OFFSET");
  });
});
describe("Ad4mModel.instancesFromSurrealResult() and SurrealDB integration", () => {
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

  // Mock perspective with both querySurrealDB and infer methods
  const mockPerspective = {
    querySurrealDB: jest.fn(),
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should convert empty SurrealDB results correctly", async () => {
    const result = await Recipe.instancesFromSurrealResult(mockPerspective, {}, []);
    
    expect(result.results).toEqual([]);
    expect(result.totalCount).toBe(0);
  });

  it("should convert SurrealDB results to model instances", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
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
        source_uri: "literal://recipe2",
        links: [
          { predicate: "recipe://name", target: "Pizza", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://rating", target: "4", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "dough", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "cheese", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromSurrealResult(mockPerspective, {}, surrealResults);

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
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      { properties: ["name"] },
      surrealResults
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
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "tomato", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      { properties: ["name"] },
      surrealResults
    );

    expect(result.results).toHaveLength(1);
    const recipe = result.results[0];
    expect(recipe.name).toBe("Pasta");
    // rating and ingredients should be removed since only "name" was requested
    expect(recipe.rating).toBeUndefined();
  });

  it("should handle results missing base field", async () => {
    const surrealResults = [
      {
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
        // Missing source field
      } as any
    ];

    const result = await Recipe.instancesFromSurrealResult(mockPerspective, {}, surrealResults);

    // Should filter out the invalid result (or handle gracefully)
    expect(result.results).toHaveLength(0);
    expect(result.totalCount).toBe(0);
  });

  it("should use SurrealDB when engine is 'surreal' in findAll()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const results = await Recipe.findAll(mockPerspective, {}, 'surreal');

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use Prolog when useSurrealDB is false in findAll()", async () => {
    const prologResults = [{
      AllInstances: [
        ["literal://recipe1", [["name", "Pasta"]], [["ingredients", ["pasta"]]], "2023-01-01T00:00:00Z", "did:key:alice"]
      ],
      TotalCount: 1
    }];

    mockPerspective.infer.mockResolvedValue(prologResults);

    const results = await Recipe.findAll(mockPerspective, {}, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySurrealDB).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
  });

  it("should use SurrealDB when engine is 'surreal' in findAllAndCount()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const { results, totalCount } = await Recipe.findAllAndCount(mockPerspective, {}, 'surreal');

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(totalCount).toBe(1);
  });

  it("should use SurrealDB when engine is 'surreal' in paginate()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const page = await Recipe.paginate(mockPerspective, 10, 1, {});

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
  });

  it("should use SurrealDB when engine is 'surreal' in count()", async () => {
    // Since count() uses result.length and GROUP BY returns one row per source,
    // mock 5 recipe sources
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]
    }));

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const count = await Recipe.count(mockPerspective, {});

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(5);
  });

  it("should use Prolog when useSurrealDB is false in count()", async () => {
    const prologResults = [{ TotalCount: 10 }];
    mockPerspective.infer.mockResolvedValue(prologResults);

    const count = await Recipe.count(mockPerspective, {}, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySurrealDB).not.toHaveBeenCalled();
    expect(count).toBe(10);
  });

  it("should use SurrealDB when engine is 'surreal' in ModelQueryBuilder.get()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .get();

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use Prolog when useSurrealDB(false) in ModelQueryBuilder.get()", async () => {
    const prologResults = [{
      AllInstances: [
        ["literal://recipe1", [["name", "Pasta"]], [["ingredients", ["pasta"]]], "2023-01-01T00:00:00Z", "did:key:alice"]
      ],
      TotalCount: 1
    }];

    mockPerspective.infer.mockResolvedValue(prologResults);

    const results = await Recipe.query(mockPerspective)
      .where({ name: "Pasta" })
      .useSurrealDB(false)
      .get();
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySurrealDB).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
  });

  it("should use SurrealDB when engine is 'surreal' in ModelQueryBuilder.count()", async () => {
    // count() counts the number of rows returned by the query (one row per source)
    const surrealResults = Array.from({ length: 3 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]
    }));
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 4 } })
      .count();

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });

  it("should use SurrealDB when engine is 'surreal' in ModelQueryBuilder.paginate()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          { predicate: "recipe://name", target: "Pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://ingredient", target: "pasta", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      }
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const page = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .paginate(10, 1);

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
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
    querySurrealDB: jest.fn(),
    infer: jest.fn(),
    uuid: 'test-perspective-uuid',
    stringOrTemplateObjectToSubjectClassName: jest.fn().mockResolvedValue('Recipe')
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should apply JS-level filtering for gt operator on properties in SurrealDB count()", async () => {
    // Mock SurrealDB results: 5 recipes with ratings 1, 2, 3, 4, 5
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes with rating > 3 (should match 2 recipes: rating 4 and 5)
    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } });
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { rating: { gt: 3 } } });
    
    expect(count).toBe(2);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for between operator on properties in SurrealDB count()", async () => {
    // Mock SurrealDB results: 5 recipes with ratings 1, 2, 3, 4, 5
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes with rating between 2 and 4 (should match 3 recipes: rating 2, 3, 4)
    const count = await Recipe.count(mockPerspective, { where: { rating: { between: [2, 4] } } });
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { rating: { between: [2, 4] } } });
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for timestamp gt operator in SurrealDB count()", async () => {
    // Mock SurrealDB results: 5 recipes with different timestamps
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes with timestamp > 2023-01-03 (should match 2 recipes: 2023-01-04 and 2023-01-05)
    const targetTimestamp = new Date("2023-01-03T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { where: { timestamp: { gt: targetTimestamp } } });
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { timestamp: { gt: targetTimestamp } } });
    
    expect(count).toBe(2);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for timestamp between operator in SurrealDB count()", async () => {
    // Mock SurrealDB results: 5 recipes with different timestamps
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes with timestamp between 2023-01-02 and 2023-01-04
    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    const count = await Recipe.count(mockPerspective, { 
      where: { timestamp: { between: [startTimestamp, endTimestamp] } } 
    });
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { 
      where: { timestamp: { between: [startTimestamp, endTimestamp] } } 
    });
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering for author filtering in SurrealDB count()", async () => {
    // Mock SurrealDB results: 3 recipes by Alice and 2 by Bob
    const surrealResults = [
      ...Array.from({ length: 3 }, (_, i) => ({
        source: `node:abc${i+1}`,
        source_uri: `literal://recipe${i+1}`,
        links: [
          { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
        ]
      })),
      ...Array.from({ length: 2 }, (_, i) => ({
        source: `node:def${i+4}`,
        source_uri: `literal://recipe${i+4}`,
        links: [
          { predicate: "recipe://name", target: `Recipe ${i+4}`, author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" },
          { predicate: "recipe://rating", target: "5", author: "did:key:bob", timestamp: "2023-01-02T00:00:00Z" }
        ]
      }))
    ];
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes by Alice (should match 3 recipes)
    const count = await Recipe.count(mockPerspective, { where: { author: "did:key:alice" } });
    
    // Verify count matches the number of instances that would be returned by findAll
    const findAllResults = await Recipe.findAll(mockPerspective, { where: { author: "did:key:alice" } });
    
    expect(count).toBe(3);
    expect(count).toBe(findAllResults.length);
  });

  it("should apply JS-level filtering in ModelQueryBuilder.count() with gt operator", async () => {
    // Mock SurrealDB results: 5 recipes with ratings 1, 2, 3, 4, 5
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" },
        { predicate: "recipe://rating", target: `${i+1}`, author: "did:key:alice", timestamp: "2023-01-01T00:00:00Z" }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Count recipes with rating > 3 using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .count();
    
    // Verify count matches the number of instances that would be returned by get()
    const getResults = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 3 } })
      .get();
    
    expect(count).toBe(2);
    expect(count).toBe(getResults.length);
  });

  it("should apply JS-level filtering in ModelQueryBuilder.count() with timestamp between", async () => {
    // Mock SurrealDB results: 5 recipes with different timestamps
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i+1}`,
      source_uri: `literal://recipe${i+1}`,
      links: [
        { predicate: "recipe://name", target: `Recipe ${i+1}`, author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` },
        { predicate: "recipe://rating", target: "5", author: "did:key:alice", timestamp: `2023-01-0${i+1}T00:00:00Z` }
      ]
    }));
    
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const startTimestamp = new Date("2023-01-02T00:00:00Z").getTime();
    const endTimestamp = new Date("2023-01-04T00:00:00Z").getTime();
    
    // Count using ModelQueryBuilder
    const count = await Recipe.query(mockPerspective)
      .where({ timestamp: { between: [startTimestamp, endTimestamp] } })
      .count();
    
    // Verify count matches the number of instances that would be returned by get()
    const getResults = await Recipe.query(mockPerspective)
      .where({ timestamp: { between: [startTimestamp, endTimestamp] } })
      .get();
    
    expect(count).toBe(3);
    expect(count).toBe(getResults.length);
  });

  it("should handle count() with Prolog for gt operator (legacy)", async () => {
    const prologResults = [{ TotalCount: 2 }];
    mockPerspective.infer.mockResolvedValue(prologResults);

    const count = await Recipe.count(mockPerspective, { where: { rating: { gt: 3 } } }, false);
    
    expect(mockPerspective.infer).toHaveBeenCalledTimes(1);
    expect(mockPerspective.querySurrealDB).not.toHaveBeenCalled();
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
    expect(mockPerspective.querySurrealDB).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });
});

