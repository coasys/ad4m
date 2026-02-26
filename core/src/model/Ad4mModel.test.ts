import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, HasMany, Flag } from "./decorators";
import { Literal } from "../Literal";

// ── Shared test fixtures ──────────────────────────────────────────────────────
// Defined once at module level — used by queryToSurrealQL,
// instancesFromSurrealResult, and hydration describe blocks alike.

@Model({ name: "Recipe" })
class Recipe extends Ad4mModel {
  @Property({ through: "recipe://name", required: true })
  name: string = "";

  @Property({ through: "recipe://rating", required: true })
  rating: number = 0;

  @HasMany({ through: "recipe://ingredient" })
  ingredients: string[] = [];
}

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
      @Property({
        through: "test://name",
        resolveLanguage: "literal",
        required: true,
      })
      name: string = "";

      @Property({ through: "test://optional" })
      optional: string = "";

      @Property({ through: "test://readonly", readOnly: true })
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
    expect(metadata.properties.name.readOnly).toBeFalsy();
    expect(metadata.properties.name.resolveLanguage).toBe("literal");

    // Verify "optional" property
    expect(metadata.properties.optional.predicate).toBe("test://optional");
    expect(metadata.properties.optional.readOnly).toBeFalsy();

    // Verify "readonly" property
    expect(metadata.properties.readonly.predicate).toBe("test://readonly");
    expect(metadata.properties.readonly.readOnly).toBe(true);

    // Verify "type" property (flag)
    expect(metadata.properties.type.predicate).toBe("test://type");
    expect(metadata.properties.type.flag).toBe(true);
    expect(metadata.properties.type.initial).toBe("test://flag");
  });

  it("should extract collection metadata", () => {
    @Model({ name: "CollectionModel" })
    class CollectionModel extends Ad4mModel {
      @HasMany({ through: "test://items" })
      items: string[] = [];

      @HasMany({ through: "test://local", local: true })
      local: string[] = [];
    }

    const metadata = CollectionModel.getModelMetadata();

    // Should have 2 collections
    expect(Object.keys(metadata.relations)).toHaveLength(2);

    // Verify "items" collection
    expect(metadata.relations.items.predicate).toBe("test://items");

    // Verify "local" collection
    expect(metadata.relations.local.predicate).toBe("test://local");
    expect(metadata.relations.local.local).toBe(true);
  });

  it("should extract transform function from property metadata", () => {
    @Model({ name: "TransformModel" })
    class TransformModel extends Ad4mModel {
      @Property({
        through: "test://data",
        transform: (value: string) => value.toUpperCase(),
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

  it("should extract custom SurrealQL getter from property metadata", () => {
    @Model({ name: "CustomModel" })
    class CustomModel extends Ad4mModel {
      @Property({
        through: "test://computed",
        getter: "(<-link[WHERE predicate = 'test://value'].in.uri)[0]",
      })
      computed: string = "";
    }

    const metadata = CustomModel.getModelMetadata();

    // Assert getter contains the custom SurrealQL code
    expect(metadata.properties.computed.getter).toContain("test://value");
  });

  it("should register relatedModel factory for typed @HasMany", () => {
    @Model({ name: "Comment" })
    class Comment extends Ad4mModel {}

    @Model({ name: "Post" })
    class Post extends Ad4mModel {
      @HasMany(() => Comment, { through: "post://comment" })
      comments: Comment[] = [];
    }

    const metadata = Post.getModelMetadata();

    // relatedModel factory should be registered and return Comment
    expect(metadata.relations.comments.relatedModel).toBeDefined();
    expect(metadata.relations.comments.relatedModel!()).toBe(Comment);
  });

  it("should throw error for class without @ModelOptions decorator", () => {
    class NoDecoratorModel extends Ad4mModel {}

    // Assert that calling getModelMetadata throws an error
    expect(() => NoDecoratorModel.getModelMetadata()).toThrow(
      "Model class must be decorated with @Model",
    );
  });

  it("should handle complex model with mixed property and collection types", () => {
    @Model({ name: "Recipe" })
    class Recipe extends Ad4mModel {
      @Property({
        through: "recipe://name",
        required: true,
      })
      name: string = "";

      @Property({ through: "recipe://description" })
      description: string = "";

      @Property({
        through: "recipe://rating",
        getter: "avg_rating_surreal(Base, Value)",
        readOnly: true,
      })
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

    // Assert collections has 2 entries
    expect(Object.keys(metadata.relations)).toHaveLength(2);
    expect(metadata.relations.ingredients).toBeDefined();
    expect(metadata.relations.steps).toBeDefined();

    // Verify all metadata fields are correctly extracted
    expect(metadata.properties.name.predicate).toBe("recipe://name");
    expect(metadata.properties.name.resolveLanguage).toBeUndefined(); // "literal" is now the implicit default — no need to store it
    expect(metadata.properties.description.predicate).toBe(
      "recipe://description",
    );
    expect(metadata.properties.rating.predicate).toBe("recipe://rating");
    expect(metadata.properties.rating.getter).toBe(
      "avg_rating_surreal(Base, Value)",
    );
    expect(metadata.relations.ingredients.predicate).toBe(
      "recipe://ingredient",
    );
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
        description: { type: "string" },
      },
      required: ["name", "price"],
    };

    const ProductClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Product",
      namespace: "product://",
    });

    const metadata = ProductClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Product");

    // Verify properties are extracted
    expect(Object.keys(metadata.properties).length).toBeGreaterThan(0);
    expect(metadata.properties.name).toBeDefined();
    expect(metadata.properties.name.predicate).toBe("product://name");
    expect(metadata.properties.name.required).toBe(true);
    expect(metadata.properties.name.readOnly).toBeFalsy();
    expect(metadata.properties.name.resolveLanguage).toBeUndefined(); // implicit literal default

    expect(metadata.properties.price).toBeDefined();
    expect(metadata.properties.price.predicate).toBe("product://price");
    expect(metadata.properties.price.required).toBe(true);
    expect(metadata.properties.price.resolveLanguage).toBeUndefined(); // implicit literal default

    expect(metadata.properties.description).toBeDefined();
    expect(metadata.properties.description.predicate).toBe(
      "product://description",
    );
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
          items: { type: "string" },
        },
        comments: {
          type: "array",
          items: { type: "string" },
        },
      },
      required: ["title"],
    };

    const PostClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Post",
      namespace: "post://",
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
        namespace: "contact://",
      },
      type: "object",
      properties: {
        name: {
          type: "string",
          "x-ad4m": {
            through: "foaf://name",
          },
        },
        email: {
          type: "string",
          "x-ad4m": {
            through: "foaf://mbox",
            local: true,
          },
        },
      },
      required: ["name"],
    };

    const ContactClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Contact",
    });

    const metadata = ContactClass.getModelMetadata();

    // Verify x-ad4m metadata is respected
    expect(metadata.properties.name.predicate).toBe("foaf://name");
    expect(metadata.properties.name.resolveLanguage).toBeUndefined(); // implicit literal default
    expect(metadata.properties.name.readOnly).toBeFalsy();
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
        fullName: { type: "string" },
      },
      required: ["username"],
    };

    const UserClass = Ad4mModel.fromJSONSchema(schema, {
      name: "User",
      namespace: "user://",
      propertyMapping: {
        username: "custom://identifier",
        fullName: "custom://name",
      },
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
          items: { type: "string" },
        },
        tags: {
          type: "array",
          items: { type: "string" },
          "x-ad4m": {
            local: true,
          },
        },
      },
      required: ["title", "published"],
    };

    const ArticleClass = Ad4mModel.fromJSONSchema(schema, {
      name: "Article",
      namespace: "article://",
    });

    const metadata = ArticleClass.getModelMetadata();

    // Verify className
    expect(metadata.className).toBe("Article");

    // Verify properties
    expect(metadata.properties.title).toBeDefined();
    expect(metadata.properties.title.predicate).toBe("article://title");
    expect(metadata.properties.title.required).toBe(true);
    expect(metadata.properties.title.resolveLanguage).toBeUndefined(); // implicit literal default

    expect(metadata.properties.views).toBeDefined();
    expect(metadata.properties.views.predicate).toBe("article://views");
    expect(metadata.properties.views.resolveLanguage).toBeUndefined(); // implicit literal default

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

  it("should handle models with only an auto-generated type flag", () => {
    const warnSpy = jest.spyOn(console, "warn").mockImplementation(() => {});

    const schema = {
      title: "EmptyModel",
      type: "object",
      properties: {},
    };

    const EmptyModelClass = Ad4mModel.fromJSONSchema(schema, {
      name: "EmptyModel",
      namespace: "empty://",
    });

    const metadata = EmptyModelClass.getModelMetadata();

    // Should have className
    expect(metadata.className).toBe("EmptyModel");

    // Should have the auto-generated __ad4m_type property
    expect(metadata.properties.__ad4m_type).toBeDefined();
    expect(metadata.properties.__ad4m_type.predicate).toBe("ad4m://type");
    expect(metadata.properties.__ad4m_type.initial).toBe("empty://instance");
    expect(metadata.properties.__ad4m_type.flag).toBe(true);

    // The warning is the expected fallback signal for empty schemas
    expect(warnSpy).toHaveBeenCalledWith(
      expect.stringContaining("No properties with initial values found"),
    );
    warnSpy.mockRestore();
  });
});

describe("Ad4mModel.queryToSurrealQL()", () => {
  // Mock perspective proxy (minimal since queryToSurrealQL doesn't actually call it)
  const mockPerspective = {} as any;

  // Helper function to normalize whitespace in queries for easier comparison
  function normalizeQuery(query: string): string {
    return query.replace(/\s+/g, " ").trim();
  }

  it("should generate basic query with no filters", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {});

    expect(query).toContain("id AS source");
    expect(query).toContain("uri AS source_uri");
    expect(query).toContain("FROM node");
    expect(query).toContain(
      "(SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links",
    );
    expect(query).toContain("WHERE");
    // Should have graph traversal filters for required properties
    expect(query).toContain("count(->link[WHERE");
  });

  it("should generate query with simple property filter", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "Pasta" },
    });

    // Should have graph traversal filters for required properties and user filter
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name']) > 0",
    );
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://rating']) > 0",
    );
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0",
    );
  });

  it("should generate query with multiple property filters", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "Pasta", rating: 5 },
    });

    expect(query).toContain("WHERE");
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0",
    );
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://rating' AND fn::parse_literal(out.uri) = 5]) > 0",
    );
    expect(query).toContain("AND");
  });

  it("should handle gt operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: { gt: 4 } },
    });

    // Comparison operators for regular properties are now filtered in JavaScript post-query
    // The SQL just filters on the predicate existing
    expect(query).toContain("FROM node");

    // Should NOT contain comparison operators in SQL for regular properties
    expect(query).not.toContain("target > 4");
  });

  it("should handle lt operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: { lt: 3 } },
    });

    // Comparison operators are filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target < 3");
  });

  it("should handle gte and lte operators (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: { gte: 3, lte: 5 } },
    });

    // Comparison operators are filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target >= 3");
    expect(query).not.toContain("target <= 5");
  });

  it("should handle not operator with single value", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: { not: "Salad" } },
    });

    // Not operator uses graph traversal with count = 0
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Salad']) = 0",
    );
  });

  it("should handle not operator with array (NOT IN)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: { not: ["Salad", "Soup"] } },
    });

    // Not operator with array uses graph traversal with count = 0
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) IN ['Salad', 'Soup']]) = 0",
    );
  });

  it("should handle between operator (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: { between: [3, 5] } },
    });

    // Between operator for regular properties is now filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target >= 3");
    expect(query).not.toContain("target <= 5");
  });

  it("should handle contains operator on string property (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: { contains: "Past" } },
    });

    // Contains operator for regular properties is now filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target CONTAINS 'Past'");
  });

  it("should handle contains operator on regular property with substring (filtered in JavaScript, not SQL)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: { contains: "Salad" } },
    });

    // Contains operator is filtered in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("target CONTAINS 'Salad'");
  });

  it("should handle array values (IN clause)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: ["Pasta", "Pizza"] },
    });

    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) IN ['Pasta', 'Pizza']]) > 0",
    );
  });

  it("should not generate ORDER BY clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      order: { timestamp: "DESC" },
    });

    // ORDER BY is now handled in JavaScript post-query
    // Note: the links correlated subquery uses ORDER BY timestamp ASC internally for link ordering
    expect(query).toContain("FROM node");
    expect(query).not.toContain("ORDER BY timestamp DESC");
  });

  it("should not generate LIMIT clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, { limit: 10 });

    // LIMIT is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("LIMIT");
  });

  it("should not generate START clause in SQL (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      offset: 20,
    });

    // START/offset is now handled in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("START");
  });

  it("should generate complete query with all options (ORDER BY, LIMIT, START handled in JS)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "Pasta", rating: { gt: 4 } },
      order: { timestamp: "DESC" },
      limit: 10,
      offset: 20,
    });

    // WHERE clause uses graph traversal filters
    expect(query).toContain("WHERE");
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0",
    );
    // Comparison operators (gt) are filtered in JavaScript, not SQL
    expect(query).not.toContain("out.uri > 4");
    expect(query).not.toContain("target > 4");
    // ORDER BY, LIMIT, START are now handled in JavaScript post-query
    // Note: the links subquery uses ORDER BY timestamp ASC internally
    expect(query).not.toContain("ORDER BY timestamp DESC");
    expect(query).not.toContain("LIMIT");
    expect(query).not.toContain("START");
  });

  it("should only select requested fields", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      properties: ["name"],
    });

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain(
      "(SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links",
    );
  });

  it("should only select requested relations", async () => {
    @Model({ name: "MultiCollectionModel" })
    class MultiCollectionModel extends Ad4mModel {
      @HasMany({ through: "test://coll1" })
      coll1: string[] = [];

      @HasMany({ through: "test://coll2" })
      coll2: string[] = [];
    }

    const query = await MultiCollectionModel.queryToSurrealQL(
      mockPerspective,
      {},
    );

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain(
      "(SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links",
    );
  });

  it("should escape single quotes in string values", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { name: "O'Brien's Recipe" },
    });

    // Single quotes are escaped with backslash in SurrealDB
    expect(query).toContain("O\\'Brien\\'s Recipe");
  });

  it("should handle numeric values without quotes", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: 5 },
    });

    // Numeric values should not have quotes around them
    expect(query).toContain("fn::parse_literal(out.uri) = 5");
    expect(query).not.toContain("out.uri = '5'");
  });

  it("should handle complex nested query (comparisons, ORDER BY, LIMIT handled in JS)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        name: "Pasta",
        rating: { gte: 4, lte: 5 },
        author: "did:key:alice",
      },
      order: { rating: "DESC" },
      limit: 5,
    });

    const normalized = normalizeQuery(query);

    // Verify graph traversal filters are present
    expect(normalized).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0",
    );
    // Comparison operators (gte, lte) are filtered in JavaScript, not SQL
    expect(normalized).not.toContain("out.uri >= 4");
    expect(normalized).not.toContain("target >= 4");
    expect(normalized).not.toContain("out.uri <= 5");
    expect(normalized).not.toContain("target <= 5");
    // author and timestamp filtering is done in JavaScript post-query
    expect(normalized).not.toContain("author = 'did:key:alice'");

    // ORDER BY and LIMIT are handled in JavaScript post-query
    // Note: the links correlated subquery uses ORDER BY timestamp ASC internally
    expect(normalized).not.toContain("ORDER BY rating");
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
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://name']) > 0",
    );
    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://rating']) > 0",
    );
    // No user-requested ORDER BY or START should appear at the top level
    expect(query).not.toContain("START");
  });

  it("should handle base special field", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: "literal://test" },
    });

    expect(query).toContain("uri = 'literal://test'");
  });

  it("should handle base special field with array (IN clause)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: ["literal://test1", "literal://test2"] },
    });

    expect(query).toContain("uri IN ['literal://test1', 'literal://test2']");
  });

  it("should handle base special field with not operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: { not: "literal://test" } },
    });

    expect(query).toContain("uri != 'literal://test'");
  });

  it("should handle base special field with not operator and array (NOT IN)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: { not: ["literal://test1", "literal://test2"] } },
    });

    expect(query).toContain(
      "uri NOT IN ['literal://test1', 'literal://test2']",
    );
  });

  it("should handle base special field with between operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: { between: ["literal://a", "literal://z"] } },
    } as any);

    const normalized = normalizeQuery(query);
    expect(normalized).toContain(
      "uri >= 'literal://a' AND uri <= 'literal://z'",
    );
  });

  it("should handle base special field with gt operator", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: { gt: "literal://m" } },
    } as any);

    expect(query).toContain("uri > 'literal://m'");
  });

  it("should handle base special field with gte and lte operators", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { base: { gte: "literal://a", lte: "literal://z" } },
    } as any);

    const normalized = normalizeQuery(query);
    expect(normalized).toContain("uri >= 'literal://a'");
    expect(normalized).toContain("uri <= 'literal://z'");
  });

  it("should handle timestamp special field with gt operator (filtered in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { timestamp: { gt: 1234567890 } },
    });

    // timestamp filtering is done in JavaScript post-query
    expect(query).toContain("FROM node");
    expect(query).not.toContain("timestamp > 1234567890");
  });

  it("should handle multiple special fields (filtered in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: {
        author: "did:key:alice",
        timestamp: { gt: 1000 },
      },
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
        rating: { gt: 4 },
      },
    });

    const normalized = normalizeQuery(query);
    // Regular properties use graph traversal filters
    expect(normalized).toContain(
      "count(->link[WHERE predicate = 'recipe://name' AND fn::parse_literal(out.uri) = 'Pasta']) > 0",
    );
    // Comparison operators (gt) are filtered in JavaScript, not SQL
    expect(normalized).not.toContain("out.uri > 4");
    expect(normalized).not.toContain("target > 4");
    // author filtering is done in JavaScript post-query
    expect(normalized).not.toContain("author = 'did:key:alice'");
  });

  it("should handle boolean values", async () => {
    @Model({ name: "Task" })
    class Task extends Ad4mModel {
      @Property({
        through: "task://completed",
        required: true,
      })
      completed: boolean = false;
    }
    const query = await Task.queryToSurrealQL(mockPerspective, {
      where: { completed: true },
    });

    expect(query).toContain("fn::parse_literal(out.uri) = true");
  });

  it("should handle array of numbers", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { rating: [4, 5] },
    });

    expect(query).toContain(
      "count(->link[WHERE predicate = 'recipe://rating' AND fn::parse_literal(out.uri) IN [4, 5]]) > 0",
    );
  });

  it("should skip unknown properties in where clause", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      where: { unknownProp: "value" } as any,
    });

    // Should not throw error, just skip the unknown property
    expect(query).toContain("source");

    // Should not contain any condition for unknownProp
    expect(query).not.toContain("unknownProp");
  });

  it("should handle order by regular property (handled in JavaScript)", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      order: { name: "ASC" },
    });

    // ORDER BY is now handled in JavaScript post-query
    // Note: the links correlated subquery uses ORDER BY timestamp ASC internally
    expect(query).toContain("FROM node");
    expect(query).not.toContain("ORDER BY name");
  });

  it("should generate query with only fields, no relations", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      properties: ["name", "rating"],
    });

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain(
      "(SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links",
    );
  });

  it("should generate query with only relations, no fields", async () => {
    const query = await Recipe.queryToSurrealQL(mockPerspective, {
      properties: [],
    });

    // With array::group(), all link data is selected, filtering happens in instancesFromSurrealResult
    expect(query).toContain(
      "(SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links",
    );
  });
});

describe("Ad4mModel.instancesFromSurrealResult() — hydration and query dispatch", () => {
  // Mock perspective with both querySurrealDB and infer methods
  const mockPerspective = {
    querySurrealDB: jest.fn(),
    infer: jest.fn(),
    uuid: "test-perspective-uuid",
    stringOrTemplateObjectToSubjectClassName: jest
      .fn()
      .mockResolvedValue("Recipe"),
  } as any;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should convert empty SurrealDB results correctly", async () => {
    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      [],
    );

    expect(result.results).toEqual([]);
    expect(result.totalCount).toBe(0);
  });

  it("should convert SurrealDB results to model instances", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: Literal.from("Pasta").toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: Literal.from(5).toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "tomato",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "cheese",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
      {
        source: "node:def456",
        source_uri: "literal://recipe2",
        links: [
          {
            predicate: "recipe://name",
            target: Literal.from("Pizza").toUrl(),
            author: "did:key:bob",
            timestamp: "2023-01-02T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: Literal.from(4).toUrl(),
            author: "did:key:bob",
            timestamp: "2023-01-02T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "dough",
            author: "did:key:bob",
            timestamp: "2023-01-02T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "cheese",
            author: "did:key:bob",
            timestamp: "2023-01-02T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "tomato",
            author: "did:key:bob",
            timestamp: "2023-01-02T00:00:00Z",
          },
        ],
      },
    ];

    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      surrealResults,
    );

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

  it("should set instance id from source_uri", async () => {
    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      [
        {
          source: "node:abc123",
          source_uri: "literal://recipe1",
          links: [
            {
              predicate: "recipe://name",
              target: Literal.from("Pasta").toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-01T00:00:00Z",
            },
            {
              predicate: "recipe://rating",
              target: Literal.from(5).toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-01T00:00:00Z",
            },
          ],
        },
      ],
    );

    expect(result.results).toHaveLength(1);
    // id is derived from source_uri (the node's URI in the graph)
    expect(result.results[0].id).toBe("literal://recipe1");
  });

  it("should use latest-wins semantics when multiple links share a predicate", async () => {
    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      [
        {
          source: "node:abc123",
          source_uri: "literal://recipe1",
          links: [
            {
              predicate: "recipe://name",
              target: Literal.from("Old Name").toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-01T00:00:00Z",
            },
            {
              // later timestamp — this value should win
              predicate: "recipe://name",
              target: Literal.from("New Name").toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-02T00:00:00Z",
            },
            {
              predicate: "recipe://rating",
              target: Literal.from(5).toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-01T00:00:00Z",
            },
          ],
        },
      ],
    );

    expect(result.results).toHaveLength(1);
    expect(result.results[0].name).toBe("New Name");
  });

  it("should apply transform function during hydration", async () => {
    @Model({ name: "TransformRecipe" })
    class TransformRecipe extends Ad4mModel {
      @Property({
        through: "recipe://name",
        transform: (v: string) => v.toUpperCase(),
      })
      name: string = "";
    }

    const result = await TransformRecipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      [
        {
          source: "node:abc123",
          source_uri: "literal://tr1",
          links: [
            {
              predicate: "recipe://name",
              target: Literal.from("pasta").toUrl(),
              author: "did:key:alice",
              timestamp: "2023-01-01T00:00:00Z",
            },
          ],
        },
      ],
    );

    expect(result.results).toHaveLength(1);
    expect(result.results[0].name).toBe("PASTA");
  });

  it("should apply JS-level operator filters — smoke test", async () => {
    // Ratings 1–5 encoded as Literals so the JS filter receives actual numbers
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i + 1}`,
      source_uri: `literal://recipe${i + 1}`,
      links: [
        {
          predicate: "recipe://name",
          target: Literal.from(`Recipe ${i + 1}`).toUrl(),
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
        {
          predicate: "recipe://rating",
          target: Literal.from(i + 1).toUrl(),
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
      ],
    }));
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    // Only ratings 4 and 5 pass the JS-level gt filter
    const results = await Recipe.findAll(mockPerspective, {
      where: { rating: { gt: 3 } },
    });
    expect(results).toHaveLength(2);

    // count() must be consistent with findAll()
    const count = await Recipe.count(mockPerspective, {
      where: { rating: { gt: 3 } },
    });
    expect(count).toBe(2);
  });

  it("should filter fields when query specifies fields", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "tomato",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      { properties: ["name"] },
      surrealResults,
    );

    expect(result.results).toHaveLength(1);
    const recipe = result.results[0];
    expect(recipe.name).toBe("Pasta");
    // rating and ingredients should be removed since only "name" was requested
    expect(recipe.rating).toBeUndefined();
    expect(recipe.ingredients).toBeUndefined();
    // author and timestamp should still be present
    expect(recipe.author).toBe("did:key:alice");
    // Timestamp is converted to Unix epoch (milliseconds)
    expect(recipe.timestamp).toBe(new Date("2023-01-01T00:00:00Z").getTime());
  });

  it("should handle results missing base field", async () => {
    const surrealResults = [
      {
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
        // Missing source field
      } as any,
    ];

    const result = await Recipe.instancesFromSurrealResult(
      mockPerspective,
      {},
      surrealResults,
    );

    // Should filter out the invalid result (or handle gracefully)
    expect(result.results).toHaveLength(0);
    expect(result.totalCount).toBe(0);
  });

  it("should use SurrealDB by default in findAll()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const results = await Recipe.findAll(mockPerspective, {});

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(results[0].name).toBe("Pasta");
  });

  it("should use SurrealDB by default in findAllAndCount()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const { results, totalCount } = await Recipe.findAllAndCount(
      mockPerspective,
      {},
    );

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(results).toHaveLength(1);
    expect(totalCount).toBe(1);
  });

  it("should use SurrealDB by default in paginate()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const page = await Recipe.paginate(mockPerspective, 10, 1, {});

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(page.results).toHaveLength(1);
    expect(page.pageSize).toBe(10);
    expect(page.pageNumber).toBe(1);
  });

  it("should use SurrealDB by default in count()", async () => {
    // Since count() uses result.length and GROUP BY returns one row per source,
    // mock 5 recipe sources
    const surrealResults = Array.from({ length: 5 }, (_, i) => ({
      source: `node:abc${i + 1}`,
      source_uri: `literal://recipe${i + 1}`,
      links: [
        {
          predicate: "recipe://name",
          target: `Recipe ${i + 1}`,
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
        {
          predicate: "recipe://rating",
          target: "5",
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
      ],
    }));

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const count = await Recipe.count(mockPerspective, {});

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(5);
  });

  it("should use SurrealDB by default in ModelQueryBuilder.get()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
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

  it("should use SurrealDB by default in ModelQueryBuilder.count()", async () => {
    // count() counts the number of rows returned by the query (one row per source)
    const surrealResults = Array.from({ length: 3 }, (_, i) => ({
      source: `node:abc${i + 1}`,
      source_uri: `literal://recipe${i + 1}`,
      links: [
        {
          predicate: "recipe://name",
          target: `Recipe ${i + 1}`,
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
        {
          predicate: "recipe://rating",
          target: "5",
          author: "did:key:alice",
          timestamp: "2023-01-01T00:00:00Z",
        },
      ],
    }));
    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const count = await Recipe.query(mockPerspective)
      .where({ rating: { gt: 4 } })
      .count();

    expect(mockPerspective.querySurrealDB).toHaveBeenCalledTimes(1);
    expect(mockPerspective.infer).not.toHaveBeenCalled();
    expect(count).toBe(3);
  });

  it("should use SurrealDB by default in ModelQueryBuilder.paginate()", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://recipe1",
        links: [
          {
            predicate: "recipe://name",
            target: "Pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://rating",
            target: "5",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "recipe://ingredient",
            target: "pasta",
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
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

describe("resolveLanguage implicit literal default", () => {
  // A plain @Property with no resolveLanguage should behave identically to
  // resolveLanguage: "literal" — scalar values encoded as literal:// URIs
  // are unwrapped on read, and no createExpression call is made on write.
  @Model({ name: "ImplicitLiteralModel" })
  class ImplicitLiteralModel extends Ad4mModel {
    @Property({ through: "test://name" })
    name: string = "";

    @Property({ through: "test://count" })
    count: number = 0;

    @Property({ through: "test://active" })
    active: boolean = false;
  }

  const mockPerspective = {
    querySurrealDB: jest.fn(),
    infer: jest.fn(),
    uuid: "test-perspective-uuid",
    stringOrTemplateObjectToSubjectClassName: jest
      .fn()
      .mockResolvedValue("ImplicitLiteralModel"),
  } as any;

  beforeEach(() => jest.clearAllMocks());

  it('should store undefined resolveLanguage in metadata (not "literal")', () => {
    const metadata = ImplicitLiteralModel.getModelMetadata();
    expect(metadata.properties.name.resolveLanguage).toBeUndefined();
    expect(metadata.properties.count.resolveLanguage).toBeUndefined();
    expect(metadata.properties.active.resolveLanguage).toBeUndefined();
  });

  it("should hydrate literal:// encoded targets for properties with no resolveLanguage", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://base",
        links: [
          {
            predicate: "test://name",
            target: Literal.from("hello world").toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "test://count",
            target: Literal.from(42).toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
          {
            predicate: "test://active",
            target: Literal.from(true).toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);

    const result = await ImplicitLiteralModel.instancesFromSurrealResult(
      mockPerspective,
      {},
      surrealResults,
    );

    expect(result.results).toHaveLength(1);
    const instance = result.results[0];
    expect(instance.name).toBe("hello world");
    expect(instance.count).toBe(42);
    expect(instance.active).toBe(true);
  });

  it("should NOT call perspective.getExpression for properties with no resolveLanguage", async () => {
    const surrealResults = [
      {
        source: "node:abc123",
        source_uri: "literal://base",
        links: [
          {
            predicate: "test://name",
            target: Literal.from("test").toUrl(),
            author: "did:key:alice",
            timestamp: "2023-01-01T00:00:00Z",
          },
        ],
      },
    ];

    mockPerspective.querySurrealDB.mockResolvedValue(surrealResults);
    const getExpression = jest.fn();
    mockPerspective.getExpression = getExpression;

    await ImplicitLiteralModel.instancesFromSurrealResult(
      mockPerspective,
      {},
      surrealResults,
    );

    expect(getExpression).not.toHaveBeenCalled();
  });
});
