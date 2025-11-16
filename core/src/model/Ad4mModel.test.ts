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

