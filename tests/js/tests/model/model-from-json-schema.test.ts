/**
 * Ad4mModel.fromJSONSchema — integration tests
 *
 * Tests for the fromJSONSchema factory method: creating Ad4mModel subclasses
 * from JSON Schema definitions with explicit config, x-ad4m metadata,
 * title-based namespace inference, and error handling.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --serial --exit tests/model/model-from-json-schema.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy, Ad4mModel } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

describe("Ad4mModel.fromJSONSchema", () => {
  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy | null = null;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-from-json-schema");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    perspective = await ad4m.perspective.add("json-schema-test");
  });

  describe("with explicit configuration", () => {
    it("should create Ad4mModel class from JSON Schema with explicit namespace", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "Person",
        type: "object",
        properties: {
          name: { type: "string" },
          age: { type: "number" },
          email: { type: "string" },
        },
        required: ["name"],
      };

      const PersonClass = Ad4mModel.fromJSONSchema(schema, {
        name: "Person",
        namespace: "person://",
      }) as any;

      expect(PersonClass).to.be.a("function");
      expect(PersonClass.className).to.equal("Person");

      // Test instance creation
      const person = new PersonClass(perspective!);
      expect(person).to.be.instanceOf(Ad4mModel);
      expect(person.id).to.be.a("string");

      // Test property assignment
      person.name = "Alice Johnson";
      person.age = 30;
      person.email = "alice.johnson@example.com";

      await perspective!.ensureSDNASubjectClass(PersonClass);
      await person.save();

      // Create a second person to test multiple instances
      const person2 = new PersonClass(perspective!);
      person2.name = "Bob Smith";
      person2.age = 25;
      person2.email = "bob.smith@example.com";
      await person2.save();

      // Verify data was saved and can be retrieved
      const savedPeople = await PersonClass.findAll(perspective!);
      expect(savedPeople).to.have.lengthOf(2);

      // Find Alice
      const alice = savedPeople.find((p: any) => p.name === "Alice Johnson");
      expect(alice).to.exist;
      expect(alice!.name).to.equal("Alice Johnson");
      expect(alice!.age).to.equal(30);
      expect(alice!.email).to.equal("alice.johnson@example.com");

      // Find Bob
      const bob = savedPeople.find((p: any) => p.name === "Bob Smith");
      expect(bob).to.exist;
      expect(bob!.age).to.equal(25);

      // Test querying with where clauses
      const adults = await PersonClass.findAll(perspective!, {
        where: { age: { gt: 28 } },
      });
      expect(adults).to.have.lengthOf(1);
      expect(adults[0].name).to.equal("Alice Johnson");
    });

    it("should support property mapping overrides", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "Contact",
        type: "object",
        properties: {
          name: { type: "string" },
          email: { type: "string" },
        },
        required: ["name"],
      };

      const ContactClass = Ad4mModel.fromJSONSchema(schema, {
        name: "Contact",
        namespace: "contact://",
        propertyMapping: {
          name: "foaf://name",
          email: "foaf://mbox",
        },
      }) as any;
      expect(ContactClass.className).to.equal("Contact");

      // Test that custom predicates are used
      const contact = new ContactClass(perspective!);
      contact.name = "Bob Wilson";
      contact.email = "bob.wilson@company.com";

      await perspective!.ensureSDNASubjectClass(ContactClass);
      await contact.save();

      // Create second contact to test multiple instances
      const contact2 = new ContactClass(perspective!);
      contact2.name = "Carol Davis";
      contact2.email = "carol.davis@company.com";
      await contact2.save();

      // Verify data retrieval works with custom predicates
      const savedContacts = await ContactClass.findAll(perspective!);
      expect(savedContacts).to.have.lengthOf(2);
      const bob = savedContacts.find((c: any) => c.name === "Bob Wilson");
      expect(bob).to.exist;
      expect(bob!.email).to.equal("bob.wilson@company.com");

      // Verify the custom predicates were used by checking the generated SHACL
      const { shape: contactShape } = ContactClass.generateSHACL();
      expect(contactShape.properties.some((p: any) => p.path === "foaf://name"))
        .to.be.true;
      expect(contactShape.properties.some((p: any) => p.path === "foaf://mbox"))
        .to.be.true;

      // Test querying works with custom predicates
      const bobQuery = await ContactClass.findAll(perspective!, {
        where: { name: "Bob Wilson" },
      });
      expect(bobQuery).to.have.lengthOf(1);
      expect(bobQuery[0].email).to.equal("bob.wilson@company.com");
    });
  });

  describe("with JSON Schema x-ad4m metadata", () => {
    it("should use x-ad4m metadata when available", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "Product",
        type: "object",
        "x-ad4m": {
          namespace: "product://",
          className: "Product",
        },
        properties: {
          name: {
            type: "string",
            "x-ad4m": {
              through: "product://title",
            },
          },
          price: {
            type: "number",
            "x-ad4m": {
              through: "product://cost",
            },
          },
          description: {
            type: "string",
            "x-ad4m": {},
          },
        },
        required: ["name"],
      };

      const ProductClass = Ad4mModel.fromJSONSchema(schema, {
        name: "ProductOverride", // This should take precedence
      }) as any;

      expect(ProductClass.className).to.equal("ProductOverride");

      const product = new ProductClass(perspective!);
      product.name = "Gaming Laptop";
      product.price = 1299.99;
      product.description =
        "A high-performance gaming laptop with RTX graphics";

      await perspective!.ensureSDNASubjectClass(ProductClass);
      await product.save();

      // Create a second product with different pricing
      const product2 = new ProductClass(perspective!);
      product2.name = "Office Laptop";
      product2.price = 799.99;
      product2.description = "A reliable laptop for office work";
      await product2.save();

      // Test data retrieval and validation
      const savedProducts = await ProductClass.findAll(perspective!);
      expect(savedProducts).to.have.lengthOf(2);

      // Verify x-ad4m custom predicates work for data retrieval
      const gamingLaptop = savedProducts.find(
        (p: any) => p.name === "Gaming Laptop",
      );
      expect(gamingLaptop).to.exist;
      expect(gamingLaptop!.price).to.equal(1299.99);
      expect(gamingLaptop!.description).to.equal(
        "A high-performance gaming laptop with RTX graphics",
      );

      // Test querying with price ranges
      const expensiveProducts = await ProductClass.findAll(perspective!, {
        where: { price: { gt: 1000 } },
      });
      expect(expensiveProducts).to.have.lengthOf(1);
      expect(expensiveProducts[0].name).to.equal("Gaming Laptop");

      // Verify custom predicates from x-ad4m were used
      const { shape: productShape } = ProductClass.generateSHACL();
      expect(
        productShape.properties.some((p: any) => p.path === "product://title"),
      ).to.be.true; // custom predicate for name
      expect(
        productShape.properties.some((p: any) => p.path === "product://cost"),
      ).to.be.true; // custom predicate for price
      expect(
        productShape.properties.some(
          (p: any) => p.path === "product://description",
        ),
      ).to.be.true; // inferred from namespace + property
    });
  });

  describe("with title-based inference", () => {
    it("should infer namespace from schema title when no explicit config", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "Book",
        type: "object",
        properties: {
          title: { type: "string" },
          // Avoid reserved top-level "author" which conflicts with Ad4mModel built-in
          writer: { type: "string" },
          isbn: { type: "string" },
        },
        required: ["title"],
      };

      const BookClass = Ad4mModel.fromJSONSchema(schema, {
        name: "Book",
      }) as any;
      expect(BookClass.className).to.equal("Book");

      const book = new BookClass(perspective!);
      book.title = "The Great Gatsby";
      book.writer = "F. Scott Fitzgerald";
      book.isbn = "978-0-7432-7356-5";

      await perspective!.ensureSDNASubjectClass(BookClass);
      await book.save();

      // Add a second book
      const book2 = new BookClass(perspective!);
      book2.title = "To Kill a Mockingbird";
      book2.writer = "Harper Lee";
      book2.isbn = "978-0-06-112008-4";
      await book2.save();

      // Test data retrieval with inferred predicates
      const savedBooks = await BookClass.findAll(perspective!);
      expect(savedBooks).to.have.lengthOf(2);
      const gatsby = savedBooks.find(
        (b: any) => b.title === "The Great Gatsby",
      );
      expect(gatsby).to.exist;
      expect(gatsby!.writer).to.equal("F. Scott Fitzgerald");
      expect(gatsby!.isbn).to.equal("978-0-7432-7356-5");

      // Test querying by author
      const fitzgeraldBooks = await BookClass.findAll(perspective!, {
        where: { writer: "F. Scott Fitzgerald" },
      });
      expect(fitzgeraldBooks).to.have.lengthOf(1);
      expect(fitzgeraldBooks[0].title).to.equal("The Great Gatsby");

      // Verify inferred predicates (should be book://title, book://writer, etc.)
      const { shape: bookShape } = BookClass.generateSHACL();
      expect(bookShape.properties.some((p: any) => p.path === "book://title"))
        .to.be.true;
      expect(bookShape.properties.some((p: any) => p.path === "book://writer"))
        .to.be.true;
      expect(bookShape.properties.some((p: any) => p.path === "book://isbn")).to
        .be.true;
    });
  });

  describe("error handling", () => {
    it("should throw error when no title and no namespace provided", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        type: "object",
        properties: {
          value: { type: "string" },
        },
        required: ["value"], // Add required property to avoid constructor error
      };

      expect(() => {
        Ad4mModel.fromJSONSchema(schema, { name: "Test" });
      }).to.throw(/Cannot infer namespace/);
    });

    it("should handle all-optional properties without auto-generating a type flag", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "OptionalOnly",
        type: "object",
        properties: {
          optionalValue: { type: "string" },
          anotherOptional: { type: "number" },
        },
        // No required array - all properties are optional
      };

      // Should not throw error — open-world structural matching applies
      const OptionalClass = Ad4mModel.fromJSONSchema(schema, {
        name: "OptionalOnly",
        namespace: "test://",
      }) as any;

      expect(OptionalClass).to.be.a("function");
      expect(OptionalClass.className).to.equal("OptionalOnly");

      // Should NOT have automatic type flag
      const instance = new OptionalClass(perspective!);
      expect(instance.__ad4m_type).to.be.undefined;

      // SHACL should NOT include an ad4m://type property
      const { shape: optionalShape } = OptionalClass.generateSHACL();
      expect(
        optionalShape.properties.some((p: any) => p.path === "ad4m://type"),
      ).to.be.false;

      // Constructor/destructor should exist but be empty
      expect(optionalShape.constructor_actions).to.deep.equal([]);
      expect(optionalShape.destructor_actions).to.deep.equal([]);
    });

    it("should work when properties have explicit initial values even if not required", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "WithInitials",
        type: "object",
        properties: {
          status: { type: "string" },
          count: { type: "number" },
        },
        // No required array, but we'll provide initial values
      };

      // This should work because we provide initial values
      const TestClass = Ad4mModel.fromJSONSchema(schema, {
        name: "WithInitials",
        namespace: "test://",
        propertyOptions: {
          status: { initial: "test://active" },
          count: { initial: "literal://number:0" },
        },
      }) as any;

      expect(TestClass).to.be.a("function");
      expect(TestClass.className).to.equal("WithInitials");

      // Verify SHACL has constructor actions
      const { shape: testShape } = TestClass.generateSHACL();
      expect(testShape.constructor_actions).to.exist;
      expect(testShape.constructor_actions!.length).to.be.greaterThan(0);
      expect(
        testShape.constructor_actions!.some(
          (a: any) => a.target === "test://active",
        ),
      ).to.be.true;
      expect(
        testShape.constructor_actions!.some(
          (a: any) => a.target === "literal://number:0",
        ),
      ).to.be.true;
    });

    it("should handle complex property types with full data storage and retrieval", async () => {
      const schema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "BlogPost",
        type: "object",
        properties: {
          title: { type: "string" },
          tags: {
            type: "array",
            items: { type: "string" },
          },
          metadata: {
            type: "object",
            properties: {
              created: { type: "string" },
              author: { type: "string" },
              views: { type: "number" },
            },
          },
          categories: {
            type: "array",
            items: { type: "string" },
          },
        },
        required: ["title"],
      };

      const BlogPostClass = Ad4mModel.fromJSONSchema(schema, {
        name: "BlogPost",
      }) as any;
      expect(BlogPostClass.className).to.equal("BlogPost");

      await perspective!.ensureSDNASubjectClass(BlogPostClass);

      // Create a blog post with complex data
      const post1 = new BlogPostClass(perspective!);
      post1.title = "Getting Started with AD4M";

      // Test array/collection handling
      post1.tags = ["tag://ad4m", "tag://tutorial", "tag://blockchain"];
      post1.categories = ["category://technology", "category://development"];

      // Test complex object handling (should be stored as JSON)
      post1.metadata = {
        created: "2025-09-22T10:00:00Z",
        author: "Alice",
        views: 42,
      };

      await post1.save();

      // Create a second post
      const post2 = new BlogPostClass(perspective!);
      post2.title = "Advanced AD4M Patterns";
      post2.tags = ["tag://ad4m", "tag://advanced", "tag://patterns"];
      post2.categories = ["category://technology"];
      post2.metadata = {
        created: "2025-09-22T11:00:00Z",
        author: "Bob",
        views: 15,
      };
      await post2.save();

      // Test data retrieval
      const savedPosts = await BlogPostClass.findAll(perspective!);
      expect(savedPosts).to.have.lengthOf(2);

      // Verify complex object data is preserved
      const tutorialPost = savedPosts.find(
        (p: any) => p.title === "Getting Started with AD4M",
      );
      expect(tutorialPost).to.exist;
      expect(tutorialPost!.tags).to.be.an("array");
      expect(tutorialPost!.tags).to.include.members([
        "tag://ad4m",
        "tag://tutorial",
        "tag://blockchain",
      ]);
      expect(tutorialPost!.metadata).to.be.an("object");
      expect(tutorialPost!.metadata.author).to.equal("Alice");
      expect(tutorialPost!.metadata.views).to.equal(42);
      expect(tutorialPost!.metadata.created).to.equal("2025-09-22T10:00:00Z");

      // Test querying by title
      const advancedPosts = await BlogPostClass.findAll(perspective!, {
        where: { title: "Advanced AD4M Patterns" },
      });
      expect(advancedPosts).to.have.lengthOf(1);
      expect(advancedPosts[0].metadata.author).to.equal("Bob");

      // Verify SHACL structure for complex types
      const { shape: blogShape } = BlogPostClass.generateSHACL();
      // collections: no maxCount constraint
      expect(blogShape.properties.some((p: any) => !p.maxCount)).to.be.true; // tags and categories should be collections
      // scalars: maxCount === 1
      expect(blogShape.properties.some((p: any) => p.maxCount === 1)).to.be
        .true; // title and metadata should be scalar properties
      expect(
        blogShape.properties.some((p: any) => p.path === "blogpost://title"),
      ).to.be.true;
      expect(
        blogShape.properties.some((p: any) => p.path === "blogpost://tags"),
      ).to.be.true;
      expect(
        blogShape.properties.some((p: any) => p.path === "blogpost://metadata"),
      ).to.be.true;
      expect(
        blogShape.properties.some(
          (p: any) => p.path === "blogpost://categories",
        ),
      ).to.be.true;
    });

    it("should handle realistic Holon-like schema with nested objects", async () => {
      const holonSchema = {
        $schema: "http://json-schema.org/draft-07/schema#",
        title: "PersonHolon",
        type: "object",
        properties: {
          name: { type: "string" },
          email: { type: "string" },
          profile: {
            type: "object",
            properties: {
              bio: { type: "string" },
              location: { type: "string" },
            },
          },
          skills: {
            type: "array",
            items: { type: "string" },
          },
        },
        required: ["name", "email"],
      };

      const PersonHolonClass = Ad4mModel.fromJSONSchema(holonSchema, {
        name: "PersonHolon",
        namespace: "holon://person/",
      }) as any;

      await perspective!.ensureSDNASubjectClass(PersonHolonClass);

      // Test with realistic data
      const person = new PersonHolonClass(perspective!);
      person.name = "Alice Cooper";
      person.email = "alice@example.com";
      person.skills = [
        "skill://javascript",
        "skill://typescript",
        "skill://ad4m",
      ];
      person.profile = {
        bio: "Software developer passionate about decentralized systems",
        location: "San Francisco",
      };
      await person.save();

      // Verify retrieval preserves nested structure
      const retrieved = await PersonHolonClass.findAll(perspective!);
      expect(retrieved).to.have.lengthOf(1);

      const alice = retrieved[0];
      expect(alice.profile).to.be.an("object");
      expect(alice.profile.bio).to.equal(
        "Software developer passionate about decentralized systems",
      );
      expect(alice.skills).to.include.members([
        "skill://javascript",
        "skill://typescript",
        "skill://ad4m",
      ]);
    });
  });
});
