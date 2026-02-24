import { expect } from "chai";
import { ChildProcess } from "node:child_process";
import {
  Ad4mClient,
  Link,
  LinkQuery,
  Literal,
  PerspectiveProxy,
  SmartLiteral,
  SMART_LITERAL_CONTENT_PREDICATE,
  Ad4mModel,
  Flag,
  Property,
  HasMany,
  Model,
  PropertyOptions,
} from "@coasys/ad4m";
import { readFileSync } from "node:fs";
import { startExecutor, apolloClient, sleep } from "../utils/utils";
import path from "path";
import { fileURLToPath } from "url";
import fetch from "node-fetch";
import sinon from "sinon";

//@ts-ignore
global.fetch = fetch;

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("SDNA", () => {
  let ad4m: Ad4mClient | null = null;
  let executorProcess: ChildProcess | null = null;

  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "prolog-agent");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
  const gqlPort = 16600;
  const hcAdminPort = 16601;
  const hcAppPort = 16602;

  before(async () => {
    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      gqlPort,
      hcAdminPort,
      hcAppPort,
    );

    console.log("Creating ad4m client");
    // @ts-ignore - Apollo Client version mismatch between dependencies
    ad4m = new Ad4mClient(apolloClient(gqlPort));
    console.log("Generating agent");
    await ad4m.agent.generate("secret");
    console.log("Done");
  });

  after(async () => {
    if (executorProcess) {
      while (!executorProcess?.killed) {
        let status = executorProcess?.kill();
        console.log("killed executor with", status);
        await sleep(500);
      }
    }
  });

  it("should get agent status", async () => {
    let result = await ad4m!.agent.status();
    expect(result).to.not.be.null;
    expect(result!.isInitialized).to.be.true;
  });

  describe("Subjects (SHACL-based API)", () => {
    let perspective: PerspectiveProxy | null = null;

    before(async () => {
      perspective = await ad4m!.perspective.add("test");
      // for test debugging:
      //console.log("UUID: " + perspective.uuid)
    });

    // REMOVED: Legacy Prolog SDNA test - Prolog SDNA is superseded by SHACL
    // The addSdna API now accepts optional sdnaCode with shaclJson being the primary input.
    // See "SDNA creation decorators" tests below for the modern SHACL-based API.

    // NOTE: Legacy Subject proxy tests removed in SHACL migration PR.
    // The Subject proxy API (Subject.init(), getSubjectProxy()) requires Prolog queries
    // and has been superseded by the Ad4mModel API which is Prolog-free and SHACL-native.
    // Production code (Flux) uses Ad4mModel exclusively.
    // See "SDNA creation decorators" tests below for the modern API.

    describe("SDNA creation decorators", () => {
      @Model({
        name: "Message",
      })
      class Message extends Ad4mModel {
        @Flag({
          through: "ad4m://type",
          value: "ad4m://message",
        })
        type: string = "";

        static async all(perspective: PerspectiveProxy): Promise<Message[]> {
          return Message.findAll(perspective);
        }

        @Property({
          through: "todo://state",
        })
        body?: string;
      }

      // This class matches the SDNA in ./sdna/subject.pl
      // and this test proves the decorators create the exact same SDNA code
      @Model({
        name: "Todo",
      })
      class Todo extends Ad4mModel {
        // Setting this member "subjectConstructer" allows for adding custom
        // actions that will be run when a subject is constructed.
        //
        // In this test, we don't need to use it, because the used "initial"
        // parameter on "state" below will have the same effect as the following:
        // subjectConstructor = [addLink("this", "todo://state", "todo://ready")]

        // Setting this member "isSubjectInstance" allows for adding custom clauses
        // to the instance check.
        //
        // In this test, we don't need to use it, because the used "required"
        // parameter on "state" below will have the same effect as the following:
        // isSubjectInstance = [hasLink("todo://state")]

        static async all(perspective: PerspectiveProxy): Promise<Todo[]> {
          return Todo.findAll(perspective);
        }

        static async allReady(perspective: PerspectiveProxy): Promise<Todo[]> {
          return Todo.findAll(perspective, {
            where: { state: "todo://ready" },
          });
        }

        static async allDone(perspective: PerspectiveProxy): Promise<Todo[]> {
          return Todo.findAll(perspective, { where: { state: "todo://done" } });
        }

        //@ts-ignore
        @Property({
          through: "todo://state",
          initial: "todo://ready",
        })
        state!: string;

        @Property({
          through: "todo://has_title",
          writable: true,
          resolveLanguage: "literal",
        })
        title?: string;

        @HasMany({ through: "todo://comment" })
        comments: string[] = [];

        @HasMany({ through: "flux://entry_type" })
        entries: string[] = [];

        @HasMany({
          through: "flux://entry_type",
          where: { isInstance: Message },
        })
        messages: string[] = [];
      }

      before(async () => {
        // Register SHACL SDNA once for all tests in this block
        await perspective!.ensureSDNASubjectClass(Todo);
      });

      it("should find the TODO subject class from the test SDNA", async () => {
        let classes = await perspective!.subjectClasses();

        expect(classes.length).to.equal(1);
        expect(classes[0]).to.equal("Todo");
      });

      it("should generate correct SDNA from a JS class", async () => {
        // @ts-ignore
        const { name, sdna } = Todo.generateSDNA();

        const regExp = /\("Todo", ([^)]+)\)/;
        const matches = regExp.exec(sdna);
        const value = matches![1];

        const equal = readFileSync("./sdna/subject.pl")
          .toString()
          .replace(/c\)/g, `${value})`)
          .replace(/\(c/g, `(${value}`);

        expect(sdna.normalize("NFC")).to.equal(equal.normalize("NFC"));
      });

      it("should be possible to use that class for type-safe interaction with subject instances", async () => {
        // Create additional todos for the following tests
        // Todo 1: stays at initial "ready" state
        let root1 = Literal.from("Ready todo").toUrl();
        let todo1 = new Todo(perspective!, root1);
        await todo1.save();

        // Todo 2 & 3: set to "done" state
        let root2 = Literal.from("Done todo 1").toUrl();
        let todo2 = new Todo(perspective!, root2);
        await todo2.save();
        todo2.state = "todo://done";
        await todo2.save();

        let root3 = Literal.from("Done todo 2").toUrl();
        let todo3 = new Todo(perspective!, root3);
        await todo3.save();
        todo3.state = "todo://done";
        await todo3.save();

        // construct new subject intance using Ad4mModel API
        let root = Literal.from("Decorated class construction test").toUrl();

        let todo = new Todo(perspective!, root);
        await todo.save();

        // Verify the instance was created with required links
        const stateLinks = await perspective!.get(
          new LinkQuery({ source: root, predicate: "todo://state" }),
        );
        expect(stateLinks.length).to.equal(1);
        expect(stateLinks[0].data.target).to.equal("todo://ready");

        // Check name mapping
        const nameMappingUrl = Literal.fromUrl(
          `literal://string:shacl://Todo`,
        ).toUrl();
        const nameMappingLinks = await perspective!.get(
          new LinkQuery({ source: nameMappingUrl }),
        );
        nameMappingLinks.forEach((link) =>
          console.log("  ", link.data.predicate, "->", link.data.target),
        );

        const isInstance = await perspective!.isSubjectInstance(root, Todo);
        expect(isInstance).to.not.be.false;

        // Ad4mModel API - use the todo instance directly (no need for getSubjectProxy)
        expect(todo).to.have.property("state");
        expect(todo).to.have.property("title");
        expect(todo).to.have.property("comments");

        todo.state = "todo://review";
        await todo.save();
        const stateAfter = todo.state;

        expect(stateAfter).to.equal("todo://review");
        expect(todo.comments).to.be.empty;

        let comment = Literal.from("new comment").toUrl();
        todo.comments = [comment];
        await todo.save();
        expect(todo.comments).to.deep.equal([comment]);
      });

      it("can retrieve all instances through instaceQuery decoratored all()", async () => {
        let todos = await Todo.all(perspective!);
        expect(todos.length).to.equal(4);
      });

      it("can retrieve all mathching instance through InstanceQuery(where: ..)", async () => {
        let todos = await Todo.allReady(perspective!);
        expect(todos.length).to.equal(1);
        expect(todos[0].state).to.equal("todo://ready");

        todos = await Todo.allDone(perspective!);
        expect(todos.length).to.equal(2);
        expect(todos[0].state).to.equal("todo://done");
      });

      // REMOVED: InstanceQuery(condition: ..) test - required Prolog-only allSelf method
      // The InstanceQuery with condition parameter required Prolog inference.
      // Future: Could be reimplemented with SHACL-based query conditions via SurrealDB.

      it("can deal with properties that resolve the URI and create Expressions", async () => {
        let todos = await Todo.all(perspective!);

        // Guard: If no todos exist, create one for this test
        if (todos.length === 0) {
          throw new Error(
            "Test prerequisite failed: No todos available. Please ensure todos are created in the setup or earlier tests.",
          );
        }

        // Find a todo without a title (to avoid data contamination from other tests)
        let todo = null;
        for (const t of todos) {
          const title = t.title;
          if (title === undefined || title === null || title === "") {
            todo = t;
            break;
          }
        }

        if (!todo) {
          // If all todos have titles, use the first one and clear its title
          // Safe to access todos[0] since we've checked todos.length > 0 above
          todo = todos[0];
          const existingLinks = await perspective!.get(
            new LinkQuery({
              source: todo.id,
              predicate: "todo://has_title",
            }),
          );
          for (const link of existingLinks) {
            await perspective!.remove(link);
          }
        }

        expect(todo.title).to.be.undefined;

        // Use direct assignment + update() pattern (setters are stubs)
        todo.title = "new title";
        await todo.save();
        expect(todo.title).to.equal("new title");

        let links = await perspective!.get(
          new LinkQuery({
            source: todo.id,
            predicate: "todo://has_title",
          }),
        );
        expect(links.length).to.equal(1);
        let literal = Literal.fromUrl(links[0].data.target).get();
        expect(literal.data).to.equal("new title");
      });

      it("can easily be initialized with PerspectiveProxy.ensureSDNASubjectClass()", async () => {
        expect(await perspective!.getSdna()).to.have.lengthOf(1);

        @Model({
          name: "Test",
        })
        class Test {
          @Property({
            through: "test://test_numer",
          })
          number: number = 0;
        }

        await perspective!.ensureSDNASubjectClass(Test);

        expect(await perspective!.getSdna()).to.have.lengthOf(2);
        //console.log((await perspective!.getSdna())[1])
      });

      // REMOVED: Custom getter prolog code test - required Prolog-based property getters
      // The isLiked property used custom Prolog code for computed values.
      // Future: Could be reimplemented with SHACL-based computed properties or SurrealDB queries.

      describe("with Message subject class registered", () => {
        before(async () => {
          await perspective!.ensureSDNASubjectClass(Message);
        });

        afterEach(async () => {
          // Clean up any Message flags created during tests to prevent data contamination
          const links = await perspective!.get(
            new LinkQuery({
              predicate: "ad4m://type",
              target: "ad4m://message",
            }),
          );
          for (const link of links) {
            await perspective!.remove(link);
          }
        });

        it("can find instances through the exact flag link", async () => {
          await perspective!.add(
            new Link({
              source: "test://message",
              predicate: "ad4m://type",
              target: "ad4m://undefined",
            }),
          );

          const first = await Message.all(perspective!);
          expect(first.length).to.be.equal(0);

          await perspective!.add(
            new Link({
              source: "test://message",
              predicate: "ad4m://type",
              target: "ad4m://message",
            }),
          );

          const second = await Message.all(perspective!);
          expect(second.length).to.be.equal(1);
        });

        it("can constrain collection entries through 'where' clause", async () => {
          let root = Literal.from("Collection where test").toUrl();
          let messageEntry = Literal.from("test message").toUrl();

          // Create todo with entries already set
          let todo = new Todo(perspective!, root);
          todo.entries = [messageEntry];
          await todo.save();

          let entries = todo.entries;
          expect(entries.length).to.equal(1);

          let messageEntries = todo.messages;
          expect(messageEntries.length).to.equal(0);

          let message = new Message(perspective!, messageEntry);
          await message.save();

          // Refresh todo data to apply collection filtering
          await todo.get();
          messageEntries = todo.messages;
          expect(messageEntries.length).to.equal(1);
        });
      });

      describe("Active record implementation", () => {
        @Model({
          name: "Recipe",
        })
        class Recipe extends Ad4mModel {
          @Flag({
            through: "ad4m://type",
            value: "ad4m://recipe",
          })
          type: string = "";

          @Property({
            through: "recipe://plain",
          })
          plain: string = "";

          @Property({
            through: "recipe://name",
            resolveLanguage: "literal",
          })
          name: string = "";

          @Property({
            through: "recipe://boolean",
            resolveLanguage: "literal",
          })
          booleanTest: boolean = false;

          @Property({
            through: "recipe://number",
            resolveLanguage: "literal",
          })
          number: number = 0;

          @HasMany({ through: "recipe://entries" })
          entries: string[] = [];

          @HasMany({ through: "recipe://comment" })
          comments: string[] = [];

          @Property({
            through: "recipe://local",
            local: true,
          })
          local: string = "";

          @Property({
            through: "recipe://resolve",
            resolveLanguage: "literal",
          })
          resolve: string = "";

          @Property({
            through: "recipe://image",
            resolveLanguage: "", // Will be set dynamically to note-store language
            transform: (data: any) => {
              if (data && typeof data === "object" && data.data_base64) {
                return `data:image/png;base64,${data.data_base64}`;
              }
              return data;
            },
          } as PropertyOptions)
          image: string | any = "";
        }

        beforeEach(async () => {
          if (perspective) {
            await ad4m!.perspective.remove(perspective.uuid);
          }
          perspective = await ad4m!.perspective.add(
            "active-record-implementation-test",
          );
          await perspective!.ensureSDNASubjectClass(Recipe);
        });

        it("save() & get() local", async () => {
          let root = Literal.from(
            "Active record implementation test local link",
          ).toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "Local test";
          recipe.local = "recipe://test";

          await recipe.save();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();

          expect(recipe2.name).to.equal("Local test");
          expect(recipe2.local).to.equal("recipe://test");

          // @ts-ignore
          const links = await perspective?.get({
            source: root,
            predicate: "recipe://local",
          });

          expect(links!.length).to.equal(1);
          expect(links![0].status).to.equal("LOCAL");
        });

        it("can constrain collection entries through 'where' clause with condition", async () => {
          // Define a Recipe model with condition filtering
          @Model({ name: "RecipeWithSurrealFilter" })
          class RecipeWithSurrealFilter extends Ad4mModel {
            @Flag({
              through: "ad4m://type",
              value: "recipe://instance",
            })
            type: string = "";

            @Property({
              through: "recipe://name",
              resolveLanguage: "literal",
            })
            name: string = "";

            @HasMany({ through: "recipe://entries" })
            entries: string[] = [];

            @HasMany({
              through: "recipe://entries",
              where: {
                condition: `WHERE in.uri = Target AND predicate = 'recipe://has_ingredient' AND out.uri = 'recipe://test'`,
              },
            })
            ingredients: string[] = [];
          }

          // Register the class
          await perspective!.ensureSDNASubjectClass(RecipeWithSurrealFilter);

          // Wait for SHACL metadata to be indexed
          await sleep(500);

          let root = Literal.from(
            "Active record surreal condition test",
          ).toUrl();
          const recipe = new RecipeWithSurrealFilter(perspective!, root);

          let entry1 = Literal.from("entry with ingredient").toUrl();
          let entry2 = Literal.from("entry without ingredient").toUrl();

          recipe.entries = [entry1, entry2];
          recipe.name = "Condition test";

          await recipe.save();

          // Add the ingredient link to entry1 only
          await perspective?.add(
            new Link({
              source: entry1,
              predicate: "recipe://has_ingredient",
              target: "recipe://test",
            }),
          );

          // Small delay for SurrealDB indexing
          await sleep(500);

          const recipe2 = new RecipeWithSurrealFilter(perspective!, root);
          await recipe2.get();

          // Should have 2 entries total
          expect(recipe2.entries.length).to.equal(2);

          // But only 1 ingredient (entry1 which has the ingredient link)
          expect(recipe2.ingredients.length).to.equal(1);
          expect(recipe2.ingredients[0]).to.equal(entry1);
        });

        it("can implement the resolveLanguage property type", async () => {
          let root = Literal.from(
            "Active record implementation test resolveLanguage",
          ).toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.resolve = "Test name literal";

          await recipe.save();

          //@ts-ignore
          let links = await perspective!.get(
            new LinkQuery({ source: root, predicate: "recipe://resolve" }),
          );
          expect(links.length).to.equal(1);
          let literal = Literal.fromUrl(links[0].data.target).get();
          expect(literal.data).to.equal(recipe.resolve);

          const recipe3 = new Recipe(perspective!, root);
          await recipe3.get();
          expect(recipe3.resolve).to.equal("Test name literal");
        });

        it("can resolve non-literal languages with resolveLanguage and transform", async () => {
          // Publish note-store language to use as a non-literal resolveLanguage
          const noteLanguage = await ad4m!.languages.publish(
            path
              .join(__dirname, "../languages/note-store/build/bundle.js")
              .replace(/\\/g, "/"),
            {
              name: "note-store-test",
              description: "Test language for non-literal resolution",
            },
          );
          const noteLangAddress = noteLanguage.address;

          // Create an expression in the note-store language with test data (simulating file data)
          const testImageData = {
            data_base64:
              "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
          };
          const imageExprUrl = await ad4m!.expression.create(
            testImageData,
            noteLangAddress,
          );

          let root = Literal.from(
            "Active record implementation test resolveLanguage non-literal",
          ).toUrl();
          const recipe = new Recipe(perspective!, root);

          // Manually add the link instead of using save() to test the query resolution path
          recipe.name = "Test with image";
          await recipe.save(); // Save the name

          // Add the image link manually
          await perspective!.setSingleTarget(
            new Link({
              source: root,
              predicate: "recipe://image",
              target: imageExprUrl,
            }),
          );

          // Verify the link was created with the expression URL
          //@ts-ignore
          let links = await perspective!.get(
            new LinkQuery({ source: root, predicate: "recipe://image" }),
          );
          expect(links.length).to.equal(1);
          expect(links[0].data.target).to.equal(imageExprUrl);

          // Retrieve the recipe and verify the image was resolved and transformed
          const results = await Recipe.findAll(perspective!, {
            where: { name: "Test with image" },
          });
          const recipe2 = results[0];

          expect(recipe2.name).to.equal("Test with image");
          // The image should be resolved from the note-store language and transformed to a data URL
          expect(recipe2.image).to.equal(
            `data:image/png;base64,${testImageData.data_base64}`,
          );
        });

        it("works with very long property values", async () => {
          let root = Literal.from(
            "Active record implementation test long value",
          ).toUrl();
          const recipe = new Recipe(perspective!, root);

          const longName =
            "This is a very long recipe name that goes on and on with many many characters to test that we can handle long property values without any issues whatsoever and keep going even longer to make absolutely sure we hit at least 300 characters in this test string that just keeps getting longer and longer until we are completely satisfied that it works properly with such lengthy content. But wait, there's more! We need to make this string even longer to properly test the system's ability to handle extremely long property values. Let's add some more meaningful content about recipes - ingredients like flour, sugar, eggs, milk, butter, vanilla extract, baking powder, salt, and detailed instructions for mixing them together in just the right way to create the perfect baked goods. We could go on about preheating the oven to the right temperature, greasing the pans properly, checking for doneness with a toothpick, and letting things cool completely before frosting. The possibilities are endless when it comes to recipe details and instructions that could make this string longer and longer. We want to be absolutely certain that our system can handle property values of any reasonable length without truncating or corrupting the data in any way. This is especially important for recipes where precise instructions and ingredient amounts can make the difference between success and failure in the kitchen. Testing with realistically long content helps ensure our system works reliably in real-world usage scenarios where users might enter detailed information that extends well beyond a few simple sentences.";
          // Use resolve (resolveLanguage: "literal") to store the long string value.
          // The plain property is for storing URI addresses; resolve encodes arbitrary strings.
          recipe.resolve = longName;

          await recipe.save();

          let linksResolve = await perspective!.get(
            new LinkQuery({ source: root, predicate: "recipe://resolve" }),
          );
          expect(linksResolve.length).to.equal(1);
          let expression = Literal.fromUrl(linksResolve[0].data.target).get();
          expect(expression.data).to.equal(longName);

          const recipe2 = new Recipe(perspective!, root);
          await recipe2.get();

          expect(recipe2.resolve.length).to.equal(longName.length);
          expect(recipe2.resolve).to.equal(longName);
        });

        it("get() returns all subject entity properties (via getData())", async () => {
          let root = Literal.from("getData test").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "getData all test";
          recipe.booleanTest = true;
          recipe.comments = ["recipe://comment1", "recipe://comment2"];
          recipe.local = "recipe://local_test";
          recipe.resolve = "Resolved literal value";

          await recipe.save();

          const data = await recipe.get();

          expect(data.name).to.equal("getData all test");
          expect(data.booleanTest).to.equal(true);
          // Collection order might not be preserved when items are added simultaneously
          // Check that both items exist rather than exact order
          expect(data.comments).to.have.lengthOf(2);
          expect(data.comments).to.include("recipe://comment1");
          expect(data.comments).to.include("recipe://comment2");
          expect(data.local).to.equal("recipe://local_test");
          expect(data.resolve).to.equal("Resolved literal value");

          await recipe.delete();
        });

        it("findAll() works with constraining resolved literal properties", async () => {
          // Create a recipe with a resolved literal property
          const recipe = new Recipe(perspective!);
          recipe.resolve = "Hello World";
          await recipe.save();

          // Test with resolved literal property
          const recipes1 = await Recipe.findAll(perspective!, {
            where: { resolve: "Hello World" },
          });
          expect(recipes1.length).to.equal(1);
          expect(recipes1[0].resolve).to.equal("Hello World");
        });

        it("transform option in property decorators works", async () => {
          const transformTestPerspective =
            await ad4m?.perspective.add("transform-test");
          @Model({ name: "ImagePost" })
          class ImagePost extends Ad4mModel {
            @Property({
              through: "image://data",
              resolveLanguage: "literal",
              transform: (data: any) =>
                data ? `data:image/png;base64,${data}` : undefined,
            } as PropertyOptions)
            image: string = "";
            //TODO: having json objects as properties in our new queries breaks the JSON
            // construction of Prolog query results.
            // Need to find a way to make this work:
            //image: { data_base64: string } = { data_base64: "" };
          }

          // Register the ImagePost class
          await transformTestPerspective!.ensureSDNASubjectClass(ImagePost);

          // Create a new image post
          const post = new ImagePost(transformTestPerspective!);
          const imageData = "abc123";
          //const imageData = { data_base64: "abc123" };

          post.image = imageData;
          await post.save();

          // Retrieve the post and check transformed values
          const [retrieved] = await ImagePost.findAll(
            transformTestPerspective!,
          );
          expect(retrieved.image).to.equal("data:image/png;base64,abc123");
        });

        it("should support batch operations with multiple models", async () => {
          let perspective = await ad4m!.perspective.add("batch test");
          @Model({
            name: "BatchRecipe",
          })
          class BatchRecipe extends Ad4mModel {
            @Property({
              through: "recipe://name",
              resolveLanguage: "literal",
            })
            name: string = "";

            @HasMany({ through: "recipe://ingredients" })
            ingredients: string[] = [];
          }

          @Model({
            name: "BatchNote",
          })
          class BatchNote extends Ad4mModel {
            @Property({
              through: "note://title",
              resolveLanguage: "literal",
            })
            title: string = "";

            @Property({
              through: "note://content",
              resolveLanguage: "literal",
            })
            content: string = "";
          }

          // Register the classes
          await perspective!.ensureSDNASubjectClass(BatchRecipe);
          await perspective!.ensureSDNASubjectClass(BatchNote);

          // Create batch
          const batchId = await perspective!.createBatch();

          // Create and save multiple models in batch
          const recipe = new BatchRecipe(perspective!);
          recipe.name = "Pasta";
          recipe.ingredients = [
            "recipe://ingredient/pasta",
            "recipe://ingredient/sauce",
            "recipe://ingredient/cheese",
          ];
          await recipe.save(batchId);

          const note = new BatchNote(perspective!);
          note.title = "Recipe Notes";
          note.content = "Make sure to use fresh ingredients";
          await note.save(batchId);

          // Verify models are not visible before commit
          const recipesBeforeCommit = await BatchRecipe.findAll(perspective!);
          expect(recipesBeforeCommit.length).to.equal(0);

          const notesBeforeCommit = await BatchNote.findAll(perspective!);
          expect(notesBeforeCommit.length).to.equal(0);

          // Commit batch
          const result = await perspective!.commitBatch(batchId);
          expect(result.additions.length).to.be.greaterThan(0);
          expect(result.removals.length).to.equal(0);

          // Verify models are now visible
          const recipesAfterCommit = await BatchRecipe.findAll(perspective!);
          expect(recipesAfterCommit.length).to.equal(1);
          expect(recipesAfterCommit[0].name).to.equal("Pasta");
          expect(recipesAfterCommit[0].ingredients).to.have.members([
            "recipe://ingredient/pasta",
            "recipe://ingredient/sauce",
            "recipe://ingredient/cheese",
          ]);

          const notesAfterCommit = await BatchNote.findAll(perspective!);
          expect(notesAfterCommit.length).to.equal(1);
          expect(notesAfterCommit[0].title).to.equal("Recipe Notes");
          expect(notesAfterCommit[0].content).to.equal(
            "Make sure to use fresh ingredients",
          );

          // Test updating models in batch
          const updateBatchId = await perspective!.createBatch();
          recipe.ingredients.push("recipe://ingredient/garlic");
          await recipe.update(updateBatchId);

          note.content = "Updated: Use fresh ingredients and add garlic";
          await note.update(updateBatchId);

          // Verify models haven't changed before commit
          const recipesBeforeUpdate = await BatchRecipe.findAll(perspective!);
          expect(recipesBeforeUpdate[0].ingredients).to.have.members([
            "recipe://ingredient/pasta",
            "recipe://ingredient/sauce",
            "recipe://ingredient/cheese",
          ]);

          const notesBeforeUpdate = await BatchNote.findAll(perspective!);
          expect(notesBeforeUpdate[0].content).to.equal(
            "Make sure to use fresh ingredients",
          );

          // Commit update batch
          const updateResult = await perspective!.commitBatch(updateBatchId);
          expect(updateResult.additions.length).to.be.greaterThan(0);

          // Verify models are updated
          const recipesAfterUpdate = await BatchRecipe.findAll(perspective!);
          expect(recipesAfterUpdate[0].ingredients.length).to.equal(4);
          expect(
            recipesAfterUpdate[0].ingredients.includes(
              "recipe://ingredient/pasta",
            ),
          ).to.be.true;
          expect(
            recipesAfterUpdate[0].ingredients.includes(
              "recipe://ingredient/sauce",
            ),
          ).to.be.true;
          expect(
            recipesAfterUpdate[0].ingredients.includes(
              "recipe://ingredient/cheese",
            ),
          ).to.be.true;
          expect(
            recipesAfterUpdate[0].ingredients.includes(
              "recipe://ingredient/garlic",
            ),
          ).to.be.true;

          const notesAfterUpdate = await BatchNote.findAll(perspective!);
          expect(notesAfterUpdate[0].content).to.equal(
            "Updated: Use fresh ingredients and add garlic",
          );

          // Test deleting models in batch
          const deleteBatchId = await perspective!.createBatch();

          await recipesAfterUpdate[0].delete(deleteBatchId);
          await notesAfterUpdate[0].delete(deleteBatchId);

          // Verify models still exist before commit
          const recipesBeforeDelete = await BatchRecipe.findAll(perspective!);
          expect(recipesBeforeDelete.length).to.equal(1);

          const notesBeforeDelete = await BatchNote.findAll(perspective!);
          expect(notesBeforeDelete.length).to.equal(1);

          // Commit delete batch
          const deleteResult = await perspective!.commitBatch(deleteBatchId);
          expect(deleteResult.removals.length).to.be.greaterThan(0);

          // Verify models are deleted
          const recipesAfterDelete = await BatchRecipe.findAll(perspective!);
          expect(recipesAfterDelete.length).to.equal(0);

          const notesAfterDelete = await BatchNote.findAll(perspective!);
          expect(notesAfterDelete.length).to.equal(0);
        });

        describe("Emoji and Special Character Handling", () => {
          @Model({
            name: "Message",
          })
          class EmojiMessage extends Ad4mModel {
            @Flag({
              through: "ad4m://entry_type",
              value: "flux://message",
            })
            type: string = "";

            @Property({
              through: "flux://body",
              writable: true,
              resolveLanguage: "literal",
            })
            body: string = "";
          }

          // before(async () => {
          //     // Add a small delay to ensure Prolog engine is stable
          //     await sleep(2000);

          //     // Register the EmojiMessage class using ensureSDNASubjectClass
          //     await perspective!.ensureSDNASubjectClass(EmojiMessage);

          //     // Clear any existing EmojiMessage instances to start fresh
          //     const existingMessages = await EmojiMessage.findAll(perspective!);
          //     for (const msg of existingMessages) {
          //         await msg.delete();
          //     }
          // });

          beforeEach(async () => {
            // Register the EmojiMessage class using ensureSDNASubjectClass
            await perspective!.ensureSDNASubjectClass(EmojiMessage);
            // Clean up any messages from previous tests
            const existingMessages = await EmojiMessage.findAll(perspective!);
            for (const msg of existingMessages) {
              await msg.delete();
            }
          });

          it("should correctly create and retrieve messages with emoji content", async () => {
            // Create a message with emoji content using Active Record
            const emojiMessage = new EmojiMessage(perspective!);
            emojiMessage.body = "<p>👋</p>";
            await emojiMessage.save();

            // Retrieve using findAll to test the full Prolog → Ad4mModel pipeline
            const messages = await EmojiMessage.findAll(perspective!);
            const retrievedMessage = messages.find(
              (m: EmojiMessage) => m.body === "<p>👋</p>",
            );

            expect(retrievedMessage).to.not.be.undefined;
            expect(retrievedMessage!.body).to.equal("<p>👋</p>");
          });

          it("should handle complex emoji sequences in Active Record properties", async () => {
            // Test with complex emoji sequences
            const complexMessage = new EmojiMessage(perspective!);
            complexMessage.body = "<p>🏳️‍🌈 Complex emoji with modifiers 👨‍👩‍👧‍👦</p>";
            await complexMessage.save();

            // Test retrieval with findAll
            const messages = await EmojiMessage.findAll(perspective!);
            const foundMessage = messages.find(
              (m: EmojiMessage) =>
                m.body === "<p>🏳️‍🌈 Complex emoji with modifiers 👨‍👩‍👧‍👦</p>",
            );

            expect(foundMessage).to.not.be.undefined;
            expect(foundMessage!.body).to.equal(
              "<p>🏳️‍🌈 Complex emoji with modifiers 👨‍👩‍👧‍👦</p>",
            );
          });

          it("should correctly handle special characters and Unicode", async () => {
            // Test with various special characters that could break URL encoding
            const specialMessage = new EmojiMessage(perspective!);
            specialMessage.body =
              "<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>";
            await specialMessage.save();

            // Verify retrieval through findAll
            const messages = await EmojiMessage.findAll(perspective!);
            const special = messages.find(
              (m: EmojiMessage) =>
                m.body === "<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>",
            );

            expect(special).to.not.be.undefined;
            expect(special!.body).to.equal(
              "<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>",
            );
          });

          it("should handle mixed content with emojis and HTML entities", async () => {
            // Test HTML entities mixed with emojis
            const mixedMessage = new EmojiMessage(perspective!);
            mixedMessage.body =
              "<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>";
            await mixedMessage.save();

            // Test direct property access after save/reload cycle
            const allMessages = await EmojiMessage.findAll(perspective!);
            const mixedMsg = allMessages.find(
              (m: EmojiMessage) =>
                m.body ===
                "<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>",
            );

            expect(mixedMsg).to.not.be.undefined;
            expect(mixedMsg!.body).to.equal(
              "<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>",
            );
          });

          // it("should preserve UTF-8 byte sequences through Prolog query system", async () => {
          //     // Test edge case UTF-8 sequences that previously caused issues
          //     const utf8Message = new EmojiMessage(perspective!);
          //     utf8Message.body = "UTF-8 test: 🌍🌎🌏 💫⭐✨ 🔥💯🚀 with metadata: {\"tags\": [\"🏷️\", \"📝\"], \"priority\": \"🔴\"}";
          //     await utf8Message.save();

          //     // Query using findAll to test the exact pipeline that was broken
          //     const messages = await EmojiMessage.findAll(perspective!);
          //     const testMsg = messages.find((m: EmojiMessage) => m.body === "UTF-8 test: 🌍🌎🌏 💫⭐✨ 🔥💯🚀 with metadata: {\"tags\": [\"🏷️\", \"📝\"], \"priority\": \"🔴\"}");

          //     expect(testMsg).to.not.be.undefined;
          //     // These assertions test the exact issue that was fixed:
          //     // Previously these would return undefined due to Prolog URL decoding issues
          //     expect(testMsg!.body).to.not.be.undefined;
          //     expect(testMsg!.body).to.equal("UTF-8 test: 🌍🌎🌏 💫⭐✨ 🔥💯🚀 with metadata: {\"tags\": [\"🏷️\", \"📝\"], \"priority\": \"🔴\"}");
          // });

          it("should handle subscription-based queries with emoji content", async () => {
            // Clear any previous messages
            let existingMessages = await EmojiMessage.findAll(perspective!);
            for (const msg of existingMessages) await msg.delete();

            // Set up subscription for emoji content
            let updateCount = 0;
            let subscriptionResults: EmojiMessage[] = [];
            const builder = EmojiMessage.query(perspective!);
            const emojiSub = builder.live((messages: EmojiMessage[]) => {
              subscriptionResults = messages;
              updateCount++;
            });
            const initialResults = await builder.get();

            // Initially no results
            expect(initialResults.length).to.equal(0);
            expect(updateCount).to.equal(0);

            // Create a message after setting up subscription - should trigger callback
            const subscriptionMessage = new EmojiMessage(perspective!);
            subscriptionMessage.body = "Subscription test with emoji: 🎯✅";
            await subscriptionMessage.save();

            // Wait for subscription to process with proper condition checking
            await waitForCondition(() => updateCount === 1, {
              timeoutMs: 5000,
              errorMessage:
                "Subscription did not fire after first message save",
            });

            // Verify subscription callback was called
            expect(updateCount).to.equal(1);
            expect(subscriptionResults.length).to.equal(1);
            expect(subscriptionResults[0].body).to.equal(
              "Subscription test with emoji: 🎯✅",
            );

            // Add another message with emojis - should trigger subscription again
            const secondMessage = new EmojiMessage(perspective!);
            secondMessage.body = "Another emoji message: 🚀💯";
            await secondMessage.save();

            // Wait for subscription to process with proper condition checking
            await waitForCondition(() => updateCount === 2, {
              timeoutMs: 5000,
              errorMessage:
                "Subscription did not fire after second message save",
            });

            // Verify subscription was called again
            expect(updateCount).to.equal(2);
            expect(subscriptionResults.length).to.equal(2);
            const foundSecond = subscriptionResults.find(
              (m) => m.body === "Another emoji message: 🚀💯",
            );
            expect(foundSecond).to.not.be.undefined;

            // Also verify the message exists through direct query
            const messages = await EmojiMessage.findAll(perspective!);
            const found = messages.find(
              (m: EmojiMessage) =>
                m.body === "Subscription test with emoji: 🎯✅",
            );
            expect(found).to.not.be.undefined;
            expect(found!.body).to.equal("Subscription test with emoji: 🎯✅");

            // Dispose the subscription to prevent cross-test interference
            emojiSub.unsubscribe();
          });
        });
      });

      describe("getter feature tests", () => {
        @Model({ name: "BlogPost" })
        class BlogPost extends Ad4mModel {
          @Property({
            through: "blog://title",
            resolveLanguage: "literal",
          })
          title: string = "";

          @Property({
            through: "blog://parent",
            getter:
              "(->link[WHERE perspective = $perspective AND predicate = 'blog://reply_to'].out.uri)[0]",
          })
          parentPost: string | undefined;

          @HasMany({
            through: "blog://tags",
            getter:
              "(->link[WHERE perspective = $perspective AND predicate = 'blog://tagged_with'].out.uri)",
          })
          tags: string[] = [];
        }

        beforeEach(async () => {
          if (perspective) {
            await ad4m!.perspective.remove(perspective.uuid);
          }
          perspective = await ad4m!.perspective.add("getter-test");
          await perspective!.ensureSDNASubjectClass(BlogPost);
        });

        it("should evaluate getter for property", async () => {
          const postRoot = Literal.from(
            "Blog post for getter property test",
          ).toUrl();
          const parentRoot = Literal.from("Parent blog post").toUrl();

          const post = new BlogPost(perspective!, postRoot);
          post.title = "Reply Post";
          await post.save();

          const parent = new BlogPost(perspective!, parentRoot);
          parent.title = "Original Post";
          await parent.save();

          // Create the link that getter should find
          await perspective!.add(
            new Link({
              source: postRoot,
              predicate: "blog://reply_to",
              target: parentRoot,
            }),
          );

          // Get the post and check if getter resolved the parent
          const retrievedPost = new BlogPost(perspective!, postRoot);
          await retrievedPost.get();

          expect(retrievedPost.parentPost).to.equal(parentRoot);
        });

        it("should evaluate getter for collection", async () => {
          const postRoot = Literal.from(
            "Blog post for getter collection test",
          ).toUrl();
          const tag1 = Literal.from("tag:javascript").toUrl();
          const tag2 = Literal.from("tag:typescript").toUrl();

          const post = new BlogPost(perspective!, postRoot);
          post.title = "Test Post";
          await post.save();

          // Create links that getter should find
          await perspective!.add(
            new Link({
              source: postRoot,
              predicate: "blog://tagged_with",
              target: tag1,
            }),
          );
          await perspective!.add(
            new Link({
              source: postRoot,
              predicate: "blog://tagged_with",
              target: tag2,
            }),
          );

          // Get the post and check if getter resolved the tags
          const retrievedPost = new BlogPost(perspective!, postRoot);
          await retrievedPost.get();

          expect(retrievedPost.tags).to.include(tag1);
          expect(retrievedPost.tags).to.include(tag2);
          expect(retrievedPost.tags.length).to.equal(2);
        });

        it("should filter out 'None' and empty values from getter results", async () => {
          const postRoot = Literal.from(
            "Blog post for None filtering test",
          ).toUrl();

          const post = new BlogPost(perspective!, postRoot);
          post.title = "Post without parent";
          await post.save();

          // Don't create any reply_to link, so getter should return None/empty

          const retrievedPost = new BlogPost(perspective!, postRoot);
          await retrievedPost.get();

          // Property should be undefined, not 'None' or empty string
          expect(retrievedPost.parentPost).to.be.undefined;
        });
      });

      describe("isInstance filtering tests", () => {
        @Model({ name: "Comment" })
        class Comment extends Ad4mModel {
          @Flag({
            through: "ad4m://type",
            value: "ad4m://comment",
          })
          type!: string;

          @Property({
            through: "comment://text",
            resolveLanguage: "literal",
          })
          text: string = "";
        }

        @Model({ name: "Article" })
        class Article extends Ad4mModel {
          @Property({
            through: "article://title",
            resolveLanguage: "literal",
          })
          title: string = "";

          @HasMany({
            through: "article://has_comment",
            where: { isInstance: Comment },
          })
          comments: string[] = [];
        }

        @Model({ name: "ArticleWithString" })
        class ArticleWithString extends Ad4mModel {
          @Property({
            through: "article://title",
            resolveLanguage: "literal",
          })
          title: string = "";

          @HasMany({
            through: "article://has_comment",
            where: { isInstance: "Comment" },
          })
          comments: string[] = [];
        }

        beforeEach(async () => {
          if (perspective) {
            await ad4m!.perspective.remove(perspective.uuid);
          }
          perspective = await ad4m!.perspective.add("isInstance-test");

          // Register both Comment and Article classes using ensureSDNASubjectClass
          await perspective!.ensureSDNASubjectClass(Comment);
          await perspective!.ensureSDNASubjectClass(Article);
          await perspective!.ensureSDNASubjectClass(ArticleWithString);

          // Give perspective time to fully index the SDNA classes
          await sleep(200);
        });

        it("should filter collection by isInstance with class reference", async () => {
          const articleRoot = Literal.from(
            "Article for isInstance test",
          ).toUrl();
          const validComment1 = Literal.from("Valid comment 1").toUrl();
          const validComment2 = Literal.from("Valid comment 2").toUrl();
          const invalidItem = Literal.from("Invalid item").toUrl();

          const article = new Article(perspective!, articleRoot);
          article.title = "Test Article";
          await article.save();

          // Create valid comments
          const comment1 = new Comment(perspective!, validComment1);
          comment1.text = "This is a valid comment";
          await comment1.save();

          const comment2 = new Comment(perspective!, validComment2);
          comment2.text = "This is another valid comment";
          await comment2.save();

          // Add delay to allow SurrealDB to finish indexing
          await sleep(1500);

          // Add links to article
          await perspective!.add(
            new Link({
              source: articleRoot,
              predicate: "article://has_comment",
              target: validComment1,
            }),
          );
          await perspective!.add(
            new Link({
              source: articleRoot,
              predicate: "article://has_comment",
              target: invalidItem,
            }),
          );
          await perspective!.add(
            new Link({
              source: articleRoot,
              predicate: "article://has_comment",
              target: validComment2,
            }),
          );

          const retrievedArticle = new Article(perspective!, articleRoot);
          await retrievedArticle.get();

          // Should only contain valid Comments, not the invalid item
          expect(retrievedArticle.comments).to.have.lengthOf(2);
          expect(retrievedArticle.comments).to.include(validComment1);
          expect(retrievedArticle.comments).to.include(validComment2);
          expect(retrievedArticle.comments).to.not.include(invalidItem);
        });

        it("should filter collection by isInstance with string class name", async () => {
          const articleRoot = Literal.from(
            "Article for string isInstance test",
          ).toUrl();
          const validComment = Literal.from("Valid comment").toUrl();
          const invalidItem = Literal.from("Invalid item").toUrl();

          const article = new ArticleWithString(perspective!, articleRoot);
          article.title = "Test Article with String";
          await article.save();

          // Create one valid comment
          const comment = new Comment(perspective!, validComment);
          comment.text = "Valid comment text";
          await comment.save();

          // Add both to article
          await perspective!.add(
            new Link({
              source: articleRoot,
              predicate: "article://has_comment",
              target: validComment,
            }),
          );
          await perspective!.add(
            new Link({
              source: articleRoot,
              predicate: "article://has_comment",
              target: invalidItem,
            }),
          );

          const retrievedArticle = new ArticleWithString(
            perspective!,
            articleRoot,
          );
          await retrievedArticle.get();

          expect(retrievedArticle.comments).to.have.lengthOf(1);
          expect(retrievedArticle.comments[0]).to.equal(validComment);
        });

        it("should filter results in findAll() by isInstance", async () => {
          // Create two articles
          const article1Root = Literal.from(
            "Article 1 for findAll isInstance",
          ).toUrl();
          const article2Root = Literal.from(
            "Article 2 for findAll isInstance",
          ).toUrl();

          const comment1 = Literal.from("Comment 1").toUrl();
          const invalid1 = Literal.from("Invalid 1").toUrl();
          const comment2 = Literal.from("Comment 2").toUrl();
          const invalid2 = Literal.from("Invalid 2").toUrl();

          // Create articles
          const article1 = new Article(perspective!, article1Root);
          article1.title = "Article 1";
          await article1.save();

          const article2 = new Article(perspective!, article2Root);
          article2.title = "Article 2";
          await article2.save();

          // Create valid comments
          const c1 = new Comment(perspective!, comment1);
          c1.text = "Comment 1 text";
          await c1.save();

          const c2 = new Comment(perspective!, comment2);
          c2.text = "Comment 2 text";
          await c2.save();

          // Add comments to articles (mix of valid and invalid)
          await perspective!.add(
            new Link({
              source: article1Root,
              predicate: "article://has_comment",
              target: comment1,
            }),
          );
          await perspective!.add(
            new Link({
              source: article1Root,
              predicate: "article://has_comment",
              target: invalid1,
            }),
          );
          await perspective!.add(
            new Link({
              source: article2Root,
              predicate: "article://has_comment",
              target: comment2,
            }),
          );
          await perspective!.add(
            new Link({
              source: article2Root,
              predicate: "article://has_comment",
              target: invalid2,
            }),
          );

          // Use findAll and verify filtering
          const articles = await Article.findAll(perspective!);

          expect(articles).to.have.lengthOf(2);

          const foundArticle1 = articles.find((a) => a.title === "Article 1");
          const foundArticle2 = articles.find((a) => a.title === "Article 2");

          expect(foundArticle1).to.not.be.undefined;
          expect(foundArticle2).to.not.be.undefined;

          // Each article should only have valid comments
          expect(foundArticle1!.comments).to.have.lengthOf(1);
          expect(foundArticle1!.comments[0]).to.equal(comment1);

          expect(foundArticle2!.comments).to.have.lengthOf(1);
          expect(foundArticle2!.comments[0]).to.equal(comment2);
        });
      });
    });
  });

  describe("Smart Literal", () => {
    let perspective: PerspectiveProxy | null = null;

    before(async () => {
      perspective = await ad4m!.perspective.add("smart literal test");
      // for test debugging:
      //console.log("UUID: " + perspective.uuid)
    });

    it("can create and use a new smart literal", async () => {
      let sl = await SmartLiteral.create(perspective!, "Hello World");
      let base = sl.base;

      expect(await sl.get()).to.equal("Hello World");

      let links = await perspective!.get(
        new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE }),
      );
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      let literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal("Hello World");

      await sl.set(5);
      expect(await sl.get()).to.equal(5);

      links = await perspective!.get(
        new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE }),
      );
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal(5);
    });

    it("can instantiate smart literal from perspective", async () => {
      let source = Literal.from("base").toUrl();
      let target = Literal.from("Hello World 2").toUrl();
      await perspective!.add({
        source,
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
        target,
      });

      let sl = new SmartLiteral(perspective!, source);
      expect(await sl.get()).to.equal("Hello World 2");
    });

    it("can get all smart literals in a perspective", async () => {
      let all = await SmartLiteral.getAllSmartLiterals(perspective!);
      expect(all.length).to.equal(2);
      expect(all[1].base).to.equal(Literal.from("base").toUrl());
      expect(await all[0].get()).to.equal(5);
      expect(await all[1].get()).to.equal("Hello World 2");
    });
  });

  // SKIPPED: Embedding cache tests - only applies to Prolog-pooled mode
  // These tests verify embedding URL post-processing with Prolog infer() queries.
  // With SHACL migration, embedding queries should use SurrealDB vector search instead.
  // Keeping as reference for future SurrealDB vector embedding implementation.
  describe.skip("Embedding cache", () => {
    let perspective: PerspectiveProxy | null = null;
    const EMBEDDING_LANG =
      "QmzSYwdbqjGGbYbWJvdKA4WnuFwmMx3AsTfgg7EwbeNUGyE555c";

    before(async () => {
      perspective = await ad4m!.perspective.add("embedding-cache-test");
    });

    it("correctly post-processes nested query results containing embedding URLs", async () => {
      // Create some links with embedding URLs
      const embeddingUrl1 = `${EMBEDDING_LANG}://vector1/1.2,3.4,5.6`;
      const embeddingUrl2 = `${EMBEDDING_LANG}://vector2/7.8,9.0,1.2`;
      const embeddingUrl3 = `${EMBEDDING_LANG}://vector3/2.3,4.5,6.7`;

      // Create a link structure that will produce nested results
      await perspective!.add({
        source: "test://root",
        predicate: "test://has-vector",
        target: embeddingUrl1,
      });

      await perspective!.add({
        source: embeddingUrl1,
        predicate: "test://related-to",
        target: embeddingUrl2,
      });

      await perspective!.add({
        source: embeddingUrl2,
        predicate: "test://points-to",
        target: embeddingUrl3,
      });

      // Query that will produce nested results with embedding URLs at different levels
      const result = await perspective!.infer(`
                % Find all vectors connected to root
                findall(
                    [FirstVector, RelatedVectors],
                    (
                        % Get first vector from root
                        triple("test://root", "test://has-vector", FirstVector),
                        % Find all vectors related to the first one
                        findall(
                            [SecondVector, ThirdVector],
                            (
                                triple(FirstVector, "test://related-to", SecondVector),
                                triple(SecondVector, "test://points-to", ThirdVector)
                            ),
                            RelatedVectors
                        )
                    ),
                    Results
                ).
            `);

      // The query should return a deeply nested structure:
      // Results = [
      //   [embeddingUrl1, [
      //     [embeddingUrl2, embeddingUrl3]
      //   ]]
      // ]
      console.log("result", result);
      expect(result).to.be.an("array");
      expect(result.length).to.be.greaterThan(0);

      let binding = result[0];
      expect(binding.Results).to.be.an("array");
      expect(binding.Results).to.have.lengthOf(1);

      const [firstLevel] = binding.Results;
      expect(firstLevel).to.be.an("array");
      expect(firstLevel[0]).to.equal(embeddingUrl1);
      expect(firstLevel[1]).to.be.an("array");

      const relatedVectors = firstLevel[1];
      expect(relatedVectors).to.have.lengthOf(1);
      expect(relatedVectors[0]).to.be.an("array");
      expect(relatedVectors[0][0]).to.equal(embeddingUrl2);
      expect(relatedVectors[0][1]).to.equal(embeddingUrl3);
    });
  });

  describe("Ad4mModel.fromJSONSchema", () => {
    let perspective: PerspectiveProxy | null = null;

    beforeEach(async () => {
      perspective = await ad4m!.perspective.add("json-schema-test");
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
          resolveLanguage: "literal",
        });

        expect(PersonClass).to.be.a("function");
        // @ts-ignore - className is added dynamically
        expect(PersonClass.className).to.equal("Person");

        // Test instance creation
        const person = new PersonClass(perspective!);
        expect(person).to.be.instanceOf(Ad4mModel);
        expect(person.id).to.be.a("string");

        // Test property assignment
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.name = "Alice Johnson";
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.age = 30;
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.email = "alice.johnson@example.com";

        await perspective!.ensureSDNASubjectClass(PersonClass);
        await person.save();

        // Create a second person to test multiple instances
        const person2 = new PersonClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        person2.name = "Bob Smith";
        // @ts-ignore - properties are added dynamically from JSON Schema
        person2.age = 25;
        // @ts-ignore - properties are added dynamically from JSON Schema
        person2.email = "bob.smith@example.com";
        await person2.save();

        // Verify data was saved and can be retrieved
        const savedPeople = await PersonClass.findAll(perspective!);
        expect(savedPeople).to.have.lengthOf(2);

        // Find Alice
        // @ts-ignore - properties are added dynamically from JSON Schema
        const alice = savedPeople.find((p) => p.name === "Alice Johnson");
        expect(alice).to.exist;
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice!.name).to.equal("Alice Johnson");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice!.age).to.equal(30);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice!.email).to.equal("alice.johnson@example.com");

        // Find Bob
        // @ts-ignore - properties are added dynamically from JSON Schema
        const bob = savedPeople.find((p) => p.name === "Bob Smith");
        expect(bob).to.exist;
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(bob!.age).to.equal(25);

        // Test querying with where clauses
        const adults = await PersonClass.findAll(perspective!, {
          where: { age: { gt: 28 } },
        });
        expect(adults).to.have.lengthOf(1);
        // @ts-ignore - properties are added dynamically from JSON Schema
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
          resolveLanguage: "literal",
        });

        // @ts-ignore - className is added dynamically
        expect(ContactClass.className).to.equal("Contact");

        // Test that custom predicates are used
        const contact = new ContactClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        contact.name = "Bob Wilson";
        // @ts-ignore - properties are added dynamically from JSON Schema
        contact.email = "bob.wilson@company.com";

        await perspective!.ensureSDNASubjectClass(ContactClass);
        await contact.save();

        // Create second contact to test multiple instances
        const contact2 = new ContactClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        contact2.name = "Carol Davis";
        // @ts-ignore - properties are added dynamically from JSON Schema
        contact2.email = "carol.davis@company.com";
        await contact2.save();

        // Verify data retrieval works with custom predicates
        const savedContacts = await ContactClass.findAll(perspective!);
        expect(savedContacts).to.have.lengthOf(2);

        // @ts-ignore - properties are added dynamically from JSON Schema
        const bob = savedContacts.find((c) => c.name === "Bob Wilson");
        expect(bob).to.exist;
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(bob!.email).to.equal("bob.wilson@company.com");

        // Verify the custom predicates were used by checking the generated SDNA
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = ContactClass.generateSDNA();
        expect(sdna.sdna).to.include("foaf://name");
        expect(sdna.sdna).to.include("foaf://mbox");

        // Test querying works with custom predicates
        const bobQuery = await ContactClass.findAll(perspective!, {
          where: { name: "Bob Wilson" },
        });
        expect(bobQuery).to.have.lengthOf(1);
        // @ts-ignore - properties are added dynamically from JSON Schema
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
                resolveLanguage: "literal",
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
              "x-ad4m": {
                resolveLanguage: "literal",
              },
            },
          },
          required: ["name"],
        };

        const ProductClass = Ad4mModel.fromJSONSchema(schema, {
          name: "ProductOverride", // This should take precedence
        });

        // @ts-ignore - className is added dynamically
        expect(ProductClass.className).to.equal("ProductOverride");

        const product = new ProductClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        product.name = "Gaming Laptop";
        // @ts-ignore - properties are added dynamically from JSON Schema
        product.price = 1299.99;
        // @ts-ignore - properties are added dynamically from JSON Schema
        product.description =
          "A high-performance gaming laptop with RTX graphics";

        await perspective!.ensureSDNASubjectClass(ProductClass);
        await product.save();

        // Create a second product with different pricing
        const product2 = new ProductClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        product2.name = "Office Laptop";
        // @ts-ignore - properties are added dynamically from JSON Schema
        product2.price = 799.99;
        // @ts-ignore - properties are added dynamically from JSON Schema
        product2.description = "A reliable laptop for office work";
        await product2.save();

        // Test data retrieval and validation
        const savedProducts = await ProductClass.findAll(perspective!);
        expect(savedProducts).to.have.lengthOf(2);

        // Verify x-ad4m custom predicates work for data retrieval
        // @ts-ignore - properties are added dynamically from JSON Schema
        const gamingLaptop = savedProducts.find(
          (p) => p.name === "Gaming Laptop",
        );
        expect(gamingLaptop).to.exist;
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(gamingLaptop!.price).to.equal(1299.99);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(gamingLaptop!.description).to.equal(
          "A high-performance gaming laptop with RTX graphics",
        );

        // Test querying with price ranges
        const expensiveProducts = await ProductClass.findAll(perspective!, {
          where: { price: { gt: 1000 } },
        });
        expect(expensiveProducts).to.have.lengthOf(1);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(expensiveProducts[0].name).to.equal("Gaming Laptop");

        // Verify custom predicates from x-ad4m were used
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = ProductClass.generateSDNA();
        expect(sdna.sdna).to.include("product://title"); // custom predicate for name
        expect(sdna.sdna).to.include("product://cost"); // custom predicate for price
        expect(sdna.sdna).to.include("product://description"); // inferred from namespace + property
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
          resolveLanguage: "literal",
        });

        // @ts-ignore - className is added dynamically
        expect(BookClass.className).to.equal("Book");

        const book = new BookClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        book.title = "The Great Gatsby";
        // @ts-ignore - properties are added dynamically from JSON Schema
        // @ts-ignore - properties are added dynamically from JSON Schema
        book.writer = "F. Scott Fitzgerald";
        // @ts-ignore - properties are added dynamically from JSON Schema
        book.isbn = "978-0-7432-7356-5";

        await perspective!.ensureSDNASubjectClass(BookClass);
        await book.save();

        // Add a second book
        const book2 = new BookClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        book2.title = "To Kill a Mockingbird";
        // @ts-ignore - properties are added dynamically from JSON Schema
        // @ts-ignore - properties are added dynamically from JSON Schema
        book2.writer = "Harper Lee";
        // @ts-ignore - properties are added dynamically from JSON Schema
        book2.isbn = "978-0-06-112008-4";
        await book2.save();

        // Test data retrieval with inferred predicates
        const savedBooks = await BookClass.findAll(perspective!);
        expect(savedBooks).to.have.lengthOf(2);

        // @ts-ignore - properties are added dynamically from JSON Schema
        // @ts-ignore - properties are added dynamically from JSON Schema
        const gatsby = savedBooks.find((b) => b.title === "The Great Gatsby");
        expect(gatsby).to.exist;
        // @ts-ignore - properties are added dynamically from JSON Schema
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(gatsby!.writer).to.equal("F. Scott Fitzgerald");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(gatsby!.isbn).to.equal("978-0-7432-7356-5");

        // Test querying by author
        const fitzgeraldBooks = await BookClass.findAll(perspective!, {
          where: { writer: "F. Scott Fitzgerald" },
        });
        expect(fitzgeraldBooks).to.have.lengthOf(1);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(fitzgeraldBooks[0].title).to.equal("The Great Gatsby");

        // Verify inferred predicates (should be book://title, book://author, etc.)
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = BookClass.generateSDNA();
        expect(sdna.sdna).to.include("book://title");
        expect(sdna.sdna).to.include("book://writer");
        expect(sdna.sdna).to.include("book://isbn");
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

      it("should automatically add type flag when no required properties are provided", async () => {
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

        // Should not throw error - instead adds automatic type flag
        const OptionalClass = Ad4mModel.fromJSONSchema(schema, {
          name: "OptionalOnly",
          namespace: "test://",
        });

        expect(OptionalClass).to.be.a("function");
        // @ts-ignore - className is added dynamically
        expect(OptionalClass.className).to.equal("OptionalOnly");

        // Should have automatic type flag
        const instance = new OptionalClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(instance.__ad4m_type).to.equal("test://instance");

        // Verify SDNA includes the automatic type flag
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = OptionalClass.generateSDNA();
        expect(sdna.sdna).to.include("ad4m://type");
        expect(sdna.sdna).to.include("test://instance");
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
        });

        expect(TestClass).to.be.a("function");
        // @ts-ignore - className is added dynamically
        expect(TestClass.className).to.equal("WithInitials");

        // Verify SDNA has constructor actions
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = TestClass.generateSDNA();
        expect(sdna.sdna).to.include("constructor(");
        expect(sdna.sdna).to.include("test://active");
        expect(sdna.sdna).to.include("literal://number:0");
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
          resolveLanguage: "literal",
        });

        // @ts-ignore - className is added dynamically
        expect(BlogPostClass.className).to.equal("BlogPost");

        await perspective!.ensureSDNASubjectClass(BlogPostClass);

        // Create a blog post with complex data
        const post1 = new BlogPostClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        post1.title = "Getting Started with AD4M";

        // Test array/collection handling
        // @ts-ignore - properties are added dynamically from JSON Schema
        post1.tags = ["tag://ad4m", "tag://tutorial", "tag://blockchain"];
        // @ts-ignore - properties are added dynamically from JSON Schema
        post1.categories = ["category://technology", "category://development"];

        // Test complex object handling (should be stored as JSON)
        // @ts-ignore - properties are added dynamically from JSON Schema
        post1.metadata = {
          created: "2025-09-22T10:00:00Z",
          author: "Alice",
          views: 42,
        };

        await post1.save();

        // Create a second post
        const post2 = new BlogPostClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        post2.title = "Advanced AD4M Patterns";
        // @ts-ignore - properties are added dynamically from JSON Schema
        post2.tags = ["tag://ad4m", "tag://advanced", "tag://patterns"];
        // @ts-ignore - properties are added dynamically from JSON Schema
        post2.categories = ["category://technology"];
        // @ts-ignore - properties are added dynamically from JSON Schema
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
        // @ts-ignore - properties are added dynamically from JSON Schema
        const tutorialPost = savedPosts.find(
          (p) => p.title === "Getting Started with AD4M",
        );
        expect(tutorialPost).to.exist;

        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.tags).to.be.an("array");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.tags).to.include.members([
          "tag://ad4m",
          "tag://tutorial",
          "tag://blockchain",
        ]);

        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.metadata).to.be.an("object");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.metadata.author).to.equal("Alice");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.metadata.views).to.equal(42);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(tutorialPost!.metadata.created).to.equal("2025-09-22T10:00:00Z");

        // Test querying by title
        const advancedPosts = await BlogPostClass.findAll(perspective!, {
          where: { title: "Advanced AD4M Patterns" },
        });
        expect(advancedPosts).to.have.lengthOf(1);
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(advancedPosts[0].metadata.author).to.equal("Bob");

        // Verify SDNA structure for complex types
        // @ts-ignore - generateSDNA is added dynamically
        const sdna = BlogPostClass.generateSDNA();
        expect(sdna.sdna).to.include("collection("); // tags and categories should be collections
        expect(sdna.sdna).to.include("property("); // title and metadata should be properties
        expect(sdna.sdna).to.include("blogpost://title");
        expect(sdna.sdna).to.include("blogpost://tags");
        expect(sdna.sdna).to.include("blogpost://metadata");
        expect(sdna.sdna).to.include("blogpost://categories");
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
          resolveLanguage: "literal",
        });

        await perspective!.ensureSDNASubjectClass(PersonHolonClass);

        // Test with realistic data
        const person = new PersonHolonClass(perspective!);
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.name = "Alice Cooper";
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.email = "alice@example.com";
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.skills = [
          "skill://javascript",
          "skill://typescript",
          "skill://ad4m",
        ];
        // @ts-ignore - properties are added dynamically from JSON Schema
        person.profile = {
          bio: "Software developer passionate about decentralized systems",
          location: "San Francisco",
        };
        await person.save();

        // Verify retrieval preserves nested structure
        const retrieved = await PersonHolonClass.findAll(perspective!);
        expect(retrieved).to.have.lengthOf(1);

        const alice = retrieved[0];
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice.profile).to.be.an("object");
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice.profile.bio).to.equal(
          "Software developer passionate about decentralized systems",
        );
        // @ts-ignore - properties are added dynamically from JSON Schema
        expect(alice.skills).to.include.members([
          "skill://javascript",
          "skill://typescript",
          "skill://ad4m",
        ]);
      });
    });
  });
});

/**
 * Wait for a condition to become true with exponential backoff.
 * This is more reliable than fixed sleep() for async operations.
 */
async function waitForCondition(
  condition: () => boolean,
  options: {
    timeoutMs?: number;
    checkIntervalMs?: number;
    errorMessage?: string;
  } = {},
): Promise<void> {
  const {
    timeoutMs = 5000,
    checkIntervalMs = 50,
    errorMessage = "Condition was not met within timeout",
  } = options;

  const startTime = Date.now();

  while (!condition()) {
    if (Date.now() - startTime > timeoutMs) {
      throw new Error(`${errorMessage} (timeout: ${timeoutMs}ms)`);
    }
    await sleep(checkIntervalMs);
  }
}
