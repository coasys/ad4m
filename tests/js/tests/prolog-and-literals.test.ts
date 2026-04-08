import { expect } from "chai";
import { ChildProcess } from 'node:child_process';
import { Ad4mClient, Link, LinkQuery, Literal, PerspectiveProxy,
    SmartLiteral, SMART_LITERAL_CONTENT_PREDICATE,
    Subject,
    Ad4mModel,
    Flag,
    Property,
    ReadOnly,
    HasMany,
    Model,
    Optional,
    PropertyOptions,
} from "@coasys/ad4m";
import { readFileSync } from "node:fs";
import { startExecutor, apolloClient, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import path from "path";
import { fileURLToPath } from 'url';
import fetch from 'node-fetch'
import sinon from 'sinon';

//@ts-ignore
global.fetch = fetch

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Prolog + Literals", () => {
    let ad4m: Ad4mClient | null = null
    let executorProcess: ChildProcess | null = null

    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "prolog-agent");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    let gqlPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;

    before(async () => {
        [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([gqlPort, hcAdminPort, hcAppPort]);
        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
            gqlPort, hcAdminPort, hcAppPort);

        console.log("Creating ad4m client")
        // @ts-ignore - Apollo Client version mismatch between dependencies
        ad4m = new Ad4mClient(apolloClient(gqlPort))
        console.log("Generating agent")
        await ad4m.agent.generate("secret")
        console.log("Done")
    })

    after(async () => {
        if (executorProcess) {
            await quitExecutor(executorProcess, gqlPort);
        }
        deregisterPorts([gqlPort, hcAdminPort, hcAppPort]);
    })

    it("should get agent status", async () => {
        let result = await ad4m!.agent.status()
        expect(result).to.not.be.null
        expect(result!.isInitialized).to.be.true
    })

    describe("Subjects (SHACL-based API)", () => {
        let perspective: PerspectiveProxy | null = null

        before(async () => {
            perspective = await ad4m!.perspective.add("test")
            // for test debugging:
            //console.log("UUID: " + perspective.uuid)
        })

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
                name: "Message"
            })
            class Message extends Ad4mModel {
                @Flag({
                    through: "ad4m://type",
                    value: "ad4m://message"
                })
                type: string = ""

                static async all(perspective: PerspectiveProxy): Promise<Message[]> {
                    return Message.query(perspective).get() as Promise<Message[]>
                }

                @Optional({
                    through: "todo://state",
                })
                body?: string
            }

            // This class matches the SDNA in ./sdna/subject.pl
            // and this test proves the decorators create the exact same SDNA code
            @Model({
                name: "Todo"
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
                    return Todo.query(perspective).get() as Promise<Todo[]>
                }

                static async allReady(perspective: PerspectiveProxy): Promise<Todo[]> {
                    return Todo.query(perspective).where({ state: "todo://ready" }).get() as Promise<Todo[]>
                }

                static async allDone(perspective: PerspectiveProxy): Promise<Todo[]> {
                    return Todo.query(perspective).where({ state: "todo://done" }).get() as Promise<Todo[]>
                }

                //@ts-ignore
                @Property({
                    through: "todo://state",
                    initial: "todo://ready"
                })
                state!: string

                @Optional({
                    through: "todo://has_title",
                    resolveLanguage: "literal"
                })
                title?: string

                @HasMany({ through: "todo://comment" })
                comments: string[] = []

                @HasMany({ through: "flux://entry_type" })
                entries: string[] = []

                @HasMany(() => Message, { through: "flux://entry_type" })
                messages: Message[] = []
            }

            before(async () => {
                // Register SHACL SDNA once for all tests in this block
                await perspective!.ensureSDNASubjectClass(Todo)
            })

            it("should find the TODO subject class from the test SDNA", async () => {
                let classes = await perspective!.subjectClasses();

                expect(classes.length).to.equal(1)
                expect(classes[0]).to.equal("Todo")
            })

            it.skip("should generate correct SDNA from a JS class", async () => {
                // @ts-ignore
                const { name, sdna } = Todo.generateSDNA();

                const regExp = /\("Todo", ([^)]+)\)/;
                const matches = regExp.exec(sdna);
                const value = matches![1];

                const equal = readFileSync("./sdna/subject.pl").toString().replace(/c\)/g, `${value})`).replace(/\(c/g, `(${value}`);

                expect(sdna.normalize('NFC')).to.equal(equal.normalize('NFC'))
            })

            it("should be possible to use that class for type-safe interaction with subject instances", async () => {
                // Create additional todos for the following tests
                // Todo 1: stays at initial "ready" state
                let root1 = Literal.from("Ready todo").toUrl()
                let todo1 = new Todo(perspective!, root1)
                await todo1.save()
                
                // Todo 2 & 3: set to "done" state  
                let root2 = Literal.from("Done todo 1").toUrl()
                let todo2 = new Todo(perspective!, root2)
                await todo2.save()
                todo2.state = "todo://done"
                await todo2.save()
                
                let root3 = Literal.from("Done todo 2").toUrl()
                let todo3 = new Todo(perspective!, root3)
                await todo3.save()
                todo3.state = "todo://done"
                await todo3.save()
                
                // construct new subject intance using Ad4mModel API
                let root = Literal.from("Decorated class construction test").toUrl()
                
                let todo = new Todo(perspective!, root)
                await todo.save()

                // Verify the instance was created with required links
                const stateLinks = await perspective!.get(new LinkQuery({source: root, predicate: "todo://state"}))
                expect(stateLinks.length).to.equal(1)
                expect(stateLinks[0].data.target).to.equal("todo://ready")

                // Check name mapping
                const nameMappingUrl = Literal.fromUrl(`literal:string:shacl://Todo`).toUrl()
                const nameMappingLinks = await perspective!.get(new LinkQuery({source: nameMappingUrl}))
                nameMappingLinks.forEach(link => console.log("  ", link.data.predicate, "->", link.data.target))

                const isInstance = await perspective!.isSubjectInstance(root, Todo)
                expect(isInstance).to.not.be.false
                
                // Ad4mModel API - use the todo instance directly (no need for getSubjectProxy)
                expect(todo).to.have.property("state")
                expect(todo).to.have.property("title")
                expect(todo).to.have.property("comments")
                
                todo.state = "todo://review"
                await todo.save()
                const stateAfter = await todo.state
                
                expect(stateAfter).to.equal("todo://review")
                expect(await todo.comments).to.be.empty

                let comment = Literal.from("new comment").toUrl()
                todo.comments = [comment]
                await todo.save()
                expect(await todo.comments).to.deep.equal([comment])
            })

            it("can retrieve all instances through instaceQuery decoratored all()", async () => {
                let todos = await Todo.all(perspective!)
                expect(todos.length).to.equal(4)
            })

            it("can retrieve all mathching instance through InstanceQuery(where: ..)", async () => {
                let todos = await Todo.allReady(perspective!)
                expect(todos.length).to.equal(1)
                expect(await todos[0].state).to.equal("todo://ready")

                todos = await Todo.allDone(perspective!)
                expect(todos.length).to.equal(2)
                expect(await todos[0].state).to.equal("todo://done")
            })

            // REMOVED: InstanceQuery(condition: ..) test - required Prolog-only allSelf method
            // The InstanceQuery with condition parameter required Prolog inference.
            // Future: Could be reimplemented with SHACL-based query conditions via SPARQL.

            it("can deal with properties that resolve the URI and create Expressions", async () => {
                let todos = await Todo.all(perspective!)
                
                // Guard: If no todos exist, create one for this test
                if (todos.length === 0) {
                    throw new Error("Test prerequisite failed: No todos available. Please ensure todos are created in the setup or earlier tests.")
                }
                
                // Find a todo without a title (to avoid data contamination from other tests)
                let todo = null;
                for (const t of todos) {
                    const title = await t.title
                    if (title === undefined || title === null || title === "") {
                        todo = t;
                        break;
                    }
                }

                if (!todo) {
                    // If all todos have titles, use the first one and clear its title
                    // Safe to access todos[0] since we've checked todos.length > 0 above
                    todo = todos[0]
                    // @ts-ignore
                    const existingLinks = await perspective!.get(new LinkQuery({source: todo.id, predicate: "todo://has_title"}))
                    for (const link of existingLinks) {
                        await perspective!.remove(link)
                    }
                }

                expect(await todo.title).to.be.undefined

                // Use direct assignment + update() pattern (setters are stubs)
                todo.title = "new title"
                await todo.save()
                expect(await todo.title).to.equal("new title")

                //@ts-ignore
                let links = await perspective!.get(new LinkQuery({source: todo.id, predicate: "todo://has_title"}))
                expect(links.length).to.equal(1)
                let literal = Literal.fromUrl(links[0].data.target).get()
                expect(literal.data).to.equal("new title")
            })

            it("can easily be initialized with PerspectiveProxy.ensureSDNASubjectClass()", async () => {
                expect(await perspective!.getSdna()).to.have.lengthOf(1)

                @Model({
                    name: "Test"
                })
                class Test {
                    @Property({
                        through: "test://test_numer"
                    })
                    number: number = 0
                }

                await perspective!.ensureSDNASubjectClass(Test)

                expect(await perspective!.getSdna()).to.have.lengthOf(2)
                //console.log((await perspective!.getSdna())[1])
            })

            // REMOVED: Custom getter prolog code test - required Prolog-based property getters
            // The isLiked property used custom Prolog code for computed values.
            // Future: Could be reimplemented with SHACL-based computed properties or SPARQL queries.

            describe("with Message subject class registered", () => {
                before(async () => {
                    await perspective!.ensureSDNASubjectClass(Message)
                })

                afterEach(async () => {
                    // Clean up any Message flags created during tests to prevent data contamination
                    const links = await perspective!.get(new LinkQuery({predicate: "ad4m://type", target: "ad4m://message"}))
                    for (const link of links) {
                        await perspective!.remove(link)
                    }
                })

                it("can find instances through the exact flag link", async() => {
                    await perspective!.add(new Link({
                        source: "test://message",
                        predicate: "ad4m://type",
                        target: "ad4m://undefined"
                    }))

                    const first = await Message.all(perspective!)
                    expect(first.length).to.be.equal(0)

                    await perspective!.add(new Link({
                        source: "test://message",
                        predicate: "ad4m://type",
                        target: "ad4m://message"
                    }))

                    const second = await Message.all(perspective!)
                    expect(second.length).to.be.equal(1)
                })

                it("can constrain collection entries through 'where' clause", async () => {
                    let root = Literal.from("Collection where test").toUrl()
                    let messageEntry = Literal.from("test message").toUrl()
                    
                    // Create todo with entries already set
                    let todo = new Todo(perspective!, root)
                    todo.entries = [messageEntry]
                    await todo.save()

                    let entries = await todo.entries
                    expect(entries.length).to.equal(1)

                    let messageEntries = await todo.messages
                    expect(messageEntries.length).to.equal(0)

                    let message = new Message(perspective!, messageEntry)
                    await message.save()

                    // Allow SPARQL to index the new type flag
                    await sleep(500)
                    
                    // Refresh todo data to apply collection filtering
                    await todo.get()
                    messageEntries = await todo.messages
                    expect(messageEntries.length).to.equal(1)
                })
            })

            describe("Active record implementation", () => {
                @Model({
                    name: "Recipe"
                })
                class Recipe extends Ad4mModel {
                    @Flag({
                        through: "ad4m://type",
                        value: "ad4m://recipe"
                    })
                    type: string = ""

                    @Optional({
                        through: "recipe://plain",
                    })
                    plain: string = ""

                    @Optional({
                        through: "recipe://name",
                        resolveLanguage: "literal"
                    })
                    name: string = ""

                    @Optional({
                        through: "recipe://boolean",
                        resolveLanguage: "literal"
                    })
                    booleanTest: boolean = false

                    @Optional({
                        through: "recipe://number",
                        resolveLanguage: "literal"
                    })
                    number: number = 0

                    @HasMany({ through: "recipe://entries" })
                    entries: string[] = []

                    @HasMany({ through: "recipe://comment" })
                    comments: string[] = []

                    @Optional({
                        through: "recipe://local",
                        local: true
                    })
                    local: string = ""

                    @Optional({
                        through: "recipe://resolve",
                        resolveLanguage: "literal"
                    })
                    resolve: string = ""

                    @Optional({
                        through: "recipe://image",
                        resolveLanguage: "", // Will be set dynamically to note-store language
                        transform: (data: any) => {
                            if (data && typeof data === 'object' && data.data_base64) {
                                return `data:image/png;base64,${data.data_base64}`;
                            }
                            return data;
                        }
                    } as PropertyOptions)
                    image: string | any = ""
                }

                beforeEach(async () => {
                    if(perspective) {
                        await ad4m!.perspective.remove(perspective.uuid)
                    }
                    perspective = await ad4m!.perspective.add("active-record-implementation-test")
                    await perspective!.ensureSDNASubjectClass(Recipe)
                })

                it("save() & get()", async () => {
                    let root = Literal.from("Active record implementation test").toUrl()

                    const recipe = new Recipe(perspective!, root)
                    recipe.name = "Save and get test";
                    recipe.plain = "recipe://test";
                    recipe.booleanTest = false;

                    await recipe.save();

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.name).to.equal("Save and get test")
                    expect(recipe2.plain).to.equal("recipe://test")
                    expect(recipe2.booleanTest).to.equal(false)
                })

                it("update()", async () => {
                    let root = Literal.from("Active record implementation test").toUrl()

                    const recipe = new Recipe(perspective!, root)
                    recipe.name = "Update test";
                    recipe.plain = "recipe://update_test";

                    await recipe.save();

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.name).to.equal("Update test")
                    expect(recipe2.plain).to.equal("recipe://update_test")
                })

                it("find()", async () => {
                    let recipe1 = new Recipe(perspective!, Literal.from("Active record implementation test find").toUrl());
                    recipe1.name = "Active record implementation test find";
                    await recipe1.save();

                    const recipes = await Recipe.findAll(perspective!);

                    expect(recipes.length).to.equal(1)
                })

                it("can constrain collection entries clause", async () => {
                    let root = Literal.from("Active record implementation collection test").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    recipe.name = "Collection test";

                    recipe.comments = ['recipe://test', 'recipe://test1']

                    await recipe.save()

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.comments.length).to.equal(2)
                })

                it("save() & get() local", async () => {
                    let root = Literal.from("Active record implementation test local link").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    recipe.name = "Local test";
                    recipe.local = 'recipe://test'

                    await recipe.save();

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.name).to.equal("Local test")
                    expect(recipe2.local).to.equal("recipe://test")

                    // @ts-ignore
                    const links = await perspective?.get({
                        source: root,
                        predicate: "recipe://local"
                    })

                    expect(links!.length).to.equal(1)
                    expect(links![0].status).to.equal('LOCAL')
                })

                it("delete()", async () => {
                    let recipe1 = new Recipe(perspective!, Literal.from("Active record implementation test delete1 ").toUrl());
                    recipe1.name = "Active record implementation test delete 1";
                    await recipe1.save();


                    let recipe2 = new Recipe(perspective!, Literal.from("Active record implementation test delete2 ").toUrl());
                    recipe2.name = "Active record implementation test delete 2";
                    await recipe2.save();


                    let recipe3 = new Recipe(perspective!, Literal.from("Active record implementation test delete3 ").toUrl());
                    recipe3.name = "Active record implementation test delete 3";
                    await recipe3.save();

                    const recipes = await Recipe.findAll(perspective!);

                    expect(recipes.length).to.equal(3)

                    await recipes[0].delete();

                    const updatedRecipies = await Recipe.findAll(perspective!);

                    expect(updatedRecipies.length).to.equal(2)
                })

                it("can constrain relation entries through SPARQL getter", async () => {
                    // Define a Recipe model with a getter-based filtered relation.
                    // Both `entries` and `ingredients` share the same predicate ("recipe://entries"),
                    // but `ingredients` uses an explicit getter to filter by an arbitrary link condition.
                    @Model({ name: "RecipeWithSparqlFilter" })
                    class RecipeWithSparqlFilter extends Ad4mModel {
                        @Flag({
                            through: "ad4m://type",
                            value: "recipe://instance"
                        })
                        type: string = ""

                        @Optional({
                            through: "recipe://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @HasMany({ through: "recipe://entries" })
                        entries: string[] = [];

                        @HasMany({
                            getter: `SELECT ?target WHERE { <Base> <recipe://entries> ?target . ?target <recipe://has_ingredient> <recipe://test> . }`
                        })
                        ingredients: string[] = [];
                    }

                    // Register the class
                    await perspective!.ensureSDNASubjectClass(RecipeWithSparqlFilter);
                    
                    // Wait for SHACL metadata to be indexed
                    await sleep(500);

                    let root = Literal.from("Active record SPARQL condition test").toUrl();
                    const recipe = new RecipeWithSparqlFilter(perspective!, root);

                    let entry1 = Literal.from("entry with ingredient").toUrl();
                    let entry2 = Literal.from("entry without ingredient").toUrl();

                    recipe.entries = [entry1, entry2];
                    recipe.name = "Condition test";

                    await recipe.save();

                    // Add the ingredient link to entry1 only
                    await perspective?.add(new Link({
                        source: entry1, 
                        predicate: "recipe://has_ingredient", 
                        target: "recipe://test"
                    }));

                    // Small delay for SPARQL indexing
                    await sleep(500);

                    const recipe2 = new RecipeWithSparqlFilter(perspective!, root);
                    await recipe2.get();

                    // Should have 2 entries total
                    expect(recipe2.entries.length).to.equal(2);
                    
                    // But only 1 ingredient (entry1 which has the ingredient link)
                    expect(recipe2.ingredients.length).to.equal(1);
                    expect(recipe2.ingredients[0]).to.equal(entry1);
                })

                it("can implement the resolveLanguage property type", async () => {
                    let root = Literal.from("Active record implementation test resolveLanguage").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    recipe.resolve = "Test name literal";

                    await recipe.save();

                    //@ts-ignore
                    let links = await perspective!.get(new LinkQuery({source: root, predicate: "recipe://resolve"}))
                    expect(links.length).to.equal(1)
                    let literal = Literal.fromUrl(links[0].data.target).get()
                    expect(literal.data).to.equal(recipe.resolve)

                    const recipe3 = new Recipe(perspective!, root);
                    await recipe3.get();
                    expect(recipe3.resolve).to.equal("Test name literal");
                })

                it("can resolve non-literal languages with resolveLanguage and transform", async () => {
                    // Publish note-store language to use as a non-literal resolveLanguage
                    const noteLanguage = await ad4m!.languages.publish(
                        path.join(__dirname, "../languages/note-store/build/bundle.js").replace(/\\/g, "/"),
                        { name: "note-store-test", description: "Test language for non-literal resolution" }
                    );
                    const noteLangAddress = noteLanguage.address;

                    // Create an expression in the note-store language with test data (simulating file data)
                    const testImageData = { data_base64: "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==" };
                    const imageExprUrl = await ad4m!.expression.create(testImageData, noteLangAddress);

                    let root = Literal.from("Active record implementation test resolveLanguage non-literal").toUrl();
                    const recipe = new Recipe(perspective!, root);

                    // Manually add the link instead of using save() to test the query resolution path
                    recipe.name = "Test with image";
                    await recipe.save(); // Save the name

                    // Add the image link manually
                    await perspective!.setSingleTarget(new Link({
                        source: root,
                        predicate: "recipe://image",
                        target: imageExprUrl
                    }));

                    // Verify the link was created with the expression URL
                    //@ts-ignore
                    let links = await perspective!.get(new LinkQuery({source: root, predicate: "recipe://image"}));
                    expect(links.length).to.equal(1);
                    expect(links[0].data.target).to.equal(imageExprUrl);

                    // Retrieve the recipe and verify the image was resolved and transformed
                    const results = await Recipe.findAll(perspective!, { where: { name: "Test with image" } });
                    const recipe2 = results[0];
                    
                    expect(recipe2.name).to.equal("Test with image");
                    // The image should be resolved from the note-store language and transformed to a data URL
                    expect(recipe2.image).to.equal(`data:image/png;base64,${testImageData.data_base64}`);
                })

                it("works with very long property values", async() => {
                    let root = Literal.from("Active record implementation test long value").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    const longName = "This is a very long recipe name that goes on and on with many many characters to test that we can handle long property values without any issues whatsoever and keep going even longer to make absolutely sure we hit at least 300 characters in this test string that just keeps getting longer and longer until we are completely satisfied that it works properly with such lengthy content. But wait, there's more! We need to make this string even longer to properly test the system's ability to handle extremely long property values. Let's add some more meaningful content about recipes - ingredients like flour, sugar, eggs, milk, butter, vanilla extract, baking powder, salt, and detailed instructions for mixing them together in just the right way to create the perfect baked goods. We could go on about preheating the oven to the right temperature, greasing the pans properly, checking for doneness with a toothpick, and letting things cool completely before frosting. The possibilities are endless when it comes to recipe details and instructions that could make this string longer and longer. We want to be absolutely certain that our system can handle property values of any reasonable length without truncating or corrupting the data in any way. This is especially important for recipes where precise instructions and ingredient amounts can make the difference between success and failure in the kitchen. Testing with realistically long content helps ensure our system works reliably in real-world usage scenarios where users might enter detailed information that extends well beyond a few simple sentences."
                    // Use resolve (resolveLanguage: "literal") to store the long string value.
                    // The plain property is for storing URI addresses; resolve encodes arbitrary strings.
                    recipe.resolve = longName

                    await recipe.save()

                    let linksResolve = await perspective!.get(new LinkQuery({source: root, predicate: "recipe://resolve"}))
                    expect(linksResolve.length).to.equal(1)
                    let expression = Literal.fromUrl(linksResolve[0].data.target).get()
                    expect(expression.data).to.equal(longName)

                    const recipe2 = new Recipe(perspective!, root)
                    await recipe2.get()

                    expect(recipe2.resolve.length).to.equal(longName.length)
                    expect(recipe2.resolve).to.equal(longName)
                })

                it("should have author and timestamp properties", async () => {
                    let root = Literal.from("Author and timestamp test").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    recipe.name = "author and timestamp test";
                    await recipe.save();

                    const recipe2 = new Recipe(perspective!, root);
                    await recipe2.get();

                    const me = await ad4m!.agent.me();
                    // @ts-ignore - author and timestamp are added by the system
                    expect(recipe2.author).to.equal(me!.did)
                    // @ts-ignore
                    expect(recipe2.timestamp).to.not.be.undefined;
                })

                it("get() returns all subject entity properties (via getData())", async () => {
                    let root = Literal.from("getData test").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    recipe.name = "getData all test";
                    recipe.booleanTest = true;
                    recipe.comments = ['recipe://comment1', 'recipe://comment2'];
                    recipe.local = "recipe://local_test";
                    recipe.resolve = "Resolved literal value";

                    await recipe.save();

                    const data = await recipe.get();

                    expect(data.name).to.equal("getData all test");
                    expect(data.booleanTest).to.equal(true);
                    // Collection order might not be preserved when items are added simultaneously
                    // Check that both items exist rather than exact order
                    expect(data.comments).to.have.lengthOf(2);
                    expect(data.comments).to.include('recipe://comment1');
                    expect(data.comments).to.include('recipe://comment2');
                    expect(data.local).to.equal("recipe://local_test");
                    expect(data.resolve).to.equal("Resolved literal value");

                    await recipe.delete();
                })

                it("findAll() returns properties on instances", async () => {
                    let root1 = Literal.from("findAll test 1").toUrl()
                    let root2 = Literal.from("findAll test 2").toUrl()
                    
                    const recipe1 = new Recipe(perspective!, root1)
                    recipe1.name = "findAll test 1";
                    recipe1.resolve = "Resolved literal value 1";
                    recipe1.plain = "recipe://findAll_test1";
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!, root2)
                    recipe2.name = "findAll test 2";
                    recipe2.resolve = "Resolved literal value 2";
                    recipe2.plain = "recipe://findAll_test2";
                    await recipe2.save();

                    // Test findAll (sort by name — SPARQL result order is non-deterministic)
                    const recipes = (await Recipe.findAll(perspective!)).sort((a, b) => a.name.localeCompare(b.name));

                    expect(recipes.length).to.equal(2);
                    expect(recipes[0].name).to.equal("findAll test 1");
                    expect(recipes[0].resolve).to.equal("Resolved literal value 1");
                    expect(recipes[1].name).to.equal("findAll test 2");
                    expect(recipes[1].resolve).to.equal("Resolved literal value 2");
                    expect(recipes[0].plain).to.equal("recipe://findAll_test1");
                    expect(recipes[1].plain).to.equal("recipe://findAll_test2");
                })

                it("findAll() returns collections on instances", async () => {
                    let root1 = Literal.from("findAll test 1").toUrl()
                    let root2 = Literal.from("findAll test 2").toUrl()

                    const recipe1 = new Recipe(perspective!, root1)
                    recipe1.comments = ["recipe://comment/r1/1", "recipe://comment/r1/2"];
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!, root2)
                    recipe2.comments = ["recipe://comment/r2/1", "recipe://comment/r2/2"];
                    await recipe2.save();

                    // Test findAll (sort by id — SPARQL result order is non-deterministic)
                    const recipes = (await Recipe.findAll(perspective!)).sort((a, b) => a.id.localeCompare(b.id));

                    expect(recipes.length).to.equal(2);
                    // Find which recipe is which by matching root expressions
                    const r1 = recipes.find(r => r.id === root1)!;
                    const r2 = recipes.find(r => r.id === root2)!;
                    expect(r1).to.not.be.undefined;
                    expect(r2).to.not.be.undefined;

                    expect(r1.comments.length).to.equal(2);
                    expect(r1.comments).to.include("recipe://comment/r1/1");
                    expect(r1.comments).to.include("recipe://comment/r1/2");

                    expect(r2.comments.length).to.equal(2);
                    expect(r2.comments).to.include("recipe://comment/r2/1");
                    expect(r2.comments).to.include("recipe://comment/r2/2");
                })

                it("findAll() returns author & timestamp on instances", async () => {
                    let root1 = Literal.from("findAll test 1").toUrl()
                    let root2 = Literal.from("findAll test 2").toUrl()
                    
                    const recipe1 = new Recipe(perspective!, root1);
                    recipe1.name = "findAll test 1";
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!, root2);
                    recipe2.name = "findAll test 2";
                    await recipe2.save();

                    const recipes = await Recipe.findAll(perspective!);
                    const me = await ad4m!.agent.me();
                    expect(recipes[0].author).to.equal(me!.did)
                    expect(recipes[0].timestamp).to.not.be.undefined;
                    expect(recipes[1].author).to.equal(me!.did)
                    expect(recipes[1].timestamp).to.not.be.undefined;
                })

                it("findAll() works with source prop", async () => {
                    const source1 = Literal.from("Source 1").toUrl()
                    const source2 = Literal.from("Source 2").toUrl()
                    const parentPredicate = "ad4m://has_child"
                    
                    const recipe1 = new Recipe(perspective!)
                    recipe1.name = "Recipe 1: Name";
                    await recipe1.save();
                    await perspective!.add(new Link({ source: source1, predicate: parentPredicate, target: recipe1.id }))

                    const recipe2 = new Recipe(perspective!)
                    recipe2.name = "Recipe 2: Name";
                    await recipe2.save();
                    await perspective!.add(new Link({ source: source2, predicate: parentPredicate, target: recipe2.id }))

                    const recipe3 = new Recipe(perspective!)
                    recipe3.name = "Recipe 3: Name";
                    await recipe3.save();
                    await perspective!.add(new Link({ source: source2, predicate: parentPredicate, target: recipe3.id }))

                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3);

                    const source1Recipes = await Recipe.findAll(perspective!, { parent: { id: source1, predicate: parentPredicate } });
                    expect(source1Recipes.length).to.equal(1);
                    expect(source1Recipes[0].name).to.equal("Recipe 1: Name");

                    const source2Recipes = await Recipe.findAll(perspective!, { parent: { id: source2, predicate: parentPredicate } });
                    expect(source2Recipes.length).to.equal(2);
                })

                it("findAll() works with properties query", async () => {
                    let root = Literal.from("findAll test 1").toUrl()
                    const recipe = new Recipe(perspective!, root);
                    recipe.name = "recipe://test_name";
                    recipe.booleanTest = true;
                    await recipe.save();

                    const me = await ad4m!.agent.me();

                    // Test recipes with all properties
                    const recipesWithAllAttributes = await Recipe.findAll(perspective!);
                    expect(recipesWithAllAttributes[0].name).to.equal("recipe://test_name")
                    expect(recipesWithAllAttributes[0].booleanTest).to.equal(true)
                    expect(recipesWithAllAttributes[0].author).to.equal(me!.did)
                    
                    // Test recipes with name only
                    const recipesWithNameOnly = await Recipe.findAll(perspective!, { properties: ["name"] });
                    expect(recipesWithNameOnly[0].name).to.equal("recipe://test_name")
                    expect(recipesWithNameOnly[0].booleanTest).to.be.undefined

                    // Test recipes with name and booleanTest only
                    const recipesWithTypeAndBooleanTestOnly = await Recipe.findAll(perspective!, { properties: ["name", "booleanTest"] });
                    expect(recipesWithTypeAndBooleanTestOnly[0].name).to.equal("recipe://test_name")
                    expect(recipesWithTypeAndBooleanTestOnly[0].booleanTest).to.equal(true)

                    // Test recipes with author only
                    const recipesWithAuthorOnly = await Recipe.findAll(perspective!, { properties: ["author"] });
                    expect(recipesWithAuthorOnly[0].name).to.be.undefined
                    expect(recipesWithAuthorOnly[0].booleanTest).to.be.undefined
                    expect(recipesWithAuthorOnly[0].author).to.equal(me!.did)
                })

                it("findAll() returns all relations on instances", async () => {
                    let root = Literal.from("findAll test 1").toUrl()
                    const recipe = new Recipe(perspective!, root);
                    recipe.comments = ["recipe://comment/1", "recipe://comment/2"];
                    recipe.entries = ["recipe://entry/1", "recipe://entry/2"];
                    await recipe.save();

                    // All relations are always returned (use include map for eager-loading related models)
                    const recipes = await Recipe.findAll(perspective!);
                    expect(recipes[0].comments.length).to.equal(2)
                    expect(recipes[0].entries.length).to.equal(2)
                })

                it("findAll() works with basic where queries", async () => {
                    // Create recipies
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 5;
                    recipe1.booleanTest = true;
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.number = 10;
                    recipe2.booleanTest = true;
                    await recipe2.save();

                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 15;
                    recipe3.booleanTest = false;
                    await recipe3.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3)

                    // Test where with valid name
                    const recipes1 = await Recipe.findAll(perspective!, { where: { name: "Recipe 1" } });
                    expect(recipes1.length).to.equal(1);

                    // Test where with invalid name
                    const recipes2 = await Recipe.findAll(perspective!, { where: { name: "This name doesn't exist" } });
                    expect(recipes2.length).to.equal(0);

                    // Test where with boolean
                    const recipes3 = await Recipe.findAll(perspective!, { where: { booleanTest: true } });
                    expect(recipes3.length).to.equal(2);

                    // Test where with number
                    const recipes4 = await Recipe.findAll(perspective!, { where: { number: 5 } });
                    expect(recipes4.length).to.equal(1);

                    // Test where with an array of possible matches
                    const recipes5 = await Recipe.findAll(perspective!, { where: { name: ["Recipe 1", "Recipe 2"] } });
                    expect(recipes5.length).to.equal(2);

                    // Test where with author
                    const me = await ad4m!.agent.me();
                    // Test where with valid author
                    const recipes6 = await Recipe.findAll(perspective!, { where: { author: me.did } });
                    expect(recipes6.length).to.equal(3);
                    // Test where with invalid author
                    const recipes7 = await Recipe.findAll(perspective!, { where: { author: "This author doesn't exist" } });
                    expect(recipes7.length).to.equal(0);

                    // Test where with timestamp
                    const validTimestamp1 = allRecipes[0].timestamp;
                    const validTimestamp2 = allRecipes[1].timestamp;
                    const invalidTimestamp = new Date().getTime();
                    // Test where with valid timestamp
                    const recipes8 = await Recipe.findAll(perspective!, { where: { timestamp: validTimestamp1 } });
                    expect(recipes8.length).to.equal(1);
                    // Test where with invalid timestamp
                    const recipes9 = await Recipe.findAll(perspective!, { where: { timestamp: invalidTimestamp } });
                    expect(recipes9.length).to.equal(0);
                    // Test where with an array of possible timestamp matches
                    const recipes10 = await Recipe.findAll(perspective!, { where: { timestamp: [validTimestamp1, validTimestamp2] } });
                    expect(recipes10.length).to.equal(2);
                })

                it("findAll() works with where query not operations", async () => {
                    // Create recipies
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 5;
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.number = 10;
                    await recipe2.save();

                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 15;
                    await recipe3.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3);

                    // Store valid timestamps
                    const validTimestamp1 = allRecipes[0].timestamp;
                    const validTimestamp2 = allRecipes[1].timestamp;
                    const validTimestamp3 = allRecipes[2].timestamp;

                    // Test not operation on standard property
                    const recipes1 = await Recipe.findAll(perspective!, { where: { name: { not: "Recipe 1" } } });
                    expect(recipes1.length).to.equal(2);

                    // Test not operation on author
                    const me = await ad4m!.agent.me();
                    const recipes2 = await Recipe.findAll(perspective!, { where: { author: { not: me.did } } });
                    expect(recipes2.length).to.equal(0);

                    // Test not operation on timestamp
                    const recipes3 = await Recipe.findAll(perspective!, { where: { timestamp: { not: validTimestamp1 } } });
                    expect(recipes3.length).to.equal(2);

                    // Test not operation with an array of possible string matches
                    const recipes4 = await Recipe.findAll(perspective!, { where: { name: { not: ["Recipe 1", "Recipe 2"] } } });
                    expect(recipes4.length).to.equal(1);
                    expect(recipes4[0].name).to.equal("Recipe 3");

                    // Test not operation with an array of possible timestamp matches
                    const recipes5 = await Recipe.findAll(perspective!, { where: { timestamp: { not: [validTimestamp1, validTimestamp2] } } });
                    expect(recipes5.length).to.equal(1);
                    expect(recipes5[0].timestamp).to.equal(validTimestamp3);
                })

                it("findAll() works with where query lt, lte, gt, & gte operations", async () => {
                    // Create recipes
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 5;
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.number = 10;
                    await recipe2.save();

                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 15;
                    await recipe3.save();

                    const recipe4 = new Recipe(perspective!);
                    recipe4.name = "Recipe 4";
                    recipe4.number = 20;
                    await recipe4.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(4);

                    // 1. Number properties
                    // Test less than (lt) operation on number property
                    const recipes1 = await Recipe.findAll(perspective!, { where: { number: { lt: 10 } } });
                    expect(recipes1.length).to.equal(1);

                    // Test less than or equal to (lte) operation on number property
                    const recipes2 = await Recipe.findAll(perspective!, { where: { number: { lte: 10 } } });
                    expect(recipes2.length).to.equal(2);
                    
                    // Test greater than (gt) operation on number property
                    const recipes3 = await Recipe.findAll(perspective!, { where: { number: { gt: 10 } } });
                    expect(recipes3.length).to.equal(2);

                    // Test greater than or equal to (gte) operation on number property
                    const recipes4 = await Recipe.findAll(perspective!, { where: { number: { gte: 10 } } });
                    expect(recipes4.length).to.equal(3);

                    // 2. Timestamps
                    // Sort recipes by timestamp to ensure consistent ordering
                    const sortedRecipes = [...allRecipes].sort((a, b) => {
                        const aTime = typeof a.timestamp === 'number' ? a.timestamp : parseInt(a.timestamp);
                        const bTime = typeof b.timestamp === 'number' ? b.timestamp : parseInt(b.timestamp);
                        return aTime - bTime;
                    });
                    const recipe2timestamp = typeof sortedRecipes[1].timestamp === 'number' 
                        ? sortedRecipes[1].timestamp 
                        : parseInt(sortedRecipes[1].timestamp); // Second recipe by timestamp
                    
                    // Test less than (lt) operation on timestamp
                    const recipes5 = await Recipe.findAll(perspective!, { where: { timestamp: { lt: recipe2timestamp } } });
                    expect(recipes5.length).to.equal(1);

                    // Test less than or equal to (lte) operation on timestamp
                    const recipes6 = await Recipe.findAll(perspective!, { where: { timestamp: { lte: recipe2timestamp } } });
                    expect(recipes6.length).to.equal(2);

                    // Test greater than (gt) operation on timestamp
                    const recipes7 = await Recipe.findAll(perspective!, { where: { timestamp: { gt: recipe2timestamp } } });
                    expect(recipes7.length).to.equal(2);

                    // Test greater than or equal to (gte) operation on timestamp
                    const recipes8 = await Recipe.findAll(perspective!, { where: { timestamp: { gte: recipe2timestamp } } });
                    expect(recipes8.length).to.equal(3);
                })

                it("findAll() works with where query between operations", async () => {
                    @Model({
                        name: "Task_due"
                    })
                    class TaskDue extends Ad4mModel {
                        @Property({
                            through: "task://title",
                            resolveLanguage: "literal"
                        })
                        title: string = "";

                        @Property({
                            through: "task://priority",
                            resolveLanguage: "literal"
                        })
                        priority: number = 0;

                        @Property({
                            through: "task://dueDate",
                            resolveLanguage: "literal"
                        })
                        dueDate: number = 0;
                    }

                    // Register the Task class
                    await perspective!.ensureSDNASubjectClass(TaskDue);

                    // Create timestamps & tasks
                    const start = new Date().getTime();

                    const task1 = new TaskDue(perspective!);
                    task1.title = "Low priority task";
                    task1.priority = 2;
                    task1.dueDate = start;
                    await task1.save();

                    // Small delay to ensure different timestamps
                    await sleep(10);

                    let mid = new Date().getTime();
                    // Ensure mid > start even if system clock resolution is low
                    if (mid <= start) {
                        mid = start + 1;
                    }

                    const task2 = new TaskDue(perspective!);
                    task2.title = "Medium priority task";
                    task2.priority = 5;
                    task2.dueDate = mid + 1;
                    await task2.save();

                    const task3 = new TaskDue(perspective!);
                    task3.title = "High priority task";
                    task3.priority = 8;
                    task3.dueDate = mid + 2;
                    await task3.save();

                    // Small delay to ensure different timestamps
                    await sleep(10);

                    let end = new Date().getTime();
                    // Ensure end > mid even if system clock resolution is low
                    if (end <= mid) {
                        end = mid + 1;
                    }

                    // Check all tasks are there
                    const allTasks = await TaskDue.findAll(perspective!);
                    expect(allTasks.length).to.equal(3);

                    // Test between operation on priority
                    const lowToMediumTasks = await TaskDue.findAll(perspective!, { where: { priority: { between: [1, 5] } } });
                    expect(lowToMediumTasks.length).to.equal(2);

                    // Test between operation on priority with different values
                    const mediumToHighTasks = await TaskDue.findAll(perspective!, { where: { priority: { between: [5, 10] } } });
                    expect(mediumToHighTasks.length).to.equal(2);

                    // Test between operation on dueDate
                    const earlyTasks = await TaskDue.findAll(perspective!, { where: { dueDate: { between: [start, mid] } } });
                    expect(earlyTasks.length).to.equal(1);

                    // Test between operation on dueDate with different values
                    const laterTasks = await TaskDue.findAll(perspective!, { where: { dueDate: { between: [mid, end] } } });
                    expect(laterTasks.length).to.equal(2);

                    // Clean up
                    await task1.delete();
                    await task2.delete();
                    await task3.delete();
                })

                it("findAll() works with ordering", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
                    // Create recipes
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 10;
                    await recipe1.save();
                    
                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.number = 5;
                    await recipe2.save();
                    
                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 15;
                    await recipe3.save();
                    
                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3);

                    // Test ordering by number properties
                    const recipes1 = await Recipe.findAll(perspective!, { order: { number: "ASC" } });
                    expect(recipes1[0].number).to.equal(5);
                    expect(recipes1[1].number).to.equal(10);
                    expect(recipes1[2].number).to.equal(15);

                    const recipes2 = await Recipe.findAll(perspective!, { order: { number: "DESC" } });
                    expect(recipes2[0].number).to.equal(15);
                    expect(recipes2[1].number).to.equal(10);
                    expect(recipes2[2].number).to.equal(5);

                    // Test ordering by timestamp
                    const recipes3 = await Recipe.findAll(perspective!, { order: { timestamp: "ASC" } });
                    expect(recipes3[0].name).to.equal("Recipe 1");
                    expect(recipes3[1].name).to.equal("Recipe 2");
                    expect(recipes3[2].name).to.equal("Recipe 3");

                    const recipes4 = await Recipe.findAll(perspective!, { order: { timestamp: "DESC" } });
                    expect(recipes4[0].name).to.equal("Recipe 3");
                    expect(recipes4[1].name).to.equal("Recipe 2");
                    expect(recipes4[2].name).to.equal("Recipe 1");
                })

                it("findAll() works with limit and offset", async () => {
                                        // Create 6 recipe instances with sequential names
                    for (let i = 1; i <= 6; i++) {
                        const recipe = new Recipe(perspective!);
                        recipe.name = `Recipe ${i}`;
                        await recipe.save();
                    }
                    
                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(6);

                    // Test limit
                    const recipes1 = await Recipe.findAll(perspective!, { limit: 2 });
                    expect(recipes1.length).to.equal(2);

                    const recipes2 = await Recipe.findAll(perspective!, { limit: 4 });
                    expect(recipes2.length).to.equal(4);

                    // Test offset
                    const recipes3 = await Recipe.findAll(perspective!, { offset: 2 });
                    expect(recipes3[0].name).to.equal("Recipe 3");

                    const recipes4 = await Recipe.findAll(perspective!, { offset: 4 });
                    expect(recipes4[0].name).to.equal("Recipe 5");

                    // Test limit and offset
                    const recipes5 = await Recipe.findAll(perspective!, { limit: 2, offset: 1 });
                    expect(recipes5.length).to.equal(2);
                    expect(recipes5[0].name).to.equal("Recipe 2");

                    const recipes6 = await Recipe.findAll(perspective!, { limit: 3, offset: 2 });
                    expect(recipes6.length).to.equal(3);
                    expect(recipes6[0].name).to.equal("Recipe 3");
                })

                it("findAll() works with a mix of query constraints", async () => {
                    // Create recipies
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.booleanTest = true;
                    recipe1.comments = ["recipe://comment/r1/1", "recipe://comment/r1/2"];
                    recipe1.entries = ["recipe://entry/r1/1", "recipe://entry/r1/2"];
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.booleanTest = false;
                    recipe2.comments = ["recipe://comment/r2/1", "recipe://comment/r2/2"];
                    recipe2.entries = ["recipe://entry/r2/1", "recipe://entry/r2/2"];
                    await recipe2.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(2);

                    // Test with where and properties
                    const recipes1 = await Recipe.findAll(perspective!, { where: { name: "Recipe 1" }, properties: ["name"] });
                    expect(recipes1.length).to.equal(1);
                    expect(recipes1[0].name).to.equal("Recipe 1");
                    expect(recipes1[0].booleanTest).to.be.undefined;

                    // Test with different where and properties
                    const recipes2 = await Recipe.findAll(perspective!, { where: { name: "Recipe 2" }, properties: ["booleanTest"] });
                    expect(recipes2.length).to.equal(1);
                    expect(recipes2[0].name).to.be.undefined;
                    expect(recipes2[0].booleanTest).to.equal(false);
                })

                it("findAll() works with constraining resolved literal properties", async () => {
                    // Create a recipe with a resolved literal property
                    const recipe = new Recipe(perspective!);
                    recipe.resolve = "Hello World"
                    await recipe.save();

                    // Test with resolved literal property
                    const recipes1 = await Recipe.findAll(perspective!, { where: { resolve: "Hello World" } });
                    expect(recipes1.length).to.equal(1);
                    expect(recipes1[0].resolve).to.equal("Hello World");
                })

                it("findAll() works with multiple property constraints in one where clause", async () => { 
                    // Create recipes with different combinations of properties
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 5;
                    recipe1.booleanTest = true;
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2"; 
                    recipe2.number = 10;
                    recipe2.booleanTest = true;
                    await recipe2.save();

                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 15;
                    recipe3.booleanTest = false;
                    await recipe3.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3);

                    // Test where with multiple property constraints
                    const recipes1 = await Recipe.findAll(perspective!, { 
                        where: { 
                            name: "Recipe 1",
                            number: 5,
                            booleanTest: true
                        }
                    });
                    expect(recipes1.length).to.equal(1);

                    // Test where with multiple property constraints that match multiple recipes
                    const recipes2 = await Recipe.findAll(perspective!, {
                        where: {
                            number: { gt: 5 },
                            booleanTest: true
                        }
                    });
                    expect(recipes2.length).to.equal(1);
                    expect(recipes2[0].name).to.equal("Recipe 2");

                    // Test where with multiple property constraints that match no recipes
                    const recipes3 = await Recipe.findAll(perspective!, {
                        where: {
                            name: "Recipe 1",
                            booleanTest: false
                        }
                    });
                    expect(recipes3.length).to.equal(0);
                })

                it("findAllAndCount() returns both the retrived instances and the total count", async () => {
                    // Create 6 recipe instances with sequential names
                    for (let i = 1; i <= 6; i++) {
                        const recipe = new Recipe(perspective!);
                        recipe.name = `Recipe ${i}`;
                        recipe.number = 5;
                        await recipe.save();
                    }
                    
                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(6);

                    // Test count with limit
                    const { results: recipes1, totalCount: count1 } = await Recipe.findAllAndCount(perspective!, { limit: 2, count: true });
                    expect(recipes1.length).to.equal(2);
                    expect(count1).to.equal(6);

                    // Test count with offset & limit
                    const { results: recipes3, totalCount: count3 } = await Recipe.findAllAndCount(perspective!, { offset: 3, limit: 3, count: true });
                    expect(recipes3.length).to.equal(3);
                    expect(count3).to.equal(6);

                    // Test count with where constraints & limit
                    const { results: recipes2, totalCount: count2 } = await Recipe.findAllAndCount(perspective!, { where: { name: ["Recipe 1", "Recipe 2", "Recipe 3"] }, limit: 2, count: true });
                    expect(recipes2.length).to.equal(2);
                    expect(count2).to.equal(3);

                    // Test count with where equality constraint (exists), offset, & limit
                    const { results: recipes4, totalCount: count4 } = await Recipe.findAllAndCount(perspective!, { where: { number: 5 }, offset: 3, limit: 3, count: true });
                    expect(recipes4.length).to.equal(3);
                    expect(count4).to.equal(6);

                    // Test count with where equality constraint (does not exist), offset, & limit
                    const { results: recipes5, totalCount: count5 } = await Recipe.findAllAndCount(perspective!, { where: { number: 3 }, offset: 3, limit: 3, count: true });
                    expect(recipes5.length).to.equal(0);
                    expect(count5).to.equal(0);

                    // Test count with where not constraint & limit
                    const { results: recipes6, totalCount: count6 } = await Recipe.findAllAndCount(perspective!, { where: { name: { not: "Recipe 1" } }, limit: 3, count: true });
                    expect(recipes6.length).to.equal(3);
                    expect(count6).to.equal(5);

                    // Test count with where not constraint, offset, & limit
                    const { results: recipes7, totalCount: count7 } = await Recipe.findAllAndCount(perspective!, { where: { name: { not: "Recipe 2" } }, offset: 1, limit: 3, count: true });
                    expect(recipes7.length).to.equal(3);
                    expect(count7).to.equal(5);

                    // Test count with where not constraint, offset, & limit greater than remaining results
                    const { results: recipes8, totalCount: count8 } = await Recipe.findAllAndCount(perspective!, { where: { name: { not: "Recipe 4" } }, offset: 3, limit: 3, count: true });
                    expect(recipes8.length).to.equal(2);
                    expect(count8).to.equal(5);
                })

                it("paginate() helper function works with pageNumber & pageSize props", async () => {
                    // Create 6 recipe instances with sequential names
                    for (let i = 1; i <= 6; i++) {
                        const recipe = new Recipe(perspective!);
                        recipe.name = `Recipe ${i}`;
                        await recipe.save();
                    }
                    
                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(6);

                    // Test basic pagination (pageSize: 2, pageNumber: 1)
                    const { results: recipes1, totalCount: count1 } = await Recipe.paginate(perspective!, 2, 1);
                    expect(recipes1.length).to.equal(2);
                    expect(count1).to.equal(6);
                    expect(recipes1[0].name).to.equal("Recipe 1");
                    expect(recipes1[1].name).to.equal("Recipe 2");

                    // Test pagination with where constraints (pageSize: 3, pageNumber: 2)
                    const { results: recipes2, totalCount: count2 } = await Recipe.paginate(perspective!, 3, 2, { where: { name: { not: "Recipe 4" } } });
                    expect(recipes2.length).to.equal(2);
                    expect(count2).to.equal(5);
                    expect(recipes2[0].name).to.equal("Recipe 5");
                    expect(recipes2[1].name).to.equal("Recipe 6");
                });

                it("count() returns only the count without retrieving instances", async () => {
                    // Create 6 recipe instances with sequential names
                    for (let i = 1; i <= 6; i++) {
                        const recipe = new Recipe(perspective!);
                        recipe.name = `Recipe ${i}`;
                        await recipe.save();
                    }
                    
                    // Test count with no constraints
                    const count1 = await Recipe.count(perspective!);
                    expect(count1).to.equal(6);

                    // Test count with where constraints
                    const count2 = await Recipe.count(perspective!, { where: { name: ["Recipe 1", "Recipe 2", "Recipe 3"] } });
                    expect(count2).to.equal(3);

                    // Test count with more complex constraints
                    const count3 = await Recipe.count(perspective!, { where: { name: { not: "Recipe 1" } } });
                    expect(count3).to.equal(5);
                });

                it("count() and countSubscribe() work on the query builder", async () => {
                    // Create recipes
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    await recipe1.save();
                    
                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2"; 
                    await recipe2.save();
                    
                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    await recipe3.save();

                    // Test count() on query builder
                    const query = Recipe.query(perspective!);
                    const count = await query.count();
                    expect(count).to.equal(3);

                    // Test count with where clause
                    const filteredQuery = Recipe.query(perspective!)
                        .where({ name: ["Recipe 1", "Recipe 2"] });
                    const filteredCount = await filteredQuery.count();
                    expect(filteredCount).to.equal(2);

                    // Test countSubscribe
                    let lastCount = 0;
                    const builder = Recipe.query(perspective!);
                    const subscription = await builder
                        .countSubscribe((count) => {
                            lastCount = count;
                        });
                    expect(subscription).to.equal(3);

                    // Small delay to ensure subscription is fully registered before triggering changes
                    await sleep(500);

                    // Add another recipe and verify callback is called
                    const recipe4 = new Recipe(perspective!);
                    recipe4.name = "Recipe 4";
                    await recipe4.save();

                    // Wait for subscription to process with proper condition checking
                    // Use longer timeout for CI environments which may be slower
                    await waitForCondition(
                        () => lastCount === 4,
                        {
                            timeoutMs: 15000,
                            errorMessage: 'Count subscription did not update after recipe save'
                        }
                    );
                    expect(lastCount).to.equal(4);

                    // Dispose the subscription to prevent cross-test interference
                    builder.dispose();
                })

                it("count() works with advanced where conditions (gt, between, timestamp)", async () => {
                    // Create recipes with different numbers
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.number = 1;
                    await recipe1.save();
                    
                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2"; 
                    recipe2.number = 2;
                    await recipe2.save();
                    
                    const recipe3 = new Recipe(perspective!);
                    recipe3.name = "Recipe 3";
                    recipe3.number = 3;
                    await recipe3.save();

                    const recipe4 = new Recipe(perspective!);
                    recipe4.name = "Recipe 4";
                    recipe4.number = 4;
                    await recipe4.save();

                    const recipe5 = new Recipe(perspective!);
                    recipe5.name = "Recipe 5";
                    recipe5.number = 5;
                    await recipe5.save();

                    // Test count() with gt operator
                    const countGt3 = await Recipe.count(perspective!, { where: { number: { gt: 3 } } });
                    const findAllGt3 = await Recipe.findAll(perspective!, { where: { number: { gt: 3 } } });
                    expect(countGt3).to.equal(findAllGt3.length);
                    expect(countGt3).to.equal(2); // recipes 4 and 5

                    // Test count() with between operator
                    const countBetween2And4 = await Recipe.count(perspective!, { where: { number: { between: [2, 4] } } });
                    const findAllBetween2And4 = await Recipe.findAll(perspective!, { where: { number: { between: [2, 4] } } });
                    expect(countBetween2And4).to.equal(findAllBetween2And4.length);
                    expect(countBetween2And4).to.equal(3); // recipes 2, 3, and 4

                    // Test count() with gte and lte operators
                    const countGte2Lte4 = await Recipe.count(perspective!, { where: { number: { gte: 2, lte: 4 } } });
                    const findAllGte2Lte4 = await Recipe.findAll(perspective!, { where: { number: { gte: 2, lte: 4 } } });
                    expect(countGte2Lte4).to.equal(findAllGte2Lte4.length);
                    expect(countGte2Lte4).to.equal(3); // recipes 2, 3, and 4

                    // Test count() with lt operator
                    const countLt3 = await Recipe.count(perspective!, { where: { number: { lt: 3 } } });
                    const findAllLt3 = await Recipe.findAll(perspective!, { where: { number: { lt: 3 } } });
                    expect(countLt3).to.equal(findAllLt3.length);
                    expect(countLt3).to.equal(2); // recipes 1 and 2

                    // Test query builder count() with gt operator
                    const queryCountGt3 = await Recipe.query(perspective!)
                        .where({ number: { gt: 3 } })
                        .count();
                    const queryGetGt3 = await Recipe.query(perspective!)
                        .where({ number: { gt: 3 } })
                        .get();
                    expect(queryCountGt3).to.equal(queryGetGt3.length);
                    expect(queryCountGt3).to.equal(2);

                    // Test query builder count() with between operator
                    const queryCountBetween = await Recipe.query(perspective!)
                        .where({ number: { between: [2, 4] } })
                        .count();
                    const queryGetBetween = await Recipe.query(perspective!)
                        .where({ number: { between: [2, 4] } })
                        .get();
                    expect(queryCountBetween).to.equal(queryGetBetween.length);
                    expect(queryCountBetween).to.equal(3);

                    // Test count() with timestamp filtering
                    // Get the timestamp of recipe3
                    const allRecipes = await Recipe.findAll(perspective!);
                    const recipe3Instance = allRecipes.find((r: any) => r.name === "Recipe 3");
                    expect(recipe3Instance).to.not.be.undefined;
                    
                    if (recipe3Instance && recipe3Instance.timestamp) {
                        // Convert timestamp to number if it's a string
                        const recipe3Timestamp = typeof recipe3Instance.timestamp === 'string' 
                            ? new Date(recipe3Instance.timestamp).getTime() 
                            : recipe3Instance.timestamp;
                        
                        // Count recipes with timestamp greater than recipe3's timestamp
                        const countAfterRecipe3 = await Recipe.count(perspective!, { 
                            where: { timestamp: { gt: recipe3Timestamp } } 
                        });
                        const findAllAfterRecipe3 = await Recipe.findAll(perspective!, { 
                            where: { timestamp: { gt: recipe3Timestamp } } 
                        });
                        expect(countAfterRecipe3).to.equal(findAllAfterRecipe3.length);
                        expect(countAfterRecipe3).to.be.at.least(2); // At least recipes 4 and 5
                    }
                })

                it("paginate() and paginateSubscribe() work on the query builder", async () => {
                    // Create test recipes
                    for (let i = 1; i <= 10; i++) {
                        const recipe = new Recipe(perspective!);
                        recipe.name = `Recipe ${i}`;
                        await recipe.save();
                    }

                    // Test paginate()
                    const query = Recipe.query(perspective!);
                    const page1 = await query.paginate(3, 1);
                    expect(page1.results.length).to.equal(3);
                    expect(page1.totalCount).to.equal(10);
                    expect(page1.results[0].name).to.equal("Recipe 1");
                    expect(page1.results[2].name).to.equal("Recipe 3");

                    const page2 = await query.paginate(3, 2);
                    expect(page2.results.length).to.equal(3);
                    expect(page2.results[0].name).to.equal("Recipe 4");

                    const lastPage = await query.paginate(3, 4);
                    expect(lastPage.results.length).to.equal(1);
                    expect(lastPage.results[0].name).to.equal("Recipe 10");

                    // Test paginateSubscribe()
                    let lastResult: any = null;
                    const initialResult = await query.paginateSubscribe(3, 1, (result) => {
                        lastResult = result;
                    });

                    expect(initialResult.results.length).to.equal(3);
                    expect(initialResult.totalCount).to.equal(10);
                    // Reset lastResult to verify we get an update
                    lastResult = null;

                    // Small delay to ensure subscription is fully registered before triggering changes
                    await sleep(500);

                    // Add a new recipe and verify subscription updates
                    const newRecipe = new Recipe(perspective!);
                    newRecipe.name = "Recipe 11";
                    await newRecipe.save();

                    // Wait for subscription update with proper condition checking
                    // Use longer timeout for CI environments which may be slower
                    await waitForCondition(
                        () => lastResult !== null,
                        {
                            timeoutMs: 15000,
                            errorMessage: 'Paginate subscription did not update after recipe save'
                        }
                    );

                    expect(lastResult.totalCount).to.equal(11);

                    // Dispose the subscription to prevent cross-test interference
                    query.dispose();
                })

                it("query builder works with subscriptions", async () => {
                    @Model({
                        name: "Notification"
                    })
                    class Notification extends Ad4mModel {
                        @Property({
                            through: "notification://title",
                            resolveLanguage: "literal"
                        })
                        title: string = "";

                        @Property({
                            through: "notification://priority",
                            resolveLanguage: "literal"
                        })
                        priority: number = 0;

                        @Property({
                            through: "notification://read",
                            resolveLanguage: "literal"
                        })
                        read: boolean = false;
                    }

                    // Register the Notification class
                    await perspective!.ensureSDNASubjectClass(Notification);

                    // Clear any previous notifications
                    let notifications = await Notification.findAll(perspective!);
                    for (const notification of notifications) await notification.delete();

                    // Set up subscription for high-priority unread notifications
                    let updateCount = 0;
                    const builder = Notification
                        .query(perspective!)
                        .where({ 
                            priority: { gt: 5 },
                            read: false
                        });
                    const initialResults = await builder
                        .subscribe((newNotifications) => {
                            notifications = newNotifications;
                            updateCount++;
                        });

                    // Initially no results (subscribe() invokes callback with initial results)
                    expect(initialResults.length).to.equal(0);
                    // Reset updateCount since subscribe() fires the callback once with initial results
                    updateCount = 0;

                    // Add matching notification - should trigger subscription
                    const notification1 = new Notification(perspective!);
                    notification1.title = "High priority notification";
                    notification1.priority = 8;
                    notification1.read = false;
                    await notification1.save();

                    // Wait for subscription to fire with smart polling
                    for (let i = 0; i < 30; i++) {
                        if (updateCount >= 1 && notifications.length === 1) break;
                        await sleep(50);
                    }
                    expect(updateCount).to.be.at.least(1);
                    expect(notifications.length).to.equal(1);

                    // Add another matching notification - should trigger subscription again
                    const notification2 = new Notification(perspective!);
                    notification2.title = "Another high priority";
                    notification2.priority = 7;
                    notification2.read = false;
                    await notification2.save();

                    for (let i = 0; i < 30; i++) {
                        if (updateCount >= 2 && notifications.length === 2) break;
                        await sleep(50);
                    }
                    expect(updateCount).to.be.at.least(2);
                    expect(notifications.length).to.equal(2);

                    // Add non-matching notification (low priority) - should not trigger subscription
                    const notification3 = new Notification(perspective!);
                    notification3.title = "Low priority notification";
                    notification3.priority = 3;
                    notification3.read = false;
                    await notification3.save();

                    await sleep(200); // Give it time but don't wait the full second
                    // With SPARQL we get 3 updates because we do comparison filtering in the client
                    // and not the query. So the raw query result actually is different, even though
                    // the ultimate result is the same.
                    //expect(updateCount).to.equal(2);
                    expect(notifications.length).to.equal(2);

                    // Mark notification1 as read - should trigger subscription to remove it
                    notification1.read = true;
                    await notification1.save();
                    for (let i = 0; i < 30; i++) {
                        if (notifications.length === 1) break;
                        await sleep(50);
                    }
                    expect(notifications.length).to.equal(1);

                    // Dispose the subscription to prevent cross-test interference
                    builder.dispose();
                });

                it("query builder should filter by subject class", async () => {
                    // Define a second subject class
                    @Model({
                        name: "Note1"
                    })
                    class Note1 extends Ad4mModel {
                        @Property({
                            through: "note://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @Property({
                            through: "note1://content",
                            resolveLanguage: "literal",
                            required: true,
                        })
                        content1: string = "";
                    }

                    @Model({
                        name: "Note2"
                    })
                    class Note2 extends Ad4mModel {
                        @Property({
                            through: "note://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @Property({
                            through: "note2://content",
                            resolveLanguage: "literal",
                            required: true,
                        })
                        content2: string = "";
                    }

                    // Register the Note class
                    await perspective!.ensureSDNASubjectClass(Note1);
                    await perspective!.ensureSDNASubjectClass(Note2);

                    // Create instances of both classes with the same name
                    const note1 = new Note1(perspective!);
                    note1.name = "Test Item";
                    await note1.save();

                    const note2 = new Note2(perspective!);
                    note2.name = "Test Item";
                    await note2.save();

                    // Query for recipes - this should only return the recipe instance
                    const note1Results = await Note1.query(perspective!).where({ name: "Test Item" }).get()
                    
                    //console.log("note1Results: ", note1Results)
                    // This assertion will fail because the query builder doesn't filter by class
                    expect(note1Results.length).to.equal(1);
                    expect(note1Results[0]).to.be.instanceOf(Note1);
                });

                it("query builder works with single query object, complex query and subscriptions", async () => {
                    @Model({
                        name: "Task"
                    })
                    class Task extends Ad4mModel {
                        @Property({
                            through: "task://description",
                            resolveLanguage: "literal"
                        })
                        description: string = "";

                        @Property({
                            through: "task://dueDate",
                            resolveLanguage: "literal"
                        })
                        dueDate: number = 0;


                        @Property({
                            through: "task://completed",
                            resolveLanguage: "literal"
                        })
                        completed: boolean = false;

                        @Property({
                            through: "task://assignee",
                            resolveLanguage: "literal"
                        })
                        assignee: string = "";
                    }

                    // Register the Task class
                    await perspective!.ensureSDNASubjectClass(Task);

                    // Clear any previous tasks
                    let tasks = await Task.findAll(perspective!);
                    for (const task of tasks) await task.delete();

                    const tomorrow = new Date();
                    tomorrow.setDate(tomorrow.getDate() + 1);
                    const tomorrowTimestamp = tomorrow.getTime();

                    const nextWeek = new Date();
                    nextWeek.setDate(nextWeek.getDate() + 7);
                    const nextWeekTimestamp = nextWeek.getTime();

                    // Set up subscription for upcoming incomplete tasks assigned to "alice"
                    let updateCount = 0;
                    const builder = Task.query(perspective!, { 
                        where: { 
                            dueDate: { lte: nextWeekTimestamp },
                            completed: false,
                            assignee: "alice"
                        }
                    });
                    const initialResults = await builder.subscribe((newTasks) => {
                        tasks = newTasks;
                        updateCount++;
                    });

                    // Initially no results (returned via Promise, callback not fired)
                    expect(initialResults.length).to.equal(0);

                    // Add matching task - should trigger subscription
                    const task1 = new Task(perspective!);
                    task1.description = "Urgent task for tomorrow";
                    task1.dueDate = tomorrowTimestamp;
                    task1.completed = false;
                    task1.assignee = "alice";
                    await task1.save();
                    
                    await task1.get();

                    // Wait for subscription to fire with proper condition checking
                    await waitForCondition(
                        () => updateCount === 1 && tasks.length === 1,
                        { 
                            timeoutMs: 5000, 
                            errorMessage: 'Subscription did not fire after first task save' 
                        }
                    );

                    expect(updateCount).to.equal(1);
                    expect(tasks.length).to.equal(1);

                    // Add another matching task - should trigger subscription again
                    const task2 = new Task(perspective!);
                    task2.description = "Another task for next week";
                    task2.dueDate = nextWeekTimestamp;
                    task2.completed = false;
                    task2.assignee = "alice";
                    await task2.save();

                    // Wait for subscription to fire with proper condition checking
                    await waitForCondition(
                        () => updateCount === 2 && tasks.length === 2,
                        { 
                            timeoutMs: 5000, 
                            errorMessage: 'Subscription did not fire after second task save' 
                        }
                    );
                    expect(updateCount).to.equal(2);
                    expect(tasks.length).to.equal(2);

                    // Add non-matching task (wrong assignee) - should not trigger subscription
                    const task3 = new Task(perspective!);
                    task3.description = "Task assigned to bob";
                    task3.dueDate = tomorrowTimestamp;
                    task3.completed = false;
                    task3.assignee = "bob";
                    await task3.save();

                    await sleep(1000);
                    expect(updateCount).to.equal(2);
                    expect(tasks.length).to.equal(2);

                    // Mark task1 as completed - should trigger subscription to remove it
                    task1.completed = true;
                    await task1.save();
                    
                    // Wait for subscription to fire with proper condition checking
                    await waitForCondition(
                        () => tasks.length === 1,
                        { 
                            timeoutMs: 5000, 
                            errorMessage: 'Subscription did not fire after task update' 
                        }
                    );

                    expect(tasks.length).to.equal(1);   

                    // Dispose the subscription to prevent cross-test interference
                    builder.dispose();
                });

                it("transform option in property decorators works", async () => {
                    const transformTestPerspective = await ad4m?.perspective.add("transform-test");
                    @Model({ name: "ImagePost" })
                    class ImagePost extends Ad4mModel {
                        @Property({
                            through: "image://data",
                            resolveLanguage: "literal",
                            transform: (data: any) => data ? `data:image/png;base64,${data}` : undefined,
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
                    const [retrieved] = await ImagePost.findAll(transformTestPerspective!);
                    expect(retrieved.image).to.equal("data:image/png;base64,abc123");
                });

                it("should support batch operations with multiple models", async () => {
                    let perspective = await ad4m!.perspective.add("batch test")
                    @Model({
                        name: "BatchRecipe"
                    })
                    class BatchRecipe extends Ad4mModel {
                        @Property({
                            through: "recipe://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @HasMany({ through: "recipe://ingredients" })
                        ingredients: string[] = [];
                    }

                    @Model({
                        name: "BatchNote"
                    })
                    class BatchNote extends Ad4mModel {
                        @Property({
                            through: "note://title",
                            resolveLanguage: "literal"
                        })
                        title: string = "";

                        @Property({
                            through: "note://content",
                            resolveLanguage: "literal"
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
                    recipe.ingredients = ["recipe://ingredient/pasta", "recipe://ingredient/sauce", "recipe://ingredient/cheese"];
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
                    expect(recipesAfterCommit[0].ingredients).to.have.members(["recipe://ingredient/pasta", "recipe://ingredient/sauce", "recipe://ingredient/cheese"]);

                    const notesAfterCommit = await BatchNote.findAll(perspective!);
                    expect(notesAfterCommit.length).to.equal(1);
                    expect(notesAfterCommit[0].title).to.equal("Recipe Notes");
                    expect(notesAfterCommit[0].content).to.equal("Make sure to use fresh ingredients");

                    // Test updating models in batch
                    const updateBatchId = await perspective!.createBatch();
                    recipe.ingredients.push("recipe://ingredient/garlic");
                    await recipe.save(updateBatchId);

                    note.content = "Updated: Use fresh ingredients and add garlic";
                    await note.save(updateBatchId);

                    // Verify models haven't changed before commit
                    const recipesBeforeUpdate = await BatchRecipe.findAll(perspective!);
                    expect(recipesBeforeUpdate[0].ingredients).to.have.members(["recipe://ingredient/pasta", "recipe://ingredient/sauce", "recipe://ingredient/cheese"]);

                    const notesBeforeUpdate = await BatchNote.findAll(perspective!);
                    expect(notesBeforeUpdate[0].content).to.equal("Make sure to use fresh ingredients");

                    // Commit update batch
                    const updateResult = await perspective!.commitBatch(updateBatchId);
                    expect(updateResult.additions.length).to.be.greaterThan(0);

                    // Verify models are updated
                    const recipesAfterUpdate = await BatchRecipe.findAll(perspective!);
                    expect(recipesAfterUpdate[0].ingredients.length).to.equal(4);
                    expect(recipesAfterUpdate[0].ingredients.includes("recipe://ingredient/pasta")).to.be.true;
                    expect(recipesAfterUpdate[0].ingredients.includes("recipe://ingredient/sauce")).to.be.true;
                    expect(recipesAfterUpdate[0].ingredients.includes("recipe://ingredient/cheese")).to.be.true;
                    expect(recipesAfterUpdate[0].ingredients.includes("recipe://ingredient/garlic")).to.be.true;

                    const notesAfterUpdate = await BatchNote.findAll(perspective!);
                    expect(notesAfterUpdate[0].content).to.equal("Updated: Use fresh ingredients and add garlic");

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

                describe("Query Subscriptions", () => {
                    let perspective: PerspectiveProxy;

                    @Model({ name: "SubscriptionTestModel" })
                    class TestModel extends Ad4mModel {
                        @Property({
                            through: "test://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @Property({
                            through: "test://status",
                            resolveLanguage: "literal"
                        })
                        status: string = "";
                    }

                    beforeEach(async () => {
                        perspective = await ad4m!.perspective.add("subscription-parity-test");
                        await perspective!.ensureSDNASubjectClass(TestModel);
                    });

                    afterEach(async () => {
                        if (perspective) {
                            await ad4m!.perspective.remove(perspective.uuid);
                        }
                    });

                    // REMOVED: SPARQL vs Prolog parity test
                    // This test compared SPARQL and Prolog subscription results.
                    // With SHACL migration, SPARQL is now the primary query engine.
                    // Prolog subscriptions are deprecated - no need for parity testing.

                    it("should demonstrate subscription performance", async () => {
                        // Measure latency of update
                        const subscriptionCallback = sinon.fake();
                        const queryBuilder = TestModel.query(perspective).where({ status: "perf-test" });
                        await queryBuilder.subscribe(subscriptionCallback);

                        const start = Date.now();
                        const model = new TestModel(perspective);
                        model.name = "Perf Item";
                        model.status = "perf-test";
                        await model.save();
                        const saveTime = Date.now();

                        // Poll until callback called
                        while (!subscriptionCallback.called) {
                            await sleep(10);
                            if (Date.now() - saveTime > 5000) throw new Error("Timeout waiting for subscription update");
                        }

                        const saveLatency = saveTime - start;
                        const subscriptionLatency = Date.now() - saveTime;
                        console.log(`TestModel.save() latency: ${saveLatency}ms`);
                        console.log(`Subscription update latency: ${subscriptionLatency}ms`);

                        queryBuilder.dispose();
                    });
                });

                describe('ModelQueryBuilder', () => {
                    let perspective: PerspectiveProxy;

                    // Define a simple test model
                    @Model({ name: "TestModel" })
                    class TestModel extends Ad4mModel {
                        @Property({
                            through: "test://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @Property({
                            through: "test://status",
                            resolveLanguage: "literal"
                        })
                        status: string = "";
                    }

                    beforeEach(async () => {
                        perspective = await ad4m!.perspective.add("query-builder-test");
                        await perspective!.ensureSDNASubjectClass(TestModel);
                    });

                    afterEach(async () => {
                        // Clean up perspective to prevent cross-test interference
                        if (perspective) {
                            await ad4m!.perspective.remove(perspective.uuid);
                        }
                    });

                    it('handles subscriptions and disposal correctly', async () => {
                        // Create a query builder
                        const builder = TestModel.query(perspective)
                            .where({ status: "active" });

                        // Set up callback spies
                        const callback1 = sinon.fake();
                        const callback2 = sinon.fake();

                        // Create first subscription
                        const initialResults1 = await builder.subscribe(callback1);
                        expect(initialResults1).to.be.an('array');
                        expect(initialResults1.length).to.equal(0);

                        // Add a matching model
                        const model1 = new TestModel(perspective);
                        model1.name = "Test 1";
                        model1.status = "active";
                        await model1.save();

                        // Wait for subscription update with proper condition checking
                        // subscribe() returns initial results via Promise only — callback
                        // fires only for subsequent updates (to avoid double-setState in Preact)
                        await waitForCondition(
                            () => callback1.callCount >= 1,
                            {
                                timeoutMs: 5000,
                                errorMessage: 'First callback was not called after model save'
                            }
                        );

                        // Verify callback was called with the saved model
                        expect(callback1.callCount).to.be.at.least(1);
                        expect(callback1.lastCall.args[0]).to.be.an('array');
                        expect(callback1.lastCall.args[0].length).to.equal(1);
                        expect(callback1.lastCall.args[0][0].name).to.equal("Test 1");

                        // Create second subscription (should dispose first one)
                        const initialResults2 = await builder.subscribe(callback2);
                        expect(initialResults2).to.be.an('array');
                        expect(initialResults2.length).to.equal(1);

                        // Add another matching model
                        const model2 = new TestModel(perspective);
                        model2.name = "Test 2";
                        model2.status = "active";
                        await model2.save();

                        // Wait for subscription update with proper condition checking
                        await waitForCondition(
                            () => callback2.callCount >= 1,
                            {
                                timeoutMs: 5000,
                                errorMessage: 'Second callback was not called after model save'
                            }
                        );

                        // Verify only second callback was called (callback1 was disposed)
                        // callback1: 1 (model1 save only), no more after dispose
                        expect(callback1.callCount).to.equal(1);
                        expect(callback2.callCount).to.be.at.least(1);
                        expect(callback2.lastCall.args[0]).to.be.an('array');
                        expect(callback2.lastCall.args[0].length).to.equal(2);

                        // Dispose subscription
                        builder.dispose();

                        // Add another model - should not trigger callback
                        const model3 = new TestModel(perspective);
                        model3.name = "Test 3";
                        model3.status = "active";
                        await model3.save();

                        // Wait to ensure no callbacks
                        await sleep(1000);

                        // Verify no new callbacks after dispose
                        // callback1: 1 (model1 save only)
                        // callback2: 1 (model2 save only)
                        expect(callback1.callCount).to.equal(1);
                        expect(callback2.callCount).to.equal(1);
                    });

                    it('handles count subscriptions and disposal', async () => {
                        const builder = TestModel.query(perspective)
                            .where({ status: "active" });

                        const countCallback = sinon.fake();
                        const initialCount = await builder.countSubscribe(countCallback);
                        expect(initialCount).to.equal(0);

                        // Small delay to ensure subscription is fully registered before triggering changes
                        await sleep(500);

                        // Add a matching model
                        const model = new TestModel(perspective);
                        model.name = "Test";
                        model.status = "active";
                        await model.save();

                        // Wait for subscription update with proper condition checking
                        // countSubscribe() returns initial count via Promise only —
                        // callback fires only for subsequent updates
                        await waitForCondition(
                            () => countCallback.callCount >= 1,
                            {
                                timeoutMs: 15000,
                                errorMessage: 'Count callback was not called after model save'
                            }
                        );

                        // Verify callback was called with new count
                        expect(countCallback.callCount).to.be.at.least(1);
                        expect(countCallback.lastCall.args[0]).to.equal(1);
                        let count = countCallback.callCount

                        // Dispose subscription
                        builder.dispose();

                        // Add another model - should not trigger callback
                        const model2 = new TestModel(perspective);
                        model2.name = "Test 2";
                        model2.status = "active";
                        await model2.save();

                        // Wait to ensure no callback (still using sleep since we're verifying no change)
                        await sleep(1000);

                        // Verify no new callbacks
                        expect(countCallback.callCount).to.equal(count);
                    });

                    it('handles paginated subscriptions and disposal', async () => {
                        const builder = TestModel.query(perspective)
                            .where({ status: "active" });

                        const pageCallback = sinon.fake();
                        const initialPage = await builder.paginateSubscribe(2, 1, pageCallback);
                        expect(initialPage.results.length).to.equal(0);
                        expect(initialPage.totalCount).to.equal(0);

                        // Small delay to ensure subscription is fully registered before triggering changes
                        await sleep(500);

                        // Add models
                        const model1 = new TestModel(perspective);
                        model1.name = "Test 1";
                        model1.status = "active";
                        await model1.save();

                        const model2 = new TestModel(perspective);
                        model2.name = "Test 2";
                        model2.status = "active";
                        await model2.save();

                        // Wait for subscription updates with proper condition checking
                        // Use longer timeout for CI environments which may be slower
                        await waitForCondition(
                            () => pageCallback.called && pageCallback.lastCall.args[0].results.length >= 2,
                            {
                                timeoutMs: 15000,
                                errorMessage: 'Paginate callback was not called with expected results after model saves'
                            }
                        );

                        // Verify callback was called with updated page
                        expect(pageCallback.called).to.be.true;
                        expect(pageCallback.lastCall.args[0].results.length).to.equal(2);
                        expect(pageCallback.lastCall.args[0].totalCount).to.equal(2);

                        console.log("countCallback", pageCallback.lastCall.args[0])
                        let count = pageCallback.callCount

                        // Dispose subscription
                        builder.dispose();

                        // Add another model - should not trigger callback
                        const model3 = new TestModel(perspective);
                        model3.name = "Test 3";
                        model3.status = "active";
                        await model3.save();

                        // Wait to ensure no callback
                        await sleep(1000);

                        // Verify no new callbacks
                        expect(pageCallback.callCount).to.equal(count);
                    });
                });

                describe("Emoji and Special Character Handling", () => {
                    @Model({
                        name: "Message"
                    })
                    class EmojiMessage extends Ad4mModel {
                        @Flag({
                            through: "ad4m://entry_type",
                            value: "flux://message"
                        })
                        type: string = ""

                        @Property({
                            through: "flux://body",
                            resolveLanguage: "literal"
                        })
                        body: string = ""
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
                        const retrievedMessage = messages.find((m: EmojiMessage) => m.body === "<p>👋</p>");

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
                        const foundMessage = messages.find((m: EmojiMessage) => m.body === "<p>🏳️‍🌈 Complex emoji with modifiers 👨‍👩‍👧‍👦</p>");
                        
                        expect(foundMessage).to.not.be.undefined;
                        expect(foundMessage!.body).to.equal("<p>🏳️‍🌈 Complex emoji with modifiers 👨‍👩‍👧‍👦</p>");
                    });

                    it("should correctly handle special characters and Unicode", async () => {
                        // Test with various special characters that could break URL encoding
                        const specialMessage = new EmojiMessage(perspective!);
                        specialMessage.body = "<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>";
                        await specialMessage.save();

                        // Verify retrieval through findAll
                        const messages = await EmojiMessage.findAll(perspective!);
                        const special = messages.find((m: EmojiMessage) => m.body === "<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>");
                        
                        expect(special).to.not.be.undefined;
                        expect(special!.body).to.equal("<p>Special chars: àáâãäåæçèéêë ñ © ® ™ €</p>");
                    });

                    it("should handle mixed content with emojis and HTML entities", async () => {
                        // Test HTML entities mixed with emojis
                        const mixedMessage = new EmojiMessage(perspective!);
                        mixedMessage.body = "<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>";
                        await mixedMessage.save();

                        // Test direct property access after save/reload cycle
                        const allMessages = await EmojiMessage.findAll(perspective!);
                        const mixedMsg = allMessages.find((m: EmojiMessage) => m.body === "<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>");
                        
                        expect(mixedMsg).to.not.be.undefined;
                        expect(mixedMsg!.body).to.equal("<p>Mixed: &lt;emoji&gt; 😊 &amp; &quot;quotes&quot; 🎉</p>");
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
                        const initialResults = await builder.subscribe((messages: EmojiMessage[]) => {
                            subscriptionResults = messages;
                            updateCount++;
                        });

                        // Initially no results
                        expect(initialResults.length).to.equal(0);
                        // Reset updateCount since subscribe() fires the callback once with initial results
                        updateCount = 0;

                        // Create a message after setting up subscription - should trigger callback
                        const subscriptionMessage = new EmojiMessage(perspective!);
                        subscriptionMessage.body = "Subscription test with emoji: 🎯✅";
                        await subscriptionMessage.save();

                        // Wait for subscription to process with proper condition checking
                        await waitForCondition(
                            () => updateCount === 1,
                            { 
                                timeoutMs: 5000, 
                                errorMessage: 'Subscription did not fire after first message save' 
                            }
                        );

                        // Verify subscription callback was called
                        expect(updateCount).to.equal(1);
                        expect(subscriptionResults.length).to.equal(1);
                        expect(subscriptionResults[0].body).to.equal("Subscription test with emoji: 🎯✅");

                        // Add another message with emojis - should trigger subscription again
                        const secondMessage = new EmojiMessage(perspective!);
                        secondMessage.body = "Another emoji message: 🚀💯";
                        await secondMessage.save();

                        // Wait for subscription to process with proper condition checking
                        await waitForCondition(
                            () => updateCount === 2,
                            { 
                                timeoutMs: 5000, 
                                errorMessage: 'Subscription did not fire after second message save' 
                            }
                        );

                        // Verify subscription was called again
                        expect(updateCount).to.equal(2);
                        expect(subscriptionResults.length).to.equal(2);
                        const foundSecond = subscriptionResults.find(m => m.body === "Another emoji message: 🚀💯");
                        expect(foundSecond).to.not.be.undefined;

                        // Also verify the message exists through direct query
                        const messages = await EmojiMessage.findAll(perspective!);
                        const found = messages.find((m: EmojiMessage) => m.body === "Subscription test with emoji: 🎯✅");
                        expect(found).to.not.be.undefined;
                        expect(found!.body).to.equal("Subscription test with emoji: 🎯✅");

                        // Dispose the subscription to prevent cross-test interference
                        builder.dispose();
                    });
                });
            })

            describe("getter feature tests", () => {
                @Model({ name: "BlogPost" })
                class BlogPost extends Ad4mModel {
                    @Property({ 
                        through: "blog://title",
                        resolveLanguage: "literal"
                    })
                    title: string = "";

                    @Optional({
                        through: "blog://parent",
                        getter: "SELECT ?target WHERE { <Base> <blog://reply_to> ?target . } LIMIT 1"
                    })
                    parentPost: string | undefined;

                    @HasMany({
                        getter: "SELECT ?target WHERE { <Base> <blog://tagged_with> ?target . }"
                    })
                    tags: string[] = [];
                }

                beforeEach(async () => {
                    if(perspective) {
                        await ad4m!.perspective.remove(perspective.uuid)
                    }
                    perspective = await ad4m!.perspective.add("getter-test")
                    await perspective!.ensureSDNASubjectClass(BlogPost)
                });

                it("should evaluate getter for property", async () => {
                    const postRoot = Literal.from("Blog post for getter property test").toUrl();
                    const parentRoot = Literal.from("Parent blog post").toUrl();

                    const post = new BlogPost(perspective!, postRoot);
                    post.title = "Reply Post";
                    await post.save();

                    const parent = new BlogPost(perspective!, parentRoot);
                    parent.title = "Original Post";
                    await parent.save();

                    // Create the link that getter should find
                    await perspective!.add(new Link({
                        source: postRoot,
                        predicate: "blog://reply_to",
                        target: parentRoot
                    }));

                    // Get the post and check if getter resolved the parent
                    const retrievedPost = new BlogPost(perspective!, postRoot);
                    await retrievedPost.get();

                    expect(retrievedPost.parentPost).to.equal(parentRoot);
                });

                it("should evaluate getter for collection", async () => {
                    const postRoot = Literal.from("Blog post for getter collection test").toUrl();
                    const tag1 = Literal.from("tag:javascript").toUrl();
                    const tag2 = Literal.from("tag:typescript").toUrl();

                    const post = new BlogPost(perspective!, postRoot);
                    post.title = "Test Post";
                    await post.save();

                    // Create links that getter should find
                    await perspective!.add(new Link({
                        source: postRoot,
                        predicate: "blog://tagged_with",
                        target: tag1
                    }));
                    await perspective!.add(new Link({
                        source: postRoot,
                        predicate: "blog://tagged_with",
                        target: tag2
                    }));

                    // Get the post and check if getter resolved the tags
                    const retrievedPost = new BlogPost(perspective!, postRoot);
                    await retrievedPost.get();

                    expect(retrievedPost.tags).to.include(tag1);
                    expect(retrievedPost.tags).to.include(tag2);
                    expect(retrievedPost.tags.length).to.equal(2);
                });

                it("should filter out 'None' and empty values from getter results", async () => {
                    const postRoot = Literal.from("Blog post for None filtering test").toUrl();

                    const post = new BlogPost(perspective!, postRoot);
                    post.title = "Post without parent";
                    await post.save();

                    // Don't create any reply_to link, so getter should return None/empty

                    const retrievedPost = new BlogPost(perspective!, postRoot);
                    await retrievedPost.get();

                    // Property should be undefined, not 'None' or empty string
                    expect(retrievedPost.parentPost).to.be.undefined;
                });
            })

            describe("type-filtered relation tests (replaces isInstance)", () => {
                // The old @Collection({ where: { isInstance: Comment } }) pattern is replaced
                // by @HasMany(() => Comment, { through: ... }) which auto-generates a
                // conformance filter from the target model's metadata (flags, required props).
                // This achieves the same result: only linked items that are valid Comment
                // instances (have the ad4m://type -> ad4m://comment flag) are returned.

                @Model({ name: "Comment" })
                class Comment extends Ad4mModel {
                    @Flag({
                        through: "ad4m://type",
                        value: "ad4m://comment"
                    })
                    type!: string;

                    @Property({ 
                        through: "comment://text",
                        resolveLanguage: "literal"
                    })
                    text: string = "";
                }

                @Model({ name: "Article" })
                class Article extends Ad4mModel {
                    @Property({ 
                        through: "article://title",
                        resolveLanguage: "literal"
                    })
                    title: string = "";

                    @HasMany(() => Comment, { through: "article://has_comment" })
                    comments: string[] = [];
                }

                @Model({ name: "ArticleWithString" })
                class ArticleWithString extends Ad4mModel {
                    @Property({ 
                        through: "article://title",
                        resolveLanguage: "literal"
                    })
                    title: string = "";

                    @HasMany(() => Comment, { through: "article://has_comment" })
                    comments: string[] = [];
                }

                beforeEach(async () => {
                    if(perspective) {
                        await ad4m!.perspective.remove(perspective.uuid)
                    }
                    perspective = await ad4m!.perspective.add("type-filter-test")
                    
                    // Register both Comment and Article classes using ensureSDNASubjectClass
                    await perspective!.ensureSDNASubjectClass(Comment);
                    await perspective!.ensureSDNASubjectClass(Article);
                    await perspective!.ensureSDNASubjectClass(ArticleWithString);

                    // Give perspective time to fully index the SDNA classes
                    await sleep(200);
                });

                it("should filter collection by type with class reference", async () => {
                    const articleRoot = Literal.from("Article for isInstance test").toUrl();
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

                    // Add delay to allow SPARQL to finish indexing
                    await sleep(1500);

                    // Add links to article
                    await perspective!.add(new Link({
                        source: articleRoot,
                        predicate: "article://has_comment",
                        target: validComment1
                    }));
                    await perspective!.add(new Link({
                        source: articleRoot,
                        predicate: "article://has_comment",
                        target: invalidItem
                    }));
                    await perspective!.add(new Link({
                        source: articleRoot,
                        predicate: "article://has_comment",
                        target: validComment2
                    }));

                    await sleep(500);

                    const retrievedArticle = new Article(perspective!, articleRoot);
                    await retrievedArticle.get();

                    // Should only contain valid Comments, not the invalid item
                    expect(retrievedArticle.comments).to.have.lengthOf(2);
                    expect(retrievedArticle.comments).to.include(validComment1);
                    expect(retrievedArticle.comments).to.include(validComment2);
                    expect(retrievedArticle.comments).to.not.include(invalidItem);
                });

                it("should filter collection by type with string class name", async () => {
                    const articleRoot = Literal.from("Article for string isInstance test").toUrl();
                    const validComment = Literal.from("Valid comment").toUrl();
                    const invalidItem = Literal.from("Invalid item").toUrl();

                    const article = new ArticleWithString(perspective!, articleRoot);
                    article.title = "Test Article with String";
                    await article.save();

                    // Create one valid comment
                    const comment = new Comment(perspective!, validComment);
                    comment.text = "Valid comment text";
                    await comment.save();

                    // Add delay to allow SPARQL to finish indexing
                    await sleep(1500);

                    // Add both to article
                    await perspective!.add(new Link({
                        source: articleRoot,
                        predicate: "article://has_comment",
                        target: validComment
                    }));
                    await perspective!.add(new Link({
                        source: articleRoot,
                        predicate: "article://has_comment",
                        target: invalidItem
                    }));

                    await sleep(500);

                    const retrievedArticle = new ArticleWithString(perspective!, articleRoot);
                    await retrievedArticle.get();

                    expect(retrievedArticle.comments).to.have.lengthOf(1);
                    expect(retrievedArticle.comments[0]).to.equal(validComment);
                });

                it("should filter results in findAll() by type", async () => {
                    // Create two articles
                    const article1Root = Literal.from("Article 1 for findAll isInstance").toUrl();
                    const article2Root = Literal.from("Article 2 for findAll isInstance").toUrl();
                    
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

                    // Add delay to allow SPARQL to finish indexing
                    await sleep(1500);

                    // Add comments to articles (mix of valid and invalid)
                    await perspective!.add(new Link({
                        source: article1Root,
                        predicate: "article://has_comment",
                        target: comment1
                    }));
                    await perspective!.add(new Link({
                        source: article1Root,
                        predicate: "article://has_comment",
                        target: invalid1
                    }));
                    await perspective!.add(new Link({
                        source: article2Root,
                        predicate: "article://has_comment",
                        target: comment2
                    }));
                    await perspective!.add(new Link({
                        source: article2Root,
                        predicate: "article://has_comment",
                        target: invalid2
                    }));

                    await sleep(500);

                    // Use findAll and verify filtering
                    const articles = await Article.findAll(perspective!);
                    
                    expect(articles).to.have.lengthOf(2);
                    
                    const foundArticle1 = articles.find(a => a.title === "Article 1");
                    const foundArticle2 = articles.find(a => a.title === "Article 2");
                    
                    expect(foundArticle1).to.not.be.undefined;
                    expect(foundArticle2).to.not.be.undefined;
                    
                    // Each article should only have valid comments
                    expect(foundArticle1!.comments).to.have.lengthOf(1);
                    expect(foundArticle1!.comments[0]).to.equal(comment1);
                    
                    expect(foundArticle2!.comments).to.have.lengthOf(1);
                    expect(foundArticle2!.comments[0]).to.equal(comment2);
                });
            })
        })
    })

    describe("Smart Literal", () => {
        let perspective: PerspectiveProxy | null = null

        before(async () => {
            perspective = await ad4m!.perspective.add("smart literal test")
            // for test debugging:
            //console.log("UUID: " + perspective.uuid)
        })

        it("can create and use a new smart literal", async () => {
            let sl = await SmartLiteral.create(perspective!, "Hello World")
            let base = sl.base

            expect(await sl.get()).to.equal("Hello World")

            let links = await perspective!.get(new LinkQuery({predicate: SMART_LITERAL_CONTENT_PREDICATE}))
            expect(links.length).to.equal(1)
            expect(links[0].data.source).to.equal(base)
            let literal = Literal.fromUrl(links[0].data.target)
            expect(literal.get()).to.equal("Hello World")

            await sl.set(5)
            expect(await sl.get()).to.equal(5)

            links = await perspective!.get(new LinkQuery({predicate: SMART_LITERAL_CONTENT_PREDICATE}))
            expect(links.length).to.equal(1)
            expect(links[0].data.source).to.equal(base)
            literal = Literal.fromUrl(links[0].data.target)
            expect(literal.get()).to.equal(5)
        })


        it("can instantiate smart literal from perspective", async () => {
            let source = Literal.from("base").toUrl()
            let target = Literal.from("Hello World 2").toUrl()
            await perspective!.add({source, predicate: SMART_LITERAL_CONTENT_PREDICATE, target})

            let sl = new SmartLiteral(perspective!, source)
            expect(await sl.get()).to.equal("Hello World 2")
        })

        it("can get all smart literals in a perspective",async () => {
            let all = await SmartLiteral.getAllSmartLiterals(perspective!)
            expect(all.length).to.equal(2)
            expect(all[1].base).to.equal(Literal.from("base").toUrl())
            expect(await all[0].get()).to.equal(5)
            expect(await all[1].get()).to.equal("Hello World 2")
        })

    })

    // SKIPPED: Embedding cache tests - only applies to Prolog-pooled mode
    // These tests verify embedding URL post-processing with Prolog infer() queries.
    // With SHACL migration, embedding queries should use SPARQL vector search instead.
    // Keeping as reference for future SPARQL vector embedding implementation.
    describe.skip('Embedding cache', () => {
        let perspective: PerspectiveProxy | null = null;
        const EMBEDDING_LANG = "QmzSYwdbqjGGbYbWJvdKA4WnuFwmMx3AsTfgg7EwbeNUGyE555c";

        before(async () => {
            perspective = await ad4m!.perspective.add("embedding-cache-test");
        });

        it('correctly post-processes nested query results containing embedding URLs', async () => {
            // Create some links with embedding URLs
            const embeddingUrl1 = `${EMBEDDING_LANG}://vector1/1.2,3.4,5.6`;
            const embeddingUrl2 = `${EMBEDDING_LANG}://vector2/7.8,9.0,1.2`;
            const embeddingUrl3 = `${EMBEDDING_LANG}://vector3/2.3,4.5,6.7`;

            // Create a link structure that will produce nested results
            await perspective!.add({
                source: "test://root",
                predicate: "test://has-vector",
                target: embeddingUrl1
            });

            await perspective!.add({
                source: embeddingUrl1,
                predicate: "test://related-to",
                target: embeddingUrl2
            });

            await perspective!.add({
                source: embeddingUrl2,
                predicate: "test://points-to",
                target: embeddingUrl3
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
            console.log("result", result)
            expect(result).to.be.an('array')
            expect(result.length).to.be.greaterThan(0)

            let binding = result[0]
            expect(binding.Results).to.be.an('array');
            expect(binding.Results).to.have.lengthOf(1);
            
            const [firstLevel] = binding.Results;
            expect(firstLevel).to.be.an('array');
            expect(firstLevel[0]).to.equal(embeddingUrl1);
            expect(firstLevel[1]).to.be.an('array');
            
            const relatedVectors = firstLevel[1];
            expect(relatedVectors).to.have.lengthOf(1);
            expect(relatedVectors[0]).to.be.an('array');
            expect(relatedVectors[0][0]).to.equal(embeddingUrl2);
            expect(relatedVectors[0][1]).to.equal(embeddingUrl3);
        });
    });

    describe("Ad4mModel.fromJSONSchema", () => {
        let perspective: PerspectiveProxy | null = null

        beforeEach(async () => {
            perspective = await ad4m!.perspective.add("json-schema-test")
        })

        describe("with explicit configuration", () => {
            it("should create Ad4mModel class from JSON Schema with explicit namespace", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "Person",
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" },
                        "age": { "type": "number" },
                        "email": { "type": "string" }
                    },
                    "required": ["name"]
                }

                const PersonClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "Person",
                    namespace: "person://",
                    resolveLanguage: "literal"
                })

                expect(PersonClass).to.be.a('function')
                // @ts-ignore - className is added dynamically
                expect(PersonClass.className).to.equal("Person")

                // Test instance creation
                const person = new PersonClass(perspective!)
                expect(person).to.be.instanceOf(Ad4mModel)
                expect(person.id).to.be.a('string')

                // Test property assignment
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.name = "Alice Johnson"
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.age = 30
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.email = "alice.johnson@example.com"

                await perspective!.ensureSDNASubjectClass(PersonClass)
                await person.save()

                // Create a second person to test multiple instances
                const person2 = new PersonClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                person2.name = "Bob Smith"
                // @ts-ignore - properties are added dynamically from JSON Schema
                person2.age = 25
                // @ts-ignore - properties are added dynamically from JSON Schema
                person2.email = "bob.smith@example.com"
                await person2.save()

                // Verify data was saved and can be retrieved
                const savedPeople = await PersonClass.findAll(perspective!)
                expect(savedPeople).to.have.lengthOf(2)
                
                // Find Alice
                // @ts-ignore - properties are added dynamically from JSON Schema
                const alice = savedPeople.find(p => p.name === "Alice Johnson")
                expect(alice).to.exist
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice!.name).to.equal("Alice Johnson")
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice!.age).to.equal(30)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice!.email).to.equal("alice.johnson@example.com")

                // Find Bob
                // @ts-ignore - properties are added dynamically from JSON Schema
                const bob = savedPeople.find(p => p.name === "Bob Smith")
                expect(bob).to.exist
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(bob!.age).to.equal(25)

                // Test querying with where clauses
                const adults = await PersonClass.findAll(perspective!, {
                    where: { age: { gt: 28 } }
                })
                expect(adults).to.have.lengthOf(1)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(adults[0].name).to.equal("Alice Johnson")
            })

            it("should support property mapping overrides", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "Contact",
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" },
                        "email": { "type": "string" }
                    },
                    "required": ["name"]
                }

                const ContactClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "Contact",
                    namespace: "contact://",
                    propertyMapping: {
                        "name": "foaf://name",
                        "email": "foaf://mbox"
                    },
                    resolveLanguage: "literal"
                })

                // @ts-ignore - className is added dynamically
                expect(ContactClass.className).to.equal("Contact")

                // Test that custom predicates are used
                const contact = new ContactClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                contact.name = "Bob Wilson"
                // @ts-ignore - properties are added dynamically from JSON Schema
                contact.email = "bob.wilson@company.com"

                await perspective!.ensureSDNASubjectClass(ContactClass)
                await contact.save()

                // Create second contact to test multiple instances
                const contact2 = new ContactClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                contact2.name = "Carol Davis"
                // @ts-ignore - properties are added dynamically from JSON Schema
                contact2.email = "carol.davis@company.com"
                await contact2.save()

                // Verify data retrieval works with custom predicates
                const savedContacts = await ContactClass.findAll(perspective!)
                expect(savedContacts).to.have.lengthOf(2)
                
                // @ts-ignore - properties are added dynamically from JSON Schema
                const bob = savedContacts.find(c => c.name === "Bob Wilson")
                expect(bob).to.exist
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(bob!.email).to.equal("bob.wilson@company.com")

                // Verify the custom predicates were used by checking the generated SDNA
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = ContactClass.generateSDNA()
                expect(sdna.sdna).to.include("foaf://name")
                expect(sdna.sdna).to.include("foaf://mbox")

                // Test querying works with custom predicates
                const bobQuery = await ContactClass.findAll(perspective!, {
                    where: { name: "Bob Wilson" }
                })
                expect(bobQuery).to.have.lengthOf(1)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(bobQuery[0].email).to.equal("bob.wilson@company.com")
            })
        })

        describe("with JSON Schema x-ad4m metadata", () => {
            it("should use x-ad4m metadata when available", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "Product",
                    "type": "object",
                    "x-ad4m": {
                        "namespace": "product://",
                        "className": "Product"
                    },
                    "properties": {
                        "name": { 
                            "type": "string",
                            "x-ad4m": {
                                "through": "product://title",
                                "resolveLanguage": "literal"
                            }
                        },
                        "price": { 
                            "type": "number",
                            "x-ad4m": {
                                "through": "product://cost"
                            }
                        },
                        "description": { 
                            "type": "string",
                            "x-ad4m": {
                                "resolveLanguage": "literal"
                            }
                        }
                    },
                    "required": ["name"]
                }

                const ProductClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "ProductOverride" // This should take precedence
                })

                // @ts-ignore - className is added dynamically
                expect(ProductClass.className).to.equal("ProductOverride")

                const product = new ProductClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                product.name = "Gaming Laptop"
                // @ts-ignore - properties are added dynamically from JSON Schema
                product.price = 1299.99
                // @ts-ignore - properties are added dynamically from JSON Schema
                product.description = "A high-performance gaming laptop with RTX graphics"

                await perspective!.ensureSDNASubjectClass(ProductClass)
                await product.save()

                // Create a second product with different pricing
                const product2 = new ProductClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                product2.name = "Office Laptop"
                // @ts-ignore - properties are added dynamically from JSON Schema
                product2.price = 799.99
                // @ts-ignore - properties are added dynamically from JSON Schema
                product2.description = "A reliable laptop for office work"
                await product2.save()

                // Test data retrieval and validation
                const savedProducts = await ProductClass.findAll(perspective!)
                expect(savedProducts).to.have.lengthOf(2)

                // Verify x-ad4m custom predicates work for data retrieval
                // @ts-ignore - properties are added dynamically from JSON Schema
                const gamingLaptop = savedProducts.find(p => p.name === "Gaming Laptop")
                expect(gamingLaptop).to.exist
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(gamingLaptop!.price).to.equal(1299.99)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(gamingLaptop!.description).to.equal("A high-performance gaming laptop with RTX graphics")

                // Test querying with price ranges
                const expensiveProducts = await ProductClass.findAll(perspective!, {
                    where: { price: { gt: 1000 } }
                })
                expect(expensiveProducts).to.have.lengthOf(1)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(expensiveProducts[0].name).to.equal("Gaming Laptop")

                // Verify custom predicates from x-ad4m were used
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = ProductClass.generateSDNA()
                expect(sdna.sdna).to.include("product://title") // custom predicate for name
                expect(sdna.sdna).to.include("product://cost")  // custom predicate for price
                expect(sdna.sdna).to.include("product://description") // inferred from namespace + property
            })
        })

        describe("with title-based inference", () => {
            it("should infer namespace from schema title when no explicit config", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "Book",
                    "type": "object",
                    "properties": {
                        "title": { "type": "string" },
                        // Avoid reserved top-level "author" which conflicts with Ad4mModel built-in
                        "writer": { "type": "string" },
                        "isbn": { "type": "string" }
                    },
                    "required": ["title"]
                }

                const BookClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "Book",
                    resolveLanguage: "literal"
                })

                // @ts-ignore - className is added dynamically
                expect(BookClass.className).to.equal("Book")

                const book = new BookClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                book.title = "The Great Gatsby"
                // @ts-ignore - properties are added dynamically from JSON Schema
                // @ts-ignore - properties are added dynamically from JSON Schema
                book.writer = "F. Scott Fitzgerald"
                // @ts-ignore - properties are added dynamically from JSON Schema
                book.isbn = "978-0-7432-7356-5"

                await perspective!.ensureSDNASubjectClass(BookClass)
                await book.save()

                // Add a second book
                const book2 = new BookClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                book2.title = "To Kill a Mockingbird"
                // @ts-ignore - properties are added dynamically from JSON Schema
                // @ts-ignore - properties are added dynamically from JSON Schema
                book2.writer = "Harper Lee"
                // @ts-ignore - properties are added dynamically from JSON Schema
                book2.isbn = "978-0-06-112008-4"
                await book2.save()

                // Test data retrieval with inferred predicates
                const savedBooks = await BookClass.findAll(perspective!)
                expect(savedBooks).to.have.lengthOf(2)

                // @ts-ignore - properties are added dynamically from JSON Schema
                // @ts-ignore - properties are added dynamically from JSON Schema
                const gatsby = savedBooks.find(b => b.title === "The Great Gatsby")
                expect(gatsby).to.exist
                // @ts-ignore - properties are added dynamically from JSON Schema
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(gatsby!.writer).to.equal("F. Scott Fitzgerald")
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(gatsby!.isbn).to.equal("978-0-7432-7356-5")

                // Test querying by author
                const fitzgeraldBooks = await BookClass.findAll(perspective!, {
                    where: { writer: "F. Scott Fitzgerald" }
                })
                expect(fitzgeraldBooks).to.have.lengthOf(1)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(fitzgeraldBooks[0].title).to.equal("The Great Gatsby")

                // Verify inferred predicates (should be book://title, book://author, etc.)
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = BookClass.generateSDNA()
                expect(sdna.sdna).to.include("book://title")
                expect(sdna.sdna).to.include("book://writer")
                expect(sdna.sdna).to.include("book://isbn")
            })
        })

        describe("error handling", () => {
            it("should throw error when no title and no namespace provided", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "type": "object",
                    "properties": {
                        "value": { "type": "string" }
                    },
                    "required": ["value"]  // Add required property to avoid constructor error
                }

                expect(() => {
                    Ad4mModel.fromJSONSchema(schema, { name: "Test" })
                }).to.throw(/Cannot infer namespace/)
            })

            it("should handle all-optional properties without auto-generating a type flag", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "OptionalOnly",
                    "type": "object",
                    "properties": {
                        "optionalValue": { "type": "string" },
                        "anotherOptional": { "type": "number" }
                    }
                    // No required array - all properties are optional
                }

                // Should not throw error — open-world structural matching applies
                const OptionalClass = Ad4mModel.fromJSONSchema(schema, { 
                    name: "OptionalOnly",
                    namespace: "test://" 
                });

                expect(OptionalClass).to.be.a('function')
                // @ts-ignore - className is added dynamically
                expect(OptionalClass.className).to.equal("OptionalOnly")

                // Should NOT have automatic type flag
                const instance = new OptionalClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(instance.__ad4m_type).to.be.undefined

                // Verify SDNA does NOT include ad4m://type auto-flag
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = OptionalClass.generateSDNA()
                expect(sdna.sdna).to.not.include('ad4m://type')
            })

            it("should work when properties have explicit initial values even if not required", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "WithInitials",
                    "type": "object",
                    "properties": {
                        "status": { "type": "string" },
                        "count": { "type": "number" }
                    }
                    // No required array, but we'll provide initial values
                }

                // This should work because we provide initial values
                const TestClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "WithInitials",
                    namespace: "test://",
                    propertyOptions: {
                        "status": { initial: "test://active" },
                        "count": { initial: "literal:number:0" }
                    }
                })

                expect(TestClass).to.be.a('function')
                // @ts-ignore - className is added dynamically
                expect(TestClass.className).to.equal("WithInitials")

                // Verify SDNA has constructor actions
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = TestClass.generateSDNA()
                expect(sdna.sdna).to.include('constructor(')
                expect(sdna.sdna).to.include('test://active')
                expect(sdna.sdna).to.include('literal:number:0')
            })

            it("should handle complex property types with full data storage and retrieval", async () => {
                const schema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "BlogPost",
                    "type": "object",
                    "properties": {
                        "title": { "type": "string" },
                        "tags": { 
                            "type": "array",
                            "items": { "type": "string" }
                        },
                        "metadata": { 
                            "type": "object",
                            "properties": {
                                "created": { "type": "string" },
                                "author": { "type": "string" },
                                "views": { "type": "number" }
                            }
                        },
                        "categories": {
                            "type": "array",
                            "items": { "type": "string" }
                        }
                    },
                    "required": ["title"]
                }

                const BlogPostClass = Ad4mModel.fromJSONSchema(schema, {
                    name: "BlogPost",
                    resolveLanguage: "literal"
                })

                // @ts-ignore - className is added dynamically
                expect(BlogPostClass.className).to.equal("BlogPost")

                await perspective!.ensureSDNASubjectClass(BlogPostClass)

                // Create a blog post with complex data
                const post1 = new BlogPostClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                post1.title = "Getting Started with AD4M"
                
                // Test array/collection handling
                // @ts-ignore - properties are added dynamically from JSON Schema
                post1.tags = ["tag://ad4m", "tag://tutorial", "tag://blockchain"]
                // @ts-ignore - properties are added dynamically from JSON Schema
                post1.categories = ["category://technology", "category://development"]
                
                // Test complex object handling (should be stored as JSON)
                // @ts-ignore - properties are added dynamically from JSON Schema
                post1.metadata = {
                    created: "2025-09-22T10:00:00Z",
                    author: "Alice",
                    views: 42
                }
                
                await post1.save()

                // Create a second post
                const post2 = new BlogPostClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                post2.title = "Advanced AD4M Patterns"
                // @ts-ignore - properties are added dynamically from JSON Schema
                post2.tags = ["tag://ad4m", "tag://advanced", "tag://patterns"]
                // @ts-ignore - properties are added dynamically from JSON Schema
                post2.categories = ["category://technology"]
                // @ts-ignore - properties are added dynamically from JSON Schema
                post2.metadata = {
                    created: "2025-09-22T11:00:00Z",
                    author: "Bob",
                    views: 15
                }
                await post2.save()

                // Test data retrieval
                const savedPosts = await BlogPostClass.findAll(perspective!)
                expect(savedPosts).to.have.lengthOf(2)

                // Verify complex object data is preserved
                // @ts-ignore - properties are added dynamically from JSON Schema
                const tutorialPost = savedPosts.find(p => p.title === "Getting Started with AD4M")
                expect(tutorialPost).to.exist
                
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.tags).to.be.an('array')
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.tags).to.include.members(["tag://ad4m", "tag://tutorial", "tag://blockchain"])
                
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.metadata).to.be.an('object')
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.metadata.author).to.equal("Alice")
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.metadata.views).to.equal(42)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(tutorialPost!.metadata.created).to.equal("2025-09-22T10:00:00Z")

                // Test querying by title
                const advancedPosts = await BlogPostClass.findAll(perspective!, {
                    where: { title: "Advanced AD4M Patterns" }
                })
                expect(advancedPosts).to.have.lengthOf(1)
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(advancedPosts[0].metadata.author).to.equal("Bob")

                // Verify SDNA structure for complex types
                // @ts-ignore - generateSDNA is added dynamically
                const sdna = BlogPostClass.generateSDNA()
                expect(sdna.sdna).to.include('collection(') // tags and categories should be collections
                expect(sdna.sdna).to.include('property(') // title and metadata should be properties
                expect(sdna.sdna).to.include('blogpost://title')
                expect(sdna.sdna).to.include('blogpost://tags')
                expect(sdna.sdna).to.include('blogpost://metadata')
                expect(sdna.sdna).to.include('blogpost://categories')
            })

            it("should handle realistic Holon-like schema with nested objects", async () => {
                const holonSchema = {
                    "$schema": "http://json-schema.org/draft-07/schema#",
                    "title": "PersonHolon",
                    "type": "object",
                    "properties": {
                        "name": { "type": "string" },
                        "email": { "type": "string" },
                        "profile": {
                            "type": "object",
                            "properties": {
                                "bio": { "type": "string" },
                                "location": { "type": "string" }
                            }
                        },
                        "skills": {
                            "type": "array", 
                            "items": { "type": "string" }
                        }
                    },
                    "required": ["name", "email"]
                }

                const PersonHolonClass = Ad4mModel.fromJSONSchema(holonSchema, {
                    name: "PersonHolon",
                    namespace: "holon://person/",
                    resolveLanguage: "literal"
                })


                await perspective!.ensureSDNASubjectClass(PersonHolonClass)

                // Test with realistic data
                const person = new PersonHolonClass(perspective!)
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.name = "Alice Cooper"
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.email = "alice@example.com"
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.skills = ["skill://javascript", "skill://typescript", "skill://ad4m"]
                // @ts-ignore - properties are added dynamically from JSON Schema
                person.profile = {
                    bio: "Software developer passionate about decentralized systems",
                    location: "San Francisco"
                }
                await person.save()

                // Verify retrieval preserves nested structure
                const retrieved = await PersonHolonClass.findAll(perspective!)
                expect(retrieved).to.have.lengthOf(1)
                
                const alice = retrieved[0]
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice.profile).to.be.an('object')
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice.profile.bio).to.equal("Software developer passionate about decentralized systems")
                // @ts-ignore - properties are added dynamically from JSON Schema
                expect(alice.skills).to.include.members(["skill://javascript", "skill://typescript", "skill://ad4m"])
            })
        })
    })

})

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Wait for a condition to become true with exponential backoff.
 * This is more reliable than fixed sleep() for async operations.
 */
async function waitForCondition(
  condition: () => boolean,
  options: { 
    timeoutMs?: number, 
    checkIntervalMs?: number,
    errorMessage?: string 
  } = {}
): Promise<void> {
  const { 
    timeoutMs = 5000, 
    checkIntervalMs = 50,
    errorMessage = 'Condition was not met within timeout'
  } = options;
  
  const startTime = Date.now();
  
  while (!condition()) {
    if (Date.now() - startTime > timeoutMs) {
      throw new Error(`${errorMessage} (timeout: ${timeoutMs}ms)`);
    }
    await sleep(checkIntervalMs);
  }
}