import { expect } from "chai";
import { ChildProcess } from 'node:child_process';
import { Ad4mClient, Link, LinkQuery, Literal, PerspectiveProxy,
    SmartLiteral, SMART_LITERAL_CONTENT_PREDICATE,
    InstanceQuery, Subject,
    Ad4mModel,
    Flag,
    Property,
    ReadOnly,
    Collection,
    ModelOptions,
    Optional,
    PropertyOptions,
} from "@coasys/ad4m";
import { readFileSync } from "node:fs";
import { startExecutor, apolloClient } from "../utils/utils";
import path from "path";
import { fileURLToPath } from 'url';
import fetch from 'node-fetch'

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
    const gqlPort = 16600
    const hcAdminPort = 16601
    const hcAppPort = 16602

    before(async () => {
        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
            gqlPort, hcAdminPort, hcAppPort);

        console.log("Creating ad4m client")
        ad4m = new Ad4mClient(apolloClient(gqlPort))
        console.log("Generating agent")
        await ad4m.agent.generate("secret")
        console.log("Done")
    })

    after(async () => {
        if (executorProcess) {
            while (!executorProcess?.killed) {
                let status  = executorProcess?.kill();
                console.log("killed executor with", status);
                await sleep(500);
            }
        }
    })

    it("should get agent status", async () => {
        let result = await ad4m!.agent.status()
        expect(result).to.not.be.null
        expect(result!.isInitialized).to.be.true
    })

    describe("Subjects", () => {
        let perspective: PerspectiveProxy | null = null

        before(async () => {
            perspective = await ad4m!.perspective.add("test")
            // for test debugging:
            //console.log("UUID: " + perspective.uuid)

            let classes = await perspective.subjectClasses();
            expect(classes.length).to.equal(0)

            let sdna = readFileSync("./sdna/subject.pl").toString()
            await perspective.addSdna("Todo", sdna, "subject_class")

            let retrievedSdna = await perspective.getSdna()
            expect(retrievedSdna).to.deep.equal([sdna])
        })

        it("should find the TODO subject class from the test SDNA", async () => {
            let classes = await perspective!.subjectClasses();

            expect(classes.length).to.equal(1)
            expect(classes[0]).to.equal("Todo")
        })

        it("should be able to construct a subject instance from a literal", async () => {
            let root = Literal.from("construct test").toUrl()
            expect(await perspective!.createSubject("Todo", root)).to.not.be.undefined
            expect(await perspective!.isSubjectInstance(root, "Todo")).to.not.be.false
        })

        it("can get subject instance proxy via class string", async () => {
            let root = Literal.from("get proxy test").toUrl()
            await perspective!.createSubject("Todo", root)
            let subject = await perspective!.getSubjectProxy(root, "Todo") as unknown as Subject
            expect(subject).to.not.be.undefined
            expect(subject).to.have.property("state")
            expect(subject).to.have.property("setState")
            expect(subject).to.have.property("title")
        })

        describe("with an instance", () => {
            let subject: Subject | null = null

            before(async () => {
                let root = Literal.from("construct test").toUrl()
                subject = await perspective!.createSubject("Todo", root) as unknown as Subject
            })

            it("should be able to read a property as JS property", async () => {
                //@ts-ignore
                expect(await subject.state).to.equal("todo://ready")
            })

            it("should be able to set a property with JS setter method", async () => {
                //@ts-ignore
                await subject.setState("todo://done")
                //@ts-ignore
                expect(await subject.state).to.equal("todo://done")
            })

            it("should work with a property that is not set initially and that auto-resolves", async () => {
                //@ts-ignore
                expect(await subject.title).to.be.undefined

                let title = "test title"
                //@ts-ignore
                await subject.setTitle(title)
                //@ts-ignore
                expect(await subject.title).to.equal(title)
            })

            it("should be able to get collections as arrays", async () => {
                //@ts-ignore
                expect(await subject.comments).to.be.an("array")
                //@ts-ignore
                expect(await subject.comments).to.be.empty

                let c1 = Literal.from("comment 1").toUrl()
                await perspective!.add(new Link({
                    source: subject!.baseExpression,
                    predicate: "todo://comment",
                    target: c1
                }))

                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1])

                let c2 = Literal.from("comment 2").toUrl()
                await perspective!.add(new Link({
                    source: subject!.baseExpression,
                    predicate: "todo://comment",
                    target: c2
                }))

                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1, c2])
            })

            it("should be able to add to collections", async () => {
                let commentLinks = await perspective!.get(new LinkQuery({
                    source: subject!.baseExpression,
                    predicate: "todo://comment"
                }))
                for(let link of commentLinks) {
                    await perspective!.remove(link)
                }

                //@ts-ignore
                expect(await subject.comments).to.be.empty

                let c1 = Literal.from("new comment 1").toUrl()
                let c2 = Literal.from("new comment 2").toUrl()

                //@ts-ignore
                await subject.addComments(c1)
                await sleep(100)
                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1])

                //@ts-ignore
                await subject.addComments(c2)
                await sleep(100)
                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1, c2])
            })

            it("should be able to get all subject instance of a given class", async () => {
                let todos = await perspective!.getAllSubjectInstances("Todo") as unknown as Subject[]
                expect(todos.length).to.equal(2)
                //@ts-ignore
                expect(await todos[1].state).to.exist
            })

            it("should create a subject with initial values", async () => {
                let root = Literal.from("initial values test").toUrl()
                const initialValues = {
                    title: "Initial Title",
                    state: "todo://done"
                }
                await perspective!.createSubject("Todo", root, initialValues)
                let subject = await perspective!.getSubjectProxy(root, "Todo") as unknown as Subject

                //@ts-ignore
                expect(await subject.title).to.equal("Initial Title")
                //@ts-ignore
                expect(await subject.state).to.equal("todo://done")
            })
        })

        describe("TypeScript compatibility", () => {

            // This class mathces the SDNA in ./sdna/subject.pl
            class Todo {
                state: string = ""
                title: string = ""
                comments: string[] = []

                setState(state: string) {}
                setTitle(title: string) {}
                addComments(comment: string) {}
                setCollectionComments(comment: string) {}
            }

            // This class doesn not match the SDNA in ./sdna/subject.pl
            class UnknownSubject {
                name: string = ""
                x: string = ""

                setTop(top: string) {}
            }

            // This class is like Todo, but has a setter that
            // is not defined in the SDNA (-> should not match)
            class AlmostTodo {
                state: string = ""
                title: string = ""
                comments: string[] = []

                setState(state: string) {}
                setTitle(title: string) {}
                addComment(comment: string) {}
                setTop(top: string) {}
            }

            let todo: Todo = new Todo()
            let unknown: UnknownSubject = new UnknownSubject()
            let almostTodo: AlmostTodo = new AlmostTodo()

            it("can find subject classes mapping to JS objects", async () => {
                let todoClasses = await perspective!.subjectClassesByTemplate(todo)
                expect(todoClasses).to.include("Todo")
                expect(todoClasses.length).to.equal(1)

                let unknownClasses = await perspective!.subjectClassesByTemplate(unknown)
                expect(unknownClasses).to.be.empty

                let almostTodoClasses = await perspective!.subjectClassesByTemplate(almostTodo)
                expect(almostTodoClasses).to.be.empty
            })

            it("can find subject and create instances in a type-safe way", async () => {
                // PerspectiveProxe.getAllSubjectInstances() is a generic that returns
                // an array of the given type.
                let todos = await perspective!.getAllSubjectInstances(todo)

                // todos is an array of Todo objects
                // note how we don't need @ts-ignore here:
                expect(todos.length).to.equal(3)
                expect(await todos[1].state).to.exist
            })

        })

        describe("SDNA creation decorators", () => {
            @ModelOptions({
                name: "Message"
            })
            class Message {
                @Flag({
                    through: "ad4m://type",
                    value: "ad4m://message"
                })
                type: string = ""

                @InstanceQuery()
                static async all(perspective: PerspectiveProxy): Promise<Message[]> { return [] }

                @Optional({
                    through: "todo://state",
                    initial: "todo://ready",
                })
                body: string = ""
            }

            // This class matches the SDNA in ./sdna/subject.pl
            // and this test proves the decorators create the exact same SDNA code
            @ModelOptions({
                name: "Todo"
            })
            class Todo {
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

                //@ts-ignore
                @InstanceQuery()
                static async all(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @InstanceQuery({where: {state: "todo://ready"}})
                static async allReady(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @InstanceQuery({where: { state: "todo://done" }})
                static async allDone(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @InstanceQuery({condition: 'triple("ad4m://self", _, Instance)'})
                static async allSelf(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                //@ts-ignore
                @Property({
                    through: "todo://state",
                    initial: "todo://ready",
                })
                state: string = ""

                @Optional({
                    through: "todo://has_title",
                    writable: true,
                    resolveLanguage: "literal"
                })
                title: string = ""

                @ReadOnly({
                    getter: `triple(Base, "flux://has_reaction", "flux://thumbsup"), Value = true`
                })
                isLiked: boolean = false

                @Collection({ through: "todo://comment" })
                comments: string[] = []

                @Collection({ through: "flux://entry_type" })
                entries: string[] = []

                @Collection({
                    through: "flux://entry_type",
                    where: { isInstance: Message }
                })
                messages: string[] = []

                @Collection({
                    through: "flux://entry_type",
                    where: { condition: `triple(Target, "flux://has_reaction", "flux://thumbsup")` }
                })
                likedMessages: string[] = []
            }

            it("should generate correct SDNA from a JS class", async () => {
                // @ts-ignore
                const { name, sdna } = Todo.generateSDNA();

                const regExp = /\("Todo", ([^)]+)\)/;
                const matches = regExp.exec(sdna);
                const value = matches![1];

                const equal = readFileSync("./sdna/subject.pl").toString().replace(/c\)/g, `${value})`).replace(/\(c/g, `(${value}`);

                expect(sdna.normalize('NFC')).to.equal(equal.normalize('NFC'))
            })

            it("should be possible to use that class for type-safe interaction with subject instances", async () => {
                // construct new subject intance
                let root = Literal.from("Decorated class construction test").toUrl()
                // get instance with type information
                let todo = await perspective!.createSubject(new Todo(), root)

                expect(await perspective!.isSubjectInstance(root, new Todo())).to.not.be.false
                let todo2 = await perspective!.getSubjectProxy(root, new Todo())
                expect(todo2).to.have.property("state")
                expect(todo2).to.have.property("title")
                expect(todo2).to.have.property("comments")
                // @ts-ignore
                await todo.setState("todo://review")
                await sleep(1000)
                expect(await todo.state).to.equal("todo://review")
                expect(await todo.comments).to.be.empty

                let comment = Literal.from("new comment").toUrl()
                // @ts-ignore
                await todo.addComments(comment)
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

            it("can retrieve matching instance through InstanceQuery(condition: ..)", async () => {
                let todos = await Todo.allSelf(perspective!)
                expect(todos.length).to.equal(0)

                todos = await Todo.all(perspective!)
                let todo = todos[0]
                //@ts-ignore
                await perspective!.add(new Link({source: "ad4m://self", target: todo.baseExpression}))

                todos = await Todo.allSelf(perspective!)
                expect(todos.length).to.equal(1)
            })

            it("can deal with properties that resolve the URI and create Expressions", async () => {
                let todos = await Todo.all(perspective!)
                let todo = todos[0]
                expect(await todo.title).to.be.undefined

                // @ts-ignore
                await todo.setTitle("new title")
                expect(await todo.title).to.equal("new title")

                //@ts-ignore
                let links = await perspective!.get(new LinkQuery({source: todo.baseExpression, predicate: "todo://has_title"}))
                expect(links.length).to.equal(1)
                let literal = Literal.fromUrl(links[0].data.target).get()
                expect(literal.data).to.equal("new title")
            })

            it("can easily be initialized with PerspectiveProxy.ensureSDNASubjectClass()", async () => {
                expect(await perspective!.getSdna()).to.have.lengthOf(1)

                @ModelOptions({
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

            it("can constrain collection entries through 'where' clause with prolog condition", async () => {
                let root = Literal.from("Collection where test with prolog condition").toUrl()
                let todo = await perspective!.createSubject(new Todo(), root)

                let messageEntry = Literal.from("test message").toUrl()

                // @ts-ignore
                await todo.addEntries(messageEntry)

                let entries = await todo.entries
                expect(entries.length).to.equal(1)

                let messageEntries = await todo.likedMessages
                expect(messageEntries.length).to.equal(0)

                await perspective?.add(new Link({source: messageEntry, predicate: "flux://has_reaction", target: "flux://thumbsup"}))

                messageEntries = await todo.likedMessages
                expect(messageEntries.length).to.equal(1)
            })

            it("can use properties with custom getter prolog code", async () => {
                let root = Literal.from("Custom getter test").toUrl()
                let todo = await perspective!.createSubject(new Todo(), root)

                // @ts-ignore
                const liked1 = await todo.isLiked
                expect(liked1).to.be.undefined

                await perspective?.add(new Link({source: root, predicate: "flux://has_reaction", target: "flux://thumbsup"}))

                // @ts-ignore
                const liked2 = await todo.isLiked
                expect(liked2).to.be.true
            })

            describe("with Message subject class registered", () => {
                before(async () => {
                    // @ts-ignore
                    const { name, sdna } = Message.generateSDNA();
                    await perspective!.addSdna(name, sdna, "subject_class")
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
                    let todo = await perspective!.createSubject(new Todo(), root)

                    let messageEntry = Literal.from("test message").toUrl()

                    // @ts-ignore
                    await todo.addEntries(messageEntry)

                    let entries = await todo.entries
                    expect(entries.length).to.equal(1)

                    let messageEntries = await todo.messages
                    expect(messageEntries.length).to.equal(0)

                    await perspective!.createSubject(new Message(), messageEntry)

                    messageEntries = await todo.messages
                    expect(messageEntries.length).to.equal(1)
                })

            })

            describe("Active record implementation", () => {
                @ModelOptions({
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

                    @Collection({ through: "recipe://entries" })
                    entries: string[] = []

                    @Collection({
                        through: "recipe://entries",
                        where: { condition: `triple(Target, "recipe://has_ingredient", "recipe://test")` }
                    })
                    ingredients: string[] = []

                    @Collection({ through: "recipe://comment" })
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
                }

                before(async () => {
                    // @ts-ignore
                    const { name, sdna } = Recipe.generateSDNA();
                    perspective!.addSdna(name, sdna, 'subject_class')
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

                    await recipe.update();

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.name).to.equal("Update test")
                    expect(recipe2.plain).to.equal("recipe://update_test")
                })

                it("find()", async () => {
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
                    const recipes = await Recipe.findAll(perspective!);

                    expect(recipes.length).to.equal(3)

                    await recipes[0].delete();

                    const updatedRecipies = await Recipe.findAll(perspective!);

                    expect(updatedRecipies.length).to.equal(2)
                })

                it("can constrain collection entries through 'where' clause with prolog condition", async () => {
                    let root = Literal.from("Active record implementation collection test with where").toUrl();
                    const recipe = new Recipe(perspective!, root);

                    let recipeEntries = Literal.from("test recipes").toUrl();

                    recipe.entries = [recipeEntries];
                    // @ts-ignore
                    recipe.comments = ['recipe://test', 'recipe://test1'];
                    recipe.name = "Collection test";

                    await recipe.save();

                    await perspective?.add(new Link({source: recipeEntries, predicate: "recipe://has_ingredient", target: "recipe://test"}));

                    await recipe.get();

                    const recipe2 = new Recipe(perspective!, root);

                    await recipe2.get();

                    expect(recipe2.ingredients.length).to.equal(1);
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

                it("works with very long property values", async() => {
                    let root = Literal.from("Active record implementation test long value").toUrl()
                    const recipe = new Recipe(perspective!, root)

                    const longName = "This is a very long recipe name that goes on and on with many many characters to test that we can handle long property values without any issues whatsoever and keep going even longer to make absolutely sure we hit at least 300 characters in this test string that just keeps getting longer and longer until we are completely satisfied that it works properly with such lengthy content. But wait, there's more! We need to make this string even longer to properly test the system's ability to handle extremely long property values. Let's add some more meaningful content about recipes - ingredients like flour, sugar, eggs, milk, butter, vanilla extract, baking powder, salt, and detailed instructions for mixing them together in just the right way to create the perfect baked goods. We could go on about preheating the oven to the right temperature, greasing the pans properly, checking for doneness with a toothpick, and letting things cool completely before frosting. The possibilities are endless when it comes to recipe details and instructions that could make this string longer and longer. We want to be absolutely certain that our system can handle property values of any reasonable length without truncating or corrupting the data in any way. This is especially important for recipes where precise instructions and ingredient amounts can make the difference between success and failure in the kitchen. Testing with realistically long content helps ensure our system works reliably in real-world usage scenarios where users might enter detailed information that extends well beyond a few simple sentences."
                    recipe.plain = longName
                    recipe.resolve = longName

                    await recipe.save()

                    let linksName = await perspective!.get(new LinkQuery({source: root, predicate: "recipe://plain"}))
                    expect(linksName.length).to.equal(1)
                    expect(linksName[0].data.target).to.equal(longName)

                    let linksResolve = await perspective!.get(new LinkQuery({source: root, predicate: "recipe://resolve"}))
                    expect(linksResolve.length).to.equal(1)
                    let expression = Literal.fromUrl(linksResolve[0].data.target).get()
                    expect(expression.data).to.equal(longName)

                    const recipe2 = new Recipe(perspective!, root)
                    await recipe2.get()

                    expect(recipe2.plain.length).to.equal(longName.length)
                    expect(recipe2.plain).to.equal(longName)

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
                    expect(data.comments).to.deep.equal(['recipe://comment1', 'recipe://comment2']);
                    expect(data.local).to.equal("recipe://local_test");
                    expect(data.resolve).to.equal("Resolved literal value");

                    await recipe.delete();
                })

                it("findAll() returns properties on instances", async () => {
                    // Clear all previous recipes
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) await recipe.delete();
                    
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

                    // Test findAll
                    const recipes = await Recipe.findAll(perspective!);

                    expect(recipes.length).to.equal(2);
                    expect(recipes[0].name).to.equal("findAll test 1");
                    expect(recipes[0].resolve).to.equal("Resolved literal value 1");
                    expect(recipes[1].name).to.equal("findAll test 2");
                    expect(recipes[1].resolve).to.equal("Resolved literal value 2");
                    expect(recipes[0].plain).to.equal("recipe://findAll_test1");
                    expect(recipes[1].plain).to.equal("recipe://findAll_test2");

                    await recipe1.delete();
                    await recipe2.delete();
                })

                it("findAll() returns collections on instances", async () => {
                    // Clear all previous recipes
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) await recipe.delete();
                    
                    let root1 = Literal.from("findAll test 1").toUrl()
                    let root2 = Literal.from("findAll test 2").toUrl()
                    
                    const recipe1 = new Recipe(perspective!, root1)
                    recipe1.comments = ["Recipe 1: Comment 1", "Recipe 1: Comment 2"];
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!, root2)
                    recipe2.comments = ["Recipe 2: Comment 1", "Recipe 2: Comment 2"];
                    await recipe2.save();

                    // Test findAll
                    const recipes = await Recipe.findAll(perspective!);

                    expect(recipes.length).to.equal(2);
                    expect(recipes[0].comments.length).to.equal(2);
                    expect(recipes[0].comments[0]).to.equal("Recipe 1: Comment 1");
                    expect(recipes[0].comments[1]).to.equal("Recipe 1: Comment 2");

                    expect(recipes[1].comments.length).to.equal(2);
                    expect(recipes[1].comments[0]).to.equal("Recipe 2: Comment 1");
                    expect(recipes[1].comments[1]).to.equal("Recipe 2: Comment 2");

                    await recipe1.delete();
                    await recipe2.delete();
                })

                it("findAll() returns author & timestamp on instances", async () => {
                    // Clear all previous recipes
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) await recipe.delete();
                    
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

                    await recipe1.delete();
                    await recipe2.delete();
                })

                it("findAll() works with source prop", async () => {
                    // Clear all previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();

                    const source1 = Literal.from("Source 1").toUrl()
                    const source2 = Literal.from("Source 2").toUrl()
                    
                    const recipe1 = new Recipe(perspective!, undefined, source1)
                    recipe1.name = "Recipe 1: Name";
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!, undefined, source2)
                    recipe2.name = "Recipe 2: Name";
                    await recipe2.save();

                    const recipe3 = new Recipe(perspective!, undefined, source2)
                    recipe3.name = "Recipe 3: Name";
                    await recipe3.save();

                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(3);

                    const source1Recipes = await Recipe.findAll(perspective!, { source: source1 });
                    expect(source1Recipes.length).to.equal(1);
                    expect(source1Recipes[0].name).to.equal("Recipe 1: Name");

                    const source2Recipes = await Recipe.findAll(perspective!, { source: source2 });
                    expect(source2Recipes.length).to.equal(2);

                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                })

                it("findAll() works with properties query", async () => {
                    // Clear all previous recipes
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) await recipe.delete();
                    
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

                    await recipe.delete();
                })

                it("findAll() works with collections query", async () => {
                    // Clear all previous recipes
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) await recipe.delete();
                    
                    let root = Literal.from("findAll test 1").toUrl()
                    const recipe = new Recipe(perspective!, root);
                    recipe.comments = ["Recipe 1: Comment 1", "Recipe 1: Comment 2"];
                    recipe.entries = ["Recipe 1: Entry 1", "Recipe 1: Entry 2"];
                    await recipe.save();

                    // Test recipes with all collections
                    const recipesWithAllCollections = await Recipe.findAll(perspective!);
                    expect(recipesWithAllCollections[0].comments.length).to.equal(2)
                    expect(recipesWithAllCollections[0].entries.length).to.equal(2)
                    
                    // Test recipes with comments only
                    const recipesWithCommentsOnly = await Recipe.findAll(perspective!, { collections: ["comments"] });
                    expect(recipesWithCommentsOnly[0].comments.length).to.equal(2)
                    expect(recipesWithCommentsOnly[0].entries).to.be.undefined

                    // Test recipes with entries only
                    const recipesWithEntriesOnly = await Recipe.findAll(perspective!, { collections: ["entries"] });
                    expect(recipesWithEntriesOnly[0].comments).to.be.undefined
                    expect(recipesWithEntriesOnly[0].entries.length).to.equal(2)

                    await recipe.delete();
                })

                it("findAll() works with basic where queries", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                })

                it("findAll() works with where query not operations", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                })

                it("findAll() works with where query lt, lte, gt, & gte operations", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();

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
                    const recipe2timestamp = parseInt(allRecipes[1].timestamp);
                    
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

                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                    await recipe4.delete();
                })

                it("findAll() works with where query between operations", async () => {
                    @ModelOptions({
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
                            writable: true,
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

                    // Clear any previous tasks
                    let tasks = await TaskDue.findAll(perspective!);
                    for (const task of tasks) await task.delete();

                    // Create timestamps & tasks
                    const start = new Date().getTime();

                    const task1 = new TaskDue(perspective!);
                    task1.title = "Low priority task";
                    task1.priority = 2;
                    task1.dueDate = start;
                    await task1.save();

                    await sleep(2000);

                    const mid = new Date().getTime();

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

                    await sleep(2000);

                    const end = new Date().getTime();

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
                    
                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                })

                it("findAll() works with limit and offset", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    // Delete recipies
                    for (const recipe of allRecipes) await recipe.delete();
                })

                it("findAll() works with a mix of query constraints", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
                    // Create recipies
                    const recipe1 = new Recipe(perspective!);
                    recipe1.name = "Recipe 1";
                    recipe1.booleanTest = true;
                    recipe1.comments = ["Recipe 1: Comment 1", "Recipe 1: Comment 2"];
                    recipe1.entries = ["Recipe 1: Entry 1", "Recipe 1: Entry 2"];
                    await recipe1.save();

                    const recipe2 = new Recipe(perspective!);
                    recipe2.name = "Recipe 2";
                    recipe2.booleanTest = false;
                    recipe2.comments = ["Recipe 2: Comment 1", "Recipe 2: Comment 2"];
                    recipe2.entries = ["Recipe 2: Entry 1", "Recipe 2: Entry 2"];
                    await recipe2.save();

                    // Check all recipes are there
                    const allRecipes = await Recipe.findAll(perspective!);
                    expect(allRecipes.length).to.equal(2);

                    // Test with where, properties, and collections
                    const recipes1 = await Recipe.findAll(perspective!, { where: { name: "Recipe 1" }, properties: ["name"], collections: ["comments"] });
                    expect(recipes1.length).to.equal(1);
                    expect(recipes1[0].name).to.equal("Recipe 1");
                    expect(recipes1[0].booleanTest).to.be.undefined;
                    expect(recipes1[0].comments.length).to.equal(2);
                    expect(recipes1[0].entries).to.be.undefined;

                    // Test with different where, properties, and collections
                    const recipes2 = await Recipe.findAll(perspective!, { where: { name: "Recipe 2" }, properties: ["booleanTest"], collections: ["entries"] });
                    expect(recipes2.length).to.equal(1);
                    expect(recipes2[0].name).to.be.undefined;
                    expect(recipes2[0].booleanTest).to.equal(false);
                    expect(recipes2[0].comments).to.be.undefined;
                    expect(recipes2[0].entries.length).to.equal(2);

                    await recipe1.delete();
                    await recipe2.delete();
                })

                it("findAll() works with constraining resolved literal properties", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
                    // Create a recipe with a resolved literal property
                    const recipe = new Recipe(perspective!);
                    recipe.resolve = "Hello World"
                    await recipe.save();

                    // Test with resolved literal property
                    const recipes1 = await Recipe.findAll(perspective!, { where: { resolve: "Hello World" } });
                    expect(recipes1.length).to.equal(1);
                    expect(recipes1[0].resolve).to.equal("Hello World");
                    
                    await recipe.delete();
                })

                it("findAll() works with multiple property constraints in one where clause", async () => {
                    // Clear previous recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    await recipe1.delete();
                    await recipe2.delete();
                    await recipe3.delete();
                })

                it("findAllAndCount() returns both the retrived instances and the total count", async () => {
                    // Clear any old recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    // Delete recipies
                    for (const recipe of allRecipes) await recipe.delete();
                })

                it("paginate() helper function works with pageNumber & pageSize props", async () => {
                    // Clear any old recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    // Delete recipies
                    for (const recipe of allRecipes) await recipe.delete();
                });

                it("count() returns only the count without retrieving instances", async () => {
                    // Clear any old recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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

                    // Delete recipes
                    for (const recipe of await Recipe.findAll(perspective!)) await recipe.delete();
                });

                it("count() and countSubscribe() work on the query builder", async () => {
                    // Clear any old recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();
                    
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
                    const subscription = await Recipe.query(perspective!)
                        .countSubscribe((count) => {
                            lastCount = count;
                        });
                    expect(subscription).to.equal(3);

                    // Add another recipe and verify callback is called
                    const recipe4 = new Recipe(perspective!);
                    recipe4.name = "Recipe 4";
                    await recipe4.save();

                    // Give time for subscription to process
                    await sleep(1000);
                    expect(lastCount).to.equal(4);

                    // Clean up
                    for (const recipe of await Recipe.findAll(perspective!)) {
                        await recipe.delete();
                    }
                })

                it("paginate() and paginateSubscribe() work on the query builder", async () => {
                    // Clear any existing recipes
                    const oldRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of oldRecipes) await recipe.delete();

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

                    // Add a new recipe and verify subscription updates
                    const newRecipe = new Recipe(perspective!);
                    newRecipe.name = "Recipe 11";
                    await newRecipe.save();

                    let tries = 0;
                    while (!lastResult) {
                        await sleep(500);
                        tries++;
                        if (tries > 20) {
                            throw new Error("Timeout waiting for subscription to update");
                        }
                    }
                    expect(lastResult.totalCount).to.equal(11);

                    // Clean up
                    const allRecipes = await Recipe.findAll(perspective!);
                    for (const recipe of allRecipes) {
                        await recipe.delete();
                    }
                })

                it("query builder works with subscriptions", async () => {
                    @ModelOptions({
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
                    const initialResults = await Notification
                        .query(perspective!)
                        .where({ 
                            priority: { gt: 5 },
                            read: false
                        })
                        .subscribe((newNotifications) => {
                            notifications = newNotifications;
                            updateCount++;
                        });

                    // Initially no results
                    expect(initialResults.length).to.equal(0);
                    expect(updateCount).to.equal(0);

                    // Add matching notification - should trigger subscription
                    const notification1 = new Notification(perspective!);
                    notification1.title = "High priority notification";
                    notification1.priority = 8;
                    notification1.read = false;
                    await notification1.save();

                    // Wait for subscription to fire
                    await sleep(1000);
                    expect(updateCount).to.equal(1);
                    expect(notifications.length).to.equal(1);

                    // Add another matching notification - should trigger subscription again
                    const notification2 = new Notification(perspective!);
                    notification2.title = "Another high priority";
                    notification2.priority = 7;
                    notification2.read = false;
                    await notification2.save();

                    await sleep(1000);
                    expect(updateCount).to.equal(2);
                    expect(notifications.length).to.equal(2);

                    // Add non-matching notification (low priority) - should not trigger subscription
                    const notification3 = new Notification(perspective!);
                    notification3.title = "Low priority notification";
                    notification3.priority = 3;
                    notification3.read = false;
                    await notification3.save();

                    await sleep(1000);
                    expect(updateCount).to.equal(2);
                    expect(notifications.length).to.equal(2);

                    // Mark notification1 as read - should trigger subscription to remove it
                    notification1.read = true;
                    await notification1.update();
                    await sleep(1000);
                    expect(notifications.length).to.equal(1);

                    // Clean up
                    await notification1.delete();
                    await notification2.delete();
                    await notification3.delete();
                });

                it("query builder should filter by subject class", async () => {
                    // Define a second subject class
                    @ModelOptions({
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
                            resolveLanguage: "literal"
                        })
                        content1: string = "";
                    }

                    @ModelOptions({
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
                            resolveLanguage: "literal"
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

                    // Clean up
                    await note1.delete();
                    await note2.delete();
                });

                it("query builder works with single query object, complex query and subscriptions", async () => {
                    @ModelOptions({
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
                    const initialResults = await (Task.query(perspective!, { 
                        where: { 
                            dueDate: { lte: nextWeekTimestamp },
                            completed: false,
                            assignee: "alice"
                        }
                    })).subscribe((newTasks) => {
                        tasks = newTasks;
                        updateCount++;
                    });

                    // Initially no results
                    expect(initialResults.length).to.equal(0);
                    expect(updateCount).to.equal(0);

                    // Add matching task - should trigger subscription
                    const task1 = new Task(perspective!);
                    task1.description = "Urgent task for tomorrow";
                    task1.dueDate = tomorrowTimestamp;
                    task1.completed = false;
                    task1.assignee = "alice";
                    await task1.save();
                    
                    await task1.get();

                    // Wait for subscription to fire
                    await sleep(1000);

                    expect(updateCount).to.equal(1);
                    expect(tasks.length).to.equal(1);

                    // Add another matching task - should trigger subscription again
                    const task2 = new Task(perspective!);
                    task2.description = "Another task for next week";
                    task2.dueDate = nextWeekTimestamp;
                    task2.completed = false;
                    task2.assignee = "alice";
                    await task2.save();

                    await sleep(1000);
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
                    await task1.update();
                    await sleep(1000);

                    expect(tasks.length).to.equal(1);   

                    // Clean up
                    await task1.delete();
                    await task2.delete();
                    await task3.delete();
                });

                it("transform option in property decorators works", async () => {
                    @ModelOptions({ name: "ImagePost" })
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
                    await perspective!.ensureSDNASubjectClass(ImagePost);

                    // Create a new image post
                    const post = new ImagePost(perspective!);
                    const imageData = "abc123";
                    //const imageData = { data_base64: "abc123" };
                    
                    post.image = imageData;
                    await post.save();

                    // Retrieve the post and check transformed values
                    const [retrieved] = await ImagePost.findAll(perspective!);
                    expect(retrieved.image).to.equal("data:image/png;base64,abc123");
                });

                it("should support batch operations with multiple models", async () => {
                    let perspective = await ad4m!.perspective.add("batch test")
                    @ModelOptions({
                        name: "BatchRecipe"
                    })
                    class BatchRecipe extends Ad4mModel {
                        @Property({
                            through: "recipe://name",
                            resolveLanguage: "literal"
                        })
                        name: string = "";

                        @Collection({ through: "recipe://ingredients" })
                        ingredients: string[] = [];
                    }

                    @ModelOptions({
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
                    recipe.ingredients = ["pasta", "sauce", "cheese"];
                    console.log("recipe: ", recipe)
                    console.log("first save")
                    await recipe.save(batchId);
                    console.log("saved")

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
                    console.log("committing batch")
                    const result = await perspective!.commitBatch(batchId);
                    console.log("done committed batch")
                    expect(result.additions.length).to.be.greaterThan(0);
                    expect(result.removals.length).to.equal(0);
                    
                    // Verify models are now visible
                    const recipesAfterCommit = await BatchRecipe.findAll(perspective!);
                    expect(recipesAfterCommit.length).to.equal(1);
                    expect(recipesAfterCommit[0].name).to.equal("Pasta");
                    expect(recipesAfterCommit[0].ingredients).to.deep.equal(["pasta", "sauce", "cheese"]);

                    const notesAfterCommit = await BatchNote.findAll(perspective!);
                    expect(notesAfterCommit.length).to.equal(1);
                    expect(notesAfterCommit[0].title).to.equal("Recipe Notes");
                    expect(notesAfterCommit[0].content).to.equal("Make sure to use fresh ingredients");

                    console.log("creation batch done")

                    // Test updating models in batch
                    const updateBatchId = await perspective!.createBatch();
                    console.log("update batch created")
                    recipe.ingredients.push("garlic");
                    await recipe.update(updateBatchId);

                    note.content = "Updated: Use fresh ingredients and add garlic";
                    await note.update(updateBatchId);

                    // Verify models haven't changed before commit
                    const recipesBeforeUpdate = await BatchRecipe.findAll(perspective!);
                    expect(recipesBeforeUpdate[0].ingredients).to.deep.equal(["pasta", "sauce", "cheese"]);

                    const notesBeforeUpdate = await BatchNote.findAll(perspective!);
                    expect(notesBeforeUpdate[0].content).to.equal("Make sure to use fresh ingredients");

                    // Commit update batch
                    const updateResult = await perspective!.commitBatch(updateBatchId);
                    console.log("update batch done")
                    expect(updateResult.additions.length).to.be.greaterThan(0);

                    // Verify models are updated
                    const recipesAfterUpdate = await BatchRecipe.findAll(perspective!);
                    expect(recipesAfterUpdate[0].ingredients).to.deep.equal(["pasta", "sauce", "cheese", "garlic"]);

                    const notesAfterUpdate = await BatchNote.findAll(perspective!);
                    expect(notesAfterUpdate[0].content).to.equal("Updated: Use fresh ingredients and add garlic");

                    // Test deleting models in batch
                    // const deleteBatchId = await perspective!.createBatch();

                    // await recipesAfterUpdate[0].delete(deleteBatchId);
                    // await notesAfterUpdate[0].delete(deleteBatchId);

                    // // Verify models still exist before commit
                    // const recipesBeforeDelete = await BatchRecipe.findAll(perspective!);
                    // expect(recipesBeforeDelete.length).to.equal(1);

                    // const notesBeforeDelete = await BatchNote.findAll(perspective!);
                    // expect(notesBeforeDelete.length).to.equal(1);

                    // // Commit delete batch
                    // const deleteResult = await perspective!.commitBatch(deleteBatchId);
                    // expect(deleteResult.removals.length).to.be.greaterThan(0);

                    // // Verify models are deleted
                    // const recipesAfterDelete = await BatchRecipe.findAll(perspective!);
                    // expect(recipesAfterDelete.length).to.equal(0);

                    // const notesAfterDelete = await BatchNote.findAll(perspective!);
                    // expect(notesAfterDelete.length).to.equal(0);
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

})

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}