import { expect } from "chai";
import { exec, execSync, ChildProcess } from 'node:child_process';
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import { HttpLink } from "@apollo/client/link/http/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";
import { Ad4mClient, Link, LinkQuery, Literal, PerspectiveProxy, 
    SmartLiteral, SMART_LITERAL_CONTENT_PREDICATE, 
    instanceQuery, Subject, subjectProperty, subjectPropertySetter,
    subjectCollection, sdnaOutput,
} from "@perspect3vism/ad4m";
import { rmSync, readFileSync } from "node:fs";
import fetch from 'node-fetch';


function apolloClient(port: number, token?: string): ApolloClient<any> {
    const wsLink = new GraphQLWsLink(createClient({
        url: `ws://localhost:${port}/graphql`,
        webSocketImpl: Websocket,
    }));

    const link = new HttpLink({
        uri: "http://localhost:4000/graphql",
        //@ts-ignore
        fetch
      });
  
    return new ApolloClient({
        link: wsLink,
        cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
        defaultOptions: {
            watchQuery: {
                fetchPolicy: "no-cache",
            },
            query: {
                fetchPolicy: "no-cache",
            }
        },
    });
  }

describe("Integration", () => {
    let ad4m: Ad4mClient | null = null
    let executorProcess: ChildProcess | null = null

    before(async () => {
        //ad4m = new Ad4mClient(apolloClient(4000))
        //return
        
        rmSync("../ad4mJS", { recursive: true, force: true })
        console.log("Initialzing executor data directory")
        //@ts-ignore
        execSync('../../host/dist/ad4m-macos-x64 init --dataPath ../ad4mJS', {})
        
        console.log("Starting executor")
        try {
            execSync("killall holochain")
        } catch (e) {
            console.log("No holochain process running")
        }
        
        //@ts-ignore
        executorProcess = exec('../../host/dist/ad4m-macos-x64 serve --dataPath ../ad4mJS', {})

        let executorReady = new Promise<void>((resolve, reject) => {
            executorProcess!.stdout!.on('data', (data) => {
                if (data.includes("GraphQL server started")) {
                    resolve()
                }
            });
        })

        executorProcess!.stdout!.on('data', (data) => {
            console.log(`${data}`);
        });
        executorProcess!.stderr!.on('data', (data) => {
            console.log(`${data}`);
        });
    
        console.log("Waiting for executor to settle...")
        await executorReady
        console.log("Creating ad4m client")
        ad4m = new Ad4mClient(apolloClient(4000))
        console.log("Generating agent")
        await ad4m.agent.generate("secret")
        console.log("Done")
        
    })

    after(() => {
        if (executorProcess) {
            executorProcess.kill()
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
            console.log("UUID: " + perspective.uuid)

            let classes = await perspective.subjectClasses();
            expect(classes.length).to.equal(0)

            let sdna = readFileSync("./subject.pl").toString()
            await perspective.setSdna(sdna)

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
            expect(await perspective!.isSubjectInstance(root, "Todo")).to.be.true
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
                subject.addComment(c1)
                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1])

                //@ts-ignore
                subject.addComment(c2)
                //@ts-ignore
                expect(await subject.comments).to.deep.equal([c1, c2])
            })

            it("should be able to get all subject instance of a given class", async () => {
                let todos = await perspective!.getAllSubjectInstances("Todo") as unknown as Subject[]
                expect(todos.length).to.equal(2)
                //@ts-ignore
                expect(await todos[1].state).to.equal("todo://done")
            })
        })

        describe("TypeScript compatibility", () => {

            // This class mathces the SDNA in ./subject.pl
            class Todo {
                state: string = ""
                title: string = ""
                comments: string[] = []

                setState(state: string) {}
                setTitle(title: string) {}
                addComment(comment: string) {}
            }

            // This class doesn not match the SDNA in ./subject.pl
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
                expect(todos.length).to.equal(2)
                expect(await todos[1].state).to.equal("todo://done")
            })

        })

        describe("SDNA creation decorators", () => {
            // This class matches the SDNA in ./subject.pl
            // and this test proves the decorators create the exact same SDNA code
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
                @instanceQuery()
                static async all(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @instanceQuery({where: {state: "todo://ready"}})
                static async allReady(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @instanceQuery({where: { state: "todo://done" }})
                static async allDone(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                @instanceQuery({condition: 'triple("ad4m://self", _, Instance)'})
                static async allSelf(perspective: PerspectiveProxy): Promise<Todo[]> { return [] }

                //@ts-ignore
                @subjectProperty({
                    through: "todo://state", 
                    initial:"todo://ready",
                    required: true,
                })
                state: string = ""

                // This function need to be present (next to the "through" parameter on the property itself)
                // in order to trigger the creation of setter code in the SDNA.
                // It can be left emtpy when used with PerspectiveProxy.subjectInstancesByTemplate()
                // since an implementation will also be auto-generated there.
                //
                // NOTE thate the name must be `set${capitalize(propertyName)}`.
                setState(state: string) {}

                //@ts-ignore
                @subjectProperty({
                    through: "todo://has_title",
                    resolve: true,
                })
                title: string = ""

                @subjectPropertySetter({
                    resolveLanguage: 'literal'
                })
                setTitle(title: string) {}

                //@ts-ignore
                @subjectCollection({through: "todo://comment"})
                comments: string[] = []
                addComment(comment: string) {}

                @sdnaOutput
                static generateSDNA(): string { return "" }
            }

            it("should generate correct SDNA from a JS class", async () => {
                let sdna = Todo.generateSDNA()
                expect(sdna).to.equal(readFileSync("./subject.pl").toString())
            })

            it("should be possible to use that class for type-safe interaction with subject instances", async () => {
                // construct new subject intance
                let root = Literal.from("Decorated class construction test").toUrl()
                // get instance with type information
                let todo = await perspective!.createSubject(new Todo(), root)

                expect(await perspective!.isSubjectInstance(root, new Todo())).to.be.true
                let todo2 = await perspective!.getSubjectProxy(root, new Todo())
                expect(todo2).to.have.property("state")
                expect(todo2).to.have.property("title")
                expect(todo2).to.have.property("comments")

                await todo.setState("todo://review")
                expect(await todo.state).to.equal("todo://review")
                expect(await todo.comments).to.be.empty

                let comment = Literal.from("new comment").toUrl()
                await todo.addComment(comment)
                expect(await todo.comments).to.deep.equal([comment])
            })

            it("can retrieve all instances through instaceQuery decoratored all()", async () => {
                let todos = await Todo.all(perspective!)
                expect(todos.length).to.equal(3)
            })

            it("can retrieve all mathching instance through instanceQuery(where: ..)", async () => {
                let todos = await Todo.allReady(perspective!)
                expect(todos.length).to.equal(1)
                expect(await todos[0].state).to.equal("todo://ready")

                todos = await Todo.allDone(perspective!)
                expect(todos.length).to.equal(1)
                expect(await todos[0].state).to.equal("todo://done")
            })

            it("can retrieve matching instance through instanceQuery(condition: ..)", async () => {
                let todos = await Todo.allSelf(perspective!)
                expect(todos.length).to.equal(0)

                todos = await Todo.all(perspective!)
                let todo = todos[0]
                //@ts-ignore
                perspective!.add(new Link({source: "ad4m://self", target: todo.baseExpression}))
                
                todos = await Todo.allSelf(perspective!)
                expect(todos.length).to.equal(1)
            })

            it("can deal with properties that resolve the URI and create Expressions", async () => {
                let todos = await Todo.all(perspective!)
                let todo = todos[0]
                expect(await todo.title).to.equal(undefined)

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

                class Test {
                    @subjectProperty({through: "test://test_numer"})
                    number: number = 0


                    @sdnaOutput
                    static generateSDNA(): string { return "" }
                }

                await perspective!.ensureSDNASubjectClass(Test)

                expect(await perspective!.getSdna()).to.have.lengthOf(2)
                console.log(await perspective!.getSdna()[1])
            })
        })
    })


    describe("Smart Literal", () => {
        let perspective: PerspectiveProxy | null = null

        before(async () => {
            perspective = await ad4m!.perspective.add("smart literal test")
            // for test debugging:
            console.log("UUID: " + perspective.uuid)
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