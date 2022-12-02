import { expect } from "chai";
import { exec, execSync, ChildProcess } from 'node:child_process';
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import { HttpLink } from "@apollo/client/link/http/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";
import { Ad4mClient, Link, LinkQuery, Literal, PerspectiveProxy, Subject, subjectProperty, subjectCollection, sdnaOutput, addLink, hasLink } from "@perspect3vism/ad4m";
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
            expect(classes[0]).to.equal("TODO")
        })

        it("should be able to construct a subject instance from a literal", async () => {
            let root = Literal.from("construct test").toUrl()
            expect(await perspective!.constructSubject("TODO", root)).to.be.true
            expect(await perspective!.isSubjectInstance(root, "TODO")).to.be.true
        })
    
        describe("with an instance", () => {
            let subject: Subject | null = null

            before(async () => {
                let root = Literal.from("construct test").toUrl()
                await perspective!.constructSubject("TODO", root)
                subject = await perspective!.subjectInstance(root, "TODO")
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

            it("should work with a property that is not set initially", async () => {
                //@ts-ignore
                expect(await subject.title).to.be.undefined
        
                let title = Literal.from("test title").toUrl()
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
                expect(todoClasses).to.include("TODO")
                expect(todoClasses.length).to.equal(1)

                let unknownClasses = await perspective!.subjectClassesByTemplate(unknown)
                expect(unknownClasses).to.be.empty

                let almostTodoClasses = await perspective!.subjectClassesByTemplate(almostTodo)
                expect(almostTodoClasses).to.be.empty
            })

            it("can find subject and create instances in a type-safe way", async () => {
                // PerspectiveProxe.subejctInstancesByTemplate() is a generic that returns
                // an array of the given type.
                let todos = await perspective!.subjectInstancesByTemplate(todo)

                // todos is an array of Todo objects
                // note how we don't need @ts-ignore here:
                expect(todos.length).to.equal(1)
                expect(await todos[0].state).to.equal("todo://done")
            })

        })

        describe("SDNA creation decorators", () => {
            it("should be able to create an SDNA from a class", async () => {

                class Todo {
                    subjectConstructor = [addLink("this", "todo://state", "todo://ready")]
                    isSubjectInstance = [hasLink("todo://state")]

                    //@ts-ignore
                    @subjectProperty({through: "todo://state", initial:"todo://ready"})
                    state: string = ""

                    //@ts-ignore
                    @subjectProperty({through: "todo://title"})
                    title: string = ""

                    //@ts-ignore
                    @subjectCollection({through: "todo://comment"})
                    comments: string[] = []
    
                    setState(state: string) {}
                    setTitle(title: string) {}
                    addComment(comment: string) {}


                    @sdnaOutput
                    static generateSdna(): string { return "" }
                }

                let sdna = Todo.generateSdna()
                console.log(sdna)
            })
        })
    })


})