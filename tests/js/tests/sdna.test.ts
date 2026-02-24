import { expect } from "chai";
import {
  Ad4mClient,
  Link,
  LinkQuery,
  Literal,
  PerspectiveProxy,
  Ad4mModel,
  Flag,
  Property,
  HasMany,
  Model,
} from "@coasys/ad4m";
import { startAgent } from "../helpers/executor";
import type { AgentHandle } from "../helpers/executor";
import fetch from "node-fetch";

//@ts-ignore
global.fetch = fetch;

describe("SDNA", () => {
  let ad4m: Ad4mClient | null = null;
  let agent: AgentHandle | null = null;

  before(async () => {
    agent = await startAgent("prolog-agent", { passphrase: "secret" });
    ad4m = agent.client;
  });

  after(async () => {
    await agent?.stop();
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

        // Plain untyped relation – simplest HasMany case
        @HasMany({ through: "todo://comment" })
        comments: string[] = [];
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

      it("should generate correct SHACL shape from a JS class", async () => {
        // generateSDNA() was the legacy Prolog-based method; the modern API is
        // generateSHACL() which returns a W3C SHACL shape with AD4M action
        // definitions instead of raw Prolog code.
        const { name, shape } = Todo.generateSHACL();

        expect(name).to.equal("Todo");

        // --- Constructor action ---
        // The `state` property has `initial: "todo://ready"`, so a constructor
        // action that writes that link should be present.
        expect(shape.constructor_actions)
          .to.be.an("array")
          .with.length.greaterThan(0);
        const constructorAction = shape.constructor_actions!.find(
          (a: any) => a.predicate === "todo://state",
        );
        expect(constructorAction, "constructor action for todo://state").to
          .exist;
        expect(constructorAction!.target).to.equal("todo://ready");

        // --- Properties ---
        const stateProp = shape.properties.find(
          (p: any) => p.path === "todo://state",
        );
        expect(stateProp, "state property").to.exist;

        const titleProp = shape.properties.find(
          (p: any) => p.path === "todo://has_title",
        );
        expect(titleProp, "title property").to.exist;
        expect(titleProp!.resolveLanguage).to.equal("literal");

        // --- Relations (formerly collections – HasMany generates adder/remover, no maxCount:1) ---
        const commentsColl = shape.properties.find(
          (p: any) => p.path === "todo://comment",
        );
        expect(commentsColl, "comments relation").to.exist;
        // A HasMany relation shape has adder/remover actions and no maxCount constraint
        expect(commentsColl!.adder, "comments relation adder").to.be.an(
          "array",
        );
        expect(commentsColl!.remover, "comments relation remover").to.be.an(
          "array",
        );
        expect(commentsColl!.maxCount).to.be.undefined;
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
      });
    });
  });
});
