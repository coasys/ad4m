import {
  Ad4mClient,
  InstanceQuery,
  Link,
  LinkQuery,
  Literal,
  PerspectiveProxy,
  SDNAClass,
  SMART_LITERAL_CONTENT_PREDICATE,
  SmartLiteral,
  Subject,
  SubjectCollection,
  SubjectEntity,
  SubjectFlag,
  SubjectProperty,
} from "@coasys/ad4m";
import { expect } from "chai";
import fetch from "node-fetch";
import { ChildProcess } from "node:child_process";
import { readFileSync } from "node:fs";
import path from "path";
import { fileURLToPath } from "url";
import { apolloClient, startExecutor } from "../utils/utils";

//@ts-ignore
global.fetch = fetch;

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Prolog + Literals", () => {
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
      hcAppPort
    );

    console.log("Creating ad4m client");
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

  describe("Subjects", () => {
    let perspective: PerspectiveProxy | null = null;

    before(async () => {
      perspective = await ad4m!.perspective.add("test");
      // for test debugging:
      console.log("UUID: " + perspective.uuid);

      let classes = await perspective.subjectClasses();
      expect(classes.length).to.equal(0);

      let sdna = readFileSync("./sdna/subject.pl").toString();
      await perspective.addSdna("Todo", sdna, "subject_class");

      let retrievedSdna = await perspective.getSdna();
      expect(retrievedSdna).to.deep.equal([sdna]);
    });

    it("should find the TODO subject class from the test SDNA", async () => {
      let classes = await perspective!.subjectClasses();

      expect(classes.length).to.equal(1);
      expect(classes[0]).to.equal("Todo");
    });

    it("should be able to construct a subject instance from a literal", async () => {
      let root = Literal.from("construct test").toUrl();
      expect(await perspective!.createSubject("Todo", root)).to.not.be.undefined;
      expect(await perspective!.isSubjectInstance(root, "Todo")).to.not.be.false;
    });

    it("can get subject instance proxy via class string", async () => {
      let root = Literal.from("get proxy test").toUrl();
      await perspective!.createSubject("Todo", root);
      let subject = (await perspective!.getSubjectProxy(root, "Todo")) as unknown as Subject;
      expect(subject).to.not.be.undefined;
      expect(subject).to.have.property("state");
      expect(subject).to.have.property("setState");
      expect(subject).to.have.property("title");
    });

    describe("with an instance", () => {
      let subject: Subject | null = null;

      before(async () => {
        let root = Literal.from("construct test").toUrl();
        subject = (await perspective!.createSubject("Todo", root)) as unknown as Subject;
      });

      it("should be able to read a property as JS property", async () => {
        //@ts-ignore
        expect(await subject.state).to.equal("todo://ready");
      });

      it("should be able to set a property with JS setter method", async () => {
        //@ts-ignore
        await subject.setState("todo://done");
        //@ts-ignore
        expect(await subject.state).to.equal("todo://done");
      });

      it("should work with a property that is not set initially and that auto-resolves", async () => {
        //@ts-ignore
        expect(await subject.title).to.be.undefined;

        let title = "test title";
        //@ts-ignore
        await subject.setTitle(title);
        //@ts-ignore
        expect(await subject.title).to.equal(title);
      });

      it("should be able to get collections as arrays", async () => {
        //@ts-ignore
        expect(await subject.comments).to.be.an("array");
        //@ts-ignore
        expect(await subject.comments).to.be.empty;

        let c1 = Literal.from("comment 1").toUrl();
        await perspective!.add(
          new Link({
            source: subject!.baseExpression,
            predicate: "todo://comment",
            target: c1,
          })
        );

        //@ts-ignore
        expect(await subject.comments).to.deep.equal([c1]);

        let c2 = Literal.from("comment 2").toUrl();
        await perspective!.add(
          new Link({
            source: subject!.baseExpression,
            predicate: "todo://comment",
            target: c2,
          })
        );

        //@ts-ignore
        expect(await subject.comments).to.deep.equal([c1, c2]);
      });

      it("should be able to add to collections", async () => {
        let commentLinks = await perspective!.get(
          new LinkQuery({
            source: subject!.baseExpression,
            predicate: "todo://comment",
          })
        );
        for (let link of commentLinks) {
          await perspective!.remove(link);
        }

        //@ts-ignore
        expect(await subject.comments).to.be.empty;

        let c1 = Literal.from("new comment 1").toUrl();
        let c2 = Literal.from("new comment 2").toUrl();

        //@ts-ignore
        await subject.addComments(c1);
        await sleep(100);
        //@ts-ignore
        expect(await subject.comments).to.deep.equal([c1]);

        //@ts-ignore
        await subject.addComments(c2);
        await sleep(100);
        //@ts-ignore
        expect(await subject.comments).to.deep.equal([c1, c2]);
      });

      it("should be able to get all subject instance of a given class", async () => {
        let todos = (await perspective!.getAllSubjectInstances("Todo")) as unknown as Subject[];
        expect(todos.length).to.equal(2);
        //@ts-ignore
        expect(await todos[1].state).to.exist;
      });
    });

    describe("TypeScript compatibility", () => {
      // This class mathces the SDNA in ./sdna/subject.pl
      class Todo {
        state: string = "";
        title: string = "";
        comments: string[] = [];

        setState(state: string) {}
        setTitle(title: string) {}
        addComments(comment: string) {}
        setCollectionComments(comment: string) {}
      }

      // This class doesn not match the SDNA in ./sdna/subject.pl
      class UnknownSubject {
        name: string = "";
        x: string = "";

        setTop(top: string) {}
      }

      // This class is like Todo, but has a setter that
      // is not defined in the SDNA (-> should not match)
      class AlmostTodo {
        state: string = "";
        title: string = "";
        comments: string[] = [];

        setState(state: string) {}
        setTitle(title: string) {}
        addComment(comment: string) {}
        setTop(top: string) {}
      }

      let todo: Todo = new Todo();
      let unknown: UnknownSubject = new UnknownSubject();
      let almostTodo: AlmostTodo = new AlmostTodo();

      it("can find subject classes mapping to JS objects", async () => {
        let todoClasses = await perspective!.subjectClassesByTemplate(todo);
        expect(todoClasses).to.include("Todo");
        expect(todoClasses.length).to.equal(1);

        let unknownClasses = await perspective!.subjectClassesByTemplate(unknown);
        expect(unknownClasses).to.be.empty;

        let almostTodoClasses = await perspective!.subjectClassesByTemplate(almostTodo);
        expect(almostTodoClasses).to.be.empty;
      });

      it("can find subject and create instances in a type-safe way", async () => {
        // PerspectiveProxe.getAllSubjectInstances() is a generic that returns
        // an array of the given type.
        let todos = await perspective!.getAllSubjectInstances(todo);

        // todos is an array of Todo objects
        // note how we don't need @ts-ignore here:
        expect(todos.length).to.equal(2);
        expect(await todos[1].state).to.exist;
      });
    });

    describe("SDNA creation decorators", () => {
      @SDNAClass({
        name: "Message",
      })
      class Message {
        //@ts-ignore
        @SubjectFlag({
          through: "ad4m://type",
          value: "ad4m://message",
        })
        type: string = "";

        //@ts-ignore
        @InstanceQuery()
        static async all(perspective: PerspectiveProxy): Promise<Message[]> {
          return [];
        }

        //@ts-ignore
        @SubjectProperty({
          through: "todo://state",
          initial: "todo://ready",
          writable: true,
        })
        body: string = "";
      }

      // This class matches the SDNA in ./sdna/subject.pl
      // and this test proves the decorators create the exact same SDNA code
      @SDNAClass({
        name: "Todo",
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
        static async all(perspective: PerspectiveProxy): Promise<Todo[]> {
          return [];
        }

        @InstanceQuery({ where: { state: "todo://ready" } })
        static async allReady(perspective: PerspectiveProxy): Promise<Todo[]> {
          return [];
        }

        @InstanceQuery({ where: { state: "todo://done" } })
        static async allDone(perspective: PerspectiveProxy): Promise<Todo[]> {
          return [];
        }

        @InstanceQuery({ condition: 'triple("ad4m://self", _, Instance)' })
        static async allSelf(perspective: PerspectiveProxy): Promise<Todo[]> {
          return [];
        }

        //@ts-ignore
        @SubjectProperty({
          through: "todo://state",
          initial: "todo://ready",
          writable: true,
          required: true,
        })
        state: string = "";

        //@ts-ignore
        @SubjectProperty({
          through: "todo://has_title",
          writable: true,
          resolveLanguage: "literal",
        })
        title: string = "";

        @SubjectProperty({
          getter: `triple(Base, "flux://has_reaction", "flux://thumbsup"), Value = true`,
        })
        isLiked: boolean = false;

        //@ts-ignore
        @SubjectCollection({ through: "todo://comment" })
        // @ts-ignore
        comments: string[] = [];

        //@ts-ignore
        @SubjectCollection({ through: "flux://entry_type" })
        entries: string[] = [];

        //@ts-ignore
        @SubjectCollection({
          through: "flux://entry_type",
          where: { isInstance: Message },
        })
        messages: string[] = [];

        //@ts-ignore
        @SubjectCollection({
          through: "flux://entry_type",
          where: { condition: `triple(Target, "flux://has_reaction", "flux://thumbsup")` },
        })
        likedMessages: string[] = [];
      }

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
        // construct new subject intance
        let root = Literal.from("Decorated class construction test").toUrl();
        // get instance with type information
        let todo = await perspective!.createSubject(new Todo(), root);

        expect(await perspective!.isSubjectInstance(root, new Todo())).to.not.be.false;
        let todo2 = await perspective!.getSubjectProxy(root, new Todo());
        expect(todo2).to.have.property("state");
        expect(todo2).to.have.property("title");
        expect(todo2).to.have.property("comments");
        // @ts-ignore
        await todo.setState("todo://review");
        await sleep(1000);
        expect(await todo.state).to.equal("todo://review");
        expect(await todo.comments).to.be.empty;

        let comment = Literal.from("new comment").toUrl();
        // @ts-ignore
        await todo.addComments(comment);
        expect(await todo.comments).to.deep.equal([comment]);
      });

      it("can retrieve all instances through instaceQuery decoratored all()", async () => {
        let todos = await Todo.all(perspective!);
        expect(todos.length).to.equal(3);
      });

      it("can retrieve all mathching instance through InstanceQuery(where: ..)", async () => {
        let todos = await Todo.allReady(perspective!);
        expect(todos.length).to.equal(1);
        expect(await todos[0].state).to.equal("todo://ready");

        todos = await Todo.allDone(perspective!);
        expect(todos.length).to.equal(1);
        expect(await todos[0].state).to.equal("todo://done");
      });

      it("can retrieve matching instance through InstanceQuery(condition: ..)", async () => {
        let todos = await Todo.allSelf(perspective!);
        expect(todos.length).to.equal(0);

        todos = await Todo.all(perspective!);
        let todo = todos[0];
        //@ts-ignore
        await perspective!.add(new Link({ source: "ad4m://self", target: todo.baseExpression }));

        todos = await Todo.allSelf(perspective!);
        expect(todos.length).to.equal(1);
      });

      it("can deal with properties that resolve the URI and create Expressions", async () => {
        let todos = await Todo.all(perspective!);
        let todo = todos[0];
        expect(await todo.title).to.be.undefined;

        // @ts-ignore
        await todo.setTitle("new title");
        expect(await todo.title).to.equal("new title");

        //@ts-ignore
        let links = await perspective!.get(
          new LinkQuery({ source: todo.baseExpression, predicate: "todo://has_title" })
        );
        expect(links.length).to.equal(1);
        let literal = Literal.fromUrl(links[0].data.target).get();
        expect(literal.data).to.equal("new title");
      });

      it("can easily be initialized with PerspectiveProxy.ensureSDNASubjectClass()", async () => {
        expect(await perspective!.getSdna()).to.have.lengthOf(1);

        @SDNAClass({
          name: "Test",
        })
        class Test {
          @SubjectProperty({ through: "test://test_numer" })
          number: number = 0;
        }

        await perspective!.ensureSDNASubjectClass(Test);

        expect(await perspective!.getSdna()).to.have.lengthOf(2);
        //console.log((await perspective!.getSdna())[1])
      });

      it("can constrain collection entries through 'where' clause with prolog condition", async () => {
        let root = Literal.from("Collection where test with prolog condition").toUrl();
        let todo = await perspective!.createSubject(new Todo(), root);

        let messageEntry = Literal.from("test message").toUrl();

        // @ts-ignore
        await todo.addEntries(messageEntry);

        let entries = await todo.entries;
        expect(entries.length).to.equal(1);

        let messageEntries = await todo.likedMessages;
        expect(messageEntries.length).to.equal(0);

        await perspective?.add(
          new Link({
            source: messageEntry,
            predicate: "flux://has_reaction",
            target: "flux://thumbsup",
          })
        );

        messageEntries = await todo.likedMessages;
        expect(messageEntries.length).to.equal(1);
      });

      it("can use properties with custom getter prolog code", async () => {
        let root = Literal.from("Custom getter test").toUrl();
        let todo = await perspective!.createSubject(new Todo(), root);

        // @ts-ignore
        const liked1 = await todo.isLiked;
        expect(liked1).to.be.undefined;

        await perspective?.add(
          new Link({ source: root, predicate: "flux://has_reaction", target: "flux://thumbsup" })
        );

        // @ts-ignore
        const liked2 = await todo.isLiked;
        expect(liked2).to.be.true;
      });

      describe("with Message subject class registered", () => {
        before(async () => {
          // @ts-ignore
          const { name, sdna } = Message.generateSDNA();
          await perspective!.addSdna(name, sdna, "subject_class");
        });

        it("can find instances through the exact flag link", async () => {
          await perspective!.add(
            new Link({
              source: "test://message",
              predicate: "ad4m://type",
              target: "ad4m://undefined",
            })
          );

          const first = await Message.all(perspective!);
          expect(first.length).to.be.equal(0);

          await perspective!.add(
            new Link({
              source: "test://message",
              predicate: "ad4m://type",
              target: "ad4m://message",
            })
          );

          const second = await Message.all(perspective!);
          expect(second.length).to.be.equal(1);
        });

        it("can constrain collection entries through 'where' clause", async () => {
          let root = Literal.from("Collection where test").toUrl();
          let todo = await perspective!.createSubject(new Todo(), root);

          let messageEntry = Literal.from("test message").toUrl();

          // @ts-ignore
          await todo.addEntries(messageEntry);

          let entries = await todo.entries;
          expect(entries.length).to.equal(1);

          let messageEntries = await todo.messages;
          expect(messageEntries.length).to.equal(0);

          await perspective!.createSubject(new Message(), messageEntry);

          messageEntries = await todo.messages;
          expect(messageEntries.length).to.equal(1);
        });
      });

      describe("with Vector Embedding", () => {
        const EMBEDDING_VECTOR_LANGUAGE = "QmzSYwdbqjGGbYbWJvdKA4WnuFwmMx3AsTfgg7EwbeNUGyE555c";
        @SDNAClass({
          name: "Embedding",
        })
        class Embedding extends SubjectEntity {
          @SubjectFlag({
            through: "flux://entry_type",
            value: "flux://has_embedding",
          })
          type: string = "";

          @SubjectProperty({
            through: "flux://embedding",
            writable: true,
            resolveLanguage: EMBEDDING_VECTOR_LANGUAGE,
          })
          embedding: any;

          @SubjectProperty({
            through: "flux://model",
            writable: true,
            resolveLanguage: "literal",
          })
          model: string = "";
        }

        before(async () => {
          // @ts-ignore
          const { name, sdna } = Recipe.generateSDNA();
          perspective!.addSdna(name, sdna, "subject_class");
        });

        it("save() & get()", async () => {
          let root = Literal.from("Vector Embedding test").toUrl();
          const embedding = new Embedding(perspective!, root);

          const rawVectorEmbedding = [
            -0.5413716, 0.5974387, 0.5129491, 0.18664218, -0.1861755, 0.085656315, 0.5215098,
            -0.24746835, -0.6430347, -0.01569271, -0.02165576, -0.25324866, 0.22480337, -0.19014868,
            0.33667958, -0.1120325, 0.01894546, -0.27940926, -0.6764706, 0.54505956, 0.5119401,
            0.3237624, 0.0047205277, -0.116946, 0.44467133, 0.011771314, -0.03708661, -0.25051358,
            -0.008421741, -1.7470672, -0.27426726, 0.03421378, 0.43204668, -0.44639337,
            -0.044898342, 0.25885978, 0.13260947, -0.28053063, 0.058713168, 0.27690077, 0.093775906,
            -0.19087604, 0.16013421, -0.7351685, -0.03028106, -0.30583733, 0.2232902, -0.016065951,
            -0.1615155, -0.32375947, -0.26523426, -0.47226027, 0.055058092, -0.0397439, 0.16290191,
            0.54276115, 0.7474365, -0.0059786215, 0.4408039, -0.08878346, 0.17489468, -0.18138516,
            -1.0133929, 0.8738722, 0.29102522, 0.19524539, -0.14175479, -0.5836162, -0.2746603,
            0.33337557, -0.1931299, 0.25817087, 0.09113194, 0.36701578, -0.275113, 0.26660612,
            -0.11256427, 0.1328292, 0.03685254, 0.41874394, -0.6070844, 0.14838715, 0.09131886,
            -0.29986614, -0.34411016, -0.39830512, 0.23409046, 0.10575019, -0.123748526,
            0.036885437, 0.13870603, -0.041074138, 0.5802447, 0.15398957, -0.49066472, -0.25104246,
            -0.36116713, -0.11275424, 0.0339157, 3.759519, -0.5644825, -0.48690718, -0.09575926,
            0.15370692, -0.27068952, -0.22003902, 0.062313832, -0.322717, 0.3085762, -0.14262284,
            0.4208305, 0.023162186, 0.12855758, -0.2834187, -0.38060334, 0.6849952, 0.57469255,
            0.009787157, -0.06544287, -0.27215353, 0.049883388, 0.06632178, -0.37825397,
            -0.87222743, 0.20129372, -0.2651834, 0.7217372, 0.57768023, 0.44572616, -0.4627049,
            0.09734708, -0.09921621, -0.52516305, 0.05863847, -0.5217986, 0.44417876, 0.10287761,
            0.55057013, 0.11682952, 0.3689786, -0.087215535, -0.5724287, -0.5423473, -0.0336574,
            -0.4195259, -0.24866524, -0.45594287, 0.32275626, 0.33199638, 0.2965936, 0.08787434,
            0.34923148, 0.15858784, 0.057337478, 0.12540832, -0.49556142, 0.1953059, 0.47052452,
            -0.027037464, -0.10555437, -0.06591491, -0.25286174, -0.27934837, -0.36611792,
            -0.14711301, -0.32234308, -0.51687926, -0.12219818, 0.07037106, -0.74558413, 0.26445925,
            0.42400435, -0.7038652, 0.036906447, -0.09007336, -0.27758795, -0.7201515, 0.29669318,
            0.09310605, 0.33384278, 0.06456944, -0.044854254, -0.34391895, 0.4094884, -0.14343314,
            -0.17308651, 0.4532166, -0.12274496, 0.49326307, 0.1045721, 1.0361375, 0.15331317,
            -0.030621171, 0.08221489, -0.32819343, -0.12024782, -0.3555685, 0.08783853, -0.15282439,
            -0.35455585, -0.49457747, 0.5373812, -0.3466457, -0.36921352, -0.44823533, 0.41074783,
            0.27740148, -0.24895224, 0.7914439, -0.3750428, -0.38974378, 0.011665132, 0.09318183,
            0.054567397, -0.3119109, -0.18734437, -0.3516543, 0.104986966, 0.23590438, -0.22841361,
            -0.2832229, -0.10654402, 0.2078313, -3.216804, 0.13740683, -0.20481409, 0.01714848,
            0.33197623, -0.22984509, 0.0013700873, 0.29294252, 0.83458006, 0.038422212, 0.49855256,
            -0.42281786, 0.17158844, 0.3022111, -0.09763882, 0.8885196, -0.40951702, -0.5215268,
            0.52945614, 0.4030705, -0.32687637, 0.46249256, 0.15252042, 0.062765524, -0.22832027,
            0.37806004, 0.8202449, 0.8706305, -0.047499277, -0.6216804, 0.5385408, 0.34541866,
            -0.16974539, -0.113647774, 0.12928979, -0.21002841, -0.17089877, 0.41334808,
            -0.36820135, 0.28659222, 0.26408842, 0.26989335, 0.20420799, -0.3608973, -0.48788488,
            0.07979423, -0.10610728, 0.116735175, -0.23233192, 0.4606759, 0.1791032, -0.24911512,
            0.028786369, 0.019584531, -0.37892678, 0.30497172, -0.32719076, -0.45698017,
            -0.31811136, -0.3133526, -0.13484725, 0.14464971, -0.28697276, 0.53140557, -0.54138005,
            0.020188892, 0.14715932, 0.32195368, 0.45060638, 0.362418, -0.36841652, 0.5400325,
            -0.11533563, -0.349285, 0.6185844, 0.5184965, 0.80876946, -1.1001662, -0.03967222,
            0.13763744, 0.1987848, 0.34872684, -0.3155128, -0.16528262, 0.48429385, 0.21350153,
            0.5278436, -0.3768565, 0.4839805, -0.2598911, -0.16542408, -0.39500543, 0.15935627,
            -0.2947644, -0.02566649, 0.30490294, -2.0424395, 0.14384143, 0.04789734, 0.17812076,
            0.16042641, 0.005340446, -0.31485057, -0.2755958, -0.16868669, 0.4210082, 0.44282886,
            -0.0496165, 0.24369837, -0.065564975, -0.1872047, 0.5980563, 0.7102051, 0.44285667,
            0.23312089, -0.24180548, 0.3047854, -0.5179677, 1.5266978, -0.11216003, -0.1624395,
            -0.6954616, -0.07699692, -0.056248218, 0.37231028, -0.03712916, 0.19097452, -0.10224208,
            1.140045, 0.05836712, -0.25410536, -0.34189942, -0.47475946, -0.18986088, -0.023036875,
            -0.073339865, -0.07181844, 0.4827925, -0.30811742, -0.81760263, 0.33078164, 0.10136539,
            -0.32741952, 0.10867341, -0.29567698, -0.3715789, -0.022593096, -0.67722744,
            -0.08501198, 0.18224041, 0.40690592, -0.09881668, 0.3076409, 0.12329048, 0.47051162,
            0.13286667, -0.07777829, 0.15944448, 0.46484095, 0.43779847, -0.09716542,
          ];

          embedding.model = "bert";
          embedding.embedding = rawVectorEmbedding;

          await embedding.save();

          const retrievedEmbedding = await embedding.get();

          expect(retrievedEmbedding.model).to.equal("bert");
          expect(retrievedEmbedding.embedding).to.equal(rawVectorEmbedding);
        });
      });

      describe("Active record implementation", () => {
        @SDNAClass({
          name: "Recipe",
        })
        class Recipe extends SubjectEntity {
          //@ts-ignore
          @SubjectFlag({
            through: "ad4m://type",
            value: "ad4m://recipe",
          })
          type: string = "";

          //@ts-ignore
          @SubjectProperty({
            through: "recipe://name",
            writable: true,
          })
          name: string = "";

          // @ts-ignore
          @SubjectProperty({
            through: "recipe://boolean",
            writable: true,
          })
          booleanTest: boolean = false;

          //@ts-ignore
          @SubjectCollection({ through: "recipe://entries" })
          entries: string[] = [];

          // @ts-ignore
          @SubjectCollection({
            through: "recipe://entries",
            where: { condition: `triple(Target, "recipe://has_ingredient", "recipe://test")` },
          })
          // @ts-ignore
          ingredients: [];

          //@ts-ignore
          @SubjectCollection({ through: "recipe://comment" })
          // @ts-ignore
          comments: string[] = [];

          //@ts-ignore
          @SubjectProperty({
            through: "recipe://local",
            writable: true,
            local: true,
          })
          local: string = "";

          @SubjectProperty({
            through: "recipe://resolve",
            writable: true,
            resolveLanguage: "literal",
          })
          resolve: string = "";
        }

        before(async () => {
          // @ts-ignore
          const { name, sdna } = Recipe.generateSDNA();
          perspective!.addSdna(name, sdna, "subject_class");
        });

        it("save() & get()", async () => {
          let root = Literal.from("Active record implementation test").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "recipe://test";
          recipe.booleanTest = false;

          await recipe.save();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();

          expect(recipe2.name).to.equal("recipe://test");
          expect(recipe2.booleanTest).to.equal(false);
        });

        it("update()", async () => {
          let root = Literal.from("Active record implementation test").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "recipe://test1";

          await recipe.update();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();

          expect(recipe2.name).to.equal("recipe://test1");
        });

        it("find()", async () => {
          const recipes = await Recipe.all(perspective!);

          expect(recipes.length).to.equal(1);
        });

        it("can constrain collection entries clause", async () => {
          let root = Literal.from("Active record implementation collection test").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "recipe://collection_test";

          recipe.comments = ["recipe://test", "recipe://test1"];

          await recipe.save();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();
          console.log("comments:", recipe2.comments);

          expect(recipe2.comments.length).to.equal(2);
        });

        it("save() & get() local", async () => {
          let root = Literal.from("Active record implementation test local link").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.name = "recipe://locallink";
          recipe.local = "recipe://test";

          await recipe.save();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();

          expect(recipe2.name).to.equal("recipe://locallink");
          expect(recipe2.local).to.equal("recipe://test");

          // @ts-ignore
          const links = await perspective?.get({
            source: root,
            predicate: "recipe://local",
          });

          expect(links!.length).to.equal(1);
          expect(links![0].status).to.equal("LOCAL");
        });

        it("query()", async () => {
          let recipes = await Recipe.query(perspective!, { page: 1, size: 2 });

          expect(recipes.length).to.equal(2);

          recipes = await Recipe.query(perspective!, { page: 2, size: 1 });

          expect(recipes.length).to.equal(1);
        });

        it("delete()", async () => {
          const recipe2 = await Recipe.all(perspective!);

          expect(recipe2.length).to.equal(3);

          await recipe2[0].delete();

          const recipe3 = await Recipe.all(perspective!);

          expect(recipe3.length).to.equal(2);
        });

        it("can constrain collection entries through 'where' clause with prolog condition", async () => {
          let root = Literal.from(
            "Active record implementation collection test with where"
          ).toUrl();
          const recipe = new Recipe(perspective!, root);

          let recipeEntries = Literal.from("test recipes").toUrl();

          recipe.entries = [recipeEntries];
          // @ts-ignore
          recipe.comments = ["recipe://test", "recipe://test1"];
          recipe.name = "recipe://collection_test";

          await recipe.save();

          await perspective?.add(
            new Link({
              source: recipeEntries,
              predicate: "recipe://has_ingredient",
              target: "recipe://test",
            })
          );

          await recipe.get();

          const recipe2 = new Recipe(perspective!, root);

          await recipe2.get();

          expect(recipe2.ingredients.length).to.equal(1);
        });

        it("can implement the resolveLanguage property type", async () => {
          let root = Literal.from("Active record implementation test resolveLanguage").toUrl();
          const recipe = new Recipe(perspective!, root);

          recipe.resolve = "Test name literal";

          await recipe.save();
          await recipe.get();

          //@ts-ignore
          let links = await perspective!.get(
            new LinkQuery({ source: root, predicate: "recipe://resolve" })
          );
          expect(links.length).to.equal(1);
          let literal = Literal.fromUrl(links[0].data.target).get();
          expect(literal.data).to.equal(recipe.resolve);
        });
      });
    });
  });

  describe("Smart Literal", () => {
    let perspective: PerspectiveProxy | null = null;

    before(async () => {
      perspective = await ad4m!.perspective.add("smart literal test");
      // for test debugging:
      console.log("UUID: " + perspective.uuid);
    });

    it("can create and use a new smart literal", async () => {
      let sl = await SmartLiteral.create(perspective!, "Hello World");
      let base = sl.base;

      expect(await sl.get()).to.equal("Hello World");

      let links = await perspective!.get(
        new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE })
      );
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      let literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal("Hello World");

      await sl.set(5);
      expect(await sl.get()).to.equal(5);

      links = await perspective!.get(new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE }));
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal(5);
    });

    it("can instantiate smart literal from perspective", async () => {
      let source = Literal.from("base").toUrl();
      let target = Literal.from("Hello World 2").toUrl();
      await perspective!.add({ source, predicate: SMART_LITERAL_CONTENT_PREDICATE, target });

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
});

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
