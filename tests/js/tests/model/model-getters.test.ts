/**
 * Ad4mModel — custom getter integration tests
 *
 * Covers:
 *   - @Property(getter:) — custom SPARQL expression for computed properties
 *   - @HasMany(getter:)  — custom SPARQL expression for computed relations
 *   - @HasMany(where:)   — DSL-compiled getter for relation filtering
 *   - None / empty-value filtering from getter results
 *
 * Run standalone:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-getters.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  HasMany,
  Link,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

describe("Ad4mModel — Custom Getters", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  @Model({ name: "BlogPost" })
  class BlogPost extends Ad4mModel {
    @Property({ through: "blog://title" })
    title: string = "";

    @Property({
      through: "blog://parent",
      getter:
        "SELECT ?target WHERE { <Base> <blog://reply_to> ?target . } LIMIT 1",
    })
    parentPost: string | undefined;

    @HasMany({
      getter:
        "SELECT ?target WHERE { <Base> <blog://tagged_with> ?target . }",
    })
    tags: string[] = [];
  }

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-getters");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    if (perspective) {
      await ad4m.perspective.remove(perspective.uuid);
    }
    perspective = await ad4m.perspective.add("getter-test");
    await perspective.ensureSDNASubjectClass(BlogPost);
  });

  it("should evaluate getter for property", async () => {
    const postRoot = Literal.from("Blog post for getter property test").toUrl();
    const parentRoot = Literal.from("Parent blog post").toUrl();

    const post = new BlogPost(perspective, postRoot);
    post.title = "Reply Post";
    await post.save();

    const parent = new BlogPost(perspective, parentRoot);
    parent.title = "Original Post";
    await parent.save();

    // Create the link that getter should find
    await perspective.add(
      new Link({
        source: postRoot,
        predicate: "blog://reply_to",
        target: parentRoot,
      }),
    );

    const retrievedPost = new BlogPost(perspective, postRoot);
    await retrievedPost.get();

    expect(retrievedPost.parentPost).to.equal(parentRoot);
  });

  it("should evaluate getter for relation", async () => {
    const postRoot = Literal.from("Blog post for getter relation test").toUrl();
    const tag1 = Literal.from("tag:javascript").toUrl();
    const tag2 = Literal.from("tag:typescript").toUrl();

    const post = new BlogPost(perspective, postRoot);
    post.title = "Test Post";
    await post.save();

    await perspective.add(
      new Link({
        source: postRoot,
        predicate: "blog://tagged_with",
        target: tag1,
      }),
    );
    await perspective.add(
      new Link({
        source: postRoot,
        predicate: "blog://tagged_with",
        target: tag2,
      }),
    );

    const retrievedPost = new BlogPost(perspective, postRoot);
    await retrievedPost.get();

    expect(retrievedPost.tags).to.include(tag1);
    expect(retrievedPost.tags).to.include(tag2);
    expect(retrievedPost.tags.length).to.equal(2);
  });

  it("should filter out 'None' and empty values from getter results", async () => {
    const postRoot = Literal.from("Blog post for None filtering test").toUrl();

    const post = new BlogPost(perspective, postRoot);
    post.title = "Post without parent";
    await post.save();

    const retrievedPost = new BlogPost(perspective, postRoot);
    await retrievedPost.get();

    // Property should be undefined, not 'None' or empty string
    expect(retrievedPost.parentPost).to.be.undefined;
  });
});

// ── where-clause compiled getter ────────────────────────────────────────────

describe("Ad4mModel — Where-Clause Relation Filtering", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  // Target model — has a status flag so we can filter by it
  @Model({ name: "Task" })
  class Task extends Ad4mModel {
    @Flag({ through: "task://type", value: "task://task" })
    type = "task://task";

    @Property({ through: "task://title", required: true })
    title: string = "";

    @Property({ through: "task://status", required: true })
    status: string = "";
  }

  // Parent model — uses `where` to only include active tasks
  @Model({ name: "TaskBoard" })
  class TaskBoard extends Ad4mModel {
    @Property({ through: "board://name" })
    name: string = "";

    @HasMany(() => Task, {
      through: "board://has_task",
      where: { status: "active" },
    })
    activeTasks: string[] = [];

    // Unfiltered relation for comparison
    @HasMany(() => Task, { through: "board://has_task" })
    allTasks: string[] = [];
  }

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-where-getter");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    if (perspective) {
      await ad4m.perspective.remove(perspective.uuid);
    }
    perspective = await ad4m.perspective.add("where-getter-test");
    await perspective.ensureSDNASubjectClass(Task);
    await perspective.ensureSDNASubjectClass(TaskBoard);
  });

  it("where-compiled getter should only return matching children", async () => {
    // Create a board
    const board = await TaskBoard.create(perspective, { name: "Sprint 1" });

    // Create three tasks with different statuses
    const active1 = await Task.create(perspective, { title: "Active 1", status: "active" });
    const active2 = await Task.create(perspective, { title: "Active 2", status: "active" });
    const done = await Task.create(perspective, { title: "Done Task", status: "done" });

    // Link all three tasks to the board
    for (const task of [active1, active2, done]) {
      await perspective.add(
        new Link({ source: board.id, predicate: "board://has_task", target: task.id }),
      );
    }

    // DIAGNOSTIC: dump actual SPARQL links for active1 to see stored format
    const sparqlDump = await perspective.querySparql(
      `SELECT ?p ?o WHERE { <${active1.id}> ?p ?o . } LIMIT 20`
    );
    console.log("[WHERE_TEST_DEBUG] active1 links:", JSON.stringify(sparqlDump, null, 2));

    // Also dump board→task links
    const boardLinks = await perspective.querySparql(
      `SELECT ?target WHERE { <${board.id}> <board://has_task> ?target . }`
    );
    console.log("[WHERE_TEST_DEBUG] board->task links:", JSON.stringify(boardLinks));

    // Retrieve the board and check filtered vs unfiltered relations
    const retrieved = new TaskBoard(perspective, board.id);
    await retrieved.get();

    // DIAGNOSTIC: log actual values so CI output shows what's happening
    console.log("[WHERE_TEST_DEBUG] activeTasks:", JSON.stringify(retrieved.activeTasks));
    console.log("[WHERE_TEST_DEBUG] allTasks:", JSON.stringify(retrieved.allTasks));
    console.log("[WHERE_TEST_DEBUG] board.id:", board.id);
    console.log("[WHERE_TEST_DEBUG] active1.id:", active1.id);
    console.log("[WHERE_TEST_DEBUG] active2.id:", active2.id);
    console.log("[WHERE_TEST_DEBUG] done.id:", done.id);

    // allTasks (no where filter) should have all 3 — check first to isolate conformance vs where-clause
    expect(retrieved.allTasks).to.have.lengthOf(3);

    // activeTasks (where: status = "active") should only have 2
    expect(retrieved.activeTasks).to.have.lengthOf(2);
    expect(retrieved.activeTasks).to.include(active1.id);
    expect(retrieved.activeTasks).to.include(active2.id);
    expect(retrieved.activeTasks).to.not.include(done.id);
  });
});
