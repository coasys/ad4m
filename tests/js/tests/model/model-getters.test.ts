/**
 * Ad4mModel — custom getter integration tests
 *
 * Covers:
 *   - @Property(getter:) — custom SurrealQL expression for computed properties
 *   - @HasMany(getter:)  — custom SurrealQL expression for computed relations
 *   - None / empty-value filtering from getter results
 *
 * Run standalone:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-getters.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
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
