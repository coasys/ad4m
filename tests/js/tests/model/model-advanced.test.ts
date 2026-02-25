/**
 * Ad4mModel — advanced feature integration tests
 *
 * Covers:
 *   - @Property(getter:) and @HasMany(getter:) — custom SurrealQL getters
 *   - @HasMany(() => Model, ...) — typed relation with eager hydration via include
 *
 * Extracted from sdna.test.ts as part of the test suite restructure.
 *
 * Run standalone:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-advanced.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  HasMany,
  HasManyMethods,
  Link,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { sleep } from "../../utils/utils.js";

describe("Ad4mModel — Advanced Features", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  // perspective is reassigned per-describe-block via beforeEach
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-advanced");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  // ── Getter feature tests ───────────────────────────────────────────────────

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
        await ad4m.perspective.remove(perspective.uuid);
      }
      perspective = await ad4m.perspective.add("getter-test");
      await perspective.ensureSDNASubjectClass(BlogPost);
    });

    it("should evaluate getter for property", async () => {
      const postRoot = Literal.from(
        "Blog post for getter property test",
      ).toUrl();
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
      const postRoot = Literal.from(
        "Blog post for getter relation test",
      ).toUrl();
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
      const postRoot = Literal.from(
        "Blog post for None filtering test",
      ).toUrl();

      const post = new BlogPost(perspective, postRoot);
      post.title = "Post without parent";
      await post.save();

      const retrievedPost = new BlogPost(perspective, postRoot);
      await retrievedPost.get();

      // Property should be undefined, not 'None' or empty string
      expect(retrievedPost.parentPost).to.be.undefined;
    });
  });

  // ── Typed relation hydration tests ────────────────────────────────────────

  describe("typed relation hydration tests", () => {
    @Model({ name: "Comment" })
    class Comment extends Ad4mModel {
      @Flag({ through: "ad4m://type", value: "ad4m://comment" })
      type!: string;

      @Property({ through: "comment://text", resolveLanguage: "literal" })
      text: string = "";
    }

    @Model({ name: "Article" })
    class Article extends Ad4mModel {
      @Property({ through: "article://title", resolveLanguage: "literal" })
      title: string = "";

      @HasMany(() => Comment, { through: "article://has_comment" })
      comments: Comment[] = [];
    }
    interface Article extends HasManyMethods<"comments"> {}

    beforeEach(async () => {
      if (perspective) {
        await ad4m.perspective.remove(perspective.uuid);
      }
      perspective = await ad4m.perspective.add("typed-relation-test");
      await perspective.ensureSDNASubjectClass(Comment);
      await perspective.ensureSDNASubjectClass(Article);
      await sleep(200);
    });

    it("include: { comments: true } hydrates linked Comments as typed instances", async () => {
      const article = await Article.create(perspective, {
        title: "Test Article",
      });
      const comment1 = await Comment.create(perspective, {
        text: "First comment",
      });
      const comment2 = await Comment.create(perspective, {
        text: "Second comment",
      });

      await article.addComments(comment1);
      await article.addComments(comment2);

      const retrieved = await Article.findOne(perspective, {
        where: { id: article.id },
        include: { comments: true },
      });

      expect(retrieved).to.not.be.null;
      expect(retrieved!.comments).to.have.lengthOf(2);
      expect(retrieved!.comments[0]).to.be.instanceOf(Comment);
      expect(retrieved!.comments.some((c) => c.id === comment1.id)).to.be.true;
      expect(retrieved!.comments.some((c) => c.id === comment2.id)).to.be.true;
    });

    it("links to non-Comment nodes are excluded when hydrating with include", async () => {
      const article = await Article.create(perspective, {
        title: "Article with mixed links",
      });
      const validComment = await Comment.create(perspective, { text: "Valid" });
      // A bare URI with no Comment SDNA — adding the link manually
      const invalidItem = Literal.from("not-a-comment").toUrl();

      await article.addComments(validComment);
      // Manually add a link to a non-Comment target
      await perspective.add(
        new Link({
          source: article.id,
          predicate: "article://has_comment",
          target: invalidItem,
        }),
      );

      const retrieved = await Article.findOne(perspective, {
        where: { id: article.id },
        include: { comments: true },
      });

      expect(retrieved).to.not.be.null;
      expect(retrieved!.comments).to.have.lengthOf(1);
      expect(retrieved!.comments[0].id).to.equal(validComment.id);
    });

    it("findAll() with include hydrates comments across multiple articles", async () => {
      const article1 = await Article.create(perspective, {
        title: "Article 1",
      });
      const article2 = await Article.create(perspective, {
        title: "Article 2",
      });

      const c1 = await Comment.create(perspective, { text: "Comment on 1" });
      const c2 = await Comment.create(perspective, { text: "Comment on 2" });
      const invalid1 = Literal.from("not-a-comment-1").toUrl();
      const invalid2 = Literal.from("not-a-comment-2").toUrl();

      await article1.addComments(c1);
      await article2.addComments(c2);
      // Manually add non-Comment links to both
      await perspective.add(
        new Link({
          source: article1.id,
          predicate: "article://has_comment",
          target: invalid1,
        }),
      );
      await perspective.add(
        new Link({
          source: article2.id,
          predicate: "article://has_comment",
          target: invalid2,
        }),
      );

      const articles = await Article.findAll(perspective, {
        include: { comments: true },
      });

      expect(articles).to.have.lengthOf(2);

      const found1 = articles.find((a) => a.title === "Article 1");
      const found2 = articles.find((a) => a.title === "Article 2");

      expect(found1).to.not.be.undefined;
      expect(found2).to.not.be.undefined;

      expect(found1!.comments).to.have.lengthOf(1);
      expect(found1!.comments[0]).to.be.instanceOf(Comment);
      expect(found1!.comments[0].id).to.equal(c1.id);

      expect(found2!.comments).to.have.lengthOf(1);
      expect(found2!.comments[0]).to.be.instanceOf(Comment);
      expect(found2!.comments[0].id).to.equal(c2.id);
    });
  });
});
