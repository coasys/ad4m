/**
 * Ad4mModel — advanced feature integration tests
 *
 * Covers:
 *   - @Property(getter:) and @HasMany(getter:) — custom SurrealQL getters
 *   - @HasMany(where: { isInstance: ... }) — relation filtering by subject class
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
  Link,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import fetch from "node-fetch";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { sleep } from "../../utils/utils.js";

//@ts-ignore
global.fetch = fetch;

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

  // ── isInstance filtering tests ─────────────────────────────────────────────

  describe("isInstance filtering tests", () => {
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

      @HasMany({
        through: "article://has_comment",
        where: { isInstance: Comment },
      })
      comments: string[] = [];
    }

    @Model({ name: "ArticleWithString" })
    class ArticleWithString extends Ad4mModel {
      @Property({ through: "article://title", resolveLanguage: "literal" })
      title: string = "";

      @HasMany({
        through: "article://has_comment",
        where: { isInstance: "Comment" },
      })
      comments: string[] = [];
    }

    beforeEach(async () => {
      if (perspective) {
        await ad4m.perspective.remove(perspective.uuid);
      }
      perspective = await ad4m.perspective.add("isInstance-test");
      await perspective.ensureSDNASubjectClass(Comment);
      await perspective.ensureSDNASubjectClass(Article);
      await perspective.ensureSDNASubjectClass(ArticleWithString);
      await sleep(200);
    });

    it("should filter relation by isInstance with class reference", async () => {
      const articleRoot = Literal.from("Article for isInstance test").toUrl();
      const validComment1 = Literal.from("Valid comment 1").toUrl();
      const validComment2 = Literal.from("Valid comment 2").toUrl();
      const invalidItem = Literal.from("Invalid item").toUrl();

      const article = new Article(perspective, articleRoot);
      article.title = "Test Article";
      await article.save();

      const comment1 = new Comment(perspective, validComment1);
      comment1.text = "This is a valid comment";
      await comment1.save();

      const comment2 = new Comment(perspective, validComment2);
      comment2.text = "This is another valid comment";
      await comment2.save();

      await sleep(1500);

      await perspective.add(
        new Link({
          source: articleRoot,
          predicate: "article://has_comment",
          target: validComment1,
        }),
      );
      await perspective.add(
        new Link({
          source: articleRoot,
          predicate: "article://has_comment",
          target: invalidItem,
        }),
      );
      await perspective.add(
        new Link({
          source: articleRoot,
          predicate: "article://has_comment",
          target: validComment2,
        }),
      );

      const retrievedArticle = new Article(perspective, articleRoot);
      await retrievedArticle.get();

      expect(retrievedArticle.comments).to.have.lengthOf(2);
      expect(retrievedArticle.comments).to.include(validComment1);
      expect(retrievedArticle.comments).to.include(validComment2);
      expect(retrievedArticle.comments).to.not.include(invalidItem);
    });

    it("should filter relation by isInstance with string class name", async () => {
      const articleRoot = Literal.from(
        "Article for string isInstance test",
      ).toUrl();
      const validComment = Literal.from("Valid comment").toUrl();
      const invalidItem = Literal.from("Invalid item").toUrl();

      const article = new ArticleWithString(perspective, articleRoot);
      article.title = "Test Article with String";
      await article.save();

      const comment = new Comment(perspective, validComment);
      comment.text = "Valid comment text";
      await comment.save();

      await perspective.add(
        new Link({
          source: articleRoot,
          predicate: "article://has_comment",
          target: validComment,
        }),
      );
      await perspective.add(
        new Link({
          source: articleRoot,
          predicate: "article://has_comment",
          target: invalidItem,
        }),
      );

      const retrievedArticle = new ArticleWithString(perspective, articleRoot);
      await retrievedArticle.get();

      expect(retrievedArticle.comments).to.have.lengthOf(1);
      expect(retrievedArticle.comments[0]).to.equal(validComment);
    });

    it("should filter results in findAll() by isInstance", async () => {
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

      const article1 = new Article(perspective, article1Root);
      article1.title = "Article 1";
      await article1.save();

      const article2 = new Article(perspective, article2Root);
      article2.title = "Article 2";
      await article2.save();

      const c1 = new Comment(perspective, comment1);
      c1.text = "Comment 1 text";
      await c1.save();

      const c2 = new Comment(perspective, comment2);
      c2.text = "Comment 2 text";
      await c2.save();

      await perspective.add(
        new Link({
          source: article1Root,
          predicate: "article://has_comment",
          target: comment1,
        }),
      );
      await perspective.add(
        new Link({
          source: article1Root,
          predicate: "article://has_comment",
          target: invalid1,
        }),
      );
      await perspective.add(
        new Link({
          source: article2Root,
          predicate: "article://has_comment",
          target: comment2,
        }),
      );
      await perspective.add(
        new Link({
          source: article2Root,
          predicate: "article://has_comment",
          target: invalid2,
        }),
      );

      const articles = await Article.findAll(perspective);

      expect(articles).to.have.lengthOf(2);

      const foundArticle1 = articles.find((a) => a.title === "Article 1");
      const foundArticle2 = articles.find((a) => a.title === "Article 2");

      expect(foundArticle1).to.not.be.undefined;
      expect(foundArticle2).to.not.be.undefined;

      expect(foundArticle1!.comments).to.have.lengthOf(1);
      expect(foundArticle1!.comments[0]).to.equal(comment1);

      expect(foundArticle2!.comments).to.have.lengthOf(1);
      expect(foundArticle2!.comments[0]).to.equal(comment2);
    });
  });
});
