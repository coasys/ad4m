/**
 * Ad4mModel — `linkStatus`: read an instance from links of one status only
 * (issue #1116)
 *
 * A Local link is executor-private: it is never gossiped. #1028 made
 * `local: true` properties read only their Local links, but nothing let a
 * caller ask for the converse, "this instance as it exists in Shared links".
 * A multi-user read (#1024) needs exactly that, for every property, including
 * ordinary ones that happen to have a Local link.
 *
 * The card here has an ordinary `title` written Shared and an ordinary `note`
 * whose only link was written Local. Neither is declared `local`, so the
 * pre-#1116 read returns both, whoever asks.
 *
 * A typed relation is read from links of the same status. `remarks` below is a
 * `@HasMany` to a flagged class, so the SDK gives it a generated conformance
 * getter, and the relation is filled by that getter rather than by the
 * filtered hydration read. Its only link is Local.
 *
 * The last case pins how `linkStatus` combines with #1113's
 * `includeUnverified`: both apply to the same link, so a Local link whose
 * signature does not verify is read only with `includeUnverified`, and never
 * under `linkStatus: 'shared'`.
 *
 * Which instances are *selected* follows the same rule (#1120): a Shared-only
 * `where` on the Local note does not return the card, nor count it. For the
 * same reason the card is flagged in a Local link as well as a Shared one, so
 * that it is an instance under `linkStatus: 'local'` too.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-link-status.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  HasMany,
  Link,
  LinkExpression,
  LinkQuery,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

@Model({ name: "LinkStatusRemark" })
class LinkStatusRemark extends Ad4mModel {
  @Flag({ through: "lsc://type", value: "lsc://remark" })
  type = "lsc://remark";

  @Property({ through: "lsc://body" })
  body: string = "";
}

@Model({ name: "LinkStatusCard" })
class LinkStatusCard extends Ad4mModel {
  @Flag({ through: "lsc://type", value: "lsc://card" })
  type = "lsc://card";

  @Property({ through: "lsc://title" })
  title: string = "";

  /** Not declared `local`: its link is Local only because it was written so. */
  @Property({ through: "lsc://note" })
  note: string = "";

  /** Typed relation: filled by its generated conformance getter. */
  @HasMany(() => LinkStatusRemark, { through: "lsc://remark" })
  remarks: string[] = [];
}

describe("Ad4mModel — linkStatus reads", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let cardId: string;
  let remarkId: string;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-link-status");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-link-status-test");
    await LinkStatusRemark.register(perspective);
    await LinkStatusCard.register(perspective);

    const card = new LinkStatusCard(perspective);
    card.title = "shared title";
    await card.save();
    cardId = card.id;

    // `save()` writes the empty default `note` as a Shared link. Drop it, so the
    // note's only link is the Local one below.
    const savedNotes = await perspective.get(
      new LinkQuery({ source: cardId, predicate: "lsc://note" })
    );
    if (savedNotes.length) await perspective.removeLinks(savedNotes);

    await perspective.add(
      new Link({
        source: cardId,
        predicate: "lsc://note",
        target: Literal.from("local note").toUrl(),
      }),
      "local"
    );

    // The card's flag, written Local as well. Selection reads links of the
    // requested status (#1120), so without it the card is not an instance
    // under `linkStatus: 'local'`.
    await perspective.add(
      new Link({ source: cardId, predicate: "lsc://type", target: "lsc://card" }),
      "local"
    );

    // A conforming remark (its own links Shared), related to the card only by
    // a Local link.
    const remark = new LinkStatusRemark(perspective);
    remark.body = "shared body";
    await remark.save();
    remarkId = remark.id;
    // Flagged Local as well, so it is a Remark under `linkStatus: 'local'`:
    // the typed relation's generated getter checks the target's flag under
    // the same status (#1120).
    await perspective.add(
      new Link({ source: remarkId, predicate: "lsc://type", target: "lsc://remark" }),
      "local"
    );
    await perspective.add(
      new Link({ source: cardId, predicate: "lsc://remark", target: remarkId }),
      "local"
    );

    // Precondition: one Shared and one Local property link, one Local relation link.
    const status = async (predicate: string) =>
      (await perspective.get(new LinkQuery({ source: cardId, predicate }))).map((l) =>
        String(l.status ?? "shared").toLowerCase()
      );
    expect(await status("lsc://title")).to.deep.equal(["shared"]);
    expect(await status("lsc://note")).to.deep.equal(["local"]);
    expect(await status("lsc://remark")).to.deep.equal(["local"]);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  const find = async (query: object) => {
    const all = await LinkStatusCard.findAll(perspective, query);
    const card = all.find((c) => c.id === cardId);
    expect(card, "the card is returned").to.exist;
    return card!;
  };

  it("reads both statuses by default", async () => {
    const card = await find({});
    expect(card.title).to.equal("shared title");
    expect(card.note).to.equal("local note");
  });

  it("returns only the Shared property with linkStatus: 'shared'", async () => {
    const card = await find({ linkStatus: "shared" });
    expect(card.title).to.equal("shared title");
    expect(card.note, "the Local note must not be read").to.not.equal("local note");

    const viaBuilder = await LinkStatusCard.query(perspective)
      .where({ id: cardId })
      .linkStatus("shared")
      .get();
    expect(viaBuilder[0]?.title).to.equal("shared title");
    expect(viaBuilder[0]?.note).to.not.equal("local note");
  });

  it("returns only the Local property with linkStatus: 'local'", async () => {
    const card = await find({ linkStatus: "local" });
    expect(card.note).to.equal("local note");
    expect(card.title).to.not.equal("shared title");
  });

  it("does not select or count by a Local value under linkStatus: 'shared'", async () => {
    const byNote = { where: { note: "local note" } };
    const ids = async (query: object) =>
      (await LinkStatusCard.findAll(perspective, query)).map((c) => c.id);

    expect(await ids(byNote), "without linkStatus the note selects").to.include(cardId);
    expect(await ids({ ...byNote, linkStatus: "shared" })).to.not.include(cardId);
    expect(await ids({ ...byNote, linkStatus: "shared", limit: 10 })).to.not.include(cardId);
    expect(await LinkStatusCard.count(perspective, byNote)).to.equal(1);
    expect(
      await LinkStatusCard.count(perspective, { ...byNote, linkStatus: "shared" }),
      "a Local value must not be counted under 'shared'"
    ).to.equal(0);
  });

  it("restricts the `__links` rows to the same status", async () => {
    const targets = (card: LinkStatusCard, key: string) =>
      (card.__links?.[key] ?? []).map((l) => l.data.target);
    const note = Literal.from("local note").toUrl();

    const both = await find({ links: ["note"] });
    expect(targets(both, "note")).to.deep.equal([note]);

    const shared = await find({ linkStatus: "shared", links: ["note", "title"] });
    expect(targets(shared, "note"), "the Local note link must not be listed").to.deep.equal([]);
    expect(targets(shared, "title")).to.have.length(1);

    const viaBuilder = await LinkStatusCard.query(perspective)
      .where({ id: cardId })
      .linkStatus("shared")
      .links(["note"])
      .get();
    expect(targets(viaBuilder[0], "note")).to.deep.equal([]);
  });

  it("reads a typed relation from links of the same status", async () => {
    const both = await find({});
    expect(both.remarks).to.deep.equal([remarkId]);

    const local = await find({ linkStatus: "local" });
    expect(local.remarks).to.deep.equal([remarkId]);

    const shared = await find({ linkStatus: "shared" });
    expect(shared.remarks ?? [], "the Local relation link must not be read").to.not.include(
      remarkId
    );

    const sharedIncluded = await find({ linkStatus: "shared", include: { remarks: true } });
    const included = (sharedIncluded.remarks ?? []) as unknown[];
    expect(
      included.map((r) => (typeof r === "string" ? r : (r as LinkStatusRemark).id)),
      "nor hydrate its target through `include`"
    ).to.not.include(remarkId);

    const viaBuilder = await LinkStatusCard.query(perspective)
      .where({ id: cardId })
      .linkStatus("shared")
      .get();
    expect(viaBuilder[0]?.remarks ?? []).to.not.include(remarkId);
  });

  // Runs last: it adds a forged Local `note` link the cases above must not see.
  it("combines with includeUnverified on the same link", async () => {
    // The agent's own signed Local note, with its target swapped after signing
    // and its timestamp moved later, written Local. It does not verify.
    const [signed] = await perspective.get(
      new LinkQuery({ source: cardId, predicate: "lsc://note" })
    );
    const forged = {
      author: signed.author,
      timestamp: new Date(Date.parse(signed.timestamp) + 60_000).toISOString(),
      data: {
        source: signed.data.source,
        predicate: signed.data.predicate,
        target: Literal.from("unverified local note").toUrl(),
      },
      proof: { key: signed.proof.key, signature: signed.proof.signature },
    } as LinkExpression;
    await perspective.addLinkExpression(forged, "local");
    const stored = (
      await perspective.get(new LinkQuery({ source: cardId, predicate: "lsc://note" }))
    ).find((l) => l.data.target === forged.data.target);
    expect(stored, "the forged link must be stored").to.exist;
    expect(String(stored!.status).toLowerCase()).to.equal("local");
    expect(stored!.proof.valid, "the forged link must not verify").to.not.equal(true);

    const unverified = forged.data.target;
    const cases: [object, boolean][] = [
      [{}, false],
      [{ includeUnverified: true }, true],
      [{ linkStatus: "local" }, false],
      [{ linkStatus: "local", includeUnverified: true }, true],
      [{ linkStatus: "shared" }, false],
      [{ linkStatus: "shared", includeUnverified: true }, false],
    ];
    for (const [flags, read] of cases) {
      const card = await find({ ...flags, links: ["note"] });
      const rows = (card.__links?.note ?? []).map((l) => l.data.target);
      const label = JSON.stringify(flags);
      if (read) {
        expect(card.note, label).to.equal("unverified local note");
        expect(rows, label).to.include(unverified);
      } else {
        expect(card.note, label).to.not.equal("unverified local note");
        expect(rows, label).to.not.include(unverified);
      }
    }

    const viaBuilder = await LinkStatusCard.query(perspective)
      .where({ id: cardId })
      .linkStatus("local")
      .includeUnverified()
      .get();
    expect(viaBuilder[0]?.note).to.equal("unverified local note");
  });
});
