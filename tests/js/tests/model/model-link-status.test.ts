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
 * The last case pins how `linkStatus` combines with #1113's
 * `includeUnverified`: both apply to the same link, so a Local link whose
 * signature does not verify is read only with `includeUnverified`, and never
 * under `linkStatus: 'shared'`.
 *
 * Not covered here, and still open: which instances are *selected* (`where`,
 * the class's flags, `count`) is not restricted by `linkStatus`. See the
 * `#[ignore]`d Rust test `link_status_shared_does_not_select_on_a_local_value`
 * and #1120.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-link-status.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
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

@Model({ name: "LinkStatusCard" })
class LinkStatusCard extends Ad4mModel {
  @Flag({ through: "lsc://type", value: "lsc://card" })
  type = "lsc://card";

  @Property({ through: "lsc://title" })
  title: string = "";

  /** Not declared `local`: its link is Local only because it was written so. */
  @Property({ through: "lsc://note" })
  note: string = "";
}

describe("Ad4mModel — linkStatus reads", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let cardId: string;

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

    // Precondition: one Shared and one Local property link.
    const status = async (predicate: string) =>
      (await perspective.get(new LinkQuery({ source: cardId, predicate }))).map((l) =>
        String(l.status ?? "shared").toLowerCase()
      );
    expect(await status("lsc://title")).to.deep.equal(["shared"]);
    expect(await status("lsc://note")).to.deep.equal(["local"]);
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
