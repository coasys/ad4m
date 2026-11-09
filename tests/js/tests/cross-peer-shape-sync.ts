/**
 * Cross-peer SHACL shape sync — one executor registers a subject class
 * via `addSdna`, another executor never registers the class locally
 * but still queries it after joining the neighbourhood. Under the old
 * behaviour the query threw `RpcError 500: No SHACL shape stored for
 * class 'X'` because the joining peer's in-memory shape store was
 * populated only by a LOCAL `addSdna` call — even though the SHACL
 * triples themselves ARE part of the shared graph and would have
 * arrived over p-diff-sync eventually.
 *
 * The engine fix (see `PerspectiveInstance::get_shape_or_wait` and
 * `inbound_touches_shacl` in `rust-executor/src/perspectives/
 * perspective_instance.rs`) makes `model_query` on a joined
 * perspective wait bounded-time for a missing class's SHACL to arrive,
 * and drops any cached shape when an inbound diff carries new SHACL
 * triples. This test is the end-to-end proof for that behaviour.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 300000 --exit \
 *     --require tests/model/hooks.ts tests/cross-peer-shape-sync.ts
 */
import { PerspectiveProxy, Perspective, Link, LinkQuery } from "@coasys/ad4m";
import { Ad4mModel, Model, Property } from "@coasys/ad4m";
import { TestContext } from "./integration.test";
import { waitUntil } from "../helpers/index";
import fs from "fs";
import { v4 as uuidv4 } from "uuid";
import { expect } from "chai";
import { LinkLangConfig, holochainLinkLang, publishLinkLanguage } from "../utils/linkLangConfig";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

// A minimal @Model class the test can register on one peer and query
// on the other. Not exported anywhere else in the tree so a rename
// won't cascade beyond this test.
@Model({ name: "CrossPeerNote" })
class CrossPeerNote extends Ad4mModel {
  @Property({ through: "test://note/body", required: true, identity: true })
  body: string = "";
}

export default function crossPeerShapeSyncTests(
  testContext: TestContext,
  // Which link language carries the sync. Defaults to p-diff-sync; the
  // local suite passes the server-link-language config.
  getLinkLang: () => LinkLangConfig = () => holochainLinkLang(DIFF_SYNC_OFFICIAL),
) {
  return () => {
    describe("Cross-peer SHACL shape sync", function () {
      // Wait budgets sized for Holochain gossip on CI:
      //   sync warm-up      up to 180s
      //   shape/instance    up to  60s per findAll (engine budget = 20s)
      this.timeout(360_000);

      it("Bob's findAll on a class only Alice registered succeeds after sync", async () => {
        const alice = testContext.alice;
        const bob = testContext.bob;

        // Fresh neighbourhood for this test — clean slate for the
        // cross-peer barrier so we're not measuring carry-over from
        // any prior test's shared perspective.
        const aliceHandle = await alice.perspective.add(`shape-sync-${uuidv4()}`);
        const socialContext = await publishLinkLanguage(alice, getLinkLang(), "cross-peer shape sync");
        const url = await alice.neighbourhood.publishFromPerspective(
          aliceHandle.uuid,
          socialContext.address,
          new Perspective(),
        );
        const bobHandle = await bob.neighbourhood.joinFromUrl(url);
        await testContext.makeAllNodesKnown([alice, aliceHandle.uuid], [bob, bobHandle.uuid]);

        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // Alice registers the SHACL and creates one instance. Registration
        // writes `LinkStatus::Shared` links (see `add_sdna_inner`) which
        // the link language delivers to Bob.
        await CrossPeerNote.register(aliceP);
        const note = new CrossPeerNote(aliceP);
        note.body = "hello from Alice";
        await note.save();
        // Written last: once Bob holds it, he holds the shape and the note.
        const marker = `marker://${uuidv4()}`;
        await aliceP.add(
          new Link({ source: marker, predicate: "ns://ping", target: "literal:string:ok" }),
        );
        await waitUntil(
          async () => (await bobP.get(new LinkQuery({ source: marker }))).length > 0,
          180_000,
          "Alice's shape, note and marker to reach Bob",
        );

        // Bob does NOT call `CrossPeerNote.register(bobP)` — this is the
        // whole point. Without the engine fix, this findAll throws
        // `No SHACL shape stored for class 'CrossPeerNote'`.
        const notes = await CrossPeerNote.findAll(bobP);
        expect(notes.length, "Bob should see Alice's note").to.equal(1);
        expect(notes[0].body).to.equal("hello from Alice");
      });
    });
  };
}
