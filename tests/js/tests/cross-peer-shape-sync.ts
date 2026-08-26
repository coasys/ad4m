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
import { pollUntil } from "../utils/utils";
import { waitUntil } from "../helpers/index";
import fs from "fs";
import { v4 as uuidv4 } from "uuid";
import { expect } from "chai";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

// A minimal @Model class the test can register on one peer and query
// on the other. Not exported anywhere else in the tree so a rename
// won't cascade beyond this test.
@Model({ name: "CrossPeerNote" })
class CrossPeerNote extends Ad4mModel {
  @Property({ through: "test://note/body", required: true, identity: true })
  body: string = "";
}

export default function crossPeerShapeSyncTests(testContext: TestContext) {
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
        const socialContext = await alice.languages.applyTemplateAndPublish(
          DIFF_SYNC_OFFICIAL,
          JSON.stringify({ uid: uuidv4(), name: "cross-peer shape sync" }),
        );
        const url = await alice.neighbourhood.publishFromPerspective(
          aliceHandle.uuid,
          socialContext.address,
          new Perspective(),
        );
        const bobHandle = await bob.neighbourhood.joinFromUrl(url);
        await testContext.makeAllNodesKnown();

        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // Prove the sync path is live BEFORE the assertions run. A cold
        // gossip peer failing to warm up manifests as a shape-sync
        // timeout downstream, which would be misdiagnosed.
        const marker = `warmup://${uuidv4()}`;
        await aliceP.add(
          new Link({ source: marker, predicate: "ns://ping", target: "literal:string:ok" }),
        );
        await waitUntil(
          async () => (await bobP.get(new LinkQuery({ source: marker }))).length > 0,
          180_000,
          "gossip warm-up: Alice's marker link to reach Bob",
        );

        // Alice registers the SHACL and creates one instance. Registration
        // writes `LinkStatus::Shared` links (see `add_sdna_inner`) which
        // p-diff-sync will deliver to Bob.
        await CrossPeerNote.register(aliceP);
        const note = new CrossPeerNote(aliceP);
        note.body = "hello from Alice";
        await note.save();

        // Bob does NOT call `CrossPeerNote.register(bobP)` — this is the
        // whole point. Without the engine fix, this findAll throws
        // `No SHACL shape stored for class 'CrossPeerNote'` immediately.
        // With the fix, `model_query` polls up to `MODEL_QUERY_SHAPE_WAIT`
        // (20s) for Alice's SDNA to arrive over sync, then succeeds.
        //
        // Extra tolerance beyond the engine's 20s wait so a cold
        // Holochain gossip cycle can still land: retry the query in a
        // waitUntil loop. Any single call may throw "no shape" if the
        // shape hasn't arrived within the engine budget — that's the
        // caller's cue to try again.
        const notes = await waitUntilFindAllSucceeds(bobP);
        expect(notes.length, "Bob should see Alice's note").to.equal(1);
        expect(notes[0].body).to.equal("hello from Alice");
      });

      /**
       * Wrap `findAll` in a bounded retry loop. Each `model_query` call
       * already waits up to 20s internally; here we tolerate one full
       * engine budget expiry then retry, up to a 90s ceiling.
       */
      async function waitUntilFindAllSucceeds(p: PerspectiveProxy): Promise<CrossPeerNote[]> {
        let results: CrossPeerNote[] = [];
        await pollUntil(async () => {
          try {
            results = await CrossPeerNote.findAll(p);
            return results.length > 0;
          } catch { return false; }
        }, { timeoutMs: 90000, intervalMs: 2000, label: "CrossPeerNote.findAll succeeds on non-registering peer" });
        return results;
      }
    });
  };
}
