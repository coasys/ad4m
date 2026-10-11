/**
 * Integration test: producedByFlow — `perspective.mintFlowReceipt`,
 * `perspective.flowValidOutputs`, `perspective.verifyFlowReceipt`, and the
 * model-query `where: { producedByFlow }` filter, over one executor.
 *
 * The Rust e2e suite (flow_instance_e2e.rs) carries the adversarial cases
 * (forged re-mints, edited outputs, page cutting); this test pins the WIRE:
 * that a client can drive complete → mint → enumerate → verify → filter
 * through the TS SDK and gets the documented shapes back.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 900000 --exit \
 *     tests/flow/flow-produced-by.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy, SHACLFlow } from "@coasys/ad4m";
import { FlowInstance } from "@coasys/ad4m";
import { Ad4mModel, Model, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";

// ── Domain models ──────────────────────────────────────────────────────────

@Model({ name: "ProducedTask" })
class Task extends Ad4mModel {
  @Property({ through: "produced-test://title", required: true })
  title: string = "";
}

@Model({ name: "ProducedDeliverable" })
class Deliverable extends Ad4mModel {
  @Property({ through: "produced-test://deliverable_title", required: true })
  title: string = "";
}

/** Ready → Done, `Done` terminal and guard-free — the smallest flow that can
 *  complete and commit outputs. */
function makeFlow(): SHACLFlow {
  const flow = new SHACLFlow("ProducedDelivery", "produced-test://");
  flow.inputTypes = ["ProducedTask"];
  flow.consensusRule = { n: 1 };
  flow.addState({ name: "Ready", value: 0 });
  flow.addState({ name: "Done", value: 1 });
  flow.addTransition({ actionName: "Finish", fromState: "Ready", toState: "Done", actions: [] });
  return flow;
}

const FLOW_URI = "produced-test://ProducedDeliveryFlow";

describe("producedByFlow — receipts, valid outputs, and the query filter", function () {
  this.timeout(900_000);

  let agent: AgentHandle;
  let admin: Ad4mClient;

  before(async () => {
    agent = await startAgent("flow-produced-by");
    admin = agent.client;
  });

  after(async () => {
    if (agent) await agent.stop();
  });

  it("complete → mint → enumerate → verify → filter, through the SDK", async () => {
    const handle = await admin.perspective.add("flow-produced-by");
    const p = (await admin.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await (Task as any).register(p);
    await (Deliverable as any).register(p);
    await p.addFlow("ProducedDelivery", makeFlow());

    const task = (await (Task as any).create(p, { title: "Ship producedByFlow" })) as Task;
    const deliverable = (await (Deliverable as any).create(p, {
      title: "produced.rs",
    })) as Deliverable;

    // Before anything completed: the filter admits nothing (fail-closed
    // baseline — no receipt, no output), while an unfiltered query sees the
    // instance.
    const unfiltered = await (Deliverable as any).findAll(p, {});
    expect(unfiltered.map((d: Deliverable) => d.id)).to.include(deliverable.id);
    const before = await (Deliverable as any).findAll(p, {
      where: { producedByFlow: { flow: FLOW_URI } },
    });
    expect(before, "no completed run, no valid outputs").to.be.empty;
    expect(await p.flowValidOutputs(FLOW_URI)).to.be.empty;

    // Complete the run, committing to the deliverable as its output.
    const inst = await FlowInstance.start(p, "ProducedDelivery", task.id);
    expect(inst.currentStateName).to.equal("Ready");
    const outcome = await inst.proposeTransition("Done", undefined, [
      { className: "ProducedDeliverable", id: deliverable.id },
    ]);
    expect(outcome.outcomes, "n:1 fires on the proposer's own vote").to.have.lengthOf(1);
    expect(outcome.derivedState).to.equal("Done");
    const instanceUri = outcome.outcomes[0].instanceUri;

    // Mint the completion.
    const minted = await p.mintFlowReceipt(instanceUri);
    expect(minted.receiptUri).to.match(/^ad4m:\/\/flow\/receipt\//);

    // Enumerate: the committed output, its state, and the receipt that
    // proves it.
    const outputs = await p.flowValidOutputs(FLOW_URI, "Done");
    expect(outputs).to.have.lengthOf(1);
    expect(outputs[0].output).to.deep.equal({
      className: "ProducedDeliverable",
      id: deliverable.id,
    });
    expect(outputs[0].terminalState).to.equal("Done");
    expect(outputs[0].receiptUri).to.equal(minted.receiptUri);

    // Verify: three-way verdict, verified here.
    const verdict = await p.verifyFlowReceipt(minted.receipt);
    expect(verdict.outcome, verdict.detail).to.equal("verified");
    expect(verdict.terminalState).to.equal("Done");
    expect(verdict.outputs).to.deep.equal([
      { className: "ProducedDeliverable", id: deliverable.id },
    ]);

    // A tampered copy — same signed material, outputs swapped to a node the
    // quorum never committed to — is REJECTED (a finding about the material,
    // not an "undecidable" shrug).
    const tampered = JSON.parse(JSON.stringify(minted.receipt));
    tampered.outputs[0].id = task.id;
    const bad = await p.verifyFlowReceipt(tampered);
    expect(bad.outcome, bad.detail).to.equal("rejected");

    // Filter: only the committed output passes; the task (the run's subject,
    // never an output) does not start answering Task queries either.
    const produced = await (Deliverable as any).findAll(p, {
      where: { producedByFlow: { flow: FLOW_URI, state: "Done" } },
    });
    expect(produced.map((d: Deliverable) => d.id)).to.deep.equal([deliverable.id]);
    const tasksProduced = await (Task as any).findAll(p, {
      where: { producedByFlow: { flow: FLOW_URI } },
    });
    expect(tasksProduced, "the subject is not an output").to.be.empty;

    // A malformed filter is an error, never everything.
    let err = "";
    try {
      await (Deliverable as any).findAll(p, {
        where: { producedByFlow: "not-an-object" as any },
      });
    } catch (e: any) {
      err = String(e?.message ?? e);
    }
    expect(err, "malformed filter must error, not admit everything").to.match(/producedByFlow/);
  });
});
