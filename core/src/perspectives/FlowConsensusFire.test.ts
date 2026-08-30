import type {
  AggregateFlowVotesResult,
  FlowVoteTally,
} from "./FlowVoteAggregator";
import { FlowInstance, FlowTransitionProposal } from "./FlowModels";
import {
  fireFlowConsensus,
  fireIfConsensus,
  selectFireCandidate,
} from "./FlowConsensusFire";

/**
 * Fixture helpers. Mirrors the aggregator test pattern (10.8e): we build
 * plain object literals cast to the model types. Safe here because the
 * defence layer of `fireFlowConsensus` (all three throw paths) checks
 * only scalar fields on the tally and `instance.currentState` before
 * touching any perspective — same "advertise readonly-ish contract"
 * shape used by aggregateFlowVotes.
 */

function tally(overrides: Partial<FlowVoteTally> = {}): FlowVoteTally {
  return {
    fromState: "Scoped",
    toState: "InProgress",
    distinctProposers: ["did:eth:0xA", "did:eth:0xB"],
    eligibleProposers: ["did:eth:0xA", "did:eth:0xB"],
    requiredCount: 2,
    consensusReached: true,
    contributing: [],
    ...overrides,
  };
}

function proposal(
  uri: string,
  proposedAt = "2026-08-30T00:00:00Z",
): FlowTransitionProposal {
  // `id` is an Ad4mModel getter over `_baseExpression`; test doubles bypass
  // the class prototype, so we set the property directly. Aggregator +
  // firing code both use `.id` to read a proposal's URI.
  return { id: uri, baseExpression: uri, proposedAt } as unknown as FlowTransitionProposal;
}

function instanceSnapshot(currentState: string): Pick<FlowInstance, "currentState"> {
  return { currentState };
}

// ── selectFireCandidate (pure) ─────────────────────────────────────────────

describe("selectFireCandidate", () => {
  it("returns undefined when aggregate.fires is absent (no consensus)", () => {
    const agg: AggregateFlowVotesResult = { tallies: [] };
    expect(selectFireCandidate(instanceSnapshot("Scoped"), agg)).toBeUndefined();
  });

  it("returns undefined when fires.fromState does not match instance.currentState (stale vote)", () => {
    const t = tally({ fromState: "Scoped", toState: "InProgress" });
    const agg: AggregateFlowVotesResult = { tallies: [t], fires: t };
    expect(selectFireCandidate(instanceSnapshot("InProgress"), agg)).toBeUndefined();
  });

  it("returns the tally when consensus reached AND fromState matches currentState", () => {
    const t = tally({ fromState: "Scoped", toState: "InProgress" });
    const agg: AggregateFlowVotesResult = { tallies: [t], fires: t };
    const picked = selectFireCandidate(instanceSnapshot("Scoped"), agg);
    expect(picked).toBe(t);
  });

  it("passes through the exact tally reference (no copy)", () => {
    const t = tally();
    const agg: AggregateFlowVotesResult = { tallies: [t], fires: t };
    expect(selectFireCandidate(instanceSnapshot("Scoped"), agg)).toBe(t);
  });
});

// ── fireFlowConsensus (defence layer — throws before touching perspective) ─

describe("fireFlowConsensus — defence", () => {
  const perspective = {} as any;

  function buildInstance(currentState: string, uri = "ad4m://obj/flow-instance-1") {
    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = uri;
    inst.flow = "Delivery";
    inst.subject = "ad4m://obj/base-1";
    inst.currentState = currentState;
    inst.startedAt = "2026-08-30T00:00:00Z";
    return inst;
  }

  it("throws when tally has not reached consensus", async () => {
    const inst = buildInstance("Scoped");
    const t = tally({ consensusReached: false, eligibleProposers: ["did:eth:0xA"], requiredCount: 2 });
    await expect(fireFlowConsensus(perspective, inst, t)).rejects.toThrow(
      /has not reached consensus/,
    );
  });

  it("throws when fromState does not match currentState (stale)", async () => {
    const inst = buildInstance("InProgress");
    const t = tally({ fromState: "Scoped", toState: "InProgress" });
    await expect(fireFlowConsensus(perspective, inst, t)).rejects.toThrow(
      /stale tally/,
    );
  });

  it("throws on a no-op (toState equals currentState)", async () => {
    const inst = buildInstance("InProgress");
    const t = tally({ fromState: "InProgress", toState: "InProgress" });
    await expect(fireFlowConsensus(perspective, inst, t)).rejects.toThrow(
      /no-op/,
    );
  });

  it("does not mutate the instance on defence failure", async () => {
    const inst = buildInstance("InProgress");
    const t = tally({ fromState: "Scoped", toState: "Done" });
    await expect(fireFlowConsensus(perspective, inst, t)).rejects.toThrow();
    expect(inst.currentState).toBe("InProgress");
  });
});

describe("fireFlowConsensus — happy path", () => {
  it("advances currentState, calls save(), and reports proposer + proposal URIs", async () => {
    const t = tally({
      fromState: "Scoped",
      toState: "InProgress",
      eligibleProposers: ["did:eth:0xB", "did:eth:0xA"], // aggregator sorts; already sorted here
      contributing: [
        proposal("ad4m://obj/proposal-1"),
        proposal("ad4m://obj/proposal-2"),
      ],
    });

    const saveCalls: number[] = [];
    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = "ad4m://obj/flow-instance-1";
    inst.flow = "Delivery";
    inst.subject = "ad4m://obj/base-1";
    inst.currentState = "Scoped";
    inst.startedAt = "2026-08-30T00:00:00Z";
    (inst as any).save = async function () {
      saveCalls.push(Date.now());
      // save() mutates in place through Ad4mModel.innerUpdate — for the
      // test double we just record that it fired at the right time.
    };

    const outcome = await fireFlowConsensus({} as any, inst, t);

    expect(saveCalls.length).toBe(1);
    expect(inst.currentState).toBe("InProgress");
    expect(outcome).toEqual({
      instanceUri: "ad4m://obj/flow-instance-1",
      fromState: "Scoped",
      toState: "InProgress",
      firedByProposers: ["did:eth:0xB", "did:eth:0xA"],
      contributingProposalUris: [
        "ad4m://obj/proposal-1",
        "ad4m://obj/proposal-2",
      ],
    });
  });

  it("firedByProposers is a defensive copy (mutating it does not touch the tally)", async () => {
    const t = tally({
      fromState: "Scoped",
      toState: "InProgress",
      eligibleProposers: ["did:eth:0xA"],
      requiredCount: 1,
      consensusReached: true,
    });

    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = "ad4m://obj/x";
    inst.currentState = "Scoped";
    (inst as any).save = async () => {};

    const outcome = await fireFlowConsensus({} as any, inst, t);
    outcome.firedByProposers.push("did:eth:0xEVIL");
    expect(t.eligibleProposers).toEqual(["did:eth:0xA"]);
  });
});

// ── fireIfConsensus (composition) ──────────────────────────────────────────

describe("fireIfConsensus", () => {
  it("returns undefined without touching save() when there is no fires tally", async () => {
    let saveFired = false;
    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = "ad4m://obj/x";
    inst.currentState = "Scoped";
    (inst as any).save = async () => {
      saveFired = true;
    };
    const agg: AggregateFlowVotesResult = { tallies: [] };
    expect(await fireIfConsensus({} as any, inst, agg)).toBeUndefined();
    expect(saveFired).toBe(false);
    expect(inst.currentState).toBe("Scoped");
  });

  it("returns undefined without touching save() when fires.fromState is stale", async () => {
    let saveFired = false;
    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = "ad4m://obj/x";
    inst.currentState = "InProgress";
    (inst as any).save = async () => {
      saveFired = true;
    };
    const t = tally({ fromState: "Scoped", toState: "InProgress" });
    const agg: AggregateFlowVotesResult = { tallies: [t], fires: t };
    expect(await fireIfConsensus({} as any, inst, agg)).toBeUndefined();
    expect(saveFired).toBe(false);
  });

  it("delegates to fireFlowConsensus when fires tally matches currentState", async () => {
    let saveFired = false;
    const inst = Object.create(FlowInstance.prototype) as FlowInstance;
    (inst as any)._baseExpression = "ad4m://obj/flow-instance-42";
    inst.currentState = "Scoped";
    (inst as any).save = async () => {
      saveFired = true;
      inst.currentState = "InProgress";
    };
    const t = tally({
      fromState: "Scoped",
      toState: "InProgress",
      eligibleProposers: ["did:eth:0xA"],
      requiredCount: 1,
      consensusReached: true,
      contributing: [proposal("ad4m://obj/p-1")],
    });
    const agg: AggregateFlowVotesResult = { tallies: [t], fires: t };
    const outcome = await fireIfConsensus({} as any, inst, agg);
    expect(saveFired).toBe(true);
    expect(outcome?.instanceUri).toBe("ad4m://obj/flow-instance-42");
    expect(outcome?.toState).toBe("InProgress");
    expect(outcome?.contributingProposalUris).toEqual(["ad4m://obj/p-1"]);
  });
});
