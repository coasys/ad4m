import {
  BuildFlowTransitionProposalOpts,
  FlowInstance,
  FlowTransitionProposal,
  buildFlowTransitionProposalFields,
} from "./FlowModels";
import { computeFlowEvidenceHash } from "./FlowEvidenceHash";
import * as FlowVoteAggregatorModule from "./FlowVoteAggregator";

/**
 * Unit tests for the pure companion helper of
 * {@link FlowTransitionProposal.propose}. Deliberately avoids the
 * `Ad4mModel.create` path — that requires a live `PerspectiveProxy`
 * and is exercised by the integration test set in
 * `tests/js/tests/model/flow-models.test.ts`. Here we lock the
 * field-derivation logic (hash, timestamp, optional pruning, defence)
 * that stays stable across perspective backends.
 */

describe("buildFlowTransitionProposalFields", () => {
  const base: BuildFlowTransitionProposalOpts = {
    flowInstance: "ad4m://flow/instance/i-1",
    fromState: "Identified",
    toState: "Scoped",
    proposer: "did:example:alice",
    evidence: ["ad4m://task/1", "ad4m://task/2"],
    classNames: ["ad4m://Task"],
    proposedAt: "2026-08-30T00:00:00Z",
  };

  it("mirrors every input field into the props record verbatim", () => {
    const fields = buildFlowTransitionProposalFields(base);
    expect(fields.flowInstance).toBe(base.flowInstance);
    expect(fields.fromState).toBe(base.fromState);
    expect(fields.toState).toBe(base.toState);
    expect(fields.proposer).toBe(base.proposer);
    expect(fields.evidence).toEqual([...base.evidence]);
    expect(fields.proposedAt).toBe(base.proposedAt);
  });

  it("computes evidenceHashes as scalar SHA-256 hex via computeFlowEvidenceHash", () => {
    const fields = buildFlowTransitionProposalFields(base);
    expect(fields.evidenceHashes).toBe(
      computeFlowEvidenceHash(base.classNames, base.evidence),
    );
    // Defensive: is a 64-char lowercase hex string.
    expect(fields.evidenceHashes).toMatch(/^[0-9a-f]{64}$/);
  });

  it("hash is stable across evidence-URI permutations (sort inside helper)", () => {
    const a = buildFlowTransitionProposalFields({
      ...base,
      evidence: ["ad4m://task/1", "ad4m://task/2"],
    });
    const b = buildFlowTransitionProposalFields({
      ...base,
      evidence: ["ad4m://task/2", "ad4m://task/1"],
    });
    expect(a.evidenceHashes).toBe(b.evidenceHashes);
  });

  it("hash differs when classNames differ (classNames are ordered, not sorted)", () => {
    const a = buildFlowTransitionProposalFields({
      ...base,
      classNames: ["ad4m://Task"],
    });
    const b = buildFlowTransitionProposalFields({
      ...base,
      classNames: ["ad4m://TaskV2"],
    });
    expect(a.evidenceHashes).not.toBe(b.evidenceHashes);
  });

  it("hash differs on className order (semantically load-bearing)", () => {
    const a = buildFlowTransitionProposalFields({
      ...base,
      classNames: ["ad4m://A", "ad4m://B"],
    });
    const b = buildFlowTransitionProposalFields({
      ...base,
      classNames: ["ad4m://B", "ad4m://A"],
    });
    expect(a.evidenceHashes).not.toBe(b.evidenceHashes);
  });

  it("hashes an empty evidence bag deterministically", () => {
    const fields = buildFlowTransitionProposalFields({
      ...base,
      evidence: [],
    });
    expect(fields.evidenceHashes).toBe(
      computeFlowEvidenceHash(base.classNames, []),
    );
    expect(fields.evidence).toEqual([]);
  });

  it("defensive-copies evidence so caller mutation post-build doesn't leak", () => {
    const evidence = ["ad4m://task/1"];
    const fields = buildFlowTransitionProposalFields({
      ...base,
      evidence,
    });
    (evidence as string[]).push("ad4m://task/2");
    expect(fields.evidence).toEqual(["ad4m://task/1"]);
  });

  it("omits `rationale` from the record when the caller does not pass it", () => {
    const fields = buildFlowTransitionProposalFields(base);
    expect("rationale" in fields).toBe(false);
  });

  it("omits `runUri` from the record when the caller does not pass it", () => {
    const fields = buildFlowTransitionProposalFields(base);
    expect("runUri" in fields).toBe(false);
  });

  it("preserves `rationale` verbatim when provided (including empty string)", () => {
    const withRationale = buildFlowTransitionProposalFields({
      ...base,
      rationale: "requires.satisfied via 2 Task instances",
    });
    expect(withRationale.rationale).toBe(
      "requires.satisfied via 2 Task instances",
    );
    // Empty string is a valid caller intent — still included, not pruned.
    const withEmpty = buildFlowTransitionProposalFields({ ...base, rationale: "" });
    expect(withEmpty.rationale).toBe("");
  });

  it("preserves `runUri` verbatim when provided", () => {
    const fields = buildFlowTransitionProposalFields({
      ...base,
      runUri: "ad4m://run/r-42",
    });
    expect(fields.runUri).toBe("ad4m://run/r-42");
  });

  it("defaults `proposedAt` to a real ISO timestamp when not passed", () => {
    const { proposedAt: _drop, ...rest } = base;
    const fields = buildFlowTransitionProposalFields(rest);
    // Must be an ISO string that Date() can round-trip.
    expect(typeof fields.proposedAt).toBe("string");
    const parsed = new Date(fields.proposedAt as string);
    expect(Number.isFinite(parsed.getTime())).toBe(true);
    // Sanity: within one minute of "now".
    expect(Math.abs(parsed.getTime() - Date.now())).toBeLessThan(60_000);
  });

  it("throws when toState is empty", () => {
    expect(() =>
      buildFlowTransitionProposalFields({ ...base, toState: "" }),
    ).toThrow(/toState is required/);
  });

  it("throws when proposer is empty", () => {
    expect(() =>
      buildFlowTransitionProposalFields({ ...base, proposer: "" }),
    ).toThrow(/proposer is required/);
  });

  it("hash is byte-identical to a hand-composed Rust-parity fixture", () => {
    // Locks the (classNames, evidence) → hash path through the propose
    // factory against one of the golden fixtures already verified against
    // Rust in FlowEvidenceHash.test.ts / flow_evaluator.rs.
    const fields = buildFlowTransitionProposalFields({
      ...base,
      classNames: ["ns://X", "ns://Y"],
      evidence: ["b", "a"],
    });
    expect(fields.evidenceHashes).toBe(
      "5245f683b6dcc4efe4ce46e7b0126bd56a37c8794298c2213a335248a9383f66",
    );
  });
});

describe("FlowTransitionProposal.listForInstance boundary defence", () => {
  it("rejects empty flowInstanceUri before any perspective interaction", async () => {
    // Poison-perspective stub: any call surface it exposes throws loudly,
    // proving the defence guard fires BEFORE we would have hit
    // `findAll`. If the guard regressed to `if (false)`, the test would
    // instead surface the poison error and be easy to diagnose.
    const poison: any = new Proxy(
      {},
      {
        get() {
          throw new Error(
            "listForInstance leaked past the empty-URI guard onto the perspective proxy",
          );
        },
      },
    );
    await expect(
      FlowTransitionProposal.listForInstance(poison, ""),
    ).rejects.toThrow(/flowInstanceUri is required/);
  });
});

describe("FlowInstance.proposeTransition (OO wrapper for FlowTransitionProposal.propose)", () => {
  // A poison perspective — any property read throws. Used to prove the
  // OO wrapper's own defence guards fire BEFORE the delegation would
  // have touched the perspective (i.e. the guards don't leak past their
  // conditional).
  const poison: any = new Proxy(
    {},
    {
      get() {
        throw new Error(
          "FlowInstance.proposeTransition leaked past its own defence guards onto the perspective proxy",
        );
      },
    },
  );

  const validOpts = {
    toState: "Scoped",
    proposer: "did:example:alice",
    evidence: ["ad4m://task/1"],
    classNames: ["ad4m://Task"],
  };

  it("throws when this.id is empty (unhydrated instance)", async () => {
    // Ad4mModel's constructor auto-generates an id when none is passed —
    // so simulate the "no id" case by clearing the private backing field
    // after construction. The guard reads through the public getter, so
    // this exercises exactly the runtime path a caller would hit if
    // they called .proposeTransition() on an unsaved instance whose id
    // resolution failed.
    const instance = new FlowInstance(poison, "ad4m://flow/instance/i-1");
    (instance as any)._baseExpression = "";
    instance.currentState = "Identified";
    await expect(instance.proposeTransition(validOpts)).rejects.toThrow(
      /instance has no id/,
    );
  });

  it("throws when this.currentState is empty (would allow silently stale fromState)", async () => {
    const instance = new FlowInstance(poison, "ad4m://flow/instance/i-1");
    instance.currentState = "";
    await expect(instance.proposeTransition(validOpts)).rejects.toThrow(
      /currentState is empty/,
    );
  });

  it("derives flowInstance = this.id and fromState = this.currentState, everything else passes through", async () => {
    // Spy on the static factory — proves the wrapper is *just* delegation.
    // Any field-derivation regression (evidence hashing, timestamp default,
    // optional pruning) is caught by the buildFlowTransitionProposalFields
    // suite above; here we only lock the two derived fields the wrapper
    // owns.
    const sentinel = new FlowTransitionProposal(poison, "ad4m://proposal/sentinel");
    const spy = jest
      .spyOn(FlowTransitionProposal, "propose")
      .mockResolvedValue(sentinel);
    try {
      // We pass poison for the perspective too — the spy's mock never
      // touches it, so the delegation path stays perspective-free.
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-42");
      instance.currentState = "Identified";
      const result = await instance.proposeTransition({
        toState: "Scoped",
        proposer: "did:example:alice",
        evidence: ["ad4m://task/1", "ad4m://task/2"],
        classNames: ["ad4m://Task"],
        rationale: "req satisfied",
        runUri: "ad4m://run/r-7",
        proposedAt: "2026-08-30T10:00:00Z",
      });
      expect(result).toBe(sentinel);
      expect(spy).toHaveBeenCalledTimes(1);
      const [passedPerspective, passedOpts] = spy.mock.calls[0];
      expect(passedPerspective).toBe(poison);
      expect(passedOpts).toEqual({
        flowInstance: "ad4m://flow/instance/i-42",
        fromState: "Identified",
        toState: "Scoped",
        proposer: "did:example:alice",
        evidence: ["ad4m://task/1", "ad4m://task/2"],
        classNames: ["ad4m://Task"],
        rationale: "req satisfied",
        runUri: "ad4m://run/r-7",
        proposedAt: "2026-08-30T10:00:00Z",
      });
    } finally {
      spy.mockRestore();
    }
  });

  it("caller-supplied flowInstance / fromState in opts would be a type error at compile time", () => {
    // The wrapper's Omit<..., "flowInstance" | "fromState"> parameter
    // shape forbids callers from smuggling values that would fight the
    // this-derived ones. This test is a documentation aid — the check
    // is a compile-time TS constraint, not a runtime one; a regression
    // that widens the type would surface as a tests/js type-check
    // failure. Here we just note the invariant.
    expect(true).toBe(true);
  });
});

describe("FlowInstance.currentProposals (OO wrapper for FlowTransitionProposal.listForInstance)", () => {
  // Same poison-perspective pattern as .proposeTransition tests: any
  // property read throws, so the defence guard is proven to fire before
  // the delegation would touch the perspective proxy.
  const poison: any = new Proxy(
    {},
    {
      get() {
        throw new Error(
          "FlowInstance.currentProposals leaked past its own defence guard onto the perspective proxy",
        );
      },
    },
  );

  it("throws when this.id is empty (unhydrated instance)", async () => {
    // Same setup as the proposeTransition id-guard test: clear the
    // Ad4mModel-backing `_baseExpression` field so `this.id` reads empty
    // through the public getter.
    const instance = new FlowInstance(poison, "ad4m://flow/instance/i-1");
    (instance as any)._baseExpression = "";
    await expect(instance.currentProposals()).rejects.toThrow(
      /instance has no id/,
    );
  });

  it("delegates to FlowTransitionProposal.listForInstance with this.perspective and this.id", async () => {
    const sentinelA = new FlowTransitionProposal(poison, "ad4m://proposal/a");
    const sentinelB = new FlowTransitionProposal(poison, "ad4m://proposal/b");
    const spy = jest
      .spyOn(FlowTransitionProposal, "listForInstance")
      .mockResolvedValue([sentinelA, sentinelB]);
    try {
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-42");
      const result = await instance.currentProposals();
      expect(result).toEqual([sentinelA, sentinelB]);
      expect(spy).toHaveBeenCalledTimes(1);
      const [passedPerspective, passedUri] = spy.mock.calls[0];
      expect(passedPerspective).toBe(poison);
      expect(passedUri).toBe("ad4m://flow/instance/i-42");
    } finally {
      spy.mockRestore();
    }
  });

  it("returns the delegate's array as-is (no re-sorting, no defensive copy)", async () => {
    // listForInstance owns the ordering contract (oldest-first). The
    // wrapper must not re-order or clone — else identity-based equality
    // tests downstream would fail and the ordering guarantee would
    // split across two layers.
    const sentinelA = new FlowTransitionProposal(poison, "ad4m://proposal/a");
    const expected: FlowTransitionProposal[] = [sentinelA];
    const spy = jest
      .spyOn(FlowTransitionProposal, "listForInstance")
      .mockResolvedValue(expected);
    try {
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-7");
      const result = await instance.currentProposals();
      // Reference equality: wrapper returns the very array the delegate
      // produced (Promise.resolve unwraps but does not clone).
      expect(result).toBe(expected);
    } finally {
      spy.mockRestore();
    }
  });
});

describe("FlowInstance.aggregateVotes (OO composition of currentProposals + aggregateFlowVotes)", () => {
  // Same poison-perspective pattern as .currentProposals: any property
  // read throws, so the defence guard is proven to fire before the
  // delegation would touch the perspective proxy.
  const poison: any = new Proxy(
    {},
    {
      get() {
        throw new Error(
          "FlowInstance.aggregateVotes leaked past its own defence guard onto the perspective proxy",
        );
      },
    },
  );

  it("throws when this.id is empty (unhydrated instance)", async () => {
    // Clear the Ad4mModel-backing `_baseExpression` field so `this.id`
    // reads empty through the public getter. Mirrors the id-guard tests
    // on .proposeTransition / .currentProposals.
    const instance = new FlowInstance(poison, "ad4m://flow/instance/i-1");
    (instance as any)._baseExpression = "";
    await expect(instance.aggregateVotes()).rejects.toThrow(
      /instance has no id/,
    );
  });

  it("composes currentProposals() + aggregateFlowVotes: passes the loaded bag, rule, and eligibleDIDs to the helper", async () => {
    // Spy on both the delegate that loads proposals and the pure helper
    // that reduces them. Prove the wrapper wires the two together
    // without transforming either input.
    const sentinelA = new FlowTransitionProposal(poison, "ad4m://proposal/a");
    const sentinelB = new FlowTransitionProposal(poison, "ad4m://proposal/b");
    const loadedBag = [sentinelA, sentinelB];
    const listSpy = jest
      .spyOn(FlowTransitionProposal, "listForInstance")
      .mockResolvedValue(loadedBag);

    const sentinelResult: FlowVoteAggregatorModule.AggregateFlowVotesResult = {
      tallies: [
        {
          fromState: "Identified",
          toState: "Scoped",
          distinctProposers: ["did:example:alice"],
          eligibleProposers: ["did:example:alice"],
          requiredCount: 2,
          consensusReached: false,
          contributing: [sentinelA],
        },
      ],
      fires: undefined,
    };
    const aggSpy = jest
      .spyOn(FlowVoteAggregatorModule, "aggregateFlowVotes")
      .mockReturnValue(sentinelResult);

    const rule = { n: 2 };
    const eligibleDIDs = new Set(["did:example:alice", "did:example:bob"]);
    try {
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-42");
      const result = await instance.aggregateVotes(rule, eligibleDIDs);

      expect(listSpy).toHaveBeenCalledTimes(1);
      const [listPerspective, listUri] = listSpy.mock.calls[0];
      expect(listPerspective).toBe(poison);
      expect(listUri).toBe("ad4m://flow/instance/i-42");

      expect(aggSpy).toHaveBeenCalledTimes(1);
      const [aggProposals, aggRule, aggEligible] = aggSpy.mock.calls[0];
      // Reference equality: wrapper does not clone / re-sort the bag
      // before handing it to the helper. Would surface as identity
      // failures downstream if it did.
      expect(aggProposals).toBe(loadedBag);
      expect(aggRule).toBe(rule);
      expect(aggEligible).toBe(eligibleDIDs);

      // Return value is passed through untouched — reference equality
      // proves no defensive re-wrap.
      expect(result).toBe(sentinelResult);
    } finally {
      listSpy.mockRestore();
      aggSpy.mockRestore();
    }
  });

  it("passes undefined for both consensusRule and eligibleDIDs when the caller omits them", async () => {
    // Verifies default-arg pass-through — the helper's own default of
    // `{ n: 1 }` (design §7.1) must fire in the helper, not be silently
    // substituted by the wrapper (else consensusRule surface would
    // fork between OO and static paths).
    const listSpy = jest
      .spyOn(FlowTransitionProposal, "listForInstance")
      .mockResolvedValue([]);
    const aggSpy = jest
      .spyOn(FlowVoteAggregatorModule, "aggregateFlowVotes")
      .mockReturnValue({ tallies: [], fires: undefined });
    try {
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-9");
      await instance.aggregateVotes();
      expect(aggSpy).toHaveBeenCalledTimes(1);
      const [, aggRule, aggEligible] = aggSpy.mock.calls[0];
      expect(aggRule).toBeUndefined();
      expect(aggEligible).toBeUndefined();
    } finally {
      listSpy.mockRestore();
      aggSpy.mockRestore();
    }
  });

  it("propagates aggregateFlowVotes errors verbatim (fromRole without eligibleDIDs)", async () => {
    // Guard: the wrapper must not swallow the silent-default guard from
    // aggregateFlowVotes. Feed it a bag + a rule that requires
    // eligibleDIDs; caller omitted them, so the helper must throw and
    // the wrapper must let the error surface.
    const listSpy = jest
      .spyOn(FlowTransitionProposal, "listForInstance")
      .mockResolvedValue([]);
    try {
      const instance = new FlowInstance(poison, "ad4m://flow/instance/i-9");
      await expect(
        instance.aggregateVotes({ n: 1, fromRole: "some-role-query" as any }),
      ).rejects.toThrow(/fromRole/);
    } finally {
      listSpy.mockRestore();
    }
  });
});
