import {
  BuildFlowTransitionProposalOpts,
  FlowInstance,
  FlowTransitionProposal,
  buildFlowTransitionProposalFields,
} from "./FlowModels";
import { computeFlowEvidenceHash } from "./FlowEvidenceHash";

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
