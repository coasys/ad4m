import {
  BuildFlowTransitionProposalOpts,
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
