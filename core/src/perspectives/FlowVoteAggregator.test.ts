import { FlowTransitionProposal } from "./FlowModels";
import {
  AggregateFlowVotesResult,
  FlowVoteTally,
  aggregateFlowVotes,
} from "./FlowVoteAggregator";
import type { ConsensusRule } from "../shacl/SHACLFlow";

/**
 * Fixture constructor. `FlowTransitionProposal` is an `@Model` class with
 * required Ad4mModel plumbing, but for the pure aggregator the only fields
 * that matter are `fromState` / `toState` / `proposer` / `proposedAt`. We
 * build lightweight object literals cast to the class type — this is safe
 * because the aggregator NEVER calls a method on the value (typed as
 * `readonly` in the signature specifically to advertise that contract).
 */
function proposal(
  fromState: string,
  toState: string,
  proposer: string,
  proposedAt = "2026-08-30T00:00:00Z",
): FlowTransitionProposal {
  return {
    fromState,
    toState,
    proposer,
    proposedAt,
  } as unknown as FlowTransitionProposal;
}

describe("aggregateFlowVotes — defence", () => {
  it("throws when consensusRule.n is zero", () => {
    expect(() => aggregateFlowVotes([], { n: 0 })).toThrow(
      /n must be a positive integer/,
    );
  });

  it("throws when consensusRule.n is negative", () => {
    expect(() => aggregateFlowVotes([], { n: -1 })).toThrow(
      /n must be a positive integer/,
    );
  });

  it("throws when consensusRule.n is non-integer", () => {
    expect(() => aggregateFlowVotes([], { n: 1.5 })).toThrow(
      /n must be a positive integer/,
    );
  });

  it("throws when fromRole is set but eligibleDIDs missing (silent-default would misreport)", () => {
    const rule: ConsensusRule = {
      n: 1,
      fromRole: { className: "Reviewer", didProperty: "agent" },
    };
    expect(() => aggregateFlowVotes([], rule)).toThrow(
      /fromRole is set/,
    );
  });

  it("accepts fromRole + eligibleDIDs empty set (no eligible → no fire)", () => {
    const rule: ConsensusRule = {
      n: 1,
      fromRole: { className: "Reviewer", didProperty: "agent" },
    };
    const res = aggregateFlowVotes(
      [proposal("A", "B", "did:example:alice")],
      rule,
      new Set(),
    );
    expect(res.fires).toBeUndefined();
    expect(res.tallies[0].eligibleProposers).toEqual([]);
    expect(res.tallies[0].consensusReached).toBe(false);
  });
});

describe("aggregateFlowVotes — grouping + counting", () => {
  it("empty proposal list returns empty tallies + no fire", () => {
    const res = aggregateFlowVotes([]);
    expect(res.tallies).toEqual([]);
    expect(res.fires).toBeUndefined();
  });

  it("defaults to { n: 1 } when consensusRule omitted (design §7.1)", () => {
    const res = aggregateFlowVotes([
      proposal("A", "B", "did:example:alice"),
    ]);
    expect(res.tallies[0].requiredCount).toBe(1);
    expect(res.tallies[0].consensusReached).toBe(true);
    expect(res.fires).toBeDefined();
  });

  it("same DID proposing twice for one target counts as one distinct proposer", () => {
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
        proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z"),
      ],
      { n: 2 },
    );
    expect(res.tallies[0].distinctProposers).toEqual(["did:example:alice"]);
    expect(res.tallies[0].consensusReached).toBe(false);
  });

  it("n=2 threshold: two distinct DIDs meet, one does not", () => {
    const met = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "B", "did:example:bob"),
      ],
      { n: 2 },
    );
    expect(met.tallies[0].consensusReached).toBe(true);
    expect(met.tallies[0].distinctProposers).toEqual([
      "did:example:alice",
      "did:example:bob",
    ]);

    const unmet = aggregateFlowVotes(
      [proposal("A", "B", "did:example:alice")],
      { n: 2 },
    );
    expect(unmet.tallies[0].consensusReached).toBe(false);
    expect(unmet.fires).toBeUndefined();
  });

  it("separates buckets by (fromState, toState) — proposals for different targets do not cross-count", () => {
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "C", "did:example:alice"),
      ],
      { n: 2 },
    );
    expect(res.tallies).toHaveLength(2);
    for (const t of res.tallies) {
      expect(t.distinctProposers).toEqual(["did:example:alice"]);
      expect(t.consensusReached).toBe(false);
    }
  });

  it("distinctProposers is sorted lexicographically (deterministic output)", () => {
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:zoe"),
        proposal("A", "B", "did:example:alice"),
        proposal("A", "B", "did:example:mike"),
      ],
      { n: 1 },
    );
    expect(res.tallies[0].distinctProposers).toEqual([
      "did:example:alice",
      "did:example:mike",
      "did:example:zoe",
    ]);
  });

  it("contributing preserves input ordering (oldest-first when caller sorted)", () => {
    const p1 = proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z");
    const p2 = proposal("A", "B", "did:example:bob", "2026-08-30T00:00:02Z");
    const p3 = proposal("A", "B", "did:example:cara", "2026-08-30T00:00:03Z");
    const res = aggregateFlowVotes([p1, p2, p3], { n: 3 });
    expect(res.tallies[0].contributing).toEqual([p1, p2, p3]);
  });

  it("tallies sorted lexicographically by (fromState, toState) for stable diffing", () => {
    const res = aggregateFlowVotes(
      [
        proposal("C", "D", "did:example:alice"),
        proposal("A", "B", "did:example:alice"),
        proposal("A", "Z", "did:example:alice"),
        proposal("B", "C", "did:example:alice"),
      ],
      { n: 1 },
    );
    expect(
      res.tallies.map((t) => `${t.fromState}->${t.toState}`),
    ).toEqual(["A->B", "A->Z", "B->C", "C->D"]);
  });
});

describe("aggregateFlowVotes — fromRole gating", () => {
  const rule: ConsensusRule = {
    n: 1,
    fromRole: { className: "Reviewer", didProperty: "agent" },
  };

  it("only eligibleDIDs contribute to consensus", () => {
    const eligible = new Set(["did:example:bob"]);
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "B", "did:example:bob"),
      ],
      rule,
      eligible,
    );
    expect(res.tallies[0].distinctProposers).toEqual([
      "did:example:alice",
      "did:example:bob",
    ]);
    expect(res.tallies[0].eligibleProposers).toEqual(["did:example:bob"]);
    expect(res.tallies[0].consensusReached).toBe(true);
  });

  it("consensus fails when only non-role DID proposed", () => {
    const eligible = new Set(["did:example:bob"]);
    const res = aggregateFlowVotes(
      [proposal("A", "B", "did:example:alice")],
      rule,
      eligible,
    );
    expect(res.tallies[0].eligibleProposers).toEqual([]);
    expect(res.tallies[0].consensusReached).toBe(false);
    expect(res.fires).toBeUndefined();
  });

  it("n=2 with role: requires two DISTINCT eligible DIDs (one eligible, one not = fail)", () => {
    const eligible = new Set(["did:example:bob"]);
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "B", "did:example:bob"),
      ],
      { n: 2, fromRole: rule.fromRole },
      eligible,
    );
    expect(res.tallies[0].consensusReached).toBe(false);
  });

  it("eligibleProposers preserves sorted order (subset of distinctProposers)", () => {
    const eligible = new Set([
      "did:example:alice",
      "did:example:cara",
    ]);
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:cara"),
        proposal("A", "B", "did:example:bob"),
        proposal("A", "B", "did:example:alice"),
      ],
      { n: 2, fromRole: rule.fromRole },
      eligible,
    );
    expect(res.tallies[0].eligibleProposers).toEqual([
      "did:example:alice",
      "did:example:cara",
    ]);
    expect(res.tallies[0].consensusReached).toBe(true);
  });
});

describe("aggregateFlowVotes — fires selection", () => {
  it("selects the target whose earliest contributing proposal has the smallest proposedAt", () => {
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice", "2026-08-30T00:00:05Z"),
        proposal("A", "C", "did:example:alice", "2026-08-30T00:00:01Z"),
      ],
      { n: 1 },
    );
    expect(res.fires).toBeDefined();
    expect(res.fires!.toState).toBe("C");
  });

  it("breaks ties (same earliest proposedAt) by lex (fromState, toState)", () => {
    const same = "2026-08-30T00:00:00Z";
    const res = aggregateFlowVotes(
      [
        proposal("A", "Z", "did:example:alice", same),
        proposal("A", "B", "did:example:alice", same),
      ],
      { n: 1 },
    );
    expect(res.fires!.toState).toBe("B");
  });

  it("returns undefined when no bucket clears the bar", () => {
    const res = aggregateFlowVotes(
      [proposal("A", "B", "did:example:alice")],
      { n: 5 },
    );
    expect(res.fires).toBeUndefined();
    // tally still surfaces the count so UI can render "1 of 5"
    expect(res.tallies[0].distinctProposers).toHaveLength(1);
    expect(res.tallies[0].requiredCount).toBe(5);
  });

  it("prefers a fired target over an unfired one even if unfired has an older proposal", () => {
    // A->B has one proposal at t=0 → doesn't fire under n=2.
    // A->C has two proposals at t=1 and t=2 → fires under n=2.
    // Only A->C is a candidate for `fires`, even though A->B's earliest is older.
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice", "2026-08-30T00:00:00Z"),
        proposal("A", "C", "did:example:alice", "2026-08-30T00:00:01Z"),
        proposal("A", "C", "did:example:bob", "2026-08-30T00:00:02Z"),
      ],
      { n: 2 },
    );
    expect(res.fires!.toState).toBe("C");
  });
});

describe("aggregateFlowVotes — output invariants", () => {
  it("does not mutate input proposal array or objects", () => {
    const input = [
      proposal("A", "B", "did:example:alice", "2026-08-30T00:00:01Z"),
      proposal("A", "B", "did:example:bob", "2026-08-30T00:00:02Z"),
    ];
    const snapshotOrder = input.slice();
    const snapshotFirst = { ...input[0] };
    aggregateFlowVotes(input, { n: 2 });
    expect(input).toEqual(snapshotOrder);
    expect(input[0]).toEqual(snapshotFirst);
  });

  it("requiredCount is copied onto every tally (UI does not need to re-lookup the rule)", () => {
    const res: AggregateFlowVotesResult = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "C", "did:example:alice"),
      ],
      { n: 3 },
    );
    for (const t of res.tallies) {
      expect(t.requiredCount).toBe(3);
    }
  });

  it("without fromRole, eligibleProposers === distinctProposers (reference equality irrelevant, values equal)", () => {
    const res = aggregateFlowVotes(
      [
        proposal("A", "B", "did:example:alice"),
        proposal("A", "B", "did:example:bob"),
      ],
      { n: 2 },
    );
    const t: FlowVoteTally = res.tallies[0];
    expect(t.eligibleProposers).toEqual(t.distinctProposers);
  });
});
