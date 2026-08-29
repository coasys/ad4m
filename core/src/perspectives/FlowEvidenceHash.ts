/**
 * Deterministic content-hash of the evidence bag cited by a
 * `FlowTransitionProposal`.
 *
 * # Why this exists
 *
 * `FlowTransitionProposal.evidenceHashes` is a single scalar sha256-hex.
 * The Rust engine writes it (via `rust-executor/src/perspectives/flow_evaluator.rs`
 * `evidence_hash`) when it mints a proposal from a satisfied `requires`
 * clause. When a client mints a proposal by hand — via
 * `FlowInstance.proposeTransition(toState, evidence, rationale?)` (design
 * §4.3) — TS needs to compute the same hash so:
 *
 *   1. Consumers replaying the proposal in a UI can verify the evidence
 *      list still matches by rehashing.
 *   2. Downstream consensus verification (design §7) can validate
 *      client-minted and engine-minted proposals with a single algorithm.
 *
 * # Algorithm — locked byte-for-byte with Rust
 *
 * ```text
 * sorted_ids = evidence_ids sorted lexicographically
 * digest = SHA256(
 *   utf8(class_names.join("|"))  ||
 *   0x00                          ||
 *   utf8(sorted_ids.join("\n"))
 * )
 * return hex(digest)
 * ```
 *
 * - `|` (0x7c) separates class names because it never appears in a URI
 *   scheme+authority, so no injection surface.
 * - `\0` (0x00) separates the two sections so an empty `evidence_ids`
 *   still produces a stable, class-list-dependent hash (empty joins
 *   to `""`, but the null byte anchors the boundary).
 * - `\n` (0x0a) separates IDs because it can't appear inside a URI (RFC
 *   3986 §2 forbids control characters in the URI production).
 * - Sort is applied to `evidence_ids` so re-evaluating the same `requires`
 *   against the same graph state produces the same hash regardless of
 *   the perspective's returned order. `class_names` is NOT sorted — the
 *   engine derives it from `requires[i].class`, whose order is
 *   author-controlled and part of the flow definition.
 *
 * # Parity locks
 *
 * The Rust source of truth is `rust-executor/src/perspectives/flow_evaluator.rs`
 * `evidence_hash`. The test fixtures in this module's `.test.ts` neighbour
 * are byte-computed hashes of hand-constructed byte sequences (see
 * `printf ... | sha256sum` derivations in the test file's comments);
 * they also match the Rust unit tests' invariants (stable across id
 * permutations, differs on class change, differs on id change).
 * A change to this algorithm requires bumping both sides in the same
 * commit + the on-graph schema version.
 */

import { createHash } from "crypto";

/**
 * Compute the scalar evidence-hash for a proposal.
 *
 * @param classNames Ordered class-URI list from the flow's `requires`
 *                   clauses. Passed as-is (NOT sorted) — the class ordering
 *                   is part of the flow definition and semantically load-bearing.
 * @param evidenceIds URIs of the instances cited as evidence. Sorted
 *                    inside this function; caller need not pre-sort.
 * @returns 64-character lowercase hex string (SHA-256 of the
 *          canonicalized bag).
 */
export function computeFlowEvidenceHash(
  classNames: readonly string[],
  evidenceIds: readonly string[],
): string {
  const sortedIds = [...evidenceIds].sort();
  const hasher = createHash("sha256");
  hasher.update(Buffer.from(classNames.join("|"), "utf8"));
  hasher.update(Buffer.from([0x00]));
  hasher.update(Buffer.from(sortedIds.join("\n"), "utf8"));
  return hasher.digest("hex");
}
