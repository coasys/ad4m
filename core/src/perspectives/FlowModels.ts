/**
 * TypeScript @Model classes mirroring the Rust-side hardwired SDNA for the
 * flow runtime. Apps can import these to query the perspective graph for
 * pending flow-transition proposals without knowing the raw link predicates.
 *
 * Registration:
 *   await FlowTransitionProposal.register(perspective);
 *
 * Querying:
 *   const pending = await FlowTransitionProposal.findAll(perspective);
 *
 * Slice 5/7 of the flow-implementation arc (design doc §4.2). The Rust
 * engine does not yet mint these — that lands with the consensus firing
 * work in a later PR arc. This class defines the on-graph shape so:
 *   - clients can build UI over proposals immediately (Deliberation demo);
 *   - the SDNA-parity test locks TS/Rust drift the moment the JSON exists
 *     (`rust-executor/src/perspectives/hardwired_sdna/flow_transition_proposal.json`);
 *   - the engine can start writing into this shape when consensus lands
 *     without touching the client-side surface.
 */

import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Optional, Property } from "../model/decorators";
import { Model } from "../model/decorators";

// ── FlowTransitionProposal ──────────────────────────────────────────────────
// Mirrors the Rust hardwired `flow_transition_proposal.json` SDNA (once wired
// on the engine side; see file header). One node per proposed transition; the
// URI is `ad4m://flow/proposal/<uuid>` when the engine mints it, but the class
// carries no URI-format constraint — apps constructing proposals directly may
// choose any URI they can defend as unique.

@Model({ name: "FlowTransitionProposal" })
export class FlowTransitionProposal extends Ad4mModel {
  /**
   * URI of the running `FlowInstance` this proposal targets. First-declared
   * property so `buildSHACL` derives the shape namespace from its `through`
   * prefix — `ad4m://flow/instance` → `ad4m://` → target_class
   * `ad4m://FlowTransitionProposal`. Also the identity predicate: the
   * discriminator `findAll` uses to isolate proposal nodes.
   */
  @Property({ through: "ad4m://flow/instance", required: true, identity: true })
  flowInstance: string = "";

  /** Name of the state the flow instance is currently in. */
  @Property({ through: "ad4m://flow/from_state", required: true })
  fromState: string = "";

  /** Name of the state the proposal wants to transition to. */
  @Property({ through: "ad4m://flow/to_state", required: true })
  toState: string = "";

  /** DID of the agent that issued the proposal. */
  @Property({ through: "ad4m://flow/proposer", required: true })
  proposer: string = "";

  /**
   * URIs of instances offered as evidence that `toState.requires` is now
   * satisfied. Multiple links, one per cited instance; ordering is not
   * significant.
   */
  @HasMany({ through: "ad4m://flow/evidence", datatype: "xsd:string" })
  evidence: string[] = [];

  /**
   * JSON blob: `{ [uri]: sha256_hex }` — one entry per evidence URI, hashing
   * the canonicalized graph-visible properties (sorted by property URI,
   * multi-values sorted lexicographically) at proposal time. Downstream
   * verifiers use it to detect edits to cited instances after the proposal
   * was written. Stored as an opaque string because SHACL properties carry
   * no rich-object type; the engine parses it back into a map.
   */
  @Property({ through: "ad4m://flow/evidence_hashes", required: true })
  evidenceHashes: string = "";

  /**
   * URI of the `InterpretationRun` node that produced this proposal, when
   * the proposal came from an LLM extraction pass. Absent when a human
   * clicked "propose" in the UI or an external tool wrote the proposal
   * directly.
   */
  @Optional({ through: "ad4m://flow/run_uri" })
  runUri?: string;

  /**
   * Free-form human-readable justification. Optional — the LLM includes it
   * when it wants to explain *why* the cited evidence satisfies the state's
   * `requires`; the UI shows it to reviewers.
   */
  @Optional({ through: "ad4m://flow/rationale" })
  rationale?: string;

  /** RFC3339 timestamp when the proposal was written. */
  @Property({ through: "ad4m://flow/created_at", required: true })
  createdAt: string = "";
}
