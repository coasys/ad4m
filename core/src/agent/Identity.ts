/**
 * Identity types for the agent identity system (KEL-based, did:scid).
 *
 * These TypeScript types mirror the Rust KEL module's data structures.
 * The identity module talks ONLY to the agent language — never to the
 * executor's perspective DB.
 */

// ─── Scope ─────────────────────────────────────────────────────────────────

/** What a delegated key may do. */
export interface Scope {
  /** Allowed lanes (empty = all). */
  lanes: Lane[];
  /** Allowed operations (empty = all). */
  ops: string[];
}

/** A namespace for key scoping. */
export enum Lane {
  /** Sign links and expressions. */
  Sign = "sign",
  /** Manage keys (delegate, revoke). */
  KeyManagement = "key_management",
  /** All lanes. */
  All = "*",
}

// ─── KEL types ─────────────────────────────────────────────────────────────

/** A single key entry in the key state. */
export interface KeyEntry {
  /** Verification-method id (e.g. `did:scid:ke:1:E…#key-0`). */
  id: string;
  /** Ed25519 signing key, `did:key`-encoded. */
  signingKey: string;
  /** X25519 encryption public key (hex). */
  encryptionKey?: string;
  /** What this key may do. */
  scope: Scope;
}

/** Key validity produced by the resolver. */
export enum Validity {
  /** The key exists and has not been revoked. */
  Valid = "valid",
  /** The key existed but has been revoked. */
  Revoked = "revoked",
  /** The key does not appear in any known KEL. */
  NotFound = "not_found",
}

/** Resolution result from the agent language. */
export interface ResolvedAgent {
  /** The agent's SCID (did:scid:…). */
  did: string;
  /** Validity of the queried key. */
  validity: Validity;
  /** The key state at the head of the KEL. */
  keyState: KeyState;
}

/** The derived state after replaying a KEL. */
export interface KeyState {
  /** Current head sequence number. */
  headSeq: number;
  /** Agent type. */
  agentType: AgentType;
  /** Current controller SCID (if set). */
  controller?: string;
  /** Currently-valid keys. */
  validKeys: KeyEntry[];
  /** Recovery commitment hash (if set). */
  recoveryCommitment?: string;
}

/** Agent type: human, assistant, or service. */
export enum AgentType {
  Human = "human",
  Assistant = "assistant",
  Service = "service",
}

// ─── Roster ────────────────────────────────────────────────────────────────

/** A device/executor/assistant entry rendered from the KEL. */
export interface RosterEntry {
  /** The key_id within the KEL. */
  keyId: string;
  /** Human-readable label. */
  label: string;
  /** Which lane the key operates in. */
  lane: Lane;
  /** The scope granted at delegation. */
  scope: Scope;
  /** The KEL sequence at which this key was delegated. */
  delegatedAtSeq: number;
  /** Whether this key has been revoked. */
  revoked: boolean;
  /** The sequence at which the key was revoked (if applicable). */
  revokedAtSeq?: number;
}

// ─── Enrolment ─────────────────────────────────────────────────────────────

/** An enrolment offer from a new device. */
export interface EnrolOffer {
  /** The new device's Ed25519 public key (did:key). */
  publicKey: string;
  /** The new device's X25519 encryption key (hex). */
  encryptionKey?: string;
  /** Human-readable device name. */
  label: string;
  /** Challenge nonce for freshness. */
  challenge: string;
  /** Requested scope. */
  scope: Scope;
}

/** An enrolment request from a hosted executor. */
export interface HostedEnrolRequest {
  /** The executor's public key (did:key). */
  publicKey: string;
  /** Executor label. */
  label: string;
  /** Challenge nonce. */
  challenge: string;
  /** Scope: always sign-only for hosted executors. */
  scope: Scope;
}

// ─── Assistants ────────────────────────────────────────────────────────────

/** An assistant claim request. */
export interface AssistantClaim {
  /** The assistant's SCID. */
  assistantDid: string;
  /** The assistant's inception key (did:key). */
  inceptionKey: string;
  /** Human-readable name. */
  label: string;
}

// ─── Recovery ──────────────────────────────────────────────────────────────

/** The recovery authority — describes what can recover an identity. */
export interface RecoveryAuthority {
  /** Type: "mnemonic" or "guardian". */
  type: "mnemonic" | "guardian";
  /** The public keys that can sign recovery operations. */
  keys: string[];
  /** Threshold (for guardian recovery). */
  threshold?: number;
}

// ─── Guardians ─────────────────────────────────────────────────────────────

/** A guardian in the roster. */
export interface GuardianEntry {
  /** The guardian's SCID. */
  did: string;
  /** Human-readable label. */
  label: string;
  /** Whether the guardian has given consent. */
  consented: boolean;
}

/** State of a pending recovery request. */
export interface RecoveryRequestState {
  /** Request hash (identifier). */
  requestHash: string;
  /** The SCID requesting recovery. */
  subjectDid: string;
  /** Approvals received so far. */
  approvalCount: number;
  /** Threshold needed. */
  threshold: number;
  /** Timelock expiry (ISO 8601). */
  timelockExpiry: string;
  /** Whether the request can execute (approvals >= threshold AND timelock expired). */
  canExecute: boolean;
  /** Whether the request has been vetoed. */
  vetoed: boolean;
}

// ─── Keyring ───────────────────────────────────────────────────────────────

/** A DEK version status. */
export interface DekVersionInfo {
  /** Version number. */
  version: number;
  /** Number of recipients. */
  recipientCount: number;
  /** KEL sequence at creation. */
  createdAtSeq: number;
}

// ─── Events (for KEL viewer / export) ──────────────────────────────────────

/** A KEL event rendered for display. */
export interface KelEventDisplay {
  /** Sequence number. */
  seq: number;
  /** Event type label. */
  type: string;
  /** Human-readable summary. */
  summary: string;
  /** The signer's key_id. */
  signedBy: string;
  /** Whether the event was wrapped in a ControllerOp or RecoveryOp. */
  wrapper?: "controller" | "recovery";
  /** Raw JSON (for export). */
  raw: string;
}
