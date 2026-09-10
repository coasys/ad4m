/**
 * Identity client — the API surface for identity operations.
 *
 * The WE identity module calls ONLY these methods. Every method routes
 * through the agent language; none touches the executor's perspective DB.
 *
 * This client wraps the executor's RPC interface into typed identity
 * operations, following the same `apiClient.call()` pattern as
 * AgentClient.
 */

import type { ApiClient } from "../apiClient";
import type {
  AgentType,
  AssistantClaim,
  DekVersionInfo,
  EnrolOffer,
  GuardianEntry,
  HostedEnrolRequest,
  KelEventDisplay,
  RecoveryRequestState,
  ResolvedAgent,
  RosterEntry,
  Scope,
} from "./Identity";

/** Callbacks for identity state changes. */
export interface IdentitySubscription {
  onRosterChange?: (roster: RosterEntry[]) => void;
  onRecoveryUpdate?: (state: RecoveryRequestState | null) => void;
  onGuardianUpdate?: (guardians: GuardianEntry[]) => void;
}

/**
 * Identity client.
 *
 * Talks exclusively to the agent language. Never touches the executor's
 * perspective DB (hard boundary — non-negotiable).
 */
export class IdentityClient {
  #apiClient: ApiClient;

  constructor(apiClient: ApiClient) {
    this.#apiClient = apiClient;
  }

  // ─── C1: Create identity ───────────────────────────────────────────

  /**
   * Create a new identity (KEL inception).
   *
   * Generates the inception keypair on-device, mints KEL event #0,
   * publishes the head to the agent language, and returns the new SCID.
   *
   * @param displayName - Human-readable name for the identity
   * @param password - Local encryption passphrase (never leaves the device)
   * @param agentType - Agent type (default: human)
   * @returns The new did:scid
   */
  async createIdentity(
    displayName: string,
    password: string,
    agentType?: AgentType
  ): Promise<string> {
    const result = await this.#apiClient.call<{ did: string }>(
      "identity.create",
      { displayName, password, agentType: agentType ?? "human" }
    );
    return result.did;
  }

  /**
   * Generate a mnemonic backup phrase for the current identity.
   *
   * @returns 12-word BIP-39 mnemonic
   */
  async generateMnemonic(): Promise<string> {
    return this.#apiClient.call<string>("identity.generateMnemonic", {});
  }

  /**
   * Confirm the mnemonic backup (the user proved they wrote it down).
   *
   * @param words - Two words from the mnemonic, at the positions requested
   */
  async confirmMnemonicBackup(
    words: { index: number; word: string }[]
  ): Promise<boolean> {
    return this.#apiClient.call<boolean>(
      "identity.confirmMnemonicBackup",
      { words }
    );
  }

  // ─── Resolution ────────────────────────────────────────────────────

  /**
   * Resolve an agent by DID or key_id.
   */
  async resolve(didOrKeyId: string): Promise<ResolvedAgent> {
    return this.#apiClient.call<ResolvedAgent>(
      "identity.resolve",
      { id: didOrKeyId }
    );
  }

  // ─── C2: Enrolment ─────────────────────────────────────────────────

  /**
   * Get the current roster (all devices/executors/assistants).
   */
  async roster(): Promise<RosterEntry[]> {
    return this.#apiClient.call<RosterEntry[]>("identity.roster", {});
  }

  /**
   * Create an enrolment offer (from the new device).
   * Returns the offer to display as QR or share manually.
   */
  async createEnrolOffer(label: string, scope: Scope): Promise<EnrolOffer> {
    return this.#apiClient.call<EnrolOffer>(
      "identity.createEnrolOffer",
      { label, scope }
    );
  }

  /**
   * Approve an enrolment offer (from a trusted device).
   *
   * @param offer - The offer to approve
   * @returns The new key_id
   */
  async approveEnrolment(offer: EnrolOffer): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.approveEnrolment",
      { offer }
    );
  }

  /**
   * Approve a hosted executor's enrolment request.
   */
  async approveHostedEnrolment(request: HostedEnrolRequest): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.approveHostedEnrolment",
      { request }
    );
  }

  /**
   * Enrol via mnemonic (no second device available).
   *
   * @param mnemonic - The 12-word recovery phrase
   * @param label - Label for the new device
   * @returns The new key_id
   */
  async enrolViaMnemonic(mnemonic: string, label: string): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.enrolViaMnemonic",
      { mnemonic, label }
    );
  }

  // ─── C3: Revocation ────────────────────────────────────────────────

  /**
   * Revoke a key.
   *
   * @param keyId - The key_id to revoke
   * @returns The KEL sequence of the revocation event
   */
  async revokeKey(keyId: string): Promise<number> {
    return this.#apiClient.call<number>(
      "identity.revokeKey",
      { keyId }
    );
  }

  /**
   * Rotate the current device's key.
   *
   * @returns The new key_id
   */
  async rotateKey(): Promise<string> {
    return this.#apiClient.call<string>("identity.rotateKey", {});
  }

  /**
   * Recover from mnemonic — revoke all existing keys and enrol a fresh one.
   *
   * @param mnemonic - The 12-word recovery phrase
   * @param label - Label for the new device
   * @returns The new key_id
   */
  async recoverFromMnemonic(mnemonic: string, label: string): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.recoverFromMnemonic",
      { mnemonic, label }
    );
  }

  // ─── Assistants ────────────────────────────────────────────────────

  /**
   * Claim an assistant (delegate ownership).
   */
  async claimAssistant(claim: AssistantClaim): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.claimAssistant",
      { claim }
    );
  }

  // ─── Guardians ─────────────────────────────────────────────────────

  /**
   * Set the guardian roster.
   */
  async setGuardians(
    guardians: { did: string; label: string }[],
    threshold: number
  ): Promise<void> {
    await this.#apiClient.call<void>(
      "identity.setGuardians",
      { guardians, threshold }
    );
  }

  /**
   * Get the current guardian roster.
   */
  async guardians(): Promise<GuardianEntry[]> {
    return this.#apiClient.call<GuardianEntry[]>(
      "identity.guardians",
      {}
    );
  }

  /**
   * Open a recovery request (as a person who lost their devices).
   *
   * @param newPublicKey - The new device's public key
   * @param label - Label for the new device
   * @returns The recovery request state
   */
  async openRecovery(
    newPublicKey: string,
    label: string
  ): Promise<RecoveryRequestState> {
    return this.#apiClient.call<RecoveryRequestState>(
      "identity.openRecovery",
      { newPublicKey, label }
    );
  }

  /**
   * Approve a recovery request (as a guardian).
   */
  async approveRecovery(requestHash: string): Promise<void> {
    await this.#apiClient.call<void>(
      "identity.approveRecovery",
      { requestHash }
    );
  }

  /**
   * Veto a recovery request (as the identity owner, if a key still works).
   */
  async vetoRecovery(requestHash: string): Promise<void> {
    await this.#apiClient.call<void>(
      "identity.vetoRecovery",
      { requestHash }
    );
  }

  /**
   * Execute a recovery request (after quorum + timelock).
   */
  async executeRecovery(requestHash: string): Promise<string> {
    return this.#apiClient.call<string>(
      "identity.executeRecovery",
      { requestHash }
    );
  }

  /**
   * Get the current recovery request state (if any).
   */
  async recoveryState(): Promise<RecoveryRequestState | null> {
    return this.#apiClient.call<RecoveryRequestState | null>(
      "identity.recoveryState",
      {}
    );
  }

  // ─── KEL viewer / export ───────────────────────────────────────────

  /**
   * Get the full KEL event log for display.
   */
  async kelEvents(): Promise<KelEventDisplay[]> {
    return this.#apiClient.call<KelEventDisplay[]>(
      "identity.kelEvents",
      {}
    );
  }

  /**
   * Export the KEL as a single replayable JSON file.
   */
  async exportKel(): Promise<string> {
    return this.#apiClient.call<string>("identity.exportKel", {});
  }

  // ─── Encryption (keyring) ──────────────────────────────────────────

  /**
   * Get DEK version info for the current context.
   */
  async dekVersions(): Promise<DekVersionInfo[]> {
    return this.#apiClient.call<DekVersionInfo[]>(
      "identity.dekVersions",
      {}
    );
  }

  // ─── Subscriptions ─────────────────────────────────────────────────

  /**
   * Subscribe to identity state changes.
   *
   * Returns an unsubscribe function.
   */
  subscribe(callbacks: IdentitySubscription): () => void {
    // Subscription implementation depends on the agent language's
    // change notification mechanism. The agent language uses
    // link-added/link-removed signals which the executor relays
    // as WebSocket events.
    const unsubscribers: (() => void)[] = [];

    // Wire to executor's WebSocket event stream when the identity.*
    // subscription endpoints get added to the RPC interface.

    return () => {
      for (const unsub of unsubscribers) {
        unsub();
      }
    };
  }
}
