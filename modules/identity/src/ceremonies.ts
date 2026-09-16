/**
 * Ceremony logic — the three identity ceremonies as state machines.
 *
 * Each ceremony exports a step enum + a driver function that advances
 * through the steps. The SolidJS UI renders the current step; the
 * driver handles transitions and validation.
 *
 * These functions call IdentityClient (which routes through the agent
 * language). No perspective DB access.
 */

import type { IdentityClient, AgentType, Scope, EnrolOffer } from "@coasys/ad4m";

// ─── C1: Create identity ─────────────────────────────────────────────────

export enum CreateStep {
  /** Enter display name + password. */
  Credentials = "credentials",
  /** Generating keypair + inception event (spinner). */
  Generating = "generating",
  /** Show mnemonic words. */
  ShowMnemonic = "show-mnemonic",
  /** Confirm 2 of 12 words. */
  ConfirmMnemonic = "confirm-mnemonic",
  /** Done — identity created. */
  Done = "done",
  /** Error state. */
  Error = "error",
}

export interface CreateState {
  step: CreateStep;
  displayName: string;
  password: string;
  did: string | null;
  mnemonic: string | null;
  /** Indices of the two words to confirm. */
  confirmIndices: [number, number] | null;
  /** Whether mnemonic backup was deferred. */
  deferred: boolean;
  error: string | null;
}

export function initialCreateState(): CreateState {
  return {
    step: CreateStep.Credentials,
    displayName: "",
    password: "",
    did: null,
    mnemonic: null,
    confirmIndices: null,
    deferred: false,
    error: null,
  };
}

/**
 * Drive C1 forward.
 *
 * @param state - Current state (mutated in place for reactive frameworks)
 * @param client - Identity client
 * @param action - What happened
 */
export async function advanceCreate(
  state: CreateState,
  client: IdentityClient,
  action:
    | { type: "submit-credentials"; displayName: string; password: string; agentType?: AgentType }
    | { type: "defer-backup" }
    | { type: "confirm-words"; word1: string; word2: string }
): Promise<void> {
  switch (action.type) {
    case "submit-credentials": {
      state.displayName = action.displayName;
      state.password = action.password;
      state.step = CreateStep.Generating;

      try {
        const did = await client.createIdentity(
          action.displayName,
          action.password,
          action.agentType
        );
        state.did = did;

        const mnemonic = await client.generateMnemonic();
        state.mnemonic = mnemonic;

        // Pick two random indices for confirmation.
        const words = mnemonic.split(" ");
        const i1 = Math.floor(Math.random() * words.length);
        let i2 = Math.floor(Math.random() * (words.length - 1));
        if (i2 >= i1) i2++;
        state.confirmIndices = [i1, i2];

        state.step = CreateStep.ShowMnemonic;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = CreateStep.Error;
      }
      break;
    }

    case "defer-backup": {
      state.deferred = true;
      state.step = CreateStep.Done;
      break;
    }

    case "confirm-words": {
      if (!state.mnemonic || !state.confirmIndices) {
        state.error = "No mnemonic to confirm";
        state.step = CreateStep.Error;
        return;
      }

      try {
        const confirmed = await client.confirmMnemonicBackup([
          { index: state.confirmIndices[0], word: action.word1 },
          { index: state.confirmIndices[1], word: action.word2 },
        ]);

        if (confirmed) {
          state.step = CreateStep.Done;
        } else {
          state.error = "Words do not match. Check the mnemonic and try again.";
          state.step = CreateStep.ConfirmMnemonic;
        }
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = CreateStep.Error;
      }
      break;
    }
  }
}

// ─── C2: Connect device ──────────────────────────────────────────────────

export enum ConnectStep {
  /** Choose connection method. */
  ChooseMethod = "choose-method",
  /** QR: show offer. */
  QrShow = "qr-show",
  /** QR: scan from trusted device. */
  QrScan = "qr-scan",
  /** Mnemonic: enter recovery phrase. */
  MnemonicEntry = "mnemonic-entry",
  /** Hosted: review executor request. */
  HostedReview = "hosted-review",
  /** Processing (spinner). */
  Processing = "processing",
  /** Done — device enrolled. */
  Done = "done",
  /** Error state. */
  Error = "error",
}

export type ConnectMethod = "qr" | "mnemonic" | "hosted";

export interface ConnectState {
  step: ConnectStep;
  method: ConnectMethod | null;
  offer: EnrolOffer | null;
  newKeyId: string | null;
  error: string | null;
}

export function initialConnectState(): ConnectState {
  return {
    step: ConnectStep.ChooseMethod,
    method: null,
    offer: null,
    newKeyId: null,
    error: null,
  };
}

/**
 * Drive C2 forward.
 */
export async function advanceConnect(
  state: ConnectState,
  client: IdentityClient,
  action:
    | { type: "choose-method"; method: ConnectMethod }
    | { type: "create-offer"; label: string; scope: Scope }
    | { type: "approve-offer"; offer: EnrolOffer }
    | { type: "submit-mnemonic"; mnemonic: string; label: string }
    | { type: "approve-hosted"; publicKey: string; label: string; challenge: string; scope: Scope }
): Promise<void> {
  switch (action.type) {
    case "choose-method": {
      state.method = action.method;
      switch (action.method) {
        case "qr":
          state.step = ConnectStep.QrShow;
          break;
        case "mnemonic":
          state.step = ConnectStep.MnemonicEntry;
          break;
        case "hosted":
          state.step = ConnectStep.HostedReview;
          break;
      }
      break;
    }

    case "create-offer": {
      try {
        state.step = ConnectStep.Processing;
        const offer = await client.createEnrolOffer(action.label, action.scope);
        state.offer = offer;
        state.step = ConnectStep.QrShow;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = ConnectStep.Error;
      }
      break;
    }

    case "approve-offer": {
      try {
        state.step = ConnectStep.Processing;
        const keyId = await client.approveEnrolment(action.offer);
        state.newKeyId = keyId;
        state.step = ConnectStep.Done;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = ConnectStep.Error;
      }
      break;
    }

    case "submit-mnemonic": {
      try {
        state.step = ConnectStep.Processing;
        const keyId = await client.enrolViaMnemonic(
          action.mnemonic,
          action.label
        );
        state.newKeyId = keyId;
        state.step = ConnectStep.Done;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = ConnectStep.Error;
      }
      break;
    }

    case "approve-hosted": {
      try {
        state.step = ConnectStep.Processing;
        const keyId = await client.approveHostedEnrolment({
          publicKey: action.publicKey,
          label: action.label,
          challenge: action.challenge,
          scope: action.scope,
        });
        state.newKeyId = keyId;
        state.step = ConnectStep.Done;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = ConnectStep.Error;
      }
      break;
    }
  }
}

// ─── C3: Revoke ──────────────────────────────────────────────────────────

export enum RevokeStep {
  /** Select key to revoke. */
  SelectKey = "select-key",
  /** Confirm consequences. */
  Confirm = "confirm",
  /** Processing (spinner). */
  Processing = "processing",
  /** Show propagation proof. */
  PropagationProof = "propagation-proof",
  /** Done. */
  Done = "done",
  /** Error state. */
  Error = "error",
}

export interface RevokeState {
  step: RevokeStep;
  keyId: string | null;
  /** The KEL sequence of the revocation. */
  revokedAtSeq: number | null;
  error: string | null;
}

export function initialRevokeState(): RevokeState {
  return {
    step: RevokeStep.SelectKey,
    keyId: null,
    revokedAtSeq: null,
    error: null,
  };
}

/**
 * Drive C3 forward.
 */
export async function advanceRevoke(
  state: RevokeState,
  client: IdentityClient,
  action:
    | { type: "select-key"; keyId: string }
    | { type: "confirm-revoke" }
): Promise<void> {
  switch (action.type) {
    case "select-key": {
      state.keyId = action.keyId;
      state.step = RevokeStep.Confirm;
      break;
    }

    case "confirm-revoke": {
      if (!state.keyId) {
        state.error = "No key selected";
        state.step = RevokeStep.Error;
        return;
      }

      try {
        state.step = RevokeStep.Processing;
        const seq = await client.revokeKey(state.keyId);
        state.revokedAtSeq = seq;
        state.step = RevokeStep.PropagationProof;
      } catch (e) {
        state.error = e instanceof Error ? e.message : String(e);
        state.step = RevokeStep.Error;
      }
      break;
    }
  }
}
