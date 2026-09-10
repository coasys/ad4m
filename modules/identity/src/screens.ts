/**
 * Screen definitions for the identity module.
 *
 * Each screen maps to a route in the module's navigation. The screens
 * correspond to the three ceremonies (C1/C2/C3) plus management views.
 *
 * The screen objects describe routing and layout — the actual SolidJS
 * components get injected by the host through `createIdentityModule`.
 */

/** Screen identifiers. */
export const SCREEN = {
  /** C1: Create identity — first-run flow. */
  ONBOARDING: "identity:onboarding",
  /** Home: everything that can act as you. */
  HOME: "identity:home",
  /** Detail: per-key info, rename, revoke. */
  DETAIL: "identity:detail",
  /** C2: Add a device/executor/assistant. */
  ADD: "identity:add",
  /** Assistant management. */
  ASSISTANTS: "identity:assistants",
  /** Guardian roster setup. */
  GUARDIANS: "identity:guardians",
  /** Recovery flow (request, approve, veto). */
  RECOVERY: "identity:recovery",
  /** KEL viewer — raw event log. */
  KEL_VIEWER: "identity:kel-viewer",
  /** Export — download the log. */
  EXPORT: "identity:export",
} as const;

export type ScreenId = (typeof SCREEN)[keyof typeof SCREEN];

/** Screen metadata for routing and navigation. */
export interface ScreenDefinition {
  id: ScreenId;
  /** Navigation label. */
  label: string;
  /** Icon name (design-system icon set). */
  icon: string;
  /** Whether this screen appears in the main nav. */
  showInNav: boolean;
  /** Route parameters (if any). */
  params?: string[];
}

/** All screens in navigation order. */
export const IDENTITY_SCREENS: ScreenDefinition[] = [
  {
    id: SCREEN.HOME,
    label: "Identity",
    icon: "fingerprint",
    showInNav: true,
  },
  {
    id: SCREEN.ADD,
    label: "Add device",
    icon: "plus-circle",
    showInNav: true,
  },
  {
    id: SCREEN.ASSISTANTS,
    label: "Assistants",
    icon: "robot",
    showInNav: true,
  },
  {
    id: SCREEN.GUARDIANS,
    label: "Guardians",
    icon: "shield-check",
    showInNav: true,
  },
  {
    id: SCREEN.RECOVERY,
    label: "Recovery",
    icon: "life-preserver",
    showInNav: false,
  },
  {
    id: SCREEN.KEL_VIEWER,
    label: "Event log",
    icon: "list-numbers",
    showInNav: true,
  },
  {
    id: SCREEN.EXPORT,
    label: "Export",
    icon: "download-simple",
    showInNav: true,
  },
  {
    id: SCREEN.DETAIL,
    label: "Key detail",
    icon: "key",
    showInNav: false,
    params: ["keyId"],
  },
  {
    id: SCREEN.ONBOARDING,
    label: "Create identity",
    icon: "user-plus",
    showInNav: false,
  },
];

// ─── User-facing copy ────────────────────────────────────────────────────

/** Consequence statements for the revocation confirmation (C3). */
export const REVOCATION_CONSEQUENCES = {
  /** What stays. */
  PAST_VALID:
    "Everything this key signed until now stays valid.",
  /** What breaks. */
  FUTURE_INVALID:
    "This key can no longer sign anything new as you.",
  /** Irreversibility. */
  IRREVERSIBLE:
    "You cannot undo a revocation.",
} as const;

/** Approval card wording for assistant claims. */
export const ASSISTANT_CLAIM_COPY = {
  /** The approval action. */
  ACTION: "become my assistant",
  /** NOT this. */
  NOT_THIS: "become me",
} as const;

/** Deferred-backup nag copy. */
export const DEFERRED_BACKUP_COPY = {
  /** Banner text. */
  BANNER: "Secure your identity",
  /** Risk statement. */
  RISK: "Before backup, a lost password means a lost account.",
} as const;

/** Guardian approval copy. */
export const GUARDIAN_APPROVAL_COPY = {
  /** Privacy statement on the approval card. */
  NO_ACCESS:
    "Approving this recovery request does not grant you access to this person's data.",
} as const;
