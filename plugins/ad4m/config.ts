import fs from "fs";
import path from "path";
import type { WakerSubscription } from "./types";

export function generateRandomPassphrase(length: number = 32): string {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < length; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

// ---------------------------------------------------------------------------
// Waker subscription persistence via stateDir
// ---------------------------------------------------------------------------

const WAKER_STATE_FILE = "ad4m-waker-state.json";

export interface WakerState {
  subscriptions: WakerSubscription[];
  /** Per-subscription seen message IDs (for mention subs) or result hashes (for channel subs). */
  seenMessages: Record<string, string[]>;
}

/**
 * Load persisted waker state from the state directory.
 * Backwards-compatible with old format that used resultHashes.
 */
export function loadWakerState(stateDir: string): WakerState {
  try {
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    const raw = fs.readFileSync(filePath, "utf-8");
    const data = JSON.parse(raw);
    // Support legacy format (plain array)
    if (Array.isArray(data)) {
      return { subscriptions: data, seenMessages: {} };
    }
    return {
      subscriptions: Array.isArray(data.subscriptions) ? data.subscriptions : [],
      seenMessages: data.seenMessages ?? {},
    };
  } catch {
    return { subscriptions: [], seenMessages: {} };
  }
}

/**
 * Persist waker state to the state directory.
 * Creates the stateDir if it doesn't exist.
 */
export function saveWakerState(
  stateDir: string,
  subs: WakerSubscription[],
  seenMessages: Record<string, string[]> = {},
): void {
  try {
    if (!fs.existsSync(stateDir)) {
      fs.mkdirSync(stateDir, { recursive: true });
    }
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    const state: WakerState = { subscriptions: subs, seenMessages };
    fs.writeFileSync(filePath, JSON.stringify(state, null, 2), "utf-8");
  } catch {
    // Best-effort — don't crash if we can't write
  }
}
