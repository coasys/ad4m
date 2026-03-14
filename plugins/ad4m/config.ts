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
  resultHashes: Record<string, string>;
}

/**
 * Load persisted waker state from the state directory.
 * Returns subscriptions and result hashes (for duplicate wake prevention).
 */
export function loadWakerState(stateDir: string): WakerState {
  try {
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    const raw = fs.readFileSync(filePath, "utf-8");
    const data = JSON.parse(raw);
    // Support legacy format (plain array) and new format (object with subscriptions + resultHashes)
    if (Array.isArray(data)) {
      return { subscriptions: data, resultHashes: {} };
    }
    return {
      subscriptions: Array.isArray(data.subscriptions) ? data.subscriptions : [],
      resultHashes: data.resultHashes ?? {},
    };
  } catch {
    return { subscriptions: [], resultHashes: {} };
  }
}

/**
 * Persist waker state to the state directory.
 * Creates the stateDir if it doesn't exist.
 */
export function saveWakerState(
  stateDir: string,
  subs: WakerSubscription[],
  resultHashes: Record<string, string> = {},
): void {
  try {
    if (!fs.existsSync(stateDir)) {
      fs.mkdirSync(stateDir, { recursive: true });
    }
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    const state: WakerState = { subscriptions: subs, resultHashes };
    fs.writeFileSync(filePath, JSON.stringify(state, null, 2), "utf-8");
  } catch {
    // Best-effort — don't crash if we can't write
  }
}
