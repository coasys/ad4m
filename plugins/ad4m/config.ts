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

/**
 * Load persisted waker subscriptions from the state directory.
 * Returns an empty array if the file doesn't exist or is invalid.
 */
export function loadWakerState(stateDir: string): WakerSubscription[] {
  try {
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    const raw = fs.readFileSync(filePath, "utf-8");
    const data = JSON.parse(raw);
    if (Array.isArray(data)) return data;
    return [];
  } catch {
    return [];
  }
}

/**
 * Persist waker subscriptions to the state directory.
 * Creates the stateDir if it doesn't exist.
 */
export function saveWakerState(
  stateDir: string,
  subs: WakerSubscription[],
): void {
  try {
    if (!fs.existsSync(stateDir)) {
      fs.mkdirSync(stateDir, { recursive: true });
    }
    const filePath = path.join(stateDir, WAKER_STATE_FILE);
    fs.writeFileSync(filePath, JSON.stringify(subs, null, 2), "utf-8");
  } catch {
    // Best-effort — don't crash if we can't write
  }
}
