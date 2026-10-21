export type KeyRingStatus = "none" | "pending" | "ready" | "error";

// Commit is user-initiated and its failure is visible, so it always
// retries the key ring without consulting the background cooldown.
export function commitNeedsKeyRetry(status: KeyRingStatus): boolean {
    return status === "error" || status === "pending";
}

// The background key refresh (sync's catch-up) waits out its cooldown, except
// while this agent waits for its first keys: then every sync re-checks /keys,
// so a joiner sees the room as soon as the admin grants, not up to 10 s later.
export function refreshSkippedByCooldown(status: KeyRingStatus, msSinceLast: number, cooldownMs: number): boolean {
    return status !== "pending" && msSinceLast < cooldownMs;
}
