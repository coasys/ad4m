export type KeyRingStatus = "pending" | "ready" | "error";

// Commit is user-initiated and its failure is visible, so it always
// retries the key ring without consulting the background cooldown.
export function commitNeedsKeyRetry(status: KeyRingStatus): boolean {
    return status === "error" || status === "pending";
}
