/**
 * Cross-package invariant test: the client's store.ts:hashLink and the
 * server's types.ts:linkHash MUST produce identical output for the same
 * link. Both normalize predicate to null when missing and use the same
 * field order: {source, predicate, target, author, timestamp}. Divergence
 * between the two implementations would cause OR-Set membership failures
 * (client hashes don't match server hashes → phantom links, failed
 * removals, or revision drift).
 */
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { test } from "node:test";
import { canonicalLinkPayload, linkHash } from "../src/types.js";

/** Replicates the client's store.ts:hashLink computation exactly. */
function clientSideHash(link: {
  data: { source: string; predicate?: string | null; target: string };
  author: string;
  timestamp: string;
}): string {
  const content = JSON.stringify({
    source: link.data.source,
    predicate: link.data.predicate ?? null,
    target: link.data.target,
    author: link.author,
    timestamp: link.timestamp,
  });
  return createHash("sha256").update(content, "utf8").digest("hex");
}

test("linkHash matches the client-side hashLink computation for plaintext links", () => {
  const fixtures = [
    {
      data: { source: "a", predicate: "b", target: "c" },
      author: "did:key:z123",
      timestamp: "2024-01-01T00:00:00Z",
    },
    {
      data: { source: "a", predicate: null, target: "c" },
      author: "did:key:z123",
      timestamp: "2024-01-01T00:00:00Z",
    },
    {
      data: { source: "a", predicate: undefined as unknown as null, target: "c" },
      author: "did:key:z456",
      timestamp: "2024-06-15T12:30:00Z",
    },
    {
      data: { source: "", predicate: "", target: "" },
      author: "",
      timestamp: "",
    },
    {
      data: { source: "s", predicate: "p", target: "t" },
      author: "did:key:zLong" + "x".repeat(100),
      timestamp: "2025-12-31T23:59:59.999Z",
    },
  ];

  for (const link of fixtures) {
    const serverHash = linkHash(link as any);
    const clientHash = clientSideHash(link as any);
    assert.equal(
      serverHash,
      clientHash,
      `linkHash mismatch for fixture: ${JSON.stringify(link.data)}`,
    );
  }
});

test("canonicalLinkPayload returns link_hash directly for encrypted links", () => {
  const encrypted = {
    data: { ciphertext: "abc123", nonce: "def456" },
    link_hash: "deadbeef".repeat(8),
  };
  assert.equal(canonicalLinkPayload(encrypted as any), encrypted.link_hash);
  assert.equal(linkHash(encrypted as any), encrypted.link_hash);
});

test("linkHash null vs undefined predicate produce the same hash", () => {
  const withNull = {
    data: { source: "s", predicate: null as string | null, target: "t" },
    author: "did:key:z1",
    timestamp: "2024-01-01T00:00:00Z",
  };
  const withUndefined = {
    data: { source: "s", predicate: undefined as unknown as string | null, target: "t" },
    author: "did:key:z1",
    timestamp: "2024-01-01T00:00:00Z",
  };
  assert.equal(linkHash(withNull as any), linkHash(withUndefined as any));
});
