import { computeFlowEvidenceHash } from "./FlowEvidenceHash";
import { createHash } from "crypto";

describe("computeFlowEvidenceHash", () => {
  // ── Golden fixtures — locked byte-for-byte with the Rust `evidence_hash` ──
  //
  // Each expected hex is the SHA-256 of the exact byte string documented
  // above the assertion, computed with `printf '...' | sha256sum` and
  // matching the Rust implementation in
  // `rust-executor/src/perspectives/flow_evaluator.rs::evidence_hash`.
  // A change to any expected value here MUST be paired with the Rust
  // parity update in the same commit + a schema-version bump.

  it("hashes empty evidence with a class name — matches printf 'ns://Perspective\\0' | sha256sum", () => {
    // Bytes: 6e 73 3a 2f 2f 50 65 72 73 70 65 63 74 69 76 65 00
    //         ('ns://Perspective' UTF-8) + NUL
    expect(computeFlowEvidenceHash(["ns://Perspective"], [])).toBe(
      "2fa6bf06f407e1eeeda6f76b92285cdc2fd88feaaa141807aade362459990872",
    );
  });

  it("joins multiple class names with '|' and evidence with '\\n' — matches printf 'ns://X|ns://Y\\0a\\nb'", () => {
    // Note: evidence ["b","a"] gets sorted to "a\nb".
    expect(computeFlowEvidenceHash(["ns://X", "ns://Y"], ["b", "a"])).toBe(
      "5245f683b6dcc4efe4ce46e7b0126bd56a37c8794298c2213a335248a9383f66",
    );
  });

  it("hashes a single class + single evidence — matches printf 'ns://X\\0a://1'", () => {
    expect(computeFlowEvidenceHash(["ns://X"], ["a://1"])).toBe(
      "dcbb3c36dba1ec498c46f6f6129ae78e6585a781abd1a89f8ac6d5f7c4a3e568",
    );
  });

  // ── Invariants — mirror the Rust unit tests ──

  it("is stable across evidence-ID permutations (mirrors Rust's evidence_hash_stable_across_id_permutations)", () => {
    const classes = ["ns://Perspective"];
    const a = computeFlowEvidenceHash(classes, ["b://2", "a://1", "c://3"]);
    const b = computeFlowEvidenceHash(classes, ["a://1", "b://2", "c://3"]);
    const c = computeFlowEvidenceHash(classes, ["c://3", "a://1", "b://2"]);
    expect(a).toBe(b);
    expect(a).toBe(c);
  });

  it("differs when class list changes (mirrors Rust's evidence_hash_differs_on_class_change)", () => {
    const ids = ["a://1", "b://2"];
    const a = computeFlowEvidenceHash(["ns://Perspective"], ids);
    const b = computeFlowEvidenceHash(["ns://Tension"], ids);
    expect(a).not.toBe(b);
  });

  it("differs when any evidence ID changes (mirrors Rust's evidence_hash_differs_on_id_diff)", () => {
    const classes = ["ns://Perspective"];
    const a = computeFlowEvidenceHash(classes, ["a://1"]);
    const b = computeFlowEvidenceHash(classes, ["a://2"]);
    expect(a).not.toBe(b);
  });

  it("differs when class-list order changes (author-controlled ordering is load-bearing)", () => {
    const ids = ["a"];
    const a = computeFlowEvidenceHash(["ns://X", "ns://Y"], ids);
    const b = computeFlowEvidenceHash(["ns://Y", "ns://X"], ids);
    expect(a).not.toBe(b);
  });

  it("always emits 64 lowercase hex chars (mirrors Rust's evidence_hash_hex_length)", () => {
    const h = computeFlowEvidenceHash(["ns://X"], []);
    expect(h).toHaveLength(64);
    expect(h).toMatch(/^[0-9a-f]{64}$/);
  });

  it("does not mutate the caller's evidence array (defensive copy for sort)", () => {
    const evidence = ["z", "a", "m"];
    const snapshot = [...evidence];
    computeFlowEvidenceHash(["ns://X"], evidence);
    expect(evidence).toEqual(snapshot);
  });

  it("agrees with an independent Node-native SHA-256 of the documented byte layout", () => {
    // Cross-check against a second implementation using the same crypto
    // primitive but a different concatenation surface. Catches any
    // future refactor that accidentally reorders / re-encodes.
    const classes = ["ad4m://Post", "ad4m://Like"];
    const ids = ["urn:x:2", "urn:x:1", "urn:x:3"];
    const sorted = [...ids].sort();
    const expected = createHash("sha256")
      .update(Buffer.concat([
        Buffer.from(classes.join("|"), "utf8"),
        Buffer.from([0x00]),
        Buffer.from(sorted.join("\n"), "utf8"),
      ]))
      .digest("hex");
    expect(computeFlowEvidenceHash(classes, ids)).toBe(expected);
  });
});
