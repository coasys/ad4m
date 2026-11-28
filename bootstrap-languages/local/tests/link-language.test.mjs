import "./ad4m-host.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";

const language = await import("../link-language.js");

function link(target, predicate) {
    const data = { source: "ad4m://self", target };
    if (predicate !== undefined) data.predicate = predicate;
    return { author: "did:key:alice", timestamp: "2026-11-28T00:00:00.000Z", data, proof: {} };
}

// The executor's SPARQL copy has no predicate where the link had "" or null,
// and its own property order: neither makes a held link read as missing.
test("perspectiveSyncMissing treats an empty, null or absent predicate as one", async () => {
    await language.init();
    await language.perspectiveCommit({
        additions: [link("empty", ""), link("null", null), link("absent"), link("has", "ad4m://p")],
        removals: [],
    });

    const asked = [
        { ...link("empty"), data: { target: "empty", source: "ad4m://self" } },
        link("null"),
        link("absent", ""),
        link("has", "ad4m://p"),
        link("has", "ad4m://other"),
        link("new"),
    ];
    const missing = await language.perspectiveSyncMissing(asked);

    assert.deepEqual(missing.map((l) => l.data.target), ["has", "new"]);
    assert.equal(missing[0].data.predicate, "ad4m://other");
});
