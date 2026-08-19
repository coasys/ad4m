/**
 * InterpretationOverlay / InterpretationRun / AutoProcessorConfig — @Model
 * classes that mirror the Rust SDNA for the generic interpretation subsystem.
 *
 * These tests verify that:
 *   1. The TS SHACL shapes can be registered in a perspective.
 *   2. `InterpretationOverlay.findAll()` returns nodes that carry the
 *      discriminating `ad4m://interp/kind` link (the pending-proposals query
 *      James will use from Flux to show "LLM proposals awaiting your review").
 *   3. `InterpretationRun.findAll()` returns completed-run nodes.
 *   4. `AutoProcessorConfig.findAll()` returns processor configuration nodes.
 *
 * Overlays, runs, and configs are normally written by the Rust executor.
 * Here we add the raw links manually so the test does not require a running
 * LLM — it is a pure graph-layer check.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit \
 *     --require tests/model/hooks.ts tests/model/interpretation-models.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import { AutoProcessorConfig, InterpretationOverlay, InterpretationRun } from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent } from "../../helpers/index.js";

describe("InterpretationOverlay / InterpretationRun / AutoProcessorConfig — @Model", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("interpretation-models");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("interp-models-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    // Register the three SHACL shapes so the model query engine knows them.
    await InterpretationOverlay.register(p);
    await InterpretationRun.register(p);
    await AutoProcessorConfig.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  // ── InterpretationOverlay ──────────────────────────────────────────────────

  it("InterpretationOverlay.findAll() returns pending proposals by kind link", async () => {
    // Simulate what the Rust executor writes when it stages an LLM proposal.
    await p.add(new Link({
      source: "test://instance/1",
      predicate: "ad4m://interp/kind",
      target: "literal:string:create",
    }));
    await p.add(new Link({
      source: "test://instance/1",
      predicate: "ad4m://interp/run",
      target: "literal:string:run-abc",
    }));
    // A second overlay on a different base.
    await p.add(new Link({
      source: "test://instance/2",
      predicate: "ad4m://interp/kind",
      target: "literal:string:update",
    }));

    const overlays = await InterpretationOverlay.findAll(p);
    expect(overlays.length, "should find both overlay nodes").to.equal(2);

    const kinds = overlays.map((o) => o.kind).sort();
    expect(kinds).to.deep.equal(["create", "update"]);

    const withRun = overlays.find((o) => o.run != null);
    expect(withRun, "one overlay should have a run reference").to.exist;
    expect(withRun!.run).to.equal("run-abc");
  });

  it("InterpretationOverlay.findAll() returns empty when no overlays exist", async () => {
    // No overlay links → findAll must return [].
    const overlays = await InterpretationOverlay.findAll(p);
    expect(overlays).to.be.an("array").with.length(0);
  });

  // ── InterpretationRun ──────────────────────────────────────────────────────

  it("InterpretationRun.findAll() returns completed-run nodes", async () => {
    // Simulate a completed interpretation run.
    await p.add(new Link({
      source: "ad4m://interp/run/run-001",
      predicate: "ad4m://type",
      target: "ad4m://interpretation-run",
    }));
    await p.add(new Link({
      source: "ad4m://interp/run/run-001",
      predicate: "ad4m://interp/run_id",
      target: "literal:string:run-001",
    }));
    await p.add(new Link({
      source: "ad4m://interp/run/run-001",
      predicate: "ad4m://interp/model",
      target: "literal:string:gemma3:12b",
    }));

    const runs = await InterpretationRun.findAll(p);
    expect(runs.length).to.equal(1);
    expect(runs[0].runId).to.equal("run-001");
    expect(runs[0].model).to.equal("gemma3:12b");
  });

  // ── AutoProcessorConfig ────────────────────────────────────────────────────

  it("AutoProcessorConfig.findAll() returns processor configuration nodes", async () => {
    // Simulate what addAutoProcessor writes to the perspective.
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "rdf://type",
      target: "ad4m://AutoProcessor",
    }));
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "ad4m://processor_id",
      target: "literal:string:my-proc",
    }));
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "ad4m://source_scope_query",
      target: "literal:string:SELECT ?x WHERE { ?x a ?y }",
    }));
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "ad4m://debounce_ms",
      target: "literal:string:300",
    }));
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "ad4m://batch_max",
      target: "literal:string:16",
    }));
    await p.add(new Link({
      source: "ad4m://autoprocessor/my-proc",
      predicate: "ad4m://claim_ttl_ms",
      target: "literal:string:30000",
    }));

    const configs = await AutoProcessorConfig.findAll(p);
    expect(configs.length).to.equal(1);
    expect(configs[0].processorId).to.equal("my-proc");
    expect(configs[0].debounceMs).to.equal("300");
    expect(configs[0].batchMax).to.equal("16");
  });

  // ── SDNA parity — TS @Model shape agrees with Rust hardwired SDNA ──────────
  //
  // The three hardwired classes below carry TWO parallel SHACL declarations —
  // one in Rust (`config::AUTO_PROCESSOR_SDNA`, `overlay::classes::
  // INTERP_RUN_SDNA`, `INTERP_OVERLAY_SDNA`) and one on the TS side via the
  // `@Model` decorators in `InterpretationModels.ts`. They MUST stay in sync
  // or a TS-written instance becomes invisible / unreadable to the Rust
  // watcher (and vice-versa). These tests pin the invariant by asserting the
  // TS-generated shape's property PREDICATES match the Rust-declared set,
  // property-by-property. If either side gains or renames a property, the
  // corresponding test fails until both sides move together.
  //
  // The reference sets below are the source of truth. Update BOTH sides at
  // once when adding a hardwired property.

  it("AutoProcessorConfig @Model shape matches Rust AUTO_PROCESSOR_SDNA", () => {
    const { shape } = (AutoProcessorConfig as any).generateSHACL();
    const paths = new Set(shape.properties.map((p: any) => p.path));

    // Mirrors Rust `AUTO_PROCESSOR_SDNA` in rust-executor/src/perspectives/
    // auto_processor/config.rs.
    expect(shape.target_class, "target class must match Rust SDNA")
      .to.equal("ad4m://AutoProcessor");
    const expected = new Set([
      "rdf://type",
      "ad4m://processor_id",
      "ad4m://source_scope_query",
      "ad4m://base_prefix",
      "ad4m://interpretation_class",
      "ad4m://debounce_ms",
      "ad4m://batch_min",
      "ad4m://batch_max",
      "ad4m://max_wait_ms",
      "ad4m://claim_ttl_ms",
      "ad4m://source_window_ms",
      "ad4m://dedup_strategy",
      "ad4m://existing_scope",
      "ad4m://mint_scope",
      "ad4m://debug_mode",
    ]);

    expect(paths, "TS shape must declare exactly the Rust SDNA property paths")
      .to.deep.equal(expected);
  });

  it("InterpretationRun @Model shape matches Rust INTERP_RUN_SDNA", () => {
    const { shape } = (InterpretationRun as any).generateSHACL();
    const paths = new Set(shape.properties.map((p: any) => p.path));

    // Mirrors Rust `INTERP_RUN_SDNA` in rust-executor/src/perspectives/
    // interpretation/overlay/classes.rs.
    expect(shape.target_class, "target class must match Rust SDNA")
      .to.equal("ad4m://InterpretationRun");
    const expected = new Set([
      "ad4m://type",
      "ad4m://interp/run_id",
      "ad4m://interp/model",
      "ad4m://interp/prompt_version",
      "ad4m://interp/ran_at",
      "ad4m://interp/processor",
      "ad4m://interp/sources",
      "ad4m://interp/debug_prompt",
      "ad4m://interp/debug_response",
    ]);

    expect(paths, "TS shape must declare exactly the Rust SDNA property paths")
      .to.deep.equal(expected);
  });

  it("InterpretationOverlay @Model shape matches Rust INTERP_OVERLAY_SDNA", () => {
    const { shape } = (InterpretationOverlay as any).generateSHACL();
    const paths = new Set(shape.properties.map((p: any) => p.path));

    // Mirrors Rust `INTERP_OVERLAY_SDNA` in rust-executor/src/perspectives/
    // interpretation/overlay/classes.rs. NOTE: the overlay class carries no
    // `type` flag by design — the `kind` link is the discriminator.
    expect(shape.target_class, "target class must match Rust SDNA")
      .to.equal("ad4m://InterpretationOverlay");
    const expected = new Set([
      "ad4m://interp/kind",
      "ad4m://interp/run",
    ]);

    expect(paths, "TS shape must declare exactly the Rust SDNA property paths")
      .to.deep.equal(expected);
  });
});
