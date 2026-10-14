/**
 * An end-to-end wind-tunnel scenario: a world, calls in it, and steps.
 *
 * A step that fails ends the scenario and names itself. If a participant's
 * process died of a signal nobody sent — the libwebrtc test binding crashed,
 * as it rarely does while connections close — the scenario runs once more
 * in a fresh world; an assertion failure never does.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { Call } from "./call.js";
import { CAMERA } from "./media.js";
import type { Size } from "./protocol.js";
import { Participant } from "./participant.js";
import { startWorld, type World, type WorldSpec } from "./world.js";

const BINDING_CRASH_RETRIES = 1;

export interface E2eContext {
  world: World;
  /**
   * A call in `neighbourhood`'s `room`, ended for you when the scenario ends.
   * `video` is the camera's size (default `CAMERA`), or false for no camera.
   */
  call(neighbourhood: string, room: string, options?: { video?: Size | false }): Call;
  /** Run `fn` as a named step; its duration is a sample. */
  step<T>(name: string, fn: () => Promise<T>): Promise<T>;
  metric(name: string, value: unknown): void;
}

export interface E2eDefinition {
  id: string;
  name: string;
  description: string;
  world: Omit<WorldSpec, "id">;
  unavailable?(): string | null;
  run(ctx: E2eContext): Promise<void>;
}

export function e2eScenario(def: E2eDefinition): Scenario {
  return {
    id: def.id,
    name: def.name,
    description: def.description,
    managesOwnEnvironment: true,
    unavailable: def.unavailable,
    async run(ctx: ScenarioContext): Promise<ScenarioResult> {
      const crashes: string[] = [];
      for (let attempt = 0; ; attempt++) {
        const outcome = await runOnce(def, ctx.branch, attempt);
        if (outcome.result.passed || !outcome.crashed || attempt >= BINDING_CRASH_RETRIES) {
          if (crashes.length) outcome.result.metrics["binding_crashes"] = crashes;
          return outcome.result;
        }
        crashes.push(outcome.crashed);
        console.warn(`[${def.id}] attempt ${attempt + 1} lost to a libwebrtc binding crash (${outcome.crashed}); retrying`);
      }
    },
  };
}

async function runOnce(def: E2eDefinition, branch: string, attempt: number): Promise<{ result: ScenarioResult; crashed: string | null }> {
  const startTime = Date.now();
  const samples: ScenarioResult["samples"] = [];
  const metrics: Record<string, unknown> = {};
  const participants: Participant[] = [];
  const calls: Call[] = [];
  let failure: string | null = null;
  let world: World | null = null;

  try {
    world = await startWorld({ ...def.world, id: `${def.id}-${attempt}-${Date.now().toString(36)}` });
    const w = world;
    await def.run({
      world: w,
      call: (neighbourhood, room, options) => {
        const call = new Call(w, neighbourhood, room, (p) => participants.push(p), options?.video === false ? null : options?.video ?? CAMERA);
        calls.push(call);
        return call;
      },
      step: async (name, fn) => {
        const started = Date.now();
        try {
          const result = await fn();
          samples.push({ name, durationMs: Date.now() - started, timestamp: Date.now() });
          console.log(`[${def.id}] ✓ ${name} (${Date.now() - started} ms)`);
          return result;
        } catch (error) {
          const message = error instanceof Error ? error.message : String(error);
          samples.push({ name, durationMs: Date.now() - started, timestamp: Date.now(), error: message });
          console.log(`[${def.id}] ✗ ${name}: ${message}`);
          throw new Error(`step "${name}": ${message}`);
        }
      },
      metric: (name, value) => { metrics[name] = value; },
    });
  } catch (error) {
    failure = error instanceof Error ? error.message : String(error);
  } finally {
    for (const call of calls) await call.end().catch(() => {});
    await world?.stop().catch(() => {});
  }

  const crashed = participants.find((p) => p.crashed);
  const endTime = Date.now();
  return {
    crashed: crashed ? `${crashed.name}: ${crashed.exit?.signal}` : null,
    result: {
      scenario: def.id,
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: failure === null,
      metrics,
      samples,
      summary: failure === null ? `${def.name}: every step passed` : `FAILED: ${failure}`,
    },
  };
}
