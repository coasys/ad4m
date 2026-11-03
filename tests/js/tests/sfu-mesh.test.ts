import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  f3OneWayNat,
  w1mMeshMultiMachine,
  w3MeshRtt,
  w5TurnFallback,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: WebRTC network baselines", function () {
  this.timeout(600_000);

  sfuScenario(w1mMeshMultiMachine, 120_000);
  sfuScenario(w3MeshRtt, 120_000);
  sfuScenario(w5TurnFallback, 120_000);
  sfuScenario(f3OneWayNat);
});
