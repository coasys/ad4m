import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  w1Mesh2Peer,
  w1mMeshMultiMachine,
  w2Mesh4Peer,
  w3MeshRtt,
  w4MeshBandwidthScaling,
  w5TurnFallback,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: Mesh baselines", function () {
  this.timeout(600_000);

  sfuScenario(w1Mesh2Peer, 120_000);
  sfuScenario(w1mMeshMultiMachine, 120_000);
  sfuScenario(w2Mesh4Peer, 120_000);
  sfuScenario(w3MeshRtt, 120_000);
  sfuScenario(w4MeshBandwidthScaling, 120_000);
  sfuScenario(w5TurnFallback, 120_000);
});
