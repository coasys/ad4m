import { sfuScenario } from "../helpers/sfu/harness.js";
import { t20MeshManagerLossy, t21ColocatedCall } from "../helpers/sfu/scenarios/index.js";

describe("SFU: Session surface", function () {
  this.timeout(600_000);

  sfuScenario(t20MeshManagerLossy);
  // Starts two executors with Holochain and the prepared test languages.
  sfuScenario(t21ColocatedCall, 900_000);
});
