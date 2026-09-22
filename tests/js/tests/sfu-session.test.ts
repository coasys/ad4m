import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  t16SessionLifecycle,
  t17SessionDataChannel,
  t18MeshSessionLifecycle,
  t19MeshDataChannel,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: Session surface", function () {
  this.timeout(600_000);

  sfuScenario(t16SessionLifecycle);
  sfuScenario(t17SessionDataChannel, 180_000, "sfu.sendData / sfu-data event path not delivering");
  sfuScenario(t18MeshSessionLifecycle);
  sfuScenario(t19MeshDataChannel);
});
