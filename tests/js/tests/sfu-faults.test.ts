import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  f1MeshPacketLoss,
  f2SfuPacketLoss,
  f3OneWayNat,
  f4NetworkPartition,
  f5RenegotiationFlood,
  f6NonMemberJoin,
  f7BadCapability,
  f8StuckRenegotiationRecovery,
  f9CascadeNodeCrashCleanup,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: Fault injection", function () {
  this.timeout(1200_000);

  sfuScenario(f1MeshPacketLoss);
  sfuScenario(f2SfuPacketLoss);
  sfuScenario(f3OneWayNat);
  sfuScenario(f4NetworkPartition, 300_000);
  sfuScenario(f5RenegotiationFlood);
  sfuScenario(f6NonMemberJoin);
  sfuScenario(f7BadCapability);
  sfuScenario(f8StuckRenegotiationRecovery);
  sfuScenario(f9CascadeNodeCrashCleanup, 300_000);
});
