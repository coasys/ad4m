import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  f4NetworkPartition,
  f9CascadeNodeCrashCleanup,
  m3CascadeFailover,
  s2SfuCascade4Node,
  s3MaxParticipantsEnforced,
  t3SfuCascade2Node,
  t4SfuCascade3Node,
  t6PipeHandshake,
  t7SfuCascadeMedia,
  t11CascadeRebalance,
  t15SimulcastCascade,
} from "../helpers/sfu/scenarios/index.js";

/** Several executors sharing one room. */
describe("SFU: Cascade", function () {
  this.timeout(1200_000);

  sfuScenario(t3SfuCascade2Node, 300_000);
  sfuScenario(t4SfuCascade3Node, 300_000);
  sfuScenario(t6PipeHandshake);
  sfuScenario(t7SfuCascadeMedia, 300_000);
  sfuScenario(t11CascadeRebalance, 300_000);
  sfuScenario(t15SimulcastCascade, 300_000);
  sfuScenario(s2SfuCascade4Node, 300_000);
  sfuScenario(s3MaxParticipantsEnforced);
  sfuScenario(m3CascadeFailover, 300_000);
  sfuScenario(f4NetworkPartition, 300_000);
  sfuScenario(f9CascadeNodeCrashCleanup, 300_000);
});
