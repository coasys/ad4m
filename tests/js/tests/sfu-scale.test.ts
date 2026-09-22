import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  s1Sfu20Peer,
  s2SfuCascade4Node,
  s3MaxParticipantsEnforced,
  s4SfuMemoryChurn,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: Scale", function () {
  this.timeout(1200_000);

  sfuScenario(s1Sfu20Peer, 300_000);
  sfuScenario(s2SfuCascade4Node, 300_000);
  sfuScenario(s3MaxParticipantsEnforced);
  sfuScenario(s4SfuMemoryChurn, 300_000);
});
