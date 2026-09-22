import { sfuScenario } from "../helpers/sfu/harness.js";
import {
  t1Sfu5Peer,
  t2Sfu10Peer,
  t3SfuCascade2Node,
  t4SfuCascade3Node,
  t6PipeHandshake,
  t7SfuCascadeMedia,
  t8ConcurrentJoinRace,
  t9TrackDidAttribution,
  t10SimulcastLayerSelection,
  t11CascadeRebalance,
  t12DeferredTracks,
  t13PeerDepartureMedia,
  t14MediaRoutingCorrectness,
  t15SimulcastCascade,
} from "../helpers/sfu/scenarios/index.js";

describe("SFU: Topology", function () {
  this.timeout(1200_000);

  sfuScenario(t1Sfu5Peer);
  sfuScenario(t2Sfu10Peer);
  sfuScenario(t3SfuCascade2Node, 300_000);
  sfuScenario(t4SfuCascade3Node, 300_000);
  sfuScenario(t6PipeHandshake);
  sfuScenario(t7SfuCascadeMedia, 300_000);
  sfuScenario(t8ConcurrentJoinRace);
  sfuScenario(t9TrackDidAttribution);
  sfuScenario(t10SimulcastLayerSelection);
  sfuScenario(t11CascadeRebalance, 300_000);
  sfuScenario(t12DeferredTracks);
  sfuScenario(t13PeerDepartureMedia);
  sfuScenario(t14MediaRoutingCorrectness);
  sfuScenario(t15SimulcastCascade, 300_000);
});
