// Single-executor calls run end to end through the SDK: see ../e2e/.

// Cascade: several executors sharing one room
export { t3SfuCascade2Node } from "./t3-sfu-cascade-2node.js";
export { t4SfuCascade3Node } from "./t4-sfu-cascade-3node.js";
export { t6PipeHandshake } from "./t6-pipe-handshake.js";
export { t7SfuCascadeMedia } from "./t7-sfu-cascade-media.js";
export { t11CascadeRebalance } from "./t11-cascade-rebalance.js";
export { t15SimulcastCascade } from "./t15-simulcast-cascade.js";
export { s2SfuCascade4Node } from "./s2-sfu-cascade-4node.js";
export { s3MaxParticipantsEnforced } from "./s3-max-participants.js";
export { m3CascadeFailover } from "./m3-cascade-failover.js";
export { f4NetworkPartition } from "./f4-network-partition.js";
export { f9CascadeNodeCrashCleanup } from "./f9-cascade-node-crash-cleanup.js";

// Session components and executor pairs
export { t20MeshManagerLossy } from "./t20-mesh-manager-lossy.js";
export { t21ColocatedCall } from "./t21-colocated-call.js";

// WebRTC network baselines
export { w1mMeshMultiMachine } from "./w1m-mesh-multimachine.js";
export { w3MeshRtt } from "./w3-mesh-rtt.js";
export { w5TurnFallback } from "./w5-turn-fallback.js";
export { f3OneWayNat } from "./f3-one-way-nat.js";
