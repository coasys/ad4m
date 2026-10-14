/**
 * T20's process. Forked by `t20MeshManagerLossy.run`, so a crash in the
 * libwebrtc binding ends this process and not the mocha run — see there.
 */

import { runLossyMesh } from "./t20-mesh-manager-lossy.js";

const result = await runLossyMesh(process.argv[2] ?? "test");
process.send!(result, () => process.exit(0));
