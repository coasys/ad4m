/**
 * T21's media process. Forked by `t21ColocatedCall.run` with the call config
 * as its argument, so a crash in the libwebrtc binding ends this process and
 * not the mocha run — see T20.
 */

import { runColocatedCall, type T21Config } from "./t21-colocated-call.js";

const config = JSON.parse(process.argv[2]) as T21Config;
try {
  const result = await runColocatedCall(config);
  process.send!(result, () => process.exit(0));
} catch (error) {
  console.error("[t21] call process failed:", error);
  process.exit(1);
}
