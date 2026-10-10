import { sfuScenario } from "../helpers/sfu/harness.js";
import { m3CascadeFailover } from "../helpers/sfu/scenarios/index.js";

describe("SFU: Mid-call transitions", function () {
  this.timeout(600_000);

  sfuScenario(m3CascadeFailover, 300_000);
});
