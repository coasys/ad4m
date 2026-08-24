import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import { ChildProcess } from 'child_process';
import { sleep } from "../utils/utils";

export class TestContext {
    #alice: Ad4mClient | undefined
    #bob: Ad4mClient | undefined

    #aliceCore: ChildProcess | undefined
    #bobCore: ChildProcess | undefined

    get ad4mClient(): Ad4mClient {
      return this.#alice!
    }

    get alice(): Ad4mClient {
      return this.#alice!
    }

    get bob(): Ad4mClient {
      return this.#bob!
    }

    set alice(client: Ad4mClient) {
      this.#alice = client
    }

    set bob(client: Ad4mClient) {
      this.#bob = client
    }

    set aliceCore(aliceCore: ChildProcess) {
      this.#aliceCore = aliceCore
    }

    set bobCore(bobCore: ChildProcess) {
      this.#bobCore = bobCore
    }

    async makeAllNodesKnown() {
      for (let attempt = 1; attempt <= 5; attempt++) {
        try {
          const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
          const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();

          await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
          await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
          console.log(`Agent info exchange attempt ${attempt} successful`);
          break;
        } catch (error) {
          console.log(`Agent info exchange attempt ${attempt} failed:`, error);
          if (attempt < 5) {
            await sleep(3000);
          }
        }
      }
    }
}
