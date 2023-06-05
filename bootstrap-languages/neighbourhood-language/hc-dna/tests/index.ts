import { CallableCell, runScenario, Scenario } from "@holochain/tryorama";
import test from "tape-promise/tape.js";
import path from "path";
import { resolve } from "path";
import { NeighbourhoodStorage } from "../../neighbourhoodStorage";
import { Blob } from "buffer";
import { NeighbourhoodExpression } from "@perspect3vism/ad4m";

const dnas = [{ source: {path: path.join("../workdir/neighbourhood-laguage.dna") } }];

function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

test("Share full FileExpression between Alice and Bob", async (t) => {
    await runScenario(async (scenario: Scenario) => {
        const alice = await scenario.addPlayerWithApp({
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "neighbourhood-laguage",
                    roles: [{
                        name: "main",
                        dna: {
                            //@ts-ignore
                            path: resolve(dnas[0].source.path)
                        }
                    }]
                },
                resources: {}
            }
        });

        const bob = await scenario.addPlayerWithApp({
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "neighbourhood-laguage",
                    roles: [{
                        name: "main",
                        dna: {
                            //@ts-ignore
                            path: resolve(dnas[0].source.path)
                        }
                    }]
                },
                resources: {}
            }
        });

        console.log("Sharing all agents")
        await scenario.shareAllAgents();


        const aliceClient = new NeighbourhoodStorage((fn_name, payload) => alice.cells[0].callZome({zome_name: "neighbourhood_storage", fn_name, payload}));
        const bobClient = new NeighbourhoodStorage((fn_name, payload) => bob.cells[0].callZome({zome_name: "neighbourhood_storage", fn_name, payload}));


        let randomNumbers:number[] = [];
        for(let i = 0; i < 1000000; i++) {
            randomNumbers.push(Math.floor(Math.random() * 1000));
        }
        const buf = Buffer.from(randomNumbers);
        const blobUp = new Blob([buf])

        const neighbourhoodExpression: NeighbourhoodExpression = {
            linkLangugae: "test",
            meta: {
                links: []
            }
        }

        const neighbourhoodExpressionAddr = await aliceClient.storeNeighbourhoodExpression(neighbourhoodExpression)

        await new Promise(resolve => setTimeout(resolve, 3000))

        const neighbourdhoodExpressionDown = await bobClient.getNeighbourhoodExpression(neighbourhoodExpressionAddr)
        t.deepEqual(neighbourdhoodExpressionDown, neighbourhoodExpression)

        await scenario.shutDown();
      })
  })
