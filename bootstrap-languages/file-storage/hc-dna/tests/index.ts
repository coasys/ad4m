import { CallableCell, runScenario, Scenario } from "@holochain/tryorama";
import test from "tape-promise/tape.js";
import path from "path";
import { resolve } from "path";
import { FileStorage } from "../../file-storage";
import { Blob } from "buffer";

const dnas = [{ source: {path: path.join("../workdir/file-storage.dna") } }];

test("Share 1MB between Alice and Bob", async (t) => {
  await runScenario(async (scenario: Scenario) => {
        const alice = await scenario.addPlayerWithApp({
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "file-storage",
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
                    name: "file-storage",
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
        

        const aliceClient = new FileStorage((fn_name, payload) => alice.cells[0].callZome({zome_name: "file_storage", fn_name, payload}));
        const bobClient = new FileStorage((fn_name, payload) => bob.cells[0].callZome({zome_name: "file_storage", fn_name, payload}));


        let randomNumbers:number[] = [];
        for(let i = 0; i < 1000000; i++) {
            randomNumbers.push(Math.floor(Math.random() * 1000));
        }
        const buf = Buffer.from(randomNumbers);
        const blobUp = new Blob([buf])

        console.log("Uploading file...")
        const hashes = await aliceClient.upload(blobUp);
        console.log("Done")
        console.log("Got hashes", hashes)
        console.log("Waiting 1 second...")
        await new Promise(resolve => setTimeout(resolve, 1000))
        console.log("Done")
        console.log("Downloading file...")
        const blobDown = await bobClient.download(hashes);
        console.log("Done")
        t.equal(blobDown.size, buf.length);

        // compare the contents of the blobs
        // @ts-ignore
        const bufDown = Buffer.from(await blobDown.arrayBuffer());
        const bufUp = Buffer.from(await blobUp.arrayBuffer());
        t.assert(bufUp.compare(bufDown) == 0)

        await scenario.shutDown()
    })
})


test("Share full FileExpression between Alice and Bob", async (t) => {
    await runScenario(async (scenario: Scenario) => {
        const alice = await scenario.addPlayerWithApp({
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "file-storage",
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
                    name: "file-storage",
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
        

        const aliceClient = new FileStorage((fn_name, payload) => alice.cells[0].callZome({zome_name: "file_storage", fn_name, payload}));
        const bobClient = new FileStorage((fn_name, payload) => bob.cells[0].callZome({zome_name: "file_storage", fn_name, payload}));


        let randomNumbers:number[] = [];
        for(let i = 0; i < 1000000; i++) {
            randomNumbers.push(Math.floor(Math.random() * 1000));
        }
        const buf = Buffer.from(randomNumbers);
        const blobUp = new Blob([buf])

        const hashes = await aliceClient.upload(blobUp);
        const fileMetadata = {
            name: "test.txt",
            size: buf.length,
            file_type: "text/plain",
            checksum: "1234",
            chunks_hashes: hashes
        }

        const fileExpression = {
            author: "did:test:alice",
            timestamp: new Date().toISOString(),
            data: fileMetadata,
            proof: {
                signature: "1234",
                key: "1234"
            }
        }

        const fileExpressionAddr = await aliceClient.storeFileExpression(fileExpression)


        await new Promise(resolve => setTimeout(resolve, 1000))

        const fileExpressionDown = await bobClient.getFileExpression(fileExpressionAddr)
        t.deepEqual(fileExpressionDown, fileExpression)

        const blobDown = await bobClient.download(fileExpression.data.chunks_hashes);
        t.equal(blobDown.size, buf.length);

        // compare the contents of the blobs
        // @ts-ignore
        const bufDown = Buffer.from(await blobDown.arrayBuffer());
        const bufUp = Buffer.from(await blobUp.arrayBuffer());
        t.assert(bufUp.compare(bufDown) == 0)

        await scenario.shutDown();
  
      })
  })
  