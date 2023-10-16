import { Scenario } from "@holochain/tryorama";
import { sleep, generate_link_expression } from "./utils";
import { dnas } from "./common";
import test from "tape-promise/tape.js";
import { resolve } from "path";

//@ts-ignore
export async function signals(t) {
    const scenario = new Scenario();
    let aliceSignalCount = 0;
    let bobSignalCount = 0;
    
    const aliceHapps = await scenario.addPlayerWithApp(
        {
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "perspective-diff-sync",
                    roles: [{
                        name: "main",
                        dna: {
                            //@ts-ignore
                            path: resolve(dnas[0].source.path)
                        }
                    }]
                },
                resources: {}
            },
        }
    );
    const portAlice = await aliceHapps.conductor.attachAppInterface();
    const appWs = await aliceHapps.conductor.connectAppWs(portAlice);
    appWs.on("signal", (signal) => {
        console.log("Alice Received Signal:",signal)
        aliceSignalCount += 1;
    });
    const bobHapps = await scenario.addPlayerWithApp(
        {
            bundle: {
                manifest: {
                    manifest_version: "1",
                    name: "perspective-diff-sync",
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
        }
    );
    const portBob = await bobHapps.conductor.attachAppInterface();
    const appWsBob = await bobHapps.conductor.connectAppWs(portBob);
    appWsBob.on("signal", (signal) => {
        console.log("Bob Received Signal:",signal)
        bobSignalCount += 1;
    })

    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: "did:test:alice"
    });
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: "did:test:bob"
    });

    
    await scenario.shareAllAgents();

    //Sleep to give time for bob active agent link to arrive at alice
    await sleep(2000)

    //Test case where subject object and predicate are given
    let bob_link_data = generate_link_expression("bob");
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [bob_link_data], removals: []}
    });
    
    //Test case where subject object and predicate are given
    let link_data = generate_link_expression("alice");
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [link_data], removals: []}
    });
    //Sleep to give time for signals to arrive
    await sleep(1000)
    
    t.deepEqual(bobSignalCount, 1);

    await scenario.cleanUp();
}

test("signals", async (t) => {
    await signals(t)
    t.end()
    process.exit(0)
})