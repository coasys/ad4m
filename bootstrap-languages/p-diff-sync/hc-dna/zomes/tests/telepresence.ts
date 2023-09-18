import { addAllAgentsToAllConductors, Scenario } from "@holochain/tryorama";
import { sleep, generate_link_expression, sortedObject } from "./utils";
import test from "tape-promise/tape.js";
import { resolve } from "path";
import { dnas } from "./common";
import { PerspectiveExpression } from "@perspect3vism/ad4m";

function generate_perspective_expression(author: string, linkContent: string): PerspectiveExpression {
    return {
        author: author,
        timestamp: new Date().toISOString(), 
        data: {
            links: [generate_link_expression(linkContent)]
        },
        proof: {signature: "sig", key: "key"},
    };
}
//@ts-ignore
export async function testTelepresence(t) {
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

    await scenario.shareAllAgents();
    await addAllAgentsToAllConductors([aliceHapps.conductor, bobHapps.conductor]);

    const bobDid = "did:key:bob";
    const aliceDid = "did:key:alice";

    //Create did/pub key link for alice and bob
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: aliceDid
    });
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: bobDid
    });
    console.log("Set did pub key links");

    //Sleep to give time for gossip
    await sleep(2000)

    //Test setting and getting agent status
    let perspectiveExpression = generate_perspective_expression(aliceDid, "alice");
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "set_online_status",
        payload: perspectiveExpression
    });
    console.log("Set online status");

    let bobSeenStatus = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "get_online_agents",
    });
    //@ts-ignore
    t.isEqual(bobSeenStatus.length, 1);
    //@ts-ignore
    t.equal(JSON.stringify(sortedObject(bobSeenStatus[0].status)), JSON.stringify(sortedObject(perspectiveExpression)));
    //@ts-ignore
    t.equal(JSON.stringify(sortedObject(bobSeenStatus[0].status.data.links[0])), JSON.stringify(sortedObject(perspectiveExpression.data.links[0])));

    //Test that if alice updates her online status that bob sees the update, and does not get duplicates
    perspectiveExpression = generate_perspective_expression(aliceDid, "alice2");

    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "set_online_status",
        payload: perspectiveExpression
    });
    bobSeenStatus = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "get_online_agents",
    });
    //@ts-ignore
    t.isEqual(bobSeenStatus.length, 1);
    //@ts-ignore
    t.equal(JSON.stringify(sortedObject(bobSeenStatus[0].status)), JSON.stringify(sortedObject(perspectiveExpression)));
    //@ts-ignore
    t.equal(JSON.stringify(sortedObject(bobSeenStatus[0].status.data.links[0])), JSON.stringify(sortedObject(perspectiveExpression.data.links[0])));

    //Test sending signal to single agent
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "send_signal",
        payload: {remote_agent_did: "did:key:bob", payload: perspectiveExpression}
    });
    //Sleep to give time for signals to arrive
    await sleep(1000)
    //@ts-ignore
    t.isEqual(bobSignalCount, 1);


    //Test sending broadcast
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "send_broadcast",
        payload: perspectiveExpression
    });
    //Sleep to give time for signals to arrive
    await sleep(1000)
    //@ts-ignore
    t.isEqual(aliceSignalCount, 1);

    await scenario.cleanUp();
};

test("telepresence", async (t) => {
    //t.plan(20)
    try {
        await testTelepresence(t);
    } catch(e) {
        console.error("telepresence test failed with error", e);
        //@ts-ignore
        t.fail(e)
    } finally {
        t.end()
        process.exit(0)
    }
})
