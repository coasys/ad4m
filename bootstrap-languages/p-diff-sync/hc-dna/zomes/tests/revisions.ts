import { addAllAgentsToAllConductors, cleanAllConductors } from "@holochain/tryorama";
import { sleep, createConductors, create_link_expression } from "./utils";
import test from "tape-promise/tape.js";

//@ts-ignore
export async function testRevisionUpdates(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let aliceConductor = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let bobConductor = installs[1].conductor;

    await addAllAgentsToAllConductors([aliceConductor, bobConductor]);

    //Create did/pub key link for alice and bob
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

    let current_revision = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "current_revision"
    });
    console.warn("current_revision", current_revision);

    let commit = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [], removals: []}
    });
    console.warn("\ncommit", commit);

    let current_revision2 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "current_revision"
    });
    console.warn("current_revision2", current_revision2);
    //@ts-ignore
    t.isEqual(commit.toString(), current_revision2.toString())

    await sleep(1000)

    //test bobs current revision is not updated
    let bob_current_revision = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "current_revision"
    });
    console.warn("bob_current_revision", bob_current_revision);
    //@ts-ignore
    t.isEqual(null, bob_current_revision);

    let commit2 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [], removals: []}
    });
    //@ts-ignore
    console.warn("\ncommit2", commit2);

    let current_revision3 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "current_revision"
    });
    console.warn("current_revision3", current_revision3);
    //@ts-ignore
    t.isEqual(current_revision3.toString(), commit2.toString());

    let current_revision4 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "current_revision"
    });
    console.warn("current_revision4", current_revision4);
    //@ts-ignore
    t.isEqual(commit2.toString(), current_revision4.toString())

    await aliceConductor.shutDown();
    await bobConductor.shutDown();
    await cleanAllConductors();
}

test("test revision updates", async (t) => {
    await testRevisionUpdates(t);
    t.end()
    process.exit(0)
})