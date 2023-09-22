import { addAllAgentsToAllConductors, cleanAllConductors } from "@holochain/tryorama";
import { call, sleep, generate_link_expression, createConductors, create_link_expression} from "./utils";
import test from "tape-promise/tape.js";

// //@ts-ignore
// export async function unSyncFetch(t) {
//     let installs = await createConductors(2);
//     let aliceHapps = installs[0].agent_happ;
//     let conductor1 = installs[0].conductor;
//     let bobHapps = installs[1].agent_happ;
//     let conductor2 = installs[1].conductor;
//     await addAllAgentsToAllConductors([conductor1, conductor2]);

//     //Create did/pub key link for alice and bob
//     await aliceHapps.cells[0].callZome({
//         zome_name: "perspective_diff_sync",
//         fn_name: "create_did_pub_key_link",
//         payload: "did:test:alice"
//     });
//     await bobHapps.cells[0].callZome({
//         zome_name: "perspective_diff_sync",
//         fn_name: "create_did_pub_key_link",
//         payload: "did:test:bob"
//     });
    
//     let commit = await aliceHapps.cells[0].callZome({
//         zome_name: "perspective_diff_sync", 
//         fn_name: "commit", 
//         payload: {additions: [generate_link_expression("alice")], removals: []}
//     });
//     console.warn("\ncommit", commit);

//     // need to for gossip to have commit be seen by bob
//     await sleep(5000)
    
//     const pull_bob = await bobHapps.cells[0].callZome({
//         zome_name: "perspective_diff_sync", 
//         fn_name: "pull",
//         payload: { hash: commit, is_scribe: false } 
//     });
//     //@ts-ignore
//     t.equal(pull_bob.diff.additions.length, 1);
    
//     await conductor1.shutDown();
//     await conductor2.shutDown();
//     await cleanAllConductors();
// };

//@ts-ignore
export async function mergeFetchDeep(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let aliceConductor = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let bobConductor = installs[1].conductor;
    
    //Create new commit whilst bob is not connected
    let create = await create_link_expression(aliceHapps.cells[0], "alice");
    let create2 = await create_link_expression(aliceHapps.cells[0], "alice");
    let create3 = await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");
    
    //Pull from bob and make sure he does not have the latest state
    let pull_bob = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull"
    });
    //@ts-ignore
    t.isEqual(pull_bob.diff.additions.length, 0);
    
    //Bob to commit his data, and update the latest revision, causing a fork
    let bob_create = await create_link_expression(bobHapps.cells[0], "bob");
    let bob_create2 = await create_link_expression(bobHapps.cells[0], "bob");
    let bob_create3 = await create_link_expression(bobHapps.cells[0], "bob");
    let bob_create4 = await create_link_expression(bobHapps.cells[0], "bob");
    await create_link_expression(bobHapps.cells[0], "bob");
    await create_link_expression(bobHapps.cells[0], "bob");
    await create_link_expression(bobHapps.cells[0], "bob");

    let pull_bob2 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull"
    });
    //@ts-ignore
    t.isEqual(pull_bob2.diff.additions.length, 0);
    
    //Connect nodes togther
    await addAllAgentsToAllConductors([aliceConductor, bobConductor]);
    //note; running this test on some machines may require more than 200ms wait
    await sleep(500)
    
    //Alice tries to merge
    let merge_alice = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull"
    });
    //@ts-ignore
    t.isEqual(merge_alice.diff.additions.length, 7);
    //@ts-ignore
    t.isEqual(JSON.stringify(merge_alice.additions[0]), JSON.stringify(bob_create.data));
    
    //note; running this test on some machines may require more than 200ms wait
    await sleep(2000)
    
    let pull_bob3 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull"
    });
    console.warn("bob pull3", pull_bob3);
    //@ts-ignore
    t.isEqual(pull_bob3.diff.additions.length, 7);
    //@ts-ignore
    console.log(pull_bob3.diff.additions[0].data);
    //@ts-ignore
    t.isEqual(JSON.stringify(pull_bob3.diff.additions[0]), JSON.stringify(create.data));
    //@ts-ignore
    t.isEqual(JSON.stringify(pull_bob3.diff.additions[1]), JSON.stringify(create2.data));

    //Shutdown alice conductor
    await aliceConductor.shutDown();

    //Have bob write three links
    await create_link_expression(bobHapps.cells[0], "bob");
    await create_link_expression(bobHapps.cells[0], "bob");
    await create_link_expression(bobHapps.cells[0], "bob");

    //shutdown bobs conductor
    await bobConductor.shutDown();

    //Have alice write three links
    await aliceConductor.startUp();
    await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");
    await create_link_expression(aliceHapps.cells[0], "alice");

    //start bobs conductor and pull to see if merge happens correctly
    await bobConductor.startUp();
    let pull_bob4 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull"
    });
    console.warn("bob pull4", pull_bob4);
    //@ts-ignore
    t.isEqual(pull_bob4.diff.additions.length, 3);
    //@ts-ignore
    console.log(pull_bob4.diff.additions[0].data);
    //@ts-ignore
    t.isEqual(JSON.stringify(pull_bob4.additions[0]), JSON.stringify(create.data));
    //@ts-ignore
    t.isEqual(JSON.stringify(pull_bob4.additions[1]), JSON.stringify(create2.data));

    await cleanAllConductors();
}

//@ts-ignore 
export async function mergeFetch(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let aliceConductor = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let bobConductor = installs[1].conductor;

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
    
    //Create new commit whilst bob is not connected
    let link_data = generate_link_expression("alice");
    console.log("Alice posting link data", link_data);
    let commit = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [link_data], removals: []}
    });
    //@ts-ignore
    console.warn("\ncommit", commit.toString("base64"));

    await sleep(1000)

    let bob_pull_failed = false
    try{
        // Pull from bob who is not connected to alice yet
        // to show that this will fail (because `commit` could not be found)
        await bobHapps.cells[0].callZome({
            zome_name: "perspective_diff_sync", 
            fn_name: "pull",
            payload: { hash: commit, is_scribe: false }
        });
    } catch(e) {
        bob_pull_failed = true
    }

    t.assert(bob_pull_failed)

    
    //Bob to commit his data, and update the latest revision, causing a fork
    let bob_link_data = generate_link_expression("bob");
    console.log("Bob posting link data", bob_link_data);
    let commit_bob = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [bob_link_data], removals: []}
    });
    //@ts-ignore
    console.warn("\ncommit_bob", commit_bob.toString("base64"));
    
    //Connect nodes togther
    await addAllAgentsToAllConductors([aliceConductor, bobConductor]);
    //note; running this test on some machines may require more than 200ms wait
    await sleep(1000)
    
    //Alice tries to merge
    let merge_alice = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull",
        payload: { hash: commit_bob, is_scribe: true }
    });
    //@ts-ignore
    t.isEqual(merge_alice.diff.additions.length, 1);
    //@ts-ignore
    t.isEqual(JSON.stringify(merge_alice.diff.additions[0].data), JSON.stringify(bob_link_data.data));

    const alice_merge_commit = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "current_revision",
    });
    
    //note; running this test on some machines may require more than 200ms wait
    await sleep(2000)
    
    let pull_bob3 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "pull",
        payload: { hash: alice_merge_commit, is_scribe: false }
    });
    console.warn("bob pull3", pull_bob3);
    //@ts-ignore
    t.isEqual(pull_bob3.diff.additions.length, 1);
    //@ts-ignore
    console.log(pull_bob3.diff.additions[0].data);
    //@ts-ignore
    t.isEqual(JSON.stringify(pull_bob3.diff.additions[0].data), JSON.stringify(link_data.data));

    await aliceConductor.shutDown();
    await bobConductor.shutDown();
}


test("pull", async (t) => {
    //t.plan(20)
    try {
        //await unSyncFetch(t);
        await mergeFetch(t);
    } catch(e) {
        console.error("Pull test failed with error", e);
        //@ts-ignore
        t.fail(e)
    } finally {
        t.end()
        process.exit(0)
    }
})
