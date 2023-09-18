import { addAllAgentsToAllConductors, cleanAllConductors } from "@holochain/tryorama";
import { call, sleep, generate_link_expression, createConductors} from "./utils";
import test from "tape-promise/tape.js";

//NOTE; these tests are dependant on the SNAPSHOT_INTERVAL in lib.rs being set to 2
//@ts-ignore
export async function render(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let conductor1 = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let conductor2 = installs[1].conductor;
    await addAllAgentsToAllConductors([conductor1, conductor2]);

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
    
    console.log("RENDER 1")
    let commit = await call(aliceHapps, "commit", {
        additions: [generate_link_expression("alice1")], 
        removals: []
    });
    console.warn("\ncommit", commit);
    
    await call(aliceHapps, "update_current_revision", commit);

    let commit2 = await call(aliceHapps, "commit", {
        additions: [generate_link_expression("alice2")], 
        removals: []
    });
    console.warn("\ncommit", commit2);
    
    console.log("RENDER 2")

    await call(aliceHapps, "update_current_revision", commit2);

    let alice_rendered = await call(aliceHapps, "render");
    //@ts-ignore
    t.equal(alice_rendered.links.length, 2)

    await sleep(5000);
    
    console.log("RENDER 3")

    // Bob hasn't pulled yet, so render on Bob should fail
    let firstRenderFailed = false
    try {
        let bob_render = await call(bobHapps, "render");
    } catch(e) {
        firstRenderFailed = true
    }

    t.assert(firstRenderFailed)

    await call(bobHapps, "pull", { hash: commit2, is_scribe: false })

    console.log("Bob has pulled")

    let bob_render = await call(bobHapps, "render");

    
    console.log("RENDER 4")
    console.warn("bob rendered with", bob_render);
    //@ts-ignore
    t.deepEqual(bob_render.links.length, 2);

    await call(bobHapps, "update_current_revision", commit2);

    let commit4 = await call(bobHapps, "commit", {
        additions: [generate_link_expression("bob3")], 
        removals: []
    });
    console.warn("\ncommit", commit4);
    
    await call(bobHapps, "update_current_revision", commit4);


    let commit5 = await call(bobHapps, "commit", {
        additions: [generate_link_expression("bob4")], 
        removals: []
    });
    console.warn("\ncommit", commit5);
    
    await call(bobHapps, "update_current_revision", commit5);

    await sleep(1000);

    console.log("RENDER 5")
    await call(aliceHapps, "pull", { hash: commit5, is_scribe: true }); 
    let alice_render = await call(aliceHapps, "render");
    console.warn("Alice rendered with", alice_render);
    //@ts-ignore
    t.deepEqual(alice_render.links.length, 4);

    await conductor1.shutDown();
    await conductor2.shutDown();
    await cleanAllConductors();
};

//@ts-ignore
export async function renderMerges(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let conductor1 = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let conductor2 = installs[1].conductor;

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
    
    console.log("commit1");
    let commit = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("alice1")], removals: []}
    });
    console.warn("\ncommit", commit);
    
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit
    });

    console.log("commit2");
    let commit2 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("alice2")], removals: []}
    });
    console.warn("\ncommit", commit2);

    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit2
    });

    console.log("commit3");
    let commit3 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("bob1")], removals: []}
    });
    console.warn("\ncommit", commit3);
    
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit3
    });

    console.log("commit4");
    let commit4 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("bob2")], removals: []}
    });
    console.warn("\ncommit", commit4);
    
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit4
    });

    console.log("bob render");
    let bob_render = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "render"
    });
    console.warn("bob rendered with", bob_render);
    //@ts-ignore
    t.isEqual(bob_render.links.length, 2);

    console.log("alice render");
    let alice_render = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "render"
    });
    console.warn("Alice rendered with", alice_render);
    //@ts-ignore
    t.isEqual(alice_render.links.length, 2);
    
    await addAllAgentsToAllConductors([conductor1, conductor2]);
    await sleep(500);

    //Test getting revision, should return bob's revision since that is the latest entry

    //Alice commit which will create a merge and another entry
    console.log("commit5");
    let commit5 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("alice3")], removals: []}
    });
    console.warn("\ncommit5", commit5);
    
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit5
    });

    //Alice commit which should not create another snapshot
    console.log("commit6");
    let commit6 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [generate_link_expression("alice4")], removals: []}
    });
    console.warn("\ncommit6", commit6);
    
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "update_current_revision", 
        payload: commit6
    });
    await sleep(2000)

    console.log("bob pull");
    await call(bobHapps, "pull", { hash: commit6, is_scribe: true })
    
    console.log("bob render");
    let bob_render2 = await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "render"
    });
    console.warn("bob rendered with", bob_render2);
    //@ts-ignore
    t.isEqual(bob_render2.links.length, 6);

    console.log("alice render");
    let alice_render2 = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "render"
    });
    console.warn("Alice rendered with", alice_render2);
    //@ts-ignore
    t.isEqual(alice_render2.links.length, 4);

    await conductor1.shutDown();
    await conductor2.shutDown();
    await cleanAllConductors();
}

test("render", async (t) => {
    await render(t)
    await renderMerges(t)
    t.end()
    process.exit(0);
})