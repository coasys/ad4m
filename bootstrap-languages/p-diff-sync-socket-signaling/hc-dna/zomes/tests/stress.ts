import { AgentApp, addAllAgentsToAllConductors, cleanAllConductors, Conductor, Scenario } from "@holochain/tryorama";
import { call, sleep, createConductors, create_link_expression, generate_link_expression} from "./utils";
import ad4m, { DID, HolochainLanguageDelegate, LinkExpression, Perspective } from "@perspect3vism/ad4m"
import test from "tape-promise/tape.js";
import { hrtime } from 'node:process';
//@ts-ignore
import divide from 'divide-bigint'
import { AsyncQueue } from "./queue";
import { resolve } from "path";
import { dnas } from "./common";
let createdLinks = new Map<string, Array<LinkExpression>>()

class PeerInfo {
    currentRevision: Buffer | null = null;
    lastSeen: Date = new Date();
};

let aliceRevision: Buffer | null = null;
let bobRevision: Buffer | null = null;
let aliceDid = "";
let bobDid = "";

async function createLinks(happ: AgentApp, agentName: string, count: number, queue?: AsyncQueue) {
    if(!createdLinks.get(agentName)) createdLinks.set(agentName, [])
    for(let i=0; i < count; i++) {
        if (queue) {
            await queue.add(async () => {
                let { commitRaw, data } = await create_link_expression(happ.cells[0], agentName);
                if (agentName === "alice") {
                    aliceRevision = commitRaw;
                } else if (agentName === "bob") {
                    bobRevision = commitRaw;
                }
                createdLinks.get(agentName)!.push(data)
            }).catch((e) => {
                console.log("Error in create links queue", e);
            })
        } else {
            let { data } = await create_link_expression(happ.cells[0], agentName);
            createdLinks.get(agentName)!.push(data)
        }
    }
}

//@ts-ignore
export async function latestRevisionStress(t) {
    let installs = await createConductors(2);
    let aliceHapps = installs[0].agent_happ;
    let aliceConductor = installs[0].conductor;
    let bobHapps = installs[1].agent_happ;
    let bobConductor = installs[1].conductor;

    await addAllAgentsToAllConductors([aliceConductor, bobConductor]);

    let link_data = generate_link_expression("alice");
    let commit = await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [link_data], removals: []}
    });

    for (let i = 0; i < 1000; i++) {
        console.log("Latest update revision", i);
        let now = performance.now();
        let create = await aliceHapps.cells[0].callZome({zome_name: "perspective_diff_sync", fn_name: "update_latest_revision", payload: commit});
        let after = performance.now();
        console.log(" Create execution took: ", after - now);
        let fetch = await aliceHapps.cells[0].callZome({zome_name: "perspective_diff_sync", fn_name: "latest_revision"});
        let after2 = performance.now();
        console.log("Fetch execution took: ", after2 - after);
    }
}
  
async function gossip(peers: Map<DID, PeerInfo>, me: DID, hcDna: HolochainLanguageDelegate) {
    console.log("GOSSIP for ", me)
    //@ts-ignore
    await hcDna.call("DNA_NICK", "ZOME_NAME", "sync", null);
    let lostPeers: DID[] = [];
  
    peers.forEach( (peerInfo, peer) => {
      if (peerInfo.lastSeen.getTime() + 10000 < new Date().getTime()) {
        lostPeers.push(peer);
      }
    });
  
    for (const peer of lostPeers) {
      peers.delete(peer);
    }
  
    // flatten the map into an array of peers
    let allPeers = Array.from(peers.keys())
    allPeers.push(me);
    // Lexically sort the peers
    allPeers.sort();
  
    // If we are the first peer, we are the scribe
    let is_scribe = (allPeers[0] == me);

    console.log("IS SCRIBE", is_scribe, me)
    
    // Get a deduped set of all peer's current revisions
    let revisions = new Set<Buffer>();
    for(const peerInfo of peers.values()) {
        if (peerInfo.currentRevision) revisions.add(peerInfo.currentRevision);
    }

    console.log(`
    ======
    GOSSIP
    --
    me: ${me}
    is scribe: ${is_scribe}
    --
    ${Array.from(peers.entries()).map( ([peer, peerInfo]) => {
      //@ts-ignore
      return `${peer}: ${peerInfo.currentRevision.toString('base64')} ${peerInfo.lastSeen.toISOString()}\n`
    })}
    --
    revisions: ${Array.from(revisions).map( (hash) => {
        //@ts-ignore
        return hash.toString('base64')
      })}
    `);
  
    revisions.forEach( async (hash) => {
        console.log("PULLING", hash, is_scribe, aliceRevision, bobRevision) 
        if(!hash) return
        if (me === aliceDid) {
            if (hash === aliceRevision) {
                console.log("Alice skipping pull since we already sync'd on this gossip loop");
                return
            }
        }
        if (me === bobDid) {
            if (hash === bobRevision) {
                console.log("Bob skipping pull since we already sync'd on this gossip loop");
                return
            }
        }
        let pullResult = await hcDna.call("DNA_NICK", "ZOME_NAME", "pull", { 
            hash,
            is_scribe 
        });
        if (pullResult) {
            let myRevision = pullResult.current_revision;
            if (me === aliceDid) aliceRevision = myRevision;
            if (me === bobDid) bobRevision = myRevision;
        }
    })
  }

//@ts-ignore
export async function stressTest(t) {
    const aliceQueue = new AsyncQueue();
    const bobQueue = new AsyncQueue();

    const scenario = new Scenario();
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
    const alicePeersList: Map<DID, PeerInfo> = new Map();
    const portAlice = await aliceHapps.conductor.attachAppInterface();
    const appWs = await aliceHapps.conductor.connectAppWs(portAlice);
    appWs.on("signal", (signal) => {
        //console.log("Alice Received Signal:",signal);
        const { diff, reference_hash, reference, broadcast_author } = signal.payload;
        if (diff && reference_hash && reference && broadcast_author) {
            console.log(`PerspectiveDiffSync.handleHolochainSignal: 
            diff: ${JSON.stringify(diff)}
            reference_hash: ${reference_hash.toString('base64')}
            reference: {
                diff: ${reference.diff?.toString('base64')}
                parents: ${reference.parents ? reference.parents.map( (parent: Buffer) => parent ? parent.toString('base64') : 'null').join(', '):'none'}
                diffs_since_snapshot: ${reference?.diffs_since_snapshot}
            }
            broadcast_author: ${broadcast_author}
            `)
            alicePeersList.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
        } else {
            console.log("PerspectiveDiffSync.handleHolochainSignal: got other signal:", signal.payload)
        }
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
    const bobPeersList: Map<DID, PeerInfo> = new Map();
    const portBob = await bobHapps.conductor.attachAppInterface();
    const appWsBob = await bobHapps.conductor.connectAppWs(portBob);
    appWsBob.on("signal", (signal) => {
        console.log("Bob Received Signal:",signal)
        const { diff, reference_hash, reference, broadcast_author } = signal.payload;
        if (diff && reference_hash && reference && broadcast_author) {
            bobPeersList.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
        }
    })

    //Create did/pub key link for alice and bob
    await aliceHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: "did:test:alice"
    });
    aliceDid = "did:test:alice";
    await bobHapps.cells[0].callZome({
        zome_name: "perspective_diff_sync",
        fn_name: "create_did_pub_key_link",
        payload: "did:test:bob"
    });
    bobDid = "did:test:bob";

    let done = false;
    async function processGossip() {
        await Promise.all([gossip(alicePeersList, "did:test:alice", {
            call: async (nick, zome, fn_name, payload) => {
                await aliceQueue.add( async () => {
                    try{
                        await aliceHapps.cells[0].callZome({
                            zome_name: "perspective_diff_sync", 
                            fn_name,
                            payload
                        })
                    } catch(e) {
                        console.log("ERROR during alice zome call", e)
                    }
                    
                })
            }
        } as HolochainLanguageDelegate),
        gossip(bobPeersList, "did:test:bob", {
            call: async (nick, zome, fn_name, payload) => {
                await bobQueue.add( async () => {
                    try {
                        await bobHapps.cells[0].callZome({
                            zome_name: "perspective_diff_sync", 
                            fn_name,
                            payload
                        })
                    } catch(e) {
                        console.log("ERROR during bob zome call", e)
                    }
                    
                })
            }
        } as HolochainLanguageDelegate)])
        await sleep(1000)
        if(!done) {
            processGossip()
        }
        
    }

    processGossip()

    let aliceConductor = aliceHapps.conductor
    let bobConductor = bobHapps.conductor;
    let hash = Buffer.from((await aliceConductor.adminWs().listDnas())[0]);

    await addAllAgentsToAllConductors([aliceConductor, bobConductor]);

    console.log("==============================================")
    console.log("=================START========================")
    console.log("==============================================")
    for(let i=0; i < 10; i++) {
        console.log("-------------------------");
        console.log("Iteration: ", i)
        console.log("-------------------------");
        const start = hrtime.bigint();
        await Promise.all([
            createLinks(aliceHapps, "alice", 20, aliceQueue),
            createLinks(bobHapps, "bob", 20, bobQueue)
        ])
        const end = hrtime.bigint();
        console.log(`Creating links took ${divide(end - start, 1000000)} ms`);

        console.log("-------------------------");
        console.log("Created 20 links each (Alice and Bob)");
        console.log("waiting a second");
        console.log("-------------------------");

        await sleep(1000)

        console.log("-------------------------");
        console.log("All good :)))))))))))))))");
        console.log("-------------------------");

    }

    // Wait for gossip of latest_revision, needed for render
    await sleep(15000)

    done = true;
    aliceQueue.clear();
    bobQueue.clear()

    const startRenderA = hrtime.bigint();
    let alice_rendered = await call(aliceHapps, "render") as Perspective
    const endRenderA = hrtime.bigint();
    console.log(`Alice render took ${divide(endRenderA - startRenderA, 1000000)} ms`);

    const startRenderB = hrtime.bigint();
    let bob_rendered = await call(bobHapps, "render") as Perspective
    const endRenderB = hrtime.bigint();
    console.log(`Bob render took ${divide(endRenderB - startRenderB, 1000000)} ms`);

    t.isEqual(alice_rendered.links.length, bob_rendered.links.length)

    function includes(perspective: Perspective, link: LinkExpression) {
        return perspective.links.find(l => ad4m.linkEqual(l,link))
    }

    for(let link of createdLinks.get("alice")!) {
        t.assert(includes(alice_rendered, link))
        t.assert(includes(bob_rendered, link))
    }

    await aliceConductor.shutDown();
    await bobConductor.shutDown();
    await cleanAllConductors();
}

test("stress", async (t) => {
    await stressTest(t);
    t.end()
    process.exit(0)
})
