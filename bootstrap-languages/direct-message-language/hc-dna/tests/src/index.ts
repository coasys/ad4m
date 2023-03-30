import { Scenario, runScenario, Dna } from '@holochain/tryorama'
import path from 'path'
import test from "tape-promise/tape";
import { resolve } from "path";

const dnas: Dna[] = [{ source: {path: path.join("../workdir/direct-message-language.dna") } }];

//@ts-ignore
export const sleep = ms => new Promise(r => setTimeout(r, ms))

const ZOME = "direct-message"

//@ts-ignore
test("send direct message", async (t) => {
  await runScenario(async (scenario: Scenario) => {
    let alice_last_signal
    const [alice, bob] = await scenario.addPlayersWithApps([
      {
        appBundleSource:{
          bundle: {
              manifest: {
                  manifest_version: "1",
                  name: ZOME,
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
      },
      {
        appBundleSource:{
          bundle: {
              manifest: {
                  manifest_version: "1",
                  name: ZOME,
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
    }]);
    alice.conductor.appWs().on("signal", (signal) => {
      let payload = signal.payload
      try {
        let cropped = signal.payload.toString().substring(7)
        console.log("CROPPED:", cropped)
        let parsed = JSON.parse(cropped)
        console.log("PARSED:", parsed)
        payload = parsed
      } catch(e) {
        //console.error(e)
      }
      console.log("SIGNAL @ALICE:", payload)
      alice_last_signal = payload
    });

    await scenario.shareAllAgents();
    await sleep(1000);

    const alice_agent_pubkey = alice.agentPubKey
    await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "set_test_recipient", 
      payload: alice_agent_pubkey
    });
    await bob.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "set_test_recipient", 
      payload: alice_agent_pubkey
    });
    const stored_recipient = await bob.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "get_test_recipient"
    });
    //@ts-ignore
    t.equal(stored_recipient.toString(), alice_agent_pubkey.toString())

    // ----------------------------------------------
    // ------------- Setup done ---------------------
    // ----------------------------------------------

    // ------------
    // Status:
    // ------------

    const empty_status = await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "get_status"
    });
    console.log("EMPTY status:", empty_status)

    const status = {
      author: "did:test:test",
      timestamp: new Date().toISOString(),
      data: {
        links: [],
      },
      proof: {
        signature: "asdfasdfasdf",
        key: "did:test:test#primary"
      }
    }

    const link = {
      author: "did:test:test",
      timestamp: new Date().toISOString(),
      data: {
        source: "did:test:test",
        target: "literal://string:online",
        predicate: null,
      },
      proof: {
        signature: "asdfasdfasdf",
        key: "did:test:test#primary"
      }
    }
    //@ts-ignore
    status.data.links.push(link)


    await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "set_status", 
      payload: status
    })
    //@ts-ignore
    t.deepEqual(await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "get_status"
    }), status)
    //@ts-ignore
    t.deepEqual(await bob.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "get_status"
    }), status)

    // ------------
    // P2P Message:
    // ------------

    const message1 = {
      author: "did:test:test",
      timestamp: new Date().toISOString(),
      data: {
        links: [],
      },
      proof: {
        signature: "asdfasdfasdf",
        key: "did:test:test#primary"
      }
    }

    await bob.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "send_p2p", 
      payload: message1
    });
    await sleep(1000)

    t.deepEqual(alice_last_signal, message1)

    let inbox = await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "inbox"
    })
    //@ts-ignore
    t.equal(inbox.length, 1)
    //@ts-ignore
    t.deepEqual(inbox[0], message1)

    // --------------
    // Inbox Message:
    // --------------

    const message2 = JSON.parse(JSON.stringify(message1))
    message2.data.links.push(link)

    console.log("send_inbox:", await bob.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "send_inbox", 
      payload: message2
    }))

    await sleep(1000)

    inbox = await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "inbox"
    })
    //@ts-ignore
    t.equal(inbox.length, 1)
    //@ts-ignore
    t.deepEqual(inbox[0], message1)

    let bobFetchError
    try {
      await bob.cells[0].callZome({
        zome_name: ZOME, 
        fn_name: "fetch_inbox"
      })
    } catch(e) {
      bobFetchError = e
    }
    //@ts-ignore
    t.equal(bobFetchError.data.data, 'Wasm runtime error while working with Ribosome: RuntimeError: WasmError { file: "zomes/direct-message/src/lib.rs", line: 237, error: Guest("Only recipient can fetch the inbox") }')

    console.log("fetch_inbox Alice:", await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "fetch_inbox"
    }))

    // --------------
    // Inbox filter:
    // --------------

    inbox = await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "inbox", 
      payload: "did:test:test"
    })
    //@ts-ignore
    t.equal(inbox.length, 2)

    inbox = await alice.cells[0].callZome({
      zome_name: ZOME, 
      fn_name: "inbox", 
      payload: "did:test:other"
    })
    //@ts-ignore
    t.equal(inbox.length, 0)

    await scenario.cleanUp();
  })
});