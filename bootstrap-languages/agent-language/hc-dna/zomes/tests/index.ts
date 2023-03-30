import { Scenario, runScenario, addAllAgentsToAllConductors, cleanAllConductors } from '@holochain/tryorama'
import path from 'path'
import test from "tape-promise/tape";
import { createConductors } from "./utils";

//@ts-ignore
test("Create update agent expression", async (t) => {
  await runScenario(async (scenario: Scenario) => {
    const [aliceConductor, bobConductor] = await createConductors(2);
    await addAllAgentsToAllConductors([aliceConductor.conductor, bobConductor.conductor]);
    const alice = aliceConductor.agent_happ;
    const bob = bobConductor.agent_happ;

    await scenario.shareAllAgents();
     
    const call1 = await alice.cells[0].callZome({
      zome_name: "agent_store", 
      fn_name: "create_agent_expression",  
      payload: {
        author: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
        timestamp: new Date().toISOString(),
        data: {
          did: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
          perspective: {
            links: [
              {
                author: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
                timestamp: new Date().toISOString(),    
                data: {
                  source: "language://src",
                  target: "language://target",
                  predicate: "language://pred"
                },
                proof: {
                  signature: "sig",
                  key: "key",
                  valid: true,
                  invalid: false,
                },
              }
            ]
          },
          directMessageLanguage: "language://hashyHash"
        },
        proof: {
          signature: "sig",
          key: "key",
          valid: true,
          invalid: false,
        },
      }
    });
    
    let getResp = await alice.cells[0].callZome({
      zome_name: "agent_store", 
      fn_name: "get_agent_expression", 
      payload: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC"
    });
    t.ok(getResp);
    //@ts-ignore
    t.equal(getResp.data.directMessageLanguage, "language://hashyHash");

    await alice.cells[0].callZome({
      zome_name: "agent_store", 
      fn_name: "create_agent_expression",  
      payload:{
        author: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
        timestamp: new Date().toISOString(),
        data: {
          did: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
          perspective: {
            links: [
              {
                author: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC",
                timestamp: new Date().toISOString(),    
                data: {
                  source: "language://src",
                  target: "language://target",
                  predicate: "language://pred"
                },
                proof: {
                  signature: "sig",
                  key: "key",
                  valid: true,
                  invalid: false,
                },
              }
            ]
          },
          directMessageLanguage: "language://hashyHash2"
        },
        proof: {
          signature: "sig",
          key: "key",
          valid: true,
          invalid: false,
        },
      }
    })

    let getResp2 = await alice.cells[0].callZome({
      zome_name: "agent_store", 
      fn_name: "get_agent_expression", 
      payload: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC"
    });
    t.ok(getResp2);
    //@ts-ignore
    t.deepEqual(getResp2.data.directMessageLanguage, "language://hashyHash2");


    //====================
    await new Promise(r => setTimeout(r, 1000))
    //====================

    let bobResult = await bob.cells[0].callZome({
      zome_name: "agent_store", 
      fn_name: "get_agent_expression", 
      payload: "did:key:zQ3shc5AcaZyRo6qP3wuXvYT8xtiyFFL25RjMEuT81WMHEibC"
    });
    t.ok(bobResult);
    //@ts-ignore
    t.deepEqual(bobResult.data.directMessageLanguage, "language://hashyHash2");

    await aliceConductor.conductor.shutDown();
    await bobConductor.conductor.shutDown();
    await cleanAllConductors();
    await scenario.cleanUp()
  })
})