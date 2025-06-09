import { AgentApp, CallableCell, Conductor, enableAndGetAgentApp, runLocalServices } from "@holochain/tryorama";
import faker from "faker";
import { dnas, happ_path } from './common.ts';
import { createConductor } from "@holochain/tryorama";
import { resolve } from "node:path";

export async function call(happ: AgentApp, fn_name: string, payload?: any) {
    return await happ.cells[0].callZome({
        zome_name: "perspective_diff_sync", 
        fn_name,
        payload
    }, 60000);
}

export function generate_link_expression(agent: string) {
    return {
      data: {source: faker.name.findName(), target: faker.name.findName(), predicate: faker.name.findName()},
      author: agent, 
      timestamp: new Date().toISOString(), 
      proof: {signature: "sig", key: "key"},
   }
}

export async function create_link_expression(cell: CallableCell, agent: string): Promise<{commit: string, data: any, commitRaw: Buffer}> {
    let link_data = generate_link_expression(agent);
    let commit = await cell.callZome({
        zome_name: "perspective_diff_sync", 
        fn_name: "commit", 
        payload: {additions: [link_data], removals: []}
    }, 60000);
    //@ts-ignore
    return {commit: commit.toString("base64"), data: link_data, commitRaw: commit}
}

export function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export async function createConductors(num: number): Promise<{agent_happ: AgentApp, conductor: Conductor}[]> {
    let out = [] as {agent_happ: AgentApp, conductor: Conductor}[];

    const localServices = await runLocalServices();

    for (let n of Array(num).keys()) {
        let conductor = await createConductor(localServices.signalingServerUrl, {bootstrapServerUrl: localServices.bootstrapServerUrl});
        let port = await conductor.attachAppInterface();
        let adminWs = conductor.adminWs();
        try {
            let app = await conductor.installApp({
                appBundleSource: {
                    type: "path",
                    value: happ_path
                },
            });
            
            const issued = await adminWs.issueAppAuthenticationToken({
                installed_app_id: app.installed_app_id,
              });
            let appWs = await conductor.connectAppWs(issued.token, port);
            const agentApp = await enableAndGetAgentApp(conductor.adminWs(), appWs, app);
            out.push({
                agent_happ: agentApp,
                conductor
            })
        } catch (e) {
            console.error(e);
        }
    }
    return out
}

//@ts-ignore
export function sortedObject(unordered) {
  if(typeof unordered !== "object") return unordered;
  return Object.keys(unordered).sort().reduce(
    (obj, key) => {
        //@ts-ignore
        obj[key] = sortedObject(unordered[key]);
        return obj;
    }, {});
}