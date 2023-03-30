import { AgentApp, CallableCell, Conductor } from "@holochain/tryorama";
import { dnas } from './common';
import { createConductor } from "@holochain/tryorama";
import { resolve } from "path";

export function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export async function createConductors(num: number): Promise<{agent_happ: AgentApp, conductor: Conductor}[]> {
    let out = [] as {agent_happ: AgentApp, conductor: Conductor}[];
    for (let n of Array(num).keys()) {
        let conductor = await createConductor();
        try {
            let app = await conductor.installApp({
                bundle: {
                    manifest: {
                        manifest_version: "1",
                        name: "agent_store",
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
            await conductor.adminWs().enableApp({installed_app_id: app.appId})
            out.push({
                agent_happ: app,
                conductor
            })
        } catch (e) {
            console.error(e);
        }
    }
    return out
}