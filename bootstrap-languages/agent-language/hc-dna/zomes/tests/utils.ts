import { AgentApp, CallableCell, Conductor, NetworkType, enableAndGetAgentApp, runLocalServices } from "@holochain/tryorama";
import { dnas } from './common';
import { createConductor } from "@holochain/tryorama";
import { resolve } from "path";

export function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export async function createConductors(num: number): Promise<{agent_happ: AgentApp, conductor: Conductor}[]> {
    let out = [] as {agent_happ: AgentApp, conductor: Conductor}[];

    const localServices = await runLocalServices();

    for (let n of Array(num).keys()) {
        let conductor = await createConductor(localServices.signalingServerUrl, {networkType: NetworkType.WebRtc, bootstrapServerUrl: localServices.bootstrapServerUrl});
        let port = await conductor.attachAppInterface();
        let appWs = await conductor.connectAppWs(port);
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