import fs from "fs";

const publishingAgentPath = "./tst-tmp/agents/p/ad4m/agent.json";
const bootstrapSeedPath = "./bootstrapSeed.json";

async function main() {
    if (fs.existsSync(publishingAgentPath)) {
        const didData = JSON.parse(fs.readFileSync(publishingAgentPath).toString());
        if (fs.existsSync(bootstrapSeedPath)) {
            const bootstrapSeed = JSON.parse(fs.readFileSync(bootstrapSeedPath).toString());
            bootstrapSeed["trustedAgents"].push(didData["did"]);
            fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
        } else {
            throw new Error(`Could not find boostrapSeed at path: ${bootstrapSeedPath}`)
        }
    } else {
        throw new Error(`Could not find publishingAgent at path: ${publishingAgentPath}`)
    }
}

main();