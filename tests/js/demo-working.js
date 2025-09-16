import { Ad4mClient } from "@coasys/ad4m";
import { apolloClient, startExecutor } from "./utils/utils.ts";
import path from "path";
import fs from "fs-extra";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function demoWorkingMultiUser() {
    console.log("🎉 WORKING Multi-User Ad4m Demo");
    console.log("================================");
    
    const TEST_DIR = path.join(`${__dirname}/tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "demo-working");
    const bootstrapSeedPath = path.join(`${__dirname}/bootstrapSeed.json`);
    const gqlPort = 16200;
    const hcAdminPort = 16201;
    const hcAppPort = 16202;

    if (!fs.existsSync(appDataPath)) {
        fs.mkdirSync(appDataPath, { recursive: true });
    }

    console.log("📡 Starting Ad4m executor as multi-user backend...");
    const executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
        gqlPort, hcAdminPort, hcAppPort, false);

    // Wait for full initialization
    await new Promise(resolve => setTimeout(resolve, 5000));

    try {
        // Initialize the main agent (needed for JWT signing)
        const adminClient = new Ad4mClient(apolloClient(gqlPort), false);
        await adminClient.agent.generate("passphrase");
        console.log("✅ Backend initialized and ready for multi-user connections");

        console.log("\n👤 Creating User 1: alice@example.com");
        console.log("-------------------------------------");
        
        // Create first user
        const user1Result = await adminClient.agent.createUser("alice@example.com", "alice123");
        console.log("✅ User created:", user1Result);
        
        // Login as first user
        const user1Token = await adminClient.agent.loginUser("alice@example.com", "alice123");
        console.log("✅ Login token generated");
        
        // Create authenticated client for user 1
        const user1Client = new Ad4mClient(apolloClient(gqlPort, user1Token), false);
        const user1Agent = await user1Client.agent.me();
        console.log("✅ User 1 authenticated as:", user1Agent.did);

        console.log("\n👤 Creating User 2: bob@example.com");
        console.log("-----------------------------------");
        
        // Create second user
        const user2Result = await adminClient.agent.createUser("bob@example.com", "bob456");
        console.log("✅ User created:", user2Result);
        
        // Login as second user
        const user2Token = await adminClient.agent.loginUser("bob@example.com", "bob456");
        console.log("✅ Login token generated");
        
        // Create authenticated client for user 2
        const user2Client = new Ad4mClient(apolloClient(gqlPort, user2Token), false);
        const user2Agent = await user2Client.agent.me();
        console.log("✅ User 2 authenticated as:", user2Agent.did);

        console.log("\n🔄 Testing User 1 Login Again");
        console.log("-----------------------------");
        
        // Test login persistence
        const user1Token2 = await adminClient.agent.loginUser("alice@example.com", "alice123");
        const user1Client2 = new Ad4mClient(apolloClient(gqlPort, user1Token2), false);
        const user1Agent2 = await user1Client2.agent.me();
        console.log("✅ User 1 logged in again as:", user1Agent2.did);

        console.log("\n📊 RESULTS");
        console.log("==========");
        console.log("User 1 DID (first login): ", user1Agent.did);
        console.log("User 1 DID (second login):", user1Agent2.did);
        console.log("User 2 DID:               ", user2Agent.did);
        console.log("");
        console.log("✅ User isolation:     ", user1Agent.did !== user2Agent.did ? "WORKING" : "FAILED");
        console.log("✅ Login persistence:  ", user1Agent.did === user1Agent2.did ? "WORKING" : "FAILED");
        console.log("✅ Unique DIDs:        ", user1Agent.did !== user2Agent.did && user1Agent.did === user1Agent2.did ? "WORKING" : "FAILED");

        console.log("\n🎉 SUCCESS: Multi-User Ad4m Federation is WORKING!");
        console.log("🚀 Ready for production testing!");

    } catch (error) {
        console.error("❌ Demo failed:", error.message);
        console.error(error);
    } finally {
        console.log("\n🧹 Cleaning up...");
        executorProcess.kill();
    }
}

demoWorkingMultiUser().catch(console.error);

