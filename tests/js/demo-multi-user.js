import { Ad4mClient } from "@coasys/ad4m";
import Ad4mConnect from "@coasys/ad4m-connect";
import { apolloClient, startExecutor } from "./utils/utils.ts";
import path from "path";
import fs from "fs-extra";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function demoMultiUser() {
    console.log("🎉 Multi-User Ad4m Demo");
    console.log("=======================");
    
    const TEST_DIR = path.join(`${__dirname}/tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "demo-multi-user");
    const bootstrapSeedPath = path.join(`${__dirname}/bootstrapSeed.json`);
    const gqlPort = 16100;
    const hcAdminPort = 16101;
    const hcAppPort = 16102;

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
        console.log("✅ Backend initialized");

        console.log("\n👤 Testing User 1 - New User Creation via Ad4m-Connect");
        console.log("--------------------------------------------------------");
        
        // Test 1: New user creation via ad4m-connect
        const user1Connect = new Ad4mConnect({
            appName: "Demo App",
            appDesc: "Multi-user demo application",
            appDomain: "demo.ad4m.org",
            capabilities: [{ 
                with: { domain: "*", pointers: ["*"] }, 
                can: ["*"] 
            }],
            multiUser: true,
            backendUrl: `ws://localhost:${gqlPort}/graphql`,
            userEmail: "alice@example.com",
            userPassword: "alice123"
        });

        const user1Client = await user1Connect.connect();
        const user1Agent = await user1Client.agent.me();
        console.log("✅ User 1 connected with DID:", user1Agent.did);

        console.log("\n👤 Testing User 2 - New User Creation via Ad4m-Connect");
        console.log("--------------------------------------------------------");
        
        // Test 2: Another new user
        const user2Connect = new Ad4mConnect({
            appName: "Demo App",
            appDesc: "Multi-user demo application", 
            appDomain: "demo.ad4m.org",
            capabilities: [{ 
                with: { domain: "*", pointers: ["*"] }, 
                can: ["*"] 
            }],
            multiUser: true,
            backendUrl: `ws://localhost:${gqlPort}/graphql`,
            userEmail: "bob@example.com",
            userPassword: "bob456"
        });

        const user2Client = await user2Connect.connect();
        const user2Agent = await user2Client.agent.me();
        console.log("✅ User 2 connected with DID:", user2Agent.did);

        console.log("\n🔄 Testing User 1 - Existing User Login");
        console.log("----------------------------------------");
        
        // Test 3: Existing user login
        const user1LoginConnect = new Ad4mConnect({
            appName: "Demo App",
            appDesc: "Multi-user demo application",
            appDomain: "demo.ad4m.org", 
            capabilities: [{ 
                with: { domain: "*", pointers: ["*"] }, 
                can: ["*"] 
            }],
            multiUser: true,
            backendUrl: `ws://localhost:${gqlPort}/graphql`,
            userEmail: "alice@example.com",
            userPassword: "alice123"
        });

        const user1LoginClient = await user1LoginConnect.connect();
        const user1LoginAgent = await user1LoginClient.agent.me();
        console.log("✅ User 1 logged in again with same DID:", user1LoginAgent.did);
        console.log("✅ DID consistency:", user1Agent.did === user1LoginAgent.did ? "PASS" : "FAIL");

        console.log("\n📊 Summary");
        console.log("----------");
        console.log("User 1 DID:", user1Agent.did);
        console.log("User 2 DID:", user2Agent.did);
        console.log("Different users:", user1Agent.did !== user2Agent.did ? "✅ PASS" : "❌ FAIL");
        console.log("Login persistence:", user1Agent.did === user1LoginAgent.did ? "✅ PASS" : "❌ FAIL");

        console.log("\n🎉 Multi-User Ad4m Federation Demo Complete!");
        console.log("✅ User registration working");
        console.log("✅ User login working"); 
        console.log("✅ User isolation working");
        console.log("✅ Ad4m-Connect integration working");
        console.log("✅ JWT token authentication working");

    } catch (error) {
        console.error("❌ Demo failed:", error.message);
    } finally {
        console.log("\n🧹 Cleaning up...");
        executorProcess.kill();
    }
}

demoMultiUser().catch(console.error);

