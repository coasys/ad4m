import { TestContext } from './integration.test'
import fs from "fs";
import { expect } from "chai";
import { NotificationInput } from '@coasys/ad4m/lib/src/runtime/RuntimeResolver';

const PERSPECT3VISM_AGENT = "did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n"
const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();
const PUBLISHING_AGENT = JSON.parse(fs.readFileSync("./tst-tmp/agents/p/ad4m/agent.json").toString())["did"];

export default function runtimeTests(testContext: TestContext) {
    return () => {
        it('Trusted Agents CRUD', async () => {
            const ad4mClient = testContext.ad4mClient!
            const { did } = await ad4mClient.agent.status()

            const initalAgents = await ad4mClient.runtime.getTrustedAgents();
            console.warn(initalAgents);
            console.warn([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT ]);
            expect(initalAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT ].sort())

            const addAgents = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, 'agentPubKey', 'agentPubKey2' ].sort())

            //Add the agents again to be sure we cannot get any duplicates
            const addAgentsDuplicate = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgentsDuplicate).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, 'agentPubKey', 'agentPubKey2' ].sort())

            const getAgents = await ad4mClient.runtime.getTrustedAgents();
            expect(getAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, 'agentPubKey', 'agentPubKey2' ].sort())

            const deleteAgents1 = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey2"])
            expect(deleteAgents1).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, "agentPubKey" ].sort())

            const deleteAgents2 = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey", "agentPubKey2"])
            expect(deleteAgents2).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT ].sort())

            const getAgentsPostDelete = await ad4mClient.runtime.getTrustedAgents();
            expect(getAgentsPostDelete).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT ].sort())
        })


        it('CRUD for known LinkLanguage templates', async () => {
            const ad4mClient = testContext.ad4mClient!

            const addresses = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(addresses).to.eql([ DIFF_SYNC_OFFICIAL ])

            const addAddresses = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addAddresses).to.eql([ DIFF_SYNC_OFFICIAL, 'Qm123', 'Qmabc' ].sort())

            //Add the agents again to be sure we cannot get any duplicates
            const addDuplicate = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addDuplicate).to.eql([ DIFF_SYNC_OFFICIAL, 'Qm123', 'Qmabc' ].sort())

            const get = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(get).to.eql([ DIFF_SYNC_OFFICIAL, 'Qm123', 'Qmabc' ].sort())

            const deleted = await ad4mClient.runtime.removeKnownLinkLanguageTemplates(["Qm123"])
            expect(deleted).to.eql([ DIFF_SYNC_OFFICIAL, "Qmabc" ].sort())

            const deleted2 = await ad4mClient.runtime.removeKnownLinkLanguageTemplates(["Qm123", "Qmabc"])
            expect(deleted2).to.eql([ DIFF_SYNC_OFFICIAL ])

            const getPostDelete = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(getPostDelete).to.eql([ DIFF_SYNC_OFFICIAL  ])
        })

        it('CRUD for friends', async () => {
            const ad4mClient = testContext.ad4mClient!

            const dids = await ad4mClient.runtime.friends();
            expect(dids).to.eql([ ])

            const added = await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);
            expect(added).to.eql(["did:test:1", "did:test:2"])

            //Add the agents again to be sure we cannot get any duplicates
            const addDuplicate = await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);
            expect(addDuplicate).to.eql(["did:test:1", "did:test:2"])

            const get = await ad4mClient.runtime.friends();
            expect(get).to.eql(["did:test:1", "did:test:2"])

            const deleted = await ad4mClient.runtime.removeFriends(["did:test:1"])
            expect(deleted).to.eql([ "did:test:2" ])

            const deleted2 = await ad4mClient.runtime.removeFriends(["did:test:1", "did:test:2"])
            expect(deleted2).to.eql([])

            const getPostDelete = await ad4mClient.runtime.friends();
            expect(getPostDelete).to.eql([  ])
        })

        it("doesn't mix up stores", async () => {
            const ad4mClient = testContext.ad4mClient!
            const { did } = await ad4mClient.agent.status()

            await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);

            const addresses = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(addresses).to.eql([ DIFF_SYNC_OFFICIAL ])

            const initalAgents = await ad4mClient.runtime.getTrustedAgents();
            expect(initalAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT ].sort())


            const addAddresses = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addAddresses).to.eql([ DIFF_SYNC_OFFICIAL, 'Qm123', 'Qmabc' ].sort())

            const addAgents = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, 'agentPubKey', 'agentPubKey2' ].sort())

            const dids = await ad4mClient.runtime.friends()
            expect(dids).to.eql(["did:test:1", "did:test:2"].sort())


            const deleted = await ad4mClient.runtime.removeFriends(["did:test:1", "agentPubKey", "Qm123"])
            expect(deleted).to.eql(["did:test:2" ])

            const postDeleteAddresses = await ad4mClient.runtime.knownLinkLanguageTemplates()
            expect(postDeleteAddresses).to.eql([ DIFF_SYNC_OFFICIAL, 'Qm123', 'Qmabc' ].sort())

            const postDeleteAgents = await ad4mClient.runtime.getTrustedAgents()
            expect(postDeleteAgents).to.eql([ did, PERSPECT3VISM_AGENT, PUBLISHING_AGENT, 'agentPubKey', 'agentPubKey2' ].sort())
        })

        it("can deal with Holochain's agent_infos", async () => {
            const ad4mClient = testContext.ad4mClient!
            // @ts-ignore
            const agentInfos = await ad4mClient.runtime.hcAgentInfos()
            // @ts-ignore
            expect(await ad4mClient.runtime.hcAddAgentInfos(agentInfos)).to.be.true;
        })

        it("can get runtimeInfo", async () => {
            const ad4mClient = testContext.ad4mClient!
            const runtimeInfo = await ad4mClient.runtime.info();
            expect(runtimeInfo.ad4mExecutorVersion).to.be.equal(process.env.npm_package_version);
            expect(runtimeInfo.isUnlocked).to.be.true;
            expect(runtimeInfo.isInitialized).to.be.true;
        })


    it("can handle notifications", async () => {
        const ad4mClient = testContext.ad4mClient!

        const notification: NotificationInput = {
            description: "Test Description",
            appName: "Test App Name",
            appUrl: "Test App URL",
            appIconPath: "Test App Icon Path",
            trigger: "Test Trigger",
            perspectiveIds: ["Test Perspective ID"],
            webhookUrl: "Test Webhook URL",
            webhookAuth: "Test Webhook Auth"
        }

        // Request to install a new notification
        const notificationId = await ad4mClient.runtime.requestInstallNotification(notification)
        const mockFunction = () => {
            setTimeout(() => {
                const requestedNotification = {
                    id: notificationId,
                    description: notification.description,
                    appName: notification.appName,
                    appUrl: notification.appUrl,
                    appIconPath: notification.appIconPath,
                    trigger: notification.trigger,
                    perspective_ids: notification.perspective_ids,
                    webhookUrl: notification.webhookUrl,
                    webhookAuth: notification.webhookAuth
                };
                expect(requestedNotification.id).to.equal(notificationId);
                expect(requestedNotification.description).to.equal(notification.description);
                expect(requestedNotification.appName).to.equal(notification.appName);
                expect(requestedNotification.appUrl).to.equal(notification.appUrl);
                expect(requestedNotification.appIconPath).to.equal(notification.appIconPath);
                expect(requestedNotification.trigger).to.equal(notification.trigger);
                expect(requestedNotification.perspective_ids).to.eql(notification.perspective_ids);
                expect(requestedNotification.webhookUrl).to.equal(notification.webhookUrl);
                expect(requestedNotification.webhookAuth).to.equal(notification.webhookAuth);
            }, 1000);
        };
        ad4mClient.runtime.addNotificationRequestedCallback(mockFunction);
        // Check if the notification is in the list of notifications
        const notificationsBeforeGrant = await ad4mClient.runtime.notifications()
        const notificationInList = notificationsBeforeGrant.find((n) => n.id === notificationId)
        expect(notificationInList).to.exist
        expect(notificationInList?.granted).to.be.false

        // Grant the notification
        const granted = await ad4mClient.runtime.grantNotification(notificationId)
        expect(granted).to.be.true

        // Check if the notification is updated
        const updatedNotification: NotificationInput = {
            description: "Update Test Description",
            appName: "Test App Name",
            appUrl: "Test App URL",
            appIconPath: "Test App Icon Path",
            trigger: "Test Trigger",
            perspectiveIds: ["Test Perspective ID"],
            webhookUrl: "Test Webhook URL",
            webhookAuth: "Test Webhook Auth"
        }
        const updated = await ad4mClient.runtime.updateNotification(notificationId, updatedNotification)
        expect(updated).to.be.true

        const updatedNotificationCheck = await ad4mClient.runtime.notifications()
        const updatedNotificationInList = updatedNotificationCheck.find((n) => n.id === notificationId)
        expect(updatedNotificationInList).to.exist
        expect(updatedNotificationInList?.granted).to.be.true
        expect(updatedNotificationInList?.description).to.equal(updatedNotification.description)

        // Check if the notification is removed
        const removed = await ad4mClient.runtime.removeNotification(notificationId)
        expect(removed).to.be.true
    })
    }
}
