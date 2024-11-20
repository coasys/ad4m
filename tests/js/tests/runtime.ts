import { TestContext } from './integration.test'
import fs from "fs";
import { expect } from "chai";
import { ModelInput, Notification, NotificationInput, TriggeredNotification } from '@coasys/ad4m/lib/src/runtime/RuntimeResolver';
import sinon from 'sinon';
import { sleep } from '../utils/utils';
import { ExceptionType, Link } from '@coasys/ad4m';
// Imports needed for webhook tests:
// (deactivated for now because these imports break the test suite on CI)
// (( local execution works - I leave this here for manualy local testing ))
//import express from 'express';
//import bodyParser from 'body-parser';
//import { Server } from 'http';

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

            const mockFunction = sinon.stub();

            let ignoreRequest = false

            // Setup the stub to automatically resolve when called
            mockFunction.callsFake((exception) => {
                if(ignoreRequest) return

                if (exception.type === ExceptionType.InstallNotificationRequest) {
                    const requestedNotification = JSON.parse(exception.addon);

                    expect(requestedNotification.description).to.equal(notification.description);
                    expect(requestedNotification.appName).to.equal(notification.appName);
                    expect(requestedNotification.appUrl).to.equal(notification.appUrl);
                    expect(requestedNotification.appIconPath).to.equal(notification.appIconPath);
                    expect(requestedNotification.trigger).to.equal(notification.trigger);
                    expect(requestedNotification.perspectiveIds).to.eql(notification.perspectiveIds);
                    expect(requestedNotification.webhookUrl).to.equal(notification.webhookUrl);
                    expect(requestedNotification.webhookAuth).to.equal(notification.webhookAuth);
                    // Automatically resolve without needing to manually manage a Promise
                    return null;
                }
            });

            await ad4mClient.runtime.addExceptionCallback(mockFunction);

            // Request to install a new notification
            const notificationId = await ad4mClient.runtime.requestInstallNotification(notification);

            await sleep(2000)

            // Use sinon's assertions to wait for the stub to be called
            await sinon.assert.calledOnce(mockFunction);
            ignoreRequest = true;

            // Check if the notification is in the list of notifications
            const notificationsBeforeGrant = await ad4mClient.runtime.notifications()
            expect(notificationsBeforeGrant.length).to.equal(1)
            const notificationInList = notificationsBeforeGrant[0]
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
            // after changing a notification it needs to be granted again
            expect(updatedNotificationInList?.granted).to.be.false
            expect(updatedNotificationInList?.description).to.equal(updatedNotification.description)

            // Check if the notification is removed
            const removed = await ad4mClient.runtime.removeNotification(notificationId)
            expect(removed).to.be.true
        })

        it("can trigger notifications", async () => {
            const ad4mClient = testContext.ad4mClient!

            let triggerPredicate = "ad4m://notification"

            let notificationPerspective = await ad4mClient.perspective.add("notification test perspective")
            let otherPerspective = await ad4mClient.perspective.add("other perspective")

            const notification: NotificationInput = {
                description: "ad4m://notification predicate used",
                appName: "ADAM tests",
                appUrl: "Test App URL",
                appIconPath: "Test App Icon Path",
                trigger: `triple(Source, "${triggerPredicate}", Target)`,
                perspectiveIds: [notificationPerspective.uuid],
                webhookUrl: "Test Webhook URL",
                webhookAuth: "Test Webhook Auth"
            }

            // Request to install a new notification
            const notificationId = await ad4mClient.runtime.requestInstallNotification(notification);
            sleep(1000)
            // Grant the notification
            const granted = await ad4mClient.runtime.grantNotification(notificationId)
            expect(granted).to.be.true

            const mockFunction = sinon.stub();
            await ad4mClient.runtime.addNotificationTriggeredCallback(mockFunction)

            // Ensuring no false positives
            await notificationPerspective.add(new Link({source: "control://source", target: "control://target"}))
            await sleep(1000)
            expect(mockFunction.called).to.be.false

            // Ensuring only selected perspectives will trigger
            await otherPerspective.add(new Link({source: "control://source", predicate: triggerPredicate, target: "control://target"}))
            await sleep(1000)
            expect(mockFunction.called).to.be.false

            // Happy path
            await notificationPerspective.add(new Link({source: "test://source", predicate: triggerPredicate, target: "test://target1"}))
            await sleep(7000)
            expect(mockFunction.called).to.be.true
            let triggeredNotification = mockFunction.getCall(0).args[0] as TriggeredNotification
            expect(triggeredNotification.notification.description).to.equal(notification.description)
            let triggerMatch = JSON.parse(triggeredNotification.triggerMatch)
            expect(triggerMatch.length).to.equal(1)
            let match = triggerMatch[0]
            //@ts-ignore
            expect(match.Source).to.equal("test://source")
            //@ts-ignore
            expect(match.Target).to.equal("test://target1")

            // Ensuring we don't get old data on a new trigger
            await notificationPerspective.add(new Link({source: "test://source", predicate: triggerPredicate, target: "test://target2"}))
            await sleep(7000)
            expect(mockFunction.callCount).to.equal(2)
            triggeredNotification = mockFunction.getCall(1).args[0] as TriggeredNotification
            triggerMatch = JSON.parse(triggeredNotification.triggerMatch)
            expect(triggerMatch.length).to.equal(1)
            match = triggerMatch[0]
            //@ts-ignore
            expect(match.Source).to.equal("test://source")
            //@ts-ignore
            expect(match.Target).to.equal("test://target2")
        })


         
        // See comments on the imports at the top
        // breaks CI for some reason but works locally
        // leaving this here for manual local testing
        /*
        it("should trigger a notification and call the webhook", async () => {
            const ad4mClient = testContext.ad4mClient!
            const webhookUrl = 'http://localhost:8080/webhook';
            const webhookAuth = 'Test Webhook Auth'
             // Setup Express server
             const app = express();
             app.use(bodyParser.json());
 
             let webhookCalled = false;
             let webhookGotAuth = ""
             let webhookGotBody = null
 
             app.post('/webhook', (req, res) => {
                 webhookCalled = true;
                 webhookGotAuth = req.headers['authorization']?.substring("Bearer ".length)||"";
                 webhookGotBody = req.body;
                 res.status(200).send({ success: true });
             });
 
             let server: Server|void
             let serverRunning = new Promise<void>((done) => {
                server = app.listen(8080, () => {
                    console.log('Test server running on port 8080');
                    done()
                });
             })

             await serverRunning
             

            let triggerPredicate = "ad4m://notification_webhook"
            let notificationPerspective = await ad4mClient.perspective.add("notification test perspective")
            let otherPerspective = await ad4mClient.perspective.add("other perspective")

            const notification: NotificationInput = {
                description: "ad4m://notification predicate used",
                appName: "ADAM tests",
                appUrl: "Test App URL",
                appIconPath: "Test App Icon Path",
                trigger: `triple(Source, "${triggerPredicate}", Target)`,
                perspectiveIds: [notificationPerspective.uuid],
                webhookUrl: webhookUrl,
                webhookAuth: webhookAuth
            }

            // Request to install a new notification
            const notificationId = await ad4mClient.runtime.requestInstallNotification(notification);
            sleep(1000)
            // Grant the notification
            const granted = await ad4mClient.runtime.grantNotification(notificationId)
            expect(granted).to.be.true

            // Ensuring no false positives
            await notificationPerspective.add(new Link({source: "control://source", target: "control://target"}))
            await sleep(1000)
            expect(webhookCalled).to.be.false

            // Ensuring only selected perspectives will trigger
            await otherPerspective.add(new Link({source: "control://source", predicate: triggerPredicate, target: "control://target"}))
            await sleep(1000)
            expect(webhookCalled).to.be.false

            // Happy path
            await notificationPerspective.add(new Link({source: "test://source", predicate: triggerPredicate, target: "test://target1"}))
            await sleep(1000)
            expect(webhookCalled).to.be.true
            expect(webhookGotAuth).to.equal(webhookAuth)
            expect(webhookGotBody).to.be.not.be.null
            let triggeredNotification = webhookGotBody as unknown as TriggeredNotification
            let triggerMatch = JSON.parse(triggeredNotification.triggerMatch)
            expect(triggerMatch.length).to.equal(1)
            let match = triggerMatch[0]
            //@ts-ignore
            expect(match.Source).to.equal("test://source")
            //@ts-ignore
            expect(match.Target).to.equal("test://target1")

            // Reset webhookCalled for the next test
            webhookCalled = false;
            webhookGotAuth = ""
            webhookGotBody = null

            await notificationPerspective.add(new Link({source: "test://source", predicate: triggerPredicate, target: "test://target2"}))
            await sleep(1000)
            expect(webhookCalled).to.be.true
            expect(webhookGotAuth).to.equal(webhookAuth)
            triggeredNotification = webhookGotBody as unknown as TriggeredNotification
            triggerMatch = JSON.parse(triggeredNotification.triggerMatch)
            expect(triggerMatch.length).to.equal(1)
            match = triggerMatch[0]
            //@ts-ignore
            expect(match.Source).to.equal("test://source")
            //@ts-ignore
            expect(match.Target).to.equal("test://target2")

            // Close the server after the test
            //@ts-ignore
            server!.close()
        })
        */
    }
}
