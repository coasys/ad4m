import { Link, LinkQuery, SHACLFlow } from "@coasys/ad4m";
import { TestContext } from './integration.test'
import { expect } from "chai";
import { sleep } from "../utils/utils";

export default function socialDNATests(testContext: TestContext) {
    return () => {
        describe("SHACL-based TODO flow", () => {
            before(async () => {
                // Ensure agent is generated even when this describe is run in isolation
                // (e.g. via mocha --grep). In the full suite the Agent describe runs first
                // and does this; this is a no-op then.
                const ad4mClient = testContext.ad4mClient!
                const status = await ad4mClient.agent.status()
                if (!status.isInitialized) {
                    await ad4mClient.agent.generate("passphrase")
                }
            });

            it('can add SHACL flow and go through full TODO workflow', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create perspective
                const perspective = await ad4mClient.perspective.add("shacl-flow-test");
                expect(perspective.name).to.be.equal("shacl-flow-test");

                // Create a SHACLFlow for TODO workflow
                const todoFlow = new SHACLFlow('TODO', 'todo://');
                todoFlow.inputTypes = ['any'];

                // Define states
                todoFlow.addState({
                    name: 'ready',
                    value: 0,
                    stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
                });
                todoFlow.addState({
                    name: 'doing',
                    value: 0.5,
                    stateCheck: { predicate: 'todo://state', target: 'todo://doing' }
                });
                todoFlow.addState({
                    name: 'done',
                    value: 1,
                    stateCheck: { predicate: 'todo://state', target: 'todo://done' }
                });

                // Define start action
                todoFlow.startAction = [{
                    action: 'addLink',
                    source: 'this',
                    predicate: 'todo://state',
                    target: 'todo://ready'
                }];

                // Define transitions
                todoFlow.addTransition({
                    actionName: 'Start',
                    fromState: 'ready',
                    toState: 'doing',
                    actions: [
                        { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' },
                        { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
                    ]
                });
                todoFlow.addTransition({
                    actionName: 'Finish',
                    fromState: 'doing',
                    toState: 'done',
                    actions: [
                        { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://done' },
                        { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' }
                    ]
                });

                // Register the flow
                await perspective.addFlow('TODO', todoFlow);

                // Test sdnaFlows() returns the flow name
                let flows = await perspective.sdnaFlows();
                expect(flows).to.include('TODO');

                // Add an expression and test availableFlows()
                await perspective.add(new Link({ source: 'ad4m://self', target: 'test-lang://1234' }));
                let availableFlows = await perspective.availableFlows('test-lang://1234');
                expect(availableFlows.length).to.be.equal(1);
                expect(availableFlows[0]).to.be.equal('TODO');

                // Test startFlow() creates the right links
                await perspective.startFlow('TODO', 'test-lang://1234');

                let flowLinks = await ad4mClient.perspective.queryLinks(
                    perspective.uuid,
                    new LinkQuery({ source: 'test-lang://1234', predicate: 'todo://state' })
                );
                expect(flowLinks.length).to.be.equal(1);
                expect(flowLinks[0].data.target).to.be.equal('todo://ready');

                // Test flowState() returns correct state
                let todoState = await perspective.flowState('TODO', 'test-lang://1234');
                expect(todoState).to.be.equal(0);

                // Test expressionsInFlowState() finds expressions
                let expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0);
                expect(expressionsInTodo.length).to.be.equal(1);
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234');

                // Test flowActions() returns available actions
                let flowActions = await perspective.flowActions('TODO', 'test-lang://1234');
                expect(flowActions.length).to.be.equal(1);
                expect(flowActions[0]).to.be.equal('Start');

                // Test runFlowAction() transitions state: ready -> doing
                await perspective.runFlowAction('TODO', 'test-lang://1234', 'Start');
                await sleep(100);

                todoState = await perspective.flowState('TODO', 'test-lang://1234');
                expect(todoState).to.be.equal(0.5);

                flowLinks = await ad4mClient.perspective.queryLinks(
                    perspective.uuid,
                    new LinkQuery({ source: 'test-lang://1234', predicate: 'todo://state' })
                );
                expect(flowLinks.length).to.be.equal(1);
                expect(flowLinks[0].data.target).to.be.equal('todo://doing');

                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0.5);
                expect(expressionsInTodo.length).to.be.equal(1);
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234');

                // Test transition: doing -> done
                flowActions = await perspective.flowActions('TODO', 'test-lang://1234');
                expect(flowActions.length).to.be.equal(1);
                expect(flowActions[0]).to.be.equal('Finish');

                await perspective.runFlowAction('TODO', 'test-lang://1234', 'Finish');
                await sleep(100);

                todoState = await perspective.flowState('TODO', 'test-lang://1234');
                expect(todoState).to.be.equal(1);

                flowLinks = await ad4mClient.perspective.queryLinks(
                    perspective.uuid,
                    new LinkQuery({ source: 'test-lang://1234', predicate: 'todo://state' })
                );
                expect(flowLinks.length).to.be.equal(1);
                expect(flowLinks[0].data.target).to.be.equal('todo://done');

                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 1);
                expect(expressionsInTodo.length).to.be.equal(1);
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234');
            });
        })
    }
}
