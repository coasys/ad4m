import { Link, LinkQuery, Literal } from "@coasys/ad4m";
import { TestContext } from './integration.test'
import { expect } from "chai";
import { sleep } from "../utils/utils";

export default function socialDNATests(testContext: TestContext) {
    return  () => {
        describe("There is a SDNA test exercising an example TODO SDNA", () => {
            // SdnaOnly doesn't load links into prolog engine
            it.skip('can add social DNA to perspective and go through flow', async () => {
                const sdna = [
                    // The name of our SDNA flow: "TODO"
                    'register_sdna_flow("TODO", t).',

                    // What expressions can be used to start this flow? -> all
                    'flowable(_, t).',

                    // This Flow has 3 states (0=ready, 0.5=doing, 1=done), 
                    // which are represented by links with predicate 'todo://state'
                    'flow_state(ExprAddr, 0, t) :- triple(ExprAddr, "todo://state", "todo://ready").',
                    'flow_state(ExprAddr, 0.5, t) :- triple(ExprAddr, "todo://state", "todo://doing").',
                    'flow_state(ExprAddr, 1, t) :- triple(ExprAddr, "todo://state", "todo://done").',

                    // Initial action renders any expression into a todo item by adding a state link to 'ready'
                    `start_action('[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]', t).`,
                    // A ready todo can be 'started' = commencing work on it. Removes 'ready' link and replaces it by 'doing' link
                    `action(0, "Start", 0.5, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://doing"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').`,
                    // A todo in doing can be 'finished'
                    `action(0.5, "Finish", 1, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://done"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://doing"}]').`,
                ]

                const ad4mClient = testContext.ad4mClient!

                const perspective = await ad4mClient.perspective.add("sdna-test");
                expect(perspective.name).to.be.equal("sdna-test");

                await perspective.addSdna("Todo", sdna.join('\n'), "flow");
                

                let sDNAFacts = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "ad4m://self", predicate: "ad4m://has_flow"}));
                expect(sDNAFacts.length).to.be.equal(1);
                let flows = await perspective.sdnaFlows()
                expect(flows[0]).to.be.equal('TODO')
                
                await perspective.add(new Link({source: 'ad4m://self', target: 'test-lang://1234'}))
                let availableFlows = await perspective.availableFlows('test-lang://1234')
                expect(availableFlows.length).to.be.equal(1)
                expect(availableFlows[0]).to.be.equal('TODO')
                let startAction = await perspective.infer(`start_action(Action, F), register_sdna_flow("TODO", F)`)
                await perspective.startFlow('TODO', 'test-lang://1234')

                let flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).to.be.equal(1)
                expect(flowLinks[0].data.target).to.be.equal("todo://ready")

                let todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).to.be.equal(0)

                let expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0)
                expect(expressionsInTodo.length).to.be.equal(1)
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234')


                // continue flow
                let flowActions = await perspective.flowActions('TODO', 'test-lang://1234')
                expect(flowActions.length).to.be.equal(1)
                expect(flowActions[0]).to.be.equal("Start")


                await perspective.runFlowAction('TODO', 'test-lang://1234', "Start")
                await sleep(100)
                todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).to.be.equal(0.5)

                flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).to.be.equal(1)
                expect(flowLinks[0].data.target).to.be.equal("todo://doing")

                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0.5)
                expect(expressionsInTodo.length).to.be.equal(1)
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234')

                // continue flow
                flowActions = await perspective.flowActions('TODO', 'test-lang://1234')
                expect(flowActions.length).to.be.equal(1)
                expect(flowActions[0]).to.be.equal("Finish")


                await perspective.runFlowAction('TODO', 'test-lang://1234', "Finish")
                await sleep(100)
                todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).to.be.equal(1)

                flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).to.be.equal(1)
                expect(flowLinks[0].data.target).to.be.equal("todo://done")
                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 1)
                expect(expressionsInTodo.length).to.be.equal(1)
                expect(expressionsInTodo[0]).to.be.equal('test-lang://1234')

            }) 
        })
    }
}
