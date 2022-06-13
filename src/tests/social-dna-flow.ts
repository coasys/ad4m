import { isReference } from "@apollo/client";
import { Ad4mClient, Link, LinkQuery, PerspectiveProxy, LinkExpression, ExpressionProof, Literal } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";
import * as fs from 'fs';
const path = require('path')

export default function socialDNATests(testContext: TestContext) {
    return  () => {
        describe("There is a SDNA test exercising an example TODO SDNA", () => {
            it('can add social DNA to perspective and go through flow', async () => {
                const prologRules = [
                    'register_sdna_flow("TODO", t).',
                    'flowable(_, t).',
                    `start_action('[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]', t).`,
                    'flow_state(ExprAddr, 0, t) :- triple(ExprAddr, "todo://state", "todo://ready").',
                    `action(0, "Start", 0.5, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://doing"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').`,
                    'flow_state(ExprAddr, 0.5, t) :- triple(ExprAddr, "todo://state", "todo://doing").',
                    `action(0.5, "Finish", 1, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://done"}, {action: "removeLink", source: "this", predicate: "todo://state", target: "todo://doing"}]').`,
                    'flow_state(ExprAddr, 1, t) :- triple(ExprAddr, "todo://state", "todo://done").'
                ]

                const ad4mClient = testContext.ad4mClient!

                const perspective = await ad4mClient.perspective.add("sdna-test");
                expect(perspective.name).toEqual("sdna-test");

                for (const fact of prologRules) {
                    await perspective.add(new Link({
                        source: 'self', 
                        predicate: 'ad4m://has_zome',
                        target: Literal.from(fact).toUrl(),
                    }))
                }

                let sDNAFacts = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "self", predicate: "ad4m://has_zome"}));
                expect(sDNAFacts.length).toEqual(8);
                let flows = await perspective.sdnaFlows()
                expect(flows[0]).toBe('TODO')

                await perspective.add(new Link({source: 'self', target: 'test-lang://1234'}))
                let availableFlows = await perspective.availableFlows('test-lang://1234')
                expect(availableFlows.length).toEqual(1)
                expect(availableFlows[0]).toEqual('TODO')
                let startAction = await perspective.infer(`start_action(Action, F), register_sdna_flow("TODO", F)`)
                await perspective.startFlow('TODO', 'test-lang://1234')

                let flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).toEqual(1)
                expect(flowLinks[0].data.target).toEqual("todo://ready")

                let todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).toEqual(0)

                let expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0)
                expect(expressionsInTodo.length).toEqual(1)
                expect(expressionsInTodo[0]).toEqual('test-lang://1234')


                // continue flow
                let flowActions = await perspective.flowActions('TODO', 'test-lang://1234')
                expect(flowActions.length).toEqual(1)
                expect(flowActions[0]).toEqual("Start")


                await perspective.runFlowAction('TODO', 'test-lang://1234', "Start")
                todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).toEqual(0.5)

                flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).toEqual(1)
                expect(flowLinks[0].data.target).toEqual("todo://doing")

                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 0.5)
                expect(expressionsInTodo.length).toEqual(1)
                expect(expressionsInTodo[0]).toEqual('test-lang://1234')

                // continue flow
                flowActions = await perspective.flowActions('TODO', 'test-lang://1234')
                expect(flowActions.length).toEqual(1)
                expect(flowActions[0]).toEqual("Finish")


                await perspective.runFlowAction('TODO', 'test-lang://1234', "Finish")
                todoState = await perspective.flowState('TODO', 'test-lang://1234')
                expect(todoState).toEqual(1)

                flowLinks = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "test-lang://1234", predicate: "todo://state"}))
                expect(flowLinks.length).toEqual(1)
                expect(flowLinks[0].data.target).toEqual("todo://done")
                expressionsInTodo = await perspective.expressionsInFlowState('TODO', 1)
                expect(expressionsInTodo.length).toEqual(1)
                expect(expressionsInTodo[0]).toEqual('test-lang://1234')

            }) 
        })
    }
}
