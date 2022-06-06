import { isReference } from "@apollo/client";
import { Ad4mClient, Link, LinkQuery, PerspectiveProxy, LinkExpression, ExpressionProof, Literal } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";
import * as fs from 'fs';
const path = require('path')

export default function socialDNATests(testContext: TestContext) {
    return  () => {
        describe("There is a SDNA test exercising an example TODO SDNA", () => {
            // read in prolog facts from todo.pl
            // add each line as target in new link has_zome
            it('can add social DNA to perspective', async () => {
                const prologRules = [
                    'register_sdna_flow("TODO", t).',
                    'flowable(_, t).',
                    `start_action(t, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}').`,
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
                    perspective.add(new Link({
                        source: 'self', 
                        predicate: 'ad4m://has_zome',
                        target: Literal.from(fact).toUrl(),
                    }))
                }

                let sDNAFacts = await ad4mClient!.perspective.queryLinks(perspective.uuid, new LinkQuery({source: "self", predicate: "ad4m://has_zome"}));
                expect(sDNAFacts.length).toEqual(8);
                // expect there to be 8 links off self
                let flows = await perspective.sdnaFlows()
                console.log('dna flows: ', flows)
                expect(flows[0]).toBe('TODO')
                perspective.add(new Link({source: 'self', target: 'test-lang://1234'}))
                perspective.startFlow('TODO', 'test-lang://1234')
            }) 
        })
    }
}
