import { expect } from "chai";
import { exec, execSync, ChildProcess } from 'node:child_process';
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import { HttpLink } from "@apollo/client/link/http/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";
import { Ad4mClient, Literal, PerspectiveProxy, Subject } from "@perspect3vism/ad4m";
import { rmSync, readFileSync } from "node:fs";
import fetch from 'node-fetch';

function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


function apolloClient(port: number, token?: string): ApolloClient<any> {
    const wsLink = new GraphQLWsLink(createClient({
        url: `ws://localhost:${port}/graphql`,
        webSocketImpl: Websocket,
        /*connectionParams: () => {
            return {
                headers: {
                    authorization: token
                }
            }
        },*/
    }));

    const link = new HttpLink({
        uri: "http://localhost:4000/graphql",
        //@ts-ignore
        fetch
      });
  
    return new ApolloClient({
        link: wsLink,
        cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
        defaultOptions: {
            watchQuery: {
                fetchPolicy: "no-cache",
            },
            query: {
                fetchPolicy: "no-cache",
            }
        },
    });
  }

describe("Integration", () => {
    let ad4m: Ad4mClient | null = null
    let executorProcess: ChildProcess | null = null

    before(async () => {
        ad4m = new Ad4mClient(apolloClient(4000))
        return
        /*
        rmSync("../ad4mJS", { recursive: true, force: true })
        console.log("Initialzing executor data directory")
        //@ts-ignore
        execSync('../../host/dist/ad4m-macos-x64 init --dataPath ../ad4mJS', {})
        
        console.log("Starting executor")
        execSync("killall holochain")
        //@ts-ignore
        executorProcess = exec('../../host/dist/ad4m-macos-x64 serve --dataPath ../ad4mJS', {})

        let executorReady = new Promise<void>((resolve, reject) => {
            executorProcess!.stdout!.on('data', (data) => {
                if (data.includes("GraphQL server started")) {
                    resolve()
                }
            });
        })

        executorProcess!.stdout!.on('data', (data) => {
            console.log(`${data}`);
        });
        //Log holochain process stderr to out
        executorProcess!.stderr!.on('data', (data) => {
            console.log(`${data}`);
        });
    
        console.log("Waiting for executor to settle...")
        await executorReady
        console.log("Creating ad4m client")
        ad4m = new Ad4mClient(apolloClient(4000))
        console.log("Generating agent")
        await ad4m.agent.generate("secret")
        console.log("Done")
        */
    })

    after(() => {
        if (executorProcess) {
            executorProcess.kill()
        }
    })

    it("should get agent status", async () => {
        let result = await ad4m!.agent.status()
        expect(result).to.not.be.null
        expect(result!.isInitialized).to.be.true
    })

    it("should find and instantiate subject classes", async () => {
        let perspective: PerspectiveProxy = await ad4m!.perspective.add("test")
        console.log("UUID: " + perspective.uuid)

        let classes = await perspective.subjectClasses();
        expect(classes.length).to.equal(0)

        let sdna = readFileSync("./subject.pl").toString()
        await perspective.setSdna(sdna)

        let retrievedSdna = await perspective.getSdna()
        expect(retrievedSdna).to.deep.equal([sdna])

        classes = await perspective.subjectClasses();
        expect(classes.length).to.equal(1)
        expect(classes[0]).to.equal("TODO")

        let root = Literal.from("test").toUrl()
        expect(await perspective.constructSubject("TODO", root)).to.be.true

        expect(await perspective.isSubjectInstance(root, "TODO")).to.be.true

        let subject: Subject = await perspective.subjectInstance(root, "TODO")

        //@ts-ignore
        expect(await subject.state).to.equal("todo://ready")

    })
})