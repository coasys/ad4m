import { TestContext } from './test-context'
import { pollUntil } from '../utils/utils'
import { expect } from "chai";
import type { AiEndpoint } from '@coasys/ad4m';
import {
    FakeUpstream,
    FAKE_CHAT_MODEL,
    FAKE_EMBED_MODEL,
    FAKE_STT_MODEL,
    fakeVector,
    startFakeUpstream,
} from '../helpers/ai-upstream';

// The AI contract against a fake OpenAI-shaped upstream (helpers/ai-upstream.ts): the
// executor's real wire to a model server, no model weights. Real models run nightly in the
// Rust e2e tests (INTERPRETATION_E2E_*, TRANSCRIPTION_E2E_*).
export default function aiTests(testContext: TestContext) {
    return () => {
        describe('AI service', () => {
            let upstream: FakeUpstream
            let endpoint: AiEndpoint

            before(async () => {
                upstream = await startFakeUpstream()
                endpoint = await testContext.ad4mClient!.ai.addEndpoint({
                    name: "fake-upstream",
                    kind: "OPEN_AI_COMPATIBLE",
                    baseUrl: upstream.url,
                })
            })

            after(async () => {
                await testContext.ad4mClient!.ai.removeEndpoint(endpoint.id, true)
                await upstream.close()
            })

            it('registers an endpoint, checks it, and lists what it serves', async () => {
                const ad4mClient = testContext.ad4mClient!
                expect(endpoint.health.state).to.equal("REACHABLE")
                expect(endpoint.health.servedModels).to.include.members([FAKE_CHAT_MODEL, FAKE_EMBED_MODEL])
                expect(endpoint.hasApiKey).to.be.false

                const endpoints = await ad4mClient.ai.endpoints()
                expect(endpoints.map(e => e.id)).to.include(endpoint.id)

                const bySaved = await ad4mClient.ai.discoverModels({ endpointId: endpoint.id })
                expect(bySaved.map(m => m.id)).to.include(FAKE_CHAT_MODEL)
                const byServer = await ad4mClient.ai.discoverModels({ kind: "OPEN_AI_COMPATIBLE", baseUrl: upstream.url })
                expect(byServer.map(m => m.id)).to.deep.equal(bySaved.map(m => m.id))
            })

            it('adds, reads, updates and removes a model on the endpoint', async () => {
                const ad4mClient = testContext.ad4mClient!
                const id = await ad4mClient.ai.addModel({
                    name: "TestChat",
                    modelType: "LLM",
                    endpointId: endpoint.id,
                    remoteModel: FAKE_CHAT_MODEL,
                })

                const added = (await ad4mClient.ai.getModels()).find(m => m.id === id)
                expect(added).to.deep.include({
                    name: "TestChat",
                    modelType: "LLM",
                    endpointId: endpoint.id,
                    remoteModel: FAKE_CHAT_MODEL,
                })
                expect(added!.status.state).to.equal("READY")
                expect((await ad4mClient.ai.modelStatus(id)).state).to.equal("READY")

                expect(await ad4mClient.ai.updateModel(id, {
                    name: "TestChatRenamed",
                    modelType: "LLM",
                    endpointId: endpoint.id,
                    remoteModel: "not-served",
                    nativeTools: true,
                })).to.be.true
                const updated = (await ad4mClient.ai.getModels()).find(m => m.id === id)
                expect(updated?.name).to.equal("TestChatRenamed")
                expect(updated?.remoteModel).to.equal("not-served")
                expect(updated?.nativeTools).to.be.true
                expect(updated?.status.state).to.equal("MISSING")

                expect(await ad4mClient.ai.removeModel(id)).to.be.true
                expect((await ad4mClient.ai.getModels()).find(m => m.id === id)).to.be.undefined
            })

            it('refuses a model on an endpoint that does not exist', async () => {
                const ad4mClient = testContext.ad4mClient!
                let refused = ""
                try {
                    await ad4mClient.ai.addModel({
                        name: "Orphan",
                        modelType: "LLM",
                        endpointId: "no-such-endpoint",
                        remoteModel: FAKE_CHAT_MODEL,
                    })
                } catch (e) {
                    refused = `${e}`
                }
                expect(refused).to.include("no-such-endpoint")
            })

            it('refuses to remove an endpoint in use, unless asked to remove its models', async () => {
                const ad4mClient = testContext.ad4mClient!
                const other = await ad4mClient.ai.addEndpoint({
                    name: "fake-upstream-2",
                    kind: "OPEN_AI_AUDIO",
                    baseUrl: upstream.url.replace("127.0.0.1", "localhost"),
                })
                const id = await ad4mClient.ai.addModel({
                    name: "InUse",
                    modelType: "TRANSCRIPTION",
                    endpointId: other.id,
                    remoteModel: FAKE_STT_MODEL,
                })
                let refused = false
                try {
                    await ad4mClient.ai.removeEndpoint(other.id)
                } catch {
                    refused = true
                }
                expect(refused).to.be.true
                expect(await ad4mClient.ai.removeEndpoint(other.id, true)).to.be.true
                expect((await ad4mClient.ai.getModels()).find(m => m.id === id)).to.be.undefined
            })

            it('keeps a default per model type', async () => {
                const ad4mClient = testContext.ad4mClient!
                const llm = await ad4mClient.ai.addModel({
                    name: "DefaultChat", modelType: "LLM", endpointId: endpoint.id, remoteModel: FAKE_CHAT_MODEL,
                })
                const embedder = await ad4mClient.ai.addModel({
                    name: "DefaultEmbed", modelType: "EMBEDDING", endpointId: endpoint.id, remoteModel: FAKE_EMBED_MODEL,
                })
                expect(await ad4mClient.ai.setDefaultModel("LLM", llm)).to.be.true
                expect(await ad4mClient.ai.setDefaultModel("EMBEDDING", embedder)).to.be.true
                expect((await ad4mClient.ai.getDefaultModel("LLM"))?.id).to.equal(llm)
                expect((await ad4mClient.ai.getDefaultModel("EMBEDDING"))?.id).to.equal(embedder)

                await ad4mClient.ai.removeModel(llm)
                await ad4mClient.ai.removeModel(embedder)
                expect(await ad4mClient.ai.getDefaultModel("LLM")).to.be.null
            })

            it('prompts a task on its model, by id and as "default"', async () => {
                const ad4mClient = testContext.ad4mClient!
                const modelId = await ad4mClient.ai.addModel({
                    name: "TaskChat", modelType: "LLM", endpointId: endpoint.id, remoteModel: FAKE_CHAT_MODEL,
                })
                await ad4mClient.ai.setDefaultModel("LLM", modelId)

                const task = await ad4mClient.ai.addTask(
                    "echo-task",
                    modelId,
                    "Repeat what you hear.",
                    [{ input: "hi", output: "echo: hi" }]
                )
                expect(task.modelId).to.equal(modelId)
                expect((await ad4mClient.ai.tasks()).map(t => t.taskId)).to.include(task.taskId)

                upstream.requests.length = 0
                expect(await ad4mClient.ai.prompt(task.taskId, "hello there")).to.equal("echo: hello there")
                const sent = upstream.requests.find(r => r.path === "/v1/chat/completions")!.json
                expect(sent.model).to.equal(FAKE_CHAT_MODEL)
                expect(sent.messages).to.deep.equal([
                    { role: "system", content: "Repeat what you hear." },
                    { role: "user", content: "hi" },
                    { role: "assistant", content: "echo: hi" },
                    { role: "user", content: "hello there" },
                ])

                const onDefault = await ad4mClient.ai.addTask("default-task", "default", "Repeat.", [])
                expect(await ad4mClient.ai.prompt(onDefault.taskId, "again")).to.equal("echo: again")

                const updated = await ad4mClient.ai.updateTask(task.taskId, { ...task, systemPrompt: "Changed." })
                expect(updated.systemPrompt).to.equal("Changed.")

                expect(await ad4mClient.ai.removeTask(task.taskId)).to.be.true
                expect(await ad4mClient.ai.removeTask(onDefault.taskId)).to.be.true
                await ad4mClient.ai.removeModel(modelId)
            })

            it('embeds text with an embedding model, by id and by name', async () => {
                const ad4mClient = testContext.ad4mClient!
                const id = await ad4mClient.ai.addModel({
                    name: "TestEmbedder", modelType: "EMBEDDING", endpointId: endpoint.id, remoteModel: FAKE_EMBED_MODEL,
                })
                const byId = await ad4mClient.ai.embed(id, "Test string")
                const byName = await ad4mClient.ai.embed("TestEmbedder", "Test string")
                expect(byId).to.deep.equal(fakeVector("Test string"))
                expect(byName).to.deep.equal(byId)
                await ad4mClient.ai.removeModel(id)
            })

            it('transcribes each fed utterance on the transcription model\'s endpoint', async () => {
                const ad4mClient = testContext.ad4mClient!
                const id = await ad4mClient.ai.addModel({
                    name: "TestTranscriber", modelType: "TRANSCRIPTION", endpointId: endpoint.id, remoteModel: FAKE_STT_MODEL,
                })
                const texts: string[] = []
                const streamId = await ad4mClient.ai.openTranscriptionStream(id, (text) => texts.push(text))

                // One utterance: 1600 samples (0.1 s at 16 kHz), sent on as a 16-bit WAV.
                await ad4mClient.ai.feedTranscriptionStream(streamId, new Float32Array(1600).fill(0.1))
                await pollUntil(() => texts.length > 0, { timeoutMs: 10000, intervalMs: 200, label: "transcript" })
                expect(texts).to.have.length(1)
                expect(texts[0]).to.match(/^fake transcript of \d+ bytes$/)
                const upload = upstream.requests.find(r => r.path === "/v1/audio/transcriptions")!
                expect(upload.body.toString("latin1")).to.include(FAKE_STT_MODEL)

                await ad4mClient.ai.closeTranscriptionStream(streamId)
                await ad4mClient.ai.removeModel(id)
            })
        })
    }
}
