import { TestContext } from './integration.test'
import { expect } from "chai";
import fs from 'fs';
//@ts-ignore
import ffmpeg from 'fluent-ffmpeg';
import { Readable } from 'stream';
import { ModelInput } from '@coasys/ad4m/lib/src/ai/AIResolver';

export default function aiTests(testContext: TestContext) {
    return () => {
        describe('AI service', () => {
            it("can perform Model CRUD operations", async () => {
                const ad4mClient = testContext.ad4mClient!

                // Test adding an API model
                const apiModelInput: ModelInput = {
                    name: "TestApiModel",
                    api: {
                        baseUrl: "https://api.example.com/",
                        apiKey: "test-api-key",
                        apiType: "OPEN_AI"
                    },
                    type: "llm"
                }

                const addApiResult = await ad4mClient.ai.addModel(apiModelInput)
                expect(addApiResult).to.be.true

                // Test adding a local model
                const localModelInput: ModelInput = {
                    name: "TestLocalModel",
                    local: {
                        fileName: "test_model.bin",
                        tokenizerSource: "test_tokenizer.json",
                        modelParameters: JSON.stringify({ param1: "value1", param2: "value2" })
                    },
                    type: "embeding"
                }

                const addLocalResult = await ad4mClient.ai.addModel(localModelInput)
                expect(addLocalResult).to.be.true

                // Test getting models
                const models = await ad4mClient.ai.getModels()
                expect(models).to.be.an('array')
                expect(models.length).to.be.at.least(2)
                
                const addedApiModel = models.find(model => model.name === "TestApiModel")
                expect(addedApiModel).to.exist
                expect(addedApiModel?.api?.baseUrl).to.equal("https://api.example.com/")
                expect(addedApiModel?.api?.apiKey).to.equal("test-api-key")
                expect(addedApiModel?.api?.apiType).to.equal("OPEN_AI")

                const addedLocalModel = models.find(model => model.name === "TestLocalModel")
                expect(addedLocalModel).to.exist
                expect(addedLocalModel?.local?.fileName).to.equal("test_model.bin")
                expect(addedLocalModel?.local?.tokenizerSource).to.equal("test_tokenizer.json")
                expect(addedLocalModel?.local?.modelParameters).to.deep.equal(JSON.stringify({ param1: "value1", param2: "value2" }))

                // Test removing models
                const removeApiResult = await ad4mClient.ai.removeModel("TestApiModel")
                expect(removeApiResult).to.be.true

                const removeLocalResult = await ad4mClient.ai.removeModel("TestLocalModel")
                expect(removeLocalResult).to.be.true

                // Verify the models were removed
                const updatedModels = await ad4mClient.ai.getModels()
                const removedApiModel = updatedModels.find(model => model.name === "TestApiModel")
                expect(removedApiModel).to.be.undefined

                const removedLocalModel = updatedModels.find(model => model.name === "TestLocalModel")
                expect(removedLocalModel).to.be.undefined
            })

            it ('AI model status', async () => {
                const ad4mClient = testContext.ad4mClient!
                const status = await ad4mClient.ai.modelLoadingStatus("bert");
                console.log("MODEL STATUS:", status);
                expect(status).to.have.property('model');
                expect(status).to.have.property('status');
            })

            it.skip('can do Tasks CRUD', async() => {
                const ad4mClient = testContext.ad4mClient!

                // Add a task
                const newTask = await ad4mClient.ai.addTask(
                    "test-name",
                    "llama",
                    "This is a test system prompt",
                    [{ input: "Test input", output: "Test output" }]
                );
                expect(newTask).to.have.property('taskId');
                expect(newTask.name).to.equal('test-name');
                expect(newTask.modelId).to.equal("llama");
                expect(newTask.systemPrompt).to.equal("This is a test system prompt");
                expect(newTask.promptExamples).to.deep.equal([{ input: "Test input", output: "Test output" }]);

                // Get all tasks
                const tasks = await ad4mClient.ai.tasks();
                expect(tasks).to.be.an('array');
                expect(tasks).to.have.lengthOf.at.least(1);
                expect(tasks.find(task => task.taskId === newTask.taskId)).to.deep.equal(newTask);

                // Update a task
                const updatedTask = await ad4mClient.ai.updateTask(newTask.taskId, {
                    ...newTask,
                    systemPrompt: "Updated system prompt",
                    promptExamples: [{ input: "Updated input", output: "Updated output" }]
                });
                expect(updatedTask.taskId).to.equal(newTask.taskId);
                expect(updatedTask.systemPrompt).to.equal("Updated system prompt");
                expect(updatedTask.promptExamples).to.deep.equal([{ input: "Updated input", output: "Updated output" }]);

                // Remove a task
                const removedTask = await ad4mClient.ai.removeTask(newTask.taskId);
                expect(removedTask).to.deep.equal(updatedTask);

                // Verify task is removed
                const tasksAfterRemoval = await ad4mClient.ai.tasks();
                expect(tasksAfterRemoval.find(task => task.taskId === newTask.taskId)).to.be.undefined;
            }).timeout(900000)

            it.skip('can prompt a task', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create a new task
                const newTask = await ad4mClient.ai.addTask(
                    "test-name",
                    "llama",
                    "You are inside a test. Please ALWAYS respond with 'works', plus something else.",
                    [
                        { input: "What's the capital of France?", output: "works. Also that is Paris" },
                        { input: "What's the largets planet in our solar system?", output: "works. That is Jupiter." }

                    ]
                );

                expect(newTask).to.have.property('taskId');

                // Prompt the task
                const promptResult = await ad4mClient.ai.prompt(newTask.taskId, "What's the largest planet in our solar system?");

                console.log("PROMPT RESULT:", promptResult)
                // Check if the result is a non-empty string
                expect(promptResult).to.be.a('string');
                expect(promptResult.length).to.be.greaterThan(0);

                // Check if the result mentions Jupiter
                expect(promptResult.toLowerCase()).to.include('works');

                // Clean up: remove the task
                await ad4mClient.ai.removeTask(newTask.taskId);
            }).timeout(900000)

            it.skip('can prompt several tasks in a row fast', async () => {
                const ad4mClient = testContext.ad4mClient!

                console.log("test 1");

                // Create a new task
                const newTask = await ad4mClient.ai.addTask(
                    "test-name",
                    "llama",
                    "You are inside a test. Please respond with a short, unique message each time.",
                    [
                        { input: "Test long 1", output: "This is a much longer response that includes various details. It talks about the weather being sunny, the importance of staying hydrated, and even mentions a recipe for chocolate chip cookies. The response goes on to discuss the benefits of regular exercise, the plot of a popular novel, and concludes with a fun fact about the migration patterns of monarch butterflies." },
                        { input: "Test long 2", output: "This is another much longer response that delves into various topics. It begins by discussing the intricate process of photosynthesis in plants, then transitions to the history of ancient civilizations, touching on the rise and fall of the Roman Empire. The response continues with an explanation of quantum mechanics and its implications for our understanding of the universe. It then explores the evolution of human language, the impact of climate change on global ecosystems, and the potential for artificial intelligence to revolutionize healthcare. The response concludes with a brief overview of the cultural significance of tea ceremonies in different parts of the world." },
                        { input: "Test long 3", output: "This extensive response covers a wide range of subjects, starting with an in-depth analysis of sustainable urban planning and its impact on modern cities. It then shifts to discuss the evolution of musical instruments throughout history, touching on the development of the piano, guitar, and electronic synthesizers. The text continues with an exploration of the human immune system, detailing how it fights off pathogens and the importance of vaccinations. Next, it delves into the world of astronomy, describing the life cycle of stars and the formation of galaxies. The response also includes a section on the history of cryptography, from ancient ciphers to modern encryption algorithms used in digital security. It concludes with a discussion on the philosophy of ethics, examining various moral frameworks and their applications in contemporary society." },
                    ]
                );

                console.log("test 2");

                expect(newTask).to.have.property('taskId');

                // Create an array of 10 prompts
                const prompts = Array.from({ length: 1 }, (_, i) => `This is a much longer test prompt number ${i + 1}. It includes various details to make it more substantial. For instance, it mentions that the sky is blue, grass is green, and water is essential for life. It also touches on the fact that technology is rapidly advancing, climate change is a global concern, and education is crucial for personal growth. Additionally, it notes that music can evoke powerful emotions, reading broadens the mind, and exercise is important for maintaining good health. Lastly, it states that kindness can make a significant difference in someone's day.`);

                console.log("test 3");

                // Run 10 prompts simultaneously
                const promptResults = await Promise.all(
                    prompts.map(prompt => ad4mClient.ai.prompt(newTask.taskId, prompt))
                );

                console.log("test 4", promptResults);

                // Check results
                promptResults.forEach((result, index) => {
                    expect(result).to.be.a('string');
                    expect(result.length).to.be.greaterThan(0);
                    console.log(`Prompt ${index + 1} result:`, result);
                });

                console.log("test 5");

                // Clean up: remove the task
                await ad4mClient.ai.removeTask(newTask.taskId);

                console.log("test 6");
            })

            it('can embed text to vectors', async () => {
                const ad4mClient = testContext.ad4mClient!

                let vector = await ad4mClient.ai.embed("Bert", "Test string");
                expect(typeof vector).to.equal("object")
                expect(Array.isArray(vector)).to.be.true
                expect(vector.length).to.be.greaterThan(300)
            })

            it('can do audio to text transcription', async() => {
                const ad4mClient = testContext.ad4mClient!;

                // Convert m4a to raw PCM data
                const pcmData: Buffer = await new Promise((resolve, reject) => {
                    const chunks: Buffer[] = [];
                    ffmpeg()
                        .input('../transcription_test.m4a')
                        .inputFormat('m4a')
                        .toFormat('f32le')
                        .audioFrequency(16000)
                        .audioChannels(1)
                        .on('error', reject)
                        .pipe()
                        .on('data', (chunk: any) => {
                            chunks.push(chunk)
                        })
                        .on('end', () => {
                            const finalBuffer = Buffer.concat(chunks);
                            console.log("Total PCM data size:", finalBuffer.length);
                            resolve(finalBuffer);
                        })
                        .on('error', reject);
                });

                //console.log("PCM DATA:", pcmData.buffer, pcmData.byteOffset, pcmData.length, pcmData.length / 4)
                // Convert PCM buffer to Float32Array
                const result = new Float32Array(pcmData.buffer, pcmData.byteOffset, pcmData.byteLength / Float32Array.BYTES_PER_ELEMENT);
                //console.log("RESULT:", result);

                //@ts-ignore
                const audioData = result// result.channelData[0]; // Assuming mono audio

                //console.log("AUDIO DATA:", audioData.length);

                // Open the transcription stream
                let transcribedText = '';
                const streamId = await ad4mClient.ai.openTranscriptionStream("Whisper", (text) => {
                    console.log("Received transcription:", text);
                    transcribedText += text;
                });

                // Define chunk size (e.g., 0.5 seconds of audio at 16000 Hz sample rate)
                const chunkSize = 8000; // 16000 * 0.5

                // Stream the audio data in chunks
                for (let i = 0; i < audioData.length; i += chunkSize) {
                    let end = i+chunkSize
                    if(end > audioData.length) {
                        end = audioData.length
                    }
                    console.log(`Sending chunk: ${i} - ${end}`)
                    const chunk = audioData.slice(i, end);
                    const floatArray = Array.from(chunk).map(x=> !x?0.0:x)
                    //@ts-ignore
                    await ad4mClient.ai.feedTranscriptionStream(streamId, floatArray);
                    //console.log(floatArray)

                    // Simulate real-time processing by adding a small delay
                    await new Promise(resolve => setTimeout(resolve, 500));
                }

                // Close the transcription stream
                try {
                    await ad4mClient.ai.closeTranscriptionStream(streamId);
                }catch(e) {
                    console.log("Error trying to close TranscriptionStream:", e)
                }

                let i=0
                while(transcribedText.length == 0 && i < 60){
                    await new Promise(resolve => setTimeout(resolve, 1000));    
                    i+=1
                }
                
                // Assertions
                expect(transcribedText).to.be.a('string');
                expect(transcribedText.length).to.be.greaterThan(0);
                expect(transcribedText).to.include("If you can read this, transcription is working.")
                console.log("Final transcription:", transcribedText);
            })
        })
    }
}