import { TestContext } from './integration.test'
import { expect } from "chai";
import fs from 'fs';
//@ts-ignore
import ffmpeg from 'fluent-ffmpeg';
import { Readable } from 'stream';
import { ModelInput } from '@coasys/ad4m/lib/src/ai/AIResolver';

// Helper function to convert audio file to PCM data
async function convertAudioToPCM(audioFilePath: string): Promise<Float32Array> {
    const pcmData: Buffer = await new Promise((resolve, reject) => {
        const chunks: any[] = [];
        ffmpeg()
            .input(audioFilePath)
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

    return new Float32Array(pcmData.buffer, pcmData.byteOffset, pcmData.byteLength / Float32Array.BYTES_PER_ELEMENT);
}

// Helper function to stream audio data in chunks
async function streamAudioData(
    audioData: Float32Array,
    streamId: string,
    feedTranscriptionStream: (streamId: string, audio: number[]) => Promise<void>,
    chunkSize: number = 8000
): Promise<void> {
    for (let i = 0; i < audioData.length; i += chunkSize) {
        let end = i + chunkSize;
        if (end > audioData.length) {
            end = audioData.length;
        }
        console.log(`Sending chunk: ${i} - ${end}`);
        const chunk = audioData.slice(i, end);
        const numberArray = Array.from(chunk);  // Convert Float32Array to number[]
        await feedTranscriptionStream(streamId, numberArray);
        // Simulate real-time processing by adding a small delay
        await new Promise(resolve => setTimeout(resolve, 500));
    }
}

// Helper function to wait for transcription results
async function waitForTranscription<T>(
    condition: () => boolean,
    maxWaitSeconds: number = 60
): Promise<void> {
    let i = 0;
    while (!condition() && i < maxWaitSeconds) {
        await new Promise(resolve => setTimeout(resolve, 1000));
        i += 1;
    }
}

export default function aiTests(testContext: TestContext) {
    return () => {
        describe('AI service', () => {
            // This is used in the skipped tests below
            // They are skipped for CI, run on local device with GPU
            let testModelFileName: string = "llama_3_1_8b_chat"
            let testModelId: string = ""
            
            it("can perform Model CRUD operations", async () => {
                const ad4mClient = testContext.ad4mClient!

                // Test adding an API model
                const apiModelInput: ModelInput = {
                    name: "TestApiModel",
                    api: {
                        baseUrl: "https://api.example.com/",
                        apiKey: "test-api-key",
                        model: "llama",
                        apiType: "OPEN_AI"
                    },
                    modelType: "LLM"
                }

                const addApiResult = await ad4mClient.ai.addModel(apiModelInput)
                expect(addApiResult).to.be.a.string

                // Test adding a local model
                const localModelInput: ModelInput = {
                    name: "TestLocalModel",
                    local: {
                        fileName: "test_model.bin",
                        tokenizerSource: {
                            repo: "test-repo",
                            revision: "main",
                            fileName: "tokenizer.json"
                        },
                        huggingfaceRepo: "test-repo",
                        revision: "main"
                    },
                    modelType: "EMBEDDING"
                }

                const addLocalResult = await ad4mClient.ai.addModel(localModelInput)
                expect(addLocalResult).to.be.a.string

                // Test getting models
                const models = await ad4mClient.ai.getModels()
                expect(models).to.be.an('array')
                expect(models.length).to.be.at.least(2)
                
                const addedApiModel = models.find(model => model.name === "TestApiModel")
                expect(addedApiModel).to.exist
                expect(addedApiModel?.id).to.equal(addApiResult)
                expect(addedApiModel?.api?.baseUrl).to.equal("https://api.example.com/")
                expect(addedApiModel?.api?.apiKey).to.equal("test-api-key")
                expect(addedApiModel?.api?.model).to.equal("llama")
                expect(addedApiModel?.api?.apiType).to.equal("OPEN_AI")

                const addedLocalModel = models.find(model => model.name === "TestLocalModel")
                expect(addedLocalModel).to.exist
                expect(addedLocalModel?.id).to.equal(addLocalResult)
                expect(addedLocalModel?.local?.fileName).to.equal("test_model.bin")
                expect(addedLocalModel?.local?.tokenizerSource?.repo).to.equal("test-repo")
                expect(addedLocalModel?.local?.tokenizerSource?.revision).to.equal("main")
                expect(addedLocalModel?.local?.tokenizerSource?.fileName).to.equal("tokenizer.json")
                expect(addedLocalModel?.local?.huggingfaceRepo).to.equal("test-repo")
                expect(addedLocalModel?.local?.revision).to.equal("main")

                // Test removing models
                const removeApiResult = await ad4mClient.ai.removeModel(addedApiModel!.id)
                expect(removeApiResult).to.be.true

                const removeLocalResult = await ad4mClient.ai.removeModel(addedLocalModel!.id)
                expect(removeLocalResult).to.be.true

                // Verify the models were removed
                const updatedModels = await ad4mClient.ai.getModels()
                const removedApiModel = updatedModels.find(model => model.name === "TestApiModel")
                expect(removedApiModel).to.be.undefined

                const removedLocalModel = updatedModels.find(model => model.name === "TestLocalModel")
                expect(removedLocalModel).to.be.undefined
            })

            it('can update model', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create initial API model
                const initialModel: ModelInput = {
                    name: "TestUpdateModel",
                    api: {
                        baseUrl: "https://api.example.com/",
                        apiKey: "initial-key",
                        model: "llama",
                        apiType: "OPEN_AI"
                    },
                    modelType: "LLM"
                }

                // Add initial model
                const addResult = await ad4mClient.ai.addModel(initialModel)
                expect(addResult).to.be.a.string

                // Get the model to retrieve its ID
                const models = await ad4mClient.ai.getModels()
                const addedModel = models.find(model => model.name === "TestUpdateModel")
                expect(addedModel).to.exist

                // Create updated model data
                const bogusModelUrls: ModelInput = {
                    name: "UpdatedModel",
                    local: {
                        fileName: "updated_model.bin",
                        tokenizerSource: {
                            repo: "updated-repo",
                            revision: "main",
                            fileName: "updated_tokenizer.json"
                        },
                        huggingfaceRepo: "updated-repo",
                        revision: "main"
                    },
                    modelType: "EMBEDDING"
                }

                // Update the model
                let updateResult = false
                let error = {}
                try {
                    updateResult = await ad4mClient.ai.updateModel(addedModel!.id, bogusModelUrls)
                }catch(e) {
                    //@ts-ignore
                    error = e
                    console.log(error)
                }
                expect(updateResult).to.be.false
                expect(error).to.have.property('message')
                //@ts-ignore
                expect(error.message).to.include('Failed to update model')
                

                // Create updated model data
                const updatedModel: ModelInput = {
                    name: "UpdatedModel",
                    api: {
                        baseUrl: "https://api.example.com/v2",
                        apiKey: "updated-api-key", 
                        model: "gpt-4",
                        apiType: "OPEN_AI"
                    },
                    modelType: "LLM"
                }

                updateResult = await ad4mClient.ai.updateModel(addedModel!.id, updatedModel)
                
                expect(updateResult).to.be.true

                // Verify the update
                const updatedModels = await ad4mClient.ai.getModels()
                const retrievedModel = updatedModels.find(model => model.id === addedModel!.id)
                expect(retrievedModel).to.exist
                expect(retrievedModel?.name).to.equal("UpdatedModel")
                expect(retrievedModel?.local).to.be.null
                expect(retrievedModel?.api?.baseUrl).to.equal("https://api.example.com/v2")
                expect(retrievedModel?.api?.apiKey).to.equal("updated-api-key")
                expect(retrievedModel?.api?.model).to.equal("gpt-4")
                expect(retrievedModel?.api?.apiType).to.equal("OPEN_AI")
                expect(retrievedModel?.modelType).to.equal("LLM")

                // Clean up
                const removeResult = await ad4mClient.ai.removeModel(addedModel!.id)
                expect(removeResult).to.be.true
            })

            it.skip('can update model and verify it works', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create initial model
                const initialModel: ModelInput = {
                    name: "TestModel",
                    local: {
                        fileName: "llama_tiny_1_1b_chat"
                    },
                    modelType: "LLM"
                }

                // Add initial model
                const modelId = await ad4mClient.ai.addModel(initialModel)
                expect(modelId).to.be.a.string

                // Wait for model to be loaded
                let status;
                do {
                    status = await ad4mClient.ai.modelLoadingStatus(modelId);
                    await new Promise(resolve => setTimeout(resolve, 1000)); // Wait 1 second between checks
                } while (status.progress < 100);

                testModelId = modelId

                // Create task using "default" as model_id
                const task = await ad4mClient.ai.addTask(
                    "test-task",
                    modelId,
                    "You are a helpful assistant",
                    [{ input: "Say hi", output: "Hello!" }]
                )

                // Test that initial model works
                const prompt = "Say hello"
                const initialResponse = await ad4mClient.ai.prompt(task.taskId, prompt)
                expect(initialResponse).to.be.a.string
                expect(initialResponse.length).to.be.greaterThan(0)

                // Create updated model config
                const updatedModel: ModelInput = {
                    name: "UpdatedTestModel",
                    local: { fileName: testModelFileName },
                    modelType: "LLM"
                }

                // Update the model
                const updateResult = await ad4mClient.ai.updateModel(modelId, updatedModel)
                expect(updateResult).to.be.true

                // Wait for model to be loaded
                do {
                    status = await ad4mClient.ai.modelLoadingStatus(modelId);
                    await new Promise(resolve => setTimeout(resolve, 1000)); // Wait 1 second between checks
                } while (status.progress < 100);

                // Verify model was updated in DB
                const models = await ad4mClient.ai.getModels()
                const retrievedModel = models.find(m => m.id === modelId)
                expect(retrievedModel).to.exist
                expect(retrievedModel?.name).to.equal("UpdatedTestModel")
                expect(retrievedModel?.local?.fileName).to.equal(testModelFileName)

                // Test that updated model still works
                const updatedResponse = await ad4mClient.ai.prompt(task.taskId, prompt)
                expect(updatedResponse).to.be.a.string 
                expect(updatedResponse.length).to.be.greaterThan(0)

                // keep model around for other tests
            })

            it ('AI model status', async () => {
                const ad4mClient = testContext.ad4mClient!
                const status = await ad4mClient.ai.modelLoadingStatus("bert-id");
                expect(status).to.have.property('model');
                expect(status).to.have.property('status');
            })

            it('can set and get default model', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create test models first
                const apiModelInput: ModelInput = {
                    name: "TestDefaultApiModel",
                    api: {
                        baseUrl: "https://api.example.com/",
                        apiKey: "test-api-key",
                        model: "llama",
                        apiType: "OPEN_AI"
                    },
                    modelType: "LLM"
                }

                let id = await ad4mClient.ai.addModel(apiModelInput)

                // Set default model
                const setResult = await ad4mClient.ai.setDefaultModel("LLM", id)
                expect(setResult).to.be.true

                // Verify default model is set correctly
                const defaultModel = await ad4mClient.ai.getDefaultModel("LLM")
                expect(defaultModel.name).to.equal("TestDefaultApiModel")
                expect(defaultModel.api?.baseUrl).to.equal("https://api.example.com/")

                // Clean up
                await ad4mClient.ai.removeModel(id)
            })

            it.skip('can use "default" as model_id in tasks and prompting works', async () => {
                const ad4mClient = testContext.ad4mClient!
                await ad4mClient.ai.setDefaultModel("LLM", testModelId)

                // Create task using "default" as model_id
                const task = await ad4mClient.ai.addTask(
                    "default-model-task",
                    "default",
                    "You are a helpful assistant. Whatever you say, it will include 'hello'",
                    [{ input: "Say hi", output: "Hello!" }]
                )
                expect(task).to.have.property('taskId')
                expect(task.modelId).to.equal("default")

                // Test prompting works with the task
                const response = await ad4mClient.ai.prompt(task.taskId, "Say hi")
                expect(response).to.be.a('string')
                expect(response.toLowerCase()).to.include('hello')


                // Create another test model
                const newModelInput: ModelInput = {
                    name: "TestDefaultModel2",
                    local: { fileName: "llama_3_1_8b_chat" },
                    modelType: "LLM"
                }
                const newModelId = await ad4mClient.ai.addModel(newModelInput)

                // Wait for new model to be loaded
                let newModelStatus;
                do {
                    newModelStatus = await ad4mClient.ai.modelLoadingStatus(newModelId);
                    await new Promise(resolve => setTimeout(resolve, 1000));
                } while (newModelStatus.progress < 100);

                // Change default model to new one
                await ad4mClient.ai.setDefaultModel("LLM", newModelId)

                // Verify new default model is set
                const newDefaultModel = await ad4mClient.ai.getDefaultModel("LLM")
                expect(newDefaultModel.name).to.equal("TestDefaultModel2")

                // Test that prompting still works with the task using "default"
                const response2 = await ad4mClient.ai.prompt(task.taskId, "Say hi")
                expect(response2).to.be.a('string')
                expect(response2.toLowerCase()).to.include('hello')

                // Clean up
                await ad4mClient.ai.removeTask(task.taskId)
                await ad4mClient.ai.removeModel(newModelId)
            })

            it.skip('can do Tasks CRUD', async() => {
                const ad4mClient = testContext.ad4mClient!

                // Add a task
                const newTask = await ad4mClient.ai.addTask(
                    "test-name",
                    testModelId,
                    "This is a test system prompt",
                    [{ input: "Test input", output: "Test output" }]
                );
                expect(newTask).to.have.property('taskId');
                expect(newTask.name).to.equal('test-name');
                expect(newTask.modelId).to.equal(testModelId);
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
                    testModelId,
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
                    testModelId,
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

                let vector = await ad4mClient.ai.embed("bert", "Test string");
                expect(typeof vector).to.equal("object")
                expect(Array.isArray(vector)).to.be.true
                expect(vector.length).to.be.greaterThan(300)
            })

            it.skip('can do audio to text transcription', async() => {
                const ad4mClient = testContext.ad4mClient!;
                const audioData = await convertAudioToPCM('../transcription_test.m4a');
                let transcribedText = '';

                // These should be the default parameters
                const customParams = {
                    startThreshold: 0.3,
                    startWindow: 150,
                    endThreshold: 0.2,
                    endWindow: 300,
                    timeBeforeSpeech: 100
                };

                const streamId = await ad4mClient.ai.openTranscriptionStream("Whisper", (text) => {
                    console.log("Received transcription:", text);
                    transcribedText += text;
                }, customParams);

                // Type assertion for the feedTranscriptionStream function with unknown intermediate
                const feedStream = (ad4mClient.ai.feedTranscriptionStream.bind(ad4mClient.ai) as unknown) as (streamId: string, audio: number[]) => Promise<void>;
                await streamAudioData(audioData, streamId, feedStream);

                try {
                    await ad4mClient.ai.closeTranscriptionStream(streamId);
                } catch(e) {
                    console.log("Error trying to close TranscriptionStream:", e);
                }

                await waitForTranscription(() => transcribedText.length > 0);
                
                // Assertions
                expect(transcribedText).to.be.a('string');
                expect(transcribedText.length).to.be.greaterThan(0);
                expect(transcribedText).to.include("If you can read this, transcription is working.");
                console.log("Final transcription:", transcribedText);
            });

            it.skip('can do fast (word-by-word) audio transcription', async() => {
                const ad4mClient = testContext.ad4mClient!;
                const audioData = await convertAudioToPCM('../transcription_test.m4a');
                let transcribedWords: string[] = [];

                // Configure parameters for word-by-word detection
                // Use very short end window and low thresholds to separate words
                const wordByWordParams = {
                    startThreshold: 0.25,        // Lower threshold to detect softer speech
                    startWindow: 100,            // Quick start detection
                    endThreshold: 0.15,          // Lower threshold to detect end of words
                    endWindow: 100,             // Short pause between words (100ms)
                    timeBeforeSpeech: 20        // Include minimal context before speech
                };

                const streamId = await ad4mClient.ai.openTranscriptionStream("Whisper", (text) => {
                    console.log("Received word:", text);
                    if (text.trim()) {  // Only add non-empty text
                        transcribedWords.push(text.trim());
                    }
                }, wordByWordParams);

                // Type assertion for the feedTranscriptionStream function with unknown intermediate
                const feedStream = (ad4mClient.ai.feedTranscriptionStream.bind(ad4mClient.ai) as unknown) as (streamId: string, audio: number[]) => Promise<void>;
                await streamAudioData(audioData, streamId, feedStream);

                try {
                    await ad4mClient.ai.closeTranscriptionStream(streamId);
                } catch(e) {
                    console.log("Error trying to close TranscriptionStream:", e);
                }

                await waitForTranscription(() => transcribedWords.length > 0);
                
                // Assertions
                expect(transcribedWords).to.be.an('array');
                expect(transcribedWords.length).to.be.greaterThan(1);
                expect(transcribedWords.join(' ')).to.include("If you can read this, transcription is working");

                console.log("Transcribed words:", transcribedWords);
            });
        })
    }
}