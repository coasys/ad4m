import { TestContext } from './integration.test'
import { expect } from "chai";

export default function aiTests(testContext: TestContext) {
    return () => {
        describe('AI service', () => {
            it('can do Tasks CRUD', async() => {
                const ad4mClient = testContext.ad4mClient!

                // Add a task
                const newTask = await ad4mClient.ai.addTask(
                    "test-model",
                    "This is a test system prompt",
                    [{ input: "Test input", output: "Test output" }]
                );
                expect(newTask).to.have.property('taskId');
                expect(newTask.modelId).to.equal("test-model");
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
            })

            it('can prompt a task', async () => {
                const ad4mClient = testContext.ad4mClient!

                // Create a new task
                const newTask = await ad4mClient.ai.addTask(
                    "test-model",
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
            })

            it('can embed text to vectors', async () => {
                const ad4mClient = testContext.ad4mClient!

                let vector = await ad4mClient.ai.embed("Bert", "Test string");
                expect(typeof vector).to.equal("object")
                expect(Array.isArray(vector)).to.be.true
                expect(vector.length).to.be.greaterThan(300)
            })
        })
    }
}