import { TestContext } from './integration.test'
import { expect } from "chai";
import { Ad4mModel, Flag, Optional, ModelOptions } from "@coasys/ad4m";

// Test subject classes to demonstrate parallel calls
@ModelOptions({
    name: "TestClass1"
})
class TestClass1 extends Ad4mModel {
    @Flag({
        through: "ad4m://type",
        value: "test://class1",
    })
    type: string = "";

    @Optional({
        through: "test://class1-property",
        resolveLanguage: "literal",
    })
    property: string = "";
}

@ModelOptions({
    name: "TestClass2"
})
class TestClass2 extends Ad4mModel {
    @Flag({
        through: "ad4m://type",
        value: "test://class2",
    })
    type: string = "";
}

@ModelOptions({
    name: "TestClass3"
})
class TestClass3 extends Ad4mModel {
    @Flag({
        through: "ad4m://type",
        value: "test://class3",
    })
    type: string = "";

    @Optional({
        through: "test://class3-property",
        resolveLanguage: "literal",
    })
    property: string = "";
}

export default function sdnaParallelCallsTests(testContext: TestContext) {
    return () => {
        describe("SDNA Parallel Calls (Regression Test)", () => {
            it("should handle multiple ensureSDNASubjectClass calls in parallel without race conditions", async function() {
                this.timeout(30000);

                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add("sdna-parallel-test");

                console.log("Testing parallel ensureSDNASubjectClass calls (including duplicates for deduplication)...");
                
                // Test with duplicate calls to verify deduplication
                await Promise.all([
                    perspective.ensureSDNASubjectClass(TestClass1),
                    perspective.ensureSDNASubjectClass(TestClass1), // duplicate - should be deduplicated
                    perspective.ensureSDNASubjectClass(TestClass2),
                    perspective.ensureSDNASubjectClass(TestClass3),
                    perspective.ensureSDNASubjectClass(TestClass1), // another duplicate
                ]);

                console.log("Parallel calls completed successfully");
                
                // Verify the classes were registered (should have 3 unique classes)
                const allSdna = await perspective.getSdna();
                console.log(`DEBUG: Got ${allSdna.length} SDNA entries:`, allSdna.map(s => s.name));
                expect(allSdna.length).to.equal(4, "Should have 4 SDNA entries (1 base + 3 test classes)");
                
                // Verify we can find instances by template
                const class1Instances = await perspective.subjectClassesByTemplate(new TestClass1(perspective));
                const class2Instances = await perspective.subjectClassesByTemplate(new TestClass2(perspective));
                const class3Instances = await perspective.subjectClassesByTemplate(new TestClass3(perspective));
                
                expect(class1Instances).to.include("TestClass1", "TestClass1 should be registered");
                expect(class2Instances).to.include("TestClass2", "TestClass2 should be registered");
                expect(class3Instances).to.include("TestClass3", "TestClass3 should be registered");
            });

            it("should work with sequential ensureSDNASubjectClass calls", async function() {
                this.timeout(30000);

                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add("sdna-sequential-test");

                console.log("Testing sequential ensureSDNASubjectClass calls...");
                
                // Sequential execution
                await perspective.ensureSDNASubjectClass(TestClass1);
                await perspective.ensureSDNASubjectClass(TestClass2);
                await perspective.ensureSDNASubjectClass(TestClass3);

                console.log("Sequential calls completed successfully");
                
                // Verify all classes were registered
                const allSdna = await perspective.getSdna();
                console.log(`DEBUG: Got ${allSdna.length} SDNA entries:`, allSdna.map(s => s.name));
                expect(allSdna.length).to.equal(4, "Should have 4 SDNA entries");
                
                const class1Instances = await perspective.subjectClassesByTemplate(new TestClass1(perspective));
                expect(class1Instances).to.include("TestClass1", "TestClass1 should be registered");
            });
        });
    }
}
