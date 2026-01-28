import { TestContext } from './integration.test'
import { expect } from "chai";
import { SDNAClass, SubjectEntity, SubjectFlag, SubjectProperty, sdna } from "@coasys/ad4m";

// Test subject classes to demonstrate parallel calls
@SDNAClass({
    name: "TestClass1",
})
class TestClass1 {
    @SubjectFlag({
        through: "test://class1-flag",
        value: "test://class1-value",
    })
    flag?: boolean;

    @SubjectProperty({
        through: "test://class1-property",
        writable: true,
        resolveLanguage: "literal",
    })
    property?: string;
}

@SDNAClass({
    name: "TestClass2",
})
class TestClass2 {
    @SubjectFlag({
        through: "test://class2-flag",
        value: "test://class2-value",
    })
    flag?: boolean;
}

@SDNAClass({
    name: "TestClass3",
})
class TestClass3 {
    @SubjectProperty({
        through: "test://class3-property",
        writable: true,
        resolveLanguage: "literal",
    })
    property?: string;
}

@SDNAClass({
    name: "TestClass4",
})
class TestClass4 {
    @SubjectFlag({
        through: "test://class4-flag",
        value: "test://class4-value",
    })
    flag?: boolean;
}

@SDNAClass({
    name: "TestClass5",
})
class TestClass5 {
    @SubjectProperty({
        through: "test://class5-property",
        writable: true,
        resolveLanguage: "literal",
    })
    property?: string;
}

export default function sdnaParallelCallsTests(testContext: TestContext) {
    return () => {
        describe("SDNA Parallel Calls (Regression Test)", () => {
            it("should handle multiple ensureSDNASubjectClass calls in parallel without race conditions", async function() {
                this.timeout(30000);

                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add("sdna-parallel-test");

                console.log("Testing parallel ensureSDNASubjectClass calls...");
                
                const startTime = Date.now();

                try {
                    // Multiple concurrent calls should be deduplicated via promise cache
                    await Promise.all([
                        perspective.ensureSDNASubjectClass(TestClass1),
                        perspective.ensureSDNASubjectClass(TestClass2),
                        perspective.ensureSDNASubjectClass(TestClass3),
                        perspective.ensureSDNASubjectClass(TestClass4),
                        perspective.ensureSDNASubjectClass(TestClass5),
                    ]);

                    const elapsed = Date.now() - startTime;
                    console.log(`Parallel calls completed in ${elapsed}ms`);

                    // Should complete efficiently without race conditions
                    expect(elapsed).to.be.lessThan(15000); // Should complete within 15s
                } catch (error) {
                    const elapsed = Date.now() - startTime;
                    console.error(`Parallel calls failed after ${elapsed}ms:`, error);
                    throw error;
                }
            });

            it("should work with sequential ensureSDNASubjectClass calls (current workaround)", async function() {
                this.timeout(30000);

                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add("sdna-sequential-test");

                console.log("Testing sequential ensureSDNASubjectClass calls...");
                
                const startTime = Date.now();

                // This WORKS - sequential execution
                await perspective.ensureSDNASubjectClass(TestClass1);
                await perspective.ensureSDNASubjectClass(TestClass2);
                await perspective.ensureSDNASubjectClass(TestClass3);
                await perspective.ensureSDNASubjectClass(TestClass4);
                await perspective.ensureSDNASubjectClass(TestClass5);

                const elapsed = Date.now() - startTime;
                console.log(`Sequential calls completed in ${elapsed}ms`);

                // Sequential should work reliably
                expect(elapsed).to.be.lessThan(15000); // Should complete within 15s
            });

            it.skip("should use batch API when implemented (ensureSDNASubjectClasses)", async function() {
                this.timeout(30000);

                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add("sdna-batch-test");

                console.log("Testing batch ensureSDNASubjectClasses API...");
                
                const startTime = Date.now();

                // Future batch API - will be implemented
                // @ts-ignore - API doesn't exist yet
                await perspective.ensureSDNASubjectClasses([
                    TestClass1,
                    TestClass2,
                    TestClass3,
                    TestClass4,
                    TestClass5,
                ]);

                const elapsed = Date.now() - startTime;
                console.log(`Batch API completed in ${elapsed}ms`);

                // Batch API should be most efficient
                expect(elapsed).to.be.lessThan(10000); // Should complete within 10s
            });
        });
    }
}
