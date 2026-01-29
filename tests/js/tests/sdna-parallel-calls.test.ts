import { TestContext } from './integration.test'
import { expect } from "chai";
import { Ad4mModel, Flag, Property } from "@coasys/ad4m";

// Test subject classes to demonstrate parallel calls
class TestClass1 extends Ad4mModel {
    @Flag({
        through: "test://class1-flag",
        value: "test://class1-value",
    })
    flag?: boolean;

    @Property({
        through: "test://class1-property",
        writable: true,
        resolveLanguage: "literal",
    })
    property?: string;
}

class TestClass2 extends Ad4mModel {
    @Flag({
        through: "test://class2-flag",
        value: "test://class2-value",
    })
    flag?: boolean;
}

class TestClass3 extends Ad4mModel {
    @Property({
        through: "test://class3-property",
        writable: true,
        resolveLanguage: "literal",
    })
    property?: string;
}

class TestClass4 extends Ad4mModel {
    @Flag({
        through: "test://class4-flag",
        value: "test://class4-value",
    })
    flag?: boolean;
}

class TestClass5 extends Ad4mModel {
    @Property({
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
        });
    }
}
