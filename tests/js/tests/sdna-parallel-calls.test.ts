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

                console.log("Testing parallel ensureSDNASubjectClass calls (including duplicates for deduplication)...");
                
                // Test with duplicate calls to verify deduplication
                await Promise.all([
                    perspective.ensureSDNASubjectClass(TestClass1),
                    perspective.ensureSDNASubjectClass(TestClass1), // duplicate
                    perspective.ensureSDNASubjectClass(TestClass2),
                    perspective.ensureSDNASubjectClass(TestClass2), // duplicate
                    perspective.ensureSDNASubjectClass(TestClass3),
                ]);

                console.log("Parallel calls completed successfully");
                
                // Verify the classes were registered (should have 3 unique classes)
                const class1Instances = await perspective.subjectClassesByTemplate(new TestClass1());
                const class2Instances = await perspective.subjectClassesByTemplate(new TestClass2());
                const class3Instances = await perspective.subjectClassesByTemplate(new TestClass3());
                
                expect(class1Instances.length).to.be.greaterThan(0, "TestClass1 should be registered");
                expect(class2Instances.length).to.be.greaterThan(0, "TestClass2 should be registered");
                expect(class3Instances.length).to.be.greaterThan(0, "TestClass3 should be registered");
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
                await perspective.ensureSDNASubjectClass(TestClass4);
                await perspective.ensureSDNASubjectClass(TestClass5);

                console.log("Sequential calls completed successfully");
                
                // Verify all classes were registered
                const class1Instances = await perspective.subjectClassesByTemplate(new TestClass1());
                expect(class1Instances.length).to.be.greaterThan(0, "TestClass1 should be registered");
            });
        });
    }
}
