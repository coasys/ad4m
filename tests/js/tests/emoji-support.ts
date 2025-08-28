import { expect } from "chai";
import { Ad4mClient, Literal, Link, LinkQuery } from "@coasys/ad4m";
import { TestContext } from "./integration.test";

/**
 * Emoji and Special Character Support Tests
 * 
 * This test suite validates the complete emoji/UTF-8 character handling pipeline:
 * 1. URL encoding/decoding of emoji characters in literal URLs
 * 2. Perspective data storage and retrieval with UTF-8 content
 * 3. Prolog query integration for properties containing emojis
 * 4. Edge cases and error handling for malformed UTF-8 sequences
 * 5. Regression tests for the original bug (undefined property values)
 */

export default function emojiSupportTests(testContext: TestContext) {
    return () => {
        describe('Literal URL Encoding/Decoding', () => {
            it('should handle basic emoji in literal URLs', () => {
                const emojiText = '👋 Hello World! 🎉';
                const literal = Literal.from(emojiText);
                const url = literal.toUrl();
                
                // Verify URL contains percent-encoded UTF-8 bytes
                expect(url).to.contain('%F0%9F%91%8B'); // 👋 emoji
                expect(url).to.contain('%F0%9F%8E%89'); // 🎉 emoji
                
                // Verify we can decode back
                const decodedLiteral = Literal.fromUrl(url);
                expect(decodedLiteral.get()).to.equal(emojiText);
            });

            it('should handle HTML with emojis in literal URLs', () => {
                const htmlWithEmoji = '<p>👋</p>';
                const literal = Literal.from(htmlWithEmoji);
                const url = literal.toUrl();
                
                // Verify URL encoding - this is the exact case from the original bug report
                expect(url).to.contain('%3Cp%3E'); // <p>
                expect(url).to.contain('%F0%9F%91%8B'); // 👋
                expect(url).to.contain('%3C%2Fp%3E'); // </p>
                
                // Verify we can decode back
                const decodedLiteral = Literal.fromUrl(url);
                expect(decodedLiteral.get()).to.equal(htmlWithEmoji);
            });

            it('should handle complex UTF-8 characters', () => {
                const complexText = '🌍 Hello 世界 🚀 Testing éñ çharacters';
                const literal = Literal.from(complexText);
                const url = literal.toUrl();
                
                // Verify we can round-trip the complex text
                const decodedLiteral = Literal.fromUrl(url);
                expect(decodedLiteral.get()).to.equal(complexText);
            });
        });

        describe('Perspective Data with Emojis', () => {
            it('should store and retrieve emoji messages in perspectives', async function() {
                this.timeout(120000);
                
                const emojiMessage = '<p>👋</p>';
                const literal = Literal.from(emojiMessage);
                
                // Create expression with emoji content
                const expression = await testContext.alice.expression.create(literal, 'literal');
                expect(expression).to.not.be.undefined;
                
                // Create a test perspective
                const perspective = await testContext.alice.perspective.add('Emoji Test Perspective');
                
                // Add link with emoji content
                const link = new Link({
                    source: 'test://emoji-test',
                    target: expression,
                    predicate: 'test://has-message'
                });
                
                await perspective.add(link);
                
                // Query back the links
                const links = await perspective.get({
                    source: 'test://emoji-test'
                } as LinkQuery);
                
                expect(links).to.have.length(1);
                expect(links[0].target).to.equal(expression);
                
                // Retrieve the expression and verify emoji content
                const retrievedExpression = await testContext.alice.expression.get(links[0].target);
                expect(retrievedExpression.data).to.equal(emojiMessage);
                
                // Clean up
                await testContext.alice.perspective.remove(perspective.uuid);
            });

            it('should handle emoji in JSON literal data', async function() {
                this.timeout(120000);
                
                const jsonWithEmoji = {
                    message: "Hello 👋",
                    reaction: "🎉",
                    text: "<p>Welcome! 🚀</p>"
                };
                
                const literal = Literal.from(JSON.stringify(jsonWithEmoji));
                const expression = await testContext.alice.expression.create(literal, 'literal');
                
                // Retrieve and parse
                const retrievedExpression = await testContext.alice.expression.get(expression);
                const parsedData = JSON.parse(retrievedExpression.data);
                
                expect(parsedData.message).to.equal("Hello 👋");
                expect(parsedData.reaction).to.equal("🎉");
                expect(parsedData.text).to.equal("<p>Welcome! 🚀</p>");
            });
        });

        describe('Prolog Integration with Emojis', () => {
            it('should handle emoji properties in Prolog queries', async function() {
                this.timeout(120000);
                
                // This tests the original bug case where emoji properties returned undefined
                const emojiProperty = '<p>👋</p>';
                const literal = Literal.from(emojiProperty);
                const expression = await testContext.alice.expression.create(literal, 'literal');
                
                // Create a perspective and add data
                const perspective = await testContext.alice.perspective.add('Prolog Emoji Test');
                
                const link = new Link({
                    source: 'test://subject',
                    target: expression,
                    predicate: 'test://message-property'
                });
                
                await perspective.add(link);
                
                // Query using Prolog-style queries
                const links = await perspective.get({
                    source: 'test://subject',
                    predicate: 'test://message-property'
                } as LinkQuery);
                
                expect(links).to.have.length(1);
                
                // Verify the target expression is not undefined (the original bug)
                expect(links[0].target).to.not.be.undefined;
                
                // Verify we can retrieve the emoji content
                const retrievedExpression = await testContext.alice.expression.get(links[0].target);
                expect(retrievedExpression.data).to.equal(emojiProperty);
                
                // Clean up
                await testContext.alice.perspective.remove(perspective.uuid);
            });
        });

        describe('Edge Cases and Error Handling', () => {
            it('should handle empty strings and null values', () => {
                const emptyLiteral = Literal.from('');
                const url = emptyLiteral.toUrl();
                const decoded = Literal.fromUrl(url);
                expect(decoded.get()).to.equal('');
            });

            it('should handle very long emoji sequences', () => {
                const longEmojiText = '🎉'.repeat(100) + ' Test Message ' + '👋'.repeat(50);
                const literal = Literal.from(longEmojiText);
                const url = literal.toUrl();
                const decoded = Literal.fromUrl(url);
                expect(decoded.get()).to.equal(longEmojiText);
            });

            it('should handle mixed content with special characters', () => {
                const mixedContent = `
                    <div>
                        <h1>Welcome! 👋</h1>
                        <p>Special chars: & < > " ' 🎉</p>
                        <code>console.log("Hello 🌍");</code>
                    </div>
                `;
                const literal = Literal.from(mixedContent);
                const url = literal.toUrl();
                const decoded = Literal.fromUrl(url);
                expect(decoded.get()).to.equal(mixedContent);
            });
        });

        describe('Regression Tests', () => {
            it('should not return undefined for emoji properties (original bug)', async function() {
                this.timeout(120000);
                
                // Test the exact scenario from the original bug report
                const problematicProperty = '<p>👋</p>';
                const literal = Literal.from(problematicProperty);
                
                // Create expression
                const expression = await testContext.alice.expression.create(literal, 'literal');
                
                // The bug was that this would return undefined
                expect(expression).to.not.be.undefined;
                expect(expression).to.be.a('string');
                
                // Verify we can retrieve the expression
                const retrievedExpression = await testContext.alice.expression.get(expression);
                expect(retrievedExpression).to.not.be.undefined;
                expect(retrievedExpression.data).to.equal(problematicProperty);
            });

            it('should handle URL encoding edge cases that caused infinite loops', () => {
                // Test cases that previously caused Prolog infinite loops
                const edgeCases = [
                    '👋', // Single emoji
                    '<p>👋</p>', // HTML with emoji
                    '%F0%9F%91%8B', // Pre-encoded UTF-8 bytes
                    '🌍🚀🎉👋', // Multiple emojis
                ];
                
                edgeCases.forEach((testCase, index) => {
                    const literal = Literal.from(testCase);
                    const url = literal.toUrl();
                    const decoded = Literal.fromUrl(url);
                    expect(decoded.get()).to.equal(testCase, `Failed for case ${index}: ${testCase}`);
                });
            });
        });

        describe('Performance Tests', () => {
            it('should handle large emoji datasets efficiently', function() {
                this.timeout(10000);
                
                // Create a large dataset with emojis
                const largeDataset = Array.from({ length: 1000 }, (_, i) => ({
                    id: i,
                    message: `Message ${i} with emoji 👋`,
                    reaction: i % 2 === 0 ? '🎉' : '🚀'
                }));
                
                const literal = Literal.from(JSON.stringify(largeDataset));
                const url = literal.toUrl();
                const decoded = Literal.fromUrl(url);
                const parsedData = JSON.parse(decoded.get());
                
                expect(parsedData).to.have.length(1000);
                expect(parsedData[0].message).to.contain('👋');
            });
        });
    };
}
