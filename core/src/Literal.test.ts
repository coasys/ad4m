import { Literal } from './Literal'

describe("Literal", () => {
    it("can handle strings", () => {
        const testString = "test string"
        const testUrl = "literal://string:test%20string"
        expect(Literal.from(testString).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toBe(testString)
    })

    it("can handle numbers", () => {
        const testNumber = 3.1415
        const testUrl = "literal://number:3.1415"
        expect(Literal.from(testNumber).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toBe(testNumber)
    })

    it("can handle objects", () => {
        const testObject = {testString: "test", testNumber: "1337"}
        const testUrl = "literal://json:%7B%22testString%22:%22test%22,%22testNumber%22:%221337%22%7D"
        expect(Literal.from(testObject).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toStrictEqual(testObject)
    })
})