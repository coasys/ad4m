import { Literal } from './Literal'

describe("Literal", () => {
    it("can handle strings", () => {
        const testString = "test string"
        const testUrl = "literal:string:test%20string"
        expect(Literal.from(testString).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toBe(testString)
    })

    it("can handle numbers", () => {
        const testNumber = 3.1415
        const testUrl = "literal:number:3.1415"
        expect(Literal.from(testNumber).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toBe(testNumber)
    })

    it("can handle objects", () => {
        const testObject = {testNumber: "1337", testString: "test"}
        const testUrl = "literal:json:%7B%22testNumber%22%3A%221337%22%2C%22testString%22%3A%22test%22%7D"
        expect(Literal.from(testObject).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toStrictEqual(testObject)
    })

    it("can handle special characters", () => {
        const testString = "message(X) :- triple('ad4m://self', _, X)."
        const testUrl = "literal:string:message%28X%29%20%3A-%20triple%28%27ad4m%3A%2F%2Fself%27%2C%20_%2C%20X%29."
        expect(Literal.from(testString).toUrl()).toBe(testUrl)
        expect(Literal.fromUrl(testUrl).get()).toBe(testString)
    })

    it("rejects legacy literal:// URLs", () => {
        expect(() => Literal.fromUrl("literal://string:hello")).toThrow("literal:// format is no longer supported")
        expect(() => Literal.fromUrl("literal://number:42")).toThrow("literal:// format is no longer supported")
    })

    describe("falsy values", () => {
        it("get() returns 0, false, \"\", null and NaN set with from()", () => {
            expect(Literal.from(0).get()).toBe(0)
            expect(Literal.from(false).get()).toBe(false)
            expect(Literal.from("").get()).toBe("")
            expect(Literal.from(null).get()).toBe(null)
            expect(Literal.from(NaN).get()).toBeNaN()
        })

        it("round-trips 0, false, null and NaN through from -> toUrl -> fromUrl -> get", () => {
            const roundTrip = (v: any) => Literal.fromUrl(Literal.from(v).toUrl()).get()
            expect(Literal.from(0).toUrl()).toBe("literal:number:0")
            expect(roundTrip(0)).toBe(0)
            expect(Literal.from(false).toUrl()).toBe("literal:boolean:false")
            expect(roundTrip(false)).toBe(false)
            expect(Literal.from(NaN).toUrl()).toBe("literal:number:NaN")
            expect(roundTrip(NaN)).toBeNaN()
            expect(Literal.fromUrl("literal:json:null").get()).toBe(null)
        })

        it("encodes \"\" and null as URLs and round-trips them", () => {
            expect(Literal.from("").toUrl()).toBe("literal:string:")
            expect(Literal.from(null).toUrl()).toBe("literal:json:null")
            for (const value of ["", null]) {
                expect(Literal.fromUrl(Literal.from(value).toUrl()).get()).toBe(value)
            }
        })

        it("still refuses to encode undefined as a URL", () => {
            expect(() => Literal.from(undefined).toUrl()).toThrow("Can't turn empty Literal into URL")
        })

        it("still throws on get() of an undefined literal", () => {
            expect(() => Literal.from(undefined).get()).toThrow("Can't render empty Literal")
        })

        it("leaves the output for non-falsy values unchanged", () => {
            const cases: [any, string][] = [
                ["test string", "literal:string:test%20string"],
                [42, "literal:number:42"],
                [-1.5, "literal:number:-1.5"],
                [true, "literal:boolean:true"],
                [{ a: 1, b: [0, false, ""] }, "literal:json:%7B%22a%22%3A1%2C%22b%22%3A%5B0%2Cfalse%2C%22%22%5D%7D"],
            ]
            for (const [value, url] of cases) {
                expect(Literal.from(value).toUrl()).toBe(url)
                expect(Literal.from(value).get()).toStrictEqual(value)
                expect(Literal.fromUrl(url).get()).toStrictEqual(value)
            }
        })
    })
})
