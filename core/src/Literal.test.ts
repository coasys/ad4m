import { Literal } from "./Literal";

describe("Literal", () => {
  it("can handle strings", () => {
    const testString = "test string";
    const testUrl = "literal://string:test%20string";
    expect(Literal.from(testString).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toBe(testString);
  });

  it("can handle numbers", () => {
    const testNumber = 3.1415;
    const testUrl = "literal://number:3.1415";
    expect(Literal.from(testNumber).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toBe(testNumber);
  });

  it("can handle objects", () => {
    const testObject = { testNumber: "1337", testString: "test" };
    const testUrl =
      "literal://json:%7B%22testNumber%22%3A%221337%22%2C%22testString%22%3A%22test%22%7D";
    expect(Literal.from(testObject).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toStrictEqual(testObject);
  });

  it("can handle special characters", () => {
    const testString = "message(X) :- triple('ad4m://self', _, X).";
    const testUrl =
      "literal://string:message%28X%29%20%3A-%20triple%28%27ad4m%3A%2F%2Fself%27%2C%20_%2C%20X%29.";
    expect(Literal.from(testString).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toBe(testString);
  });

  it("can handle boolean true", () => {
    const testUrl = "literal://boolean:true";
    expect(Literal.from(true).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toBe(true);
  });

  it("can handle boolean false", () => {
    const testUrl = "literal://boolean:false";
    expect(Literal.from(false).toUrl()).toBe(testUrl);
    expect(Literal.fromUrl(testUrl).get()).toBe(false);
  });

  it("can handle zero", () => {
    expect(Literal.from(0).toUrl()).toBe("literal://number:0");
    expect(Literal.fromUrl("literal://number:0").get()).toBe(0);
  });
});
