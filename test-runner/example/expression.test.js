const { createExpression, getExpression } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const exp = await createExpression("{\"name\": \"hello world!\"}");

    expect(exp).not.toBeNull()

    const fetched = await getExpression(exp);

    expect(fetched).not.toBeNull()
  })
})