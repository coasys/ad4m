const { createExpression, getExpression, spawnExpressionAgent  } = require('@coasys/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    const exp = await agent.create("{\"name\": \"hello world!\"}");

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp)

    expect(fetched).not.toBeNull()
  })
})