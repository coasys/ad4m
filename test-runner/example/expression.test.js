const { createExpression, getExpression, spawnExpressionAgent  } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    let data_base64 = Buffer.from("Hello world!").toString('base64');
    const exp = await agent.create({
      data_base64: data_base64,
      name: "test file object",
      file_type: "text/plain"
    });

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp)

    expect(fetched).not.toBeNull()
  })
})