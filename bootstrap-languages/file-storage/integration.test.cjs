const { spawnExpressionAgent  } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    const data_base64 = Buffer.from("test data").toString('base64');
    const exp = await agent.create({
      data_base64: data_base64,
      name: "test file object",
      file_type: "text/plain"
    });
    console.log("created exp", exp);

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp)
    expect(fetched).not.toBeNull()
    expect(fetched.proof.valid).toBe(true);
    const data = JSON.parse(fetched.data);
    expect(data.data_base64).toBe(data_base64);
  })
})