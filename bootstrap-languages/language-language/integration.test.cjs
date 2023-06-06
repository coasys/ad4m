const { spawnExpressionAgent  } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    const data_base64 = Buffer.from("test data").toString('base64');
    const exp = await agent.create({
      bundle: data_base64,
      meta: {
        name: "language-language",
        address: "QmdJ7bZY6UEZsxHpbk1fteHVWJ9dCgR1Avqw9Cj1KRG1Um",
        description: "description"
      }
    });
    console.log("created exp", exp);

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp)
    console.log("fetched exp", fetched);
    expect(fetched).not.toBeNull()
    expect(fetched.proof.valid).toBe(true);
    const data = JSON.parse(fetched.data);
    expect(data.name).toBe("language-language");
  })
})