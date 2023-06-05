const { spawnExpressionAgent  } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    const exp = await agent.create({
      linkLanguage: "test",
      meta: {
          links: []
      }
    });
    console.log("created exp", exp);

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp)
    console.log("fetched exp", fetched);
    expect(fetched).not.toBeNull()
    expect(fetched.proof.valid).toBe(true);
    const data = JSON.parse(fetched.data);
    expect(data.linkLanguage).toBe("test");
  })
})