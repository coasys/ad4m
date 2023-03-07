# AD4M Test

This is a testing library for testing languages for ad4m. This package internally uses ad4m-host (https://github.com/perspect3vism/ad4m/tree/dev/host) to start and interact with ad4m-executor. For more detailed example look go [here](https://github.com/perspect3vism/ad4m-test/tree/main/example)

## Usage

### Install the package:

npm:

`npm install @perspect3vism/ad4m-test`

yarn:

`yarn add @perspect3vism/ad4m-test`

### Write tests

You can write tests like you would with any other testing framework. You can use the helper methods to interact with the ad4m-executor.

**example**

```js
const { spawnExpressionAgent, spawnLinkAgent } = require('@perspect3vism/ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const agent = await spawnExpressionAgent()

    const exp = await agent.create("{\"name\": \"hello world!\"}");

    expect(exp).not.toBeNull()

    const fetched = await agent.get(exp);

    expect(fetched).not.toBeNull()
  })
})

describe("Link", () => {
  it("Create Link", async () => {
    const agent = await spawnLinkAgent();

    const all = await agent.queryLinks({});

    expect(all.length).toBe(0)
    
    const link = await agent.addLink({source:"root", predicate: "soic://test", target:"QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL://QmSsCCtXMDAZXMpyiNLzwjGEU4hLmhG7fphidhEEodQ4Wy"})

    const all1 = await agent.queryLinks({});

    expect(all1.length).toBe(1)
    expect(all1[0].data.source).toBe(link.data.source)
    expect(all1[0].data.predicate).toBe(link.data.predicate)
    expect(all1[0].data.target).toBe(link.data.target)
  });
})
```

Then your can run the test like so:

```cli
ad4m-test --test ./expression.test.js --bundle languages/sdp.js --meta '{\"name\":\"shortform-expression\",\"description\":\"Shortform expression for flux application\",\"sourceCodeLink\":\"https://github.com/juntofoundation/ad4m-languages\",\"possibleTemplateParams\":[\"uid\",\"name\"]}'
```

### ExpressionUI testing

Running the below command will start a local server which will host the ExpressionUI which can be used to test UI for the expression language.

```cli
ad4m-test --ui --bundle languages/sdp.js --meta '{\"name\":\"shortform-expression\",\"description\":\"Shortform expression for flux application\",\"sourceCodeLink\":\"https://github.com/juntofoundation/ad4m-languages\",\"possibleTemplateParams\":[\"uid\",\"name\"]}'
```

### API:

**Cli params:**

- `relativePath | rp` - Relative path to the appdata for ad4m-host to store binaries
- `test | t` - Runs test on a single file
- `bundle | b` - Language bundle for the language to be tested
- `meta | m` - Meta information for the language to be installed
- `defaultLangPath | dlp` - Local bulid-in language to be used instead of the packaged ones
- `hideLogs | hl` - Hide the ad4m-test logs
- `ui` - Starts a local server with expressionUI loaded for testing.

**Helpers:**
- `spawnExpressionAgent()` - Spawns a agent that can work with expressions.
- `spawnLinkAgent()` - Spawns an agent that can work with link and multiple agents are part of same neighbourhood to similify testing.