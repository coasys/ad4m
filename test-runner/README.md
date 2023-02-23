# AD4M Test

This is a testing library for testing languages for ad4m. This package internally uses [ad4m-host v0.0.9](https://github.com/fluxsocial/ad4m-host/) to start and interact with ad4m-executor. For more detailed example look go [here](https://github.com/perspect3vism/ad4m-test/tree/main/example)

## Usage

### Install the package:

npm:

`npm install ad4m-test`

yarn:

`yarn add ad4m-test`

### Write tests

You can write tests like you would with any other testing framework. You can use the helper methods to interact with the ad4m-executor.

**example**

```js
const { createExpression, getExpression } = require('ad4m-test/helpers')

describe("Expression", () => {
  it("Create Expression", async () => {
    const exp = await createExpression("{\"name\": \"hello world!\"}");

    expect(exp).not.toBeNull()

    const fetched = await getExpression(exp);

    expect(fetched).not.toBeNull()
  })
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
- `port | p` - Use this port to run ad4m GraphQL service
- `test | t` - Runs test on a single file
- `bundle | b` - Language bundle for the language to be tested
- `meta | m` - Meta information for the language to be installed
- `languageTye | lt` - Is the language a link or expression language
- `defaultLangPath | dlp` - Local bulid-in language to be used instead of the packaged ones
- `hideLogs | hl` - Hide the ad4m-test logs
- `ui` - Starts a local server with expressionUI loaded for testing.

**Helpers:**
- `addLink(link: LinkExpression)` - Adds link and returns added link.
- `removeLink(link: LinkExpression)` - Removes links.
- `udpateLink(oldLink: LinkExpression, newLink: LinkExpression)` - Updates the old link and return a new one.
- `queryLinks(query: LinkQuery)` - gets all the link for that perspective.
- `createExpression(content: any)` - Creates an expression with the supplied content and returns expression url.
- `getExpression(url: string)` - Fetches the expression from the url passed.