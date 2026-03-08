// This file takes priority over .mocharc.json (mocha config resolution order:
// .cjs > .js > .yaml > .json).
//
// `spec` is needed so the VS Code "Mocha for VS Code" extension can discover
// test files.  However, when ts-mocha (or a CI script) passes an explicit file
// on the command line, mocha's config-level `spec` would **override** that
// positional arg and cause failures.  We detect this by inspecting argv for a
// .test.ts file and omitting `spec` in that case.

const cliHasTestFile = process.argv.some(a => /\.test\.ts$/.test(a));

module.exports = {
  ...(!cliHasTestFile && !process.env.CI ? { spec: "tests/**/*.test.ts" } : {}),
  timeout: 1200000,
  exit: true,
  "node-option": [
    "experimental-specifier-resolution=node",
    "loader=ts-node/esm",
  ],
};
