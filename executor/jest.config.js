const jestConfig = {
  testEnvironment: "node",
  extensionsToTreatAsEsm: ['.ts'],
  moduleNameMapper: {
    '^(\\.{1,2}/.*)\\.js$': '$1',
  },
  "testMatch": [
      "**/?(*.)+(spec|test).[t]s?(x)"
  ]
};

export default jestConfig;