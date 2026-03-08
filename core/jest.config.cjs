/** @type {import('ts-jest').JestConfigWithTsJest} */
module.exports = {
  preset: 'ts-jest',
  rootDir: 'src',
  testTimeout: 200000,
  setupFiles: ["../jest-setup.ts"],
  extensionsToTreatAsEsm: ['.ts'],
  transform: {
    '^.+\\.tsx?$': [
      'ts-jest',
      {
        useESM: true,
      },
    ],
  },
  moduleNameMapper: {
    '^express$': '<rootDir>/__mocks__/express.js',
  },
  testPathIgnorePatterns: [
    '/node_modules/',
    'Ad4mClient.test.ts',
  ],
};
