export default {
  preset: 'ts-jest',
  rootDir: 'src',
  testTimeout: 200000,
  setupFiles: ["../jest-setup.ts"],
  transform: {
    '^.+\\.tsx?$': 'ts-jest',
    '^.+\\.jsx?$': ['ts-jest', { tsconfig: { allowJs: true } }],
  },
  transformIgnorePatterns: [
    'node_modules/\\.pnpm/(?!.*_patch_hash=)'
  ]
};