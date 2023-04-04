import esbuild from 'esbuild'

esbuild
  .build({
    entryPoints: ['lib/main.js'],
    outfile: 'lib/bundle.js',
    bundle: true,
    platform: 'node',
    target: 'es2020',
    format: 'esm',
    plugins: [],
  })
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });