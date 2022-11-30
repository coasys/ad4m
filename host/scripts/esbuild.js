import { build } from 'esbuild' 
build({
    entryPoints: ["./src/cli.ts"],
    platform: 'node',
    bundle: true,
    format: "cjs",
    minify: false,
    sourcemap: false,
    outfile: "build/cli.cjs",
    external: ['classic-level', 'default-gateway'],
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch((er) => {
    console.log('errr', er)
    process.exit(1)
  });

build({
    entryPoints: ["./src/cli.ts"],
    platform: 'node',
    bundle: true,
    format: "esm",
    minify: false,
    sourcemap: false,
    outfile: "build/index.js",
    external: ['classic-level', 'default-gateway'],
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch((er) => {
    console.log('errr', er)
    process.exit(1)
  });
