import fs from "fs";
import path from "path";
import { build } from 'esbuild' 

const nodeModules = new RegExp(/^(?:.*[\\\/])?node_modules(?:[\\\/].*)?$/);

const dirnamePlugin = {
  name: "dirname",

  setup(build) {
    build.onLoad({ filter: /.*/ }, ({ path: filePath }) => {
      if (!filePath.match(nodeModules)) {
        let contents = fs.readFileSync(filePath, "utf8");
        const loader = path.extname(filePath).substring(1);
        const dirname = path.dirname(filePath);
        console.log('wow', dirname)
        contents = contents
          .replace("__dirname", `"${dirname}"`)
          .replace("__filename", `"${filePath}"`);
        return {
          contents,
          loader,
        };
      }
    });
  },
};

build({
    entryPoints: ["./src/cli.ts"],
    platform: 'node',
    plugins: [dirnamePlugin],
    bundle: true,
    format: "cjs",
    minify: false,
    sourcemap: false,
    outfile: "build/cli.cjs",
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch((er) => {
    console.log('errr', er)
    process.exit(1)
  });