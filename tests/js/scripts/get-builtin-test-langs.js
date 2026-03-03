import fs from "https://esm.sh/v135/fs-extra@11.1.1";
import wget from "https://esm.sh/v135/node-wget-js@1.0.1";
import unzipper from "https://esm.sh/v135/unzipper@0.10.14";
import path from "node:path";
import os from "node:os";

const languages = {
  "agent-expression-store": {
    bundle: "../../bootstrap-languages/agent-language/build/bundle.js",
  },
  languages: {
    bundle: "../../bootstrap-languages/local-language-persistence-0.0.9.js",
  },
  "neighbourhood-store": {
    bundle: "../../bootstrap-languages/local-neighbourhood-persistence-0.0.6.js",
  },
  "perspective-diff-sync": {
    bundle: "../../bootstrap-languages/p-diff-sync/build/bundle.js",
  },
  "direct-message-language": {
    bundle: "../../bootstrap-languages/direct-message-language/build/bundle.js"
  },
  "perspective-language": {
    bundle: "../../bootstrap-languages/perspective-language/build/bundle.js"
  }
};

async function main() {
  for (const lang in languages) {
    const targetDir = fs.readFileSync('./scripts/download-languages-path').toString()
    const dir = path.join(targetDir, lang)
    await fs.ensureDir(dir + "/build");

    // bundle
    if (languages[lang].bundle) {
      let url = languages[lang].bundle;
      let dest = dir + "/build/bundle.js";
      if (url.slice(0, 8) != "https://" && url.slice(0, 7) != "http://") {
        if (os.platform() == "win32") url = url.replace(/\//g, "\\");
        fs.copyFileSync(path.join(Deno.cwd(), url), dest);
      } else {
        await wget({ url, dest });
      }
    }

    // dna
    if (languages[lang].dna) {
      url = languages[lang].dna;
      dest = dir + `/${languages[lang].targetDnaName}.dna`;
      await wget({ url, dest });
    }

    if (languages[lang].zipped) {
      await wget(
        {
          url: languages[lang].resource,
          dest: `${dir}/lang.zip`,
        },
        async () => {
          //Read the zip file into a temp directory
          await fs
            .createReadStream(`${dir}/lang.zip`)
            .pipe(unzipper.Extract({ path: `${dir}` }))
            .promise();

          // if (!fs.pathExistsSync(`${dir}/bundle.js`)) {
          //   throw Error("Did not find bundle file in unzipped path");
          // }

          fs.copyFileSync(
            path.join(`${dir}/bundle.js`),
            path.join(`${dir}/build/bundle.js`)
          );
          fs.rmSync(`${dir}/lang.zip`);
          fs.rmSync(`${dir}/bundle.js`);
        }
      );
    }
  }
  Deno.exit(0);
}

main();
