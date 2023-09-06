import fs from "fs-extra";
import wget from "node-wget-js";
import unzipper from "unzipper";
import path from "path";
import os from "os";

const languages = {
  "agent-expression-store": {
    bundle: "../../bootstrap-languages/agent-language/build/bundle.js",
  },
  languages: {
    bundle: "https://github.com/perspect3vism/local-language-persistence/releases/download/0.0.9/bundle.js",
  },
  "neighbourhood-store": {
    bundle: "https://github.com/perspect3vism/local-neighbourhood-persistence/releases/download/0.0.6/bundle.js",
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
        fs.copyFileSync(path.join(process.cwd(), url), dest);
      } else {
        wget({ url, dest });
      }
    }

    // dna
    if (languages[lang].dna) {
      url = languages[lang].dna;
      dest = dir + `/${languages[lang].targetDnaName}.dna`;
      wget({ url, dest });
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
}

main();
