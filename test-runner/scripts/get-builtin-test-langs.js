const fs = require("fs-extra");
const wget = require("node-wget-js");
const { Extract } = require("unzipper");
const { join } = require("path");

const languages = {
  "agent-expression-store": {
    bundle: "../bootstrap-languages/agent-language/build/bundle.js",
  },
  languages: {
      bundle: "https://github.com/perspect3vism/local-language-persistence/releases/download/0.0.5/bundle.js",
  },
  "neighbourhood-store": {
    bundle: "https://github.com/perspect3vism/local-neighbourhood-persistence/releases/download/0.0.2/bundle.js",
  },
  "perspective-diff-sync": {
    bundle: "../bootstrap-languages/perspective-diff-sync/build/bundle.js",
  },
  "note-ipfs": {
    bundle: "https://github.com/perspect3vism/lang-note-ipfs/releases/download/0.0.4/bundle.js",
  },
  "direct-message-language": {
    bundle: "../bootstrap-languages/direct-message-language/build/bundle.js",
  },
  "perspective-language": {
    bundle: "../bootstrap-languages/perspective-language/build/bundle.js",
  }
};

async function main() {
  for (const lang in languages) {
    // const targetDir = fs.readFileSync('./scripts/download-languages-path').toString()
    const dir = join('build/languages', lang)
    await fs.ensureDir(dir + "/build");

    let url = "";
    let dest = "";

    // bundle
    if (languages[lang].bundle) {
      url = languages[lang].bundle;
      dest = dir + "/build/bundle.js";
      if (url.slice(0, 8) != "https://" && url.slice(0, 7) != "http://") {
        fs.copyFileSync(url, dest);
      } else {
        wget({ url, dest });
      }
    }
    
    // dna
    if (languages[lang].dna) {
      console.log(languages[lang])
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
          await fs.createReadStream(`${dir}/lang.zip`)
            .pipe(Extract({ path: `${dir}` }))
            .promise();

          // if (!fs.pathExistsSync(`${dir}/bundle.js`)) {
          //   throw Error("Did not find bundle file in unzipped path");
          // }

          fs.copyFileSync(
            join(`${dir}/bundle.js`),
            join(`${dir}/build/bundle.js`)
          );
          fs.rmSync(`${dir}/lang.zip`);
          fs.rmSync(`${dir}/bundle.js`);
        }
      );
    }
  }
}

main();