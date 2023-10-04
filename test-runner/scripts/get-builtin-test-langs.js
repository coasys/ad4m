const fs = require("fs-extra");
const wget = require("node-wget-js");
const { Extract } = require("unzipper");
const { join } = require("path");

const languages = {
  "agent-expression-store": {
    bundle: "https://github.com/perspect3vism/agent-language/releases/download/0.2.0/bundle.js",
  },
  languages: {
      targetDnaName: "languages",
      bundle: "https://github.com/perspect3vism/local-language-persistence/releases/download/0.0.1/bundle.js",
  },
  "neighbourhood-store": {
    targetDnaName: "neighbourhood-store",
    //dna: "https://github.com/perspect3vism/neighbourhood-language/releases/download/0.0.2/neighbourhood-store.dna",
    bundle: "https://github.com/perspect3vism/local-neighbourhood-persistence/releases/download/0.0.1/bundle.js",
  },
  "perspective-diff-sync": {
    bundle: "https://github.com/perspect3vism/perspective-diff-sync/releases/download/v0.2.2-test/bundle.js",
  },
  "note-ipfs": {
    bundle: "https://github.com/perspect3vism/lang-note-ipfs/releases/download/0.0.4/bundle.js",
  },
  "direct-message-language": {
    bundle: "https://github.com/perspect3vism/direct-message-language/releases/download/0.1.0/bundle.js"
  },
  "perspective-language": {
    bundle: "https://github.com/perspect3vism/perspective-language/releases/download/0.0.1/bundle.js"
  }
};

async function main() {
  for (const lang in languages) {
    // const targetDir = Deno.readFileSync('./scripts/download-languages-path').toString()
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