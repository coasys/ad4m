const fs = require("fs-extra");
const wget = require("node-wget-js");
const unzipper = require("unzipper");
const path = require("path");

const languages = {
  "agent-expression-store": {
    targetDnaName: "agent-store",
    dna: "https://github.com/perspect3vism/agent-language/releases/download/0.0.10/agent-store.dna",
    bundle:
      "https://github.com/perspect3vism/agent-language/releases/download/0.0.10/bundle.js",
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
  "social-context": {
    zipped: true,
    targetDnaName: "social-context",
    resource:
      "https://github.com/juntofoundation/Social-Context/releases/download/0.0.22/full_features.zip",
  },
  "note-ipfs": {
    bundle: "https://github.com/perspect3vism/lang-note-ipfs/releases/download/0.0.4/bundle.js",
  },
  "direct-message-language": {
    bundle: "https://github.com/perspect3vism/direct-message-language/releases/download/0.0.3/bundle.js"
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
        fs.copyFileSync(url, dest);
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
