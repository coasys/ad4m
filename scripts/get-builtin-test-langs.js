const fs = require("fs-extra");
const wget = require("node-wget-js");

const languages = {
  "agent-expression-store": {
    targetDnaName: "agent-store",
    dna: "https://github.com/perspect3vism/agent-language/releases/download/0.0.2/agent-store.dna",
    bundle:
      "https://github.com/perspect3vism/agent-language/releases/download/0.0.2/bundle.js",
  },
  languages: {
      targetDnaName: "languages",
      dna: "https://github.com/perspect3vism/language-persistence/releases/download/0.0.1/languages.dna",
      bundle: "https://github.com/perspect3vism/language-persistence/releases/download/0.0.1/bundle.js",
  },
  "neighbourhood-store": {
    targetDnaName: "neighbourhood-store",
    dna: "https://github.com/perspect3vism/neighbourhood-language/releases/download/0.0.1/neighbourhood-store.dna",
    bundle: "https://github.com/perspect3vism/neighbourhood-language/releases/download/0.0.1/bundle.js",
  } 
};

async function main() {
  for (const lang in languages) {
    const dir = `./src/test-temp/${lang}`;
    await fs.ensureDir(dir + "/build");

    // bundle
    if (languages[lang].bundle) {
      let url = languages[lang].bundle;
      let dest = dir + "/build/bundle.js";
      wget({ url, dest });
    }

    // dna
    if (languages[lang].dna) {
      url = languages[lang].dna;
      dest = dir + `/${languages[lang].targetDnaName}.dna`;
      wget({ url, dest });
    }
  }
}

main();
