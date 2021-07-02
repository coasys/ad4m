const fs = require("fs-extra");
const wget = require("node-wget-js");

const languages = {
  profiles: {
    targetDnaName: "agent-store",
    dna: "https://github.com/perspect3vism/agent-language/releases/download/0.0.1/agent-store.dna",
    bundle:
      "https://github.com/perspect3vism/agent-language/releases/download/0.0.1/bundle.js",
  },
  languages: {
      targetDnaName: "languages",
      dna: ""
  }
};

async function main() {
  for (const lang in languages) {
    const dir = `./src/test-temp/ad4m/languages/temp/${lang}`;
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
