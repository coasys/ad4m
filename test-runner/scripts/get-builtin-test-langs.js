const fs = require("fs-extra");
const { join } = require("path");

const languages = {
  "agent-expression-store": {
    bundle: "../bootstrap-languages/agent-language/build/bundle.js",
  },
  languages: {
      bundle: "../bootstrap-languages/local-language-persistence/build/bundle.js",
  },
  "neighbourhood-store": {
    bundle: "../bootstrap-languages/local-neighbourhood-persistence/build/bundle.js",
  },
  "perspective-diff-sync": {
    bundle: "../bootstrap-languages/p-diff-sync/build/bundle.js",
  },
  "note-ipfs": {
    bundle: "../bootstrap-languages/note-ipfs/build/bundle.js",
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
    while (!fs.existsSync(dir + "/build" + "/bundle.js")) {
      await new Promise((resolve) => setTimeout(resolve, 1000));
    }

    let url = "";
    let dest = "";

    // bundle
    if (languages[lang].bundle) {
      url = languages[lang].bundle;
      dest = dir + "/build/bundle.js";

      fs.copyFileSync(url, dest);
    }
  }
}

main();