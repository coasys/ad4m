const fs = require("fs");

//get first command line argument
const arg = process.argv[2];
const mainnetSeedPath = process.argv[3];

//read file
const data = fs.readFileSync(arg).toString();
const mainnet = JSON.parse(fs.readFileSync(mainnetSeedPath));
mainnet["languageLanguageBundle"] = data;

//write file
fs.writeFileSync(mainnetSeedPath, JSON.stringify(mainnet));



