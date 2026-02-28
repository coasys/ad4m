// Resets only the dynamically-generated fields in bootstrapSeed.json and
// publishBootstrapSeed.json before each test run.
//
// The stable language-address fields (agentLanguage, perspectiveLanguage, etc.)
// are content-addressed and should remain committed — they only change when
// the bootstrap language bundles change. Only the fields that are regenerated
// per-run are cleared here:
//   - trustedAgents: reset to the single baseline DID (publishing agent is
//     re-injected by inject-publishing-agent.js each run)
//   - languageLanguageBundle: cleared (re-injected by inject-language-language.js)
import fs from "fs";

const BASE_TRUSTED_AGENT =
  "did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n";

function resetSeed(path) {
  if (!fs.existsSync(path)) {
    throw new Error(`Could not find bootstrap seed at path: ${path}`);
  }
  const seed = JSON.parse(fs.readFileSync(path).toString());
  seed["trustedAgents"] = [BASE_TRUSTED_AGENT];
  seed["languageLanguageBundle"] = "";
  fs.writeFileSync(path, JSON.stringify(seed));
}

resetSeed("./bootstrapSeed.json");
resetSeed("./publishBootstrapSeed.json");
console.log(
  "Bootstrap seed files reset (trustedAgents + languageLanguageBundle cleared).",
);
