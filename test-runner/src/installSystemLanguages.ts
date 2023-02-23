import { LanguageMetaInput } from "@perspect3vism/ad4m"
import path from "path";
import fs from 'fs-extra';
import { ChildProcessWithoutNullStreams, execSync, spawn } from "child_process";
import { ad4mDataDirectory, deleteAllAd4mData, findAndKillProcess, getAd4mHostBinary, logger } from "./utils";
import kill from 'tree-kill'
import { buildAd4mClient } from "./client";

let seed = {
  trustedAgents: [],
  knownLinkLanguages: [],
  directMessageLanguage: "",
  agentLanguage: "",
  perspectiveLanguage: "",
  neighbourhoodLanguage: "",
  languageLanguageBundle: "",
  languageLanguageSettings : {
    storagePath: ""
  },
  neighbourhoodLanguageSettings: {
    storagePath: ""
  }
}

const languagesToPublish = {
  "agent-expression-store": {name: "agent-expression-store", description: "", possibleTemplateParams: ["id", "name", "description"], sourceCodeLink: ""} as LanguageMetaInput, 
  "direct-message-language": {name: "direct-message-language", description: "", possibleTemplateParams: ["recipient_did", "recipient_hc_agent_pubkey"], sourceCodeLink: ""} as LanguageMetaInput, 
  "neighbourhood-store": {name: "neighbourhood-store", description: "", possibleTemplateParams: ["id", "name", "description"], sourceCodeLink: ""} as LanguageMetaInput, 
  "perspective-language": {name: "perspective-language", description: "", possibleTemplateParams: ["id", "name", "description"], sourceCodeLink: ""} as LanguageMetaInput,
}

export async function installSystemLanguages(relativePath = '') {
  return new Promise(async (resolve, reject) => {
    deleteAllAd4mData(relativePath);
    fs.removeSync(path.join(__dirname, 'publishedLanguages'))
    fs.removeSync(path.join(__dirname, 'publishedNeighbourhood'))
    fs.mkdirSync(path.join(__dirname, 'publishedLanguages'))
    fs.mkdirSync(path.join(__dirname, 'publishedNeighbourhood'))

    let binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary', `ad4m-host-${global.ad4mHostVersion}`);

    if (!fs.existsSync(binaryPath)) {
      await getAd4mHostBinary(relativePath);
      binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary', `ad4m-host-${global.ad4mHostVersion}`);
    }

    await findAndKillProcess('holochain')
    await findAndKillProcess('lair-keystore')

    const seedFile = path.join(__dirname, '../bootstrapSeed.json')

    execSync(`${binaryPath} init --dataPath ${relativePath} --networkBootstrapSeed ${seedFile} --overrideConfig`, { encoding: 'utf-8' });

    logger.info('ad4m-test initialized')

    let child: ChildProcessWithoutNullStreams;

    const defaultLangPath = path.join(__dirname, './languages');

    const languageLanguageBundlePath = path.join(__dirname, 'languages', "languages", "build", "bundle.js");
        
    seed['languageLanguageBundle'] = fs.readFileSync(languageLanguageBundlePath).toString();
    seed['languageLanguageSettings'] = { storagePath: path.join(__dirname, 'publishedLanguages') }
    seed['neighbourhoodLanguageSettings'] = { storagePath: path.join(__dirname, 'publishedNeighbourhood') }

    fs.writeFileSync(path.join(__dirname, '../bootstrapSeed.json'), JSON.stringify(seed));

    if (defaultLangPath) {
      child = spawn(`${binaryPath}`, ['serve', '--reqCredential', global.ad4mToken ,'--dataPath', relativePath, '--port', '4000', '--languageLanguageOnly', 'true'])
    } else {
      child = spawn(`${binaryPath}`, ['serve', '--reqCredential', global.ad4mToken, '--dataPath', relativePath, '--port', '4000', '--languageLanguageOnly', 'true'])
    }

    const client = await buildAd4mClient();

    const logFile = fs.createWriteStream(path.join(process.cwd(), 'ad4m-test.log'))

    child.stdout.on('data', async (data) => {
      logFile.write(data)
    });
    child.stderr.on('data', async (data) => {
      logFile.write(data)
    })

    child.stdout.on('data', async (data) => {
      if (data.toString().includes('GraphQL server started, Unlock the agent to start holohchain')) {
        await client.agent.generate('123456789')

        for (const [lang, languageMeta] of Object.entries(languagesToPublish)) {
          const bundlePath = path.join(__dirname, 'languages', lang, 'build', 'bundle.js')
          const language = await client.languages.publish(bundlePath, languageMeta);
          
          if (lang === "agent-expression-store") {
            seed["agentLanguage"] = language.address;
          }
          if (lang === "neighbourhood-store") {
            seed["neighbourhoodLanguage"] = language.address;
          }
          if (lang === "direct-message-language") {
            seed["directMessageLanguage"] = language.address;
          }
          if (lang === "perspective-language") {
            seed["perspectiveLanguage"] = language.address;
          }          
        }
        fs.writeFileSync(path.join(__dirname, '../bootstrapSeed.json'), JSON.stringify(seed));
        
        logger.info('bootstrapSeed file populated with system language hashes')

        kill(child.pid!, async () => {
          await findAndKillProcess('holochain')
          await findAndKillProcess('lair-keystore')
          resolve(null);
        })

      }
    });

    child.on('exit', (code) => {
      logger.info(`exit is called ${code}`);
      resolve(null);
    })

    child.on('error', () => {
      logger.error(`process error: ${child.pid}`)
      findAndKillProcess('holochain')
      findAndKillProcess('lair-keystore')
      findAndKillProcess('ad4m-host')
      reject()
    });
  });
}

if (require.main === module) {
  installSystemLanguages().then(() => {
    process.exit(0);
  }).catch(e => {
    process.exit(1);
  });
}
