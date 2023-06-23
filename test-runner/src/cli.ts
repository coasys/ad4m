#! /usr/bin/env node

import fs from 'fs-extra';
import { runtest, showTestsResults } from './index.js';
import path from 'path';
import { v4 as uuidv4 } from 'uuid';
import yargs from 'yargs'
import { hideBin } from 'yargs/helpers';
import { ChildProcessWithoutNullStreams, spawn } from 'child_process';
import { execSync } from 'child_process';
import kill from 'tree-kill'
import { resolve as resolvePath} from 'path'
import { ad4mDataDirectory, cleanOutput, deleteAllAd4mData, findAndKillProcess, getAd4mHostBinary, getTestFiles, logger } from './utils.js';
import process from 'process';
import { installSystemLanguages } from './installSystemLanguages.js';
import { buildAd4mClient } from './client.js';
import express from 'express'
import getPort from 'get-port';

async function installLanguage(child: any, binaryPath: string, bundle: string, meta: string, languageType: string, resolve: any, port?: number, callback?: any) { 
  const ad4mClient = await buildAd4mClient(port!);

  const { ui } = global.config;

  let languageAddress;
  let perspective;
  let neighbourhood;
 
  if (bundle && meta) {
    try {    
      const language = await ad4mClient.languages.publish(resolvePath(bundle), JSON.parse(meta));
      logger.info(`Published language: `, language)
     
      await ad4mClient.runtime.addTrustedAgents([language.author])

      global.languageAddress = language.address;

      languageAddress = language.address;

      if (languageType === 'linkLanguage') {
        const agents = global.agents;

        if (agents.length === 0) {
          const templateLanguage = await ad4mClient.languages.applyTemplateAndPublish(language.address, JSON.stringify({uid: '123', name: `test-${language.name}`}));
          logger.info(`Published Template Language: `, templateLanguage)
    
          global.languageAddress = templateLanguage.address;
  
          languageAddress = language.address;
  
          const createdPerspective = await ad4mClient.perspective.add('Test perspective');
          logger.info(`Perspective created: `, createdPerspective)
        
          global.perspective = createdPerspective.uuid;
  
          perspective = createdPerspective.uuid;
  
          const createdNeighbourhood = await ad4mClient.neighbourhood.publishFromPerspective(perspective, templateLanguage.address, JSON.parse('{"links":[]}'));
          logger.info(`Neighbourhood created: `, createdNeighbourhood)
          
          global.neighbourhood = createdNeighbourhood;
  
          neighbourhood = createdNeighbourhood;
        } else {
          const joinedPerspectve = await ad4mClient.neighbourhood.joinFromUrl(global.neighbourhood);
          logger.info(`Neighbourhood joined: `, joinedPerspectve)

          global.perspective = joinedPerspectve.uuid;
  
          perspective = joinedPerspectve.uuid;

          global.neighbourhood = global.neighbourhood;
  
          neighbourhood = global.neighbourhood;
        }
      }

      if (ui) {
        const found = await ad4mClient.languages.byAddress(language.address)
        fs.writeFileSync(path.join(process.cwd(), '/public/icon.js'), found.icon?.code!)  
        fs.writeFileSync(path.join(process.cwd(), '/public/constructorIcon.js'), found.constructorIcon?.code!)  
      }

      if (!ui) {
        return {
          languageAddress,
          perspective,
          neighbourhood
        }
      }
    } catch (err) {
      logger.error(`Error: ${err}`)
    }
  }

}

interface StartServerResponse {
  languageAddress?: string;
  perspective?: string;
  neighbourhood: string;
  clear: () => void;
}

export function startServer(relativePath: string, bundle: string, meta: string, languageType: string, port: number, defaultLangPath?: string, callback?: any, ): Promise<any> {
  return new Promise(async (resolve, reject) => {
    deleteAllAd4mData(relativePath);
    
    let binaryPath = path.join(ad4mDataDirectory(`.ad4m-test`), 'binary', `ad4m`);

    if (!fs.existsSync(binaryPath)) {
      await getAd4mHostBinary(`.ad4m-test`, localAd4mPath);
      binaryPath = path.join(ad4mDataDirectory(`.ad4m-test`), 'binary', `ad4m`);
    }

    //await findAndKillProcess('holochain')
    const seedFile = path.join(__dirname, '../bootstrapSeed.json')
    const agentSeedFile = path.join(__dirname, `../${relativePath}-bootstrapSeed.json`);


    const tempSeedFile = JSON.parse(fs.readFileSync(seedFile).toString())

    fs.writeFileSync(agentSeedFile, JSON.stringify(tempSeedFile));
    
    execSync(`${binaryPath} init --dataPath ${relativePath} --networkBootstrapSeed ${agentSeedFile} --overrideConfig`, { encoding: 'utf-8' });

    logger.info('ad4m-test initialized')

    let child: ChildProcessWithoutNullStreams;

    const ipfsPort = await getPort({port: 14000});
    const holochainAppPort = await getPort({port: 15000});
    const holochainAdminPort = await getPort({port: 16000});

    const args = ['serve', '--reqCredential', global.ad4mToken, '--dataPath', relativePath, 
      '--port', port.toString(), '--ipfsPort', ipfsPort.toString(),'--languageLanguageOnly', 'false', 
      '--hcAppPort', holochainAppPort.toString(), '--hcAdminPort', holochainAdminPort.toString(),
      '--hcUseLocalProxy', 'false', '--hcUseMdns', 'true', '--hcUseProxy', 'false', '--hcUseBootstrap', 'false'];
    child = spawn(`${binaryPath}`, args);

    const logFile = fs.createWriteStream(path.join(process.cwd(), `ad4m-test-agent-${global.agents.length}.log`))

    child.stdout.on('data', async (data) => {
      logFile.write(data)
    });
    child.stderr.on('data', async (data) => {
      logFile.write(data)
    })

    child.stdout.on('data', async (data) => {
      if (data.toString().includes('GraphQL server started, Unlock the agent to start holohchain')) {
        const ad4mClient = await buildAd4mClient(port);

        const generateAgentResponse = await ad4mClient.agent.generate('123456789');
        const currentAgentDid =  generateAgentResponse.did;
        
        logger.info(`Current Agent did: ${currentAgentDid}`);
      }
      
      if (data.toString().includes('AD4M init complete')) {
        let installResult = await installLanguage(child, binaryPath, bundle, meta, languageType, resolve, port, callback);
        let languageAddress = installResult!.languageAddress;
        let perspective = installResult!.perspective;
        let neighbourhood = installResult!.neighbourhood;

        const clear = () => {
          kill(child.pid!, async () => {
            await findAndKillProcess('holochain')
            await findAndKillProcess('lair-keystore')
            deleteAllAd4mData(relativePath);
            resolve(null);
          })
        };

        resolve({
          perspective: perspective,
          languageAddress: languageAddress,
          neighbourhood: neighbourhood,
          clear
        } as StartServerResponse);
      }
    });

    child.on('exit', (code) => {
      logger.info(`exit is called ${code}`);
    })

    child.on('error', () => {
      logger.error(`process error: ${child.pid}`)
      findAndKillProcess('holochain')
      findAndKillProcess('lair-keystore')
      findAndKillProcess('ad4m')
      reject()
    });
  });
}

async function run() {
  const args = await yargs(hideBin(process.argv))
    .options({
      relativePath: { 
        type: 'string', 
        describe: 'Relative path to the appdata for ad4m-host to store binaries', 
        alias: 'rp'
      },
      test: {
        type: 'string',
        describe: 'Runs test on a single file',
        alias: 't'
      },
      bundle: {
        type: 'string',
        describe: 'Language bundle for the language to be tested',
        alias: 'b'
      },
      meta: {
        type: 'string',
        describe: 'Meta information for the language to be installed',
        alias: 'm'
      },
      defaultLangPath: {
        type: 'string',
        describe: 'Local bulid-in language to be used instead of the packaged ones',
        default: path.join(__dirname, './languages'),
        alias: 'dlp'
      },
      hideLogs: {
        type: 'boolean',
        describe: 'Hide the ad4m-test logs',
        default: false,
        alias: 'hl'
      },
      ui: {
        type: 'boolean',
        default: false,
        describe: 'Starts a live-server with the UI'
      },
      localAd4mPath: {
        type: 'string',
        describe: 'Path to the ad4m-host binary',
        alias: 'lap',
        default: undefined
      }
    })
    .strict()
    .fail((msg, err) => {
      logger.error(`Error: ${msg}, ${err}`);
      process.exit(1);
    })
    .argv;

  global.hideLogs = args.hideLogs;
  global.ad4mToken = uuidv4()

  const relativePath = args.relativePath || '.ad4m-test';

  global.relativePath = relativePath;
  global.localAd4mPath = args.localAd4mPath;

  await getAd4mHostBinary(relativePath, args.localAd4mPath);

  if (!args.bundle) {
    console.error('bundle param is required')
    process.exit(1);
  }

  if (!args.meta) {
    console.error('meta param is required')
    process.exit(1);
  }

  global.config = {
    relativePath,
    bundle: args.bundle,
    meta: args.meta,
    defaultLangPath: args.defaultLangPath,
    ui: args.ui
  }

  const files = args.test ? [args.test] : getTestFiles();


  if (args.ui) {
    await startServer(relativePath, args.bundle!, args.meta!, 'expression', 4000, args.defaultLangPath, () => {
      const app = express();

      console.log(process.env.IP)

      app.use('/', express.static(path.join(__dirname, '../public')))

      app.get('/', (req, res) => {
        res.send('Hello World!');
      });
      

      app.listen(8181, () => {
        logger.info(`Server started at \n 
          localhost:8181?address=${global.languageAddress} \n
          localhost:8181/expression.html?address=${global.languageAddress}`)
      })
    });
  } else {
    if (files) {
      for (const file of files) {        
        await import(fs.realpathSync(file));
        
        await runtest()
      }
      showTestsResults();
  
    } else {
      logger.error('No test files found')
    }
  }


  process.exit(0)
}

if (require.main === module) {
  run()
}

