import { Arguments, Argv } from 'yargs';
// @ts-ignore
import { init } from "@perspect3vism/ad4m-executor";
import path from 'path';
import fs from 'fs';
// @ts-ignore
import getPort from 'get-port';
import { getConfig } from '../utils/config';
import { ad4mDataDirectory } from '../ad4mDataDirectory';
import { homedir } from 'os';


type Options = {
  port?: number;
  hcAdminPort?: number;
  hcAppPort?: number;
  connectHolochain?: boolean;
  languageLanguageOnly?: boolean;
  bootstrapLanguage?: string;
  bootstrapPerspective?: string;
  appLangAliases?: string;
  dataPath?: string;
  reqCredential?: string;
};

export const command: string = 'serve';
export const desc: string = 'Serve ad4m service at given port';

export const builder = (yargs: Argv) =>
  yargs
    .options({
      port: { 
        type: 'number', 
        describe: 'Use this port to run ad4m GraphQL service', 
        default: 4000, 
        alias: 'p'
      },
      hcAdminPort: { 
        type: 'number', 
        describe: 'Admin port of holochain conductor'
      },
      hcAppPort: { 
        type: 'number', 
        describe: 'Port used by hApp' 
      },
      connectHolochain: { 
        type: "boolean", 
        describe: 'Flag to connect existing running holochain process'
      },
      dataPath: { 
        type: 'string', 
        describe: 'Name of directory (within the systems app data path) to store ad4m data', 
        alias: 'rp'
      },
      languageLanguageOnly: {
        type: 'boolean',
        describe: 'Should the ad4m-executor be started with only the languageLanguage, so it can be used for publish other system languages',
        default: false,
        alias: 'll'
      },
      bootstrapLanguage: {
        type: 'string',
        describe: 'Path to Bootstrap languages json file (list of languages)',
      },
      bootstrapPerspective: {
        type: 'string',
        describe: 'Path to Bootstrap perspectives json file (list of perspectives)'
      },
      appLangAliases: {
        type: 'string',
        describe: 'Language aliases to be loaded into ad4m-executor',
        default: '{}'
      },
      reqCredential: {
        type: 'string',
        describe: 'The credential for an admin client to override capability checks',
      }
    });

export const handler = async (argv: Arguments<Options>): Promise<void> => {
  const {
    port, hcAdminPort, hcAppPort, connectHolochain, languageLanguageOnly,
    dataPath, bootstrapLanguage, bootstrapPerspective, appLangAliases,
    reqCredential
  } = argv;

  const globalConfig = getConfig();

  if(!globalConfig[dataPath || '']) {
    throw Error('No config found, please run ad4m-host init with the dataPath & networkBootstrapSeed params')
  }

  const { seedPath } = globalConfig[dataPath || ''];

  let appDataPath = ad4mDataDirectory(dataPath)

  const binaryPath = path.join(appDataPath, 'binary');
  const swiplHomePath = (process.platform == "win32" ? path.join(appDataPath, 'swipl/') : path.join(appDataPath, 'swipl/lib/swipl/'))
  const swiplPath = path.join(appDataPath, 'swipl/bin/swipl');
  const gqlPort = await getPort({ port })
  const ipfsRepoPath = path.join(appDataPath, 'ipfs')

  if (!fs.existsSync(appDataPath)) {
    fs.mkdirSync(appDataPath);
  }

  const bLanguage = bootstrapLanguage ? await import(path.isAbsolute(bootstrapLanguage) ? bootstrapLanguage: path.join(__dirname, bootstrapLanguage)) : [];

  const bPerspective = bootstrapPerspective ? await import(path.isAbsolute(bootstrapPerspective) ? bootstrapPerspective: path.join(__dirname, bootstrapPerspective)) : [];

  const config = {
    appDataPath: appDataPath,
    resourcePath: binaryPath,
    networkBootstrapSeed: seedPath,
    languageLanguageOnly: languageLanguageOnly,
    bootstrapFixtures: {
      languages: [...bLanguage],
      perspectives: [...bPerspective],
    },
    appLangAliases: JSON.parse(appLangAliases),
    mocks: false,
    gqlPort,
    hcPortAdmin: hcAdminPort,
    hcPortApp: hcAppPort,
    ipfsRepoPath,
    connectHolochain,
    reqCredential,
    swiplPath,
    swiplHomePath
  };

  await init(config);
};
