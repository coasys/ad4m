import { Ad4mClient, LanguageMetaInput } from '@perspect3vism/ad4m';
import type { Arguments, Argv } from 'yargs';
import { buildAd4mClient, CommonOptions, prettify } from './util';
import ReadlineSync from 'readline-sync';

export const command: string = 'languages [action]';
export const desc: string = 'Languages related action';

type Options = CommonOptions & {
  address?: string;
  filter?: string;
  all?: boolean;
  settings?: string;
  templateData?: string;
  path?: string;
  meta?: string;
};

export const builder = (yargs: Argv) =>
  yargs
    .positional('action', {
      type: 'string',
      describe: 'Action that should be executed on the languages',
      choices: ['get', 'writeSettings', 'applyTemplateAndPublish', 'publish', 'meta', 'source'],
    })
    .options({
      address: { type: "string", describe: 'Address of the language' },
      filter: { type: "string", describe: 'Filter the languages when query' },
      all: { type: "boolean", describe: 'Flag to request all the languages' },
      settings: { type: "string", describe: 'Settings of the language to write' },
      templateData: { type: "string", describe: 'Apply template data when publish templated language' },
      path: { type: "string", describe: 'Refer to the language under this path' },
      meta: { type: "string", describe: 'metadata of the language' }
    });

export const handler = async (argv: Arguments<Options>): Promise<void> => {
  const {
    server, address, filter, all, settings,
    templateData, path, meta, action
  } = argv;

  const ad4mClient = buildAd4mClient(server);
  switch (action) {
    case 'get': await get(ad4mClient, address, filter, all); break;
    case 'writeSettings': await writeSettings(ad4mClient, address, settings);  break;
    case 'applyTemplateAndPublish': await applyTemplateAndPublish(ad4mClient, address, templateData); break;
    case 'publish': await publish(ad4mClient, path, meta); break
    case 'meta': await getMeta(ad4mClient, address); break;
    case 'source': await source(ad4mClient, address); break;

    default:
      console.info(`Action "${argv.action}" is not defined on languages.`)
      break;
  }

  process.exit();
};

async function get(ad4mClient: Ad4mClient, address: string, filter: string, all: boolean) {
  if (address) {
    const language = await ad4mClient.languages.byAddress(address);
    prettify(language)
    return;
  }
  if (filter) {
    const languages = await ad4mClient.languages.byFilter(filter);
    prettify(languages)
    return;
  }
  if (all) {
    const languages = (await ad4mClient.languages.all()).map(l => {
      return { name: l.name, address: l.address };
    })
    prettify(languages)
    return;
  }
  console.info('Language get action is missing param <address>/<filter>/<all>');
}

async function writeSettings(ad4mClient: Ad4mClient, address?: string, settings?: string) {
  if (address == undefined || settings == undefined) {
    console.info('Language writeSettings action is missing params <address> and <settings>');
    return;
  }
  const result = await ad4mClient.languages.writeSettings(address!, settings!);
  prettify(result);
}

async function applyTemplateAndPublish(ad4mClient: Ad4mClient, address?: string, templateData?: string) {
  if (address == undefined || templateData == undefined) {
    console.info('Language applyTemplateAndPublish action is missing params <address> and <templateData>');
    return;
  }
  const result = await ad4mClient.languages.applyTemplateAndPublish(address, templateData);
  prettify(result);
}

async function publish(ad4mClient: Ad4mClient, path?: string, meta?: string) {
  let languageMetaInput: LanguageMetaInput;
  if (path == undefined) {
    path = ReadlineSync.question("Path of the bundled file: ");
  }
  if (meta == undefined) {
    const name = ReadlineSync.question("Name (must match name in source code): ");
    const description = ReadlineSync.question("Description: ");
    const templateParams = ReadlineSync.question("In case of a templateable Language, list of template parameters (comma separated): ");
    const sourceLink = ReadlineSync.question("Link to source code / Github repo: ");

    languageMetaInput = new LanguageMetaInput(name, description);

    if(sourceLink.trim().length > 0) {
      languageMetaInput.sourceCodeLink = sourceLink.trim()
    }

    const params = templateParams.split(',').map(e => e.trim())
    if(params.length > 0) {
      languageMetaInput.possibleTemplateParams = params
    }
  } else {
    let languageMetaObj = JSON.parse(meta);
    languageMetaInput = new LanguageMetaInput(languageMetaObj.name, languageMetaObj.description);
    languageMetaInput.sourceCodeLink = languageMetaObj.sourceCodeLink.trim();
    languageMetaInput.possibleTemplateParams = languageMetaObj.possibleTemplateParams;
  }

  const result = await ad4mClient.languages.publish(path, languageMetaInput);
  prettify(result);
}

async function getMeta(ad4mClient: Ad4mClient, address?: string) {
  if (address) {
    const result = await ad4mClient.languages.meta(address);
    prettify(result);
    return;
  }
  console.info('Language meta action is missing param <address>');
}

async function source(ad4mClient: Ad4mClient, address?: string) {
  if (address) {
    const result = await ad4mClient.languages.source(address);
    console.log(result);
    return;
  }
  console.info('Language source action is missing param <address>');
}
