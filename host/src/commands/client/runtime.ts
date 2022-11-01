import { Ad4mClient, Link, LinkQuery } from '@perspect3vism/ad4m';
import type { Arguments, Argv } from 'yargs';
import { buildAd4mClient, CommonOptions, prettify } from './util';
import fs from 'fs';

export const command: string = 'runtime [action]';
export const desc: string = 'Runtime related action';

type Options = CommonOptions & {
  address?: string;
  did?: string;
  raw?: boolean;
  path?: string;
};

export const builder = (yargs: Argv) =>
  yargs
    .positional('action', {
      type: 'string',
      describe: 'Action that should be executed on the runtime',
      choices: [
        'getTrustedAgents', 'addTrustedAgent', 'deleteTrustedAgent',
        'knownLinkLanguageTemplates', 'addKnownLinkLanguageTemplate', 'removeKnownLinkLanguageTemplate',
        'friends', 'addFriend', 'removeFriend',
        'hcAgentInfos', 'hcAddAgentInfos',
      ],
    })
    .options({
      address: { type: 'string', describe: 'Address of the known link language' },
      did: { type: 'string', describe: 'The did of one agent' },
      raw: { type: 'boolean', describe: 'Flag to request raw agent infos' },
      path: { type: 'string', describe: 'Resolve agent infos under this path' },
    });

export const handler = async (argv: Arguments<Options>): Promise<void> => {
  const { server, address, did, raw, path, action } = argv;

  const ad4mClient = buildAd4mClient(server);
  switch (action) {
    case 'getTrustedAgents': await getTrustedAgents(ad4mClient); break;
    case 'addTrustedAgent': await addTrustedAgent(ad4mClient, did); break;
    case 'deleteTrustedAgent': await deleteTrustedAgent(ad4mClient, did); break;

    case 'knownLinkLanguageTemplates': await knownLinkLanguageTemplates(ad4mClient); break;
    case 'addKnownLinkLanguageTemplate': await addKnownLinkLanguageTemplate(ad4mClient, address); break;
    case 'removeKnownLinkLanguageTemplate': await removeKnownLinkLanguageTemplate(ad4mClient, address); break;

    case 'friends': await friends(ad4mClient); break;
    case 'addFriend': await addFriend(ad4mClient, did); break;
    case 'removeFriend': await removeFriend(ad4mClient, did); break;

    case 'hcAgentInfos': await hcAgentInfos(ad4mClient, raw); break;
    case 'hcAddAgentInfos': await hcAddAgentInfos(ad4mClient, path); break;

    default:
      console.info(`Action "${argv.action}" is not defined on runtime.`)
      break;
  }

  process.exit();
};

async function getTrustedAgents(ad4mClient: Ad4mClient) {
  const result = await ad4mClient.runtime.getTrustedAgents();
  prettify(result);
}

async function addTrustedAgent(ad4mClient: Ad4mClient, did: string) {
  if (did) {
    const result = await ad4mClient.runtime.addTrustedAgents([did]);
    prettify(result);
    return;
  }

  console.info('Runtime addTrustedAgent action is missiong param <did>');
}

async function deleteTrustedAgent(ad4mClient: Ad4mClient, did: string) {
  if (did) {
    const result = await ad4mClient.runtime.deleteTrustedAgents([did]);
    prettify(result);
    return;
  }

  console.info('Runtime deleteTrustedAgent action is missiong param <did>');
}

async function knownLinkLanguageTemplates(ad4mClient: Ad4mClient) {
  const result = await ad4mClient.runtime.knownLinkLanguageTemplates();
  prettify(result);
}

async function addKnownLinkLanguageTemplate(ad4mClient: Ad4mClient, address: string) {
  if (address) {
    const result = await ad4mClient.runtime.addKnownLinkLanguageTemplates([address]);
    prettify(result);
    return;
  }

  console.info('Runtime addKnownLinkLanguageTemplate action is missiong param <address>');
}

async function removeKnownLinkLanguageTemplate(ad4mClient: Ad4mClient, address: string) {
  if (address) {
    const result = await ad4mClient.runtime.removeKnownLinkLanguageTemplates([address]);
    prettify(result);
    return;
  }

  console.info('Runtime removeKnownLinkLanguageTemplate action is missiong param <address>');
}

async function friends(ad4mClient: Ad4mClient) {
  const result = await ad4mClient.runtime.friends();
  prettify(result);
}

async function addFriend(ad4mClient: Ad4mClient, did: string) {
  if (did) {
    const result = await ad4mClient.runtime.addFriends([did]);
    prettify(result);
    return;
  }

  console.info('Runtime addFriend action is missiong param <did>');
}

async function removeFriend(ad4mClient: Ad4mClient, did: string) {
  if (did) {
    const result = await ad4mClient.runtime.removeFriends([did]);
    prettify(result);
    return;
  }

  console.info('Runtime removeFriend action is missiong param <did>');
}

async function hcAgentInfos(ad4mClient: Ad4mClient, raw: boolean) {
  const agentInfos = await ad4mClient.runtime.hcAgentInfos();

  if (raw) {
    prettify(agentInfos);
    return;
  }

  //@ts-ignore
  const agentInfosFormatted = JSON.parse(agentInfos).map(info => {
    return {
      agent: Buffer.from(info.agent.data),
      signature: Buffer.from(info.signature.data),
      agent_info: Buffer.from(info.agent_info.data)
    }
  })
  agentInfosFormatted.forEach(info => console.log("\n", info.agent_info.toString(), "\n"))
}

async function hcAddAgentInfos(ad4mClient: Ad4mClient, path: string) {
  if (path) {
    const agentInfos = fs.readFileSync(path).toString();
    const result = await ad4mClient.runtime.hcAddAgentInfos(agentInfos);
    prettify(result);
    return;
  }

  console.info('Runtime hcAddAgentInfos action is missiong param <path>');
}
