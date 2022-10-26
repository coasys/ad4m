import { Ad4mClient } from '@perspect3vism/ad4m';
import type { Arguments, Argv } from 'yargs';
import { buildAd4mClient, readPassphrase, prettify, CommonOptions } from './util';

export const command: string = 'agent [action]';
export const desc: string = 'Agent related action';

type Options = CommonOptions  & {
  passphrase?: string;
};
;

export const builder = (yargs: Argv) =>
  yargs
    .positional('action', {
      type: 'string',
      describe: 'Action that should be executed on the agent',
      choices: ['generate', 'lock', 'unlock', 'status', 'me'],
      default: 'status',
    }).options({
      passphrase: {type: 'string', describe: 'Password for the agent'}
    });

export const handler = async (argv: Arguments<Options>): Promise<void> => {
  const { server, verbose, action, passphrase } = argv;
  if (verbose) {
    console.info(`Attempting to connect to ${server}`);
  }
  const ad4mClient = buildAd4mClient(server);
  switch (action) {
    case 'generate': await generate(ad4mClient, passphrase); break;
    case 'lock': await lock(ad4mClient); break;
    case 'unlock': await unlock(ad4mClient, passphrase); break;
    case 'status': await status(ad4mClient); break;
    case 'me': await me(ad4mClient); break;

    default:
      console.info(`Action "${argv.action}" is not defined on agent.`)
      break;
  }

  process.exit();
};

async function generate(ad4mClient: Ad4mClient, password: string) {
  const passphrase = password || readPassphrase();
  const agentStatus = await ad4mClient.agent.generate(passphrase);
  prettify(agentStatus);
}

async function lock(ad4mClient: Ad4mClient) {
  // Passphrase not needed
  const agentStatus = await ad4mClient.agent.lock("");
  prettify(agentStatus);
}

async function unlock(ad4mClient: Ad4mClient, password: string) {
  const passphrase = password || readPassphrase();
  const agentStatus = await ad4mClient.agent.unlock(passphrase);
  prettify(agentStatus);
}

async function status(ad4mClient: Ad4mClient) {
  const agentStatus = await ad4mClient.agent.status();
  prettify(agentStatus);
}

async function me(ad4mClient: Ad4mClient) {
  const agent = await ad4mClient.agent.me();
  prettify(agent);
}
