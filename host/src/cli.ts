#!/usr/bin/env node

import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';
import {
  command as serveCommand, desc as serveDesc,
  builder as serveBuilder, handler as serveHandler,
} from './commands/serve';
import {
  command as agentCommand, desc as agentDesc,
  builder as agentBuilder, handler as agentHandler,
} from './commands/client/agent';
import {
  command as languagesCommand, desc as languagesDesc,
  builder as languagesBuilder, handler as languagesHandler,
} from './commands/client/languages';
import {
  command as expressionCommand, desc as expressionDesc,
  builder as expressionBuilder, handler as expressionHandler,
} from './commands/client/expression';
import {
  command as perspectiveCommand, desc as perspectiveDesc,
  builder as perspectiveBuilder, handler as perspectiveHandler,
} from './commands/client/perspective';
import {
  command as neighbourhoodCommand, desc as neighbourhoodDesc,
  builder as neighbourhoodBuilder, handler as neighbourhoodHandler,
} from './commands/client/neighbourhood';
import {
  command as runtimeCommand, desc as runtimeDesc,
  builder as runtimeBuilder, handler as runtimeHandler,
} from './commands/client/runtime';
import {
  command as initCommand, desc as initDesc,
  builder as initBuilder, handler as initHandler,
} from './commands/init';

yargs(hideBin(process.argv))
  .command(initCommand, initDesc, initBuilder, initHandler)
  .command(serveCommand, serveDesc, serveBuilder, serveHandler)
  .command(agentCommand, agentDesc, agentBuilder, agentHandler)
  .command(languagesCommand, languagesDesc, languagesBuilder, languagesHandler)
  .command(expressionCommand, expressionDesc, expressionBuilder, expressionHandler)
  .command(perspectiveCommand, perspectiveDesc, perspectiveBuilder, perspectiveHandler)
  .command(neighbourhoodCommand, neighbourhoodDesc, neighbourhoodBuilder, neighbourhoodHandler)
  .command(runtimeCommand, runtimeDesc, runtimeBuilder, runtimeHandler)
  .options({
    server: {
      type: 'string',
      describe: 'Connect to this endpoint when request to ad4m service',
      default: 'ws://localhost:4000/graphql',
      alias: 's',
    },
    verbose: { type: "boolean", default: false, alias: 'v' },
  })
  // Enable strict mode.
  .strict()
  // Useful aliases.
  .alias({ h: 'help' })
  .fail((msg, err) => {
    console.error('Running command with error: ', msg, err);
    process.exit(1);
  })
  .argv;
