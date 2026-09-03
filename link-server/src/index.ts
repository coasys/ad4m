#!/usr/bin/env node
import { buildServer } from "./server.js";

interface CliArgs {
  port: number;
  host: string;
  dataDir: string;
  selfUrl?: string;
  autoAdmit: boolean;
}

const USAGE = `link-server — self-hostable link language server for AD4M

Usage:
  link-server [--port <number>] [--data <dir>] [--host <address>] [--self-url <url>]

Options:
  --port <number>    Port to listen on (default: 3456)
  --data <dir>       Directory for the SQLite database (default: ./data)
  --host <address>   Address to bind (default: 0.0.0.0)
  --self-url <url>   This server's externally-reachable base URL, advertised to federation peers
  -h, --help         Show this help
`;

function envBool(key: string): boolean {
  const val = process.env[key];
  return val === "true" || val === "1";
}

function parseArgs(argv: string[]): CliArgs {
  // Environment variables provide defaults; CLI args override.
  let port = Number(process.env.PORT ?? "3456");
  if (!Number.isInteger(port) || port < 0 || port > 65535) {
    if (process.env.PORT) {
      console.error(`[link-server] PORT="${process.env.PORT}" is not a valid port number; falling back to 3456`);
    }
    port = 3456;
  }
  let host = process.env.HOST ?? "0.0.0.0";
  let dataDir = process.env.DATA_DIR ?? "./data";
  let selfUrl: string | undefined = process.env.SELF_URL;
  let autoAdmit = envBool("AUTO_ADMIT");

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    switch (arg) {
      case "--port": {
        const value = argv[++i];
        const parsed = Number(value ?? "");
        if (!Number.isInteger(parsed) || parsed < 0 || parsed > 65535) {
          console.error(`Invalid --port value: ${value} (must be an integer 0-65535)`);
          process.exit(1);
        }
        port = parsed;
        break;
      }
      case "--host": {
        const value = argv[++i];
        if (!value || value.startsWith("-")) {
          console.error(`Missing value for --host`);
          process.exit(1);
        }
        host = value;
        break;
      }
      case "--data": {
        const value = argv[++i];
        if (!value || value.startsWith("-")) {
          console.error(`Missing value for --data`);
          process.exit(1);
        }
        dataDir = value;
        break;
      }
      case "--self-url": {
        const value = argv[++i];
        if (!value || value.startsWith("-")) {
          console.error(`Missing value for --self-url`);
          process.exit(1);
        }
        selfUrl = value;
        break;
      }
      case "--auto-admit":
        autoAdmit = true;
        break;
      case "-h":
      case "--help":
        console.log(USAGE);
        process.exit(0);
        break;
      default:
        console.error(`Unknown argument: ${arg}\n`);
        console.log(USAGE);
        process.exit(1);
    }
  }

  return { port, host, dataDir, selfUrl, autoAdmit };
}

async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));
  const { app } = await buildServer({
    dataDir: args.dataDir,
    selfUrl: args.selfUrl,
    autoAdmit: args.autoAdmit,
    logger: true,
  });

  await app.listen({ port: args.port, host: args.host });
  app.log.info(`link-server listening on ${args.host}:${args.port} (data: ${args.dataDir})`);

  const shutdown = (signal: string) => {
    app.log.info(`received ${signal}, shutting down`);
    app
      .close()
      .then(() => process.exit(0))
      .catch((err) => {
        console.error(err);
        process.exit(1);
      });
  };
  process.on("SIGINT", () => shutdown("SIGINT"));
  process.on("SIGTERM", () => shutdown("SIGTERM"));
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
