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
  let port = Number.parseInt(process.env.PORT ?? "3456", 10);
  if (!Number.isFinite(port)) port = 3456;
  let host = process.env.HOST ?? "0.0.0.0";
  let dataDir = process.env.DATA_DIR ?? "./data";
  let selfUrl: string | undefined = process.env.SELF_URL;
  let autoAdmit = envBool("AUTO_ADMIT");

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    switch (arg) {
      case "--port": {
        const value = argv[++i];
        const parsed = Number.parseInt(value ?? "", 10);
        if (!Number.isFinite(parsed)) {
          console.error(`Invalid --port value: ${value}`);
          process.exit(1);
        }
        port = parsed;
        break;
      }
      case "--host":
        host = argv[++i];
        break;
      case "--data":
        dataDir = argv[++i];
        break;
      case "--self-url":
        selfUrl = argv[++i];
        break;
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
    port: args.port,
    host: args.host,
    selfUrl: args.selfUrl,
    autoAdmit: args.autoAdmit,
    logger: true,
  });

  await app.listen({ port: args.port, host: args.host });
  app.log.info(`ADAM Simple Server listening on ${args.host}:${args.port} (data: ${args.dataDir})`);

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
