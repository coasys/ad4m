#!/usr/bin/env node
import { isLoopbackHost, readOperatorToken } from "./operator.js";
import { buildServer } from "./server.js";

interface CliArgs {
  port: number;
  host: string;
  dataDir: string;
  autoAdmit: boolean;
  operatorPort?: number;
  operatorHost: string;
  operatorTokenFile?: string;
}

const USAGE = `link-server — self-hostable link language server for AD4M

Usage:
  link-server [--port <number>] [--data <dir>] [--host <address>]

Options:
  --port <number>    Port to listen on (default: 3456)
  --data <dir>       Directory for the SQLite database (default: ./data)
  --host <address>   Address to bind (default: 0.0.0.0)
  --auto-admit       Admit every agent that authenticates (default: allowlist)
  --operator-port <number>
                     Also serve the operator admission UI and API on this
                     port (loopback only; off by default)
  --operator-host <address>
                     Loopback address for the operator listener (default: 127.0.0.1)
  --operator-token-file <path>
                     File (mode 600) holding the operator API token;
                     required with --operator-port
  -h, --help         Show this help

Each option also reads an environment variable: PORT, DATA_DIR, HOST,
AUTO_ADMIT, OPERATOR_PORT, OPERATOR_HOST, OPERATOR_TOKEN_FILE.
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
  let autoAdmit = envBool("AUTO_ADMIT");
  let operatorPort = process.env.OPERATOR_PORT ? parsePort("OPERATOR_PORT", process.env.OPERATOR_PORT) : undefined;
  let operatorHost = process.env.OPERATOR_HOST ?? "127.0.0.1";
  let operatorTokenFile = process.env.OPERATOR_TOKEN_FILE || undefined;

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
      case "--operator-port":
        operatorPort = parsePort("--operator-port", argv[++i]);
        break;
      case "--operator-host":
        operatorHost = requireValue("--operator-host", argv[++i]);
        break;
      case "--operator-token-file":
        operatorTokenFile = requireValue("--operator-token-file", argv[++i]);
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

  if (operatorPort !== undefined) {
    if (!operatorTokenFile) {
      console.error("--operator-port needs --operator-token-file (or OPERATOR_TOKEN_FILE)");
      process.exit(1);
    }
    if (!isLoopbackHost(operatorHost)) {
      console.error(`--operator-host must be a loopback address, not ${operatorHost}`);
      process.exit(1);
    }
  }

  return { port, host, dataDir, autoAdmit, operatorPort, operatorHost, operatorTokenFile };
}

function parsePort(name: string, value: string | undefined): number {
  const parsed = Number(value ?? "");
  if (!Number.isInteger(parsed) || parsed < 0 || parsed > 65535) {
    console.error(`Invalid ${name} value: ${value} (must be an integer 0-65535)`);
    process.exit(1);
  }
  return parsed;
}

function requireValue(name: string, value: string | undefined): string {
  if (!value || value.startsWith("-")) {
    console.error(`Missing value for ${name}`);
    process.exit(1);
  }
  return value;
}

async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));
  const operatorToken =
    args.operatorPort !== undefined ? readOperatorToken(args.operatorTokenFile!) : undefined;
  const { app, operatorApp } = await buildServer({
    dataDir: args.dataDir,
    autoAdmit: args.autoAdmit,
    logger: true,
    operator: operatorToken !== undefined ? { token: operatorToken } : undefined,
  });

  await app.listen({ port: args.port, host: args.host });
  app.log.info(`link-server listening on ${args.host}:${args.port} (data: ${args.dataDir})`);
  if (operatorApp) {
    await operatorApp.listen({ port: args.operatorPort!, host: args.operatorHost });
    app.log.info(`operator admission UI on ${args.operatorHost}:${args.operatorPort}`);
  }

  const shutdown = (signal: string) => {
    app.log.info(`received ${signal}, shutting down`);
    Promise.resolve(operatorApp?.close())
      .then(() => app.close())
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
