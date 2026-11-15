#!/usr/bin/env node
import { publicOrigin } from "./auth.js";
import { isLoopbackHost, readOperatorToken } from "./operator.js";
import { buildServer } from "./server.js";

interface CliArgs {
  port: number;
  host: string;
  dataDir: string;
  autoAdmit: boolean;
  /** The origins auth is signed for (normalised); empty on a development server taking Host's. */
  publicOrigins: string[];
  hostOrigin: boolean;
  name?: string;
  storeUrl?: string;
  storeHeadKeys: string[];
  /** How often admitted keys are asked of the store again (ms); the server's default when unset. */
  refreshIntervalMs?: number;
  operatorPort?: number;
  operatorHost: string;
  operatorTokenFile?: string;
  operatorOrigins: string[];
  operatorLabel?: string;
}

const USAGE = `link-server — self-hostable link language server for AD4M

Usage:
  link-server [--port <number>] [--data <dir>] [--host <address>]

Options:
  --port <number>    Port to listen on (default: 3456)
  --data <dir>       Directory for the SQLite database (default: ./data)
  --host <address>   Address to bind (default: 0.0.0.0)
  --public-origin <origin>
                     The origin clients reach this server at, e.g.
                     https://links.example.org (scheme, host and port; the
                     default port and a trailing slash are dropped). Each auth
                     signs it, so a signature made for another server is
                     refused. Repeatable. Required, except on a development
                     server (ALLOW_AUTO_ADMIT_DEV=true, not NODE_ENV=production),
                     which without it takes the origin each request's Host
                     header names
  --name <name>      The server's name in /.well-known/coasys-link-server
                     (default: link-server)
  --store-url <url>  The identity store's base URL: whom each key speaks for.
                     Without it no person can be admitted to a room
  --store-head-keys <multikey,...>
                     The store keys whose signed heads are trusted
  --refresh-interval-ms <number>
                     How often every admitted key is asked of the store
                     again, so a revoked key loses its sockets (4005)
                     (default: 600000, 10 minutes; at least 1000)
  --auto-admit       Admit every key that authenticates, member or not
                     (tests and development only: refused unless
                     ALLOW_AUTO_ADMIT_DEV=true, and always with
                     NODE_ENV=production). Default: by the room's membership
  --operator-port <number>
                     Also serve the operator admission UI and API on this
                     port (loopback only; off by default)
  --operator-host <address>
                     Loopback address for the operator listener (default: 127.0.0.1)
  --operator-token-file <path>
                     File (mode 600) holding the operator API token;
                     required with --operator-port
  --operator-origin <origin>
                     Origin the admission page is served from (e.g.
                     https://admin.example.org); POSTs from any other
                     origin are refused. Repeatable; required with --operator-port
  --operator-label <name>
                     Instance name shown on the admission page (e.g. staging)
  -h, --help         Show this help

Each option also reads an environment variable: PORT, DATA_DIR, HOST,
PUBLIC_ORIGIN (comma-separated), NAME, STORE_URL, STORE_HEAD_KEYS (comma-separated),
REFRESH_INTERVAL_MS, AUTO_ADMIT, OPERATOR_PORT,
OPERATOR_HOST, OPERATOR_TOKEN_FILE, OPERATOR_ORIGINS (comma-separated),
OPERATOR_LABEL.
`;

/** The flag that marks a development server, the only kind that may auto-admit. */
const ALLOW_AUTO_ADMIT_ENV = "ALLOW_AUTO_ADMIT_DEV";

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
  const origins = commaList(process.env.PUBLIC_ORIGIN);
  let name = process.env.NAME || undefined;
  let storeUrl = process.env.STORE_URL || undefined;
  let storeHeadKeys = commaList(process.env.STORE_HEAD_KEYS);
  let refreshIntervalMs = process.env.REFRESH_INTERVAL_MS
    ? parseRefreshInterval("REFRESH_INTERVAL_MS", process.env.REFRESH_INTERVAL_MS)
    : undefined;
  // The operator port must be fixed: a reverse proxy points at it.
  let operatorPort = process.env.OPERATOR_PORT ? parsePort("OPERATOR_PORT", process.env.OPERATOR_PORT, 1) : undefined;
  let operatorHost = process.env.OPERATOR_HOST ?? "127.0.0.1";
  let operatorTokenFile = process.env.OPERATOR_TOKEN_FILE || undefined;
  const operatorOrigins = commaList(process.env.OPERATOR_ORIGINS);
  let operatorLabel = process.env.OPERATOR_LABEL || undefined;

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
        operatorPort = parsePort("--operator-port", argv[++i], 1);
        break;
      case "--operator-host":
        operatorHost = requireValue("--operator-host", argv[++i]);
        break;
      case "--operator-token-file":
        operatorTokenFile = requireValue("--operator-token-file", argv[++i]);
        break;
      case "--operator-origin":
        operatorOrigins.push(requireValue("--operator-origin", argv[++i]));
        break;
      case "--operator-label":
        operatorLabel = requireValue("--operator-label", argv[++i]);
        break;
      case "--auto-admit":
        autoAdmit = true;
        break;
      case "--public-origin":
        origins.push(requireValue("--public-origin", argv[++i]));
        break;
      case "--name":
        name = requireValue("--name", argv[++i]);
        break;
      case "--store-url":
        storeUrl = requireValue("--store-url", argv[++i]);
        break;
      case "--store-head-keys":
        storeHeadKeys = commaList(requireValue("--store-head-keys", argv[++i]));
        break;
      case "--refresh-interval-ms":
        refreshIntervalMs = parseRefreshInterval("--refresh-interval-ms", argv[++i]);
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

  // Auto-admit lets every key into every room: only on a server explicitly marked as a
  // development one. Fail closed: an image or unit that forgot NODE_ENV is not a dev server.
  if (autoAdmit && !envBool(ALLOW_AUTO_ADMIT_ENV)) {
    console.error(
      `AUTO_ADMIT (--auto-admit) admits every key to every room; it is refused unless ${ALLOW_AUTO_ADMIT_ENV}=true marks this a development server`
    );
    process.exit(1);
  }
  if (autoAdmit && process.env.NODE_ENV === "production") {
    console.error("AUTO_ADMIT (--auto-admit) is refused when NODE_ENV=production, even with " + ALLOW_AUTO_ADMIT_ENV);
    process.exit(1);
  }

  // The origin auth is signed for. Without one, only a server marked for development starts,
  // taking the Host header's origin: fail closed, as for auto-admit.
  const publicOrigins: string[] = [];
  for (const origin of origins) {
    try {
      publicOrigins.push(publicOrigin(origin));
    } catch (e) {
      console.error(`PUBLIC_ORIGIN (--public-origin): ${(e as Error).message}`);
      process.exit(1);
    }
  }
  const devServer = envBool(ALLOW_AUTO_ADMIT_ENV) && process.env.NODE_ENV !== "production";
  if (publicOrigins.length === 0 && !devServer) {
    console.error(
      "PUBLIC_ORIGIN (--public-origin) is required: the origin clients reach this server at, e.g. " +
        `https://links.example.org. Only a development server (${ALLOW_AUTO_ADMIT_ENV}=true, not NODE_ENV=production) starts without it`
    );
    process.exit(1);
  }
  const hostOrigin = publicOrigins.length === 0;

  if (operatorPort !== undefined) {
    if (!operatorTokenFile) {
      console.error("--operator-port needs --operator-token-file (or OPERATOR_TOKEN_FILE)");
      process.exit(1);
    }
    if (operatorOrigins.length === 0) {
      console.error("--operator-port needs --operator-origin (or OPERATOR_ORIGINS)");
      process.exit(1);
    }
    if (!isLoopbackHost(operatorHost)) {
      console.error(`--operator-host must be a loopback address, not ${operatorHost}`);
      process.exit(1);
    }
  }

  return {
    port, host, dataDir, autoAdmit, publicOrigins, hostOrigin, name, storeUrl, storeHeadKeys, refreshIntervalMs,
    operatorPort, operatorHost, operatorTokenFile, operatorOrigins, operatorLabel,
  };
}

function commaList(value: string | undefined): string[] {
  return (value ?? "").split(",").map((v) => v.trim()).filter(Boolean);
}

function parsePort(name: string, value: string | undefined, min = 0): number {
  const parsed = Number(value ?? "");
  if (!Number.isInteger(parsed) || parsed < min || parsed > 65535) {
    console.error(`Invalid ${name} value: ${value} (must be an integer ${min}-65535)`);
    process.exit(1);
  }
  return parsed;
}

/** A refresh interval in ms, at least 1000 (a shorter one would ask the store for every key
 * many times a second). */
function parseRefreshInterval(name: string, value: string | undefined): number {
  const parsed = Number(value ?? "");
  if (!Number.isInteger(parsed) || parsed < 1000) {
    console.error(`Invalid ${name} value: ${value} (must be an integer number of milliseconds, at least 1000)`);
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
    publicOrigins: args.publicOrigins,
    hostOrigin: args.hostOrigin,
    name: args.name,
    storeUrl: args.storeUrl,
    storeHeadKeys: args.storeHeadKeys,
    refreshIntervalMs: args.refreshIntervalMs,
    logger: true,
    operator:
      operatorToken !== undefined
        ? { token: operatorToken, allowedOrigins: args.operatorOrigins, label: args.operatorLabel }
        : undefined,
  });

  await app.listen({ port: args.port, host: args.host });
  app.log.info(`link-server listening on ${args.host}:${args.port} (data: ${args.dataDir})`);
  if (operatorApp) {
    await operatorApp.listen({ port: args.operatorPort!, host: args.operatorHost });
    app.log.info(
      `operator admission UI${args.operatorLabel ? ` (${args.operatorLabel})` : ""} on ${args.operatorHost}:${args.operatorPort}`
    );
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
