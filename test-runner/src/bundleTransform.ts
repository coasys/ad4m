/**
 * Bundle Transform Utilities
 *
 * Converts CJS system language bundles to ESM for Deno runtime compatibility,
 * and patches out removed IPFS references from the Language Language bundle.
 *
 * Background: System language bundles downloaded from perspect3vism repos are
 * CommonJS, but the AD4M executor's Deno runtime requires ESM. Additionally,
 * the Language Language bundle still references `context.IPFS.add()` which was
 * removed from the executor, causing crashes during language publishing.
 *
 * @module bundleTransform
 */

import { logger } from './utils';

/** Node built-in modules that need 'node:' prefix in Deno */
const NODE_BUILTINS = new Set([
  'assert', 'buffer', 'child_process', 'cluster', 'console', 'constants',
  'crypto', 'dgram', 'dns', 'domain', 'events', 'fs', 'http', 'https',
  'module', 'net', 'os', 'path', 'perf_hooks', 'process', 'punycode',
  'querystring', 'readline', 'repl', 'stream', 'string_decoder', 'sys',
  'timers', 'tls', 'tty', 'url', 'util', 'v8', 'vm', 'worker_threads', 'zlib'
]);

/**
 * Detect whether a bundle is CJS (contains require() or exports assignments).
 */
export function isCjsBundle(code: string): boolean {
  return /\brequire\s*\(/.test(code) || /\bexports\s*[.[]/.test(code);
}

/**
 * Convert a CJS bundle to ESM.
 *
 * Handles:
 * - `var x = require('module')` → `import x from 'module'`
 * - `exports.name = value` → `export { value as name }`
 * - `exports.default = value` / `exports["default"] = value` → `export default value`
 * - Node builtin requires get `node:` prefix for Deno compatibility
 * - Strips `'use strict'`, `Object.defineProperty(exports, '__esModule', ...)`, sourcemaps
 */
export function convertCjsToEsm(code: string): string {
  let esm = code;

  // Strip CJS boilerplate
  esm = esm.replace(/^'use strict';\s*/m, '');
  esm = esm.replace(/Object\.defineProperty\(exports,\s*'__esModule'.*?\);\s*/g, '');

  // Extract require() calls → import statements
  const requires: Array<{ varName: string; modName: string }> = [];
  esm = esm.replace(
    /(?:var|let|const)\s+(\w+)\s*=\s*require\('([^']+)'\);?/g,
    (_match, varName, modName) => {
      const resolvedMod = NODE_BUILTINS.has(modName) ? `node:${modName}` : modName;
      requires.push({ varName, modName: resolvedMod });
      return '';
    }
  );

  // Extract exports → export statements
  const namedExports = new Set<string>();

  esm = esm.replace(/exports\["default"\]\s*=\s*(\w+);/g, (_m, name) => {
    namedExports.add(`default:${name}`);
    return '';
  });
  esm = esm.replace(/exports\.default\s*=\s*(\w+);/g, (_m, name) => {
    namedExports.add(`default:${name}`);
    return '';
  });
  esm = esm.replace(/exports\.(\w+)\s*=\s*(\w+);/g, (_m, exportName, localName) => {
    namedExports.add(`named:${exportName}:${localName}`);
    return '';
  });

  // Strip sourcemaps
  esm = esm.replace(/\/\/# sourceMappingURL=.*$/m, '');

  // Build import block
  const imports = requires.map(r => `import ${r.varName} from '${r.modName}';`).join('\n');

  // Build export block
  const exportLines: string[] = [];
  for (const exp of namedExports) {
    if (exp.startsWith('default:')) {
      exportLines.push(`export default ${exp.split(':')[1]};`);
    } else {
      const [, exportName, localName] = exp.split(':');
      if (exportName === localName) {
        exportLines.push(`export { ${exportName} };`);
      } else {
        exportLines.push(`export { ${localName} as ${exportName} };`);
      }
    }
  }

  return `${imports}\n${esm}\n${exportLines.join('\n')}\n`;
}

/**
 * Patch out IPFS references from the Language Language bundle.
 *
 * The Language Language's PutAdapter originally used `context.IPFS.add()` to
 * content-address language bundles, but IPFS was removed from the executor.
 * This patches the compiled bundle to skip the IPFS verification and use the
 * address from language.meta directly.
 */
export function patchIpfsReferences(code: string): string {
  let patched = code;

  // Remove the IPFS.add() call and hash comparison
  const ipfsPatterns = [
    'const ipfsAddress = await __classPrivateFieldGet$1(this, _PutAdapter_IPFS, "f").add({ content: language.bundle.toString() }, { onlyHash: true });',
    '// @ts-ignore',
    'const hash = ipfsAddress.cid.toString();',
    'if (hash != language.meta.address)',
  ];

  for (const pattern of ipfsPatterns) {
    patched = patched.split(pattern).join('');
  }

  // Replace the error throw with a direct address assignment
  patched = patched.replace(
    /throw new Error\(`Language Persistence: Can't store language[^`]*`\);/,
    'const hash = language.meta.address;'
  );

  return patched;
}

/**
 * Transform a bundle: CJS→ESM conversion + IPFS patching if needed.
 *
 * @param code - The raw bundle source
 * @param name - Bundle name for logging
 * @param isLanguageLanguage - Whether this is the Language Language bundle (needs IPFS patching)
 * @returns Transformed bundle, or original if no transform needed
 */
export function transformBundle(code: string, name: string, isLanguageLanguage = false): string {
  let result = code;
  let transformed = false;

  if (isCjsBundle(result)) {
    result = convertCjsToEsm(result);
    transformed = true;
    logger.info(`Converted ${name} bundle from CJS to ESM`);
  }

  if (isLanguageLanguage) {
    result = patchIpfsReferences(result);
    logger.info(`Patched IPFS references in ${name} bundle`);
    transformed = true;
  }

  if (!transformed) {
    logger.info(`${name} bundle already ESM-compatible, no transform needed`);
  }

  return result;
}
