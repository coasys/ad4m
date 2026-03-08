/**
 * Node.js loader for extensionless ESM imports
 * Maintains compatibility with --es-module-specifier-resolution=node behavior
 * 
 * Usage: node --import ./loader-register.js lib/src/buildSchema.js
 */

import { readFileSync } from 'fs';
import { fileURLToPath, pathToFileURL } from 'url';
import { resolve as resolvePath, dirname, extname } from 'path';

const extensions = ['.js', '.json', '.node'];

export async function resolve(specifier, context, nextResolve) {
  // Skip bare module specifiers (packages)
  if (!specifier.startsWith('.') && !specifier.startsWith('/')) {
    return nextResolve(specifier, context);
  }

  // If already has extension, use as-is
  if (extname(specifier)) {
    return nextResolve(specifier, context);
  }

  // Try adding extensions
  const baseUrl = context.parentURL ? fileURLToPath(dirname(context.parentURL)) : process.cwd();
  const resolvedPath = resolvePath(baseUrl, specifier);

  for (const ext of extensions) {
    try {
      const pathWithExt = resolvedPath + ext;
      readFileSync(pathWithExt);
      return {
        url: pathToFileURL(pathWithExt).href,
        shortCircuit: true
      };
    } catch {
      // Try index files
      try {
        const indexPath = resolvePath(resolvedPath, 'index' + ext);
        readFileSync(indexPath);
        return {
          url: pathToFileURL(indexPath).href,
          shortCircuit: true
        };
      } catch {
        // Continue to next extension
      }
    }
  }

  // Fallback to default resolution
  return nextResolve(specifier, context);
}
