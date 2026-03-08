#!/usr/bin/env node
/**
 * Script to add .js extensions to all relative imports in compiled JS files
 * Required for Node 24+ ESM compatibility
 */

import { readFileSync, writeFileSync, readdirSync, statSync } from 'fs';
import { join, extname } from 'path';

const srcDir = process.argv[2] || './lib/src';

function processFile(filePath) {
  let content = readFileSync(filePath, 'utf-8');
  let modified = false;
  
  // Match relative imports without extensions
  // import X from "./path/to/file"
  const importRegex = /from\s+["'](\.\.?\/[^"']+?)(?!\.js)["']/g;
  
  content = content.replace(importRegex, (match, importPath) => {
    // Skip if already has an extension
    if (importPath.match(/\.\w+$/)) {
      return match;
    }
    // Add .js extension
    modified = true;
    return match.replace(importPath, importPath + '.js');
  });
  
  // Also match dynamic imports
  // import("./path/to/file")
  const dynamicImportRegex = /import\s*\(\s*["'](\.\.?\/[^"']+?)(?!\.js)["']\s*\)/g;
  
  content = content.replace(dynamicImportRegex, (match, importPath) => {
    if (importPath.match(/\.\w+$/)) {
      return match;
    }
    modified = true;
    return match.replace(importPath, importPath + '.js');
  });
  
  if (modified) {
    writeFileSync(filePath, content);
    console.log(`Fixed: ${filePath}`);
  }
}

function walkDir(dir) {
  const files = readdirSync(dir);
  
  for (const file of files) {
    const filePath = join(dir, file);
    const stat = statSync(filePath);
    
    if (stat.isDirectory()) {
      walkDir(filePath);
    } else if (extname(file) === '.js') {
      processFile(filePath);
    }
  }
}

console.log(`Processing JS files in ${srcDir}...`);
walkDir(srcDir);
console.log('Done!');
