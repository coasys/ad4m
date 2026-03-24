#!/usr/bin/env node
// harness.ts — CLI entry point for AD4M test harness
//
// Usage:
//   npx tsx harness.ts <action> [--param value ...]
//   npx tsx harness.ts --list
//   npx tsx harness.ts --run executor/start:port=12000 agent/generate:executorId=exec-1

import { promises as fs } from 'node:fs';
import path from 'node:path';
import { Context } from './lib/context.js';
import type { Action, ActionResult } from './lib/types.js';

// Action registry — discovered from actions/ directory
const ACTION_DIR = path.join(path.dirname(new URL(import.meta.url).pathname), 'actions');

/** Discover all available actions by scanning the actions directory */
async function discoverActions(baseDir: string): Promise<Map<string, string>> {
  const actions = new Map<string, string>();

  async function scan(dir: string, prefix: string = '') {
    let entries: string[];
    try {
      entries = await fs.readdir(dir);
    } catch {
      return;
    }

    for (const entry of entries) {
      const fullPath = path.join(dir, entry);
      const stat = await fs.stat(fullPath);
      if (stat.isDirectory()) {
        await scan(fullPath, prefix ? `${prefix}/${entry}` : entry);
      } else if (entry.endsWith('.ts') && !entry.endsWith('.d.ts') || entry.endsWith('.js') && !entry.endsWith('.d.js')) {
        const name = entry.replace(/\.(ts|js)$/, '');
        const actionName = prefix ? `${prefix}/${name}` : name;
        actions.set(actionName, fullPath);
      }
    }
  }

  await scan(baseDir);
  return actions;
}

/** Load an action module */
async function loadAction(filePath: string): Promise<Action> {
  const mod = await import(filePath);
  return mod.default as Action;
}

/** Parse CLI params from --key value pairs */
function parseParams(args: string[]): Record<string, unknown> {
  const params: Record<string, unknown> = {};
  let i = 0;
  while (i < args.length) {
    const arg = args[i];
    if (arg.startsWith('--')) {
      const key = arg.slice(2);
      const next = args[i + 1];
      if (next === undefined || next.startsWith('--')) {
        params[key] = true;
        i++;
      } else {
        // Try to parse JSON values
        try {
          params[key] = JSON.parse(next);
        } catch {
          params[key] = next;
        }
        i += 2;
      }
    } else {
      i++;
    }
  }
  return params;
}

/** Parse --run chain format: action1:key=val,key2=val2 action2:key=val */
function parseRunChain(chain: string[]): Array<{ action: string; params: Record<string, unknown> }> {
  return chain.map(item => {
    const [action, paramStr] = item.split(':');
    const params: Record<string, unknown> = {};
    if (paramStr) {
      for (const pair of paramStr.split(',')) {
        const [key, value] = pair.split('=');
        try {
          params[key] = JSON.parse(value);
        } catch {
          params[key] = value;
        }
      }
    }
    return { action, params };
  });
}

/** Output structured result */
function output(result: ActionResult): void {
  process.stdout.write(JSON.stringify(result) + '\n');
}

/** Main */
async function main() {
  const args = process.argv.slice(2);

  // Discover actions
  const actionMap = await discoverActions(ACTION_DIR);

  // --list: show all available actions
  if (args.includes('--list')) {
    const list: Array<{ name: string; description: string; params: Record<string, unknown> }> = [];
    for (const [name, filePath] of actionMap) {
      try {
        const action = await loadAction(filePath);
        list.push({ name: action.name, description: action.description, params: action.params });
      } catch {
        list.push({ name, description: '(failed to load)', params: {} });
      }
    }
    output({ ok: true, data: { actions: list }, duration_ms: 0 });
    return;
  }

  // Create context with action resolver for ctx.run()
  const resolveAction = async (actionName: string, params: Record<string, unknown>): Promise<ActionResult> => {
    const filePath = actionMap.get(actionName);
    if (!filePath) {
      return { ok: false, error: `Action not found: ${actionName}`, duration_ms: 0 };
    }
    const action = await loadAction(filePath);
    return action.run(params, ctx);
  };

  const ctx = await Context.load({ actionResolver: resolveAction });

  // --run: chain multiple actions
  const runIdx = args.indexOf('--run');
  if (runIdx !== -1) {
    const chainArgs = args.slice(runIdx + 1);
    const chain = parseRunChain(chainArgs);
    const results: ActionResult[] = [];

    for (const { action: actionName, params } of chain) {
      const result = await resolveAction(actionName, params);
      results.push(result);
      await ctx.save();

      if (!result.ok) {
        output({ ok: false, data: { results }, error: `Chain failed at ${actionName}: ${result.error}`, duration_ms: 0 });
        process.exit(1);
      }
    }

    output({ ok: true, data: { results }, duration_ms: 0 });
    return;
  }

  // Single action: first non-flag argument
  const actionName = args.find(a => !a.startsWith('--'));
  if (!actionName) {
    console.error('Usage: harness.ts <action> [--param value ...] | --list | --run action1:k=v action2:k=v');
    process.exit(1);
  }

  const filePath = actionMap.get(actionName);
  if (!filePath) {
    output({ ok: false, error: `Action not found: ${actionName}. Use --list to see available actions.`, duration_ms: 0 });
    process.exit(1);
  }

  // Parse remaining params (skip action name)
  const paramArgs = args.filter(a => a !== actionName);

  // Support --params '{"key": "value"}' for complex params
  const paramsIdx = paramArgs.indexOf('--params');
  let params: Record<string, unknown>;
  if (paramsIdx !== -1 && paramArgs[paramsIdx + 1]) {
    params = JSON.parse(paramArgs[paramsIdx + 1]);
  } else {
    params = parseParams(paramArgs);
  }

  const action = await loadAction(filePath);
  const result = await action.run(params, ctx);
  await ctx.save();
  output(result);

  if (!result.ok) process.exit(1);
}

main().catch(err => {
  output({ ok: false, error: err instanceof Error ? err.message : String(err), duration_ms: 0 });
  process.exit(1);
});
