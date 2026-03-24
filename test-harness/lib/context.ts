// lib/context.ts — Dynamic session state with persistence

import { promises as fs } from 'node:fs';
import path from 'node:path';
import type {
  HarnessContext,
  Resource,
  ExecutorResource,
  BrowserResource,
  NeighbourhoodResource,
  SessionData,
  ActionResult,
} from './types.js';

const DEFAULT_SESSION_PATH = path.join(process.cwd(), 'session.json');

export class Context implements HarnessContext {
  resources: Map<string, Resource> = new Map();
  state: Map<string, unknown> = new Map();
  private counters: Map<string, number> = new Map();
  private _portCounter: number = 12000;
  private sessionPath: string;
  private actionResolver?: (action: string, params: Record<string, unknown>) => Promise<ActionResult>;

  constructor(opts?: { sessionPath?: string; actionResolver?: (a: string, p: Record<string, unknown>) => Promise<ActionResult> }) {
    this.sessionPath = opts?.sessionPath ?? DEFAULT_SESSION_PATH;
    this.actionResolver = opts?.actionResolver;
  }

  /** Load session from disk */
  static async load(opts?: { sessionPath?: string; actionResolver?: (a: string, p: Record<string, unknown>) => Promise<ActionResult> }): Promise<Context> {
    const ctx = new Context(opts);
    const filePath = ctx.sessionPath;
    try {
      const raw = await fs.readFile(filePath, 'utf-8');
      const data: SessionData = JSON.parse(raw);
      for (const [k, v] of Object.entries(data.resources ?? {})) {
        ctx.resources.set(k, v);
      }
      for (const [k, v] of Object.entries(data.state ?? {})) {
        ctx.state.set(k, v);
      }
      for (const [k, v] of Object.entries(data.counters ?? {})) {
        ctx.counters.set(k, v);
      }
      ctx._portCounter = data.portCounter ?? 12000;
    } catch {
      // No existing session — start fresh
    }
    return ctx;
  }

  executor(id: string): ExecutorResource | undefined {
    const r = this.resources.get(id);
    return r?.kind === 'executor' ? r as ExecutorResource : undefined;
  }

  executors(): ExecutorResource[] {
    return [...this.resources.values()].filter((r): r is ExecutorResource => r.kind === 'executor');
  }

  browser(id: string): BrowserResource | undefined {
    const r = this.resources.get(id);
    return r?.kind === 'browser' ? r as BrowserResource : undefined;
  }

  browsers(): BrowserResource[] {
    return [...this.resources.values()].filter((r): r is BrowserResource => r.kind === 'browser');
  }

  neighbourhood(id: string): NeighbourhoodResource | undefined {
    const r = this.resources.get(id);
    return r?.kind === 'neighbourhood' ? r as NeighbourhoodResource : undefined;
  }

  neighbourhoods(): NeighbourhoodResource[] {
    return [...this.resources.values()].filter((r): r is NeighbourhoodResource => r.kind === 'neighbourhood');
  }

  nextId(prefix: string): string {
    const count = (this.counters.get(prefix) ?? 0) + 1;
    this.counters.set(prefix, count);
    return `${prefix}-${count}`;
  }

  nextPort(base?: number): number {
    if (base !== undefined && base > this._portCounter) {
      this._portCounter = base;
    }
    return this._portCounter++;
  }

  async run(action: string, params: Record<string, unknown>): Promise<ActionResult> {
    if (!this.actionResolver) {
      throw new Error('No action resolver configured — cannot use ctx.run()');
    }
    return this.actionResolver(action, params);
  }

  async save(): Promise<void> {
    const data = this.toJSON();
    await fs.writeFile(this.sessionPath, JSON.stringify(data, null, 2), 'utf-8');
  }

  toJSON(): SessionData {
    const resources: Record<string, Resource> = {};
    for (const [k, v] of this.resources) resources[k] = v;
    const state: Record<string, unknown> = {};
    for (const [k, v] of this.state) state[k] = v;
    const counters: Record<string, number> = {};
    for (const [k, v] of this.counters) counters[k] = v;
    return { resources, state, counters, portCounter: this._portCounter };
  }
}
