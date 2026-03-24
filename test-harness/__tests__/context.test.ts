import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { Context } from '../lib/context.js';
import { promises as fs } from 'node:fs';
import path from 'node:path';
import os from 'node:os';

describe('Context', () => {
  let sessionPath: string;

  beforeEach(() => {
    sessionPath = path.join(os.tmpdir(), `harness-test-${Date.now()}.json`);
  });

  afterEach(async () => {
    try { await fs.unlink(sessionPath); } catch {}
  });

  it('should create fresh context', async () => {
    const ctx = await Context.load({ sessionPath });
    expect(ctx.resources.size).toBe(0);
    expect(ctx.state.size).toBe(0);
  });

  it('should generate sequential IDs', async () => {
    const ctx = await Context.load({ sessionPath });
    expect(ctx.nextId('exec')).toBe('exec-1');
    expect(ctx.nextId('exec')).toBe('exec-2');
    expect(ctx.nextId('browser')).toBe('browser-1');
  });

  it('should track ports sequentially', async () => {
    const ctx = await Context.load({ sessionPath });
    const p1 = ctx.nextPort(12000);
    const p2 = ctx.nextPort();
    expect(p2).toBe(p1 + 1);
  });

  it('should persist and restore resources', async () => {
    const ctx = await Context.load({ sessionPath });
    ctx.resources.set('exec-1', {
      kind: 'executor', id: 'exec-1', pid: 1234, port: 12000, host: 'local', dataDir: '/tmp/test',
    });
    ctx.state.set('testKey', 'testValue');
    ctx.nextId('exec'); // counter=1
    await ctx.save();

    const ctx2 = await Context.load({ sessionPath });
    expect(ctx2.resources.size).toBe(1);
    expect(ctx2.executor('exec-1')?.port).toBe(12000);
    expect(ctx2.state.get('testKey')).toBe('testValue');
    expect(ctx2.nextId('exec')).toBe('exec-2'); // counter restored
  });

  it('should filter resources by kind', async () => {
    const ctx = await Context.load({ sessionPath });
    ctx.resources.set('exec-1', { kind: 'executor', id: 'exec-1', pid: 1, port: 12000, host: 'local' });
    ctx.resources.set('nh-1', { kind: 'neighbourhood', id: 'nh-1', url: 'nh://test', perspectiveUuid: 'abc', executorId: 'exec-1' });
    ctx.resources.set('browser-1', { kind: 'browser', id: 'browser-1', url: 'http://localhost', executorId: 'exec-1' });

    expect(ctx.executors()).toHaveLength(1);
    expect(ctx.neighbourhoods()).toHaveLength(1);
    expect(ctx.browsers()).toHaveLength(1);
  });

  it('should serialize to JSON correctly', async () => {
    const ctx = await Context.load({ sessionPath });
    ctx.resources.set('exec-1', { kind: 'executor', id: 'exec-1', pid: 1, port: 12000, host: 'local' });
    ctx.state.set('key', 'val');

    const json = ctx.toJSON();
    expect(json.resources['exec-1']).toBeDefined();
    expect(json.resources['exec-1'].kind).toBe('executor');
    expect(json.state['key']).toBe('val');
    expect(typeof json.portCounter).toBe('number');
  });

  it('should throw when ctx.run called without resolver', async () => {
    const ctx = await Context.load({ sessionPath });
    await expect(ctx.run('test/action', {})).rejects.toThrow('No action resolver');
  });

  it('should call action resolver via ctx.run', async () => {
    const resolver = async (action: string, params: Record<string, unknown>) => ({
      ok: true,
      data: { action, params },
      duration_ms: 0,
    });
    const ctx = await Context.load({ sessionPath, actionResolver: resolver });
    const result = await ctx.run('test/action', { key: 'val' });
    expect(result.ok).toBe(true);
    expect(result.data?.action).toBe('test/action');
  });
});
