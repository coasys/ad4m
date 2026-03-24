// lib/ssh.ts — Remote execution via SSH

import { execSync, spawn as nodeSpawn } from 'node:child_process';
import type { CommandRunner, SSHOptions, SpawnOptions } from './types.js';

export class SSHRunner implements CommandRunner {
  host: string;
  private user: string;
  private port: number;
  private keyPath?: string;

  constructor(opts: SSHOptions) {
    this.host = opts.host;
    this.user = opts.user ?? 'root';
    this.port = opts.port ?? 22;
    this.keyPath = opts.keyPath;
  }

  private get sshArgs(): string[] {
    const args = [
      '-o', 'StrictHostKeyChecking=no',
      '-o', 'UserKnownHostsFile=/dev/null',
      '-o', 'LogLevel=ERROR',
      '-p', String(this.port),
    ];
    if (this.keyPath) args.push('-i', this.keyPath);
    args.push(`${this.user}@${this.host}`);
    return args;
  }

  async exec(cmd: string, opts?: { cwd?: string; timeout?: number }): Promise<{ stdout: string; stderr: string; code: number }> {
    const remoteCmd = opts?.cwd ? `cd ${opts.cwd} && ${cmd}` : cmd;
    const fullArgs = [...this.sshArgs, remoteCmd];

    try {
      const stdout = execSync(`ssh ${fullArgs.map(a => `'${a}'`).join(' ')}`, {
        timeout: opts?.timeout ?? 120000,
        encoding: 'utf-8',
        stdio: ['pipe', 'pipe', 'pipe'],
        maxBuffer: 50 * 1024 * 1024,
      });
      return { stdout, stderr: '', code: 0 };
    } catch (err: unknown) {
      const e = err as { stdout?: string; stderr?: string; status?: number };
      return {
        stdout: e.stdout ?? '',
        stderr: e.stderr ?? '',
        code: e.status ?? 1,
      };
    }
  }

  async spawn(cmd: string, opts?: SpawnOptions): Promise<number> {
    const remoteCmd = opts?.cwd ? `cd ${opts.cwd} && nohup ${cmd} &` : `nohup ${cmd} &`;
    const fullArgs = [...this.sshArgs, remoteCmd];

    const proc = nodeSpawn('ssh', fullArgs, {
      detached: true,
      stdio: 'ignore',
    });

    if (!proc.pid) throw new Error(`Failed to spawn remote process: ${cmd}`);
    proc.unref();

    // Get the remote PID
    const result = await this.exec(`pgrep -f "${cmd.slice(0, 40)}"`);
    const remotePid = parseInt(result.stdout.trim().split('\n').pop() ?? '0', 10);

    return remotePid || proc.pid;
  }

  /** Copy a file to the remote host */
  async copyTo(localPath: string, remotePath: string): Promise<void> {
    const args = [
      '-o', 'StrictHostKeyChecking=no',
      '-o', 'UserKnownHostsFile=/dev/null',
      '-o', 'LogLevel=ERROR',
      '-P', String(this.port),
    ];
    if (this.keyPath) args.push('-i', this.keyPath);
    args.push(localPath, `${this.user}@${this.host}:${remotePath}`);

    execSync(`scp ${args.map(a => `'${a}'`).join(' ')}`, {
      timeout: 120000,
      encoding: 'utf-8',
    });
  }

  /** Copy a file from the remote host */
  async copyFrom(remotePath: string, localPath: string): Promise<void> {
    const args = [
      '-o', 'StrictHostKeyChecking=no',
      '-o', 'UserKnownHostsFile=/dev/null',
      '-o', 'LogLevel=ERROR',
      '-P', String(this.port),
    ];
    if (this.keyPath) args.push('-i', this.keyPath);
    args.push(`${this.user}@${this.host}:${remotePath}`, localPath);

    execSync(`scp ${args.map(a => `'${a}'`).join(' ')}`, {
      timeout: 120000,
      encoding: 'utf-8',
    });
  }
}

/** Create a command runner — local or SSH based on host string */
export function createRunner(host: string, opts?: Omit<SSHOptions, 'host'>): CommandRunner {
  if (host === 'local' || host === 'localhost') {
    // Lazy import to avoid circular dependency
    const { LocalRunner } = require('./process.js');
    return new LocalRunner();
  }

  // Parse user@host:port format
  let user = opts?.user;
  let hostname = host;
  let port = opts?.port;

  if (hostname.includes('@')) {
    const parts = hostname.split('@');
    user = parts[0];
    hostname = parts[1];
  }
  if (hostname.includes(':')) {
    const parts = hostname.split(':');
    hostname = parts[0];
    port = parseInt(parts[1], 10);
  }

  return new SSHRunner({ host: hostname, user, port, keyPath: opts?.keyPath });
}
