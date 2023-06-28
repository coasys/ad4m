import { LinkSyncAdapter, PerspectiveDiffObserver, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState, AgentService } from "@perspect3vism/ad4m";
import type { SyncStateChangeObserver } from "@perspect3vism/ad4m";
import { name } from "./index";

class PeerInfo {
  currentRevision: Buffer;
  lastSeen: Date;
};

export class LinkAdapter implements LinkSyncAdapter {
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  peers: Map<DID, PeerInfo> = new Map();
  peersMutex: Mutex = new Mutex();
  currentRevisionMutex: Mutex = new Mutex();
  me: DID
  gossipLogCount: number = 0;
  myCurrentRevision: Buffer | null = null;
  gun: any
  agentService: AgentService;

  constructor(context: LanguageContext) {
    //@ts-ignore
    this.gun = context.gun;
    this.me = context.agent.did;
    this.agentService = context.agent;
  }

  writable(): boolean {
    return true;
  }

  public(): boolean {
    return false;
  }

  async others(): Promise<DID[]> {
    return [];
  }

  async currentRevision(): Promise<string> {
    let links = this.gun.get(name);
    console.log("Gun db got links from currentRevision", links);
    return "";
  }

  async sync(): Promise<PerspectiveDiff> {
    let links = this.gun.get(name);
    console.log("Gun db got links from sync", links);
    return new PerspectiveDiff()
  }

  async gossip() {
  }

  async render(): Promise<Perspective> {
    let links = this.gun.get(name);
    console.log("Gun db got links", links);
    return new Perspective();
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    let diffExp = this.agentService.createSignedExpression(diff);
    this.gun.get(name).put(diffExp);
    return diffExp.proof.signature;
  }

  addCallback(callback: PerspectiveDiffObserver): number {
    this.linkCallback = callback;
    return 1;
  }

  addSyncStateChangeCallback(callback: SyncStateChangeObserver): number {
    this.syncStateChangeCallback = callback;
    return 1;
  }

  async handleHolochainSignal(signal: any): Promise<void> {
    return null;
  }
}

function prepareLinkExpression(link: LinkExpression): object {
  const data = Object.assign(link);
  if (data.data.source == "") {
    data.data.source = null;
  }
  if (data.data.target == "") {
    data.data.target = null;
  }
  if (data.data.predicate == "") {
    data.data.predicate = null;
  }
  if (data.data.source == undefined) {
    data.data.source = null;
  }
  if (data.data.target == undefined) {
    data.data.target = null;
  }
  if (data.data.predicate == undefined) {
    data.data.predicate = null;
  }
  return data;
}


class Mutex {
  private locked = false;
  private waitingResolvers: (() => void)[] = [];

  async lock(): Promise<void> {
    if (this.locked) {
      return new Promise((resolve) => {
        this.waitingResolvers.push(resolve);
      });
    }
    this.locked = true;
  }

  unlock(): void {
    if (!this.locked) return;
    if (this.waitingResolvers.length > 0) {
      const resolve = this.waitingResolvers.shift();
      if (resolve) resolve();
    } else {
      this.locked = false;
    }
  }
}