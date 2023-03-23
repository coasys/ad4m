import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "@perspect3vism/ad4m";
import type { SyncStateChangeObserver } from "@perspect3vism/ad4m";
import { DNA_NICK, ZOME_NAME } from "./dna";

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

class PeerInfo {
  currentRevision: Buffer;
  lastSeen: Date;
};

export class LinkAdapter implements LinkSyncAdapter {
  hcDna: HolochainLanguageDelegate;
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  peers: Map<DID, PeerInfo> = new Map();
  me: DID
  gossipLogCount: number = 0;
  myCurrentRevision: Buffer | null = null;

  constructor(context: LanguageContext) {
    //@ts-ignore
    this.hcDna = context.Holochain as HolochainLanguageDelegate;
    this.me = context.agent.did;
  }

  writable(): boolean {
    return true;
  }

  public(): boolean {
    return false;
  }

  async others(): Promise<DID[]> {
    return await this.hcDna.call(DNA_NICK, ZOME_NAME, "get_others", null);
  }

  async currentRevision(): Promise<string> {
    let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "current_revision", null);
    return res as string;
  }

  async sync(): Promise<PerspectiveDiff> {
    let current_revision = await this.hcDna.call(DNA_NICK, ZOME_NAME, "sync", null);
    if (current_revision && Buffer.isBuffer(current_revision)) {
      this.myCurrentRevision = current_revision; 
    }
    await this.gossip();
    return new PerspectiveDiff()
  }

  async gossip() {
    this.gossipLogCount += 1;
    let lostPeers: DID[] = [];

    this.peers.forEach( (peerInfo, peer) => {
      if (peerInfo.lastSeen.getTime() + 10000 < new Date().getTime()) {
        lostPeers.push(peer);
      }
    });

    for (const peer of lostPeers) {
      this.peers.delete(peer);
    }

    // flatten the map into an array of peers
    let peers = Array.from(this.peers.keys());
    peers.push(this.me);
    
    // Lexically sort the peers
    peers.sort();

    // If we are the first peer, we are the scribe
    let is_scribe = peers[0] == this.me;
    
    // Get a deduped set of all peer's current revisions
    let revisions = new Set<Buffer>();
    for(const peerInfo of this.peers.values()) {
      if (peerInfo.currentRevision) revisions.add(peerInfo.currentRevision);
    }

    //Do checking on incoming gossip revisions and see if we have the same hash as the majority of the peers
    //Get a copied array of revisions that are the same as mine
    let sameRevisions;
    //Get a copied array of revisions that are different than mine
    let differentRevisions;

    function generateRevisionStates(myCurrentRevision: Buffer) {
      sameRevisions = revisions.size == 0 ? [] : Array.from(revisions).filter( (revision) => {
        return myCurrentRevision && revision.equals(myCurrentRevision);
      });
      if (myCurrentRevision) {
        sameRevisions.push(myCurrentRevision);
      };
      differentRevisions = revisions.size == 0 ? [] : Array.from(revisions).filter( (revision) => {
        return myCurrentRevision && !revision.equals(myCurrentRevision);
      });
    }

    function checkSyncState(callback: SyncStateChangeObserver) {
      if (sameRevisions.length > 0 || differentRevisions.length > 0) {
        if (sameRevisions.length <= differentRevisions.length) {
          callback(PerspectiveState.LinkLanguageInstalledButNotSynced);
        } else {
          callback(PerspectiveState.Synced);
        };
      }
    }

    generateRevisionStates(this.myCurrentRevision);

    checkSyncState(this.syncStateChangeCallback);

    revisions.forEach( async (hash) => {
      if(!hash) return
      if (this.myCurrentRevision && hash.equals(this.myCurrentRevision)) return
      let pullResult = await this.hcDna.call(DNA_NICK, ZOME_NAME, "pull", { 
        hash,
        is_scribe 
      });
      if (pullResult) {
        if (pullResult.current_revision && Buffer.isBuffer(pullResult.current_revision)) {
          let myRevision = pullResult.current_revision;
          this.myCurrentRevision = myRevision;

          generateRevisionStates(this.myCurrentRevision);
          checkSyncState(this.syncStateChangeCallback);
        }
      }
    })

    //Only show the gossip log every 10th iteration
    if (this.gossipLogCount == 10) {
      console.log(`
      ======
      GOSSIP
      --
      me: ${this.me}
      is scribe: ${is_scribe}
      --
      ${Array.from(this.peers.entries()).map( ([peer, peerInfo]) => {
        //@ts-ignore
        return `${peer}: ${peerInfo.currentRevision.toString('base64')} ${peerInfo.lastSeen.toISOString()}\n`
      })}
      --
      revisions: ${Array.from(revisions).map( (hash) => {
        //@ts-ignore
        return hash.toString('base64')
      })}
      `);
      this.gossipLogCount = 0;
    }
  }

  async render(): Promise<Perspective> {
    let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "render", null);
    return new Perspective(res.links);
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    let prep_diff = {
      additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
      removals: diff.removals.map((diff) => prepareLinkExpression(diff))
    }
    let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "commit", prep_diff);
    if (res && Buffer.isBuffer(res)) {
      this.myCurrentRevision = res;
    }
    return res as string;
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
    const { diff, reference_hash, reference, broadcast_author } = signal.payload;
    //Check if this signal came from another agent & contains a diff and reference_hash
    if (diff && reference_hash && reference && broadcast_author) {
      // console.log(`PerspectiveDiffSync.handleHolochainSignal: 
      //       diff: ${JSON.stringify(diff)}
      //       reference_hash: ${reference_hash.toString('base64')}
      //       reference: {
      //           diff: ${reference.diff?.toString('base64')}
      //           parents: ${reference.parents ? reference.parents.map( (parent: Buffer) => parent ? parent.toString('base64') : 'null').join(', '):'none'}
      //           diffs_since_snapshot: ${reference?.diffs_since_snapshot}
      //       }
      //       broadcast_author: ${broadcast_author}
      //       `)
      this.peers.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
    } else {
      //console.log("PerspectiveDiffSync.handleHolochainSignal: received a signals from ourselves in fast_forward_signal or in a pull: ", signal.payload);
      //This signal only contains link data and no reference, and therefore came from us in a pull in fast_forward_signal
      if (this.linkCallback) {
        this.linkCallback(signal.payload);
      }
    }
  }

  async addActiveAgentLink(hcDna: HolochainLanguageDelegate): Promise<any> {
    if (hcDna == undefined) {
      console.warn("===Perspective-diff-sync: Error tried to add an active agent link but received no hcDna to add the link onto");
    } else {
      return await hcDna.call(
        DNA_NICK,
        ZOME_NAME,
        "add_active_agent_link",
        null
      );
    }
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
