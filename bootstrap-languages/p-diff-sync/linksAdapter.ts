import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import type { SyncStateChangeObserver } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { Mutex, withTimeout } from "https://esm.sh/async-mutex@0.4.0";
import { DNA_NICK, ZOME_NAME } from "./build/dna.js";
import { io } from "https://esm.sh/socket.io-client@4.7.2";

class PeerInfo {
  //@ts-ignore
  currentRevision: Buffer;
  //@ts-ignore
  lastSeen: Date;
};

export class LinkAdapter implements LinkSyncAdapter {
  hcDna: HolochainLanguageDelegate;
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  peers: Map<DID, PeerInfo> = new Map();
  generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error('PerspectiveDiffSync: generalMutex timeout'));
  me: DID
  gossipLogCount: number = 0;
  myCurrentRevision: Buffer | null = null;
  languageName: String | null = null;
  socket: any;

  constructor(context: LanguageContext, name: String) {
    //@ts-ignore
    this.hcDna = context.Holochain as HolochainLanguageDelegate;
    this.me = context.agent.did;
    this.languageName = name;
    this.socket = io("https://socket.ad4m.dev", { transports: ['websocket', 'polling'], autoConnect: true });
    console.log("Created socket connection");
    this.socket.on('error', (error: any) => {
      console.error('Error:', error);
    });
    this.socket.on('connect', () => {
      console.log('Connected to the server');
      try {
        this.socket.emit("join-room", this.languageName);
        console.log("Sent the join-room signal");
      } catch (e) {
        console.error("Error in socket connection: ", e);
      }
    });
    this.socket.on("signal", (signal: any) => {
      this.handleHolochainSignal(signal);
    });
    this.socket.on('disconnect', () => {
      console.log('Disconnected from the server');
    });
    this.socket.on('connect_error', (error) => {
      console.error('Connection Error:', error);
    });
    this.socket.on('reconnect_attempt', () => {
      console.log('Trying to reconnect...');
    });
  }

  writable(): boolean {
    return true;
  }

  public(): boolean {
    return false;
  }

  async others(): Promise<DID[]> {
    //@ts-ignore
    return await this.hcDna.call(DNA_NICK, ZOME_NAME, "get_others", null);
  }

  async currentRevision(): Promise<string> {
    //@ts-ignore
    let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "current_revision", null);
    return res as string;
  }

  async sync(): Promise<PerspectiveDiff> {
    //console.log("PerspectiveDiffSync.sync(); Getting lock");
    const release = await this.generalMutex.acquire();
    //console.log("PerspectiveDiffSync.sync(); Got lock");
    try {
      //@ts-ignore
      let broadcast_payload = await this.hcDna.call(DNA_NICK, ZOME_NAME, "get_broadcast_payload", null);
      if (broadcast_payload) {
        if (broadcast_payload.reference_hash && Buffer.isBuffer(broadcast_payload.reference_hash)) {
          this.myCurrentRevision = broadcast_payload.reference_hash;
        }
        //Use client to send to socketIO
        broadcast_payload.reference_hash = Buffer.from(broadcast_payload.reference_hash).toString('base64');
        broadcast_payload.reference.diff = Buffer.from(broadcast_payload.reference.diff).toString('base64');
        if (broadcast_payload.reference.parents) {
          broadcast_payload.reference.parents = broadcast_payload.reference.parents.map( (parent: Buffer) => parent ? Buffer.from(parent).toString('base64') : 'null');
        };
        console.log("sync(); sending referenceh hash", broadcast_payload.reference_hash);
        console.log("sync(); sending broadcast payload");
        console.log(JSON.stringify(broadcast_payload));
        this.socket.emit("broadcast", {roomId: this.languageName, signal: broadcast_payload});
      }
    } catch (e) {
      console.error("PerspectiveDiffSync.sync(); got error", e);
    } finally {
      release();
    }
    await this.gossip();
    return new PerspectiveDiff()
  }

  async gossip() {
    this.gossipLogCount += 1;
    let lostPeers: DID[] = [];

    const release = await this.generalMutex.acquire();
    try {
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

      async function checkSyncState(callback: SyncStateChangeObserver) {
        if (sameRevisions.length > 0 || differentRevisions.length > 0) {
          if (sameRevisions.length <= differentRevisions.length) {
            await callback(PerspectiveState.LinkLanguageInstalledButNotSynced);
          } else {
            await callback(PerspectiveState.Synced);
          };
        }
      }

      //@ts-ignore
      generateRevisionStates(this.myCurrentRevision);

      //@ts-ignore
      await checkSyncState(this.syncStateChangeCallback);

      for (const hash of Array.from(revisions)) {
        if(!hash) continue
        if (this.myCurrentRevision && hash.equals(this.myCurrentRevision)) continue
        console.log("Pulling with hash", hash);
        let pullResult = await this.hcDna.call(DNA_NICK, ZOME_NAME, "pull", { 
          hash,
          is_scribe 
        });
        if (pullResult) {
          if (pullResult.current_revision && Buffer.isBuffer(pullResult.current_revision)) {
            let myRevision = pullResult.current_revision;
            this.myCurrentRevision = myRevision;

            //@ts-ignore
            generateRevisionStates(this.myCurrentRevision);
            //@ts-ignore
            await checkSyncState(this.syncStateChangeCallback);
          }
        }
      }

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
    } catch (e) {
      console.error("PerspectiveDiffSync.gossip(); got error", e);
    } finally {
      release();
    }
  }

  async render(): Promise<Perspective> {
    //@ts-ignore
    let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "render", null);
    return new Perspective(res.links);
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    //console.log("PerspectiveDiffSync.commit(); Getting lock");
    const release = await this.generalMutex.acquire();
    try {
      //console.log("PerspectiveDiffSync.commit(); Got lock");
      let prep_diff = {
        additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
        removals: diff.removals.map((diff) => prepareLinkExpression(diff))
      }
      let res = await this.hcDna.call(DNA_NICK, ZOME_NAME, "commit", prep_diff);
      if (res && Buffer.isBuffer(res)) {
        this.myCurrentRevision = res;
      }
      let broadcast_payload = await this.hcDna.call(DNA_NICK, ZOME_NAME, "get_broadcast_payload", null);
      console.log('commit got broadcast payload', broadcast_payload.referencence_hash);
      console.log("which has type", typeof broadcast_payload.reference_hash);
      if (broadcast_payload) {
        broadcast_payload.reference_hash = Buffer.from(broadcast_payload.reference_hash).toString('base64');
        broadcast_payload.reference.diff = Buffer.from(broadcast_payload.reference.diff).toString('base64');
        if (broadcast_payload.reference.parents) {
          broadcast_payload.reference.parents = broadcast_payload.reference.parents.map( (parent: Buffer) => parent ? Buffer.from(parent).toString('base64') : 'null');
        };
        console.log("commit sending referenceh hash", broadcast_payload.reference_hash);
        this.socket.emit("broadcast", {roomId: this.languageName, signal: broadcast_payload});
      }
      return res as string;
    } catch (e) {
      console.error("PerspectiveDiffSync.commit(); got error", e);
    } finally {
      release();
    }
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
    let diff;
    let reference_hash;
    let reference;
    let broadcast_author;
    if (signal.payload) {
      ({ diff, reference_hash, reference, broadcast_author } = signal.payload);
    } else {
      ({ diff, reference_hash, reference, broadcast_author } = signal);
    }
    // console.log("Setting a peer hash to", reference_hash);
    // console.log(JSON.stringify(diff));
    // console.log("Reference", JSON.stringify(reference));
    // console.log(broadcast_author);
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
      try {
        //console.log("PerspectiveDiffSync.handleHolochainSignal: Getting lock");

        //console.log("PerspectiveDiffSync.handleHolochainSignal: Got lock");
        //const parsed = JSON.parse(reference_hash);
        console.log("Handle holochain signal parsed ref hash");
        console.log(JSON.stringify({
          diff,
          reference_hash,
          reference,
          broadcast_author
        }));
        if (!Buffer.isBuffer(reference_hash)) {
          reference_hash = Buffer.from(reference_hash, 'base64');
        }
        if (!Buffer.isBuffer(reference.diff)) {
          reference.diff = Buffer.from(reference.diff, 'base64');
        }
        if (reference.parents) {
          reference.parents = reference.parents.map( (parent: string) => parent == 'null' ? null : Buffer.from(parent, 'base64'));
        }

        await this.hcDna.call(DNA_NICK, ZOME_NAME, "handle_broadcast", {
          diff,
          reference_hash,
          reference,
          broadcast_author
        });
        this.peers.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
      } catch (e) {
        console.error("PerspectiveDiffSync.handleHolochainSignal: got error", e);
      }
    } else {
      console.log("PerspectiveDiffSync.handleHolochainSignal: received a signals from ourselves in fast_forward_signal or in a pull: ", JSON.stringify(signal.payload));
      //This signal only contains link data and no reference, and therefore came from us in a pull in fast_forward_signal
      if (this.linkCallback) {
        console.log("PerspectiveDiffSync.handleHolochainSignal: calling linkCallback");
        await this.linkCallback(signal.payload);
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
        //@ts-ignore
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
