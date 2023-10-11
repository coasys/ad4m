import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import type { SyncStateChangeObserver } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { Mutex, withTimeout } from "https://esm.sh/async-mutex@0.4.0";
import { io } from "https://esm.sh/socket.io-client@4.7.2";
import makeHttpRequest from "./util.ts";

class PeerInfo {
  //@ts-ignore
  currentRevision: Buffer;
  //@ts-ignore
  lastSeen: Date;
};

export class LinkAdapter implements LinkSyncAdapter {
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  peers: Map<DID, PeerInfo> = new Map();
  generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error('PerspectiveDiffSync: generalMutex timeout'));
  me: DID
  gossipLogCount: number = 0;
  myCurrentRevision: any | null = null;
  languageName: String | null = null;
  socket: any;

  constructor(context: LanguageContext, name: String) {
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
    // @ts-ignore
    return await makeHttpRequest("http://127.0.0.1:8787/getActiveAgent", "GET",  {}, {
      LinkLanguageUUID: this.languageName,
      did: this.me
    })
  }

  async currentRevision(): Promise<string> {
    //@ts-ignore
    const result = await makeHttpRequest("http://127.0.0.1:8787/render", "GET",  {}, {
      LinkLanguageUUID: this.languageName
    })

    if (result) {
      //@ts-ignore
      return result.hash as string;
    }

    return "";
  }

  async sync(): Promise<PerspectiveDiff> {
    //console.log("PerspectiveDiffSync.sync(); Getting lock");
    const release = await this.generalMutex.acquire();
    //console.log("PerspectiveDiffSync.sync(); Got lock");
    try {
        //@ts-ignore
        const result = await makeHttpRequest("http://127.0.0.1:8787/sync", "GET",  {}, {
          LinkLanguageUUID: this.languageName,
          hash: this.myCurrentRevision.hash,
          timestamp: this.myCurrentRevision.timestamp
        })

        this.linkCallback(result)
    } catch (e) {
      console.error("PerspectiveDiffSync.sync(); got error", e);
    } finally {
      release();
    }
    return new PerspectiveDiff()
  }

  async render(): Promise<Perspective> {
    //@ts-ignore
    const result = await makeHttpRequest("http://127.0.0.1:8787/render", "GET",  {}, {
      LinkLanguageUUID: this.languageName
    })
    return new Perspective(result);
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    const release = await this.generalMutex.acquire();
    try {
      let prep_diff = {
        additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
        removals: diff.removals.map((diff) => prepareLinkExpression(diff))
      }

      const result = await makeHttpRequest("http://127.0.0.1:8787/commit", "POST",  {}, {
        ...prep_diff,
        LinkLanguageUUID: this.languageName
      })

      this.myCurrentRevision = result;

      this.socket.emit("broadcast", {roomId: this.languageName, signal: {
        reference_hash: "", 
        diff: prep_diff,
        broadcast_author: this.me
      }});
      return ;
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

    if (diff && reference_hash && reference && broadcast_author) {
      try {
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
    const result = await makeHttpRequest("http://127.0.0.1:8787/addActiveAgent", "POST",  {}, {
      LinkLanguageUUID: this.languageName,
      did: this.me
    })

    return result
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
