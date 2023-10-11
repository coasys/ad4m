import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import type { SyncStateChangeObserver } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { Mutex, withTimeout } from "https://esm.sh/async-mutex@0.4.0";
import { io } from "https://esm.sh/socket.io-client@4.7.2";
import makeHttpRequest from "./util.ts";

export class LinkAdapter implements LinkSyncAdapter {
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error('PerspectiveDiffSync: generalMutex timeout'));
  me: DID
  myCurrentRevision: any | null = null;
  languageUid: String | null = null;
  socket: any;

  constructor(context: LanguageContext, uid: String) {
    this.me = context.agent.did;
    this.languageUid = uid;

    this.addAgentRecord();

    this.socket = io("https://socket.ad4m.dev", { transports: ['websocket', 'polling'], autoConnect: true });
    console.log("Created socket connection");
    this.socket.on('error', (error: any) => {
      console.error('Error:', error);
    });
    this.socket.on('connect', async () => {
      console.log('Connected to the server');
      try {
        this.socket.emit("sync", {
          linkLanguageUUID: this.languageUid,
          did: this.me,
        })

        this.socket.emit("join-room", this.languageUid);
        console.log("Sent the join-room signal");
      } catch (e) {
        console.error("Error in socket connection: ", e);
      }
    });
    this.socket.on("signal", (signal: any) => {
      this.handleSignal(signal);
    });
    this.socket.on("sync-emit", (signal: any) => {
      this.handleSignal(signal);
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
    return await makeHttpRequest("http://127.0.0.1:8787/getOthers", "GET",  {}, {
      LinkLanguageUUID: this.languageUid
    })
  }

  async currentRevision(): Promise<string> {
    //@ts-ignore
    const result = await makeHttpRequest("http://127.0.0.1:8787/currentRevision", "GET",  {}, {
      LinkLanguageUUID: this.languageUid,
      did: this.me
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
      this.socket.emit("sync", {
        linkLanguageUUID: this.languageUid,
        did: this.me,
      })
    } catch (e) {
      console.error("PerspectiveDiffSync.sync(); got error", e);
    } finally {
      release();
    }
    return new PerspectiveDiff()
  }

  async render(): Promise<Perspective> {
    return new Promise((resolve) => {
      this.socket.emit("render", {
        linkLanguageUUID: this.languageUid,
      })

      this.socket.on("render-emit", (signal) => {
        resolve(new Perspective(signal.payload))
      })
    });
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    return new Promise(async (resolve, reject) => {
      const release = await this.generalMutex.acquire();
      
      try {
        let prep_diff = {
          additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
          removals: diff.removals.map((diff) => prepareLinkExpression(diff))
        }
  
        this.socket.emit("commit", {
          additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
          removals: diff.removals.map((diff) => prepareLinkExpression(diff)),
          linkLanguageUUID: this.languageUid,
          did: this.me,
        })

        this.socket.on("commit-status", (signal) => {
          if (signal.status === "Ok") {
            resolve(null);
          } else {
            reject()
          }
        });
        
        return ;
      } catch (e) {
        console.error("PerspectiveDiffSync.commit(); got error", e);
      } finally {
        release();
        reject(null);
      }
    })
  }

  addCallback(callback: PerspectiveDiffObserver): number {
    this.linkCallback = callback;
    return 1;
  }

  addSyncStateChangeCallback(callback: SyncStateChangeObserver): number {
    this.syncStateChangeCallback = callback;
    return 1;
  }

  async handleSignal(signal: any): Promise<void> {
    //This signal only contains link data and no reference, and therefore came from us in a pull in fast_forward_signal
    if (this.linkCallback) {
      console.log("PerspectiveDiffSync.handleHolochainSignal: calling linkCallback", signal);
      await this.linkCallback(signal);
    }
  }

  async addAgentRecord(): Promise<any> {
    const others = await this.others();

    if (others.filter((other) => other === this.me).length == 0) {
        const result = await makeHttpRequest("http://127.0.0.1:8787/addAgent", "POST",  {}, {
          LinkLanguageUUID: this.languageUid,
          did: this.me
        })
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
