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
  hasCalledSync = false;

  constructor(context: LanguageContext, uid: String) {
    this.me = context.agent.did;
    this.languageUid = uid;

    //this.addAgentRecord();

    this.socket = io("https://socket.ad4m.dev", { transports: ['websocket', 'polling'], autoConnect: true });
    console.log("Created socket connection");
    
    this.socket.on('error', (error: any) => {
      console.error('Error:', error);
    });
    
    this.socket.on('connect', async () => {
      console.log('Connected to the server');
      try {
        this.socket.emit("join-room", this.languageUid);
        console.log("Sent the join-room signal");
      } catch (e) {
        console.error("Error in socket connection: ", e);
      }
    });

    //Response from a given call to sync; contains all the data we need to update our local state and our recordTimestamp as held by the server
    this.socket.on("sync-emit", (signal) => {
      console.log("Got some result from sync", signal, this.myCurrentRevision.timestamp);

      this.myCurrentRevision.timestamp = signal.serverRecordTimestamp;
      this.updateServerSyncState();
      this.hasCalledSync = true;

      this.handleSignal(signal.payload);

      //Emit and event saying that we are synced
      this.syncStateChangeCallback(PerspectiveState.Synced);
    });

    //Response from a given call to commit by us or any other agent
    //contains all the data we need to update our local state and our recordTimestamp as held by the server
    this.socket.on("signal-emit", (signal) => {
      console.log("Got some live signal from the server", signal, this.myCurrentRevision.timestamp);

      let serverRecordTimestamp = signal.serverRecordTimestamp;
      if (!this.myCurrentRevision.timestamp || this.myCurrentRevision.timestamp < serverRecordTimestamp) {
        this.myCurrentRevision.timestamp = serverRecordTimestamp;
        this.updateServerSyncState();
        
        this.handleSignal(signal.payload);
      }
    })
    
    this.socket.on('disconnect', () => {
      console.log('Disconnected from the server');
    });
    
    this.socket.on('connect_error', (error) => {
      console.error('Connection Error:', error);
    });

    this.socket.on('reconnect', (attemptNumber) => {
      console.log('Reconnected to the server on attempt:', attemptNumber);

      //If we have disconnected and reconnected, we need to sync again
      this.hasCalledSync = false;
      this.sync();
    });
    
    this.socket.on('reconnect_attempt', () => {
      console.log('Trying to reconnect...');
    });
  }

  //Tell the server that we have updated our current timestamp so that the server can keep in sync with what we have seen
  updateServerSyncState() {
    this.socket.emit("update-sync-state", {did: this.me, date: this.myCurrentRevision.timestamp, linkLanguageUUID: this.languageUid})
  }

  writable(): boolean {
    return true;
  }

  public(): boolean {
    return false;
  }

  async others(): Promise<DID[]> {
    // @ts-ignore
    return await makeHttpRequest("https://socket.ad4m.dev/getOthers", "GET",  {}, {
      linkLanguageUUID: this.languageUid
    })
  }

  async currentRevision(): Promise<string> {
    //@ts-ignore
    const result = await makeHttpRequest("https://socket.ad4m.dev/currentRevision", "POST",  {}, {
      linkLanguageUUID: this.languageUid,
      did: this.me
    })

    if (result) {
      //@ts-ignore
      return result.currentRevision;
    }

    return "";
  }

  //Call sync on the server, which will should fetch all the links we missed since last start of the link language
  async sync(): Promise<PerspectiveDiff> {
    //Only allow sync to be called once since once we have sync'd once we will get future links via signal
    if (!this.hasCalledSync) {
      //console.log("PerspectiveDiffSync.sync(); Getting lock");
      const release = await this.generalMutex.acquire();
      //console.log("PerspectiveDiffSync.sync(); Got lock");
      try {
        this.socket.emit("sync", {
          linkLanguageUUID: this.languageUid,
          did: this.me,
        });
      } catch (e) {
        console.error("PerspectiveDiffSync.sync(); got error", e);
      } finally {
        release();
      }
    }
    return new PerspectiveDiff()
  }

  //Fetch all the links from the server
  async render(): Promise<Perspective> {
    return new Promise((resolve) => {
      //Send the request to get the links
      this.socket.emit("render", {
        linkLanguageUUID: this.languageUid,
      })

      //Wait for the response
      this.socket.on("render-emit", (signal) => {
        this.myCurrentRevision.timestamp = signal.serverRecordTimestamp;
        resolve(new Perspective(signal.payload))
      })
    });
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    return new Promise(async (resolve, reject) => {
      const release = await this.generalMutex.acquire();
      
      try {  
        //Send the commit to the server
        this.socket.emit("commit", {
          additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
          removals: diff.removals.map((diff) => prepareLinkExpression(diff)),
          linkLanguageUUID: this.languageUid,
          did: this.me,
        })

        //Wait for a response saying that the commit was successful
        this.socket.on("commit-status", (signal) => {
          if (signal.status === "Ok") {
            //Update our local timestamp to match the server
            this.myCurrentRevision.timestamp = signal.serverRecordTimestamp;
            this.updateServerSyncState();

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
      const result = await makeHttpRequest("https://socket.ad4m.dev/addAgent", "POST",  {}, {
        linkLanguageUUID: this.languageUid,
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
