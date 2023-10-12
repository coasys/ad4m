import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import type { SyncStateChangeObserver } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { Mutex, withTimeout } from "https://esm.sh/async-mutex@0.4.0";
import { io, Socket, ServerToClientEvents, ClientToServerEvents } from "https://esm.sh/socket.io-client@4.7.2";
import makeHttpRequest from "./util.ts";

export class LinkAdapter implements LinkSyncAdapter {
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error('PerspectiveDiffSync: generalMutex timeout'));
  me: DID
  myCurrentTime: any | null = null;
  languageUid: String | null = null;
  socketClient: Socket<ServerToClientEvents, ClientToServerEvents>;
  hasCalledSync = false;

  constructor(context: LanguageContext, uid: String) {
    this.me = context.agent.did;
    this.languageUid = uid;

    //this.addAgentRecord();

    this.socketClient = io("https://socket.ad4m.dev", { transports: ['websocket', 'polling'], autoConnect: true });
    console.log("Created socket connection");
    
    this.socketClient.on('error', (error: any) => {
      console.error('Error:', error);
    });
    
    this.socketClient.on('connect', async () => {
      console.log('Connected to the server');
      try {
        console.log("Trying to join room", this.languageUid);
        this.socketClient.emit("join-room", this.languageUid);
        console.log("Sent the join-room signal");
      } catch (e) {
        console.error("Error in socket connection: ", e);
      }
    });

    //Response from a given call to commit by us or any other agent
    //contains all the data we need to update our local state and our recordTimestamp as held by the server
    this.socketClient.on("signal-emit", async (signal) => {
      //Try and get the mutex, so that we dont allow signals to be processed until we have done the first sync
      const release = await this.generalMutex.acquire();

      try {
        console.log("Got some live signal from the server");
        console.dir(signal);
        console.log(this.me);

        if (this.myCurrentTime) {
          console.log("With current time", this.myCurrentTime);
        }

        let serverRecordTimestamp = signal.serverRecordTimestamp;
        if (!this.myCurrentTime|| this.myCurrentTime < serverRecordTimestamp) {
          console.log("Returning that live signal to executor");
          this.myCurrentTime = serverRecordTimestamp;
          this.updateServerSyncState();
          
          this.handleSignal(signal.payload);
        }
      } catch (e) {
        console.error("PerspectiveDiffSync.signal-emit(); got error", e);
      } finally {
        release();
      }
    })
    
    this.socketClient.on('disconnect', () => {
      console.log('Disconnected from the server');
    });
    
    this.socketClient.on('connect_error', (error) => {
      console.error('Connection Error:', error);
    });

    this.socketClient.on('reconnect', (attemptNumber) => {
      console.log('Reconnected to the server on attempt:', attemptNumber);

      //If we have disconnected and reconnected, we need to sync again
      this.hasCalledSync = false;
      this.sync();
    });
    
    this.socketClient.on('reconnect_attempt', () => {
      console.log('Trying to reconnect...');
    });
  }

  //Tell the server that we have updated our current timestamp so that the server can keep in sync with what we have seen
  updateServerSyncState() {
    if (this.myCurrentTime) {
      this.socketClient.emit("update-sync-state", {did: this.me, date: this.myCurrentTime, linkLanguageUUID: this.languageUid}, (err, signal) => {
        if (err) {
          console.error("Error in update-sync-state call", err);
        };
        console.log("Got some result from update-sync-state");
        console.dir(signal);
      })
    }
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

    console.log("Current revision got result", result);

    if (result) {
      //@ts-ignore
      if (result.currentRevision === null) {
        return "";
      } else {
        //@ts-ignore
        return result.currentRevision;
      };
    }

    return "";
  }

  //Call sync on the server, which will should fetch all the links we missed since last start of the link language
  async sync(): Promise<PerspectiveDiff> {
    console.log("Sync call has called sync", this.hasCalledSync);
    //Only allow sync to be called once since once we have sync'd once we will get future links via signal
    if (!this.hasCalledSync) {
      //console.log("PerspectiveDiffSync.sync(); Getting lock");
      const release = await this.generalMutex.acquire();
      //console.log("PerspectiveDiffSync.sync(); Got lock");
      try {
        console.log("Sending the sync event to server");
        console.log(this.me);
        this.socketClient.emit("sync", {
          linkLanguageUUID: this.languageUid,
          did: this.me,
        }, (err, signal) => {
          if (err) {
            console.error("Error in sync call", err);
            throw Error(err);
          };
          console.log("Got some result from sync");
          console.dir(signal);
          console.log(this.me);

          if (this.myCurrentTime) {
            console.log("With current time", this.myCurrentTime);
          }

          this.myCurrentTime = signal.serverRecordTimestamp;
          this.updateServerSyncState();
          this.hasCalledSync = true;

          if (signal.payload.additions.length > 0 || signal.payload.removals.length > 0) {
            this.handleSignal(signal.payload);
          }

          //Emit and event saying that we are synced
          this.syncStateChangeCallback(PerspectiveState.Synced);
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
    return new Promise((resolve, reject) => {
      //Send the request to get the links
      this.socketClient.emit("render", {
        linkLanguageUUID: this.languageUid,
      }, (err, signal) => {
        if (err) {
          console.error("Error in sync call", err);
          return reject(err);
        };
        this.myCurrentTime = signal.serverRecordTimestamp;
        this.updateServerSyncState();
        resolve(new Perspective(signal.payload))
      })
    });
  }

  async commit(diff: PerspectiveDiff): Promise<string> {
    return new Promise(async (resolve, reject) => {
      const release = await this.generalMutex.acquire();
      
      try {  
        let preppedDiff = {
          additions: diff.additions.map((diff) => prepareLinkExpression(diff)),
          removals: diff.removals.map((diff) => prepareLinkExpression(diff)),
          linkLanguageUUID: this.languageUid,
          did: this.me,
        };
        console.log("Commit sending prepped diff", preppedDiff);
        //Send the commit to the server
        this.socketClient.emit("commit", preppedDiff, (err, signal) => {
          if (err) {
            console.error("Error in sync call", err);
            return reject(err);
          };
          if (signal.status === "Ok") {
            //Update our local timestamp to match the server
            this.myCurrentTime = signal.serverRecordTimestamp;
            this.updateServerSyncState();

            resolve("");
          } else {
            reject()
          }
        });
      } catch (e) {
        console.error("PerspectiveDiffSync.commit(); got error", e);
      } finally {
        release();
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
