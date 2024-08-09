import { LinkSyncAdapter, PerspectiveDiffObserver, HolochainLanguageDelegate, LanguageContext, PerspectiveDiff, 
  LinkExpression, DID, Perspective, PerspectiveState } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import type { SyncStateChangeObserver } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { Mutex, withTimeout } from "https://esm.sh/async-mutex@0.4.0";
import type { Socket, ServerToClientEvents, ClientToServerEvents } from "https://esm.sh/socket.io-client@4.7.2";
import axiod from "https://deno.land/x/axiod/mod.ts";

export class LinkAdapter implements LinkSyncAdapter {
  linkCallback?: PerspectiveDiffObserver
  syncStateChangeCallback?: SyncStateChangeObserver
  generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error('PerspectiveDiffSync: generalMutex timeout'));
  me: DID
  myCurrentTime: any | null = null;
  languageUid: String | null = null;
  socketClient: Socket<ServerToClientEvents, ClientToServerEvents>;
  hasCalledSync = false;

  constructor(context: LanguageContext, uid: String, socketClient: Socket<ServerToClientEvents, ClientToServerEvents>) {
    this.me = context.agent.did;
    this.languageUid = uid;
    this.socketClient = socketClient;
    
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
      //const release = await this.generalMutex.acquire();

      try {
        // console.log("Got some live signal from the server");
        // console.dir(signal);
        // console.log(this.me);

        // if (this.myCurrentTime) {
        //   console.log("With current time", this.myCurrentTime);
        // }

        let serverRecordTimestamp = signal.serverRecordTimestamp;
        if (this.myCurrentTime && this.myCurrentTime < serverRecordTimestamp) {
          //console.log("Returning that live signal to executor");
          this.myCurrentTime = serverRecordTimestamp;
          this.updateServerSyncState();
          
          this.handleSignal(signal.payload);
        }
      } catch (e) {
        console.error("PerspectiveDiffSync.signal-emit(); got error", e);
      } finally {
        //release();
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
        // console.log("Got some result from update-sync-state");
        // console.dir(signal);
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
    const others = await axiod.get("https://socket.ad4m.dev/getOthers", {
      params: {
        linkLanguageUUID: this.languageUid
      }
    });
    if (others.status === 200) {
      //Remove myself from others if it exists
      const othersIndex = others.data.indexOf(this.me);
      if (othersIndex > -1) {
        others.data.splice(othersIndex, 1);
      }
      return others.data;
    } else {
      console.error("Error fetching others in linkAdapter, got status", others.status);
      return [];
    }
  }

  async currentRevision(): Promise<string> {
    //console.log("Getting current revision");
    let result;
    try {
      result = await axiod.post("https://socket.ad4m.dev/currentRevision", {
        linkLanguageUUID: this.languageUid,
        did: this.me
      })
      if (result.status === 200) {
        result = result.data;
      } else {
        console.error("Error in currentRevision call");
        console.error("Got status", result.status);
        result = null;
      }
      //console.log("Current revision returned with result");
    } catch (e) {
      console.log("Error in currentRevision call", e);
      result = null;
    }

    //console.log("Current revision got result", result);

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
    //console.log("Sync call has called sync", this.hasCalledSync);
    //Only allow sync to be called once since once we have sync'd once we will get future links via signal
    if (!this.hasCalledSync) {
      //console.log("PerspectiveDiffSync.sync(); Getting lock");
      //const release = await this.generalMutex.acquire();
      //console.log("PerspectiveDiffSync.sync(); Got lock");
      try {
        //console.log("Sending the sync event to server");
        //console.log(this.me);
        this.socketClient.emit("sync", {
          linkLanguageUUID: this.languageUid,
          did: this.me,
        }, (err, signal) => {
          if (err) {
            console.error("Error in sync call", err);
            throw Error(err);
          };
          // console.log("Got some result from sync");
          // console.dir(signal);
          //console.log(this.me);

          // if (this.myCurrentTime) {
          //   console.log("With current time", this.myCurrentTime);
          // }

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
        //release();
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
    //const release = await this.generalMutex.acquire();
    try {
      const preppedDiff = {
        additions: diff.additions.map((item) => prepareLinkExpression(item)),
        removals: diff.removals.map((item) => prepareLinkExpression(item)),
        linkLanguageUUID: this.languageUid,
        did: this.me,
      };
      //console.log("Commit sending prepped diff", preppedDiff);

      const signal = await this.emitCommit(preppedDiff);

      if (signal.status === "Ok") {
        //console.log("Got some result from commit");
        //console.dir(signal);
        //Update our local timestamp to match the server
        this.myCurrentTime = signal.serverRecordTimestamp;
        this.updateServerSyncState();
        return ""; // Resolve the function with an empty string
      } else {
        throw new Error("Commit failed with non-Ok status");
      }
    } catch (e) {
      console.error("PerspectiveDiffSync.commit(); got error", e);
      throw e; // Propagate the error up
    } finally {
      //release();
    }
  }

  // Utility method to wrap the socketClient.emit in a Promise
  private emitCommit(preppedDiff: any): Promise<any> {
    return new Promise((resolve, reject) => {
      this.socketClient.emit("commit", preppedDiff, (err, signal) => {
        if (err) {
          console.error("Error in commit call", err);
          reject(err);
        } else {
          resolve(signal);
        }
      });
    });
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
      //console.log("PerspectiveDiffSync.handleHolochainSignal: calling linkCallback", signal);
      await this.linkCallback(signal);
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
