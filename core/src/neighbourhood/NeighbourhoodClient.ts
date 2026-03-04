import { ApolloClient, gql, FetchResult } from "@apollo/client/core";
import { Address } from "../Address";
import { DID } from "../DID";
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language";
import {
  Perspective,
  PerspectiveUnsignedInput,
} from "../perspectives/Perspective";
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle";
import unwrapApolloResult from "../unwrapApolloResult";
import { isSocketCloseError } from "../utils";

export class NeighbourhoodClient {
  private _apolloClient: ApolloClient<any>;
  private _signalHandlers: Map<string, TelepresenceSignalCallback[]> =
    new Map();
  private _signalSubscriptions: Map<string, { unsubscribe(): void }> =
    new Map();

  constructor(client: ApolloClient<any>) {
    this._apolloClient = client;
  }

  async publishFromPerspective(
    perspectiveUUID: string,
    linkLanguage: Address,
    meta: Perspective,
  ): Promise<string> {
    const { neighbourhoodPublishFromPerspective } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodPublishFromPerspective(
            $linkLanguage: String!
            $meta: PerspectiveInput!
            $perspectiveUUID: String!
          ) {
            neighbourhoodPublishFromPerspective(
              linkLanguage: $linkLanguage
              meta: $meta
              perspectiveUUID: $perspectiveUUID
            )
          }
        `,
        variables: { perspectiveUUID, linkLanguage, meta: meta },
      }),
    );
    return neighbourhoodPublishFromPerspective;
  }

  async joinFromUrl(url: string): Promise<PerspectiveHandle> {
    const { neighbourhoodJoinFromUrl } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodJoinFromUrl($url: String!) {
            neighbourhoodJoinFromUrl(url: $url) {
              uuid
              name
              sharedUrl
              state
              neighbourhood {
                data {
                  linkLanguage
                  meta {
                    links {
                      author
                      timestamp
                      data {
                        source
                        predicate
                        target
                      }
                      proof {
                        valid
                        invalid
                        signature
                        key
                      }
                    }
                  }
                }
                author
              }
            }
          }
        `,
        variables: { url },
      }),
    );
    return neighbourhoodJoinFromUrl;
  }

  async otherAgents(perspectiveUUID: string): Promise<DID[]> {
    const { neighbourhoodOtherAgents } = unwrapApolloResult(
      await this._apolloClient.query({
        query: gql`
          query neighbourhoodOtherAgents($perspectiveUUID: String!) {
            neighbourhoodOtherAgents(perspectiveUUID: $perspectiveUUID)
          }
        `,
        variables: { perspectiveUUID },
      }),
    );
    return neighbourhoodOtherAgents;
  }

  async hasTelepresenceAdapter(perspectiveUUID: string): Promise<boolean> {
    const { neighbourhoodHasTelepresenceAdapter } = unwrapApolloResult(
      await this._apolloClient.query({
        query: gql`
          query neighbourhoodHasTelepresenceAdapter($perspectiveUUID: String!) {
            neighbourhoodHasTelepresenceAdapter(
              perspectiveUUID: $perspectiveUUID
            )
          }
        `,
        variables: { perspectiveUUID },
      }),
    );
    return neighbourhoodHasTelepresenceAdapter;
  }

  async onlineAgents(perspectiveUUID: string): Promise<OnlineAgent[]> {
    const { neighbourhoodOnlineAgents } = unwrapApolloResult(
      await this._apolloClient.query({
        query: gql`
          query neighbourhoodOnlineAgents($perspectiveUUID: String!) {
            neighbourhoodOnlineAgents(perspectiveUUID: $perspectiveUUID) {
              did
              status {
                author
                timestamp
                data {
                  links {
                    author
                    timestamp
                    data {
                      source
                      predicate
                      target
                    }
                    proof {
                      valid
                      invalid
                      signature
                      key
                    }
                  }
                }
                proof {
                  valid
                  invalid
                  signature
                  key
                }
              }
            }
          }
        `,
        variables: { perspectiveUUID },
      }),
    );
    return neighbourhoodOnlineAgents;
  }

  async setOnlineStatus(
    perspectiveUUID: string,
    status: Perspective,
  ): Promise<boolean> {
    const { neighbourhoodSetOnlineStatus } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSetOnlineStatus(
            $perspectiveUUID: String!
            $status: PerspectiveInput!
          ) {
            neighbourhoodSetOnlineStatus(
              perspectiveUUID: $perspectiveUUID
              status: $status
            )
          }
        `,
        variables: { perspectiveUUID, status },
      }),
    );
    return neighbourhoodSetOnlineStatus;
  }

  async setOnlineStatusU(
    perspectiveUUID: string,
    status: PerspectiveUnsignedInput,
  ): Promise<boolean> {
    const { neighbourhoodSetOnlineStatusU } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSetOnlineStatusU(
            $perspectiveUUID: String!
            $status: PerspectiveUnsignedInput!
          ) {
            neighbourhoodSetOnlineStatusU(
              perspectiveUUID: $perspectiveUUID
              status: $status
            )
          }
        `,
        variables: { perspectiveUUID, status },
      }),
    );
    return neighbourhoodSetOnlineStatusU;
  }

  async sendSignal(
    perspectiveUUID: string,
    remoteAgentDid: string,
    payload: Perspective,
  ): Promise<boolean> {
    const { neighbourhoodSendSignal } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSendSignal(
            $perspectiveUUID: String!
            $remoteAgentDid: String!
            $payload: PerspectiveInput!
          ) {
            neighbourhoodSendSignal(
              perspectiveUUID: $perspectiveUUID
              remoteAgentDid: $remoteAgentDid
              payload: $payload
            )
          }
        `,
        variables: { perspectiveUUID, remoteAgentDid, payload },
      }),
    );
    return neighbourhoodSendSignal;
  }

  async sendSignalU(
    perspectiveUUID: string,
    remoteAgentDid: string,
    payload: PerspectiveUnsignedInput,
  ): Promise<boolean> {
    const { neighbourhoodSendSignalU } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSendSignalU(
            $perspectiveUUID: String!
            $remoteAgentDid: String!
            $payload: PerspectiveUnsignedInput!
          ) {
            neighbourhoodSendSignalU(
              perspectiveUUID: $perspectiveUUID
              remoteAgentDid: $remoteAgentDid
              payload: $payload
            )
          }
        `,
        variables: { perspectiveUUID, remoteAgentDid, payload },
      }),
    );
    return neighbourhoodSendSignalU;
  }

  async sendBroadcast(
    perspectiveUUID: string,
    payload: Perspective,
    loopback: boolean = false,
  ): Promise<boolean> {
    const { neighbourhoodSendBroadcast } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSendBroadcast(
            $perspectiveUUID: String!
            $payload: PerspectiveInput!
            $loopback: Boolean
          ) {
            neighbourhoodSendBroadcast(
              perspectiveUUID: $perspectiveUUID
              payload: $payload
              loopback: $loopback
            )
          }
        `,
        variables: { perspectiveUUID, payload, loopback },
      }),
    );
    return neighbourhoodSendBroadcast;
  }

  async sendBroadcastU(
    perspectiveUUID: string,
    payload: PerspectiveUnsignedInput,
    loopback: boolean = false,
  ): Promise<boolean> {
    const { neighbourhoodSendBroadcastU } = unwrapApolloResult(
      await this._apolloClient.mutate({
        mutation: gql`
          mutation neighbourhoodSendBroadcastU(
            $perspectiveUUID: String!
            $payload: PerspectiveUnsignedInput!
            $loopback: Boolean
          ) {
            neighbourhoodSendBroadcastU(
              perspectiveUUID: $perspectiveUUID
              payload: $payload
              loopback: $loopback
            )
          }
        `,
        variables: { perspectiveUUID, payload, loopback },
      }),
    );
    return neighbourhoodSendBroadcastU;
  }

  dispatchSignal(perspectiveUUID: string, signal: any) {
    const handlers = this._signalHandlers.get(perspectiveUUID);
    if (handlers) {
      for (const handler of handlers) {
        try {
          handler(signal);
        } catch (e) {
          console.error("Error in signal handler:", e);
        }
      }
    }
  }

  async subscribeToSignals(perspectiveUUID: string): Promise<void> {
    const that = this;
    const sub = this._apolloClient
      .subscribe({
        query: gql`
          subscription neighbourhoodSignal($perspectiveUUID: String!) {
            neighbourhoodSignal(perspectiveUUID: $perspectiveUUID) {
              author
              timestamp
              data {
                links {
                  author
                  timestamp
                  data {
                    source
                    predicate
                    target
                  }
                  proof {
                    valid
                    invalid
                    signature
                    key
                  }
                }
              }
              proof {
                valid
                invalid
                signature
                key
              }
            }
          }
        `,
        variables: { perspectiveUUID },
      })
      .subscribe({
        next: (result: FetchResult<any>) => {
          try {
            const { neighbourhoodSignal } = unwrapApolloResult(result);
            that.dispatchSignal(perspectiveUUID, neighbourhoodSignal);
          } catch (e) {
            console.error("Error in signal subscription:", e);
          }
        },
        error: (e) => {
          if (!isSocketCloseError(e))
            console.error("neighbourhoodSignal subscription error:", e);
        },
      });
    this._signalSubscriptions.set(perspectiveUUID, sub);
  }

  async addSignalHandler(
    perspectiveUUID: string,
    handler: TelepresenceSignalCallback,
  ): Promise<void> {
    let handlersForPerspective = this._signalHandlers.get(perspectiveUUID);
    if (!handlersForPerspective) {
      handlersForPerspective = [];
      this._signalHandlers.set(perspectiveUUID, handlersForPerspective);
      // Push handler BEFORE subscribing so it's available when signals arrive
      handlersForPerspective.push(handler);
      await this.subscribeToSignals(perspectiveUUID);
    } else {
      handlersForPerspective.push(handler);
    }
  }

  removeSignalHandler(
    perspectiveUUID: string,
    handler: TelepresenceSignalCallback,
  ): void {
    const handlersForPerspective = this._signalHandlers.get(perspectiveUUID);
    if (handlersForPerspective) {
      const index = handlersForPerspective.indexOf(handler);
      if (index > -1) {
        handlersForPerspective.splice(index, 1);
      }
      // Intentionally keep the Apollo subscription and the handlers-map entry alive
      // even when the array becomes empty. This prevents an unnecessary re-subscription
      // if a new handler is added shortly after (the empty array makes addSignalHandler
      // skip subscribeToSignals). For explicit teardown use unsubscribeFromPerspective().
    }
  }

  /** Fully tears down the Apollo subscription for a perspective UUID. */
  unsubscribeFromPerspective(perspectiveUUID: string): void {
    this._signalSubscriptions.get(perspectiveUUID)?.unsubscribe();
    this._signalSubscriptions.delete(perspectiveUUID);
    this._signalHandlers.delete(perspectiveUUID);
  }
}
