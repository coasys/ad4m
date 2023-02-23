import { Ad4mClient } from '@perspect3vism/ad4m';
import { ApolloClient, InMemoryCache } from '@apollo/client';
import { WebSocketLink } from '@apollo/client/link/ws';
import WebSocket from 'ws';

export function buildAd4mClient(): Ad4mClient {
  const token = global.ad4mToken;

  let apolloClient = new ApolloClient({
    link: new WebSocketLink({
      uri: 'ws://localhost:4000/graphql',
      options: { 				
        lazy: true,
				reconnect: true,
				connectionParams: async () => {
					return {
						headers: {
							authorization: token
						}
					}
				} },
      webSocketImpl: WebSocket,
    }),
    cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
    defaultOptions: {
      watchQuery: {
        fetchPolicy: "no-cache",
      },
      query: {
        fetchPolicy: "no-cache",
      }
    },
  });

  //@ts-ignore
  return new Ad4mClient(apolloClient);
}